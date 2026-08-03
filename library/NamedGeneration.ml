type cardinality = Exactly_one | First
type capture = Numbered of int | Named of string

type generated_name =
  | Sequential
  | Regex of {
      pattern : string;
      flags : string;
      capture : capture;
      cardinality : cardinality;
    }

let extensions = Hashtbl.create 8
let regexp_cache = Hashtbl.create 8
let compiled_regexp_count_ref = ref 0
let compiled_regexp_count () = !compiled_regexp_count_ref

let capture_of_args ~kind ~value =
  match kind with
  | "numbered" -> (
      match int_of_string_opt value with
      | Some index when index >= 0 -> Numbered index
      | _ ->
          failwith
            (Printf.sprintf
               "invalid numbered capture %S; expected a non-negative integer" value))
  | "named" when not (String.equal value "") -> Named value
  | "named" -> failwith "invalid named capture; expected a non-empty name"
  | kind -> failwith (Printf.sprintf "unsupported capture kind %S" kind)

let cardinality_of_arg = function
  | "exactlyOne" -> Exactly_one
  | "first" -> First
  | value -> failwith (Printf.sprintf "unsupported cardinality %S" value)

let base64url_value = function
  | 'A' .. 'Z' as character -> Char.code character - Char.code 'A'
  | 'a' .. 'z' as character -> Char.code character - Char.code 'a' + 26
  | '0' .. '9' as character -> Char.code character - Char.code '0' + 52
  | '-' -> 62
  | '_' -> 63
  | character ->
      failwith
        (Printf.sprintf "invalid base64url character %C in generated-name pattern"
           character)

let decode_base64url input =
  let length = String.length input in
  if length mod 4 = 1 then
    failwith "invalid base64url generated-name pattern length";
  let output = Buffer.create ((length * 3) / 4) in
  let rec decode offset =
    if offset < length then (
      let remaining = length - offset in
      let first = base64url_value input.[offset] in
      let second = base64url_value input.[offset + 1] in
      Buffer.add_char output (Char.chr ((first lsl 2) lor (second lsr 4)));
      if remaining > 2 then (
        let third = base64url_value input.[offset + 2] in
        Buffer.add_char output
          (Char.chr (((second land 15) lsl 4) lor (third lsr 2)));
        if remaining > 3 then (
          let fourth = base64url_value input.[offset + 3] in
          Buffer.add_char output
            (Char.chr (((third land 3) lsl 6) lor fourth))));
      decode (offset + 4))
  in
  decode 0;
  Buffer.contents output

let add_regex ~extension ~pattern ~flags ~capture_kind ~capture_value ~cardinality =
  if String.equal extension "" then
    failwith "invalid generated-name extension; expected a non-empty name";
  let generated_name =
    Regex
      {
        pattern;
        flags;
        capture = capture_of_args ~kind:capture_kind ~value:capture_value;
        cardinality = cardinality_of_arg cardinality;
      }
  in
  Hashtbl.replace extensions extension generated_name

let add_regex_base64url ~extension ~pattern ~flags ~capture_kind ~capture_value
    ~cardinality =
  add_regex ~extension ~pattern:(decode_base64url pattern)
    ~flags:(if String.equal flags "-" then "" else flags)
    ~capture_kind ~capture_value ~cardinality

let for_extension extension =
  Hashtbl.find_opt extensions extension |> Option.value ~default:Sequential

let regexp_key pattern flags = pattern ^ "\000" ^ flags

let compile_regexp pattern flags =
  let key = regexp_key pattern flags in
  match Hashtbl.find_opt regexp_cache key with
  | Some regexp -> regexp
  | None -> (
      match Quickjs.RegExp.compile ~flags pattern with
      | Ok regexp ->
          Hashtbl.add regexp_cache key regexp;
          incr compiled_regexp_count_ref;
          regexp
      | Error error ->
          failwith
            (Printf.sprintf "invalid ECMAScript regex /%s/%s: %s" pattern flags
               (Quickjs.RegExp.compile_error_to_string error)))

let capture_description = function
  | Numbered index -> Printf.sprintf "numbered capture %d" index
  | Named name -> Printf.sprintf "named capture %S" name

let selected_capture capture (result : Quickjs.RegExp.match_result) =
  match capture with
  | Numbered index ->
      if index < 0 || index >= Array.length result.captures then None
      else result.captures.(index)
  | Named name -> Quickjs.RegExp.group name result

let flags_for_iteration flags =
  if String.contains flags 'g' || String.contains flags 'y' then flags
  else flags ^ "g"

let first_two_matches regexp source =
  Quickjs.RegExp.set_last_index regexp 0;
  let prepared = Quickjs.RegExp.prepare_input source in
  let rec loop acc =
    if List.length acc = 2 then List.rev acc
    else
      match Quickjs.RegExp.exec_prepared ~timeout_ms:100. regexp prepared with
      | None -> List.rev acc
      | Some prepared_match ->
          let start, end_ = prepared_match.range.utf16 in
          if start = end_ then
            Quickjs.RegExp.set_last_index regexp
              (Quickjs.RegExp.prepared_advance_index prepared
                 ~unicode:(Quickjs.RegExp.unicode regexp) end_);
          loop (prepared_match.result :: acc)
  in
  loop []

let validate_name name =
  let length = String.length name in
  let is_start = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_continue = function
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  length > 0 && is_start name.[0]
  && String.for_all is_continue (String.sub name 1 (length - 1))

let extract_name ~extension ~source = function
  | Sequential -> None
  | Regex { pattern; flags; capture; cardinality } ->
      let regexp =
        compile_regexp pattern
          (match cardinality with First -> flags | Exactly_one -> flags_for_iteration flags)
      in
      let matches =
        try
          match cardinality with
          | First ->
              Quickjs.RegExp.set_last_index regexp 0;
              (match Quickjs.RegExp.exec ~timeout_ms:100. regexp source with
              | None -> []
              | Some result -> [ result ])
          | Exactly_one -> first_two_matches regexp source
        with Quickjs.RegExp.Timeout ->
          failwith
            (Printf.sprintf "generated-name regex /%s/%s timed out after 100ms" pattern
               flags)
      in
      let result =
        match (cardinality, matches) with
        | _, [] ->
            failwith
              (Printf.sprintf "extension %S: generated-name regex /%s/%s did not match"
                 extension pattern flags)
        | Exactly_one, _ :: _ :: _ ->
            failwith
              (Printf.sprintf
                 "extension %S: generated-name regex /%s/%s matched more than once"
                 extension pattern flags)
        | _, result :: _ -> result
      in
      let name =
        match selected_capture capture result with
        | None ->
            failwith
              (Printf.sprintf "extension %S: %s was missing for regex /%s/%s"
                 extension (capture_description capture) pattern flags)
        | Some "" ->
            failwith
              (Printf.sprintf "extension %S: %s was empty for regex /%s/%s"
                 extension (capture_description capture) pattern flags)
        | Some name -> name
      in
      if not (validate_name name) then
        failwith
          (Printf.sprintf
             "extension %S: extracted generated name %S is invalid; expected [_A-Za-z][_0-9A-Za-z]* (%s from /%s/%s)"
             extension name (capture_description capture) pattern flags);
      Some name

let source_module file_name =
  let base = Filename.basename file_name in
  if Filename.check_suffix base ".res" then Filename.chop_suffix base ".res"
  else if Filename.check_suffix base ".resi" then Filename.chop_suffix base ".resi"
  else base

let named_target ~file_name ~extension ~name =
  Printf.sprintf "%s__%s__%s" (source_module file_name) extension name
