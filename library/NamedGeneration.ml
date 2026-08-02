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

type config = { version : int; extensions : (string, generated_name) Hashtbl.t }

let config_version = 1
let config_path = ref None
let loaded_config = ref None
let regexp_cache = Hashtbl.create 8
let compiled_regexp_count_ref = ref 0
let compiled_regexp_count () = !compiled_regexp_count_ref

let set_config_path path =
  config_path := Some path;
  loaded_config := None

let member name json = Yojson.Safe.Util.member name json

let string_member name json =
  match member name json with
  | `String value -> value
  | _ -> failwith (Printf.sprintf "missing or invalid string field %S" name)

let int_member name json =
  match member name json with
  | `Int value -> value
  | _ -> failwith (Printf.sprintf "missing or invalid integer field %S" name)

let parse_capture json =
  match string_member "kind" json with
  | "numbered" -> Numbered (int_member "index" json)
  | "named" -> Named (string_member "name" json)
  | kind -> failwith (Printf.sprintf "unsupported capture kind %S" kind)

let parse_cardinality = function
  | "exactlyOne" -> Exactly_one
  | "first" -> First
  | value -> failwith (Printf.sprintf "unsupported cardinality %S" value)

let parse_generated_name json =
  match string_member "kind" json with
  | "sequential" -> Sequential
  | "regex" ->
      Regex
        {
          pattern = string_member "pattern" json;
          flags = string_member "flags" json;
          capture = parse_capture (member "capture" json);
          cardinality = parse_cardinality (string_member "cardinality" json);
        }
  | kind -> failwith (Printf.sprintf "unsupported generatedName kind %S" kind)

let load_config path =
  let json = Yojson.Safe.from_file path in
  let version = int_member "version" json in
  if version <> config_version then
    failwith
      (Printf.sprintf
         "unsupported embed-language config version %d (this PPX supports version %d)"
         version config_version);
  let extensions_json = member "extensions" json in
  let extensions = Hashtbl.create 8 in
  Yojson.Safe.Util.to_assoc extensions_json
  |> List.iter (fun (extension, extension_json) ->
         let generated_name =
           parse_generated_name (member "generatedName" extension_json)
         in
         Hashtbl.replace extensions extension generated_name);
  { version; extensions }

let rec find_config_from directory relative_path =
  let candidate = Filename.concat directory relative_path in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None
    else find_config_from parent relative_path

let resolve_config_path ~source_file path =
  if not (Filename.is_relative path) then path
  else
    match find_config_from (Filename.dirname source_file) path with
    | Some resolved -> resolved
    | None ->
        failwith
          (Printf.sprintf
             "cannot resolve relative -embed-lang-config %S from source file %S or any parent directory"
             path source_file)

let get_config ~source_file =
  match !config_path with
  | None -> { version = config_version; extensions = Hashtbl.create 0 }
  | Some configured_path ->
      let resolved_path = resolve_config_path ~source_file configured_path in
      (match !loaded_config with
      | Some (loaded_path, config) when String.equal loaded_path resolved_path -> config
      | _ ->
          let config =
            try load_config resolved_path
            with exn ->
              failwith
                (Printf.sprintf "cannot load -embed-lang-config %S: %s" resolved_path
                   (Printexc.to_string exn))
          in
          loaded_config := Some (resolved_path, config);
          config)

let for_extension ~source_file extension =
  Hashtbl.find_opt (get_config ~source_file).extensions extension
  |> Option.value ~default:Sequential

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
