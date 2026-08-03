module Named = RescriptEmbedLangLibrary.NamedGeneration

let fail format = Printf.ksprintf failwith format

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

type fixture_case = {
  label : string;
  pattern : string;
  flags : string;
  capture_kind : string;
  capture_value : string;
  cardinality : string;
  source : string;
  result_kind : string;
  result_value : string;
}

let decode_field value =
  let length = String.length value in
  let output = Buffer.create length in
  let rec decode index =
    if index < length then
      match value.[index] with
      | '\\' ->
          if index + 1 >= length then fail "trailing escape in fixture field";
          Buffer.add_char output
            (match value.[index + 1] with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | '\\' -> '\\'
            | '"' -> '"'
            | character -> fail "unknown fixture escape \\%c" character);
          decode (index + 2)
      | character ->
          Buffer.add_char output character;
          decode (index + 1)
  in
  decode 0;
  Buffer.contents output

let parse_case line_number line =
  match String.split_on_char '\t' line with
  | [
   label;
   pattern;
   flags;
   capture_kind;
   capture_value;
   cardinality;
   source;
   result_kind;
   result_value;
  ] ->
      {
        label = decode_field label;
        pattern = decode_field pattern;
        flags = decode_field flags;
        capture_kind = decode_field capture_kind;
        capture_value = decode_field capture_value;
        cardinality = decode_field cardinality;
        source = decode_field source;
        result_kind = decode_field result_kind;
        result_value = decode_field result_value;
      }
  | fields ->
      fail "fixture line %d has %d fields; expected 9" line_number
        (List.length fields)

let read_cases path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      let rec read line_number cases =
        match input_line channel with
        | line ->
            let length = String.length line in
            let line =
              if length > 0 && Char.equal line.[length - 1] '\r' then
                String.sub line 0 (length - 1)
              else line
            in
            let trimmed = String.trim line in
            let cases =
              if String.equal trimmed "" || trimmed.[0] = '#' then cases
              else parse_case line_number line :: cases
            in
            read (line_number + 1) cases
        | exception End_of_file -> List.rev cases
      in
      read 1 [])

let capture fixture =
  match fixture.capture_kind with
  | "numbered" -> Named.Numbered (int_of_string fixture.capture_value)
  | "named" -> Named.Named fixture.capture_value
  | kind -> fail "unknown capture kind %S" kind

let cardinality fixture =
  match fixture.cardinality with
  | "exactlyOne" -> Named.Exactly_one
  | "first" -> Named.First
  | value -> fail "unknown cardinality %S" value

let config fixture =
  Named.Regex
    {
      pattern = fixture.pattern;
      flags = fixture.flags;
      capture = capture fixture;
      cardinality = cardinality fixture;
    }

let run_case fixture =
  let result =
    try
      Ok
        (Named.extract_name ~extension:"fixture" ~source:fixture.source
           (config fixture))
    with Failure message -> Error message
  in
  match (fixture.result_kind, result) with
  | "name", Ok (Some actual) when String.equal fixture.result_value actual -> ()
  | "error", Error message when contains message fixture.result_value -> ()
  | "name", Ok actual ->
      fail "%s: expected name %S, got %s" fixture.label fixture.result_value
        (match actual with None -> "none" | Some value -> Printf.sprintf "%S" value)
  | "name", Error message ->
      fail "%s: expected name %S, got error %S" fixture.label fixture.result_value
        message
  | "error", Error message ->
      fail "%s: expected error containing %S, got %S" fixture.label
        fixture.result_value message
  | "error", Ok _ ->
      fail "%s: expected error containing %S" fixture.label fixture.result_value
  | result_kind, _ -> fail "%s: unknown result kind %S" fixture.label result_kind

let test_cache () =
  let config =
    Named.Regex
      {
        pattern = "cache-(?<name>[A-Za-z]+)";
        flags = "";
        capture = Named.Named "name";
        cardinality = Named.First;
      }
  in
  let before = Named.compiled_regexp_count () in
  ignore (Named.extract_name ~extension:"cache" ~source:"cache-First" config);
  ignore (Named.extract_name ~extension:"cache" ~source:"cache-Second" config);
  let after = Named.compiled_regexp_count () in
  if after <> before + 1 then
    fail "regex cache regression: expected one compilation, observed %d" (after - before)

let test_stable_target () =
  let actual =
    Named.named_target ~file_name:"src/Operations.res" ~extension:"fixture" ~name:"GetThing"
  in
  if not (String.equal actual "Operations__fixture__GetThing") then
    fail "unexpected stable named target %S" actual

let test_cli_configuration () =
  Named.add_regex_base64url ~extension:"configured"
    ~pattern:"b3BlcmF0aW9uIChbQS1aYS16XSsp" ~flags:"-" ~capture_kind:"numbered"
    ~capture_value:"1" ~cardinality:"first";
  let actual =
    Named.extract_name ~extension:"configured" ~source:"operation FromCli"
      (Named.for_extension "configured")
  in
  if actual <> Some "FromCli" then fail "unexpected CLI-configured name";
  match Named.for_extension "unconfigured" with
  | Named.Sequential -> ()
  | Named.Regex _ -> fail "unconfigured extensions should remain sequential"

let test_timeout () =
  let config =
    Named.Regex
      {
        pattern = "(a+)+$";
        flags = "";
        capture = Named.Numbered 0;
        cardinality = Named.First;
      }
  in
  let source = String.make 100_000 'a' ^ "!" in
  match Named.extract_name ~extension:"timeout" ~source config with
  | _ -> fail "pathological regex unexpectedly completed"
  | exception Failure message when contains message "timed out" -> ()
  | exception Failure message -> fail "unexpected timeout diagnostic: %s" message

let () =
  if Array.length Sys.argv <> 2 then fail "expected shared fixture path";
  read_cases Sys.argv.(1) |> List.iter run_case;
  test_cache ();
  test_stable_target ();
  test_cli_configuration ();
  test_timeout ();
  Printf.printf "native named-generation corpus: ok\n"
