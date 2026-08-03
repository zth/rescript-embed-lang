module Named = RescriptEmbedLangLibrary.NamedGeneration

let fail format = Printf.ksprintf failwith format

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

type fixture_case = {
  label : string;
  strategy : string;
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
   strategy;
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
        strategy = decode_field strategy;
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
      fail "fixture line %d has %d fields; expected 10" line_number
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
  match fixture.strategy with
  | "graphqlDefinition" -> Named.Graphql_definition
  | "nameDirective" -> Named.Name_directive
  | "nameDirectivePostgreSQL" -> Named.Name_directive_postgresql
  | "nameDirectiveShell" -> Named.Name_directive_shell
  | "nameDirectivePython" -> Named.Name_directive_python
  | "regex" ->
      Named.Regex
        {
          pattern = fixture.pattern;
          flags = fixture.flags;
          capture = capture fixture;
          cardinality = cardinality fixture;
        }
  | strategy -> fail "unknown naming strategy %S" strategy

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

let test_first_class_strategies_skip_quickjs () =
  let before = Named.compiled_regexp_count () in
  ignore
    (Named.extract_name ~extension:"graphql" ~source:"query NativeScan { viewer }"
       Named.Graphql_definition);
  ignore
    (Named.extract_name ~extension:"comments" ~source:"-- @name NativeScan\nselect 1"
       Named.Name_directive_postgresql);
  let after = Named.compiled_regexp_count () in
  if after <> before then
    fail "first-class naming strategies unexpectedly compiled a QuickJS regexp"

let test_stable_target () =
  let actual =
    Named.named_target ~file_name:"src/Operations.res" ~extension:"fixture" ~name:"GetThing"
  in
  if not (String.equal actual "Operations__fixture__GetThing") then
    fail "unexpected stable named target %S" actual

let test_config_loading () =
  let path = Filename.temp_file "rescript-embed-lang-" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let channel = open_out_bin path in
      output_string channel
        {|{"version":1,"extensions":{"graphql":{"generatedName":{"kind":"graphqlDefinition"}},"comments":{"generatedName":{"kind":"nameDirective","syntax":"javascript"}},"postgres":{"generatedName":{"kind":"nameDirective","syntax":"postgresql"}},"shell":{"generatedName":{"kind":"nameDirective","syntax":"shell"}},"python":{"generatedName":{"kind":"nameDirective","syntax":"python"}}}}|};
      close_out channel;
      Named.set_config_path path;
      (match Named.for_extension ~source_file:"src/Test.res" "graphql" with
      | Named.Graphql_definition -> ()
      | _ -> fail "unexpected GraphQL config strategy");
      (match Named.for_extension ~source_file:"src/Test.res" "comments" with
      | Named.Name_directive -> ()
      | _ -> fail "unexpected name-directive config strategy");
      (match Named.for_extension ~source_file:"src/Test.res" "postgres" with
      | Named.Name_directive_postgresql -> ()
      | _ -> fail "unexpected PostgreSQL name-directive config strategy");
      (match Named.for_extension ~source_file:"src/Test.res" "shell" with
      | Named.Name_directive_shell -> ()
      | _ -> fail "unexpected shell name-directive config strategy");
      (match Named.for_extension ~source_file:"src/Test.res" "python" with
      | Named.Name_directive_python -> ()
      | _ -> fail "unexpected Python name-directive config strategy");
      match Named.for_extension ~source_file:"src/Test.res" "unconfigured" with
      | Named.Sequential -> ()
      | _ -> fail "unconfigured extensions should remain sequential")

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
  test_first_class_strategies_skip_quickjs ();
  test_cache ();
  test_stable_target ();
  test_config_loading ();
  test_timeout ();
  Printf.printf "native named-generation corpus: ok\n"
