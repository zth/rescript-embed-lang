module Named = RescriptEmbedLangLibrary.NamedGeneration

let fail format = Printf.ksprintf failwith format

let contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= haystack_length
    && (String.sub haystack index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

type fixture_case = {
  label : string;
  strategy : string;
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
  | [ label; strategy; source; result_kind; result_value ] ->
      {
        label = decode_field label;
        strategy = decode_field strategy;
        source = decode_field source;
        result_kind = decode_field result_kind;
        result_value = decode_field result_value;
      }
  | fields ->
      fail "fixture line %d has %d fields; expected 5" line_number
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

let config = function
  | "graphqlDefinition" -> Named.Graphql_definition
  | "nameDirective" -> Named.Name_directive
  | strategy -> fail "unknown naming strategy %S" strategy

let run_case fixture =
  let result =
    try
      Ok
        (Named.extract_name ~source:fixture.source (config fixture.strategy))
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

let test_stable_target () =
  let actual =
    Named.named_target ~file_name:"src/Operations.res" ~extension:"fixture"
      ~name:"GetThing"
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
        {|{"version":1,"extensions":{"graphql":{"generatedName":{"kind":"graphqlDefinition"}},"comments":{"generatedName":{"kind":"nameDirective"}}}}|};
      close_out channel;
      Named.set_config_path path;
      (match Named.for_extension ~source_file:"src/Test.res" "graphql" with
      | Named.Graphql_definition -> ()
      | _ -> fail "unexpected GraphQL config strategy");
      (match Named.for_extension ~source_file:"src/Test.res" "comments" with
      | Named.Name_directive -> ()
      | _ -> fail "unexpected name-directive config strategy");
      match Named.for_extension ~source_file:"src/Test.res" "unconfigured" with
      | Named.Sequential -> ()
      | _ -> fail "unconfigured extensions should remain sequential")

let () =
  if Array.length Sys.argv <> 2 then fail "expected shared fixture path";
  read_cases Sys.argv.(1) |> List.iter run_case;
  test_stable_target ();
  test_config_loading ();
  Printf.printf "native named-generation corpus: ok\n"
