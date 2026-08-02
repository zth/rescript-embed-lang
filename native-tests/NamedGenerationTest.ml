module Named = RescriptEmbedLangLibrary.NamedGeneration

let fail format = Printf.ksprintf failwith format

let contains haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let string_member name json =
  match Yojson.Safe.Util.member name json with
  | `String value -> value
  | _ -> fail "missing string field %S" name

let optional_string_member name json =
  match Yojson.Safe.Util.member name json with
  | `String value -> Some value
  | `Null -> None
  | _ -> fail "invalid optional string field %S" name

let capture json =
  match string_member "captureKind" json with
  | "numbered" -> Named.Numbered (int_of_string (string_member "captureValue" json))
  | "named" -> Named.Named (string_member "captureValue" json)
  | kind -> fail "unknown capture kind %S" kind

let cardinality json =
  match string_member "cardinality" json with
  | "exactlyOne" -> Named.Exactly_one
  | "first" -> Named.First
  | value -> fail "unknown cardinality %S" value

let config json =
  Named.Regex
    {
      pattern = string_member "pattern" json;
      flags = string_member "flags" json;
      capture = capture json;
      cardinality = cardinality json;
    }

let run_case json =
  let label = string_member "label" json in
  let source = string_member "source" json in
  let result =
    try Ok (Named.extract_name ~extension:"fixture" ~source (config json))
    with Failure message -> Error message
  in
  match
    ( optional_string_member "expectedName" json,
      optional_string_member "errorContains" json,
      result )
  with
  | Some expected, _, Ok (Some actual) when String.equal expected actual -> ()
  | _, Some expected, Error message when contains message expected -> ()
  | Some expected, _, Ok actual ->
      fail "%s: expected name %S, got %s" label expected
        (match actual with None -> "none" | Some value -> Printf.sprintf "%S" value)
  | Some expected, _, Error message ->
      fail "%s: expected name %S, got error %S" label expected message
  | _, Some expected, Error message ->
      fail "%s: expected error containing %S, got %S" label expected message
  | _, Some expected, Ok _ -> fail "%s: expected error containing %S" label expected
  | _ -> fail "%s: fixture has no expected result" label

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
  let json = Yojson.Safe.from_file Sys.argv.(1) in
  Yojson.Safe.Util.member "cases" json
  |> Yojson.Safe.Util.to_list |> List.iter run_case;
  test_cache ();
  test_stable_target ();
  test_timeout ();
  Printf.printf "native named-generation corpus: ok\n"
