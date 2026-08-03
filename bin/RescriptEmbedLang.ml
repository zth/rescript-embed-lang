open Ppxlib

let () =
  Driver.add_arg "-enable-generic-transform"
    (Arg.Unit
       (fun () -> RescriptEmbedLangLibrary.Utils.enableGenericTransform := true))
    ~doc:"Enable the generic transform"

let generated_name_regex_arg =
  let extension = ref "" in
  let pattern = ref "" in
  let flags = ref "" in
  let capture_kind = ref "" in
  let capture_value = ref "" in
  Arg.Tuple
    [
      Arg.Set_string extension;
      Arg.Set_string pattern;
      Arg.Set_string flags;
      Arg.Set_string capture_kind;
      Arg.Set_string capture_value;
      Arg.String
        (fun cardinality ->
          RescriptEmbedLangLibrary.NamedGeneration.add_regex_base64url
            ~extension:!extension ~pattern:!pattern ~flags:!flags
            ~capture_kind:!capture_kind ~capture_value:!capture_value ~cardinality);
    ]

let () =
  Driver.add_arg "-embed-lang-generated-name-regex" generated_name_regex_arg
    ~doc:
      "EXTENSION PATTERN_BASE64URL FLAGS_OR_DASH CAPTURE_KIND CAPTURE_VALUE CARDINALITY Configure named generation for one extension; repeat for additional extensions"

let _ = Driver.run_as_ppx_rewriter ()
