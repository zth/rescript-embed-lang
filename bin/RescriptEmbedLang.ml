open Ppxlib

let () =
  Driver.add_arg "-enable-generic-transform"
    (Arg.Unit
       (fun () -> RescriptEmbedLangLibrary.Utils.enableGenericTransform := true))
    ~doc:"Enable the generic transform"

let () =
  Driver.add_arg "-embed-lang-config"
    (Arg.String RescriptEmbedLangLibrary.NamedGeneration.set_config_path)
    ~doc:"PATH Load the versioned rescript-embed-lang generated-name configuration"

let _ = Driver.run_as_ppx_rewriter ()
