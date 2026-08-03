open Ppxlib

let () =
  Driver.add_arg "-enable-generic-transform"
    (Arg.Unit
       (fun () -> RescriptEmbedLangLibrary.Utils.enableGenericTransform := true))
    ~doc:"Enable the generic transform"

let () =
  Driver.add_arg "-embed-lang-config"
    (Arg.String RescriptEmbedLangLibrary.NamedGeneration.set_config_path)
    ~doc:"PATH Load generated-name strategies from one generated config file"

let _ = Driver.run_as_ppx_rewriter ()
