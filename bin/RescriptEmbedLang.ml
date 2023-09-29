open Ppxlib

let () =
  Driver.add_arg "-enable-generic-transform"
    (Arg.Unit
       (fun () -> RescriptEmbedLangLibrary.Utils.enableGenericTransform := true))
    ~doc:"Enable the generic transform"

let _ = Driver.run_as_ppx_rewriter ()
