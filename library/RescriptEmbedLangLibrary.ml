open Ppxlib

let () =
  Driver.register_transformation
    ~extensions:[EdgeQL.expressionExtension; EdgeQL.moduleExtension]
    "rescript-embed-lang"
