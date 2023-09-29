open Ppxlib
module Utils = Utils

let () =
  Driver.register_transformation
    ~preprocess_impl:GenericTransform.structure_mapper
    ~extensions:[EdgeQL.expressionExtension; EdgeQL.moduleExtension]
    "rescript-embed-lang"
