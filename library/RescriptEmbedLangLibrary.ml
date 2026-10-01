open Ppxlib
module Utils = Utils
module NamedGeneration = NamedGeneration

let () =
  Driver.register_transformation
    ~preprocess_impl:(fun structure ->
      structure |> SqlCoreSQL.fragment_structure_mapper
      |> GenericTransform.structure_mapper)
    ~extensions:
      [
        EdgeQL.expressionExtension;
        EdgeQL.moduleExtension;
        SqlCoreSQL.sqlExprExtension;
        SqlCoreSQL.sqlManyExprExtension;
        SqlCoreSQL.sqlOneExprExtension;
        SqlCoreSQL.sqlExpectOneExprExtension;
        SqlCoreSQL.sqlExecuteExprExtension;
        SqlCoreSQL.sqlModuleExtension;
      ]
    "rescript-embed-lang"
