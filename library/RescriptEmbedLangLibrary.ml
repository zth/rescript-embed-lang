open Ppxlib
module Utils = Utils

let () =
  Driver.register_transformation
    ~preprocess_impl:GenericTransform.structure_mapper
    ~extensions:
      [
        EdgeQL.expressionExtension;
        EdgeQL.moduleExtension;
        PgTypedSQL.sqlExprExtension;
        PgTypedSQL.sqlManyExprExtension;
        PgTypedSQL.sqlOneExprExtension;
        PgTypedSQL.sqlExpectOneExprExtension;
        PgTypedSQL.sqlExecuteExprExtension;
        PgTypedSQL.sqlModuleExtension;
      ]
    "rescript-embed-lang"
