open Ppxlib

let capitalizeFirstLetter str =
  if String.length str = 0 then str
  else
    String.uppercase_ascii (String.sub str 0 1)
    ^ String.sub str 1 (String.length str - 1)

let extractSQLQueryName ~loc input =
  let pattern =
    Str.regexp
      "/\\*[\t\n\
       \r ]*@name[\t\n\
       \r ]+\\([^\t\n\
       \r *][^\t\n\
       \r ]*\\)[\t\n\
       \r ]*\\*\\/"
  in

  try
    let _ = Str.search_forward pattern input 0 in
    let name = Str.matched_group 1 input in
    name
  with Not_found ->
    Ppxlib.Location.raise_errorf ~loc
      {|Could not find a comment with the query name.
  Each SQL code block needs to be prepended with a comment
  defining the query name.

  Example:
  let findUser = %%sql.many(`
    /* @name findBooks */
    select * from books
  `)|}

let isSQLExtensionNode name =
  name = "sql" || String.starts_with ~prefix:"sql." name

let extractSQLTargetVariant str =
  match String.split_on_char '.' str with
  | ["sql"; tag] -> Some tag
  | _ -> None

type transformMode = LetBinding | ModuleBinding

let makeLident fileName ~queryName ~targetFn ~transformMode =
  Longident.parse
    (Printf.sprintf "%s__sql.%s%s"
       (if String.ends_with fileName ~suffix:".res" then
          Filename.(chop_suffix (basename fileName) ".res")
        else Filename.(chop_suffix (basename fileName) ".resi"))
       (capitalizeFirstLetter queryName)
       (match (transformMode, targetFn) with
       | LetBinding, Some targetFn -> "." ^ targetFn
       | ModuleBinding, _ | LetBinding, None -> ""))

let sqlExprExtension =
  Extension.declare "sql" Extension.Context.expression
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:(Some "many") ~transformMode:LetBinding
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let sqlOneExprExtension =
  Extension.declare "sql.one" Extension.Context.expression
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:(Some "one") ~transformMode:LetBinding
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let sqlManyExprExtension =
  Extension.declare "sql.many" Extension.Context.expression
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:(Some "many") ~transformMode:LetBinding
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let sqlExpectOneExprExtension =
  Extension.declare "sql.expectOne" Extension.Context.expression
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:(Some "expectOne") ~transformMode:LetBinding
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let sqlExecuteExprExtension =
  Extension.declare "sql.execute" Extension.Context.expression
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:(Some "execute") ~transformMode:LetBinding
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let sqlModuleExtension =
  Extension.declare "sql" Extension.Context.module_expr
    (let open Ast_pattern in
     single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let fileName = loc.loc_start.pos_fname in
      let lid =
        makeLident fileName
          ~queryName:(extractSQLQueryName queryStr ~loc)
          ~targetFn:None ~transformMode:ModuleBinding
      in
      Ast_helper.Mod.ident ~loc {txt = lid; loc})
