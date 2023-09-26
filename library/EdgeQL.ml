open Ppxlib

let capitalizeFirstLetter str =
  if String.length str = 0 then str
  else
    String.uppercase_ascii (String.sub str 0 1)
    ^ String.sub str 1 (String.length str - 1)

let extractEdgeQLQueryName ~loc input =
  (* Define the regular expression pattern *)
  let pattern =
    Str.regexp "[\t\n\r ]*#[\t\n\r ]*@name[\t\n\r ]+\\([^\t\n\r ]+\\)"
  in

  try
    (* Find the position of the matching substring *)
    let _ = Str.search_forward pattern input 0 in
    (* Extract the matched name *)
    let name = Str.matched_group 1 input in
    name
  with Not_found ->
    Ppxlib.Location.raise_errorf ~loc
      {|Could not find a comment with the query name.
  Each EdgeQL code block needs to be prepended with a comment
  defining the query name.

  Example:
  let findUser = %%edgeql(`
    # @name findUser
    select User {
      name
    } filter .id = <uuid>$userId
  `)|}

let expressionExtension =
  Extension.declare "edgeql" Extension.Context.expression
    (let open Ast_pattern in
    single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let bindingName = extractEdgeQLQueryName queryStr ~loc in
      let lid =
        Longident.parse
          (Printf.sprintf "%s__edgeDb.%s.query"
             Filename.(remove_extension (basename path))
             (capitalizeFirstLetter bindingName))
      in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})

let moduleExtension =
  Extension.declare "edgeql" Extension.Context.module_expr
    (let open Ast_pattern in
    single_expr_payload (estring __))
    (fun ~loc ~path queryStr ->
      let bindingName = extractEdgeQLQueryName ~loc queryStr in
      let lid =
        Longident.parse
          (Printf.sprintf "%s__edgeDb.%s"
             Filename.(remove_extension (basename path))
             (capitalizeFirstLetter bindingName))
      in
      Ast_helper.Mod.ident ~loc {txt = lid; loc = Location.none})
