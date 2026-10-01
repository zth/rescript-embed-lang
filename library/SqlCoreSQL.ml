open Ppxlib

let capitalizeFirstLetter str =
  if String.length str = 0 then str
  else
    String.uppercase_ascii (String.sub str 0 1)
    ^ String.sub str 1 (String.length str - 1)

let queryNum = ref 1

let extractSQLQueryName ~loc input =
  let pattern =
    Str.regexp
      "/\\*[\t\n\r ]*@name[\t\n\r ]+\\([^\t\n\r *][^\t\n\r ]*\\)[\t\n\r ]"
  in

  try
    let _ = Str.search_forward pattern input 0 in
    let name = Str.matched_group 1 input in
    Some name
  with Not_found -> None

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
       (let moduleName =
          if String.ends_with fileName ~suffix:".res" then
            Filename.(chop_suffix (basename fileName) ".res")
          else Filename.(chop_suffix (basename fileName) ".resi")
        in
        capitalizeFirstLetter moduleName)
       (match queryName with
       | Some queryName -> capitalizeFirstLetter queryName
       | None ->
         let num = !queryNum in
         queryNum := num + 1;
         "Query" ^ string_of_int num)
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

(* A fragment module is backed by the types generated for its source file. Keep
   the nested module path so local references match the extracted fragment key. *)
let fragment_module_path ~file_name names =
  let source_module =
    Filename.basename file_name
    |> Filename.remove_extension |> capitalizeFirstLetter
  in
  Longident.parse
    (String.concat "." ((source_module ^ "__fragments__sql") :: names))

class fragment_mapper =
  object (self)
    inherit Ast_traverse.map as super

    val mutable module_path = []

    method! structure_item item =
      match item.Parsetree.pstr_desc with
      | Pstr_module
          ({
             pmb_name = {txt = Some name; _};
             pmb_expr =
               {
                 pmod_desc =
                   Pmod_extension ({txt = "sql.fragment"; loc}, payload);
                 _;
               } as pmb_expr;
           } as binding) ->
        (match payload with
        | PStr
            [
              {
                pstr_desc =
                  Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string _); _}, _);
                _;
              };
            ] ->
          ()
        | _ ->
          Location.raise_errorf ~loc
            "sql.fragment requires a single SQL string literal");
        let names = List.rev (name :: module_path) in
        let target =
          fragment_module_path ~file_name:loc.loc_start.pos_fname names
        in
        {
          item with
          pstr_desc =
            Pstr_module
              {
                binding with
                pmb_expr =
                  {pmb_expr with pmod_desc = Pmod_ident {txt = target; loc}};
              };
        }
      | Pstr_module {pmb_name = {txt = Some name; _}; _} ->
        module_path <- name :: module_path;
        let item = super#structure_item item in
        module_path <- List.tl module_path;
        item
      | _ -> super#structure_item item
  end

let fragment_structure_mapper structure =
  (new fragment_mapper)#structure structure
