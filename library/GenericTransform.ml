open Ppxlib

let is_generated_extension_node name = String.starts_with ~prefix:"generated." name

let extract_generated_extension_node str =
  match String.split_on_char (Char.chr 46) str with
  | [ "generated"; tag ] -> Some tag
  | _ -> None

let transformed_count = Hashtbl.create 10

let increment_transformed_count extension =
  let count = Option.value (Hashtbl.find_opt transformed_count extension) ~default:0 in
  let next = count + 1 in
  Hashtbl.replace transformed_count extension next;
  next

type transform_mode = Value | Module

let sequential_target ~file_name ~extension ~transform_mode =
  Printf.sprintf "%s__%s.M%d%s" (NamedGeneration.source_module file_name) extension
    (increment_transformed_count extension)
    (match transform_mode with Value -> ".default" | Module -> "")

let literal_source ~loc payload =
  match payload with
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant (Pconst_string (source, _, _)); _ }, _);
          _;
        };
      ] ->
      source
  | _ ->
      Location.raise_errorf ~loc
        "generated language embeds require a single string-literal payload"

let target ~loc ~file_name ~extension ~payload ~transform_mode =
  try
    match NamedGeneration.for_extension ~source_file:file_name extension with
    | Sequential -> sequential_target ~file_name ~extension ~transform_mode
    | Regex _ as generated_name ->
        let source = literal_source ~loc payload in
        let name =
          match NamedGeneration.extract_name ~extension ~source generated_name with
          | Some name -> name
          | None -> assert false
        in
        NamedGeneration.named_target ~file_name ~extension ~name
        ^ (match transform_mode with Value -> ".default" | Module -> "")
  with Failure message -> Location.raise_errorf ~loc "%s" message

let extension_target ~loc ~file_name ~ext_name ~payload ~transform_mode =
  match extract_generated_extension_node ext_name with
  | None -> None
  | Some extension ->
      Some (target ~loc ~file_name ~extension ~payload ~transform_mode)

let transform_expr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_extension ({ txt = ext_name; loc }, payload)
    when is_generated_extension_node ext_name -> (
      match
        extension_target ~loc ~file_name:loc.loc_start.pos_fname ~ext_name ~payload
          ~transform_mode:Value
      with
      | None -> expr
      | Some target -> Ast_helper.Exp.ident ~loc { txt = Longident.parse target; loc })
  | _ -> expr

class mapper =
  object
    inherit Ast_traverse.map as super

    method! expression expr =
      match expr.Parsetree.pexp_desc with
      | Pexp_extension ({ txt = ext_name; _ }, _)
        when is_generated_extension_node ext_name ->
          transform_expr expr
      | _ -> super#expression expr

    method! structure_item structure_item =
      match structure_item.pstr_desc with
      | Pstr_include
          ({
             pincl_mod =
               ({ pmod_desc = Pmod_extension ({ txt = ext_name; loc }, payload); _ } as
               pmod);
             _;
           } as pincl)
        when is_generated_extension_node ext_name -> (
          match
            extension_target ~loc ~file_name:loc.loc_start.pos_fname ~ext_name
              ~payload ~transform_mode:Module
          with
          | None -> super#structure_item structure_item
          | Some target ->
              {
                structure_item with
                pstr_desc =
                  Pstr_include
                    {
                      pincl with
                      pincl_mod =
                        {
                          pmod with
                          pmod_desc =
                            Pmod_ident { txt = Longident.parse target; loc };
                        };
                    };
              })
      | Pstr_module
          ({
             pmb_expr =
               ({ pmod_desc = Pmod_extension ({ txt = ext_name; loc }, payload); _ } as
               pmod);
             _;
           } as pmb)
        when is_generated_extension_node ext_name -> (
          match
            extension_target ~loc ~file_name:loc.loc_start.pos_fname ~ext_name
              ~payload ~transform_mode:Module
          with
          | None -> super#structure_item structure_item
          | Some target ->
              {
                structure_item with
                pstr_desc =
                  Pstr_module
                    {
                      pmb with
                      pmb_expr =
                        {
                          pmod with
                          pmod_desc =
                            Pmod_ident { txt = Longident.parse target; loc };
                        };
                    };
              })
      | _ -> super#structure_item structure_item
  end

let structure_mapper structure =
  if !Utils.enableGenericTransform then (
    Hashtbl.reset transformed_count;
    (new mapper)#structure structure)
  else (new Ast_traverse.map)#structure structure
