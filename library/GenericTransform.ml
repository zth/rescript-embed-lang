open Ppxlib
open Ast_helper
open Utils

let isGeneratedExtensionNode name = String.starts_with ~prefix:"generated." name

let extractGeneratedExtensionNode str =
  match String.split_on_char '.' str with
  | ["generated"; tag] -> Some tag
  | _ -> None

let transformedCount = Hashtbl.create 10

let incrementTransformedCount (extName : string) =
  match Hashtbl.find_opt transformedCount extName with
  | None -> Hashtbl.add transformedCount extName 1
  | Some count -> Hashtbl.replace transformedCount extName (count + 1)

let getTransformedCount extName =
  match Hashtbl.find_opt transformedCount extName with
  | None -> 0
  | Some count -> count

type transformMode = LetBinding | ModuleBinding

let makeLident fileName ~extensionName ~transformMode =
  Longident.parse
    (Printf.sprintf "%s__%s.M%i%s"
       Filename.(remove_extension (basename fileName))
       extensionName
       (getTransformedCount extensionName)
       (match transformMode with
       | LetBinding -> ".default"
       | ModuleBinding -> ""))

let transformExpr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_extension
      ( {txt = extName},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {pexp_desc = Pexp_constant (Pconst_string (contents, _, _))},
                    _ );
            };
          ] )
    when extName |> isGeneratedExtensionNode -> (
    match extractGeneratedExtensionNode extName with
    | None -> expr
    | Some extensionName ->
      incrementTransformedCount extensionName;
      let loc = expr.pexp_loc in
      let fileName = loc.loc_start.pos_fname in
      let lid = makeLident fileName ~extensionName ~transformMode:LetBinding in
      Ast_helper.Exp.ident ~loc {txt = lid; loc})
  | _ -> expr

class mapper =
  object (self)
    inherit Ast_traverse.map
    method! structure_item structure_item =
      match structure_item.pstr_desc with
      | Pstr_value
          ( recFlag,
            [
              ({
                 pvb_expr =
                   {pexp_desc = Pexp_extension ({txt = extName}, _)} as expr;
               } as valueBinding);
            ] )
        when extName |> isGeneratedExtensionNode -> (
        match extractGeneratedExtensionNode extName with
        | None -> structure_item
        | Some extensionName ->
          {
            structure_item with
            pstr_desc =
              Pstr_value
                (recFlag, [{valueBinding with pvb_expr = transformExpr expr}]);
          })
      | Pstr_module
          ({
             pmb_expr =
               {pmod_desc = Pmod_extension ({txt = extName; loc}, _)} as pmod;
           } as pmb)
        when extName |> isGeneratedExtensionNode -> (
        match extractGeneratedExtensionNode extName with
        | None -> structure_item
        | Some extensionName ->
          incrementTransformedCount extensionName;
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
                        Pmod_ident
                          {
                            txt =
                              makeLident loc.loc_start.pos_fname ~extensionName
                                ~transformMode:ModuleBinding;
                            loc;
                          };
                    };
                };
          })
      | _ -> structure_item
  end

let structure_mapper s =
  if !Utils.enableGenericTransform then (new mapper)#structure s
  else (new Ast_traverse.map)#structure s
