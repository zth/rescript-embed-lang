open Ppxlib

let fail format = Printf.ksprintf failwith format

let rec module_name = function
  | Longident.Lident name -> name
  | Longident.Ldot (parent, name) -> module_name parent ^ "." ^ name
  | Longident.Lapply _ -> fail "unexpected functor application in module target"

let transform file source =
  RescriptEmbedLangLibrary.Utils.enableGenericTransform := false;
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf file;
  Parse.implementation lexbuf |> Driver.map_structure

let module_targets structure =
  let targets = ref [] in
  let iterator =
    object
      inherit Ast_traverse.iter as super

      method! module_binding binding =
        (match (binding.pmb_name.txt, binding.pmb_expr.pmod_desc) with
        | Some name, Pmod_ident {txt; _} ->
          targets := (name, module_name txt) :: !targets
        | _ -> ());
        super#module_binding binding
    end
  in
  iterator#structure structure;
  List.rev !targets

let expect_targets file source expected =
  let actual = transform file source |> module_targets in
  if actual <> expected then fail "unexpected SQL module targets in %s" file

let test_metadata () =
  match
    transform "src/seasonSql.res"
      "module Active = ([%sql.fragment \"select 1\"] [@kept]) [@@binding]"
  with
  | [{pstr_desc = Pstr_module binding; _}] ->
    if
      List.map (fun attr -> attr.attr_name.txt) binding.pmb_attributes
      <> ["binding"]
    then fail "module binding attributes lost";
    if
      List.map (fun attr -> attr.attr_name.txt) binding.pmb_expr.pmod_attributes
      <> ["kept"]
    then fail "module expression attributes lost";
    if binding.pmb_expr.pmod_loc.loc_start.pos_fname <> "src/seasonSql.res" then
      fail "authored source location lost"
  | _ -> fail "unexpected transformed module"

let reject payload =
  try
    ignore
      (transform "src/Bad.res" ("module Bad = [%sql.fragment " ^ payload ^ "]"));
    fail "invalid fragment accepted: %s" payload
  with Location.Error _ -> ()

let () =
  expect_targets "src/seasonSql.res"
    "module Active = [%sql.fragment \"select 1\"]"
    [("Active", "SeasonSql__fragments__sql.Active")];
  expect_targets "src/Shared.res"
    "module Outer = struct module Inner = struct module Value = [%sql.fragment \
     \"select 2\"] end end\n\
    \     module Sibling = [%sql.fragment \"select 3\"]"
    [
      ("Value", "Shared__fragments__sql.Outer.Inner.Value");
      ("Sibling", "Shared__fragments__sql.Sibling");
    ];
  expect_targets "src/Other.res" "module Value = [%sql.fragment \"select 4\"]"
    [("Value", "Other__fragments__sql.Value")];
  expect_targets "src/Queries.res"
    "module Find = [%sql \"/* @name Find */ select 1\"] module Ordinary = \
     Existing"
    [("Find", "Queries__sql.Find"); ("Ordinary", "Existing")];
  test_metadata ();
  List.iter reject [""; "42"; "not_a_literal"; "\"select 1\"; \"select 2\""];
  expect_targets "src/Recovered.res"
    "module Value = [%sql.fragment \"select 5\"]"
    [("Value", "Recovered__fragments__sql.Value")];
  Printf.printf
    "native SQL fragments: mapping, scope, metadata and invalid payloads ok\n"
