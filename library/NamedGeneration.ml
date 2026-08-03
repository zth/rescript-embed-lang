type cardinality = Exactly_one | First
type capture = Numbered of int | Named of string

type generated_name =
  | Sequential
  | Graphql_definition
  | Name_directive
  | Name_directive_nested_block_comments
  | Regex of {
      pattern : string;
      flags : string;
      capture : capture;
      cardinality : cardinality;
    }

type config = { version : int; extensions : (string, generated_name) Hashtbl.t }

let config_version = 1
let config_path = ref None
let loaded_config = ref None
let regexp_cache = Hashtbl.create 8
let compiled_regexp_count_ref = ref 0
let compiled_regexp_count () = !compiled_regexp_count_ref

let set_config_path path =
  config_path := Some path;
  loaded_config := None

let member name json = Yojson.Safe.Util.member name json

let string_member name json =
  match member name json with
  | `String value -> value
  | _ -> failwith (Printf.sprintf "missing or invalid string field %S" name)

let int_member name json =
  match member name json with
  | `Int value -> value
  | _ -> failwith (Printf.sprintf "missing or invalid integer field %S" name)

let bool_member_default name ~default json =
  match member name json with
  | `Bool value -> value
  | `Null -> default
  | _ -> failwith (Printf.sprintf "invalid boolean field %S" name)

let parse_capture json =
  match string_member "kind" json with
  | "numbered" -> Numbered (int_member "index" json)
  | "named" -> Named (string_member "name" json)
  | kind -> failwith (Printf.sprintf "unsupported capture kind %S" kind)

let parse_cardinality = function
  | "exactlyOne" -> Exactly_one
  | "first" -> First
  | value -> failwith (Printf.sprintf "unsupported cardinality %S" value)

let parse_generated_name json =
  match string_member "kind" json with
  | "sequential" -> Sequential
  | "graphqlDefinition" -> Graphql_definition
  | "nameDirective" ->
      if bool_member_default "nestedBlockComments" ~default:false json then
        Name_directive_nested_block_comments
      else Name_directive
  | "regex" ->
      Regex
        {
          pattern = string_member "pattern" json;
          flags = string_member "flags" json;
          capture = parse_capture (member "capture" json);
          cardinality = parse_cardinality (string_member "cardinality" json);
        }
  | kind -> failwith (Printf.sprintf "unsupported generatedName kind %S" kind)

let load_config path =
  let json = Yojson.Safe.from_file path in
  let version = int_member "version" json in
  if version <> config_version then
    failwith
      (Printf.sprintf
         "unsupported embed-language config version %d (this PPX supports version %d)"
         version config_version);
  let extensions = Hashtbl.create 8 in
  Yojson.Safe.Util.to_assoc (member "extensions" json)
  |> List.iter (fun (extension, extension_json) ->
         Hashtbl.replace extensions extension
           (parse_generated_name (member "generatedName" extension_json)));
  { version; extensions }

let rec find_config_from directory relative_path =
  let candidate = Filename.concat directory relative_path in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None
    else find_config_from parent relative_path

let resolve_config_path ~source_file path =
  if not (Filename.is_relative path) then path
  else
    let from_cwd = Filename.concat (Sys.getcwd ()) path in
    if Sys.file_exists from_cwd then from_cwd
    else
      match find_config_from (Filename.dirname source_file) path with
      | Some resolved -> resolved
      | None ->
          failwith
            (Printf.sprintf
               "cannot resolve relative -embed-lang-config %S from the working directory, source file %S, or any parent directory"
               path source_file)

let get_config ~source_file =
  match !config_path with
  | None -> { version = config_version; extensions = Hashtbl.create 0 }
  | Some configured_path ->
      let resolved_path = resolve_config_path ~source_file configured_path in
      (match !loaded_config with
      | Some (loaded_path, config) when String.equal loaded_path resolved_path -> config
      | _ ->
          let config =
            try load_config resolved_path
            with exn ->
              failwith
                (Printf.sprintf "cannot load -embed-lang-config %S: %s" resolved_path
                   (Printexc.to_string exn))
          in
          loaded_config := Some (resolved_path, config);
          config)

let for_extension ~source_file extension =
  Hashtbl.find_opt (get_config ~source_file).extensions extension
  |> Option.value ~default:Sequential

let regexp_key pattern flags = pattern ^ "\000" ^ flags

let compile_regexp pattern flags =
  let key = regexp_key pattern flags in
  match Hashtbl.find_opt regexp_cache key with
  | Some regexp -> regexp
  | None -> (
      match Quickjs.RegExp.compile ~flags pattern with
      | Ok regexp ->
          Hashtbl.add regexp_cache key regexp;
          incr compiled_regexp_count_ref;
          regexp
      | Error error ->
          failwith
            (Printf.sprintf "invalid ECMAScript regex /%s/%s: %s" pattern flags
               (Quickjs.RegExp.compile_error_to_string error)))

let capture_description = function
  | Numbered index -> Printf.sprintf "numbered capture %d" index
  | Named name -> Printf.sprintf "named capture %S" name

let selected_capture capture (result : Quickjs.RegExp.match_result) =
  match capture with
  | Numbered index ->
      if index < 0 || index >= Array.length result.captures then None
      else result.captures.(index)
  | Named name -> Quickjs.RegExp.group name result

let flags_for_iteration flags =
  if String.contains flags 'g' || String.contains flags 'y' then flags
  else flags ^ "g"

let first_two_matches regexp source =
  Quickjs.RegExp.set_last_index regexp 0;
  let prepared = Quickjs.RegExp.prepare_input source in
  let rec loop acc =
    if List.length acc = 2 then List.rev acc
    else
      match Quickjs.RegExp.exec_prepared ~timeout_ms:100. regexp prepared with
      | None -> List.rev acc
      | Some prepared_match ->
          let start, end_ = prepared_match.range.utf16 in
          if start = end_ then
            Quickjs.RegExp.set_last_index regexp
              (Quickjs.RegExp.prepared_advance_index prepared
                 ~unicode:(Quickjs.RegExp.unicode regexp) end_);
          loop (prepared_match.result :: acc)
  in
  loop []

let validate_name name =
  let length = String.length name in
  let is_start = function
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  let is_continue = function
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  length > 0 && is_start name.[0]
  && String.for_all is_continue (String.sub name 1 (length - 1))

let is_name_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_name_continue = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let starts_with_at source offset prefix =
  let source_length = String.length source in
  let prefix_length = String.length prefix in
  offset + prefix_length <= source_length
  && String.sub source offset prefix_length = prefix

type graphql_token =
  | Name of string
  | Left_brace
  | Right_brace
  | Left_paren
  | Right_paren
  | Left_bracket
  | Right_bracket
  | Other

let graphql_tokens source =
  let length = String.length source in
  let rec skip_line_comment index =
    if index < length && source.[index] <> '\n' && source.[index] <> '\r' then
      skip_line_comment (index + 1)
    else index
  in
  let rec skip_quoted_string index escaped =
    if index >= length then index
    else if escaped then skip_quoted_string (index + 1) false
    else
      match source.[index] with
      | '\\' -> skip_quoted_string (index + 1) true
      | '"' -> index + 1
      | _ -> skip_quoted_string (index + 1) false
  in
  let rec skip_block_string index =
    if index >= length then index
    else if starts_with_at source index "\\\"\"\"" then
      skip_block_string (index + 4)
    else if starts_with_at source index "\"\"\"" then index + 3
    else skip_block_string (index + 1)
  in
  let rec name_end index =
    if index < length && is_name_continue source.[index] then name_end (index + 1)
    else index
  in
  let rec loop index tokens =
    if index >= length then List.rev tokens
    else
      match source.[index] with
      | character when is_whitespace character || character = ',' ->
          loop (index + 1) tokens
      | '#' -> loop (skip_line_comment (index + 1)) tokens
      | '"' when starts_with_at source index "\"\"\"" ->
          loop (skip_block_string (index + 3)) tokens
      | '"' -> loop (skip_quoted_string (index + 1) false) tokens
      | character when is_name_start character ->
          let end_ = name_end (index + 1) in
          loop end_ (Name (String.sub source index (end_ - index)) :: tokens)
      | '{' -> loop (index + 1) (Left_brace :: tokens)
      | '}' -> loop (index + 1) (Right_brace :: tokens)
      | '(' -> loop (index + 1) (Left_paren :: tokens)
      | ')' -> loop (index + 1) (Right_paren :: tokens)
      | '[' -> loop (index + 1) (Left_bracket :: tokens)
      | ']' -> loop (index + 1) (Right_bracket :: tokens)
      | _ -> loop (index + 1) (Other :: tokens)
  in
  loop 0 []

let extract_graphql_definition source =
  let operations = ref [] in
  let fragments = ref [] in
  let depth = ref 0 in
  let paren_depth = ref 0 in
  let bracket_depth = ref 0 in
  let awaiting_body = ref false in
  let rec loop = function
    | [] -> ()
    | Left_paren :: rest ->
        incr paren_depth;
        loop rest
    | Right_paren :: rest ->
        if !paren_depth > 0 then decr paren_depth;
        loop rest
    | Left_bracket :: rest ->
        incr bracket_depth;
        loop rest
    | Right_bracket :: rest ->
        if !bracket_depth > 0 then decr bracket_depth;
        loop rest
    | Left_brace :: rest
      when !depth = 0 && !paren_depth = 0 && !bracket_depth = 0 ->
        if not !awaiting_body then operations := None :: !operations;
        awaiting_body := false;
        depth := 1;
        loop rest
    | Left_brace :: rest when !depth > 0 ->
        incr depth;
        loop rest
    | Right_brace :: rest when !depth > 0 ->
        decr depth;
        loop rest
    | Name (("query" | "mutation" | "subscription") as _kind) :: rest
      when !depth = 0 && not !awaiting_body ->
        operations :=
          (match rest with Name name :: _ -> Some name | _ -> None) :: !operations;
        awaiting_body := true;
        loop rest
    | Name "fragment" :: rest when !depth = 0 && not !awaiting_body ->
        (match rest with
        | Name name :: _ when not (String.equal name "on") ->
            fragments := name :: !fragments
        | _ -> ());
        awaiting_body := true;
        loop rest
    | _ :: rest -> loop rest
  in
  loop (graphql_tokens source);
  match List.rev !operations with
  | [ Some name ] -> name
  | [ None ] ->
      failwith "GraphQL document contains an anonymous operation; add an operation name"
  | _ :: _ :: _ ->
      failwith "GraphQL document contains multiple operations; expected exactly one"
  | [] -> (
      match List.rev !fragments with
      | [ name ] -> name
      | [] -> failwith "GraphQL document contains no named operation or fragment"
      | _ ->
          failwith
            "GraphQL document contains multiple fragments and no operation; expected exactly one")

let names_in_comment comment =
  let length = String.length comment in
  let rec skip_whitespace index =
    if index < length && is_whitespace comment.[index] then skip_whitespace (index + 1)
    else index
  in
  let rec name_end index =
    if index < length && is_name_continue comment.[index] then name_end (index + 1)
    else index
  in
  let rec loop index names =
    if index >= length then List.rev names
    else if
      comment.[index] = '@'
      && starts_with_at comment (index + 1) "name"
      && (index = 0 || not (is_name_continue comment.[index - 1]))
      && (index + 5 >= length || is_whitespace comment.[index + 5])
    then
      let start = skip_whitespace (index + 5) in
      if start < length && is_name_start comment.[start] then
        let end_ = name_end (start + 1) in
        loop end_ (String.sub comment start (end_ - start) :: names)
      else loop (index + 5) names
    else loop (index + 1) names
  in
  loop 0 []

let extract_name_directive ~nested_block_comments source =
  let length = String.length source in
  let is_sql_identifier_continue character =
    is_name_continue character || Char.equal character '$' || Char.code character >= 128
  in
  let is_dollar_tag_start character = is_name_start character || Char.code character >= 128 in
  let is_dollar_tag_continue character =
    is_name_continue character || Char.code character >= 128
  in
  let is_sql_string_terminator = function
    | ',' | ';' | ')' | ']' | '}' -> true
    | _ -> false
  in
  let dollar_quote_delimiter index =
    let rec tag_end offset =
      if offset < length && is_dollar_tag_continue source.[offset] then tag_end (offset + 1)
      else offset
    in
    let end_ = tag_end (index + 1) in
    let has_valid_tag = end_ = index + 1 || is_dollar_tag_start source.[index + 1] in
    let has_token_boundary =
      index = 0 || not (is_sql_identifier_continue source.[index - 1])
    in
    if has_token_boundary && has_valid_tag && end_ < length && source.[end_] = '$' then
      Some (String.sub source index (end_ - index + 1), end_ + 1)
    else None
  in
  let rec skip_dollar_quoted index delimiter =
    if index >= length then index
    else if starts_with_at source index delimiter then index + String.length delimiter
    else skip_dollar_quoted (index + 1) delimiter
  in
  let rec has_single_quote_before_line_end index =
    index < length
    && not (Char.equal source.[index] '\n')
    && (Char.equal source.[index] '\'' || has_single_quote_before_line_end (index + 1))
  in
  let can_start_regex_literal index =
    let rec previous_significant offset =
      if offset < 0 then None
      else if is_whitespace source.[offset] then previous_significant (offset - 1)
      else Some source.[offset]
    in
    let follows_control_condition close_paren =
      let rec find_open offset depth =
        if offset < 0 then None
        else
          match source.[offset] with
          | ')' -> find_open (offset - 1) (depth + 1)
          | '(' when depth = 1 -> Some offset
          | '(' -> find_open (offset - 1) (depth - 1)
          | _ -> find_open (offset - 1) depth
      in
      match find_open (close_paren - 1) 1 with
      | None -> false
      | Some open_paren ->
          let rec previous_non_space offset =
            if offset >= 0 && is_whitespace source.[offset] then previous_non_space (offset - 1)
            else offset
          in
          let end_ = previous_non_space (open_paren - 1) + 1 in
          let rec word_start offset =
            if offset >= 0 && is_name_continue source.[offset] then word_start (offset - 1)
            else offset + 1
          in
          let start = word_start (end_ - 1) in
          List.mem (String.sub source start (end_ - start)) [ "if"; "while"; "for"; "with" ]
    in
    match previous_significant (index - 1) with
    | None -> true
    | Some (('+' | '-') as operator) -> (
        match previous_significant (index - 2) with
        | Some previous when Char.equal previous operator -> false
        | _ -> true)
    | Some ('=' | '(' | '[' | '{' | ',' | ':' | ';' | '!' | '&' | '|' | '?' | '*'
      | '%' | '^' | '~' | '<' | '>') -> true
    | Some ')' ->
        let rec previous_non_space offset =
          if is_whitespace source.[offset] then previous_non_space (offset - 1) else offset
        in
        follows_control_condition (previous_non_space (index - 1))
    | Some character when is_name_continue character ->
        let rec word_start offset =
          if offset >= 0 && is_name_continue source.[offset] then word_start (offset - 1)
          else offset + 1
        in
        let end_ =
          let rec last_significant offset =
            if is_whitespace source.[offset] then last_significant (offset - 1) else offset + 1
          in
          last_significant (index - 1)
        in
        let start = word_start (end_ - 1) in
        List.mem (String.sub source start (end_ - start))
          [
            "return";
            "throw";
            "case";
            "delete";
            "void";
            "typeof";
            "yield";
            "await";
            "new";
            "else";
            "do";
          ]
    | Some _ -> false
  in
  let rec skip_regex_literal index ~escaped ~in_class =
    if index >= length then index
    else if escaped then skip_regex_literal (index + 1) ~escaped:false ~in_class
    else
      match source.[index] with
      | '\\' -> skip_regex_literal (index + 1) ~escaped:true ~in_class
      | '[' -> skip_regex_literal (index + 1) ~escaped:false ~in_class:true
      | ']' -> skip_regex_literal (index + 1) ~escaped:false ~in_class:false
      | '/' when not in_class -> index + 1
      | _ -> skip_regex_literal (index + 1) ~escaped:false ~in_class
  in
  let rec skip_regex_flags index =
    if index < length && is_name_continue source.[index] then skip_regex_flags (index + 1)
    else index
  in
  let rec skip_quoted index quote ~backslash_escapes escaped =
    if index >= length then index
    else if escaped then skip_quoted (index + 1) quote ~backslash_escapes false
    else if
      Char.equal source.[index] '\\'
      && (backslash_escapes
         || (Char.equal quote '\''
            && index + 1 < length
            && Char.equal source.[index + 1] '\''
            && (index + 2 >= length || not (is_sql_string_terminator source.[index + 2]))
            && has_single_quote_before_line_end (index + 2)))
    then
      skip_quoted (index + 1) quote ~backslash_escapes true
    else if source.[index] = quote then
      if Char.equal quote '\'' && index + 1 < length && Char.equal source.[index + 1] '\''
      then skip_quoted (index + 2) quote ~backslash_escapes false
      else index + 1
    else skip_quoted (index + 1) quote ~backslash_escapes false
  in
  let rec line_end index =
    if index < length && source.[index] <> '\n' && source.[index] <> '\r' then
      line_end (index + 1)
    else index
  in
  let rec block_end index depth =
    if index >= length then (index, depth)
    else if nested_block_comments && starts_with_at source index "/*" then
      block_end (index + 2) (depth + 1)
    else if starts_with_at source index "*/" then
      if depth = 1 then (index, 0) else block_end (index + 2) (depth - 1)
    else block_end (index + 1) depth
  in
  let add_comment start end_ names =
    List.rev_append (names_in_comment (String.sub source start (end_ - start))) names
  in
  let rec loop index names template_depths =
    if index >= length then List.rev names
    else
      match template_depths with
      | 0 :: rest -> (
          match source.[index] with
          | '\\' -> loop (min (index + 2) length) names template_depths
          | '`' -> loop (index + 1) names rest
          | '$' when starts_with_at source index "${" ->
              loop (index + 2) names (1 :: rest)
          | _ -> loop (index + 1) names template_depths)
      | depth :: rest when Char.equal source.[index] '{' ->
          loop (index + 1) names ((depth + 1) :: rest)
      | depth :: rest when Char.equal source.[index] '}' ->
          loop (index + 1) names ((depth - 1) :: rest)
      | _ -> (
          match source.[index] with
          | '/'
            when not (starts_with_at source index "//")
                 && not (starts_with_at source index "/*")
                 && can_start_regex_literal index ->
              loop
                (skip_regex_flags
                   (skip_regex_literal (index + 1) ~escaped:false ~in_class:false))
                names template_depths
          | '$' -> (
              match dollar_quote_delimiter index with
              | Some (delimiter, content_start) ->
                  loop (skip_dollar_quoted content_start delimiter) names template_depths
              | None -> loop (index + 1) names template_depths)
          | '`' -> loop (index + 1) names (0 :: template_depths)
          | ('"' | '\'') as quote ->
              let backslash_escapes =
                not (Char.equal quote '\'')
                || (index > 0
                   && (Char.equal source.[index - 1] 'E'
                      || Char.equal source.[index - 1] 'e')
                   && (index = 1 || not (is_sql_identifier_continue source.[index - 2])))
              in
              loop
                (skip_quoted (index + 1) quote ~backslash_escapes false)
                names template_depths
          | '#'
            when index + 1 >= length
                 || (source.[index + 1] <> '>'
                    && source.[index + 1] <> '-'
                    && source.[index + 1] <> '#') ->
              let end_ = line_end (index + 1) in
              loop end_ (add_comment (index + 1) end_ names) template_depths
          | '/' when starts_with_at source index "//" ->
              let end_ = line_end (index + 2) in
              loop end_ (add_comment (index + 2) end_ names) template_depths
          | '-' when starts_with_at source index "--" ->
              let end_ = line_end (index + 2) in
              loop end_ (add_comment (index + 2) end_ names) template_depths
          | '/' when starts_with_at source index "/*" ->
              let end_, depth = block_end (index + 2) 1 in
              let next = if depth = 0 then end_ + 2 else end_ in
              loop next (add_comment (index + 2) end_ names) template_depths
          | _ -> loop (index + 1) names template_depths)
  in
  match loop 0 [] [] with
  | [ name ] -> name
  | [] -> failwith "no valid @name <identifier> directive was found in a comment"
  | _ ->
      failwith "multiple @name <identifier> directives were found; expected exactly one"

let extract_name ~extension ~source = function
  | Sequential -> None
  | Graphql_definition -> Some (extract_graphql_definition source)
  | Name_directive -> Some (extract_name_directive ~nested_block_comments:false source)
  | Name_directive_nested_block_comments ->
      Some (extract_name_directive ~nested_block_comments:true source)
  | Regex { pattern; flags; capture; cardinality } ->
      let regexp =
        compile_regexp pattern
          (match cardinality with First -> flags | Exactly_one -> flags_for_iteration flags)
      in
      let matches =
        try
          match cardinality with
          | First ->
              Quickjs.RegExp.set_last_index regexp 0;
              (match Quickjs.RegExp.exec ~timeout_ms:100. regexp source with
              | None -> []
              | Some result -> [ result ])
          | Exactly_one -> first_two_matches regexp source
        with Quickjs.RegExp.Timeout ->
          failwith
            (Printf.sprintf "generated-name regex /%s/%s timed out after 100ms" pattern
               flags)
      in
      let result =
        match (cardinality, matches) with
        | _, [] ->
            failwith
              (Printf.sprintf "extension %S: generated-name regex /%s/%s did not match"
                 extension pattern flags)
        | Exactly_one, _ :: _ :: _ ->
            failwith
              (Printf.sprintf
                 "extension %S: generated-name regex /%s/%s matched more than once"
                 extension pattern flags)
        | _, result :: _ -> result
      in
      let name =
        match selected_capture capture result with
        | None ->
            failwith
              (Printf.sprintf "extension %S: %s was missing for regex /%s/%s"
                 extension (capture_description capture) pattern flags)
        | Some "" ->
            failwith
              (Printf.sprintf "extension %S: %s was empty for regex /%s/%s"
                 extension (capture_description capture) pattern flags)
        | Some name -> name
      in
      if not (validate_name name) then
        failwith
          (Printf.sprintf
             "extension %S: extracted generated name %S is invalid; expected [_A-Za-z][_0-9A-Za-z]* (%s from /%s/%s)"
             extension name (capture_description capture) pattern flags);
      Some name

let source_module file_name =
  let base = Filename.basename file_name in
  if Filename.check_suffix base ".res" then Filename.chop_suffix base ".res"
  else if Filename.check_suffix base ".resi" then Filename.chop_suffix base ".resi"
  else base

let named_target ~file_name ~extension ~name =
  Printf.sprintf "%s__%s__%s" (source_module file_name) extension name
