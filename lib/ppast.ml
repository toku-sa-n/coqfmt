open Printer
open Printer.Wrap
open Printer.Lineup
open Printer.Str
open Ltac_plugin

exception NotImplemented of string

(** Returns a `true` if printing the given [Constrexpr.constr_expr_r] needs
      parentheses IN ANY TIMES.

    In some cases (e.g., printing a notation), printing parentheses may be
    needed even if this function returns a `false`, but do not modify this
    function to return a `true` in such cases. *)
let exprs_generally_parens_needed = function
  | Constrexpr.CApp _ | Constrexpr.CIf _ | Constrexpr.CLambdaN _
  | Constrexpr.CProdN _ ->
      true
  | Constrexpr.CNotation (_, (_, notation), _) ->
      let parts = String.split_on_char ' ' notation in
      let enclosed_by a b =
        List.hd parts |> String.starts_with ~prefix:a
        && List.rev parts |> List.hd |> String.ends_with ~suffix:b
      in
      let enclosed =
        enclosed_by "(" ")" || enclosed_by "{" "}" || enclosed_by "<" ">"
        || enclosed_by "[" "]"
      in

      List.length parts > 0 && not enclosed
  | Constrexpr.CAppExpl ((name, None), [ _ ]) ->
      Libnames.string_of_qualid name <> ".."
  | _ -> false

let tactics_generally_parens_needed = function
  | Tacexpr.TacThen _ | Tacexpr.TacThens _ -> true
  | _ -> false

let nop _ = ()
let pp_int n = write (string_of_int n)
let pp_id id = write (Names.Id.to_string id)
let pp_lident CAst.{ v; loc = _ } = pp_id v

let pp_name = function
  | Names.Name name -> pp_id name
  | Names.Anonymous -> write "_"

let pp_lname CAst.{ v; loc = _ } = pp_name v
let pp_lstring CAst.{ v; loc = _ } = write v

let pp_definition_object_kind = function
  | Decls.Coercion -> write "Coercion"
  | Decls.Definition -> write "Definition"
  | Decls.Example -> write "Example"
  | Decls.SubClass -> write "SubClass"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sign = function NumTok.SPlus -> nop | NumTok.SMinus -> write "-"
let pp_unsigned n = write (NumTok.Unsigned.sprint n)
let pp_signed (sign, n) = sequence [ pp_sign sign; pp_unsigned n ]

let pp_prim_token = function
  | Constrexpr.Number n -> pp_signed n
  | Constrexpr.String s -> doublequoted (write s)

let pp_qualid id = write (Libnames.string_of_qualid id)

let rec pp_cases_pattern_expr CAst.{ v; loc = _ } = pp_cases_pattern_expr_r v

and pp_cases_pattern_expr_r = function
  | Constrexpr.CPatAtom (Some id) -> pp_qualid id
  | Constrexpr.CPatAtom None -> write "_"
  (* Cstr seems to mean 'Constructor'. (e.g., `S (S O)`, `Foo 0 1`) *)
  | Constrexpr.CPatCstr (outer, None, values) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CPatAtom _ | Constrexpr.CPatPrim _ ->
            pp_cases_pattern_expr expr
        | _ -> parens (pp_cases_pattern_expr expr)
      in
      spaced (pp_qualid outer :: List.map conditional_parens values)
  | Constrexpr.CPatNotation (scope, notation, (exprs_1, exprs_2, []), []) ->
      let printing_rule = Ppextend.find_notation_printing_rule scope notation in

      let rec pp unparsings exprs =
        match (unparsings, exprs) with
        | [], _ -> nop
        | Ppextend.UnpMetaVar _ :: _, [] -> failwith "Too few exprs."
        | Ppextend.UnpMetaVar _ :: t, h_exprs :: t_exprs ->
            sequence [ pp_cases_pattern_expr h_exprs; pp t t_exprs ]
        | Ppextend.UnpBinderMetaVar _ :: _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpListMetaVar _ :: _, [] -> failwith "Too few exprs."
        | Ppextend.UnpListMetaVar (_, seps) :: t, elems ->
            let get_sep = function
              | Ppextend.UnpTerminal s -> s
              | _ -> failwith "TODO"
            in

            let rec loop elems seps =
              match (elems, seps) with
              | [ x ], _ -> sequence [ pp_cases_pattern_expr x; pp t [] ]
              | [], _ -> pp t []
              | xs, [] ->
                  sequence
                    [
                      map_with_seps ~sep:(write "; ") pp_cases_pattern_expr xs;
                      pp t [];
                    ]
              | x :: xs, sep :: seps ->
                  sequence
                    [
                      pp_cases_pattern_expr x;
                      write (get_sep sep);
                      space;
                      loop xs seps;
                    ]
            in

            loop elems seps
        | Ppextend.UnpBinderListMetaVar _ :: _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpTerminal s :: t, _ -> sequence [ write s; pp t exprs ]
        | Ppextend.UnpCut _ :: t, _ -> sequence [ space; pp t exprs ]
        | Ppextend.UnpBox (_, xs) :: t, _ -> pp (List.map snd xs @ t) exprs
      in

      pp printing_rule.notation_printing_unparsing
        (exprs_1 @ List.flatten exprs_2)
  | Constrexpr.CPatPrim token -> pp_prim_token token
  | Constrexpr.CPatOr xs -> parens (map_bard pp_cases_pattern_expr xs)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sort_name_expr = function
  | Constrexpr.CProp -> write "Prop"
  | Constrexpr.CSet -> write "Set"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sort_expr = function
  | Glob_term.UAnonymous { rigid = UnivRigid } -> write "Type"
  | Glob_term.UAnonymous { rigid = _ } ->
      fun printer -> raise (NotImplemented (contents printer))
  | Glob_term.UNamed (None, [ (sort, 0) ]) -> pp_sort_name_expr sort
  | Glob_term.UNamed _ ->
      fun printer -> raise (NotImplemented (contents printer))

let rec pp_constr_expr CAst.{ v; loc = _ } = pp_constr_expr_r v

and pp_constr_expr_r = function
  | Constrexpr.CApp (fn, args) ->
      let pp_args =
        let hor =
          map_sequence
            (function
              | inner, None ->
                  sequence [ space; pp_constr_expr_with_parens inner ]
              | _, Some _ ->
                  fun printer -> raise (NotImplemented (contents printer)))
            args
        in
        let ver =
          map_sequence
            (function
              | inner, None ->
                  sequence
                    [ newline; indented (pp_constr_expr_with_parens inner) ]
              | _, Some _ ->
                  fun printer -> raise (NotImplemented (contents printer)))
            args
        in

        hor <-|> ver
      in

      sequence [ pp_constr_expr_with_parens fn; pp_args ]
  | Constrexpr.CAppExpl ((name, None), []) ->
      sequence [ write "@"; pp_qualid name ]
  | Constrexpr.CAppExpl ((dots, None), [ expr ])
    when Libnames.string_of_qualid dots = ".." ->
      spaced [ pp_qualid dots; parens (pp_constr_expr expr); pp_qualid dots ]
  | Constrexpr.CAppExpl ((name, None), exprs) ->
      sequence
        [ write "@"; pp_qualid name; space; map_spaced pp_constr_expr exprs ]
  | Constrexpr.CCases (_, None, matchees, branches) ->
      sequence
        [
          write "match ";
          map_commad pp_case_expr matchees;
          write " with";
          newline;
          map_sequence
            (fun branch -> sequence [ pp_branch_expr branch; newline ])
            branches;
          write "end";
        ]
  | Constrexpr.CCast (v, Some DEFAULTcast, t) ->
      sequence [ pp_constr_expr_with_parens v; write " : "; pp_constr_expr t ]
  | Constrexpr.CDelimiters (DelimUnboundedScope, scope, expr) ->
      sequence [ pp_constr_expr_with_parens expr; write "%"; write scope ]
  | Constrexpr.CEvar (term, []) -> sequence [ write "?"; pp_id term.v ]
  | Constrexpr.CFix (_, body) -> sequence [ write "fix "; pp_fix_expr body ]
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      sequence
        [
          write "if ";
          pp_constr_expr cond;
          newline;
          indented
            (lined
               [
                 write "then " |=> pp_constr_expr t;
                 write "else " |=> pp_constr_expr f;
               ]);
        ]
  | Constrexpr.CLambdaN (args, body) ->
      let pp_fun_and_parameters =
        let hor =
          sequence [ write "fun "; map_spaced pp_local_binder_expr args ]
        in
        let ver =
          sequence
            [
              write "fun";
              newline;
              indented (map_lined pp_local_binder_expr args);
            ]
        in
        hor <-|> ver
      in

      let pp_body =
        let hor = sequence [ space; pp_constr_expr body ] in
        let ver = sequence [ newline; indented (pp_constr_expr body) ] in
        hor <-|> ver
      in

      sequence [ pp_fun_and_parameters; write " =>"; pp_body ]
  | Constrexpr.CLetIn (name, binding, None, expr) ->
      sequence
        [
          write "let ";
          pp_lname name;
          write " := ";
          pp_constr_expr binding;
          write " in";
          newline;
          pp_constr_expr expr;
        ]
  | Constrexpr.CRef (id, None) -> pp_qualid id
  | Constrexpr.CNotation
      ( scope,
        notation,
        (init_replacers_nonrec, init_replacers_rec, init_patterns, local_assums)
      ) as op ->
      let open Ppextend in
      let open CAst in
      let notation_info = Notgram_ops.grammar_of_notation notation |> List.hd in

      let init_replacers =
        init_replacers_nonrec @ List.flatten init_replacers_rec
      in

      let assoc = notation_info.notgram_assoc in

      let entry_keys = notation_info.notgram_typs in

      let op_level = function
        | Constrexpr.CNotation (_, notation, _) ->
            Some (Notation.level_of_notation notation)
        | _ -> None
      in

      let printing_rule = Ppextend.find_notation_printing_rule scope notation in

      (* The list printing rule is unusual, with [Ppextend.UnpListMetaVar]
         excluding the first element and a [Ppextend.UnpCut] between the first
         and the rest. This complicates pretty-printing using [pp_generic],
         necessitating a specialized function for lists.*)
      let pp_list elems =
        let rec loop tactics is_first printer =
          match (tactics, is_first) with
          | [], _ -> nop printer
          | [ h ], true -> pp_constr_expr h printer
          | [ h ], false -> (
              let pp = pp_constr_expr h in

              match can_pp_oneline (sequence [ space; pp ]) printer with
              | true -> sequence [ space; pp ] printer
              | false -> sequence [ newline; pp ] printer)
          | h :: t, true ->
              sequence [ pp_constr_expr h; write ";"; loop t false ] printer
          | h :: t, false -> (
              let pp = sequence [ pp_constr_expr h; write ";" ] in

              match can_pp_oneline (sequence [ space; pp ]) printer with
              | true -> sequence [ space; pp; loop t false ] printer
              | false -> sequence [ newline; pp; loop t false ] printer)
        in

        brackets (loop elems true)
      in

      let rec pp_generic unparsings replacers local_assums entry_keys patterns =
        match (unparsings, replacers, local_assums, entry_keys, patterns) with
        | [], [], [], [], [] -> nop
        | [], _ :: _, _, _, _ -> failwith "Too many replacers."
        | [], _, _ :: _, _, _ -> failwith "Too many local assumptions."
        | [], _, _, _ :: _, _ -> failwith "Too many entry keys."
        | [], _, _, _, _ :: _ -> failwith "Too many patterns."
        | ( Ppextend.UnpMetaVar { notation_position; _ } :: t_u,
            h :: t_r,
            _,
            h_keys :: t_keys,
            _ ) ->
            (* CProdN denotes a `forall foo, ... ` value. This value needs to be
               enclosed by parentheses if it is not on the rightmost position,
               otherwise all expressions will be in the scope of the `forall
               foo`.

               For example, `(forall x, f x = x) -> (forall x, f x = x)` is
               valid, but `forall x, f x = x -> forall x, f x = x` will be
               interpreted as `forall x, (f x = x -> forall x, f x = x).` which
               is invalid. Certainly, these two have different meanings, and
               thus lhs' `forall` needs parentheses.*)
            let parens_needed =
              match (h.v, assoc, notation_position, h_keys) with
              | Constrexpr.CLambdaN _, _, _, _ -> true
              | Constrexpr.CNotation (_, (_, "( _ )"), _), _, _, _ -> false
              | Constrexpr.CNotation (_, (_, "_ -> _"), _), _, Some Left, _
                when snd notation = "_ -> _" ->
                  op_level h.v >= op_level op
              | Constrexpr.CNotation (_, (_, "_ -> _"), _), _, Some Right, _
              | Constrexpr.CNotation (_, (_, "_ -> _"), _), _, None, _
                when snd notation = "_ -> _" ->
                  op_level h.v > op_level op
              | Constrexpr.CProdN _, _, _, _ -> t_u <> []
              | _, Some LeftA, Some Right, _ | _, Some RightA, Some Left, _ ->
                  op_level h.v >= op_level op
              | _, Some LeftA, Some Left, _
              | _, Some LeftA, None, _
              | _, Some RightA, Some Right, _
              | _, Some RightA, None, _ ->
                  op_level h.v > op_level op
              | ( _,
                  None,
                  None,
                  Extend.ETConstr
                    ( Constrexpr.InConstrEntry,
                      None,
                      (Extend.NumLevel _, Extend.BorderProd (Right, None)) ) )
                ->
                  op_level h.v > op_level op
              | _ ->
                  List.length init_replacers <> 1 && op_level h.v >= op_level op
            in
            sequence
              [
                pp_constr_expr_with_parens_conditionally parens_needed h;
                pp_generic t_u t_r local_assums t_keys patterns;
              ]
        | Ppextend.UnpMetaVar _ :: _, _, _, _, _ ->
            failwith "Too few replacers."
        | Ppextend.UnpBinderMetaVar _ :: t, _, _, _ :: keys, (h_p, _) :: t_p ->
            sequence
              [
                pp_cases_pattern_expr h_p;
                pp_generic t replacers local_assums keys t_p;
              ]
        | Ppextend.UnpBinderMetaVar _ :: _, _, _, _, _ ->
            failwith "Too few entry keys."
        | Ppextend.UnpListMetaVar (_, seps) :: t, elems, _, _ :: zs, _ ->
            let sep =
              let cond = function
                | Ppextend.UnpTerminal s -> Some s
                | _ -> None
              in

              match List.find_map cond seps with Some s -> s | None -> ""
            in

            let loop elems =
              match elems with
              | [ x ] ->
                  sequence
                    [
                      pp_constr_expr x; pp_generic t [] local_assums zs patterns;
                    ]
              | [] -> pp_generic t [] local_assums zs patterns
              | xs ->
                  sequence
                    [
                      map_with_seps ~sep:(write (sep ^ " ")) pp_constr_expr xs;
                      pp_generic t [] local_assums zs patterns;
                    ]
            in
            loop elems
        | Ppextend.UnpListMetaVar (_, _) :: _, _, _, [], _ ->
            raise (NotImplemented "")
        | Ppextend.UnpBinderListMetaVar _ :: _, _, _, [], _ ->
            raise (NotImplemented "Too few entry keys.")
        | ( Ppextend.UnpBinderListMetaVar (true, true, [ _ ]) :: t,
            xs,
            assums,
            _ :: keys,
            _ ) ->
            sequence
              [
                map_spaced pp_local_binder_expr assums;
                pp_generic t xs [] keys patterns;
              ]
        | Ppextend.UnpBinderListMetaVar _ :: _, _, _, _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpTerminal s :: t, xs, _, keys, _ ->
            sequence [ write s; pp_generic t xs local_assums keys patterns ]
        | Ppextend.UnpBox (_, xs) :: t, _, _, keys, _ ->
            pp_generic
              (List.map snd xs @ t)
              replacers local_assums keys patterns
        | Ppextend.UnpCut _ :: t, xs, _, keys, _ ->
            let hor =
              sequence [ space; pp_generic t xs local_assums keys patterns ]
            in
            let ver =
              sequence [ newline; pp_generic t xs local_assums keys patterns ]
            in
            hor <-|> ver
      in

      if snd notation = "[ _ ; _ ; .. ; _ ]" then pp_list init_replacers
      else
        pp_generic printing_rule.notation_printing_unparsing init_replacers
          (List.flatten local_assums)
          entry_keys init_patterns
  | Constrexpr.CPrim prim -> pp_prim_token prim
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      map_spaced pp_local_binder_expr xs
  | Constrexpr.CProdN (xs, ty) ->
      let pp_parameters =
        let hor =
          map_sequence (fun x -> sequence [ space; pp_local_binder_expr x ]) xs
        in
        let ver =
          map_sequence
            (fun x -> sequence [ newline; indented (pp_local_binder_expr x) ])
            xs
        in
        hor <-|> ver
      in

      let pp_body =
        let hor = sequence [ space; pp_constr_expr ty ] in
        let ver = sequence [ newline; indented (pp_constr_expr ty) ] in
        hor <-|> ver
      in

      sequence [ write "forall"; pp_parameters; write ","; pp_body ]
  | Constrexpr.CHole None -> write "_"
  | Constrexpr.CSort expr -> pp_sort_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_constr_expr_with_parens_conditionally cond expr =
  if cond then parens (pp_constr_expr expr) else pp_constr_expr expr

and pp_constr_expr_with_parens expr =
  pp_constr_expr_with_parens_conditionally
    (exprs_generally_parens_needed expr.CAst.v)
    expr

and pp_case_expr = function
  | expr, None, None -> pp_constr_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_local_binder_expr = function
  | Constrexpr.CLocalAssum
      ( [ name ],
        Constrexpr.Default Explicit,
        CAst.{ v = Constrexpr.CHole _; loc = _ } ) ->
      pp_lname name
  | Constrexpr.CLocalAssum (names, Constrexpr.Default kind, ty) ->
      let wrapper =
        match kind with
        | Explicit -> parens
        | MaxImplicit -> braces
        | _ -> fun _ printer -> raise (NotImplemented (contents printer))
      in

      wrapper
        (sequence [ map_spaced pp_lname names; write " : "; pp_constr_expr ty ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_recursion_order_expr CAst.{ v; loc = _ } = pp_recursion_order_expr_r v

and pp_recursion_order_expr_r = function
  | Constrexpr.CStructRec name -> sequence [ write "struct "; pp_lident name ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_branch_expr = function
  | CAst.{ v = patterns, expr; loc = _ } ->
      let hor = sequence [ space; pp_constr_expr expr ] in
      let ver = sequence [ newline; indented (pp_constr_expr expr) ] in
      sequence
        [
          write "| ";
          map_bard (map_commad pp_cases_pattern_expr) patterns;
          write " =>";
          hor <-|> ver;
        ]

and pp_fix_expr = function
  | [ (name, None, bindings, return_type, body) ] ->
      let open CAst in
      let pp_return_type =
        match return_type.v with
        | Constrexpr.CHole None -> nop
        | _ -> sequence [ write " : "; pp_constr_expr return_type ]
      in

      sequence
        [
          pp_lident name;
          map_sequence
            (fun x -> sequence [ space; pp_local_binder_expr x ])
            bindings;
          pp_return_type;
          write " := ";
          pp_constr_expr body;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_definition_expr = function
  | Vernacexpr.ProveBody (args, expr) ->
      let hor = sequence [ space; pp_constr_expr expr ] in
      let ver = sequence [ newline; indented (pp_constr_expr expr) ] in
      sequence
        [
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args;
          write " :";
          hor <-|> ver;
        ]
  | Vernacexpr.DefineBody (args, None, def_body, return_ty) ->
      let pp_args =
        let hor =
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args
        in
        let ver =
          map_sequence
            (fun arg ->
              sequence [ newline; indented (pp_local_binder_expr arg) ])
            args
        in

        hor <-|> ver
      in

      let pp_return_ty =
        match return_ty with
        | None -> nop
        | Some ty ->
            let hor = sequence [ write " : "; pp_constr_expr ty ] in
            let ver =
              sequence
                [
                  newline; indented (sequence [ write ": "; pp_constr_expr ty ]);
                ]
            in

            hor <-|> ver
      in

      let pp_body =
        let hor = sequence [ space; pp_constr_expr def_body ] in
        let ver = sequence [ newline; indented (pp_constr_expr def_body) ] in

        hor <-|> ver
      in

      sequence [ pp_args; pp_return_ty; write " :="; pp_body ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_proof_end = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, Some ident) ->
      sequence [ clear_bullets; write "Save"; space; pp_lident ident; dot ]
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) ->
      sequence [ clear_bullets; write "Qed." ]
  | Vernacexpr.Proved (Vernacexpr.Transparent, Some ident) ->
      sequence [ clear_bullets; write "Defined"; space; pp_lident ident; dot ]
  | Vernacexpr.Proved (Vernacexpr.Transparent, None) ->
      sequence [ clear_bullets; write "Defined." ]
  | Vernacexpr.Admitted -> sequence [ clear_bullets; write "Admitted." ]

let pp_theorem_kind = function
  | Decls.Fact -> write "Fact"
  | Decls.Lemma -> write "Lemma"
  | Decls.Theorem -> write "Theorem"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_fixpoint_expr = function
  | Vernacexpr.
      {
        fname;
        univs = None;
        rec_order;
        binders;
        rtype;
        body_def;
        notations = [];
      } ->
      let pp_binders =
        let hor =
          map_sequence
            (fun x -> sequence [ space; pp_local_binder_expr x ])
            binders
        in
        let ver =
          map_sequence
            (fun x -> sequence [ newline; indented (pp_local_binder_expr x) ])
            binders
        in
        hor <-|> ver
      in

      let pp_rec =
        match rec_order with
        | None -> nop
        | Some x -> sequence [ space; braces (pp_recursion_order_expr x) ]
      in

      let pp_return_type =
        match rtype.v with
        | Constrexpr.CHole None -> nop
        | _ -> sequence [ write " : "; pp_constr_expr rtype ]
      in

      let pp_body =
        match body_def with
        | Some body ->
            sequence [ write " :="; newline; indented (pp_constr_expr body) ]
        | None -> nop
      in

      sequence [ pp_lident fname; pp_binders; pp_rec; pp_return_type; pp_body ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_constructor_expr = function
  | ([], Vernacexpr.NoCoercion, Vernacexpr.NoInstance), (name, expr) -> (
      let open CAst in
      (* TODO: Implement this completely. An `Inductive` is an inductive prop if
         it contains a `CRef` with its name. *)
      let rec is_inductive_prop = function
        | Constrexpr.CApp _ -> true
        | Constrexpr.CRef _ -> true
        | Constrexpr.CProdN (_, x) -> is_inductive_prop x.v
        | Constrexpr.CNotation (None, _, (xs, [], [], [])) ->
            List.exists (fun x -> is_inductive_prop x.v) xs
        | _ -> false
      in

      match expr.v with
      | Constrexpr.CHole _ -> sequence [ newline; write "| "; pp_lident name ]
      | _ when is_inductive_prop expr.v ->
          let pp_type =
            let hor = sequence [ space; pp_constr_expr expr ] in
            let ver = sequence [ newline; indented (pp_constr_expr expr) ] in
            hor <-|> ver
          in

          sequence [ newline; write "| " ]
          |=> sequence [ pp_lident name; write " :"; pp_type ]
      | _ ->
          sequence
            [ newline; write "| "; pp_lident name; space; pp_constr_expr expr ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_production_level = function
  | Extend.NextLevel -> write "at next level"
  | Extend.NumLevel n -> sequence [ write "at level "; pp_int n ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_notation_entry = function
  | Constrexpr.InConstrEntry -> write "constr"
  | Constrexpr.InCustomEntry scope ->
      sequence [ write "custom"; space; write scope ]

let pp_syntax_modifier = function
  | Vernacexpr.SetAssoc LeftA -> write "left associativity"
  | Vernacexpr.SetAssoc RightA -> write "right associativity"
  | Vernacexpr.SetAssoc NonA -> write "no associativity"
  | Vernacexpr.SetCustomEntry (name, level) ->
      let pp_level =
        match level with
        | None -> nop
        | Some level -> sequence [ write " at level "; pp_int level ]
      in

      sequence [ write "in custom "; write name; pp_level ]
  | Vernacexpr.SetEntryType (name, ETConstr (entry, None, DefaultLevel)) ->
      spaced [ write name; pp_notation_entry entry ]
  | Vernacexpr.SetEntryType (name, ETConstr (entry, None, level)) ->
      spaced [ write name; pp_notation_entry entry; pp_production_level level ]
  | Vernacexpr.SetEntryType (name, ETPattern (false, None)) ->
      sequence [ write name; write " pattern" ]
  | Vernacexpr.SetItemLevel ([ name ], None, level) ->
      sequence [ write name; space; pp_production_level level ]
  | Vernacexpr.SetLevel level ->
      sequence [ write "at level "; write (string_of_int level) ]
  | Vernacexpr.SetOnlyParsing -> write "only parsing"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_gen_tactic_arg = function
  | Tacexpr.ConstrMayEval (ConstrTerm expr) -> pp_constr_expr expr
  | Tacexpr.Reference name -> pp_qualid name
  | Tacexpr.TacCall CAst.{ v = name, args; loc = _ } ->
      let pp_args =
        map_sequence (fun x -> sequence [ space; pp_gen_tactic_arg x ]) args
      in

      sequence [ pp_qualid name; pp_args ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_raw_red_expr = function
  | Genredexpr.Cbv
      Genredexpr.
        {
          rStrength = Norm;
          rBeta = true;
          rMatch = true;
          rFix = true;
          rCofix = true;
          rZeta = true;
          rDelta = true;
          rConst = [];
        } ->
      write "cbv"
  | Genredexpr.Simpl
      ( Genredexpr.
          {
            rStrength = Norm;
            rBeta = true;
            rMatch = true;
            rFix = true;
            rCofix = true;
            rZeta = true;
            rDelta = true;
            rConst = [];
          },
        None ) ->
      write "simpl"
  | Genredexpr.Unfold xs ->
      let pp_single = function
        | Locus.AllOccurrences, CAst.{ v = Constrexpr.AN name; loc = _ } ->
            pp_qualid name
        | ( Locus.AllOccurrences,
            CAst.{ v = Constrexpr.ByNotation (notation, None); loc = _ } ) ->
            doublequoted (write notation)
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence [ write "unfold "; map_commad pp_single xs ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_intro_pattern_naming_expr = function
  | Namegen.IntroIdentifier name -> pp_id name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_or_and_intro_pattern_expr = function
  | Tactypes.IntroOrPattern patterns ->
      let open CAst in
      let hor =
        let prefix = function 0 -> nop | _ -> write " | " in
        let pp_patterns i pattern =
          sequence
            [
              prefix i; map_spaced (fun x -> pp_intro_pattern_expr x.v) pattern;
            ]
        in
        brackets (sequence (List.mapi pp_patterns patterns))
      in
      let ver =
        let prefix = function 0 -> nop | _ -> write "|" in
        let pp_patterns i pattern =
          sequence
            [
              prefix i;
              map_sequence
                (fun x -> sequence [ space; pp_intro_pattern_expr x.v ])
                pattern;
              newline;
            ]
        in
        sequence
          [ write "["; sequence (List.mapi pp_patterns patterns); write "]" ]
      in
      hor <-|> ver
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_action_expr = function
  | Tactypes.IntroOrAndPattern expr -> pp_or_and_intro_pattern_expr expr
  | Tactypes.IntroWildcard -> write "_"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_expr = function
  | Tactypes.IntroAction expr -> pp_intro_pattern_action_expr expr
  | Tactypes.IntroNaming expr -> pp_intro_pattern_naming_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_core_destruction_arg = function
  | Tactics.ElimOnConstr (expr, Tactypes.NoBindings) ->
      parens (pp_constr_expr expr)
  | Tactics.ElimOnIdent ident -> pp_lident ident
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_destruction_arg = function
  | None, arg -> pp_core_destruction_arg arg
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_or_var = function
  | Locus.ArgArg CAst.{ v; loc = _ } ->
      let hor = sequence [ write " as "; pp_or_and_intro_pattern_expr v ] in
      let ver =
        sequence
          [ write " as"; newline; indented (pp_or_and_intro_pattern_expr v) ]
      in
      hor <-|> ver
  | Locus.ArgVar ident -> pp_lident ident

let pp_induction_clause = function
  | arg, (eqn, as_list), None ->
      let pp_as_list = function None -> nop | Some args -> pp_or_var args in
      let pp_eqn = function
        | None -> nop
        | Some x ->
            let open CAst in
            sequence [ write " eqn:"; pp_intro_pattern_naming_expr x.v ]
      in
      sequence [ pp_destruction_arg arg; pp_as_list as_list; pp_eqn eqn ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_induction_clause_list = function
  | [ clause ], using ->
      let pp_using =
        match using with
        | None -> nop
        | Some (expr, Tactypes.NoBindings) ->
            sequence [ write " using "; pp_constr_expr expr ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence [ pp_induction_clause clause; pp_using ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_hyp_location_expr = function
  | (Locus.AllOccurrences, name), Locus.InHyp -> pp_id name.CAst.v
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_quantified_hypothesis = function
  | Tactypes.NamedHyp name -> pp_lident name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_inversion_strength = function
  | Tacexpr.NonDepInversion (FullInversion, [], Some args) -> pp_or_var args
  | Tacexpr.NonDepInversion (FullInversion, [], None) -> nop
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_bindings = function
  | Tactypes.ImplicitBindings exprs ->
      sequence [ write " with "; map_spaced pp_constr_expr_with_parens exprs ]
  | Tactypes.ExplicitBindings bindings ->
      let pp_one_binding CAst.{ v = replacee, replacer; loc = _ } =
        parens
          (sequence
             [
               pp_quantified_hypothesis replacee;
               write " := ";
               pp_constr_expr replacer;
             ])
      in

      let hor =
        sequence [ write " with "; map_spaced pp_one_binding bindings ]
      in
      let ver =
        sequence
          [
            newline;
            indented
              (sequence [ write "with "; map_spaced pp_one_binding bindings ]);
          ]
      in

      hor <-|> ver
  | Tactypes.NoBindings -> nop

let pp_match_pattern = function
  | Tacexpr.Term expr -> pp_constr_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_match_context_hyps = function
  | Tacexpr.Hyp (name, pattern) ->
      spaced [ pp_lname name; write ":"; pp_match_pattern pattern ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_message_token = function
  | Tacexpr.MsgString s -> doublequoted (write s)
  | Tacexpr.MsgInt i -> pp_int i
  | Tacexpr.MsgIdent id -> pp_lident id

let rec pp_raw_atomic_tactic_expr = function
  | Tacexpr.TacApply (true, is_eapply, [ (None, (expr, bindings)) ], in_clause)
    ->
      let pp_tactic = if is_eapply then write "eapply" else write "apply" in

      let pp_in_clause =
        match in_clause with
        | [] -> nop
        | [ (name, None) ] -> sequence [ write " in "; pp_id name.CAst.v ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence
        [
          pp_tactic;
          space;
          pp_constr_expr_with_parens expr;
          pp_bindings bindings;
          pp_in_clause;
        ]
  | Tacexpr.TacAssert (false, true, Some by, name, expr) ->
      let pp_name =
        match name with
        | None -> nop
        | Some name -> sequence [ pp_intro_pattern_expr name.v; write " : " ]
      in

      let pp_by =
        match by with
        | None -> nop
        | Some by ->
            sequence [ write " by "; pp_raw_tactic_expr_with_parens by ]
      in

      sequence
        [ write "assert ("; pp_name; pp_constr_expr expr; write ")"; pp_by ]
  | Tacexpr.TacInductionDestruct (is_induction, false, clause_list) ->
      sequence
        [
          (if is_induction then write "induction " else write "destruct ");
          pp_induction_clause_list clause_list;
        ]
  | Tacexpr.TacIntroPattern
      (false, [ CAst.{ v = Tactypes.IntroForthcoming _; loc = _ } ]) ->
      write "intros"
  | Tacexpr.TacIntroPattern (false, exprs) ->
      let open CAst in
      sequence
        [
          write "intros ";
          map_spaced (fun expr -> pp_intro_pattern_expr expr.v) exprs;
        ]
  | Tacexpr.TacReduce (expr, { onhyps; concl_occs = _ }) ->
      let pp_in =
        match onhyps with
        | Some [] -> nop
        | Some [ name ] -> sequence [ write " in "; pp_hyp_location_expr name ]
        | None -> write " in *"
        | Some _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence [ pp_raw_red_expr expr; pp_in ]
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, with_bindings))) ],
        in_bindings,
        None ) ->
      let pp_in_bindings =
        let open Locus in
        match in_bindings with
        | { onhyps = Some []; concl_occs = AllOccurrences } -> nop
        | { onhyps = None; concl_occs = AllOccurrences } -> write " in *"
        | { onhyps = Some [ name ]; concl_occs = NoOccurrences } ->
            sequence [ write " in "; pp_hyp_location_expr name ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence
        [
          write "rewrite ";
          (if is_left_to_right then write "-> " else write "<- ");
          pp_constr_expr_with_parens expr;
          pp_in_bindings;
          pp_bindings with_bindings;
        ]
  | Tacexpr.TacInversion (intros, name) ->
      sequence
        [
          write "inversion ";
          pp_quantified_hypothesis name;
          pp_inversion_strength intros;
        ]
  | Tacexpr.TacLetTac
      ( false,
        replacer,
        replacee,
        { onhyps = None; concl_occs = AllOccurrences },
        false,
        eqn ) ->
      let pp_eqn =
        match eqn with
        | None -> nop
        | Some x -> sequence [ write " eqn:"; pp_intro_pattern_naming_expr x.v ]
      in

      sequence
        [
          write "remember ";
          pp_constr_expr_with_parens replacee;
          write " as ";
          pp_name replacer;
          pp_eqn;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_raw_tactic_expr (CAst.{ v; loc = _ } : Tacexpr.raw_tactic_expr) =
  pp_raw_tactic_expr_r v

and pp_raw_tactic_expr_with_parens_conditionally cond expr =
  if cond then parens (pp_raw_tactic_expr expr) else pp_raw_tactic_expr expr

and pp_raw_tactic_expr_with_parens expr =
  pp_raw_tactic_expr_with_parens_conditionally
    (tactics_generally_parens_needed expr.CAst.v)
    expr

and pp_raw_tactic_expr_r = function
  | Tacexpr.TacAlias (alias, init_replacers) ->
      (* FIXME: Needs refactoring. *)
      let id = Pptactic.pr_alias_key alias |> Pp.string_of_ppcmds in

      let init_idents = String.split_on_char ' ' id in
      let starts_with_paren s = String.starts_with ~prefix:"(" s in
      let rec loop idents replacers =
        match (idents, replacers) with
        | [], [] -> []
        | [], _ -> failwith "Too many replacers."
        | s :: _, [] when starts_with_paren s -> failwith "Too few replacers."
        | s :: t_ids, Tacexpr.TacGeneric (None, args) :: t_reps
          when starts_with_paren s ->
            let try_pp converter pp =
              match converter args with Some x -> Some (pp x) | None -> None
            in

            let try_pp_always converter pp =
              try_pp converter (fun x -> Some (pp x))
            in

            let try_pp_constr_expr =
              try_pp_always Conversion.constr_expr_of_raw_generic_argument
                pp_constr_expr_with_parens
            in

            let try_pp_destruction_arg =
              try_pp_always Conversion.destruction_arg_of_raw_generic_argument
                pp_destruction_arg
            in

            let try_pp_intro_pattern_expr =
              try_pp_always
                Conversion.intro_pattern_list_of_raw_generic_argument
                (map_spaced (fun expr -> pp_intro_pattern_expr expr.CAst.v))
            in

            let try_pp_clause_expr =
              let pp = function
                | { Locus.onhyps = Some [ name ]; concl_occs = _ } ->
                    pp_hyp_location_expr name
                | _ -> fun printer -> raise (NotImplemented (contents printer))
              in

              try_pp_always Conversion.clause_expr_of_raw_generic_argument pp
            in

            let try_pp_bindings =
              try_pp_always Conversion.bindings_list_of_raw_generic_argument
                (map_commad (function
                  | Tactypes.ImplicitBindings [ x ] ->
                      pp_constr_expr_with_parens x
                  | _ ->
                      fun printer -> raise (NotImplemented (contents printer))))
            in

            let try_pp_id =
              try_pp_always Conversion.id_of_raw_generic_argument pp_id
            in

            let try_pp_hyp =
              try_pp_always Conversion.hyp_of_raw_generic_argument
                (map_spaced pp_lident)
            in

            let try_pp_nat_or_var =
              let pp = function
                | [] -> None
                | [ Locus.ArgArg name ] -> Some (pp_int name)
                | _ ->
                    Some
                      (fun printer -> raise (NotImplemented (contents printer)))
              in

              try_pp Conversion.nat_or_var_of_raw_generic_argument pp
            in

            let try_pp_auto_using =
              let pp = function
                | [] -> None
                | [ x ] -> Some (sequence [ write "using "; pp_constr_expr x ])
                | _ ->
                    Some
                      (fun printer -> raise (NotImplemented (contents printer)))
              in

              try_pp Conversion.auto_using_of_raw_generic_argument pp
            in

            let try_pp_hintbases =
              let pp = function
                | [] -> None
                | [ x ] -> Some (spaced [ write "with"; write x ])
                | _ ->
                    Some
                      (fun printer -> raise (NotImplemented (contents printer)))
              in

              try_pp Conversion.hintbases_of_raw_generic_argument pp
            in

            let try_pp_by_arg_tactic =
              let pp = function
                | [] -> None
                | [ x ] ->
                    Some
                      (spaced [ write "by"; pp_raw_tactic_expr_with_parens x ])
                | _ ->
                    Some
                      (fun printer -> raise (NotImplemented (contents printer)))
              in

              try_pp Conversion.by_arg_tac_of_raw_generic_argument pp
            in

            let try_pp_clause_dft_concl =
              let pp = function
                | Locus.
                    {
                      onhyps = Some [ ((AllOccurrences, name), _) ];
                      concl_occs = NoOccurrences;
                    } ->
                    Some (spaced [ write "in"; pp_id name.CAst.v ])
                | Locus.{ onhyps = Some []; concl_occs = AllOccurrences } ->
                    None
                | _ ->
                    Some
                      (fun printer -> raise (NotImplemented (contents printer)))
              in

              try_pp Conversion.clause_dft_concl_of_raw_generic_argument pp
            in

            let try_pp_constr_with_bindings =
              let pp (expr, bindings) =
                Some
                  (sequence
                     [ pp_constr_expr_with_parens expr; pp_bindings bindings ])
              in

              try_pp Conversion.constr_with_bindings_of_raw_generic_argument pp
            in

            let printers =
              [
                try_pp_constr_expr;
                try_pp_destruction_arg;
                try_pp_intro_pattern_expr;
                try_pp_clause_expr;
                try_pp_bindings;
                try_pp_id;
                try_pp_hyp;
                try_pp_nat_or_var;
                try_pp_auto_using;
                try_pp_hintbases;
                try_pp_by_arg_tactic;
                try_pp_clause_dft_concl;
                try_pp_constr_with_bindings;
              ]
            in

            let rec try_pp = function
              | [] | Some None :: _ -> loop t_ids t_reps
              | None :: t -> try_pp t
              | Some (Some x) :: _ -> x :: loop t_ids t_reps
            in

            try_pp printers
        | h_id :: t_ids, Tacexpr.Tacexp tactic :: t_reps
          when starts_with_paren h_id ->
            pp_raw_tactic_expr tactic :: loop t_ids t_reps
        | h_id :: t_id, _ -> write h_id :: loop t_id replacers
      in

      sequence [ loop init_idents init_replacers |> spaced ]
  | Tacexpr.TacArg arg -> pp_gen_tactic_arg arg
  | Tacexpr.TacAtom atom -> sequence [ pp_raw_atomic_tactic_expr atom ]
  | Tacexpr.TacFail (TacLocal, ArgArg 0, _) -> write "fail"
  | Tacexpr.TacId [] -> nop
  | Tacexpr.TacId names ->
      spaced [ write "idtac"; map_spaced pp_message_token names ]
  | Tacexpr.TacMatchGoal (Once, false, rules) ->
      lined
        [ write "match goal with"; map_lined pp_match_rule rules; write "end" ]
  | Tacexpr.TacOrelse (first, second) ->
      spaced [ pp_raw_tactic_expr first; write "||"; pp_raw_tactic_expr second ]
  | Tacexpr.TacRepeat tactic ->
      sequence [ write "repeat "; pp_raw_tactic_expr_with_parens tactic ]
  | Tacexpr.TacThen (first, second) ->
      let hor =
        sequence
          [ pp_raw_tactic_expr first; write "; "; pp_raw_tactic_expr second ]
      in
      let ver =
        let tactics =
          let rec loop tactic =
            match tactic.CAst.v with
            | Tacexpr.TacThen (first, second) -> second :: loop first
            | _ -> [ tactic ]
          in
          second :: loop first |> List.rev
        in

        let rec loop tactics is_first printer =
          match (tactics, is_first) with
          | [], _ -> nop printer
          | [ h ], _ -> (
              let pp = pp_raw_tactic_expr h in

              match can_pp_oneline (sequence [ space; pp ]) printer with
              | true -> sequence [ space; pp ] printer
              | false -> sequence [ newline; pp ] printer)
          | h :: t, true ->
              sequence [ pp_raw_tactic_expr h; write ";"; loop t false ] printer
          | h :: t, false -> (
              let pp = sequence [ pp_raw_tactic_expr h; write ";" ] in

              match can_pp_oneline (sequence [ space; pp ]) printer with
              | true -> sequence [ space; pp; loop t false ] printer
              | false -> sequence [ newline; pp; loop t false ] printer)
        in

        loop tactics true
      in

      hor <-|> ver
  | Tacexpr.TacThens (first, second) ->
      let pp_bracket_clause =
        let prefix = function 0 -> nop | _ -> write " | " in

        let pp_patterns i pattern =
          sequence [ prefix i; pp_raw_tactic_expr pattern ]
        in

        brackets (sequence (List.mapi pp_patterns second))
      in

      sequence [ pp_raw_tactic_expr first; write "; "; pp_bracket_clause ]
  | Tacexpr.TacTry tactic ->
      sequence [ write "try "; pp_raw_tactic_expr_with_parens tactic ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_match_rule = function
  | Tacexpr.Pat (contexts, pattern, expr) ->
      let pp_contexts =
        if contexts = [] then nop
        else
          sequence
            [
              map_with_seps
                ~sep:(sequence [ comma; newline ])
                pp_match_context_hyps contexts;
              space;
            ]
      in

      write "| "
      |=> sequence
            [
              pp_contexts;
              write "|- ";
              pp_match_pattern pattern;
              write " => ";
              pp_raw_tactic_expr expr;
            ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_tacdef_body = function
  | Tacexpr.TacticDefinition (name, v) ->
      let pp_params =
        match v with
        | CAst.{ v = TacFun (params, _); loc = _ } ->
            map_sequence (fun x -> sequence [ space; pp_name x ]) params
        | _ -> nop
      in

      let pp_body =
        let body =
          match v with CAst.{ v = TacFun (_, body); loc = _ } -> body | _ -> v
        in

        let hor = sequence [ space; pp_raw_tactic_expr body ] in
        let ver = sequence [ newline; indented (pp_raw_tactic_expr body) ] in
        hor <-|> ver
      in

      sequence
        [ write "Ltac "; pp_lident name; pp_params; write " :="; pp_body ]
  | Tacexpr.TacticRedefinition _ ->
      fun printer -> raise (NotImplemented (contents printer))

let pp_lang = function
  | Extraction_plugin.Table.Haskell -> write "Haskell"
  | Extraction_plugin.Table.Ocaml -> write "OCaml"
  | Extraction_plugin.Table.Scheme -> write "Scheme"
  | Extraction_plugin.Table.JSON -> write "JSON"

let pp_vernac_solve =
  let try_pp conversion pp expr =
    match conversion expr with Some x -> Some (pp x) | None -> None
  in

  let try_pp_raw_tactic_expr =
    let pp expr = sequence [ pp_raw_tactic_expr expr; dot ] in

    try_pp Conversion.raw_tactic_expr_of_raw_generic_argument pp
  in

  let try_pp_tacdef_body =
    let pp = function
      | [ t ] -> sequence [ pp_tacdef_body t; dot ]
      | _ -> fun printer -> raise (NotImplemented (contents printer))
    in

    try_pp Conversion.tacdef_body_of_raw_generic_argument pp
  in

  let try_pp_ltac_use_default =
    let pp = function true -> write ".." | false -> nop in

    try_pp Conversion.ltac_use_default_of_raw_generic_argument pp
  in

  let pp_funcs =
    [ try_pp_raw_tactic_expr; try_pp_tacdef_body; try_pp_ltac_use_default ]
  in

  let rec pp fs expr =
    match fs with
    | [] -> nop
    | h :: t -> ( match h expr with None -> pp t expr | Some p -> p)
  in

  map_sequence (pp pp_funcs)

let pp_proof_bullet = function
  | Proof_bullet.Dash n -> write_before_indent (String.make n '-' ^ " ")
  | Proof_bullet.Plus n -> write_before_indent (String.make n '+' ^ " ")
  | Proof_bullet.Star n -> write_before_indent (String.make n '*' ^ " ")

let pp_import_categories { Vernacexpr.negative; import_cats } =
  let open CAst in
  sequence
    [
      space;
      (if negative then write "-" else nop);
      parens (map_commad (fun import_cat -> write import_cat.v) import_cats);
    ]

let pp_export_flag = function
  | Vernacexpr.Export -> write "Export"
  | Vernacexpr.Import -> write "Import"

let pp_export_with_cats (flag, import_categories) =
  sequence
    [
      space;
      pp_export_flag flag;
      (match import_categories with
      | None -> nop
      | Some x -> pp_import_categories x);
    ]

let pp_import_filter_expr import_filter_expr =
  match import_filter_expr with
  | Vernacexpr.ImportAll -> nop
  | Vernacexpr.ImportNames names ->
      sequence
        [
          (* FIXME: The Coq parser will raise an exception here if Export/Import
             was omitted *)
          parens
            (map_commad (fun (filter_name, _) -> pp_qualid filter_name) names);
        ]

let pp_search_item = function
  | Vernacexpr.SearchSubPattern ((Anywhere, false), expr) ->
      pp_constr_expr_with_parens expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_search_request = function
  | Vernacexpr.SearchLiteral search_item -> pp_search_item search_item
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_searchable = function
  | Vernacexpr.Search [ (true, search_request) ] ->
      pp_search_request search_request
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_search_restriction = function
  | Vernacexpr.SearchInside [ range ] ->
      sequence [ write " inside "; pp_qualid range ]
  | Vernacexpr.SearchOutside [] -> nop
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_vernac_argument_status = function
  | Vernacexpr.VolatileArg -> write "/"
  | Vernacexpr.RealArg
      { name = ty; recarg_like = false; notation_scope = []; implicit_status }
    ->
      let encloser =
        match implicit_status with
        | Explicit -> Fun.id
        | MaxImplicit -> braces
        | NonMaxImplicit -> brackets
      in

      encloser (pp_name ty)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_option_string = function
  | Vernacexpr.OptionSetString s -> doublequoted (write s)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_ident_decl = function
  | name, None -> pp_lident name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_printable = function
  | Vernacexpr.PrintAssumptions (false, false, { v = AN name; loc = _ }) ->
      sequence [ write "Print Assumptions "; pp_qualid name; dot ]
  | Vernacexpr.PrintName (CAst.{ v = AN name; loc = _ }, None) ->
      sequence [ write "Print "; pp_qualid name; dot ]
  | Vernacexpr.PrintName (CAst.{ v = ByNotation (name, None); loc = _ }, None)
    ->
      sequence [ write "Print "; doublequoted (write name); dot ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_assumption_object_kind = function
  | Decls.Logical -> write "Axiom"
  | Decls.Conjectural -> write "Conjecture"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_notation_declaration = function
  | Vernacexpr.
      {
        ntn_decl_string;
        ntn_decl_interp;
        ntn_decl_scope;
        ntn_decl_modifiers = [];
      } ->
      let pp_scope =
        match ntn_decl_scope with
        | None -> nop
        | Some scope -> sequence [ write " : "; write scope ]
      in

      sequence
        [
          newline;
          indented
            (sequence
               [
                 write "where";
                 newline;
                 indented
                   (sequence
                      [
                        doublequoted (pp_lstring ntn_decl_string);
                        write " := ";
                        parens (pp_constr_expr ntn_decl_interp);
                        pp_scope;
                      ]);
               ]);
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_comment = function
  | Vernacexpr.CommentConstr expr -> pp_constr_expr expr
  | Vernacexpr.CommentString s -> doublequoted (write s)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_showable = function
  | Vernacexpr.ShowProof -> write "Proof"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_reference_or_constr = function
  | Vernacexpr.HintsReference name -> pp_qualid name
  | Vernacexpr.HintsConstr _ ->
      fun printer -> raise (NotImplemented (contents printer))

let pp_hints_expr = function
  | Vernacexpr.HintsConstructors [ name ] ->
      sequence [ write "Hint Constructors "; pp_qualid name ]
  | Vernacexpr.HintsResolve
      [ ({ hint_priority = None; hint_pattern = None }, true, expr) ] ->
      sequence [ write "Hint Resolve "; pp_reference_or_constr expr ]
  | Vernacexpr.HintsTransparency (HintsReferences names, true) ->
      sequence [ write "Hint Transparent "; map_spaced pp_qualid names ]
  | Vernacexpr.HintsUnfold names ->
      sequence [ write "Hint Unfold "; map_spaced pp_qualid names ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_extraction = function
  | [ filename; identifiens ] -> (
      match
        ( Conversion.string_of_raw_generic_argument filename,
          Conversion.ref_list_of_raw_generic_argument identifiens )
      with
      | Some filename, Some identifiers ->
          sequence
            [
              write "Extraction ";
              doublequoted (write filename);
              space;
              map_spaced pp_qualid identifiers;
              dot;
            ]
      | _ -> fun printer -> raise (NotImplemented (contents printer)))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_extraction_constant = function
  | [ identifier; _; constant ] -> (
      match
        ( Conversion.ref_of_raw_generic_argument identifier,
          Conversion.mlname_of_raw_generic_argument constant )
      with
      | Some identifier, Some constant ->
          sequence
            [
              write "Extract Constant ";
              pp_qualid identifier;
              write " => ";
              doublequoted (write constant);
              dot;
            ]
      | _ -> fun printer -> raise (NotImplemented (contents printer)))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_vernac_declare_tactic_definition = function
  | [ args ] -> (
      match Conversion.tacdef_body_of_raw_generic_argument args with
      | Some [ t ] -> sequence [ pp_tacdef_body t; dot ]
      | _ -> fun printer -> raise (NotImplemented (contents printer)))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_extraction_language = function
  | [ lang ] -> (
      match Conversion.lang_of_raw_generic_argument lang with
      | Some lang ->
          sequence [ write "Extraction Language "; pp_lang lang; dot ]
      | _ -> fun printer -> raise (NotImplemented (contents printer)))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_extraction_inductive = function
  | [ identifier; name; branches; matcher ] -> (
      match
        ( Conversion.ref_of_raw_generic_argument identifier,
          Conversion.mlname_of_raw_generic_argument name,
          Conversion.mlname_list_of_raw_generic_argument branches,
          Conversion.opt_string_of_raw_generic_argument matcher )
      with
      | Some identifier, Some name, Some branches, Some matcher ->
          let pp_matcher =
            match matcher with
            | Some matcher ->
                let hor = sequence [ space; doublequoted (write matcher) ] in
                let ver =
                  sequence [ newline; indented (doublequoted (write matcher)) ]
                in

                hor <-|> ver
            | None -> nop
          in

          sequence
            [
              write "Extract Inductive ";
              pp_qualid identifier;
              write " => ";
              doublequoted (write name);
              space;
              brackets
                (map_spaced
                   (fun branch -> doublequoted (write branch))
                   branches);
              pp_matcher;
              dot;
            ]
      | _ -> fun printer -> raise (NotImplemented (contents printer)))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_table_value = function
  | Goptions.StringRefValue _ ->
      fun printer -> raise (NotImplemented (contents printer))
  | Goptions.QualidRefValue name -> pp_qualid name

let pp_coercion_class = function
  | Vernacexpr.FunClass ->
      fun printer -> raise (NotImplemented (contents printer))
  | Vernacexpr.SortClass -> write "Sortclass"
  | Vernacexpr.RefClass { v = AN src; loc = _ } -> pp_qualid src
  | Vernacexpr.RefClass { v = ByNotation _; loc = _ } ->
      fun printer -> raise (NotImplemented (contents printer))

let pp_synterp_vernac_expr = function
  | Vernacexpr.VernacDeclareCustomEntry name ->
      sequence [ write "Declare Custom Entry "; write name; dot ]
  | Vernacexpr.VernacDefineModule (None, name, [], Check [], []) ->
      sequence [ write "Module "; pp_lident name; dot; increase_indent ]
  | Vernacexpr.VernacEndSegment name ->
      sequence [ decrease_indent; write "End "; pp_lident name; dot ]
  | Vernacexpr.VernacImport ((flag, None), [ (name, ImportAll) ]) ->
      sequence [ pp_export_flag flag; space; pp_qualid name; dot ]
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.extraction";
          ext_entry = "Extraction";
          ext_index = 2;
        },
        args ) ->
      pp_extraction args
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.extraction";
          ext_entry = "ExtractionConstant";
          ext_index = 0;
        },
        args ) ->
      pp_extraction_constant args
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.extraction";
          ext_entry = "ExtractionLanguage";
          ext_index = 0;
        },
        args ) ->
      pp_extraction_language args
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.extraction";
          ext_entry = "ExtractionInductive";
          ext_index = 0;
        },
        args ) ->
      pp_extraction_inductive args
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.ltac";
          ext_entry = "VernacSolve";
          ext_index = 0;
        },
        args ) ->
      pp_vernac_solve args
  | Vernacexpr.VernacExtend
      ( {
          ext_plugin = "coq-core.plugins.ltac";
          ext_entry = "VernacDeclareTacticDefinition";
          ext_index = 0;
        },
        args ) ->
      pp_vernac_declare_tactic_definition args
  | Vernacexpr.VernacNotation
      ( false,
        {
          ntn_decl_string = notation;
          ntn_decl_interp = expr;
          ntn_decl_scope = scope;
          ntn_decl_modifiers = modifiers;
        } ) ->
      let pp_modifiers =
        let pp sep =
          sequence
            [
              space;
              parens
                (map_with_seps ~sep
                   (fun modifier -> pp_syntax_modifier modifier.CAst.v)
                   modifiers);
            ]
        in
        let hor = pp (write ", ") in
        let ver = pp (sequence [ write ","; newline ]) in

        hor <-|> ver
      in

      (* We cannot use `Option.value` here because the `Option` module is
         overridden by `coq-core`'s one which does not have it. *)
      let pp_scope =
        match scope with
        | None -> nop
        | Some scope ->
            let hor = sequence [ write " : "; write scope ] in
            let ver =
              sequence [ newline; sequence [ write ": "; write scope ] ]
            in

            hor <-|> ver
      in

      let parens_needed expr =
        match expr.CAst.v with
        | Constrexpr.CLetIn _ -> true
        | _ -> exprs_generally_parens_needed expr.CAst.v
      in

      let conditional_parens expr =
        if parens_needed expr then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      let hor =
        sequence
          [
            space;
            conditional_parens expr;
            (if List.length modifiers > 0 then pp_modifiers else nop);
            pp_scope;
            dot;
          ]
      in

      let ver =
        sequence
          [
            newline;
            indented
              (sequence
                 [
                   conditional_parens expr;
                   (if List.length modifiers > 0 then pp_modifiers else nop);
                   pp_scope;
                   dot;
                 ]);
          ]
      in

      sequence
        [
          write "Notation \""; pp_lstring notation; write "\" :="; hor <-|> ver;
        ]
  | Vernacexpr.VernacRequire (dirpath, export_with_cats, filtered_import) ->
      let pp_dirpath printer =
        (match dirpath with
        | None -> nop
        | Some dirpath -> sequence [ write "From "; pp_qualid dirpath; space ])
          printer
      in

      let pp_categories =
        match export_with_cats with
        | None -> nop
        | Some x -> pp_export_with_cats x
      in

      let pp_name_and_filter =
        map_sequence
          (fun (modname, import_filter_expr) ->
            sequence
              [
                space;
                pp_qualid modname;
                pp_import_filter_expr import_filter_expr;
              ])
          filtered_import
      in

      sequence
        [ pp_dirpath; write "Require"; pp_categories; pp_name_and_filter; dot ]
  | Vernacexpr.VernacReservedNotation (false, (notation, modifiers)) ->
      let pp_modifiers =
        parens
          (map_commad
             (fun modifier -> pp_syntax_modifier modifier.CAst.v)
             modifiers)
      in

      let hor = sequence [ space; pp_modifiers; dot ] in
      let ver =
        sequence [ newline; indented (sequence [ pp_modifiers; dot ]) ]
      in

      sequence
        [
          write "Reserved Notation ";
          doublequoted (pp_lstring notation);
          hor <-|> ver;
        ]
  | Vernacexpr.VernacSetOption (false, names, OptionSetTrue) ->
      sequence [ write "Set "; map_spaced write names; dot ]
  | Vernacexpr.VernacSetOption (false, names, OptionUnset) ->
      sequence [ write "Unset "; map_spaced write names; dot ]
  | Vernacexpr.VernacSetOption (false, names, options) ->
      sequence
        [
          write "Set ";
          map_spaced write names;
          space;
          pp_option_string options;
          dot;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_synpure_vernac_expr = function
  | Vernacexpr.VernacAbort -> sequence [ clear_bullets; write "Abort." ]
  | Vernacexpr.VernacAddOption (options, names) ->
      sequence
        [
          write "Add ";
          map_spaced write options;
          space;
          map_spaced pp_table_value names;
          dot;
        ]
  | Vernacexpr.VernacArguments (CAst.{ v = AN name; loc = _ }, args, [], []) ->
      sequence
        [
          write "Arguments ";
          pp_qualid name;
          space;
          map_spaced pp_vernac_argument_status args;
          dot;
        ]
  | Vernacexpr.VernacAssumption
      ((NoDischarge, kind), NoInline, [ (NoCoercion, ([ name ], expr)) ]) ->
      sequence
        [
          pp_assumption_object_kind kind;
          space;
          pp_ident_decl name;
          write " : ";
          pp_constr_expr expr;
          dot;
        ]
  | Vernacexpr.VernacBindScope (scope, [ dst ]) ->
      sequence
        [
          write "Bind Scope ";
          write scope;
          write " with ";
          pp_coercion_class dst;
          dot;
        ]
  | Vernacexpr.VernacCheckMayEval (check_or_compute, None, expr) ->
      let pp_name =
        match check_or_compute with
        | Some
            (Cbv
              {
                rStrength = Norm;
                rBeta = true;
                rMatch = true;
                rFix = true;
                rCofix = true;
                rZeta = true;
                rDelta = true;
                rConst = [];
              }) ->
            write "Eval compute in"
        | Some (CbvVm None) -> write "Compute"
        | None -> write "Check"
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      let pp_expr =
        if exprs_generally_parens_needed expr.v then
          parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      let hor = sequence [ pp_name; space; pp_expr; dot ] in
      let ver =
        sequence [ pp_name; newline; indented (sequence [ pp_expr; dot ]) ]
      in

      hor <-|> ver
  | Vernacexpr.VernacComments xs ->
      sequence [ write "Comments "; map_spaced pp_comment xs; dot ]
  | Vernacexpr.VernacCreateHintDb (name, false) ->
      sequence [ write "Create HintDb "; write name; dot ]
  | Vernacexpr.VernacDeclareScope name ->
      sequence [ write "Declare Scope "; write name; dot ]
  | Vernacexpr.VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      sequence
        [
          pp_definition_object_kind kind;
          space;
          pp_lname name;
          pp_definition_expr expr;
          dot;
        ]
  | Vernacexpr.VernacDelimiters (scope, Some dst) ->
      sequence
        [ write "Delimit Scope "; write scope; write " with "; write dst; dot ]
  | Vernacexpr.VernacCoercion (CAst.{ v = AN name; loc = _ }, Some (src, dst))
    ->
      sequence
        [
          write "Coercion ";
          pp_qualid name;
          write " : ";
          pp_coercion_class src;
          write " >-> ";
          pp_coercion_class dst;
          dot;
        ]
  | Vernacexpr.VernacFixpoint (NoDischarge, exprs) ->
      sequence
        [
          write "Fixpoint ";
          map_with_seps
            ~sep:(sequence [ blankline; write "with " ])
            pp_fixpoint_expr exprs;
          dot;
        ]
  | Vernacexpr.VernacLocate (LocateAny CAst.{ v = Constrexpr.AN name; loc = _ })
    ->
      sequence [ write "Locate "; pp_qualid name; dot ]
  | Vernacexpr.VernacLocate
      (LocateAny CAst.{ v = Constrexpr.ByNotation (name, None); loc = _ }) ->
      sequence [ write "Locate "; doublequoted (write name); dot ]
  | Vernacexpr.VernacOpenCloseScope (true, scope) ->
      sequence [ write "Open Scope "; write scope; dot ]
  | Vernacexpr.VernacPrint printable -> pp_printable printable
  | Vernacexpr.VernacSearch (searchable, None, search_restriction) ->
      sequence
        [
          write "Search ";
          pp_searchable searchable;
          pp_search_restriction search_restriction;
          dot;
        ]
  | Vernacexpr.VernacStartTheoremProof (kind, [ ((ident, None), (args, expr)) ])
    ->
      let hor = sequence [ space; pp_constr_expr expr; dot ] in
      let ver =
        sequence [ newline; indented (sequence [ pp_constr_expr expr; dot ]) ]
      in
      sequence
        [
          pp_theorem_kind kind;
          space;
          pp_lident ident;
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args;
          write " :";
          hor <-|> ver;
        ]
  | Vernacexpr.VernacShow x -> sequence [ write "Show "; pp_showable x; dot ]
  | Vernacexpr.VernacProof (None, None) -> write "Proof."
  | Vernacexpr.VernacProof (Some expr, None) -> (
      match Conversion.ltac_of_raw_generic_argument expr with
      | Some x -> sequence [ write "Proof with "; pp_raw_tactic_expr x; dot ]
      | None -> fun printer -> raise (NotImplemented (contents printer)))
  | Vernacexpr.VernacInductive (Inductive_kw, inductives) ->
      let pp_single_inductive = function
        | ( ( (Vernacexpr.NoCoercion, (name, None)),
              (type_params, None),
              return_type,
              Vernacexpr.Constructors constructors ),
            where_clause ) ->
            let pp_type_params =
              match type_params with
              | [] -> nop
              | _ ->
                  sequence
                    [ space; map_spaced pp_local_binder_expr type_params ]
            in
            let pp_return_type =
              match return_type with
              | Some ty -> sequence [ write " : "; pp_constr_expr ty ]
              | None -> nop
            in

            let pp_where_clause =
              match where_clause with
              | [] -> nop
              | [ notation ] -> pp_notation_declaration notation
              | _ -> fun printer -> raise (NotImplemented (contents printer))
            in

            sequence
              [
                pp_lident name;
                pp_type_params;
                pp_return_type;
                write " :=";
                indented (map_sequence pp_constructor_expr constructors);
                pp_where_clause;
              ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in
      sequence
        [
          write "Inductive ";
          map_with_seps
            ~sep:(sequence [ blankline; write "with " ])
            pp_single_inductive inductives;
          dot;
        ]
  | Vernacexpr.VernacEndProof proof_end -> pp_proof_end proof_end
  | Vernacexpr.VernacBullet bullet ->
      sequence [ bullet_appears bullet; pp_proof_bullet bullet ]
  | Vernacexpr.VernacSubproof None -> sequence [ write "{"; start_subproof ]
  | Vernacexpr.VernacEndSubproof -> sequence [ end_subproof; write "}" ]
  | Vernacexpr.VernacHints ([ database ], expr) ->
      sequence [ pp_hints_expr expr; write " : "; write database; dot ]
  | Vernacexpr.VernacSyntacticDefinition (name, ([ lhs ], expr), []) ->
      sequence
        [
          write "Notation ";
          pp_lident name;
          space;
          pp_id lhs;
          write " := ";
          parens (pp_constr_expr expr);
          dot;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_vernac_expr = function
  | Vernacexpr.VernacSynterp x -> pp_synterp_vernac_expr x
  | Vernacexpr.VernacSynPure x -> pp_synpure_vernac_expr x

let pp_control_flag = function
  | Vernacexpr.ControlFail -> write "Fail"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_subast CAst.{ v = Vernacexpr.{ control; attrs = _; expr }; loc = _ } =
  let pp_controls = function
    | [] -> nop
    | _ -> sequence [ map_spaced pp_control_flag control; space ]
  in
  sequence [ pp_controls control; pp_vernac_expr expr ]

let separator current next =
  let open Vernacexpr in
  let open CAst in
  match (current.v.expr, next.v.expr) with
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure VernacAbort
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure (VernacEndProof _)
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure (VernacProof _)
  | VernacSynPure (VernacDefinition _), VernacSynPure VernacAbort
  | VernacSynPure (VernacDefinition _), VernacSynPure (VernacEndProof _)
  | VernacSynPure (VernacDefinition _), VernacSynPure (VernacProof _)
  | VernacSynPure (VernacFixpoint _), VernacSynPure VernacAbort
  | VernacSynPure (VernacFixpoint _), VernacSynPure (VernacEndProof _)
  | VernacSynPure (VernacProof _), VernacSynPure VernacAbort
  | VernacSynPure (VernacProof _), VernacSynPure (VernacEndProof _)
  | _, VernacSynPure (VernacProof _)
  (* `Eval compute in` *)
  | ( VernacSynPure
        (VernacCheckMayEval
          ( Some
              (Cbv
                {
                  rStrength = Norm;
                  rBeta = true;
                  rMatch = true;
                  rFix = true;
                  rCofix = true;
                  rZeta = true;
                  rDelta = true;
                  rConst = [];
                }),
            _,
            _ )),
      VernacSynPure
        (VernacCheckMayEval
          ( Some
              (Cbv
                {
                  rStrength = Norm;
                  rBeta = true;
                  rMatch = true;
                  rFix = true;
                  rCofix = true;
                  rZeta = true;
                  rDelta = true;
                  rConst = [];
                }),
            _,
            _ )) )
  (* `Compute` *)
  | ( VernacSynPure (VernacCheckMayEval (Some (CbvVm None), _, _)),
      VernacSynPure (VernacCheckMayEval (Some (CbvVm None), _, _)) )
  (* `Check` *)
  | ( VernacSynPure (VernacCheckMayEval (None, _, _)),
      VernacSynPure (VernacCheckMayEval (None, _, _)) )
  | VernacSynterp (VernacNotation _), VernacSynterp (VernacNotation _)
  | ( VernacSynterp (VernacReservedNotation _),
      VernacSynterp (VernacReservedNotation _) )
  | VernacSynterp (VernacDefineModule _), _
  | _, VernacSynterp (VernacEndSegment _)
  | VernacSynPure (VernacSearch _), VernacSynPure (VernacSearch _)
  (* `From ... Require ...` *)
  | ( VernacSynterp (VernacRequire (Some _, _, _)),
      VernacSynterp (VernacRequire (Some _, _, _)) )
  (* `Require ...` *)
  | ( VernacSynterp (VernacRequire (None, _, _)),
      VernacSynterp (VernacRequire (None, _, _)) )
  | VernacSynterp (VernacSetOption _), VernacSynterp (VernacSetOption _)
  | VernacSynPure (VernacLocate _), VernacSynPure (VernacLocate _)
  | VernacSynPure (VernacPrint _), VernacSynPure (VernacPrint _)
  | VernacSynPure (VernacCoercion _), VernacSynPure (VernacCoercion _)
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "Extraction";
              ext_index = 2;
            },
            _ )),
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "Extraction";
              ext_index = 2;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionConstant";
              ext_index = 0;
            },
            _ )),
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionConstant";
              ext_index = 0;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionLanguage";
              ext_index = 0;
            },
            _ )),
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionLanguage";
              ext_index = 0;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionInductive";
              ext_index = 0;
            },
            _ )),
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionInductive";
              ext_index = 0;
            },
            _ )) ) ->
      newline
  | VernacSynPure (VernacDefinition (_, _, ProveBody _)), _
  | VernacSynPure (VernacProof _), _
  | VernacSynPure (VernacStartTheoremProof _), _ ->
      sequence [ newline; increase_indent ]
  | _, VernacSynPure VernacAbort | _, VernacSynPure (VernacEndProof _) ->
      sequence [ newline; decrease_indent ]
  | VernacSynPure (VernacCheckMayEval _), _
  | _, VernacSynPure (VernacCheckMayEval _)
  | VernacSynterp (VernacNotation _), _
  | _, VernacSynterp (VernacNotation _)
  | VernacSynPure (VernacDefinition _), _
  | _, VernacSynPure (VernacDefinition _)
  | VernacSynPure (VernacFixpoint _), _
  | _, VernacSynPure (VernacFixpoint _)
  | VernacSynPure (VernacInductive _), _
  | _, VernacSynPure (VernacInductive _)
  | VernacSynPure (VernacSearch _), _
  | _, VernacSynPure (VernacSearch _)
  | VernacSynterp (VernacReservedNotation _), _
  | _, VernacSynterp (VernacReservedNotation _)
  | VernacSynPure (VernacLocate _), _
  | _, VernacSynPure (VernacLocate _)
  | VernacSynterp (VernacSetOption _), _
  | _, VernacSynterp (VernacSetOption _)
  | VernacSynPure (VernacPrint _), _
  | _, VernacSynPure (VernacPrint _)
  | VernacSynPure (VernacCoercion _), _
  | _, VernacSynPure (VernacCoercion _)
  | VernacSynterp (VernacImport _), _
  | _, VernacSynterp (VernacImport _)
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "Extraction";
              ext_index = 2;
            },
            _ )),
      _ )
  | ( _,
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "Extraction";
              ext_index = 2;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionConstant";
              ext_index = 0;
            },
            _ )),
      _ )
  | ( _,
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionConstant";
              ext_index = 0;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionLanguage";
              ext_index = 0;
            },
            _ )),
      _ )
  | ( _,
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionLanguage";
              ext_index = 0;
            },
            _ )) )
  | ( VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionInductive";
              ext_index = 0;
            },
            _ )),
      _ )
  | ( _,
      VernacSynterp
        (VernacExtend
          ( {
              ext_plugin = "coq-core.plugins.extraction";
              ext_entry = "ExtractionInductive";
              ext_index = 0;
            },
            _ )) )
  | _, VernacSynterp (VernacDefineModule _)
  | VernacSynterp (VernacEndSegment _), _
  | VernacSynPure (VernacEndProof _), _
  | VernacSynPure VernacAbort, _
  | VernacSynterp (VernacRequire _), _
  | _, VernacSynPure (VernacStartTheoremProof _) ->
      blankline
  | VernacSynPure (VernacBullet _), _ -> nop
  | _, _ -> newline

let pp_ast parser =
  (* Do not try to pp after parsing everything. After parsing everything, the
     STM will not be in nested module(s), and things that are valid inside them
     (e.g., notation declarations inside them) will be invalid, making some
     functions (e.g., `grammar_of_notation`) raise exceptions. *)
  match Astparser.next parser with
  | None -> ""
  | Some first_ast ->
      let printer = create () in

      pp_subast first_ast printer;

      let rec loop prev_ast =
        match Astparser.next parser with
        | None -> newline printer
        | Some next_ast ->
            sequence [ separator prev_ast next_ast; pp_subast next_ast ] printer;
            loop next_ast
      in

      let () = loop first_ast in
      contents printer
