open Typedtree
open Types

let fresh_var =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    "___" ^ "var" ^ "_" ^ string_of_int v ^ "___"
  in
  body

let count_vars_in_type t =
  let open Types in
  let t = get_desc t in
  match t with
  | Ttuple tl ->
      let rec count_vars_aux t' =
        let t' = get_desc t' in
        match t' with
        | Ttuple tl' -> List.fold_left (fun x y -> x @ count_vars_aux y) [] tl'
        | Tconstr (Path.Pident p, [], _) ->
            if Ident.name p = "unit" then [] else [ fresh_var () ]
        | Tconstr (Path.Pdot (_, s), _, _) ->
            if s = "uint" || s = "sint" || s = "address" then [ fresh_var () ]
            else assert false
        | Tvar _ -> [ fresh_var () ]
        | _ -> assert false
      in
      Some (List.fold_left (fun x y -> x @ count_vars_aux y) [] tl)
  | _ -> None

let flatten_tuple_pat p =
  let rec count_tuple_elem = function
    | Tconstr _ -> 1
    | Ttuple types ->
        List.fold_left
          (fun acc x -> count_tuple_elem x + acc)
          0 (List.map get_desc types)
    | Tvar _ -> 1
    | _ -> assert false
  in
  let rec gen_renaming_ids n =
    if n = 0 then []
    else
      let x = fresh_var () in
      x :: gen_renaming_ids (n - 1)
  in
  let rec flatten_tuple_pat_aux (p, t) =
    match (p, get_desc t) with
    | Tpat_any, _ -> ([], [ fresh_var () ])
    | Tpat_construct (_, { cstr_res = res_t; _ }, [], _), Tconstr (_, [], _)
      -> (
        match get_desc res_t with
        | Tconstr (Path.Pident t_name, _, _) ->
            if Ident.name t_name = "unit" then ([], []) else assert false
        | _ -> assert false)
    | Tpat_var (s, _), t ->
        let n = count_tuple_elem t in
        if n > 1 then
          let renamed = gen_renaming_ids n in
          ([ (Ident.unique_name s, renamed) ], renamed)
        else ([], [ Ident.unique_name s ])
    | Tpat_tuple pat_list, Ttuple type_list ->
        List.fold_left
          (fun acc (x, y) ->
            let renaming, generated_ids =
              flatten_tuple_pat_aux (x.pat_desc, y)
            in
            (fst acc @ renaming, snd acc @ generated_ids))
          ([], [])
          (List.combine pat_list type_list)
    | _ -> assert false
  in
  flatten_tuple_pat_aux (p.pat_desc, p.pat_type)

let same_pat_exp p e =
  let rec aux ps es =
    match (ps, es) with
    | { pat_desc = p; _ } :: p_rest, { exp_desc = e; _ } :: e_rest ->
        (match (p, e) with
        | Tpat_var (p_id, _), Texp_ident (Path.Pident e_id, _, _) ->
            Ident.same p_id e_id
        | Tpat_tuple ps, Texp_tuple es -> aux ps es
        | _, _ -> false)
        && aux p_rest e_rest
    | [], [] -> true
    | _ -> false
  in
  aux [ p ] [ e ]
