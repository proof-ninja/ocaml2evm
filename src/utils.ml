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
    | ( Tpat_construct (_, { Types.cstr_name = "()"; _ }, [], _),
        Tconstr (_, [], _) ) ->
        ([], [])
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
