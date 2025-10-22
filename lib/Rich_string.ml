include Specs

module Make (Enricher : ENRICHER) :
  TYPE with module Enricher = Enricher = struct
  module Enricher = Enricher

  (* This implementation optimizes for the case of joining a
     list of rich strings together over simple, pairwise
     concatenation as the former is much more prevalent.
     
     As such, ( ++ ) is optimized to reuse lists and keep the
     structure flat as much as possible. Tests must (and will)
     make sure that monoidal laws are preserved. *)

  type t =
    | Empty
    | String of string
    | Enriched of Enricher.t * t
    | Join of t * t list

  let rec ( = ) rs1 rs2 =
    match (rs1, rs2) with
    | (Empty, Empty) -> true
    | (String s1, String s2) -> String.equal s1 s2
    | (Enriched (e1, rs1), Enriched (e2, rs2)) ->
      Enricher.(e1 = e2) && rs1 = rs2
    | (Join (sep1, rss1), Join (sep2, rss2)) ->
      sep1 = sep2 && List.for_all2 ( = ) rss1 rss2
    | (_, _) -> false

  let empty = Empty
  let string s = String s
  let enrich e rs = Enriched (e, rs)
  let join ?on:(sep = String " ") rss = Join (sep, rss)
  let flatten rss = join ~on:Empty rss
  let join_lines rss = join ~on:(String "\n") rss

  (* depending on the output, concatenation will sometimes
     perform optimizations to produce structures as flat as
     possible. *)
  let rec decide_concat_optimization rs1 rs2 =
    match (rs1, rs2) with
    (* left identity *)
    | (Empty, _) -> Some rs2
    (* right identity *)
    | (_, Empty) -> Some rs1
    (* first optimization: if rs1 and rs2 are both lists
       joined by the same separator, we can simply concatenate
       both lists together, which avoids ending with many
       nested Joins of only two elements. *)
    | (Join (sep1, rss1), Join (sep2, rss2)) when sep1 = sep2 ->
      Some (Join (sep1, optimize_list (rss1 @ rss2)))
    (* second optimization: if rs2 is an empty-joined list of
       rich strings, we can simply cons rs1 to it for the same
       reason as the first optimization. *)
    | (_, Join (Empty, rss)) ->
      Some (Join (Empty, optimize_list (rs1 :: rss)))
    (* to preserve associativity, we must perform the above
       optimization on the left handside as well, even though
       it is less efficient. *)
    | (Join (Empty, rss), _) ->
      Some (Join (Empty, optimize_list (rss @ [ rs2 ])))
    (* third optimization: concatenating two strings that have
       the same enrichment results in reusing the enrichment
       and concatenating the underlying rich strings instead. *)
    | (Enriched (e1, rs1), Enriched (e2, rs2))
      when Enricher.(e1 = e2) ->
      Some (Enriched (e1, rs1 ++ rs2))
    (* IMPORTANT: do NOT do any optimizations on [String]!
       concatenating the underlying strings BREAK ASSOCIATIVITY
       and are not even worth it performance-wise! *)
    | (_, _) -> None

  and ( ++ ) rs1 rs2 =
    match decide_concat_optimization rs1 rs2 with
    | Some rs -> rs
    | None -> flatten [ rs1; rs2 ]

  and cons_optimized current processed =
    match processed with
    | [] -> current :: []
    | first :: rest ->
      (match decide_concat_optimization current first with
       | Some rs -> rs :: rest
       | None -> current :: processed)

  and optimize_list (rss : t list) =
    List.(fold_right cons_optimized rss [])

  let rec is_empty = function
    | Empty | Join (_, []) -> true
    | String _ -> false
    | Enriched (_, rs) -> is_empty rs
    | Join (sep, rss) ->
      is_empty sep && List.for_all is_empty rss

  let rec is_enriched = function
    | Empty | String _ -> false
    | Enriched _ -> true
    | Join (sep, rss) ->
      is_enriched sep || List.exists is_enriched rss

  let rec render = function
    | Empty -> ""
    | String s -> s
    | Enriched (e, rs) -> Enricher.enrich e (render rs)
    | Join (sep, rss) ->
      rss
      |> List.filter (Fun.negate is_empty)
      |> List.map render
      |> String.concat (render sep)

  let print ?(out = stdout) ?(ending = Some (String "\n")) rs =
    let rendered_ending =
      ending |> Option.map render |> Option.value ~default:""
    in
    Printf.fprintf out "%s%s" (render rs) rendered_ending
end
