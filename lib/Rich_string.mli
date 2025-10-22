(** A string type equipped with an enricher.

    It is a generalization of the idea of augmenting the built-
    in string type with capabilities which are provided by the
    aforementioned enricher. Rich strings are cumulative: you
    can always equip it with more enrichers!

    For example, the enricher can be an ANSI style type, giving
    the ability of colors and other text decorations in the
    terminal to strings.

    Such rich string type preserves the monoidal properties of
    strings, that is, there is an empty string ([Empty]) as
    well as a compositional operator ([++]) that satisfies the
    laws of identity ([Empty ++ s = s ++ Empty = s]) and
    associativity ([(s1 ++ s2) ++ s3 = s1 ++ (s2 ++ s3)]).

    {b Note:} This implementation optimizes for the case of
    joining a list of rich strings together over simple,
    pairwise concatenation as the former is much more
    prevalent.

    As such, ( ++ ) is optimized to reuse lists and keep the
    structure flat as much as possible. Tests must (and will)
    make sure that monoidal laws are preserved. *)

include module type of Specs

(** Given an [Enricher] type, constructs a rich string type. *)
module Make (Enricher : ENRICHER) :
  TYPE with module Enricher = Enricher
