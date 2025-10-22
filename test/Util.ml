open Rich_string

module Enricher : ENRICHER with type t = int = struct
  include Int

  let ( = ) = equal
  let enrich = Printf.sprintf "(%i %s)"
end

module Indexed_string = Make (Enricher)
