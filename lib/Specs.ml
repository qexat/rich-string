(** An [ENRICHER] is a type that represents a capability that
    can be added to strings. *)
module type ENRICHER = sig
  type t

  val ( = ) : t -> t -> bool
  val enrich : t -> string -> string
end

module type TYPE = sig
  (** The type of a rich string. *)

  (** The enricher. *)
  module Enricher : ENRICHER

  (** The type of a rich string. *)
  type t =
    | Empty
    | String of string
    | Enriched of Enricher.t * t
    | Join of t * t list

  (** [rs1 = rs2] determines whether [rs1] and [rs2] are equal.

      {b IMPORTANT:} no pruning or optimization is performed.
      This means that two rich strings that render the same
      might not be considered equal. *)
  val ( = ) : t -> t -> bool

  (** [empty] is the identity string with respect to {!( ++ )}. *)
  val empty : t

  (** [rs1 ++ rs2] appends [rs2] at the end of [rs1].

      It is equivalent to the built-in string operator [^].

      {[
        # print (String "hello" ++ String "world")
        "helloworld"
        - : unit = ()
      ]} *)
  val ( ++ ) : t -> t -> t

  (** [string s] lifts the built-in string [s] to the enriched
      world.

      For example, if the enricher is an ANSI style type, this
      enables the ability to stylize [s]. *)
  val string : string -> t

  (** [enrich enrichment rs] adds the [enrichment] to the rich
      string [rs].

      For example, if the enricher is an ANSI style type, this
      translates to adding a new style to [s]. *)
  val enrich : Enricher.t -> t -> t

  (** [join ?on:sep rss] concatenates all [rss] into one,
      separated by [sep]. By default, [sep] is a single space.

      It is equivalent to the built-in string function
      [String.concat]. *)
  val join : ?on:t -> t list -> t

  (** [flatten rss] concatenates all [rss] into one.
  
      It is equivalent to {!join}[~on:Empty rss]. *)
  val flatten : t list -> t

  (** [join_lines rss] concatenates all [rss] into one,
      separated by line feeds.

      It is equivalent to {!join}[~on:(String "\n") rss]. *)
  val join_lines : t list -> t

  (** [is_empty rs] tests whether [rs] is [Empty]. *)
  val is_empty : t -> bool

  (** [is_enriched rs] tests whether [rs] is {i not} just a
      plain [String]. *)
  val is_enriched : t -> bool

  (** [render rs] generates a built-in string such that [rs]
      can be used in interfaces that expect strings, e.g. IO. *)
  val render : t -> string

  (** [print ?out ?ending rs] prints [rs] in [out], appending
      [ending] after. [out] defaults to [stdout] and [ending]
      defaults to [Some (String "\n")]. *)
  val print : ?out:out_channel -> ?ending:t option -> t -> unit
end
