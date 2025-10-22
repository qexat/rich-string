open Util

module Test_plus_plus = struct
  open Indexed_string

  (* left identity *)

  let%test "empty ++ string s = string s" =
    empty ++ string "world" = string "world"

  let%test "empty ++ enrich e rs = enrich e rs" =
    empty ++ enrich 1 (string "world")
    = enrich 1 (string "world")

  let%test "empty ++ join sep rss = join sep rss" =
    empty ++ join ~on:empty [ string "hello"; string "world" ]
    = join ~on:empty [ string "hello"; string "world" ]

  (* right identity *)

  let%test "string s ++ empty = string s" =
    string "hello" ++ empty = string "hello"

  let%test "enrich e rs ++ empty = enrich e rs" =
    enrich 0 (string "hello") ++ empty
    = enrich 0 (string "hello")

  let%test "join sep rss ++ empty = join sep rss" =
    join ~on:empty [ string "hello"; string "world" ] ++ empty
    = join ~on:empty [ string "hello"; string "world" ]

  (* associativity *)

  let%test "(empty ++ rs1) ++ rs2 = empty ++ (rs1 ++ rs2)" =
    empty ++ string "hello" ++ string "world"
    = empty ++ (string "hello" ++ string "world")

  let%test "(rs1 ++ empty) ++ rs2 = rs1 ++ (empty ++ rs2)" =
    string "hello" ++ empty ++ string "world"
    = string "hello" ++ (empty ++ string "world")

  let%test "(rs1 ++ rs2) ++ empty = rs1 ++ (rs2 ++ empty)" =
    string "hello" ++ string "world" ++ empty
    = string "hello" ++ (string "world" ++ empty)

  (* 1st optimization does not break associativity *)

  let%test
      "(join sep rss1 ++ join sep rss2) ++ rs = join sep rss1 \
       ++ (join sep rss2 ++ rs)"
    =
    join ~on:(string ", ") [ string "foo"; string "bar" ]
    ++ join ~on:(string ", ") [ string "qux"; string "lez" ]
    ++ string "hello"
    = join ~on:(string ", ") [ string "foo"; string "bar" ]
      ++ (join ~on:(string ", ") [ string "qux"; string "lez" ]
          ++ string "hello")

  let%test
      "(rs ++ join sep rss1) ++ join sep rss2 = rs ++ (join \
       sep rss1 ++ join sep rss2)"
    =
    string "hello"
    ++ join ~on:(string ", ") [ string "foo"; string "bar" ]
    ++ join ~on:(string ", ") [ string "qux"; string "lez" ]
    = string "hello"
      ++ (join ~on:(string ", ") [ string "foo"; string "bar" ]
          ++ join
               ~on:(string ", ")
               [ string "qux"; string "lez" ])

  (* 2nd optimization does not break associativity *)

  let%test
      "(join empty rss ++ rs1) ++ rs2 = join empty rss ++ (rs1 \
       ++ rs2)"
    =
    join ~on:empty [ string "foo"; string "bar" ]
    ++ string "hello"
    ++ string "world"
    = join ~on:empty [ string "foo"; string "bar" ]
      ++ (string "hello" ++ string "world")

  let%test
      "(rs1 ++ join empty rss) ++ rs2 = rs1 ++ (join empty rss \
       ++ rs2)"
    =
    string "hello"
    ++ join ~on:empty [ string "foo"; string "bar" ]
    ++ string "world"
    = string "hello"
      ++ (join ~on:empty [ string "foo"; string "bar" ]
          ++ string "world")

  let%test
      "(rs1 ++ rs2) ++ join empty rss = rs1 ++ (rs2 ++ join \
       empty rss)"
    =
    string "hello"
    ++ string "world"
    ++ join ~on:empty [ string "foo"; string "bar" ]
    = string "hello"
      ++ (string "world"
          ++ join ~on:empty [ string "foo"; string "bar" ])

  (* 3rd optimization does not break associativity *)

  let%test
      "(enrich e rs1 ++ enrich e rs2) ++ rs3 = enrich e rs1 ++ \
       (enrich e rs2 ++ rs3)"
    =
    enrich 0 (string "hello")
    ++ enrich 0 (string "world")
    ++ string "foo"
    = enrich 0 (string "hello")
      ++ (enrich 0 (string "world") ++ string "foo")

  let%test
      "(rs1 ++ enrich e rs2) ++ enrich e rs3 = rs1 ++ (enrich \
       e rs2 ++ enrich e rs3)"
    =
    string "hello"
    ++ enrich 1 (string "foo")
    ++ enrich 1 (string "bar")
    = string "hello"
      ++ (enrich 1 (string "foo") ++ enrich 1 (string "bar"))
end

module Test_flatten = struct
  open Indexed_string

  (* this test may seem useless but it is to provide guarantees
     in case the type representation changes *)

  let%test "flatten joins on the empty rich string" =
    String.(
      equal
        (render (flatten [ string "hello"; string "world" ]))
        "helloworld")
end

module Test_join_lines = struct
  open Indexed_string

  let%test "join_lines joins on the newline rich string" =
    String.(
      equal
        (render (join_lines [ string "hello"; string "world" ]))
        "hello\nworld")
end

module Test_is_empty = struct
  open Indexed_string

  let ( = ) = Bool.equal
  let%test "is_empty empty" = is_empty empty
  let%test "is_empty (join sep [])" = is_empty (join [])

  let%test "is_empty (enrich e empty)" =
    is_empty (enrich 0 empty)

  let%test "is_empty (enrich e rs) = is_empty rs" =
    is_empty (enrich 0 (string "hello"))
    = is_empty (string "hello")

  let%test "is_empty (join empty [empty; empty])" =
    is_empty (join ~on:empty [ empty; empty ])

  let%test "is_empty (join empty [rs]) = is_empty rs" =
    is_empty (join ~on:empty [ string "hello" ])
    = is_empty (string "hello")
end

module Test_is_enriched = struct
  open Indexed_string

  let ( = ) = Bool.equal
  let%test "not (is_enriched empty)" = not (is_enriched empty)

  let%test "not (is_enriched (string s))" =
    not (is_enriched (string "hello"))

  let%test "is_enriched (enrich e rs)" =
    is_enriched (enrich 1 (string "hello"))

  let%test "not (is_enriched (join empty [empty; empty]))" =
    not (is_enriched (join ~on:empty [ empty; empty ]))

  let%test "is_enriched (join empty [rs]) = is_enriched rs" =
    is_enriched (join ~on:empty [ string "hello" ])
    = is_enriched (string "hello")

  let%test "is_enriched (join sep []) = is_enriched sep" =
    is_enriched (join ~on:(string "hello") [])
    = is_enriched (string "hello")
end

module Test_render = struct
  open Indexed_string

  let ( = ) = String.equal
  let%test {|render empty = ""|} = render empty = ""

  let%test "render (string s) = s" =
    render (string "hello") = "hello"

  let%test {|render (enriched 0 s) = "(0 " ^ s ^ ")"|} =
    render (enrich 0 (string "hello")) = "(0 hello)"

  let%test {|render (join sep []) = ""|} =
    render (join ~on:(string ", ") []) = ""

  let%test {|render (join sep [empty; empty]) = ""|} =
    render (join ~on:(string ", ") [ empty; empty ]) = ""

  let%test
      {|render (join ", " [rs1; rs2]) = render rs1 ^ ", " ^ render rs2|}
    =
    render
      (join ~on:(string ", ") [ string "hello"; string "Jane" ])
    = "hello, Jane"
end
