(lang dune 3.20)

(name rich-string)

(generate_opam_files true)

(source
 (github qexat/rich-string))

(authors "lexa <hello@lexa.qexat.com>")

(maintainers "lexa <hello@lexa.qexat.com>")

(license MIT)

(documentation https://qexat.github.io/rich-string/)

(package
 (name rich-string)
 (synopsis
  "rich-string provides a generic way to enrich the built-in string type with new capabilities.")
 (description
  "rich-string provides a generic way to enrich the built-in string type with new capabilities.")
 (depends
  (ocaml
   (>= 5.1))
  (ppx_expect :with-test))
 (tags
  (string rich "rich string" formatting)))
