# Rich String

`rich-string` provides a generic way to enrich the built-in `string` type with new capabilities.

The resulting rich string type is powerful: simple, composable, optimizing and extensible.

## Simple

The type is made of four straightforward variants:

- `Empty`
- `String (built-in string)`
- `Enriched (enrichment, rich string)`
- `Join (separator, rich strings)`

## Composable

Just like the built-in `string`, rich strings form a monoid.

- Identity: for all rich strings `rs`, `empty ++ rs = rs ++ empty = rs`.
- Associativity: for all rich strings `rs1`, `rs2` and `rs3`, `(rs1 ++ rs2) ++ rs3 = rs1 ++ (rs2 ++ rs3)`.

## Optimizing

Rich strings are carefully crafted for common cases. Operations on rich strings avoid unnecessary structural deep-nesting when possible.

- Identity: `empty ++ rs = rs ++ empty = rs`.
- Inner concatenation: `join ~on:sep rss1 ++ join ~on:sep rss2 = join ~on:sep (rss1 @ rss2)`.
- Inner consing: `rs ++ join ~on:empty rss = join ~on:empty (rs :: rss)`.
- Enrichement grouping: `enrich e rs1 ++ enrich e rs2 = enrich e (rs1 ++ rs2)`.

## Extensible

With a mostly transparent interface, rich strings can be extended to fulfill specific needs.

For example:

- Indexing
- Labelling
- ANSI formatting

## Tested

As a library, `rich-string` is tested from the ground-up, with a focus on checking that its advertised properties hold.

## Convinced?

Add `rich-string` to your `dune-project`, in the `depends` section:

```lisp
(package
  (name your-package)
  (depends
    ...
    (rich-string (>= 1.0))))
```

`rich-string` is available on the OPAM index.

```sh
opam install rich-string
```

---

Made with 🩷 by [lexa](https://github.com/qexat).
Support me on [Ko-fi](https://ko-fi.com/qexat).
Find me on [Bluesky](https://bsky.app/profile/lexa.qexat.com).

`rich-string`'s development does not involve any large language model.

[![rich-string is entirely brain-made.](https://brainmade.org/black-logo.svg)](https://brainmade.org)
