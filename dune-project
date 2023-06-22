(lang dune 3.0)
(using mdx 0.2)
(name ldl)
(generate_opam_files true)
(source
 (github c-cube/ldl))

(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name ldl)
 (synopsis "A basic event loop for OCaml 5")
 (depends (ocaml (>= 5.0)) dune poll (mdx :with-test))
 (tags (ocaml5 fiber pool)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
