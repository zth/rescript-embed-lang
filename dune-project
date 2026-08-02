(lang dune 3.7)

(name rescript-embed-lang)

(package
 (name rescript-embed-lang)
 (synopsis "A general purpose PPX for embedding languages in ReScript")
 (depends
  (ocaml
   (and
    (>= 4.14.0)
    (< 5.0.0)))
  (dune
   (>= 3.7.1))
  ppxlib))
