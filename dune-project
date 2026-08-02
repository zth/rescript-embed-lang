(lang dune 3.8)

(name rescript-embed-lang)

(package
 (name rescript-embed-lang)
 (synopsis "A general purpose PPX for embedding languages in ReScript")
 (depends
  (ocaml
   (and
    (>= 4.14.0)
    (< 5.1.0)))
  (dune
   (>= 3.8))
  ppxlib
  (quickjs (= 0.5.1))
  yojson))
