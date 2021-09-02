(lang dune 2.8)
(name subdotter)

(generate_opam_files true)

(source (github inria-parkas/subdotter))
(license BSD2)
(authors "Timothy Bourke")
(maintainers "tim@tbrk.org")

(package
  (name subdotter)
  (synopsis "Extract subgraphs from dot files")
  (description "Subdotter is a tool for extracting subgraphs from large
               graphs. It reads a file in the Graphviz dot format, takes a
               specification of the nodes and edges to extract, and produces
               another dot file.")
  (depends
    (ocamlgraph (>= 1.8.8))))

