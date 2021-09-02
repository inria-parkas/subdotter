Subdotter
========

Subdotter reads a graph in dot format and extracts the subgraph specified by 
a list of nodes and other options.

Subdotter is written in [OCaml](https://ocaml.org) and based on the 
[ocamlgraph](http://ocamlgraph.lri.fr) library. It was developed in the 
[Inria](https://www.inria.fr/fr) [PARKAS](https://parkas.di.ens.fr) team.
The original motivation was to visualize chains and cycles in the dependency 
graphs of very large 
[Lustre](https://www-verimag.imag.fr/The-Lustre-Programming-Language-and) 
programs.

Use
---

```
subdotter graph.dot
```

Options
-------

* `-i <path>`: specifies the input graph. This argument is required.

* `-o <path>`: specifies where the output should be written. The default is 
  to send it to stdout.

* `--nodes <path>` : read the list of nodes to extract from the given file
  (the names can be separated by spaces, tabs, newlines, commas, or 
  semicolons).

* `--include-scc` : include as secondary nodes any others in the same 
  strongly connected components as the given nodes.

* `--successors <int>` : include as secondary nodes any that can be reached 
  in the given number of steps from the given nodes.

* `--predecessors <int>` : include as secondary nodes any that can be 
  reached in the given number of backward steps from the given nodes.

* `--no-external-edges` : do not add extra edges to represent paths between 
  extracted nodes that pass via unextracted ones. This makes the calculation 
  much faster since there is no need to calculate transitive closures.

Building
--------

Using [Opam](https://opam.ocaml.org):

1. `opam install dune dune-build-info ocamlgraph`
2. `make`

