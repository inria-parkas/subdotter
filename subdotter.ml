(* ******************************************************************* *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique. All rights reserved. This file is distributed under   *)
(*  the BSD 2-Clause licence.                                          *)
(*                                                                     *)
(* 2021 T. Bourke                                                      *)
(* ******************************************************************* *)

(** Versioning *)

let version =
  match Build_info.V1.version () with
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v

let pp_version_string ppf () =
  Format.fprintf ppf "Subdotter, version %s" version

let show_version () =
  Format.printf "%a@," pp_version_string ()

let usage_msg = "usage: subdotter -i <file.dot> [options] [nodes]"

module Nodes = Set.Make
  (struct
    type t = Graph.Dot_ast.node_id
    let compare = Stdlib.compare
  end)

(** Global settings *)

let verbose = ref 1

let input_file = ref None
let output_file = ref None
let nodes = ref Nodes.empty
let other_nodes = ref Nodes.empty

let include_scc = ref false
let include_successors = ref 0
let include_predecessors = ref 0
let include_external_edges = ref false

let set_input_file s =
  input_file := Some s

let set_output_file s =
  output_file := Some s

(** Utilities *)

let printf = Format.printf
let eprintf = Format.printf

let pp_semi p ()  = Format.(pp_print_char p ';'; pp_print_space p ())
let pp_comma p ()  = Format.(pp_print_char p ','; pp_print_space p ())

(** Graphs *)

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
  (struct
    type t = Graph.Dot_ast.node_id
    let compare = Stdlib.compare
    let equal x y = Stdlib.compare x y = 0
    let hash = Hashtbl.hash
  end)
  (struct
    type t = Graph.Dot_ast.attr list
    let compare = Stdlib.compare
    let default = []
   end)

module GB = Graph.Builder.I(G)
module Components = Graph.Components.Make(G)
module Emap = Graph.Gmap.Edge(G)(struct include GB.G include GB end)
module Oper = Graph.Oper.I(G)

(** Graphviz interface *)

module GV = struct (* {{{ *)

  module Dot_ast = Graph.Dot_ast

  let string_of_id = function
    | Dot_ast.Ident s | Dot_ast.Number s | Dot_ast.String s -> s
    | Dot_ast.Html s -> "<" ^ s ^ ">"

  module NodeMap = Map.Make
      (struct
        type t = Dot_ast.node_id
        let compare = Stdlib.compare
      end)

  module EdgeMap = Map.Make
      (struct
        type t = Dot_ast.node_id * Dot_ast.node_id
        let compare = Stdlib.compare
      end)

  type node_attrs = Dot_ast.attr list NodeMap.t
  let no_node_attrs = NodeMap.empty

  (* hack to capture node attributes *)
  let attr_map = Hashtbl.create 1000

  module Parse = Graph.Dot.Parse (Graph.Builder.I (G))
                    (struct
                      let node n attr =
                        Hashtbl.add attr_map n attr; n
                      let edge e = e
                     end)

  let parse path =
    let g = Parse.parse path in
    let attrs = Hashtbl.fold NodeMap.add attr_map NodeMap.empty in
    Hashtbl.reset attr_map;
    (g, attrs)

  let make_id n = (Dot_ast.Ident n, None)

  let make_attr n v =
    [(Dot_ast.Ident n, Some (Dot_ast.String v))]

  let pp_id ppf = function
    | Dot_ast.Ident s -> Format.pp_print_string ppf s
    | Dot_ast.Number s -> Format.pp_print_string ppf s
    | Dot_ast.String s -> Format.fprintf ppf "\"%s\"" s
    | Dot_ast.Html s -> Format.fprintf ppf "<%s>" s

  let string_of_compass_point = function
    | Dot_ast.N  -> ":n"
    | Dot_ast.Ne -> ":ne"
    | Dot_ast.E  -> ":e"
    | Dot_ast.Se -> ":se"
    | Dot_ast.S  -> ":s"
    | Dot_ast.Sw -> ":sw"
    | Dot_ast.W  -> ":w"
    | Dot_ast.Nw -> ":nw"

  let pp_compass_point ppf cpt =
    Format.fprintf ppf ":%s" (string_of_compass_point cpt)

  let string_of_port = function
    | Dot_ast.PortId (id, None) -> string_of_id id
    | Dot_ast.PortId (id, Some cpt) ->
        string_of_id id ^ string_of_compass_point cpt
    | Dot_ast.PortC cpt -> string_of_compass_point cpt

  let pp_port ppf = function
    | Dot_ast.PortId (id, ocpt) ->
        pp_id ppf id; Option.iter (pp_compass_point ppf) ocpt
    | Dot_ast.PortC cpt ->
        pp_compass_point ppf cpt

  let pp_attr ppf (n, ov) =
    Format.open_hbox ();
    pp_id ppf n;
    Option.iter (fun v -> Format.fprintf ppf "=%a" pp_id v) ov;
    Format.close_box ()

  let pp_attrs ppf = function
    | [] -> ()
    | attrs -> Format.(fprintf ppf "@[<hv 2>[%a]@]@,"
                          (pp_print_list ~pp_sep:pp_comma pp_attr) attrs)

  let pp_attrs_list ppf attrs_l =
    Format.open_hvbox 0;
    List.iter (pp_attrs ppf) attrs_l;
    Format.close_box ()

  let string_of_node_id (id, op) =
    match op with
    | None -> string_of_id id
    | Some p -> string_of_id id ^ string_of_port p

  let pp_node_id ppf (x, op) =
    pp_id ppf x;
    (match op with
     | None -> ()
     | Some p -> Format.(pp_print_char ppf ':'; pp_port ppf p))

  let print_vertex node_attrs ppf v =
    pp_node_id ppf v;
    Option.iter (pp_attrs_list ppf) (NodeMap.find_opt v node_attrs);
    pp_semi ppf ()

  let print_edge ppf e =
    Format.fprintf ppf "%a -> %a %a@;"
      pp_node_id (G.E.src e)
      pp_node_id (G.E.dst e)
      pp_attrs_list (G.E.label e)

  let print_dot ?(node_attrs=NodeMap.empty) ppf g =
    let open Format in
    fprintf ppf "digraph@;@[<v 2>{@;";
    G.iter_vertex (print_vertex node_attrs ppf) g;
    G.iter_edges_e (print_edge ppf) g;
    fprintf ppf "@;<0 -2>}@]@."

end (* }}} *)

let add_attrs attrmap id attrs =
  let existing = Option.value (GV.NodeMap.find_opt id attrmap) ~default:[] in
  GV.NodeMap.add id (List.rev_append attrs existing) attrmap

let add_node s =
  nodes := Nodes.add (GV.make_id s) !nodes

let add_other_node s =
  other_nodes := Nodes.add (GV.make_id s) !other_nodes

(** Core algorithms *)

let add_vertexes ns g = Nodes.iter (fun v -> G.add_vertex g v) ns

(* project out the subgraph containing just the vertices in vs, i.e. G[vs]. *)
let subgraph g vs =
  let in_graph e =
    if Nodes.mem (G.E.src e) vs && Nodes.mem (G.E.dst e) vs then Some e else None
  in
  let g' = Emap.filter_map in_graph g in
  add_vertexes vs g';
  g'

let others_from_sccs g nodes =
  let f other scc =
    let scc' = Nodes.diff scc nodes in
    if Nodes.(cardinal scc <> cardinal scc')
    then Nodes.union other scc'
    else other
  in
  let sccs = Array.map Nodes.of_list (Components.scc_array g) in
  Array.fold_left f !other_nodes sccs

let others_from_neighbourhood fold_neighbours k g nodes =
  let rec bdfs n v (visited, other) =
    if n = 0 || (n < k && Nodes.mem v visited) then (visited, other)
    else fold_neighbours (bdfs (n - 1)) g v
          (Nodes.add v visited, if n < k then Nodes.add v other else other)
  in
  snd (Nodes.fold (bdfs k) nodes (nodes, Nodes.empty))

let others_from_successors = others_from_neighbourhood G.fold_succ
let others_from_predecessors = others_from_neighbourhood G.fold_pred

let external_edge_attrs = [
    GV.make_attr "color" "lightgray"
  ]

let add_external_edges ~connected_in_original ~connected_in_projection g =
  let innerfold v1 =
    let check_edge v2 =
      if not (G.V.equal v1 v2)
         && connected_in_original v1 v2
         && not (connected_in_projection v1 v2)
      then G.add_edge_e g (v1, external_edge_attrs, v2)
    in
    G.iter_vertex check_edge g
  in
  G.iter_vertex innerfold g

(** Main functions *)

let read_node_file path =
  let ic = open_in path in
  let add_line s = List.iter add_node (Str.split (Str.regexp "[ \t\n,;]+") s) in
  (try while true do
     add_line (input_line ic)
   done with End_of_file -> ());
  close_in ic

let make_input = function
  | None -> eprintf "no input file specified (-i <path>)@."; exit (-1)
  | Some path -> path

let make_output = function
  | None -> Format.std_formatter
  | Some path -> Format.formatter_of_out_channel (open_out path)

let is_connected_in g =
  let start_time = Sys.time () in
  if !verbose > 0 then
    eprintf "calculating transitive closure (%d nodes/%d edges)...@."
      (G.nb_vertex g) (G.nb_edges g);
  let g' = Oper.add_transitive_closure ~reflexive:false (G.copy g) in
  if !verbose > 0 then eprintf "done (%e seconds)@." (Sys.time () -. start_time);
  fun n1 n2 -> G.mem_edge g' n1 n2

let other_node_attr = [
    GV.make_attr "color" "darkgray" @
    GV.make_attr "fontcolor" "darkgray"
  ]

let main sin fout nodes =
  let g, attrs = GV.parse sin in
  if !verbose > 0 then
    eprintf "input graph: %d nodes/%d edges@." (G.nb_vertex g) (G.nb_edges g);
  if Nodes.is_empty nodes then eprintf "warning: no nodes specified@.";
  other_nodes :=
    (if !include_scc
     then Nodes.union (Nodes.union !other_nodes (others_from_sccs g nodes))
     else Fun.id)
    (Nodes.union
       (others_from_successors !include_successors g nodes)
       (others_from_predecessors !include_predecessors g nodes));
  let g' = subgraph g (Nodes.union nodes !other_nodes) in
  if !include_external_edges then
    add_external_edges
      ~connected_in_original:(is_connected_in g)
      ~connected_in_projection:(is_connected_in g')
      g';
  if !verbose > 0 then
    eprintf "output graph: %d nodes/%d edges@." (G.nb_vertex g') (G.nb_edges g');
  let attrs' = Nodes.fold (fun v cattrs ->
      add_attrs cattrs v other_node_attr) !other_nodes attrs
  in
  Format.(
    pp_open_vbox fout 0;
    GV.print_dot ~node_attrs:attrs' fout g';
    pp_close_box fout ()
  )

let _ =
  begin try
    Format.open_vbox 0;
    Arg.parse
    [
        "--version",
          Arg.Unit show_version,
          " Show the version number";

        "-v",
          Arg.Unit (fun () -> Printexc.record_backtrace true;
                              incr verbose),
          " Be more verbose";

        "-q",
          Arg.Unit (fun () -> decr verbose),
          " Be less verbose";

        "-i",
          Arg.String set_input_file,
          "<path> Specify the input file";

        "-o",
          Arg.String set_output_file,
          "<path> Specify the output file (default: stdout)";

        "--nodes",
          Arg.String read_node_file,
          "<path> Include the nodes listed in the given file";

        "--include-scc",
        Arg.Set include_scc,
        "Include secondary nodes from strongly connected components";

        "--successors",
        Arg.Set_int include_successors,
        "<int> Include secondary nodes from a bounded number of successsors";

        "--predecessors",
        Arg.Set_int include_predecessors,
        "<int> Include secondary nodes from a bounded number of predecessors";

        "--external-edges",
        Arg.Set include_external_edges,
        "Add edges to represent paths through other nodes";
    ]
    add_node usage_msg;
    main (make_input !input_file) (make_output !output_file) !nodes
  with Failure e ->
    (eprintf "subdotter failed:@ %s@," e;
     if !verbose > 0 then eprintf "%s@," (Printexc.get_backtrace ()))
  end;
  Format.close_box ();
  Format.print_flush ()

