(*
 Generate dot image of CFG
*)

module Node = struct
  type t = string * bool * bool
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = string
  let compare = Pervasives.compare
  let equal = (=)
  let default = ""
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* more modules available, e.g. graph traversal with depth-first-search *)
module D = Graph.Traverse.Dfs(G)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
    include G (* use the graph module from above *)
    let red = 0xee0000
    let green = 0x007700
    let edge_attributes ((_, a, _), e, (sb, b, _)) = 
      let c = match a, b with
        | true, true -> green
        | true, _ -> if String.sub sb 1 4 = "exit" then green else red
        | _, _ -> red
      in [`Label e; `Color c]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes = function 
      | (_, r, false) -> if r then [`Shape `Box; `Color green; `Style `Bold] else [`Shape `Box; `Color red; `Style `Bold]
      | (_, true, true) -> [`Shape `Box; `Color green; `Style `Bold; `Fontcolor green]
      | _ -> failwith "color vertex_attributes"
    let vertex_name = fun (a, _, _) -> a
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let string_of_expression = Ir.string_of_expression

let string_of_bb_elements bbs = 
  let open Cfg in
  let aux = function
    | BB_statement s -> Ir.string_of_statement s
    | BB_cond e -> Printf.sprintf "Cond : %s" (string_of_expression e) in
  List.fold_right (fun x acc -> Printf.sprintf {|%s\n%s|} (aux x) acc) bbs ""

open Cfg

let get_node_name : bb -> string * bool * bool = fun bb ->
  let id = match bb.id with "" -> "id" | x -> x in
  let pc = match bb.pc with
    | (-1, -1) -> ""
    | (x, y) -> Printf.sprintf "(%d,%d)" x y in
  let elements = Printf.sprintf "%s\n%s%s" id (string_of_bb_elements bb.bb_elements) pc in
  let defined_vars = List.fold_left (fun acc x -> Printf.sprintf "%s\n%s" acc x) "" bb.defined_vars in
  Printf.sprintf {|"%s\n------------------------------\ndefined_variables:%s"|} elements defined_vars, bb.reached, bb.covered

let rec of_bb  (g : G.t) : bb -> G.t = fun bb ->
  let rec mk_g g cs bn =
    match cs with
    | [] -> g
    | (f, b)::es -> 
      let g = mk_g g es (bn+1) in
      let g = of_bb g b in
      let v1 = get_node_name bb in
      let v2 = get_node_name b in
      let e = Printf.sprintf {|%d:%s|} bn (string_of_fml f) in
      G.add_edge_e g (v1, e, v2) in
  mk_g g bb.bb_children 1

let of_cfg (ig, _) = of_bb G.empty ig

let visualize filename cfg =
  let g = of_cfg cfg in
  let file = open_out_bin filename in
  Dot.output_graph file g

