(*  
  control flow graph(CFG)
  CFG is generated from IR
*)

type fml = Fexp of Ir.expression
         | Fnot of fml
         | Fdisj of fml * fml
         | Fconj of fml * fml
         | Feq of Ir.expression * Ir.expression
         | Ftrue
[@@deriving show]

let rec string_of_fml : fml -> string = function
  | Fexp e -> Ir.string_of_expression e
  | Fnot f -> "~" ^ (string_of_fml f)
  | Fdisj (f1, f2) -> "(" ^ (string_of_fml f1) ^ {|\/|} ^ (string_of_fml f2) ^ ")"
  | Fconj (f1, f2) -> "(" ^ (string_of_fml f1) ^ {|/\|} ^ (string_of_fml f2) ^ ")"
  | Feq (f1, f2) -> "(" ^ (Ir.string_of_expression f1) ^ {|=|} ^ (Ir.string_of_expression f2) ^ ")"
  | Ftrue -> "True"

type bb_element = 
  | BB_statement of Ir.statement
  | BB_cond of Ir.expression   (* condition of if and case  *)
[@@deriving show]         

type bb = { id : string;
            pc : Ir.pc; (* program counter for the last branch instruction *)
            bb_elements : bb_element list;
            bb_children : (fml * bb) list;
            reached : bool;
            covered : bool;
            defined_vars : string list
          }
[@@deriving show]         

exception Uncovered_case of string

type cfg = bb * bb (* initial block , exit block *)
[@@deriving show]

type module_data = Ir.type_dict * Ir.assignment list * cfg list * Ir.port_declaration list

let uniq_id = 
  let id = ref 0 in
  fun s -> let tmp = !id in (id := !id + 1; s ^ string_of_int tmp)

let gen_bb s : bb = 
  {id=uniq_id s;
   pc=(-1, -1) ;
   bb_elements=[]; 
   bb_children=[]; 
   reached=false;
   covered=false;
   defined_vars=[]}

let generate_conditions (e : Ir.expression) : Ir.expression option -> fml = function
  | Some exp -> Feq (e, exp)
  | None -> raise @@ Uncovered_case "generate_conditions : a case item is considered to have a condition"

let add_element l x = l @ [x] (* TODO : efficiency *)

let next_is_exit bb_children ext =
  match bb_children with
  | [(_,b)] when b = ext -> true
  | _ -> false

let rec compress (ext : bb) : bb -> bb = fun bb ->
  let rec last_is_statement = function
    | [BB_statement _] -> true
    | _ :: xs -> last_is_statement xs
    | _ -> false in
  let last_is_statement = last_is_statement bb.bb_elements in
  let next_is_exit = next_is_exit bb.bb_children ext in
  match last_is_statement, next_is_exit with
  | true, false -> 
    let child = compress ext @@ snd @@ List.hd bb.bb_children in
    { bb with pc = child.pc; 
              bb_elements = bb.bb_elements @ child.bb_elements;
              bb_children = child.bb_children
    }
  | false, false -> 
    let cs = List.map (fun (f,b) -> (f, compress ext b)) bb.bb_children in
    { bb with bb_children = cs }
  | _, _ -> bb

let rec calc_exp_used_vars : Ir.expression -> string list = function
  | Estring s -> [s]
  | Eident (hid, _) -> [Ir.id_of_hierarchical hid]
  | Econcat es -> List.map calc_exp_used_vars es |> List.flatten
  | Eunary (_, e) -> calc_exp_used_vars e
  | Ebinary (e1, _, e2) -> calc_exp_used_vars e1 @ calc_exp_used_vars e2
  | Econd (e1, e2, e3) -> calc_exp_used_vars e1 @ calc_exp_used_vars e2 @ calc_exp_used_vars e3
  | EInt _ | Efloat _ -> []

let rec calc_statement_defined_vars : Ir.statement -> string list = function
  | Assigns assigns -> List.fold_left (fun acc (s, _, _) -> calc_exp_used_vars s @ acc) [] assigns
  | Seq ss -> List.map calc_statement_defined_vars ss |> List.flatten
  | Br _ | Nope | Case _ -> [] 

let calc_element_defined_vars : bb_element -> string list = function
  | BB_statement s -> calc_statement_defined_vars s
  | BB_cond _ -> []

let calc_bb_defined_set : bb -> bb = fun bb ->
    let defined_vars = List.map calc_element_defined_vars bb.bb_elements |> List.flatten in
    { bb with defined_vars = defined_vars }

let rec calc_defined_set : cfg -> cfg = fun (bb, ext) ->
  if bb = ext then (bb, ext) else
    let bb = calc_bb_defined_set bb in
    { bb with bb_children = List.map (fun (f, bb) -> (f, fst @@ calc_defined_set (bb, ext))) bb.bb_children }, ext

let rec counts_on_nodes : (bb -> bool) -> cfg -> int = fun f (bb, ext) ->
  if bb = ext then 0 else
    let num = List.fold_left (fun acc x -> acc + counts_on_nodes f (snd x, ext)) 0 bb.bb_children in
    if f bb then num + 1 else num


let rec of_always_construct : Ir.always_construct -> cfg =
  fun (Always_comb s) -> 
    let exit_bb = gen_bb "exit" in
    let initial_bb = of_statement (gen_bb "init") exit_bb s in
    (compress exit_bb initial_bb, exit_bb)

(* 
  cur : current basic block
  ext : exit basic block
  of_statement cur ext statement: returns the head of basic blocks from cur to ext
 *)
and of_statement (cur : bb) (ext : bb) : Ir.statement -> bb = function
  | Br (p, e, s1, s2) ->
    let b1, b2 = of_statement (gen_bb "if_then") ext s1, of_statement (gen_bb "if_else") ext s2 in
    { cur with pc = p;
               bb_elements = add_element cur.bb_elements (BB_cond e); (* TODO : efficiency *)
               bb_children = [(Fexp e, b1); (Fnot (Fexp e), b2)] }
  | Case (p, e, ss) ->
    let cs, ss = List.split ss in
    let cs = List.map (fun x -> generate_conditions e x) cs in
    let bs = List.map (fun x -> of_statement (gen_bb "case_br") ext x) ss in
    let children = List.combine cs bs in
    { cur with pc = p;
               bb_elements = add_element cur.bb_elements (BB_cond e); 
               bb_children = children }
  | Nope | Assigns _ as s 
    -> { cur with bb_elements = add_element cur.bb_elements (BB_statement s);
                  bb_children = [Ftrue, ext] }
  | Seq sl -> 
    let rec aux c e = function
      | [] -> e
      | s :: ss ->
        let new_bb = gen_bb "a" in
        let new_bb = aux new_bb e ss in
        let b = of_statement c new_bb s in
        b in
    aux cur ext sl


let of_module_declaration : Ir.module_declaration -> module_data = fun (mh, ml) ->
  let dict = Hashtbl.create 100 in
  let rec aux assigns cfgs : Ir.module_item list -> Ir.assignment list * cfg list = function  
    | [] -> (assigns, List.rev cfgs)
    | mi :: mis -> (match mi with
        | Mdata_declaration d ->
          let (dt, id, _, eo) = d in
          Hashtbl.add dict id (dt, eo); aux assigns cfgs mis
        | Malways_construct ac -> aux assigns (of_always_construct ac :: cfgs) mis
        | MContinuous_assign (e1, e2) -> aux ((e1, "", e2)::assigns) cfgs mis
        | Mparameter_declaration (_, id, _, Some e) -> 
          (* todo: this `dt` is specialized to BFP_BIAS, generalize later *)
          let dt = Ir.(Integer_vector_type ("logic", None, [Dimension_range(EInt (1, 4), EInt (1, 0))])) in
          Hashtbl.add dict id (dt, Some e); 
          aux ((Ir.Eident(([], id), None), "", e)::assigns) cfgs mis
        | Mparameter_declaration (_, _, _, None) ->
          failwith "of_module_declaration : cfg : unexpected parameter"
      ) in
  let rec auxh : Ir.port_declaration list -> unit = function  
    | [] -> ()
    | (dt, id) :: mis -> Hashtbl.add dict id (dt, None); auxh mis in
  let () = auxh mh.input_ports in
  let () = auxh mh.output_ports in
  let (assigns, cfgs) = aux [] [] ml in
  let cfgs = List.map calc_defined_set cfgs in
  (dict, assigns, cfgs, mh.input_ports)

