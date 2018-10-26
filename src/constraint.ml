(* 
  run CFG and collect constraints
*)

module S = Set.Make(struct type t = string let compare = Pervasives.compare end)
let flatmap f ls = List.flatten @@ List.map f ls

type constr_type = CTint | CTfloat | CTbitvector of int | CTnone
[@@deriving show]

let bin_ops = ["+"; "-"; "<<"; ">>"; "||"; "&&"; "|"; "&"; "=="; ">"; "<"; "^"]
let un_ops = ["!"; "~"; "|"; "&"]

type dimension = 
  | CDrange of constr * constr   
  | CDat of constr

and constr =
  | CPint of int
  | CPfloat of float
  | CPconstant of constr_type * string
  | CPbitvector of int * int
  | CPbin of string * constr * constr
  | CPun of string * constr
  | CPdim of constr * dimension (* Cdim (c,a,b) rep c[a:b] *)
  | CPconcat of constr list
  | CPcond of constr * constr * constr
  | Ceq of constr * constr
  | Clt of constr * constr
  | Cgt of constr * constr
  | Cnot of constr
  | Cor of constr * constr
  | Cxor of constr * constr  
  | Cand of constr * constr
  | Cvalid of constr
[@@deriving show]

let ctt = Ceq (CPbitvector (1, 1), CPbitvector (1, 1))
let cff = Ceq (CPbitvector (1, 1), CPbitvector (1, 0))
let fold_constr x f = function
  | [] -> x
  | [x] -> x
  | x::xs -> List.fold_left f x xs

type constr_select = bool * bool * constr list


(* functions to create constraints from bb *)
let rec of_data_type (td : Ir.type_dict) : Ir.data_type -> constr_type = function
  | Integer_vector_type ("logic", None, []) -> CTbitvector 1
  | Integer_vector_type ("logic", None, [d]) -> 
    (match of_dimension td d with
     | CDrange (CPint a, CPint b) -> CTbitvector(a-b+1)
     | _ -> failwith "of_data_type: constratin"
    )
  | Implicit_data_type _ -> CTnone  
  | e -> failwith @@ Printf.sprintf "of_data_type: constratin : %s" (Ir.show_data_type e)

and of_dimension (td : Ir.type_dict) : Ir.dimension -> dimension = function
  | Dimension_range (e1, e2) -> CDrange (of_expression false td e1, of_expression false td e2)
  | Dimension_index (e) -> CDat (of_expression false td e)

and of_expression top (td : Ir.type_dict) : Ir.expression -> constr = function
  | EInt (1, n) -> CPint n
  | EInt (d, n) when d > 1 -> CPbitvector (d, n)
  | Efloat f -> CPfloat f
  | Eident (hid, s) when not top ->
    let id = Ir.id_of_hierarchical hid in
    let idc = (match Hashtbl.find td id with
        | (ty, _) -> CPconstant (of_data_type td ty, id)) in
    (match s with
     | None -> idc
     | Some d -> CPdim (idc, of_dimension td d))
  | Econcat es -> CPconcat (List.map (fun x -> of_expression false td x) es)  
  | Eunary (op, e) when List.mem op un_ops ->
    let e = of_expression false td e in
    CPun (op, e)
  | Ebinary (e1, op, e2) when List.mem op bin_ops -> 
    let e1, e2 = of_expression false td e1, of_expression false td e2 in
    CPbin (op, e1, e2)
  | Econd (p, e1, e2) -> 
    let p, e1, e2 = of_expression false td p, of_expression false td e1, of_expression false td e2 in
    CPcond (p, e1, e2)
  | Eunary (op, e) ->
    let e = of_expression false td e in
    (match op with
     | "!" -> Cnot e
     | s -> failwith @@ Printf.sprintf "of_expression: constraint: unary %s" s
    )
  | Ebinary (e1, op, e2) -> 
    let e1, e2 = of_expression false td e1, of_expression false td e2 in
    (match op with
     | ">" -> Cgt (e1, e2)
     | "<" -> Clt (e1, e2)
     | "==" -> Ceq (e1, e2)
     | "^" -> Cxor (e1, e2)
     | s -> failwith @@ Printf.sprintf "of_expression: constraint: binary %s" s
    )
  | e -> failwith @@ Printf.sprintf "of_expression: constratint: %s" (Ir.show_expression e)

let of_assignment (td : Ir.type_dict) : Ir.assignment -> constr = function
    (e1, _, e2) -> Ceq (of_expression false td e1, of_expression false td e2)

let of_statement (td : Ir.type_dict) : Ir.statement -> constr list = function
  | Nope -> []
  | Assigns s -> List.map (of_assignment td) s
  | _ -> failwith "of_statement: constraint: unexpected statement"

let rec of_elements (td : Ir.type_dict) : Cfg.bb_element list -> constr list = function
  | [] -> []
  | [BB_statement s] -> of_statement td s
  | (BB_statement s) :: es -> (of_statement td s) @ of_elements td es
  | (BB_cond _) :: es -> of_elements td es

let of_fml_primary (td : Ir.type_dict) : Cfg.fml -> constr = function
  | Fexp e -> of_expression false td e
  | e -> failwith @@ Printf.sprintf "of_fml_primary: constraint: %s" (Cfg.show_fml e)  

let rec of_fml (td : Ir.type_dict) : Cfg.fml -> constr = function
  | Fexp e -> of_expression false td e
  | Fnot f -> Cnot (of_fml td f)
  | Feq (e1, e2) -> Ceq (of_expression false td e1, of_expression false td e2)
  | Ftrue -> ctt
  | Fconj (f1, f2) -> Cand (of_fml td f1, of_fml td f2)
  | Fdisj (f1, f2) -> Cor (of_fml td f1, of_fml td f2)


(* [gen_constrs _ _ _ (bb, _)] generates constraints from [bb] to an unreached basic-block *)
let rec gen_constrs trace td cs : Cfg.cfg -> constr list = fun (bb, ext) ->
  match List.find_opt (fun (_, bb) -> not bb.Cfg.reached) bb.bb_children with
  | Some (f, _) ->
    let c = of_fml td f in
    let es = of_elements td bb.bb_elements in
    let () = if trace then Printf.printf "Add\n %s\n to constraints\n" (show_constr c) in
    c :: es @ cs
  | None -> (match List.find_opt (fun (_, bb) -> not bb.Cfg.covered) bb.bb_children with
      | Some (f, bb) -> 
        let c = of_fml td f in
        let es = of_elements td bb.bb_elements in
        let () = if trace then Printf.printf "Add\n %s\n to constraints\n" (show_constr c) in
        gen_constrs trace td (c :: es @ cs) (bb, ext)
      | None -> [])

(* generates constraints of all path in a given graph *)
let rec all_constraints td : Cfg.cfg -> constr list list =
  fun (bb, ext) ->
    if Cfg.next_is_exit bb.bb_children ext then
      [(of_fml td @@ fst @@ List.hd bb.bb_children) :: of_elements td bb.bb_elements]
    else
      let res = List.map (fun (f, bb) -> 
          flatmap (fun x -> 
              (of_fml td f) :: x)
            (all_constraints td (bb, ext)))
          bb.bb_children in
      let c = of_elements td bb.bb_elements in
      List.map (fun x -> c @ x) res

(* [[p1..], [p2...]] -> c -> \/(/\[p1...], /\[p2...]) *)
let all_constraints_one : constr list list -> constr = fun css ->
  let () = List.iter (fun cs -> (List.iter (fun c -> Printf.printf "%s\n" @@ show_constr c) cs; Printf.printf "\n########\n")) css in
  let and_list = List.map (fun ps -> List.fold_left (fun acc p -> Cand (acc, p)) ctt ps) css in
  List.fold_left (fun acc p -> Cor (acc, p)) cff and_list

let rec constr_used_vars : constr -> string list = function
  | CPconstant (_, s) -> [s]
  | Cnot c | Cvalid c | CPun (_, c) 
    -> constr_used_vars c
  | CPdim (c1, CDat c2) | Ceq (c1, c2) | Clt (c1, c2) | Cgt (c1, c2)
  | Cor (c1, c2) | Cxor (c1, c2) | Cand (c1, c2) | CPbin (_, c1, c2) 
    -> constr_used_vars c1 @ constr_used_vars c2
  | CPcond (c1, c2, c3) | CPdim (c1, CDrange(c2, c3)) 
    -> constr_used_vars c1 @ constr_used_vars c2 @ constr_used_vars c3
  | CPconcat cs -> flatmap constr_used_vars cs
  | CPint _ | CPfloat _ | CPbitvector _
    -> []

(* generates a constratint for each path with defined variables *)
let rec gen_constr_path td : Cfg.cfg -> (constr list * S.t) list =
  fun (bb, ext) ->
    if Cfg.next_is_exit bb.bb_children ext then
      [((of_fml td @@ fst @@ List.hd bb.bb_children) :: of_elements td bb.bb_elements), S.of_list bb.defined_vars]
    else 
      let c = of_elements td bb.bb_elements in
      flatmap (fun (f, bb_) -> 
          List.map (fun (cs, ds) -> 
              c @ (of_fml td f) :: cs, S.union (S.of_list bb.defined_vars) ds)
            (gen_constr_path td (bb_, ext)))
        bb.bb_children

(* generates constraints along with uncovered basic-blocks *)
let rec of_cfg trace (td : Ir.type_dict) (cs : constr list)
    ((bb, ext) : Cfg.cfg)
  : int list -> constr list = function
  | [] -> 
    let es = of_elements td bb.bb_elements in
    es @ cs
  | n :: ns ->
    let es = of_elements td bb.bb_elements in
    let (f, ce_next) = List.nth bb.bb_children n in
    if not ce_next.Cfg.covered then (* if next bb is not covered, visit *)      
      let c = of_fml td f in
      of_cfg trace td (c :: es @ cs) (ce_next, ext) ns
    else (* if next bb is covered, find other child to visit *)
      (match List.find_opt (fun (_, bb) -> not bb.Cfg.covered) bb.bb_children with
       | None -> cs (* if all children are covered, finish *)
       | Some (f, bb) -> (match bb.Cfg.reached with
           | false ->
             let c = of_fml td f in
             c :: es @ cs (* select! *) 
           | true -> 
             let c = of_fml td f in
             gen_constrs trace td [] (bb, ext) @ (c :: es @ cs)))

let rec update path : Cfg.cfg -> Cfg.cfg = function (bb, ext) ->
  let rec replace n t = function
    | [] -> []
    | _ :: xs when n = 0 -> t :: xs
    | x :: xs -> x :: replace (n - 1) t xs in
  let aux = function
    | [] -> { bb with reached = true;
                      covered = true }
    | n :: ns -> 
      let (f, s) = List.nth bb.Cfg.bb_children n in
      let (cfg_, _) = update ns (s, ext)  in
      let new_child = replace n (f, cfg_) bb.Cfg.bb_children in
      { bb with Cfg.bb_children = new_child;
                reached = true;
                covered = List.fold_left (fun acc x -> acc && (snd x).Cfg.covered) true new_child }
  in aux path, ext

let of_cfg test trace vcd td = fun cfg -> 
  let ns = Ce.gen_ce_path test trace vcd cfg in
  let res = update ns cfg in
  let cs = of_cfg trace td [] res ns in
  res, cs

let of_module_data test trace vcd = fun (dict, assigns, cfgs) ->
  let assigns_cs = 
    List.map (fun x -> of_assignment dict x) assigns in
  let rec aux not_covered res_cfgs res_constraints covered_constraints = function
    | [] -> List.rev res_cfgs, res_constraints, List.rev covered_constraints
    | g::gs ->
      let res, cs = of_cfg test trace vcd dict g in
      if cs = [] then (* g is covered *)
        let constr_paths = gen_constr_path dict g in
        aux not_covered (res :: res_cfgs) res_constraints (constr_paths :: covered_constraints) gs
      else
        aux false (res :: res_cfgs) ((if not_covered then cs else []) @ res_constraints) covered_constraints gs in
  let cfgs, cfgs_constraints, covered_constraints = aux true [] [] [] cfgs in
  (* add constraints from last to first *)
  let path_constrs = List.fold_left (fun constrs pathss ->
      let vars = List.fold_left (fun acc x -> acc @ constr_used_vars x) [] constrs
                 |> S.of_list in
      let used_paths = List.map (fun (cs, ds) -> 
          if S.(is_empty @@ inter vars ds)
          then [] (* vars and ds are disjoint *)
          else cs 
        ) pathss in
      let constr_used_paths = 
        let and_constrs = List.map (fun p -> fold_constr ctt (fun acc x -> Cand (acc, x)) p) used_paths in
        fold_constr cff (fun acc x -> Cor (acc, x)) and_constrs in
      constr_used_paths :: constrs
    ) cfgs_constraints covered_constraints in
  let cfgs_cs = cfgs_constraints @ path_constrs in
  let () = if trace then
      (Printf.printf "===\n";
       Printf.printf "collected constraints:\n";
       List.iter (fun x -> Printf.printf "%s\n" @@ show_constr x) cfgs_cs;
       Printf.printf "===\n") in
  cfgs, cfgs_cs @ List.rev assigns_cs

let gen_constraints test trace vcd md : Cfg.cfg list * constr list =
  let cfgs, cs = of_module_data test trace vcd md in
  cfgs, cs
