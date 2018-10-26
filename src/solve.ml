(*
  solve generated constraints using Z3
*)


module A = Z3.Arithmetic 
module I = A.Integer 
module E = Z3.Expr 
module B = Z3.Boolean
module BV = Z3.BitVector 
module G = Z3.Goal
module S = Z3.Solver
module M = Z3.Model

let z3_dec_of_data_type ctx id : Ir.data_type -> Z3.Expr.expr = function
  | Integer_vector_type (dt, _, []) when dt = "logic" ->     
    Z3.BitVector.mk_const_s ctx id 1
  | Integer_vector_type (dt, _, [(Dimension_range ((EInt (1, l)), (EInt (1, r))))])
    when dt = "logic" -> 
    Z3.BitVector.mk_const_s ctx id (l - r + 1)
  | Implicit_data_type _ -> 
    Z3.Arithmetic.Integer.mk_const_s ctx id 
  (* implicit_data_type mainly represents the type of parameter declaration, so it is represented as int for now *)
  | e -> failwith @@ Printf.sprintf "z3type_of_data_type: solve: %s %s" id (Ir.show_data_type e)

(* generates a list of constant declarations *)
let gen_declaration ctx : Ir.type_dict -> _ = fun tb ->
  let open Hashtbl in
  let decs = create 100 in
  (iter 
     (fun id (dt, eo) ->
        let z3dec = z3_dec_of_data_type ctx id dt in
        add decs id (z3dec, eo)) 
     tb);
  decs

let resize_bv_refs ctx x y =
  let xsize = BV.get_size @@ E.get_sort !x in
  let ysize = BV.get_size @@ E.get_sort !y in
  let size = max xsize ysize in
  (if size > xsize then 
     let xtmp = BV.mk_zero_ext ctx (size - xsize) !x in
     x := xtmp);
  (if size > ysize then 
     let ytmp = BV.mk_zero_ext ctx (size - ysize) !y in
     y := ytmp)

let z3_mk_bool2bv ctx b =
  B.mk_ite ctx b (BV.mk_numeral ctx "1" 1) (BV.mk_numeral ctx "0" 1)

let z3_mk_bv2bool ctx b =
  match BV.get_size (E.get_sort b) with
  | 1 -> 
    let cond = B.mk_eq ctx b (BV.mk_numeral ctx "1" 1) in
    B.mk_ite ctx cond (B.mk_true ctx) (B.mk_false ctx) 
  | n -> failwith 
    @@ Printf.sprintf "z3_mk_bv2bool: solve: length of b must be 1, but %d" n

let z3_mk_cast2bv ctx b =
  if BV.is_bv b then
    match b |> E.get_sort |> BV.get_size with
    | 1 -> b
    | n -> failwith 
      @@ Printf.sprintf "z3_mk_cast2bv: solve: length of b must be 1, but %d" n
  else if B.is_bool b then
    z3_mk_bool2bv ctx b
  else
    failwith "z3_mk_cast2bv: solve"


let z3_cast_bin ctx x y =
  match BV.is_bv !x, BV.is_bv !y with 
  | true, true -> resize_bv_refs ctx y x
  | true, false | false, true -> (* cast int to bv if needed *)
    if A.is_int !x then
      x := I.mk_int2bv ctx (BV.(get_size (E.get_sort !y))) !x
    else if B.is_bool !x then
      x := z3_mk_bool2bv ctx !x
    else if A.is_int !y then
      y := I.mk_int2bv ctx (BV.(get_size (E.get_sort !x))) !y
    else if B.is_bool !y then
      y := z3_mk_bool2bv ctx !y
  | false, false -> 
    if B.is_bool !x && B.is_bool !y then 
      (x := z3_mk_bool2bv ctx !x;
       y := z3_mk_bool2bv ctx !y)
    else ()

let rec bvl_of_int n =
  if n < 2 then 1 else 1 + bvl_of_int (n/2) 

(* generates a constraint *)  
let rec of_constr top ctx decs : Constraint.constr -> Z3.Expr.expr = 
  let z3_mk_eq_one top x = 
    (match top, BV.is_bv x with
     | true, true -> B.mk_eq ctx x (BV.mk_numeral ctx "1" 1)
     | _, _ -> x) in
  function
  | CPint i when i < 2 -> BV.mk_numeral ctx (string_of_int i) 1
  | CPint i -> I.mk_numeral_i ctx i
  | CPfloat f -> Z3.FloatingPoint.(mk_numeral_f ctx f (mk_sort_single ctx))
  | CPconstant (CTbitvector _, id) ->    
    let (ex, _) = Hashtbl.find decs id in
    z3_mk_eq_one top ex
  | CPconstant (CTnone, id) ->  (* assume CTnone is used for parameter *)
    (match Hashtbl.find decs id with
     | (p, Some (Ir.EInt (_, i))) -> 
       let res = Z3.Arithmetic.Integer.mk_int2bv ctx (bvl_of_int i) p in
       z3_mk_eq_one top res
     | _ -> failwith "of_constr: solve: CTnone is expected to used for parameter")
  | CPbin (op, c1, c2) as e -> 
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    (match op with
     | "+" -> z3_mk_eq_one top @@ BV.mk_add ctx !c1 !c2 
     | "-" -> z3_mk_eq_one top @@ BV.mk_sub ctx !c1 !c2
     | "<" -> z3_mk_eq_one top @@ BV.mk_ult ctx !c1 !c2
     | ">" -> z3_mk_eq_one top @@ BV.mk_ult ctx !c2 !c1
     | "<<" -> z3_mk_eq_one top @@  BV.mk_shl ctx !c1 !c2
     | ">>" -> z3_mk_eq_one top @@  BV.mk_lshr ctx !c1 !c2
     | "&&" | "&" -> z3_mk_eq_one top @@ BV.mk_and ctx !c1 !c2
     | "||" | "|" -> z3_mk_eq_one top @@ BV.mk_or ctx !c1 !c2
     | "^" -> z3_mk_eq_one top @@ BV.mk_xor ctx !c1 !c2       
     | "==" -> z3_mk_eq_one top @@ B.mk_eq ctx !c1 !c2
     | _ -> failwith
       @@ Printf.sprintf "CPbin: of_constr: solve: %s" 
       @@ Constraint.show_constr e)
  | CPun (op, c) as e -> 
    let c_ = of_constr false ctx decs c in
    (* let () = Printf.eprintf "java2\n" in *)
    (match op with
     | "!" | "~" ->
       let c = if B.is_bool c_ then z3_mk_bool2bv ctx c_ else c_ in
       z3_mk_eq_one top @@ BV.mk_not ctx c
     | "|" when BV.is_bv c_ -> of_constr top ctx decs (CPbin ("<", CPint 0, c))
     | "&" when BV.is_bv c_ ->
       let size = BV.get_size (E.get_sort c_) in
       of_constr top ctx decs (CPbin ("==", CPint (1 lsl size - 1), c))
     | _ -> failwith 
       @@ Printf.sprintf "CPun: of_constr: solve: %s"
       @@ Constraint.show_constr e)
  | CPbitvector (s, n) -> z3_mk_eq_one top @@ BV.mk_numeral ctx (string_of_int n) s
  | CPdim (c, CDrange (CPint a, CPint b)) -> 
    let c = of_constr false ctx decs c in
    z3_mk_eq_one top @@ BV.mk_extract ctx a b c
  | CPdim (c, CDat (CPint i)) -> 
    let c = of_constr false ctx decs c in
    z3_mk_eq_one top @@ BV.mk_extract ctx i i c
  | CPconcat (c::cs) ->
    z3_mk_eq_one top @@ List.fold_left 
      (fun acc x -> let x = of_constr false ctx decs x in BV.mk_concat ctx acc x) 
      (of_constr false ctx decs c) cs
  | Ceq (c1, c2) -> 
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    B.mk_eq ctx !c1 !c2
  | Clt (c1, c2) 
  | Cgt (c2, c1) -> 
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    z3_mk_eq_one top @@ BV.mk_ult ctx !c1 !c2
  | Cnot c ->
    let c = of_constr false ctx decs c in
    z3_mk_eq_one top @@ BV.mk_not ctx (z3_mk_cast2bv ctx c)
  | Cand (c1, c2) -> 
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    z3_mk_eq_one top @@ BV.mk_and ctx !c1 !c2
  | Cor (c1, c2) -> 
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    z3_mk_eq_one top @@ BV.mk_or ctx !c1 !c2
  | Cvalid c ->
    let c = of_constr false ctx decs c in
    B.mk_eq ctx c (BV.mk_numeral ctx "1" 1)
  | CPcond (p, c1, c2) ->
    let p = of_constr false ctx decs p |> z3_mk_cast2bv ctx |> z3_mk_bv2bool ctx in
    let c1 = ref @@ of_constr false ctx decs c1 in
    let c2 = ref @@ of_constr false ctx decs c2 in
    let () = z3_cast_bin ctx c1 c2 in
    z3_mk_eq_one top @@ B.mk_ite ctx p !c1 !c2
  | e -> failwith 
    @@ Printf.sprintf "of_constr: solve: %s"
    @@ Constraint.show_constr e

let rec take n = function
  | [] -> []
  | x :: xs -> if n < 1 then [] else x :: (take (n - 1) xs)

let extract_next_inputs decs : Z3.Model.model -> Ir.port_declaration list -> (string * string) list =
  fun m ps -> 
    List.map (fun (dt, id) -> 
        let (e, _) = Hashtbl.find decs id in 
        match M.eval m e false with
        | Some e when id = E.to_string e -> (id, Ir.gen_random dt)
        | Some e -> (id, E.to_string e)
        | None -> failwith @@ Printf.sprintf "extract_next_inputs: solve: cannot eval %s" id
      ) ps

let solver dz3 ctx : Ir.type_dict -> Constraint.constr list -> Ir.port_declaration list -> (string * string) list = 
  fun td cs input_ports ->
    let decs = gen_declaration ctx td in
    (* let () = Hashtbl.iter (fun id (e, _) -> Printf.printf "%s:\n\t%s\n\t%s\n" id (e |> E.get_sort |> Z3.Sort.to_string) (E.to_string e)) decs in *)
    let goal = G.mk_goal ctx true false false in
    let consts = List.map (fun x -> of_constr true ctx decs x) cs in
    let () = G.add goal consts in
    let () = if dz3 then Printf.printf "Goal: \n%s\n" (G.to_string goal) in
    let solver = S.mk_solver ctx None in
    let () = (List.iter (fun a -> (S.add solver [ a ])) (G.get_formulas goal)) in
    let status = S.check solver [] in
    let () = if dz3 then Printf.printf "Solver: \n%s\n" (S.to_string solver) in
    match status with
    | S.SATISFIABLE -> 
      let model = S.get_model solver in
      (match model with
       | None -> raise @@ Failure "sat"
       | Some model ->
         let () = if dz3 then Printf.printf "MODEL: \n%s\n" (M.to_string model) in
         (extract_next_inputs decs model input_ports))
    | S.UNSATISFIABLE -> (Printf.printf "UNSATISFIABLE\n";
                          raise @@ Failure "sat") (* exit 0)  *)     
    | S.UNKNOWN -> (Printf.printf "CANNOT SATISFY\n";
                    raise @@ Failure "sat") (* exit 0)  *)

let solve dz3 dict cl input_ports = 
  let cfg = [("model", "true"); ("proof", "false")]in
  let ctx = Z3.mk_context cfg in
  solver dz3 ctx dict cl input_ports