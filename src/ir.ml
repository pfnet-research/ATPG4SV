(* 
  intermediate representation 
*)

type pc = Parsetree.pos
[@@deriving show]  

type expression = 
  | EInt of (int * int)
  | Efloat of float
  | Estring of string
  | Eident of (string list * string) * dimension option (* XXX.YYY.ZZZ[3:0] => [XXX; YYY], ZZZ, Some `[3:0]`*)
  | Econcat of expression list
  | Eunary of string * expression
  | Ebinary of expression * string * expression
  | Econd of expression * expression * expression 
  (* need to lift to statement ? 
     -> use Z3.Boolean.mk_ite
  *)

and dimension = Dimension_range of expression * expression
              | Dimension_index of expression
[@@deriving show]  

let id_of_hierarchical (ids, id) = 
  let rec aux = function
    | [] -> id
    | [x] -> Printf.sprintf "%s.%s" x id
    | x::xs -> Printf.sprintf "%s.%s" x (aux xs) in
  aux ids

let rec string_of_expression = function
  | EInt (1, n) -> Printf.sprintf "%d" n
  | EInt (d, n) -> Printf.sprintf "%d'd%d" d n
  | Efloat f -> string_of_float f
  | Estring s -> s
  | Eident (hid, s) ->
    let ss = match s with Some s -> string_of_dimension s | None -> "" in
    id_of_hierarchical hid ^ ss
  | Econcat el -> List.fold_left (fun acc x -> acc ^ ", " ^ string_of_expression x) "" el
  | Eunary (s, e) -> s ^ string_of_expression e
  | Ebinary (e1, s, e2) -> string_of_expression e1 ^ s ^ string_of_expression e2
  | Econd (e1, e2, e3) -> 
    Printf.sprintf "%s ? %s : %s" 
      (string_of_expression e1) (string_of_expression e2) (string_of_expression e3)  

and string_of_dimension = function
  | Dimension_index e -> Printf.sprintf "[%s]" (string_of_expression e)
  | Dimension_range (e1, e2) -> Printf.sprintf "[%s:%s]" (string_of_expression e1) (string_of_expression e2)

type assignment = expression * string * expression  
[@@deriving show]  

type statement = 
  | Br of pc * expression * statement * statement
  | Nope
  | Case of pc * expression * (expression option * statement) list
  | Assigns of assignment list
  | Seq of statement list
[@@deriving show]

let rec string_of_statement = function
  | Nope -> "Nope"
  | Assigns l -> 
    List.fold_left (fun acc (e1, s, e2) -> 
        let s1 = string_of_expression e1 in
        let s2 = string_of_expression e2 in
        Printf.sprintf {|%s\n%s %s %s|} acc s1 s s2 ) "" l
  | Seq [s] -> string_of_statement s
  | _ -> failwith "string_of_statement: ir"

type data_type = Integer_vector_type of Parsetree.integer_vector_type * Parsetree.signing option * dimension list
               | Integer_atom_type of Parsetree.integer_atom_type * Parsetree.signing option
               | Non_integer_type of Parsetree.non_integer_type 
               | Struct_union of declaration list * dimension list
               | String
               | Implicit_data_type of Parsetree.signing option * dimension list

and declaration = data_type * Parsetree.ident * dimension list * expression option
[@@deriving show]

type always_construct = Always_comb of statement 
[@@deriving show]  

type port_declaration = data_type * Parsetree.ident
[@@deriving show]

type module_header = {
  module_name : string;
  input_ports : port_declaration list;
  output_ports : port_declaration list;
  interface_ports : port_declaration list
}
[@@deriving show]

type module_item = 
  (* Port_declaration of port_declaration *)
  | Mdata_declaration of declaration
  | Mparameter_declaration of declaration
  | MContinuous_assign of expression * expression
  | Malways_construct of always_construct
[@@deriving show]

type module_declaration = module_header * module_item list
[@@deriving show]

(* functions for translation from parsetree *)

exception Need_more_info of string

let rec of_statement : Parsetree.statement_or_null -> statement = function
  | Blocking_assignment (variable_lvalue, op, exp)
  | Nonblocking_assignment (variable_lvalue, op, exp) -> 
    Assigns [(of_variable_lvalue variable_lvalue, op, of_expression exp)]
  | Procedural_continuous_assignment (variable_lvalue, exp) -> 
    Assigns [(of_variable_lvalue variable_lvalue, "=", of_expression exp)]
  | Case_statement (p, e, cs) -> 
    Case (p, of_expression e, List.map of_case_item cs)
  | Conditional_statement cs -> of_conditional_statement cs
  | Seq_block (_, sl) -> Seq (List.map of_statement sl)
  | _ -> raise @@ Need_more_info "of_statement"

and of_conditional_statement : Parsetree.conditional_statement -> statement = function
  | Cond_if_else (p, cp, s, cs) -> Br (p, of_expression cp, of_statement s, of_conditional_statement cs)
  | Cond_if (p, cp, s) -> Br (p, of_expression cp, of_statement s, Nope)
  | Cond_else s -> of_statement s

and of_case_item : Parsetree.case_item -> expression option * statement = function
  | Case_branch ([e], s) -> (Some (of_expression e), of_statement s)
  | Case_default s -> (None, of_statement s)
  | _ -> raise @@ Need_more_info "of_case_item"

and of_variable_lvalue : Parsetree.variable_lvalue -> expression = function
  | Variable_ident_const_select (hierarchical_ident, select) ->
    Eident (of_hierarchical_ident hierarchical_ident, of_select select)
  | _ -> raise @@ Need_more_info "of_variable_lvalue"

and of_net_lvalue : Parsetree.net_lvalue -> expression = function
  | Net_ident_const_select (hierarchical_ident, cselect) ->
    Eident (of_hierarchical_ident hierarchical_ident, of_constant_select cselect)
  | _ -> raise @@ Need_more_info "of_variable_lvalue"

and of_expression : Parsetree.expression -> expression = function
  | Primary p -> of_primary p
  | Unary (op, e) -> Eunary (op, of_expression e)
  | Binary (e1, op, e2) -> Ebinary (of_expression e1, op, of_expression e2)
  | Conditional (ce, e1, e2) -> Econd (of_expression ce, of_expression e1, of_expression e2) 
  | _ -> raise @@ Need_more_info "of_expression"

and of_constant_expression : Parsetree.constant_expression -> expression = function
  | Constant_primary (Constant_literal pl) -> of_primary_literal pl
  | Constant_primary (Constant_expression ce) -> of_constant_expression ce
  | Constant_unary (op, ce) -> Eunary (op, of_constant_expression ce)
  | Constant_binary (ce1, op, ce2) -> Ebinary (of_constant_expression ce1, op, of_constant_expression ce2)
  | Constant_cond (ce1, ce2, ce3) -> Econd (of_constant_expression ce1, of_constant_expression ce2, of_constant_expression ce3) 

and of_param_expression : Parsetree.param_expression -> expression = function
  | Param_exp_constant ce -> of_constant_expression ce
  | _ -> raise @@ Need_more_info "of_param_expression"

and of_primary : Parsetree.primary -> expression = function
  | Primary_literal pl -> of_primary_literal pl
  | Hierarchical_ident (hi, s) -> Eident (of_hierarchical_ident hi, of_select s)
  | Concatenation (es, Some re) -> Econcat (List.map of_expression es @ [Estring (Parsetree.show_range_expression re)])
  | Concatenation (es, None) -> Econcat (List.map of_expression es)
  | Expression e -> of_expression e

and of_primary_literal : Parsetree.primary_literal -> expression = function
  | Number_literal n -> of_number n
  | String_literal s -> Estring s

and of_number : Parsetree.number -> expression = function
  | Integer_number i -> EInt (Parsetree.ints_of_integer_number i)
  | Real_number f -> Efloat f

and of_hierarchical_ident : Parsetree.hierarchical_ident -> string list * string =
  fun (is, i) -> (is, i)

and of_variable_decl_assignment : Parsetree.variable_decl_assignment -> Parsetree.ident * dimension list * expression option = function
  | (id, dl, Some e) -> (id, List.map of_dimension dl, Some (of_expression e))
  | (id, dl, None) -> (id, List.map of_dimension dl, None)

and of_dimension : Parsetree.dimension -> dimension = function
  | Packed_dimension (Some (ce1, ce2))
  | Unpacked_dimension_range (ce1, ce2)  ->Dimension_range (of_constant_expression ce1, of_constant_expression ce2)
  | Unpacked_dimension_exp ce -> Dimension_index (of_constant_expression ce)
  | _ -> raise @@ Need_more_info "of_dimension"

and of_data_declaration : Parsetree.data_declaration -> declaration list = function
  | Variable_declaration (dt, vda) -> 
    let dt = of_data_type dt in
    List.map (fun x -> 
        let (id, dl, eo) = of_variable_decl_assignment x in
        (dt, id, dl, eo)
      ) vda
  | _ -> raise @@ Need_more_info "of_data_declaration"

and of_parameter_declaration : Parsetree.parameter_declaration -> declaration list = function
  | Parameter_dec_param_assignments (dt, pal) ->
    let dt = of_data_type dt in
    List.map (fun x -> 
        let (id, dl, eo) = of_param_assignment x in
        (dt, id, dl, eo)
      ) pal
  | _ -> raise @@ Need_more_info "of_parameter_declaration"

and of_param_assignment : Parsetree.param_assignment -> Parsetree.ident * dimension list * expression option = function
  | (id, dl, Some pe) -> (id, List.map of_dimension dl, Some (of_param_expression pe))
  | (id, dl, None) -> (id, List.map of_dimension dl, None)

and of_data_type : Parsetree.data_type -> data_type = function
  | Integer_vector_type (i, s, dl) -> Integer_vector_type (i, s, List.map of_dimension dl)
  | Integer_atom_type (i, s) -> Integer_atom_type (i, s)
  | Non_integer_type n -> Non_integer_type n
  | Struct_union (dvs, dl) ->
    let decl = List.fold_left (fun acc (dt, vs) -> acc @ of_data_declaration (Parsetree.Variable_declaration (dt, vs))) [] dvs in
    let dl = List.map of_dimension dl in
    Struct_union (decl, dl)
  | String -> String
  | Implicit_data_type (s, dl) -> Implicit_data_type (s, List.map of_dimension dl)

and of_module_declaration : Parsetree.module_declaration -> module_declaration = 
  fun ((id, mpds), il) -> 
    let module_header = {module_name=id; input_ports=[]; output_ports=[]; interface_ports=[]} in
    let module_header = 
      List.fold_left (fun mh x -> match of_port_declarations x with
          | (Some "input", dt, id) -> {mh with input_ports = (dt, id) :: mh.input_ports}
          | (Some "output", dt, id) -> {mh with output_ports = (dt, id) :: mh.output_ports}
          | _ -> raise @@ Need_more_info "of_module_declaration"
        ) module_header mpds in
    let module_items = List.fold_left (fun acc x -> acc @ of_module_item x) [] il in
    (module_header, module_items)

and of_port_declarations : Parsetree.port_declarations -> _ = function
  | Net_port_header (Some (nph, Net_port_type_data_type dt), id) -> 
    let dt = of_data_type dt in (nph, dt, id)
  (* TODO: need to read interface file and get type info
     | Interface_port_header (iph, id) -> (match iph with
      | (Some (itf, Some m)) ->  *)
  | _ -> raise @@ Need_more_info "of_port_declaration"

and of_module_item : Parsetree.module_item -> module_item list = function
  | Module_non_port_module_item np -> of_non_port_module_item np
  | _ -> raise @@ Need_more_info "of_module_item"

and of_non_port_module_item : Parsetree.non_port_module_item -> module_item list = function
  | Module_common_item mci -> of_module_common_item mci
  | Module_instantiation mi -> of_module_instantiation mi
  | _ -> raise @@ Need_more_info "of_non_port_module_item"

and of_module_common_item : Parsetree.module_common_item -> module_item list = function
  | Module_or_generate_item_dec mgid -> of_module_or_generate_item_dec mgid
  | Continuous_assign ca -> of_continuous_assign ca
  | Always_construct ac -> [of_always_construct ac]
  | _ -> raise @@ Need_more_info "of_module_common_item"

and of_module_or_generate_item_dec : Parsetree.module_or_generate_item_dec -> module_item list = function
  | Data_declaration dd -> List.map (fun x -> Mdata_declaration x) (of_data_declaration dd) 
  | Parameter_declaration pd -> List.map (fun x -> Mparameter_declaration x) (of_parameter_declaration pd) 

and of_always_construct : Parsetree.always_construct -> module_item = 
  fun (Always_comb s) -> Malways_construct (Always_comb (of_statement s))

and of_continuous_assign : Parsetree.continuous_assign -> module_item list = function
  | Net_assignment nl -> List.map of_net_assignment nl
  | Variable_assignment vl -> List.map of_variable_assignment vl

and of_net_assignment : Parsetree.net_assignment -> module_item = fun (lv, e) ->
  MContinuous_assign (of_net_lvalue lv, of_expression e)

and of_variable_assignment : Parsetree.variable_assignment -> module_item = fun (lv, e) ->
  MContinuous_assign (of_variable_lvalue lv, of_expression e)

(* TODO: ignore module instatntiation for now *)
and of_module_instantiation : Parsetree.module_instantiation -> module_item list = function
  | _ -> []

and of_select : Parsetree.select -> dimension option = function
  | ([], Some (ce1, ce2)) -> Some (Dimension_range (of_constant_expression ce1, of_constant_expression ce2))
  | ([], None) -> None
  | ([e], None) -> Some (Dimension_index (of_expression e))
  | e -> raise @@ Need_more_info (Printf.sprintf "of_select: %s" (Parsetree.show_select e))

and of_constant_select : Parsetree.constant_select -> dimension option = function  
  | ([], Some (ce1, ce2)) -> Some (Dimension_range (of_constant_expression ce1, of_constant_expression ce2))
  | ([], None) -> None
  | _ -> raise @@ Need_more_info "of_constant_select"

and string_of_select : Parsetree.select -> string = function 
  | (el, Some (ce1, ce2)) -> let el = List.map (fun x -> x |> of_expression |> string_of_expression) el in
    let es = List.fold_left (fun acc x -> Printf.sprintf "%s[%s]" acc x) "" el in
    Printf.sprintf "%s[%s:%s]" es (string_of_expression (of_constant_expression ce1))
      (string_of_expression (of_constant_expression ce2))
  | (el, None) -> let el = List.map (fun x -> x |> of_expression |> string_of_expression) el in
    List.fold_left (fun acc x -> Printf.sprintf "%s[%s]" acc x) "" el


(* [ident -> type] dictionary *)

type type_dict_c = data_type * expression option
[@@deriving show]
type type_dict = (Parsetree.ident, type_dict_c) Hashtbl.t

let show_type_dict : type_dict -> string = fun dict ->
  Hashtbl.fold (fun x y acc -> Printf.sprintf "%s\n\n%s:\n %s" acc x (show_type_dict_c y)) dict ""

let gen_random dt =
  let aux : data_type -> string * int = function
    | Integer_vector_type ("logic", None, []) -> "logic", 1
    | Integer_vector_type ("logic", None, [d]) ->
      (match d with
       | Dimension_range (EInt (1, a), EInt (1, b)) -> "logic", a - b + 1
       | _ -> failwith "gen_random: ir")
    | _ -> failwith @@ Printf.sprintf "gen_random: ir" in
  let gen = function
    | ("logic", n) ->
      let rand = Random.int (1 lsl n) in
      let rec mk_bin n s = 
        if n < 2 then "#b" ^ (string_of_int n) ^ s
        else mk_bin (n / 2) @@ (string_of_int @@ n mod 2) ^ s in
      mk_bin rand ""
    | _ -> failwith @@ Printf.sprintf "gen_random: ir"
  in
  gen @@ aux dt
