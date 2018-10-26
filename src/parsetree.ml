
type pos = int * int
[@@deriving show]

type ident = string (* need to add more info *)
[@@deriving show]

type integer_vector_type = string
[@@deriving show]
(* type integer_vector_type = Bit | Logic | Reg *)

type integer_atom_type = string
[@@deriving show]
(* type integer_atom_type = Byte | Shortint | Int | Longint | Integer | Time *)

type non_integer_type = string
[@@deriving show]
(* type non_integer_type = Shortreal | Real | Realtime *)

type signing = string
[@@deriving show]
(* type signing = Signed | Unsigned *)

type random_qualifier = string
[@@deriving show]
(* type random_qualifier = Rand | Randc *)

type unary_operator = string
[@@deriving show]
(* type unary_operator = Plus | ... *)
type binary_operator = string
[@@deriving show]
(* type binary_operator = Plus | ... *)

type integer_base = Integer_dec
                  | Integer_bin
                  | Integer_oct
                  | Integer_hex
[@@deriving show]

(* type integer_number = integer_base * int *)
type integer_number = int * integer_base * string
[@@deriving show]
(* base * number(10) *)

type number = Integer_number of integer_number 
            | Real_number of float
[@@deriving show]

type primary_literal = Number_literal of number 
                     | String_literal of string    
[@@deriving show]


(********************************************************************************)

(* rep multiple dimensions *)
type dimension = Packed_dimension of constant_range option 
               | Unsized_dimension
               | Unpacked_dimension_range of constant_range
               | Unpacked_dimension_exp of constant_expression

and constant_range = constant_expression * constant_expression

and data_type = Integer_vector_type of integer_vector_type * signing option * dimension list
              | Integer_atom_type of integer_atom_type * signing option
              | Non_integer_type of non_integer_type 
              | Struct_union of struct_union_member list * dimension list
              | String
              | Implicit_data_type of signing option * dimension list

and struct_union_member = data_type * variable_decl_assignment list

and variable_decl_assignment = ident * dimension list * expression option

and expression = Primary of primary
               | Unary of unary_operator * expression
               | Binary of expression * binary_operator * expression
               | Op_assignment of operator_assignment
               | Conditional of cond_predicate * expression * expression

and cond_predicate = expression       

and primary = Primary_literal of primary_literal
            | Hierarchical_ident of hierarchical_ident * select
            | Concatenation of expression list * range_expression option
            | Expression of expression

(* TODO : support cbs *)
(* and hierarchical_ident = (ident * constant_bit_select) list * ident *)
and hierarchical_ident = ident list * ident

and range_expression = Range_expression_range of constant_range 
                     | Range_expression_expression of expression

and constant_expression = Constant_primary of constant_primary
                        | Constant_unary of unary_operator * constant_expression
                        | Constant_binary of constant_expression * binary_operator * constant_expression
                        | Constant_cond of constant_expression * constant_expression * constant_expression

and constant_primary = Constant_literal of primary_literal
                     | Constant_expression of constant_expression

and assignment_operator = string

and statement_or_null = statement

and statement = statement_item

and statement_item = Blocking_assignment of operator_assignment
                   | Nonblocking_assignment of (variable_lvalue * string (* must be <= *) * expression)
                   | Procedural_continuous_assignment of procedural_continuous_assignment
                   | Case_statement of case_statement
                   | Conditional_statement of conditional_statement
                   | Loop_statement of loop_statement
                   | Seq_block of seq_block  

and operator_assignment = variable_lvalue * assignment_operator * expression

and case_statement = pos * expression * case_item list

and case_item = Case_branch of expression list * statement_or_null
              | Case_default of statement_or_null

(* and conditional_statement = Cond of (cond_predicate option * statement_or_null) list *)
(* rep N-branch if statement by a list of N-length *)

and conditional_statement = Cond_if_else of pos * cond_predicate * statement_or_null * conditional_statement
                          | Cond_if of pos * cond_predicate * statement_or_null
                          | Cond_else of statement_or_null

and loop_statement = For of variable_assignment list * expression option * operator_assignment list * statement_or_null
(* for_initialization * expression * for_step * statement_or_null *)

(* and seq_block = block_item_declaration list * statement_or_null list *)

and procedural_continuous_assignment = variable_assignment

and variable_assignment = variable_lvalue * expression

and variable_lvalue = Variable_ident_const_select of hierarchical_ident * select
                    | Variable_lvalues_concat of variable_lvalue list
                    | Variable_assignment_pattern of assignment_pattern_expression_type option * assignment_pattern_variable_lvalue

and assignment_pattern_variable_lvalue = variable_lvalue list

and select = bit_select * constant_range option

and bit_select = expression list

and constant_select = constant_bit_select * constant_range option

and constant_bit_select = constant_expression list

and module_common_item = Module_or_generate_item_dec of module_or_generate_item_dec 
                       | Interface_instantiation of interface_instantiation 
                       | Program_instantiation of program_instantiation
                       | Continuous_assign of continuous_assign
                       | Initial_construct of initial_construct
                       (* | Final_construct of final_construct *)
                       | Always_construct of always_construct

and module_or_generate_item_dec = Data_declaration of data_declaration                        
                                | Parameter_declaration of parameter_declaration 

and data_declaration = Variable_declaration of data_type * variable_decl_assignment list
                     | Type_declaration of type_declaration

and type_declaration = Type_dec_data_type of data_type * ident * dimension list
                     | Type_dec_interface of ident * constant_bit_select * ident * ident
                     | Type_dec_struct of ident

and interface_instantiation = ident * parameter_value_assignment option * hierarchical_instance list

and parameter_value_assignment = parameter_assignment

and parameter_assignment = Ordered_parameter_assignment of param_expression list
                         | Named_parameter_assignment of (ident * param_expression option) list

and param_expression = Param_exp_data_type of data_type
                     | Param_exp_constant of constant_expression

and hierarchical_instance = ident * dimension option * port_connections
(* and name_of_instance = ident * dimension option *)

and port_connections = Ordered_port_connection of expression list
                     | Named_port_connection of (ident * expression option option) list 
(* | Named_port_connection of (ident * expression option) list *)
(* identifing `foo ()` and `foo` *)

and program_instantiation = ident * parameter_value_assignment option * hierarchical_instance list


and continuous_assign = Net_assignment of net_assignment list
                      | Variable_assignment of variable_assignment list

and net_assignment = net_lvalue * expression

and net_lvalue = Net_ident_const_select of hierarchical_ident * constant_select
               | Net_lvalues_concat of net_lvalue list
               | Net_assignment_pattern of assignment_pattern_expression_type option * assignment_pattern_net_lvalue

and assignment_pattern_expression_type = Ape_ps_parameter_identifier of ident
                                       | Ape_integer_atom_type of integer_atom_type

and assignment_pattern_net_lvalue = net_lvalue list

(* and net_lvalue_ = Net_lvalue of ident * dimension option (* this may be a too simplification *)
                | Net_lvalues of assignment_pattern_expression_type * net_lvalue_ list

   and net_lvalue = net_lvalue_ list *)

and initial_construct = statement_or_null

and always_construct = Always_comb of statement

and parameter_declaration = Parameter_dec_param_assignments of data_type * param_assignment list
                          | Parameter_dec_type_assignments of type_assignment list

and param_assignment = ident * dimension list * param_expression option

and type_assignment = ident * data_type option

and seq_block = block_item_declaration list * statement list

and block_item_declaration = module_or_generate_item_dec

and description = Module_declaration of module_declaration 
                | Interface_declaration of interface_declaration
                | Package_item of package_or_generate_item_declaration

and package_or_generate_item_declaration = data_declaration                

and module_declaration = module_header * module_item list

and module_header = ident * port_declarations list

and port_declarations = ansi_port_declaration

and ansi_port_declaration =  Interface_port_header of interface_port_header option * ident
                          | Net_port_header of net_port_header option * ident

and net_port_header = port_direction option * net_port_type

and interface_port_header = ident * ident option 

and port = Port of port_reference list
         | Port_with_name of ident * port_reference list

and port_reference = ident * constant_select

and module_item = Module_port_declaration of port_declaration
                | Module_non_port_module_item of non_port_module_item


and port_declaration = Inout of net_port_type * port_identifiers
                     | Input_port of net_port_type * port_identifiers
                     | Input_variable of variable_port_type * variable_identifiers
                     | Output_port of net_port_type * port_identifiers
                     | Output_variable of variable_port_type * variable_identifiers
                     (* rep  interface_identifier . modport_identifier list_of_interface_identifiers *)
                     | Port_interface_port_declaration of ident * ident option * interface_identifiers

and port_identifiers = (ident * dimension list) list

and variable_identifiers =  (ident * dimension list) list

and interface_identifiers = (ident * dimension list) list

and net_port_type = Net_port_type_data_type of data_type
                  | Net_port_type_ident of ident

and variable_port_type = data_type

and non_port_module_item = Module_instantiation of module_instantiation
                         | Module_common_item of module_common_item
                         | Module_declaration_ of module_declaration
                         | Module_Interface_declaration of interface_declaration

and module_instantiation = ident * parameter_value_assignment option * hierarchical_instance list

and interface_declaration = interface_header * interface_item list

and interface_header = ident

and interface_item = Interface_port_declaration of port_declaration
                   | Interface_non_port_interface_item of non_port_interface_item

and non_port_interface_item = Interface_or_generate_item of interface_or_generate_item
                            | Interface_declaration_ of interface_declaration

and interface_or_generate_item = Interface_module_common_item of module_common_item
                               | Modport_declaration of modport_declaration

and modport_declaration = modport_item list

and modport_item = ident * modport_ports_declaration list

and modport_ports_declaration = modport_simple_ports_declaration

and modport_simple_ports_declaration = port_direction * modport_simple_port list

and port_direction = string
(* port_direction = Input | Output ... *)

and modport_simple_port = ident * expression option
[@@deriving show]

let string_of_integer_base = function
  | Integer_dec -> ""
  | Integer_bin -> "b"
  | Integer_oct -> "o"
  | Integer_hex -> "x"

let string_of_integer_base_ = function
  | Integer_dec -> "[dD]"
  | Integer_bin -> "[bB]"
  | Integer_oct -> "[oO]"
  | Integer_hex -> "[hH]"  

let string_of_integer_number (s, ib, n) = Printf.sprintf "%d'%s%s" s (string_of_integer_base ib) n

(* let char_to_int b n c =  
   match b with
   Integer_dec  *)

let ints_of_integer_number : integer_number -> int * int = fun (s, b, n) ->
  let r = 
    match b with
    | Integer_dec ->
      let num = Str.split (Str.regexp "[dD]") n |> List.rev |> List.hd in
      int_of_string num
    | Integer_bin ->
      let num = Str.split (Str.regexp "[bB]") n |> List.rev |> List.hd in
      int_of_string ("0b" ^ num)
    | Integer_oct ->
      let num = Str.split (Str.regexp "[oO]") n |> List.rev |> List.hd in
      int_of_string ("0o" ^ num)
    | Integer_hex ->
      let num = Str.split (Str.regexp "[hH]") n |> List.rev |> List.hd in
      int_of_string ("0x" ^ num) 
  in (s, r)



