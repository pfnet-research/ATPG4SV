
%{
 open Parsetree
%}

%token <Parsetree.number> NUMBER
%token <Parsetree.integer_vector_type> INTEGER_VECTOR_TYPE 
%token <Parsetree.integer_atom_type> INTEGER_ATOM_TYPE
%token <Parsetree.non_integer_type> NON_INTEGER_TYPE
%token <Parsetree.signing> SIGNING
/* %token <Parsetree.random_qualifier> RANDOM_QUALIFIER */
/* %token <Parsetree.binary_operator> BINARY_OPERATOR */
/* %token <Parsetree.unary_operator> UNARY_OPERATOR */
%token <string> OPERATOR
%token <Parsetree.integer_number> INTEGER_NUMBER
%token <float> REAL_NUMBER
%token <string> STRING /* string literal */
%token <Parsetree.ident> IDENT
%token <Parsetree.assignment_operator> ASSIGNMENT_OP
%token <Parsetree.port_direction> PORT_DIRECTION
%token <Parsetree.pos> IF
%token <Parsetree.pos> CASE

%token LBRACKET
%token RBRACKET
%token QUESTION
%token COLON 
%token SEMI
%token LPAREN
%token RPAREN
%token DOT
%token SHARPLPAREN
%token COMMA
%token EQUAL
%token BACKLBRACE
%token LESSEQUAL

%token STRUCT
%token LBRACE
%token RBRACE
%token MODULE
%token ENDMODULE
%token INOUT
%token INPUT
%token OUTPUT
%token TYPEDEF
%token PARAMETER
%token TYPE
%token ASSIGN
%token INITIAL
%token ENDCASE
%token DEFAULT
%token ELSE
%token FOR
%token BEGIN
%token END
%token ALWAYS_COMB
%token INTERFACE
%token ENDINTERFACE
%token MODPORT

%token EOF


/* %start <Parsetree.description list> start */
%start <Parsetree.module_declaration> start

%%

start:
  | x = module_declaration; EOF { x }
  ;

list_of_description:
  | /* empty */ { [] }
  | d = description ; ld = list_of_description { d :: ld }
  ;

description:
  | id = interface_declaration { Interface_declaration id }
  | md = module_declaration { Module_declaration md }
  | pgid = package_or_generate_item_declaration { Package_item pgid }
  ;

package_or_generate_item_declaration:
  | dd = data_declaration { dd }  
  ;

interface_declaration:
  | i = interface_header ; il = interface_item_list ; ENDINTERFACE
    { (i, il) }
  ;

interface_header:
  | INTERFACE ; ii = IDENT ; SEMI { ii }
  ;

interface_item_list:
  | /* empty */ { [] }
  | ii = interface_item ; il = interface_item_list { ii :: il }
  ;

interface_item:
  | pd = port_declaration ; SEMI { Interface_port_declaration pd }
  | npii = non_port_interface_item { Interface_non_port_interface_item npii }
  ;

non_port_interface_item:
  | igi = interface_or_generate_item { Interface_or_generate_item igi }
  | id = interface_declaration { Interface_declaration_ id }
  ;

interface_or_generate_item:
  | mci = module_common_item { Interface_module_common_item mci }  
  | md = modport_declaration { Modport_declaration md }
  ;

modport_declaration:
  | MODPORT ; mi = list_of_modport_item ; SEMI { mi }
  ;

list_of_modport_item:
  | mi = modport_item { [mi] }
  | mi = modport_item ; COMMA ; lmi = list_of_modport_item { mi :: lmi }
  ;

modport_item:
  | mpi = IDENT ; LPAREN ; lmd = list_of_modport_ports_declaration ; RPAREN { (mpi, lmd) }
  ;

list_of_modport_ports_declaration:
  | mpd = modport_ports_declaration { [mpd] }
  | mpd = modport_ports_declaration ; COMMA ; lmpd = list_of_modport_ports_declaration { [mpd] }
  ;

modport_ports_declaration:
  | pd = PORT_DIRECTION ; lmdp = list_of_modport_simple_port { (pd, lmdp) }
  /* | INPUT ; lmdp = list_of_modport_simple_port { ("input", lmdp) }
  | OUTPUT ; lmdp = list_of_modport_simple_port { ("output", lmdp) }
  | INOUT ; lmdp = list_of_modport_simple_port { ("inout", lmdp) } */
  ;

list_of_modport_simple_port:
  | msp = modport_simple_port { [msp] }
  | msp = modport_simple_port ; COMMA ; lmsp = list_of_modport_simple_port { msp :: lmsp }
  ;

modport_simple_port:
  | id = IDENT { (id, None) }
  | DOT ; id = IDENT ; LPAREN ; e = option(expression) ; RPAREN { (id, e) }
  ;

module_declaration:
  | h = module_header ; ml = module_item_list ; ENDMODULE 
   { (h, ml) }
  ;

/* module_header:
  | MODULE ; mi = IDENT ; LPAREN ; lp = list_of_port ; RPAREN ; SEMI 
    { (mi, lp) }
  ; */
module_header:
  | MODULE ; mi = IDENT ; LPAREN ; lp = list_of_port_declarations ; RPAREN ; SEMI 
    { (mi, lp) }
  ;

/* list_of_port:
  | p = port { [p] }
  | p = port ; COMMA ; lp = list_of_port { p :: lp }
  ; */

list_of_port_declarations:
  | p = ansi_port_declaration { [p] }
  | p = ansi_port_declaration ; COMMA ; lp = list_of_port_declarations { p :: lp }
  ;

/* port:
  | pe = port_expression { Port pe }
  | DOT ; id = IDENT ; LPAREN ; RPAREN { Port_with_name (id, []) }
  | DOT ; id = IDENT ; LPAREN ; pe = port_expression ; RPAREN { Port_with_name (id, pe) }
  ; */

/* TODO add other patterns */
ansi_port_declaration:
  | iph = option(interface_port_header) ; pi = IDENT { Interface_port_header (iph, pi) }
  | nph = option(net_port_header) ; pi = IDENT { Net_port_header (nph, pi) }
  ;

interface_port_header:
  | ii = IDENT ; DOT ; mi = IDENT { (ii, Some mi) }
  | ii = IDENT { (ii, None) }
  ;

net_port_header:
  | pd = option(PORT_DIRECTION) ; npt = net_port_type { (pd, npt) } 

/* port_expression:
  | pr = port_reference { [pr] }
  | LBRACE ; lpr = list_of_port_reference ; RBRACE { lpr }
  ; */

/* list_of_port_reference:
  | pr = port_reference { [pr] }
  | pr = port_reference ; COMMA ; lpr = list_of_port_reference { pr :: lpr }
  ; */

/* port_reference:
  | id = IDENT ; cs = constant_select { (id, cs) }
  ; */

constant_select:
  | LBRACKET ; cr = constant_range ; RBRACKET { ([], Some cr) }
  | cbs = constant_bit_select ; LBRACKET ; cr = constant_range ; RBRACKET { (cbs, Some cr) }
  | cbs = constant_bit_select { (cbs, None) }
  ;

constant_bit_select: /* returns (constant_bit_select, constant_range option) */
  | /* empty */ { [] }
  | LBRACKET ; ce = constant_expression ; RBRACKET ; cbs = constant_bit_select 
    { ce :: cbs }
  ;  

module_item_list:
  | /* empty  */ { [] }
  | mi = module_item ; ml = module_item_list { mi :: ml } 
  ;

module_item:
  | pd = port_declaration ; SEMI { Module_port_declaration pd }
  | np = non_port_module_item { Module_non_port_module_item np }
  ;

port_declaration:
  | INOUT ; npt = net_port_type ; lpi = list_of_port_identifiers { Inout (npt, lpi) }
  | INPUT ; npt = net_port_type ; lpi = list_of_port_identifiers { Input_port (npt, lpi) }
  | INPUT ; vpt = data_type ; lvi = list_of_variable_identifiers { Input_variable (vpt, lvi) }
  | OUTPUT ; npt = net_port_type ; lpi = list_of_port_identifiers { Output_port (npt, lpi) }
  | OUTPUT ; vpt = data_type ; lvi = list_of_variable_identifiers { Output_variable (vpt, lvi) }
  | id = IDENT ; lii = list_of_interface_identifiers { Port_interface_port_declaration (id, None, lii) }
  | id = IDENT ; DOT ; mi = IDENT ; lii = list_of_interface_identifiers { Port_interface_port_declaration (id, Some mi, lii) }
  ;

list_of_interface_identifiers:
  | id = IDENT ; upl = unpacked_dimension_list { [(id, upl)] }
  | id = IDENT ; upl = unpacked_dimension_list ; lii = list_of_interface_identifiers { (id, upl) :: lii }
  ;

list_of_port_identifiers:
  | /* empty  */ { [] }
  | pi = IDENT ; pdl = packed_dimension_list  ; pl = list_of_port_identifiers { (pi, pdl) :: pl } 
  ;

net_port_type:
  | dt = data_type { Net_port_type_data_type dt }
  | nt = IDENT { Net_port_type_ident nt }
  ;

list_of_variable_identifiers:
  | /* empty  */ { [] }
  | vi = IDENT ; pdl = packed_dimension_list  ; vl = list_of_port_identifiers { (vi, pdl) :: vl } 
  ;

non_port_module_item:
  | id = IDENT ; pva = option(parameter_value_assignment) ; lhi = list_of_hierarchical_instance ; SEMI 
    { Module_instantiation (id, pva, lhi) } 
  | mci = module_common_item { Module_common_item mci }
  ; 
  (* TODO : ADD
    module_declaration
    | interface_declaration
  *)

parameter_value_assignment:
  | SHARPLPAREN ; lpa = list_of_parameter_assignments ; RPAREN { lpa }
  ;

list_of_parameter_assignments:
  | lop = list_of_ordered_parameter_assignments { Ordered_parameter_assignment lop }
  | lnp = list_of_named_parameter_assignments { Named_parameter_assignment lnp }
  ;

list_of_ordered_parameter_assignments:
  | pe = param_expression { [pe] }
  | pe = param_expression ; COMMA ; lpa = list_of_ordered_parameter_assignments { pe :: lpa }
  ;

list_of_named_parameter_assignments:
  | DOT ; pi = IDENT ; LPAREN ; dt = option(param_expression) ; RPAREN { [(pi, dt)] }
  | DOT ; pi = IDENT ; LPAREN ; dt = option(param_expression) ; RPAREN ; COMMA ; ldt = list_of_named_parameter_assignments 
    { (pi, dt) :: ldt }
  ;

list_of_hierarchical_instance:
  | hi = hierarchical_instance { [hi] }
  | hi = hierarchical_instance ; COMMA ; lhi = list_of_hierarchical_instance { hi :: lhi }
  ;

hierarchical_instance:
  | id = IDENT ; upd = option(unpacked_dimension)  ; LPAREN ; lpc = list_of_port_connections ; RPAREN 
    { (id, upd, lpc) }

list_of_port_connections:
  | lop = list_of_ordered_port_connection { Ordered_port_connection lop }
  | lnp = list_of_named_port_connection { Named_port_connection lnp }
  ;

list_of_ordered_port_connection:
  | e = expression { [ e ]}
  | e = expression ; COMMA ; el = list_of_ordered_port_connection { e :: el }

list_of_named_port_connection:
  | npc = named_port_connection { [npc] }
  | npc = named_port_connection ; COMMA ; lnpc = list_of_named_port_connection { npc :: lnpc } 
  ;

named_port_connection:
  | DOT ; pi = IDENT ; LPAREN ; dt = option(expression) ; RPAREN { (pi, Some dt) }
  | DOT ; pi = IDENT { (pi, None) }
  ;

(***************************************************************************************************)

module_common_item:
  | itm = module_or_generate_item_dec { Module_or_generate_item_dec itm }
  | itm = interface_instantiation { Interface_instantiation itm }
  | itm = program_instantiation { Program_instantiation itm }
  | itm = continuous_assign { Continuous_assign itm }
  | itm = initial_construct { Initial_construct itm }
  | itm = always_construct { Always_construct itm }
  ;

module_or_generate_item_dec:
  | dd = data_declaration { Data_declaration dd }
  | pd = parameter_declaration ; SEMI { Parameter_declaration pd }
  ;

data_declaration:
  | dt = data_type ; lvda = list_of_variable_decl_assignments ; SEMI
    { Variable_declaration (dt, lvda) }
  | td = type_declaration { Type_declaration td }
  ;

list_of_variable_decl_assignments:
  | vda = variable_decl_assignment { [vda] }
  | vda = variable_decl_assignment ; COMMA ; vdal = list_of_variable_decl_assignments
    { vda :: vdal }
  ;

variable_decl_assignment:
  | id = IDENT ; lvd = list_of_variable_dimension ; EQUAL ; e = expression
  { (id, lvd, Some e) }
  | id = IDENT ; lvd = list_of_variable_dimension { (id, lvd, None) }
  ;

type_declaration:
  | TYPEDEF ; dt = data_type ; ti = IDENT ; lvd = list_of_variable_dimension  ; SEMI
    { Type_dec_data_type (dt, ti, lvd) }
  | TYPEDEF ; iii = IDENT ; cbs = constant_bit_select ; DOT ; ti1 = IDENT ; ti2 = IDENT ; SEMI
    { Type_dec_interface (iii, cbs, ti1, ti2) }
  | TYPEDEF ; STRUCT ; ti = IDENT ; SEMI { Type_dec_struct ti }
  ;

list_of_variable_dimension:
  | /* empty */ { [] }
  | LBRACKET ; RBRACKET ; lvd = list_of_variable_dimension 
    { Unsized_dimension :: lvd }
  | LBRACKET ; cr = constant_range ; RBRACKET ; lvd = list_of_variable_dimension 
    { (Unpacked_dimension_range cr) :: lvd }
  | LBRACKET ; ce = constant_expression ; RBRACKET ; lvd = list_of_variable_dimension 
    { (Unpacked_dimension_exp ce) :: lvd }
  ;

parameter_declaration:
  | PARAMETER ; dt = data_type ; lpa = list_of_param_assignments
    { Parameter_dec_param_assignments (dt, lpa) }
  | PARAMETER ; TYPE ; lta = list_of_type_assignments
    { Parameter_dec_type_assignments lta }
  ;

list_of_param_assignments:
  | pa = param_assignment { [pa] }
  | pa = param_assignment ; lpa = list_of_param_assignments { pa::lpa }
  ;

param_assignment:
  | id = IDENT ; lup = unpacked_dimension_list ; EQUAL ; pe = param_expression
    { (id, lup, Some pe) }
  | id = IDENT ; lup = unpacked_dimension_list 
    { (id, lup, None) }
  ;

param_expression:
  | dt = data_type { Param_exp_data_type dt }
  | ce = constant_expression { Param_exp_constant ce }
  ;

list_of_type_assignments:
  | ta = type_assignment { [ta] }
  | ta = type_assignment ; lta = list_of_type_assignments { ta::lta }
  ;

type_assignment:
  | id = IDENT ; EQUAL ; dt = data_type
    { (id, Some dt) }
  | id = IDENT 
    { (id, None) }
  ;

interface_instantiation:
  | ii = IDENT ; pva = option(parameter_value_assignment) ; lhi = list_of_hierarchical_instance ; SEMI
  { (ii, pva, lhi) }
  ;
 
program_instantiation:
  | pi = IDENT ; pva = option(parameter_value_assignment) ; list_of_hierarchical_instance ; SEMI
    { (pi, pva, lhi) }
  ;

continuous_assign:
  | ASSIGN ; lna = list_of_net_assignments ; SEMI { Net_assignment lna }
  | ASSIGN ; lva = list_of_variable_assignments ; SEMI { Variable_assignment lva }
  ;

list_of_net_assignments:
  | na = net_assignment { [na] }
  | na = net_assignment ; lna = list_of_net_assignments { na :: lna }
  ;

net_assignment:  
  | nlv = net_lvalue ; EQUAL ; e = expression { (nlv, e) }
  ;

net_lvalue:
  | phi = hierarchical_ident ; cs = constant_select { Net_ident_const_select (phi, cs) }
  | LBRACE ; lnl = list_of_net_lvalue ; RBRACE { Net_lvalues_concat lnl }
  | apet = option(assignment_pattern_expression_type) ; apnl = assignment_pattern_net_lvalue 
    { Net_assignment_pattern (apet, apnl)}
  ;

list_of_net_lvalue:
  | nl = net_lvalue { [nl] }
  | nl = net_lvalue ; COMMA ; lnl = list_of_net_lvalue { nl :: lnl }
  ;

assignment_pattern_expression_type:
  | id = IDENT { Ape_ps_parameter_identifier id }
  | t = INTEGER_ATOM_TYPE { Ape_integer_atom_type t }
  ;

assignment_pattern_net_lvalue:
  | BACKLBRACE ; lnl = list_of_net_lvalue ; RBRACE { lnl }
  ;

list_of_variable_assignments:
  | va = variable_assignment { [va] }
  | va = variable_assignment ; lva = list_of_variable_assignments { va :: lva }
  ;

variable_assignment:  
  | vlv = variable_lvalue ; EQUAL ; e = expression { (vlv, e) }
  ;

variable_lvalue:
  | phi = hierarchical_ident ; s = select { Variable_ident_const_select (phi, s) }
  | LBRACE ; lnl = list_of_variable_lvalue ; RBRACE { Variable_lvalues_concat lnl }
  | apet = option(assignment_pattern_expression_type) ; apnl = assignment_pattern_variable_lvalue 
    { Variable_assignment_pattern (apet, apnl)}
  ;

select:
  | LBRACKET ; cr = constant_range ; RBRACKET { ([], Some cr) }
  | bs = bit_select ; LBRACKET ; cr = constant_range ; RBRACKET { (bs, Some cr) }  
  | bs = bit_select ; { (bs, None) }
  ;

bit_select:
  | /* empty */ { [] }
  | LBRACKET ; e = expression ; RBRACKET ; bs = bit_select { e :: bs }
  ;

list_of_variable_lvalue:
  | vl = variable_lvalue { [vl] }
  | vl = variable_lvalue ; COMMA ; lvl = list_of_variable_lvalue { vl :: lvl }
  ;

assignment_pattern_variable_lvalue:
  | BACKLBRACE ; lvl = list_of_variable_lvalue ; RBRACE { lvl }
  ;


initial_construct: 
  | INITIAL ; s = statement { s }
  ;

statement:
  | s = blocking_assignment ; SEMI { Blocking_assignment s }
  | s = nonblocking_assignment ; SEMI { Nonblocking_assignment s }
  | s = procedural_continuous_assignment ; SEMI 
    { Procedural_continuous_assignment s }
  | s = case_statement { Case_statement s }
  | s = conditional_statement { Conditional_statement s }
  | s = loop_statement { Loop_statement s }
  | s = seq_block { Seq_block s }
  ;

blocking_assignment: 
  | vl = variable_lvalue ; EQUAL ; e = expression
    { (vl, "=", e) }
  | vl = variable_lvalue ; aop = ASSIGNMENT_OP ; e = expression
    { (vl, aop, e) }
  ;

nonblocking_assignment: 
  | vl = variable_lvalue ; LESSEQUAL ; e = expression
    { (vl, "<=", e) }
  ;

procedural_continuous_assignment:
  | ASSIGN ; va = variable_assignment { va }

case_statement:
  | p = CASE ; LPAREN ; ce = expression ; RPAREN ; lci = list_of_case_item ; ENDCASE
  { (p, ce, lci) }

list_of_case_item:
  | ci = case_item { [ci] }
  | ci = case_item ; lc = list_of_case_item { ci :: lc }
  ;

case_item: 
  | le = list_of_expression ; COLON ; s = statement { Case_branch (le, s) }
  | DEFAULT ; COLON ; s = statement { Case_default s }
  ;

/* conditional_statement:
  | IF ; LPAREN ; cp = expression ; RPAREN ; s1 = statement ; le = list_of_else_claus ; ELSE ; s2 = statement
    { Cond (((Some cp, s1) :: le) @ [(None, s2)]) }
  | IF ; LPAREN ; cp = expression ; RPAREN ; s = statement ; le = list_of_else_claus 
    { Cond ((Some cp, s) :: le) }
  ; */

conditional_statement:
  | p = IF ; LPAREN ; cp = expression ; RPAREN ; s1 = statement
    { Cond_if (p, cp, s1) }
  | p = IF ; LPAREN ; cp = expression ; RPAREN ; s = statement ; ELSE ; cs = conditional_statement
    { Cond_if_else (p, cp, s, cs) }
  | p = IF ; LPAREN ; cp = expression ; RPAREN ; s1 = statement ; ELSE ; s2 = statement
    { Cond_if_else (p, cp, s1, Cond_else s2) }
  ;

/* list_of_else_claus:
  | { [] } // empty
  | ELSE ; IF ; LPAREN ; cp = expression ; RPAREN ; s = statement ; lec = list_of_else_claus
    { (Some cp, s) :: lec }
  ; */

loop_statement:
  | FOR ; LPAREN ; fi = list_of_variable_assignments ; e = option(expression) ; fs = list_of_operator_assignment ; RPAREN ; s = statement
    { For (fi, e, fs, s) }
  ;

list_of_operator_assignment:
  | oa = operator_assignment { [oa] }
  | oa = operator_assignment; COMMA; loa = list_of_operator_assignment { oa :: loa }
  ;

operator_assignment:
  | vl = variable_lvalue ; EQUAL ; e = expression
    { (vl, "=", e) }
  | vl = variable_lvalue ; aop = ASSIGNMENT_OP ; e = expression
    { (vl, aop, e) }
  ;

seq_block:
  | BEGIN ; lbi = list_of_block_item_declaration ; ls = list_of_statement ; END  
  { (lbi, ls) }
  ;

list_of_block_item_declaration:
  | /* empty */ { [] }
  | bid = module_or_generate_item_dec ; lbid = list_of_block_item_declaration { bid :: lbid }
  ;

list_of_statement:
  | /* expression */ { [] }
  | s = statement ; ls = list_of_statement { s :: ls }
  ;

list_of_expression:
  | e = expression { [e] }
  | e = expression ; COMMA ; le = list_of_expression { e :: le }
  ;

always_construct:
  | ALWAYS_COMB ; s = statement { Always_comb s }
  ;

expression:
  | p = primary { Primary p }
  | uo = OPERATOR ; e = expression { Unary (uo, e) }
  | e1 = expression ; bo = OPERATOR ; e2 = expression
    { Binary (e1, bo, e2) }
  | e1 = expression ; QUESTION ; e2 = expression ; COLON ; e3 = expression
    { Conditional (e1, e2, e3) }
  /* | LPAREN ; oa = operator_assignment ; RPAREN { Op_assignment oa } */
  /* TODO */
  ;

primary:
  | LPAREN ; e = expression ; RPAREN { Expression e }
  | pl = primary_literal { Primary_literal pl }  
  | hi = hierarchical_ident ; s = select { Hierarchical_ident (hi, s) }
  | c = concatenation ; LBRACKET ; re = range_expression ; RBRACKET
    { Concatenation (c, Some (re)) }
  | c = concatenation { Concatenation (c, None) }
  ;

hierarchical_ident:
  | id = IDENT { ([], id) }
  | id = IDENT ; DOT ; hi = hierarchical_ident
    { let (l, h) = hi in (id::l, h) }
  /* | id = IDENT ; cbs = constant_bit_select ; DOT ; hi = hierarchical_ident
    { let (l, h) = hi in ((id, cbs) :: l, h) } */
  /* TODO: support cbs */
  ;

concatenation:
  | LBRACE ; le = list_of_expression ; RBRACE { le }
  ;

range_expression:
  | e = expression { Range_expression_expression e }
  | cr = constant_range { Range_expression_range cr }
  ;



(***************************************************************************************************)

data_type:
  | t = INTEGER_VECTOR_TYPE ; s = option(SIGNING) ; ps = packed_dimension_list 
    { Integer_vector_type (t, s, ps) }
  | t = INTEGER_ATOM_TYPE ; s = option(SIGNING) { Integer_atom_type (t, s) }
  | t = NON_INTEGER_TYPE { Non_integer_type t }
  | STRUCT; LBRACE ; ss = struct_union_member_list ; RBRACE ; ps = packed_dimension_list
    {Struct_union (ss, ps) }
  | s = option(SIGNING) ; pd = packed_dimension_list { Implicit_data_type (s, pd) }
  ;

packed_dimension_list:
  | /* empty  */ { [] }
  | pd = packed_dimension ; pds = packed_dimension_list { pd :: pds }
  ;

unpacked_dimension_list:
  | /* empty  */ { [] }
  | upd = unpacked_dimension ; upds = unpacked_dimension_list { upd :: upds }
  ;  

packed_dimension:
  | LBRACKET ; cr = option(constant_range) ; RBRACKET { Packed_dimension cr }
  ;

unpacked_dimension:
  | LBRACKET ; cr = constant_range ; RBRACKET { Unpacked_dimension_range cr }
  | LBRACKET ; cr = constant_expression ; RBRACKET { Unpacked_dimension_exp cr }
  ;

constant_range:
  | ce1 = constant_expression ; COLON ; ce2 = constant_expression { (ce1, ce2) }
  ;

constant_expression:
  | uo = OPERATOR ; cp = constant_expression { Constant_unary (uo, cp) }
  | cp1 = constant_expression ; bo = OPERATOR ; cp2 = constant_expression 
    { Constant_binary (cp1, bo, cp2) }
  | cp1 = constant_expression ; QUESTION ; cp2 = constant_expression ; COLON ; cp3 = constant_expression
    { Constant_cond (cp1, cp2, cp3) }
  | cp = constant_primary { Constant_primary cp }  
  ;

constant_primary:
  | pl = primary_literal { Constant_literal pl }
  | LPAREN ; ce = constant_expression ; RPAREN { Constant_expression ce }
  ;

primary_literal:
  | n = number { Number_literal n }
  | s = STRING { String_literal s }
  ;

number:
  | n = INTEGER_NUMBER { Integer_number n }
  | r = REAL_NUMBER { Real_number r }
  ;

struct_union_member_list:
  | sm = struct_union_member { [sm] }
  | sm = struct_union_member ; lsm = struct_union_member_list { sm :: lsm }
  ;

struct_union_member:
  | dt = data_type; lvda = list_of_variable_decl_assignments ; SEMI
    { (dt, lvda) }
  ;
