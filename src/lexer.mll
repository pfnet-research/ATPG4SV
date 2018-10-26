
{
  open Lexing
  open Parser
  
  exception Syntax_error of string
  
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let mk_integer base lexbuf =
    let n = lexeme lexbuf in
    match Str.(split (regexp {|'|}) n) with
    | [n] -> INTEGER_NUMBER (1, base, n)
    | [s; n] -> INTEGER_NUMBER (int_of_string s, base, n)
    | _ -> failwith @@ Printf.sprintf "unexpected error at mk_integer: %s" n

  let string_buffer = Buffer.create 256
  let reset_string_buffer () = Buffer.reset string_buffer
  let get_stored_string () = Buffer.contents string_buffer
  let store_string_char c = Buffer.add_char string_buffer c

  let get_pos lexbuf =
    (lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_curr_p.pos_cnum)

}
  
  let blank = [' ' '\t']+
  let newline = '\r' | '\n' | "\r\n"

  let integer_vector_type = "bit" | "logic" | "reg"
  let signing = "signed" | "unsigned"
  let integer_atom_type = "byte" | "shortint" | "int" | "longint" | "integer" | "time"
  let non_integer_type = "shortreal" | "real" | "realtime"
  let struct_union = "struct" | "union"
  let port_direction = "input" | "output" | "inout"
  let assignment_operator = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "|=" | "^=" | "<<=" | ">>=" | "<<<=" | ">>>="
  let operator = 
      "+" | "-" | "!" | "~" | "&" | "~&" | "|" | "~|" | "^" | "~^" | "^~"
      | "+" | "-" | "*" | "/" | "%"| "==" | "!=" | "===" | "!==" | "==?" | "&&" | "||" | "**" 
      | "<" | "<=" | ">" | ">=" | "&" | "|" | "^" | "^~" | "~^" | ">>" | "<<" | ">>>" | "<<<" | "->" | "<->"

  (* let unary_operator = "+" | "-" | "!" | "~" | "&" | "~&" | "|" | "~|" | "^" | "~^" | "^~"
  let binary_operator = "+" | "-" | "*" | "/" | "%"| "==" | "!=" | "===" | "!==" | "==?" | "&&" | "||" | "**" 
                      | "<" | "<=" | ">" | ">=" | "&" | "|" | "^" | "^~" | "~^" | ">>" | "<<" | ">>>" | "<<<" | "->" | "<->" *)

  let ident = ['a'-'z' 'A'-'Z' '_' '$'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*

  let decimal_digit = ['0'-'9']
  let decimal_base = "'" (['s'  'S']?) ['d' 'D']
  let binary_digit = ['0'-'1']
  let binary_base = "'" (['s'  'S']?) ['b' 'B']
  let octal_digit = ['0'-'7']
  let octal_base = "'" (['s'  'S']?) ['o' 'O']
  let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
  let hex_base = "'" (['s'  'S']?) ['h' 'H']

  let non_zero_decimal_digit = ['1'-'9']
  let non_zero_unsigned_number = non_zero_decimal_digit ( "_" | decimal_digit )*
  let unsigned_number = decimal_digit ( "_" | decimal_digit )*
  let decimal_number = unsigned_number | (non_zero_unsigned_number ?) decimal_base unsigned_number

  let binary_value = binary_digit ("_" | binary_digit)*
  let binary_number = (non_zero_unsigned_number ?) binary_base binary_value

  let octal_value = octal_digit ("_" | octal_digit)*
  let octal_number = (non_zero_unsigned_number ?) octal_base octal_value

  let hex_value = hex_digit ("_" | hex_digit)*
  let hex_number = (non_zero_unsigned_number ?) hex_base hex_value

  let fixed_point_number = unsigned_number '.' unsigned_number
  let real_number = fixed_point_number | unsigned_number ('.' unsigned_number) ? ['e' 'E'] ['+' '-'] unsigned_number
  
  (**)

  rule token = parse
  | blank { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | "//" { comments false lexbuf }
  | "/*" { comments true lexbuf }
  | "\"" { reset_string_buffer ();
           string lexbuf;
           STRING(get_stored_string()) }
  | "[" { LBRACKET }
  | "]" { RBRACKET }  
  | "?" { QUESTION }    
  | ":" { COLON } 
  | ";" { SEMI } 
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "." { DOT }
  | "#(" { SHARPLPAREN }
  | "," { COMMA }
  | "`{" { BACKLBRACE }
  | "=" { EQUAL }
  | "<=" { LESSEQUAL }
  | "struct" { STRUCT }
  | "module" { MODULE }
  | "endmodule" { ENDMODULE }
  (* | "inout" { INOUT }
  | "input" { INPUT }
  | "output" { OUTPUT } *)
  | "typedef" { TYPEDEF }
  | "parameter" { PARAMETER }
  | "type" { TYPE }
  | "assign" { ASSIGN }
  | "case" { CASE (get_pos lexbuf) }
  | "endcase" { ENDCASE }
  | "default" { DEFAULT }
  | "if" { IF (get_pos lexbuf) }
  | "else" { ELSE }
  | "for" { FOR }
  | "begin" { BEGIN }
  | "end" { END }
  | "always_comb" { ALWAYS_COMB }
  | "interface" { INTERFACE }
  | "endinterface" { ENDINTERFACE }
  | "modport" { MODPORT }
  | port_direction { PORT_DIRECTION (lexeme lexbuf) }
  | integer_vector_type { INTEGER_VECTOR_TYPE (lexeme lexbuf) }
  | integer_atom_type { INTEGER_ATOM_TYPE (lexeme lexbuf) }
  | non_integer_type { NON_INTEGER_TYPE (lexeme lexbuf) }
  | signing { SIGNING (lexeme lexbuf) }
  (* | random_qualifier { RANDOM_QUALIFIER (lexeme lexbuf) } *)
  (* | unary_operator { UNARY_OPERATOR (lexeme lexbuf) } *)
  (* | binary_operator { BINARY_OPERATOR (lexeme lexbuf) } *)
  | operator { OPERATOR (lexeme lexbuf) }
  | decimal_number { mk_integer Integer_dec lexbuf }
  | binary_number { mk_integer Integer_bin lexbuf }
  | octal_number { mk_integer Integer_oct lexbuf }
  | hex_number { mk_integer Integer_hex lexbuf }
  | real_number { REAL_NUMBER (float_of_string @@ lexeme lexbuf) }
  | assignment_operator { ASSIGNMENT_OP (lexeme lexbuf) }
  | ident { IDENT (lexeme lexbuf) }
  
  and comments block = parse
  | newline { next_line lexbuf; if block then comments block lexbuf else token lexbuf }
  | eof     { raise @@ Syntax_error "eof in comment" }
  | "*/"    { if block then token lexbuf else comments block lexbuf}
  | _       { comments block lexbuf }

  and string = parse
  | newline { raise @@ Syntax_error "newline in string" }
  | eof     { raise @@ Syntax_error "eof in comment" }  
  | "\""    { () }
  | _       { store_string_char(Lexing.lexeme_char lexbuf 0);
              string lexbuf }

