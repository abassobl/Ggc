{
open Lexing;;
open Gubs_parser;;
(*gestion des commentaires imbriqués *)
let com_level = ref 0;;
(* gestion des environnements *)

}
(* définition du format des expressions utilisées *)
let c_open   = "/*"
let c_close  = "*/"
let com ="//"
let word     = ['a'-'z']+['a'-'z''A'-'Z''0'-'9''_']*
let const = ['A'-'Z']+['a'-'z''A'-'Z''0'-'9''_']*
let blanck = ['\t'' ']+
let number = ['0'-'9']+


(* rule début de parsing *)
rule start = parse
 | eof            {T_EOF; }
 | c_open         { com_level:=1;r_comment lexbuf; }
 | com            { s_comment lexbuf; }
 | blanck         { start lexbuf; }
 | '\n'           { incr Exception.current_line; start lexbuf; }
 | "@A"		   			{ATT}
 | "attribute"    {ATT}
 | "@O"				  	{OBS}
 | "observer"     {OBS}
 | "@B"						{BEHAVE}
 | "behaviour"    {BEHAVE}
 | "begin"        {BEG}
 | "end"          {END}
 | '{'            {BEG}
 | '}' 						{END}
 | '(' 						{L_PAR}
 | ')' 						{R_PAR}
 | '['						{L_BRACE}
 | ']'						{R_BRACE}  
 | ';' 						{L_DOT}
 | ',' 						{COMMA}
 | ':' 						{D_DOT}
 | "True" 				{CONST "True"}
 | "False" 				{CONST "False"}  
 | ">>"           {N_CAUSE}
 | ">*"           {P_CAUSE}
 | ">+"						{R_CAUSE}
 | "-->"           {N_CAUSE}
 | "==>"           {P_CAUSE}
 | "++>"						{R_CAUSE}
 | '-' 	{NEG}  
 | '!'  {NEG} 
 | '<' 	{LESS}  
 | '>'  {MORE}
 | "!=" {DIFF}  
 | "<>"  {DIFF}   
 | word  as w     {VAR w } 
 | const as c			{CONST c }
 | _              { let lex=(Lexing.lexeme lexbuf)
			              and pos=(Lexing.lexeme_start_p lexbuf) 
			              and lex_end=(Lexing.lexeme_end lexbuf) in
 				                raise (Exception.LexException("unbound value", lex, pos, lex_end)); } 

(* ******************	commentaires ************************ *)											
(* parsing comments *)
and r_comment = parse
 | c_close        { com_level:=!com_level-1;if !com_level=0 then start lexbuf else r_comment lexbuf; }
 | c_open         { com_level:=!com_level+1;r_comment lexbuf; }
 | '\n'           { Exception.current_line:=(let posi =(Lexing.lexeme_start_p lexbuf) in posi.pos_lnum);r_comment lexbuf; }
 | _              { r_comment lexbuf; }
 | eof            { Gubs_parser.T_EOF; }

and s_comment = parse
 | '\n'           { Exception.current_line:=(let posi =(Lexing.lexeme_start_p lexbuf) in posi.pos_lnum);start lexbuf; }
 | eof            { Gubs_parser.T_EOF; }
 | _              { s_comment lexbuf; }

{ (*epilogue*) }