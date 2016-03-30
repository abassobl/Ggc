{
open Lexing;;
open Bdd_parser;;
(*gestion des commentaires imbriqués *)
let com_level = ref 0;;

}
(* définition du format des expressions utilisées *)
let c_open   = "/*"
let c_close  = "*/"
let com ="//"
let word     = ['a'-'z']+['a'-'z''A'-'Z''0'-'9''_']*
let const = ['A'-'Z']+['a'-'z''A'-'Z''0'-'9''_']*
let blanck = ['\t'' ']+
let number = ['0'-'9']+


(* rule début de parsing*)
rule start = parse
 | eof            { T_EOF; }
 | c_open         { com_level:=1;r_comment lexbuf; }
 | com            { s_comment lexbuf; }
 | blanck         { start lexbuf; }
 | '\n'           { incr Exception.current_line; start lexbuf; }
 | "<name>"		  	{ B_NAME; }
 | "</name>"    	{ E_NAME; } 
 | "<meta>"		  	{ T_MDATA (string_parse "" lexbuf); }
 | "<prog>"				{ T_PROG (string_parse "" lexbuf); }
 | const					{ T_WORD (Lexing.lexeme lexbuf); }
 | _              { let lex=(Lexing.lexeme lexbuf)
			              and pos=(Lexing.lexeme_start_p lexbuf) 
			              and lex_end=(Lexing.lexeme_end lexbuf) in
 				                raise (Exception.LexException("unbound value", lex, pos, lex_end)); } 

(* non traitement du programme du composant *)
and string_parse string_b= parse
 | "</prog>" { (string_b); }
 | "</meta>" { (string_b); }
 | _				 { string_parse ((string_b)^(Lexing.lexeme lexbuf)) lexbuf; }


(* ******************	commentaires ************************ *)											
(* parsing comments *)
and r_comment = parse
 | c_close        { com_level:=!com_level-1;if !com_level=0 then start lexbuf else r_comment lexbuf; }
 | c_open         { com_level:=!com_level+1;r_comment lexbuf; }
 | '\n'           { Exception.current_line:=(let posi =(Lexing.lexeme_start_p lexbuf) in posi.pos_lnum);r_comment lexbuf; }
 | _              { r_comment lexbuf; }
 | eof            { T_EOF; }

and s_comment = parse
 | '\n'           { Exception.current_line:=(let posi =(Lexing.lexeme_start_p lexbuf) in posi.pos_lnum);start lexbuf; }
 | eof            { T_EOF; }
 | _              { s_comment lexbuf; }

{ (*epilogue*) }