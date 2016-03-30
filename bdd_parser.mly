
%{
open Internal_form ;;
let parse_sub_prog st parser lexxer_tk = 
		let lexbuf =Lexing.from_string st in
			parser lexxer_tk lexbuf
;;
let parse st = parse_sub_prog st Gubs_parser.parsing Gubs_lexxer.start ;;

let flatten l_set = let (s1,s2,s3) = List.fold_left (fun (a,b,c) (a2,b2,c2) -> (a2::a,b2::b,c2::c)) ([],[],[]) l_set 
	in ((Array.of_list s1),(Array.of_list s2),(Array.of_list s3))
;;
%}
%token B_NAME
%token E_NAME
%token <string>T_WORD
%token <string>T_MDATA
%token <string>T_PROG
%token T_EOF

%start parsing
%type <(string  array)* (Internal_form.program_t array)*(string array)> parsing
%%

/* programme */
parsing : 
	| bdd_element_list T_EOF
			{ flatten $1 }
;
bdd_element_list :
	| bdd_element_list B_NAME T_WORD E_NAME T_MDATA T_PROG
			{ ($3,(parse $6),$5)::$1 }
	| B_NAME T_WORD E_NAME T_MDATA T_PROG
			{ [($2,(parse $5),$4 )] }
;


%%
