%{
open Internal_form ;;

(* la liste des variables d'un environnement *)
let env_var = ref [] ;;
let void_count = ref 0 ;;

(* ajout d'une variable a l'environnement *)
let push a = env_var:= a::(!env_var) ;;

(* fonctions utilitaires sur les listes : head et tail *)
let tail l = try List.tl l with _ -> [];;
let head l = try List.hd l with _ -> [];;
let rem_hd ()=env_var := tail !env_var ;;
let push_var v = env_var:= (v::(head !env_var))::(tail !env_var) ;;

let define a = List.fold_left (fun k l -> (List.exists (fun x -> x=a ) l) ||(a=Const("True") || a=Const("False")) || k) false !env_var ;;
let get_string a =match a with
 | Var(x) -> x
 | Const(x) -> x
;;
 
let set_name a n =match a with
 | Var(_) -> Var(n)
 | Const(_) -> Const(n)
;;
let make_map l = List.fold_right (fun (a,b) m -> Attribute_map.add a b m ) l Attribute_map.empty ;;
let make_ag_s l = List.fold_right (fun a s -> Agent_set.add a s ) l Agent_set.empty ;;
let make_att_s l = List.fold_right (fun a s -> Attribut_set.add a s ) l Attribut_set.empty ;;
let make_r_c_s l = List.fold_right (fun a s -> Rel_cause_set.add a s ) l Rel_cause_set.empty ;; 
%}
%token T_EOF
%token VOID
%token ATT
%token OBS
%token BEHAVE
%token BEG
%token END
%token L_PAR
%token R_PAR
%token L_BRACE
%token R_BRACE
%token L_DOT
%token COMMA
%token D_DOT
%token <string> CONST
%token <string>VAR
%token N_CAUSE
%token P_CAUSE
%token R_CAUSE
%token NEG
%token LESS
%token MORE
%token DIFF

%start parsing
%type <Internal_form.program_t> parsing
%%

/* programme */
parsing : 
	| compartment_list{
			$1
		}
	| compartment_list T_EOF{
			$1
		}
;
/* liste de compartiments */
compartment_list : 
	| compartment_list L_DOT compartment {
			$1@[$3]
		}
	| compartment{
			[$1]
		}
;

/* un compartiment */
compartment: 
	| CONST BEG compartment_content END {
			push[];let (a,o,b,e) = $3 in let e1 =
			List.map (fun x ->{ x with name=set_name x.name ($1^"."^(get_string x.name))}) e
			in {name=Const($1);behaviours=b; atts=a; obs=o; subenv=e1}
		}
	| VAR BEG compartment_content END {
			push[];let (a,o,b,e) = $3 in let e1 =
			List.map (fun x ->{ x with name=set_name x.name ($1^"."^(get_string x.name))}) e
			in {name=Var($1);behaviours=b; atts=a; obs=o; subenv=e1}
		}
	| BEG compartment_content END {
			push[];incr void_count;let (a,o,b,e) = $2 in let e1 =
			List.map (fun x ->{ x with name=set_name x.name ("Void"^(string_of_int !void_count)^"."^(get_string x.name))}) e
			in {name=Const("Void"^(string_of_int !void_count));behaviours=b; atts=a; obs=o; subenv=e1}
		}

/* contenu d'un compartiment */
compartment_content :
	| ATT BEG att_content_l END OBS BEG obs_content_l END BEHAVE BEG bh_content_l END compartment_list{
			rem_hd ();((make_map $3),$7,(make_r_c_s $11),$13)
		}
	| ATT BEG att_content_l END OBS BEG obs_content_l END BEHAVE BEG bh_content_l END{
			rem_hd ();((make_map $3),$7,(make_r_c_s $11),[])
		}
	| ATT BEG att_content_l END BEHAVE BEG bh_content_l END compartment_list{
			rem_hd ();((make_map $3),[],(make_r_c_s $7),$9)
		}
	| ATT BEG att_content_l END BEHAVE BEG bh_content_l END{
			rem_hd ();((make_map $3),[],(make_r_c_s $7),[])
		}
;
/* liste d'association agent:attribut */
att_content_l :
  | att_content_l L_DOT att_content {
		  $3::$1
		}
	| att_content {
			[$1]
		}
;

/* association agent-attributs */ 
att_content :
	| VAR D_DOT L_BRACE order_list R_BRACE L_BRACE diff_list R_BRACE {
      push_var (Var $1);(Var($1),{ord=$4;diff=$7})
		}
	| VAR D_DOT L_BRACE order_list R_BRACE L_BRACE R_BRACE {
      push_var (Var($1));(Var($1),{ord=$4;diff=[]})
		}
	| VAR D_DOT L_BRACE R_BRACE L_BRACE diff_list R_BRACE {
      push_var (Var($1));(Var($1),{ord=[];diff=$6})
		}
	| VAR D_DOT L_BRACE R_BRACE L_BRACE R_BRACE {
      push_var (Var($1));(Var($1),{ord=[];diff=[]})
		}
	| CONST D_DOT L_BRACE order_list R_BRACE L_BRACE diff_list R_BRACE {
			push_var (Const($1));(Const($1),{ord=$4;diff=$7})
		}
	| CONST D_DOT L_BRACE order_list R_BRACE L_BRACE R_BRACE {
      push_var (Const($1));(Const($1),{ord=$4;diff=[]})
		}
	| CONST D_DOT L_BRACE R_BRACE L_BRACE diff_list R_BRACE {
      push_var (Const($1));(Const($1),{ord=[];diff=$6})
		}
	| CONST D_DOT L_BRACE R_BRACE L_BRACE R_BRACE {
      push_var (Const($1));(Const($1),{ord=[];diff=[]})
		}
;
/* liste ordonnée d'attributs*/
order_list :
	|	order_list MORE VAR {
			$1@[(Var($3))]
		} 
	|	order_list LESS VAR {
			(Var($3))::$1
		} 
	| VAR {
			[Var($1)]
		}
;
diff_list :
  |	diff_list DIFF VAR {
		(Var $3)::$1
		} 
	| VAR {
			[Var($1)]
		}
;
/* liste non ordonnée d'attributs */
/* liste d'observers */
obs_content_l :
  | obs_content_l L_DOT obs_content {
		  $3::$1
		}
	| obs_content {
			[$1]
		}
;
/* un observer */
obs_content :
  | VAR D_DOT agent_l {
		  make_ag_s $3
		}
;
/* une liste d'agent */	
agent_l :
	|	agent_l COMMA agent {
			$3::$1
		} 
	| agent {
			[$1]
		}
;

/* un agent */
agent :
	| VAR L_PAR atom_l R_PAR {
			if(define (Var $1)) then (Var($1),true,make_att_s $3) else Exception.parse_error "The variable is not defined" $1
		}
	| CONST L_PAR atom_l R_PAR {
			if(define (Const $1)) then (Const($1),true,make_att_s $3) else Exception.parse_error "The constant is not defined" $1
		}
	| NEG VAR L_PAR atom_l R_PAR {
			if(define (Var $2)) then (Var($2),false,make_att_s $4) else Exception.parse_error "The variable is not defined" $2
		}
	| NEG CONST L_PAR atom_l R_PAR {
			if(define (Const $2)) then (Const($2),false,make_att_s $4) else Exception.parse_error "The constant is not defined" $2
		}
	| VAR {
			if(define (Var $1)) then (Var($1),true,make_att_s []) else Exception.parse_error "The variable is not defined" $1
		}
	| CONST {
			if(define (Const $1)) then (Const($1),true,make_att_s []) else Exception.parse_error "The constant is not defined" $1
		}
	| NEG VAR{
			if(define (Var $2)) then (Var($2),false,make_att_s []) else Exception.parse_error "The variable is not defined" $2
		}
	| NEG CONST{
			if(define (Const $2)) then (Const($2),false,make_att_s []) else Exception.parse_error "The constant is not defined" $2
		}
;
/* liste d'atomes */
atom_l :
  |	atom_l COMMA VAR {
			(Var($3))::$1
		} 
	|	atom_l COMMA CONST {
			(Const($3))::$1
		} 
	| VAR {
			[Var($1)]
		}
	| CONST {
			[Const($1)]
		}
;
/* liste de behaviors */
bh_content_l :
	| bh_content_l L_DOT bh_content {
		  $3::$1
		}
	| bh_content {
			[$1]
		}
;
/* behavior */
bh_content :
  | agent_l N_CAUSE agent_l {
			{ context=Agent_set.empty; cause=(make_ag_s $1); effet=(make_ag_s $3); typec=Norm}
		}
  | agent_l P_CAUSE agent_l {
			{ context=Agent_set.empty; cause=(make_ag_s $1); effet=(make_ag_s $3); typec=Pers}
		}
	|	agent_l R_CAUSE agent_l {
			{ context=Agent_set.empty; cause=(make_ag_s $1); effet=(make_ag_s $3); typec=Rem}
		}
	| L_BRACE R_BRACE BEG agent_l N_CAUSE agent_l END {
			{ context=Agent_set.empty; cause=(make_ag_s $4); effet=(make_ag_s $6); typec=Norm}
		}
  | L_BRACE R_BRACE BEG agent_l P_CAUSE agent_l END {
			{ context=Agent_set.empty; cause=(make_ag_s $4); effet=(make_ag_s $6); typec=Pers}
		}
	|	L_BRACE R_BRACE BEG agent_l R_CAUSE agent_l END {
			{ context=Agent_set.empty; cause=(make_ag_s $4); effet=(make_ag_s $6); typec=Rem}
		}
	| L_BRACE agent_l R_BRACE BEG agent_l N_CAUSE agent_l END {
			{ context=(make_ag_s $2); cause=(make_ag_s $5); effet=(make_ag_s $7); typec=Norm}
		}
	| L_BRACE agent_l R_BRACE BEG agent_l P_CAUSE agent_l END {
			{ context=(make_ag_s $2); cause=(make_ag_s $5); effet=(make_ag_s $7); typec=Pers}
		}
	| L_BRACE agent_l R_BRACE BEG agent_l R_CAUSE agent_l END {
			{ context=(make_ag_s $2); cause=(make_ag_s $5); effet=(make_ag_s $7); typec=Rem}
		}
%%
