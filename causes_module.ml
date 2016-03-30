open Internal_form;;
#use "internal_form.ml";;

(* set de relation causales labellées *)
module Rel_cause_set_l = Set.Make (
  struct
    type t = rel_cause_t * int
    let compare = Pervasives.compare
  end)
;;
type rel_cause_set_l_t = Rel_cause_set_l.t ;;


(* convertis un programme p en un ensemble de relation causales avec un id *)
let convert_to_cause_set p id  c_set = match p with
	| [] -> 

(*convertis un set de programme en un ensemble de relation causales avec l'id correspondant au programme *)
let convert_s_to_cause_set prog_set = 
	Bdd_program_set.fold (fun (p,id) c_set -> convert_to_cause_set p id c_set) prog_set (Rel_cause_set_l.empty)
;;

(* appel la fonction de matching des causes *)
(* de type : program_t -> bdd_prog_set_t -> bdd_prog_set_t *)
let cause_subst p p_set = 
		cause_subst_main (convert_to_cause_set p (-1)) (convert_s_to_cause_set p_set ) 
;;