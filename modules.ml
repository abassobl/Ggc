open Causes_module;;
open Agents_module;;
open Strong_obs_module;;
(* ajouter les modules supplémentaires à utiliser ici *)
open Unification;;
let module_list = [
	cause_subst;
	agent_subst;
	strong_obs
];;
(* ajouter la fonction principale du module supplémentaire à cette liste *)
(* les fonctions sont de type : program_t -> bdd_prog_set_t -> bdd_prog_set_t *)
(* les fonctions doivent renvoyer un empty set si les composants ne couvrent pas le programme *)

(* fonction d'appel des modules *)
let call_modules mliste p bdd_p_s = match mlist with
	| [] -> bdd_p_s
	| f::t -> (f p (call_modules t p bdd_p_s))
;;

(* fonction viabilité des ensembles de composants *)
let resolve p bdd_p_s sigma cpnt_set = let (si,cp) = unification (call_modules module_list p bdd_p_s)
	in begin
		sigma := (!sigma)@si;
		cpnt_set := (!cpnt_set)@cp;
		if(Sigma_map.is_empty si) then false
		else true
	end
;;

