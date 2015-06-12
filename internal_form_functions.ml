open Internal_form;;
(* ******************************************************************** *)
(* ************** fonction sur les structure internes ***************** *)
(* ******************************************************************** *)

(* ***************** fonctions sur les atomes ************************ *)

(* verifie si un atome a est une constante *)
let is_const a = match a with
  | Const(_) -> true
  | _ -> false
;;

(* donne le nom d'un atome *)
let get_name a = match a with
  | Const(n) -> n
	| Var(n) -> n
;;

(* ***************** fonction sur agent ************************ *)

(* verifie si un agent est une contante *)
let ag_is_const (atom,_,attributes) = is_const atom ;;

(* vérifie si un agent est négatif *)
let ag_is_neg (_,s,_) = s ;;

(* récupere l'atome d'un agent (pour avoir son nom par exemple) *)
let get_atom (a,_,_) = a;;


(* ***************** fonction sur relation causale ****************** *)

(* recuperation des constantes dans un set d'agents *)
let get_const s = Agent_set.filter (ag_is_const) s ;;

(* recupération des variables dans un set d'agents *)
let get_var s = Agent_set.diff (get_const s) ;;

(* récupération des agents négatifs dans un set d'agents *)
let get_neg s = Agent_set.filter (ag_is_neg) s ;;

(* récupération d'un agent par son nom dans un set d'agent *)
let get_ag name s = Agent_set.choose (Agent_set.filter (function a -> get_name (get_atom a) = name) s)
;;


(* ***************** fonctions sur environnement ****************** *)

(* vérifie si une liste d'attribut est un sous ensemble possible d'un autre *)
let att_list_is_subset atl1 atl2 = 
  (List.length atl1.ord) <= (List.length atl2.ord) &&
  (List.length atl1.diff) <= (List.length atl2.diff)
;;


(* ***************** fonction sur programme ****************** *)

