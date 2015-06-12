(* *************************************************************** *)
(* ************** définition d'un programme Gubs ***************** *)
(* *************************************************************** *)

(* note : le cout est inversement proportionnel à la fitness du programme *)
(* un programme avec un cout elevé est considéré comme moins interessant qu'un programme avec un cout plus faible *)

(* Comparaison de deux éléments en fonction d'une fonction de cout, *)
(* ordre total assuré grace à Pervasive.compare *)
(* f : une foncction de cout *)
(* a1, a2 deux éléments à comparer grace à f *)
let cmp_with_cost  f a1 a2 =
 match (compare (f a1) (f a2)) with
	| 0 -> Pervasives.compare a1 a2
	| n -> n 
;;

(* ***************** définition d'un atome ************************ *)

(* type atome : une variable ou une constante *)
type atom_t = 
  | Var of string
  | Const of string
;;

(* type des attributs d'un agent : une variable dans le programme, une constante dans la BDD *)
type attribute_t = atom_t ;; 

(* cout d'un atome : le prix est fonction de la restrictivité de l'élément *)
let atom_cost = function
  |Const(_) -> 1
  |Var(_) -> 0
;;

(* ***************** définition d'un agent ************************ *)

(* nombre maximum d'attributs d'un agent *)
let _ATTMAXSIZE = 1000;;

(* ensemble d'attribut d'un agent *)
module Attribut_set = Set.Make (
  struct
    type t = attribute_t
    let compare = (cmp_with_cost atom_cost) 
  end)
;;
type att_set_t = Attribut_set.t ;;

(* état d'un agent : présent (true) ou absent (false) *)
type sign_t = bool ;; 

(* description d'un agent : un atom avec un état et un ensemble d'attributs *)
type agent_t = atom_t * sign_t * att_set_t ;;

(* cout d'un agent : fonction du type de son atom et de son nombre d'attributs *)
let agent_cost (atom,_,attributes) = _ATTMAXSIZE*(atom_cost atom) + Attribut_set.cardinal attributes;;


(* ***************** définition d'une relation causale ****************** *)

(* les differents type de causes dans le program Remanente, Persistante ou Normale *)
type cause_t = 
  | Rem
  | Pers
  | Norm
;;

(* ensemble d'agents *)
module Agent_set = Set.Make (
  struct
    type t = agent_t
    let compare = cmp_with_cost  agent_cost
  end)
;;
type agent_set_t = Agent_set.t ;;

(* un relation causale : deux ensembles d'agents relié par une cause dans un contexte donné *)
type rel_cause_t = { context:agent_set_t; cause:agent_set_t; effet:agent_set_t; typec:cause_t};;

(* cout d'une relation : fonction du cout totale de ses agents *)
let rel_cost a  =  
    let cmpc = Agent_set.fold (fun a b -> (agent_cost a)+b) a.cause 0 
    and ecmp = Agent_set.fold (fun a b -> (agent_cost a)+b) a.effet 0
    in (cmpc+ecmp)
;;


(* ***************** définition d'un environnement ****************** *)

(* table d'association agent -> attributs pour un environnement donné *)
module Attribute_map = Map.Make ( 
  struct 
    type t = atom_t
    let compare =  cmp_with_cost atom_cost
  end)
;;

(* type des attributs de la table *)
type att_list_t = {ord:attribute_t list; diff:attribute_t list};;

(* type de la table d'association *)
type attribute_map_t = att_list_t Attribute_map.t  ;;

(* Set de relations *)
module Rel_cause_set = Set.Make (
  struct
    type t = rel_cause_t
    let compare = cmp_with_cost rel_cost
  end)
;;
type rel_cause_set_t = Rel_cause_set.t ;;

(* un environnement : set de comportement nommé avec une table de variables *)
type env_t = { name:atom_t ; behaviours:rel_cause_set_t; atts:attribute_map_t; obs:agent_set_t list; subenv: env_t list} ;;

(* cout d'un environnement *)
let env_cost a = Rel_cause_set.fold (fun a b -> (rel_cost a)+b) a.behaviours 0 ;;


(* ***************** définition d'un programme ****************** *)

(* un programme décrit un objet biologique avec un ensembles d'environnements *)
type program_t = env_t list ;;

(* cout d'un programme *)
let prog_cost a = List.fold_right (fun a b -> (env_cost a)+b) a 0 ;;

(* ***************** définition d'un set de programm pour la bdd ************* *)

type bdd_program_t = program_t * int ;; (* index du programme dans la bdd *)

let bdd_prog_cost (p,id) = prog_cost p ;;

module Bdd_program_set = Set.Make (
  struct
    type t = bdd_program_t
    let compare = cmp_with_cost bdd_prog_cost
  end)
;;
type bdd_prog_set_t = Bdd_program_set.t ;;
