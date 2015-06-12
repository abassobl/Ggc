(* *************************************************************** *)
(* ************** d�finition d'un programme Gubs ***************** *)
(* *************************************************************** *)

(* note : le cout est inversement proportionnel � la fitness du programme *)
(* un programme avec un cout elev� est consid�r� comme moins interessant qu'un programme avec un cout plus faible *)

(* Comparaison de deux �l�ments en fonction d'une fonction de cout, *)
(* ordre total assur� grace � Pervasive.compare *)
(* f : une foncction de cout *)
(* a1, a2 deux �l�ments � comparer grace � f *)
let cmp_with_cost  f a1 a2 =
 match (compare (f a1) (f a2)) with
	| 0 -> Pervasives.compare a1 a2
	| n -> n 
;;

(* ***************** d�finition d'un atome ************************ *)

(* type atome : une variable ou une constante *)
type atom_t = 
  | Var of string
  | Const of string
;;

(* type des attributs d'un agent : une variable dans le programme, une constante dans la BDD *)
type attribute_t = atom_t ;; 

(* cout d'un atome : le prix est fonction de la restrictivit� de l'�l�ment *)
let atom_cost = function
  |Const(_) -> 1
  |Var(_) -> 0
;;

(* ***************** d�finition d'un agent ************************ *)

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

(* �tat d'un agent : pr�sent (true) ou absent (false) *)
type sign_t = bool ;; 

(* description d'un agent : un atom avec un �tat et un ensemble d'attributs *)
type agent_t = atom_t * sign_t * att_set_t ;;

(* cout d'un agent : fonction du type de son atom et de son nombre d'attributs *)
let agent_cost (atom,_,attributes) = _ATTMAXSIZE*(atom_cost atom) + Attribut_set.cardinal attributes;;


(* ***************** d�finition d'une relation causale ****************** *)

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

(* un relation causale : deux ensembles d'agents reli� par une cause dans un contexte donn� *)
type rel_cause_t = { context:agent_set_t; cause:agent_set_t; effet:agent_set_t; typec:cause_t};;

(* cout d'une relation : fonction du cout totale de ses agents *)
let rel_cost a  =  
    let cmpc = Agent_set.fold (fun a b -> (agent_cost a)+b) a.cause 0 
    and ecmp = Agent_set.fold (fun a b -> (agent_cost a)+b) a.effet 0
    in (cmpc+ecmp)
;;


(* ***************** d�finition d'un environnement ****************** *)

(* table d'association agent -> attributs pour un environnement donn� *)
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

(* un environnement : set de comportement nomm� avec une table de variables *)
type env_t = { name:atom_t ; behaviours:rel_cause_set_t; atts:attribute_map_t; obs:agent_set_t list; subenv: env_t list} ;;

(* cout d'un environnement *)
let env_cost a = Rel_cause_set.fold (fun a b -> (rel_cost a)+b) a.behaviours 0 ;;


(* ***************** d�finition d'un programme ****************** *)

(* un programme d�crit un objet biologique avec un ensembles d'environnements *)
type program_t = env_t list ;;

(* cout d'un programme *)
let prog_cost a = List.fold_right (fun a b -> (env_cost a)+b) a 0 ;;

(* ***************** d�finition d'un set de programm pour la bdd ************* *)

type bdd_program_t = program_t * int ;; (* index du programme dans la bdd *)

let bdd_prog_cost (p,id) = prog_cost p ;;

module Bdd_program_set = Set.Make (
  struct
    type t = bdd_program_t
    let compare = cmp_with_cost bdd_prog_cost
  end)
;;
type bdd_prog_set_t = Bdd_program_set.t ;;
