(* ***************** définition de sigma : substitution de variables *************** *)
module Sigma_map = Map.Make ( 
  struct 
    type t = (atom_t * att_list_t)
    let compare = Pervasives.compare
  end)
;; 
(* sigma est une substitution d'un atome et de ses attributs par un autre *)
type sigma_map_t = (atom_t * att_list_t) Sigma_map.t  ;;