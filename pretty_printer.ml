open Internal_form;;
(*#use "internal_form.ml";;*)

(* module pretty printer : *)
(* input : *)
(* 		* un channel d'output ouvert *)
(* 		* un programme gubs en forme interne *)
(* output : unit (dans le channel d'output (typiquement un fichier.pp) *)


(* ***************** fonction utilitaires *********************** *)
(* ************************************************************** *)

(* coupe le dernier élément d'une chaine de caractères *)
let cut_tail s = if ((String.length s) >0) then
										 let endc = String.get s ((String.length s) -1) in if(endc='\n') then
	 											((String.sub s 0 ((String.length s) - 2))^"\n")
												else String.sub s 0 ((String.length s) - 1) 
									else s;;

(* ajoute n tabulation pour une indentation correcte *)
let tab = ref 0;;

let rec put_tab t = if(t=0) then "" else "  "^(put_tab (t-1)) ;;  
(* ************************************************************** *)

(* ***************** écriture d'un atome ************************ *)
(* ************************************************************** *)

(* ecrit simplement le nom de l'atom : les constantes sont forcement en majuscule *)
let write_atom a = match a with 
  | Var(x) -> x
  | Const(x) -> x
;;

(* la fonction d'écriture des attributs, identique à celle des atomes *)
let write_att a = write_atom a ;;
(* ************************************************************** *)


(* ***************** écriture d'un agent ************************ *)
(* ************************************************************** *)

(* affiche le signe d'un agent *)
let write_sign a = if a then "" else "!" ;;

(* affiche la liste des attributs d'un agent *)
let write_att_set a = cut_tail (Attribut_set.fold (fun x b-> (write_att x)^","^b) a "") ;;

(* affiche l'agent : constitué de son signe, ses attributs et son nom *)
let write_agent (a,b,c) = (write_sign b)^(write_atom a)^"("^(write_att_set c)^")" ;; 
(* ***************************************************************** *)


(* ***************** écriture d'une relation causale ****************** *)
(* ******************************************************************** *)

(* affiche un set d'agent *)
let write_agent_set a = cut_tail (Agent_set.fold (fun x b-> (write_agent x)^","^b) a "") ;;

(* affiche le signe d'une relation causale suivant son type *)
let write_cause_t a = match a with
  | Rem -> " ++> "
  | Pers -> " ==> "
  | Norm -> " --> "
;;

(* affiche une relation causale : le contexte suivit de la liste des agents de la cause, du symbole de cause et enfin de l'effet *)
let write_rel_cause a = "["^(write_agent_set a.context)^"]{"^(write_agent_set a.cause)^(write_cause_t a.typec)^(write_agent_set a.effet)^"}" ;;
(* ********************************************************************* *)

(* ***************** écriture d'un environnement ****************** *)
(* **************************************************************** *)

(* fonction d'affichage d'une liste d'élément l avec un caractere de differentiation c *)
let write_list l c = let res = (List.fold_right (fun l b -> (write_atom l)^c^b) l "") in if c="<>" then cut_tail (cut_tail res) else cut_tail res;;

(* affiche la table des agents et de leur attributs pour l'environnement *)
let write_att_map a =(incr tab); let res = cut_tail (Attribute_map.fold (fun x y b-> (put_tab !tab)^(write_atom x)^":["^(write_list y.ord ">")^"]["^(write_list y.diff "<>")^"];\n"^b) a "") in decr tab;res ;;

(* affiche l'ensemble des relation causales de l'environnement *)
let write_rel_cause_set a =(incr tab); let res = cut_tail (Rel_cause_set.fold (fun x b-> (put_tab !tab)^(write_rel_cause x)^";\n"^b) a "") in decr tab;res;;


(* affiche l'ensemble des observateurs de l'environnement *)
let write_obs a =(incr tab); let res = cut_tail(List.fold_right (fun l b -> (put_tab !tab)^"obs:"^(write_agent_set l)^";\n"^b) a "")in decr tab;res;;

(* affiche l'environnement : la table des agents, les observateur, les relation causales et enfin les sous environnements *)
let rec write_env a = (incr tab);(write_atom a.name)^"{\n"^
												(put_tab !tab)^
												"@A{\n"^
													(write_att_map a.atts)^
												(put_tab !tab)^
												"}\n@O{\n"^
													(write_obs a.obs)^
												(put_tab !tab)^
												"}\n@B{\n"^
													(write_rel_cause_set a.behaviours)^
												(put_tab !tab)^
												"}\n"^
													(write_env_l a.subenv)^
											"}"
and write_env_l a= cut_tail (List.fold_right (fun l b -> (write_env l)^";\n"^b) a "") ;;
(* ************************************************************ *)

(* ***************** écriture d'un programme ****************** *)
let printp a chan = output_string chan (write_env_l a);;
(* ************************************************************ *)