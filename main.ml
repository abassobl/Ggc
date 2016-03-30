(* options de compilation *)
(* ********************** *)
let optimisation = ref 0 ;; (* 0 : aucune, 1 : réécriture des n-causes en r-causes, 2 : réécriture des p-causes en n-causes et 1 *) 
let extends = ref false ;;(* utilisation de l'algorithme d'extention de pathway à la base de donnée : potentiellement long *)
let biol_var = ref false ;;(* utilisation de contraintes biologiques *)
let cause_sub_list = ref true ;;(* execution minimal du compilateur (pas d'utilité actuellement *)
let sub_prog = ref false ;;(* substitut les variables du programme par les constantes trouvés *)
let simul = ref false ;;(* effectue une simulation du reseau généré *)
let proof = ref false ;;(* effectue une preuve de correction du programme initial : securité ? sureté ? *)
let pretty = ref false ;;(* pretty print le programme *)
(* ********************** *)

(* données de bases *)
(* **************** *)
(* index 0 et 1 : consstantes True et False *)
let program = ref [] ;; (* le program est une liste d'environnements biologique, par defaut vide *)
let db_names = ref (Array.make 0 "") ;; (* tableau de tout les noms des composants de la bdd identifés par leur index *)
(*let db_programs = ref (Array.make 0 []) ;;*)(* tableau de tout les programme des composants de la bdd identifiés par leur index *)
let db_datas = ref (Array.make 0 "") ;;(* tableau de toute les meta-data des composants de la bdd identifiés par leur index *)
let db_weights = ref (Array.make 0 0) ;;(* tableau de tout les poids des composants de la bdd identifiés par leur index *)
(*let db_extends = ref (Array.make 0 []) ;;*)(* table d'association des composants étendus : index+db_name.length = index dans db_programs, contient la liste des comosants necessaires pour effectuer l'assemblage *)
let bdd_set = ref ((Array.make 0 ""),(Array.make 0 []),(Array.make 0 ""));;
(* **************** *)

(* entrée du parser programme *)
let prog_files = ref [] ;;
let prog_out = ref "a.gout" ;;
let chan_in = ref stdin ;;


(* entrée du parser db *)
let db_file = ref "GGC_DB_File.gdb" ;;

(* main de GGC *)

(* liste des arguments *)
let arg_list =
	let optimisation_arg   = ("-O",  Arg.Set_int(optimisation),"Niveau d'optimisation requit, algorithmes propre au compilateur")  
	and extends_arg        = ("-E",  Arg.Set(extends),"Utilisation de l'algorithme d'extention des pathways")
	and biol_var_arg       = ("-B",  Arg.Set(biol_var),"Utilisation des contraintes biologiques spécifiées")
	and cause_sub_list_arg = ("-nse",Arg.Clear(cause_sub_list),"No Side Effect : N'output que la liste des composants")
	and sub_prog_arg       = ("-sp", Arg.Set(sub_prog),"Output le programme avec les variables substituées")
	and simul_arg          = ("-S",  Arg.Set(simul),"Effectue et output une simulation du programme")
	and proof_arg 				 = ("-H",  Arg.Set(proof),"Effectue une preuve du programme initial avec l'utilitaire Herod (non installé)")
	and pretty_arg 				 = ("-pp", Arg.Set(pretty), "Effectue UNIQUEMENT un pretty-printing du programme initial")
	and db_file_arg 			 = ("-DB", Arg.Set_string(db_file),"Utilise une autre base de données que celle par defaut")
	and prog_out_arg			 = ("-o",  Arg.Set_string(prog_out),"Définie le nom du programme compilé (liste de composants)")
		in [optimisation_arg ;
				extends_arg ;
				biol_var_arg ;
				cause_sub_list_arg ;
				sub_prog_arg ;
				simul_arg ;
				proof_arg ;
				pretty_arg ;
				db_file_arg ;
				prog_out_arg]
;;

(* gestion des arguments anonymes *)
let blank_arg input = (prog_files := input::(!prog_files)) ;;
		
(* fonction de parsing des arguments *)
let args_parser = Arg.parse arg_list blank_arg "Gubs compiler" ;;

(* fonction de parsing générique *)
let parse chan parser lexxer_tk = 
		let lexbuf =Lexing.from_channel chan in
			parser lexxer_tk lexbuf
;;
(* fonction de parsing du fichier de base de donnée *)
let parse_bd bdd = 
	chan_in := open_in bdd;
	bdd_set:= (parse !chan_in Bdd_parser.parsing Bdd_lexxer.start);
	close_in !chan_in
;;

(* fonction de parsing des fichiers de programme en entrée *)
let rec parse_prog prog = 	match prog with
	  | []   -> if ((List.length !program) = 0) then raise (Exception.NoInputFile_e("why did you run me ? there is no file to parse !")) else ()
		| h::t -> begin
								chan_in := open_in h ;
								program := (parse !chan_in Gubs_parser.parsing Gubs_lexxer.start)@(!program);
								close_in !chan_in;
								parse_prog t
							end 
;;


(* nombre d'éléments minimum à selectionner pour s'assurrer la couverture du programme *)
let true_rate = (float_of_int (Array.length !db_names)) /. (float_of_int (List.length !program)) ;;

(* initialisation des refs par passage en argument *)
let main () = 
	args_parser;
	parse_prog !prog_files;
	parse_bd !db_file;
	begin if(!pretty) then Pretty_printer.printp !program (stdout); end (* pretty printing du programme *)
;;

(* lancement *)		
main () ;;