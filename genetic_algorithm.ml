(* ALGORITHME GENETIQUE:
   - les chromosomes sont des vecteurs de booléens,
   - la population est codée par une liste de chromosomes,
   - la fonction de fitness possède comme paramètre un chromosome et retourne une valeur réelle.
 Auteur: Franck Delaplace 2013 *)

(* ROUTINES *) 
(* somme d'une liste de réels *)
let sum l =  List.fold_left (fun s x  -> s +. x) 0. l ;;

(* tri décroissant d'une liste selon la fonction f *)
let sortbyfun f l  = List.fast_sort (fun e1 e2 -> compare (f e2) (f e1)) l;;   

(* séparation d'une liste l en 2 listes de n éléments et |l|-n éléments. *)
let rec separate  n l  =
  match (n,l) with
  |(0,l)  -> ([],l)
  |(_,[]) -> ([],[])
  |(n,a::r)-> let (bl,el) = separate (n-1) r in (a::bl,el) ;;

(* opérateur de composition itérée d'une fonction: nest f x n = f^n(x) *)
let rec nest f x n = 
  match n with 
  |0 -> x
  |n -> nest f (f x) (n-1) ;;

(* AIDE A LA CONCEPTION DE LA FONCTION DE FITNESS *)
(* somme des valeurs d'un tableau 'value'  correspondant aux valeurs vraies d'un chromosome *)
let guardedsum value guard = let sum = ref 0.0 in Array.iteri (fun i elt -> if elt then sum:= !sum +. value.(i)) guard; !sum ;;

(* GENERATION DE LA POPULATION: 
   paramètres: n: taille de la population; csize:taille du chromosome;  trate: probabilité d'engendrer true.
   Retourne une liste de chromosomes. *) 
let rec genpop n csize trate  = 
  match n with 
  | 0 -> [] 
  | n -> (Array.init csize (fun i -> (Random.float 1.0) < trate)):: genpop (n-1) csize trate ;; 

(* CROSS OVER *) 
(* effectue un crossover entre 2 chromosomes. 
   Retourne un couple de chromosomes où le cross-over a été appliqué. *) 
let  chrcrossover chr1 chr2  = 
  let len = Array.length chr1 in 
  let i = Random.int len in 
    (Array.append (Array.sub chr1 0 i) (Array.sub chr2 i (len-i)),
     Array.append (Array.sub chr2 0 i) (Array.sub chr1 i (len-i))) ;;

(* application du cross over sur une  population, retourne une  population de taille identique sur laquelle le cross-over a été appliqué.
  NOTE: La fonction alterne la selection d'un chromosome candidat au crossover (crossfind) et l'application du crossover avec un autre candidat (crossoperate) *)
let  crossover  pop  crossrate = 
  let rec crossoperate  pop  chr0  = (* recherche d'un chromosome pour un cross over avec chr0 *)
    match pop with 
    |[] -> [chr0]
    |chr::r when  (Random.float 1.0) < crossrate -> let (crosschr1,crosschr2) = (chrcrossover chr0 chr) in crosschr1::crosschr2::crossfind r (* crossover *)
    |chr::r -> chr::crossoperate  r  chr0 
  and crossfind pop =  (* recherche d'un candidat au cross over*) 
    match pop with 
    |[] -> []
    |chr::r when  (Random.float 1.0) < crossrate -> crossoperate r chr  (* chr  selectionné => opération de crossover *) 
    |chr::r -> chr::(crossfind  r) 
  in crossfind pop ;; 

(* MUTATION*) 
(* mute un chromosome.
   Pas d'élément retourné, modification du chromosome. *)
let chrmutate chr = 
  let len = Array.length chr  in 
  let i = Random.int  len     in 
  chr.(i)<- not chr.(i);;

(* mutation appliquée à la population.
  Retourne la  population mutée (attention: les chromosomes initiaux ont été modifiés) *)
let mutate pop mutaterate = List.map (fun chr -> if (Random.float 1.0) < mutaterate then chrmutate chr; chr) pop;;

(*SELECTION: Selectionne les  individus dans une  population de chromosomes viables selon la méthode de la roulette *) 
(* méthode de la roulette.  paramètres: pop:population; valpop: liste de valeurs associées à chaque chromosome;  valref: valeur de référence.
  Retourne le chromosome selectionné.  *) 
let rec biasedwheel  pop  valpop  valref  = 
  match (pop,valpop) with 
  |[chr],[_] -> chr (* 1 seul individu => selection *) 
  |chr::_,valchr::_  when  valref  <= valchr -> chr  (* trouvé! *)
  |chr::poprest,valchr::valpoprest -> biasedwheel poprest valpoprest (valref -. valchr ) 
  | _  -> failwith "[biasedwheel] empty list or the two lists have not the same size." ;; 

(* selection.  paramètres: pop: population, elitesize: taille de l'élite, fitness: fonction de fitness, p: prédicat testant  si un chromosome est viable,  selectrate: taux de selection (hors elite).
   Retourne une population selectionnée avec 'elitesize' meilleurs chromosomes préservés.
   NOTE: Si le nombre de chromosomes viable est inférieur au taux de selection  alors tous les chromosomes viables seront selectionnés. *) 
let select pop  elitesize  fitness p selectrate  = 	     
  let viablepop =  sortbyfun fitness  (List.filter p pop) in  (* population viable triée par ordre de fitness décroissant *)
  let (elitepop, popwork)= separate elitesize viablepop in  (* séparation de l'élite du reste de la population *)
  let fitpopwork = (List.map fitness popwork) in   (*calcul de la fitness pour la population restante *)
  let sumfitness = sum  fitpopwork in  (* fitness totale *)    
  if popwork  <> [] then  elitepop@
    (nest (fun l  -> (biasedwheel popwork fitpopwork (Random.float sumfitness))::l) [] (truncate ((float (List.length popwork)) *. selectrate))) (* selection des chromosomes *)
  else elitepop ;;

(* ALGORITHME GENETIQUE *)
(* les arguments qui ne sont pas optionnels sont la fitness (label), la taille du chromosome et la taille de la population 
 ex:  let gain = [[.. tableau de gains .. |]  in ga  (Array.length gain)  100  ~fitness:(guardedsum gain).
 retourne une liste de 'nbest'  meilleures solutions viables triées par ordre décroissant.
 NOTE: En  Ocaml, les arguments optionnels nécessitent un argument non labellé pour être instanciés par les valeur par défaut.
       De ce fait, la taille du chromosome et de la population ne sont pas des arguments labellés (dommage). *) 
let ga 
    ?(seed=[])                   (* population initiale *)
    ?(gennumber=100)             (* nombre de générations *)
    ?(mutaterate=0.01)           (* taux de mutations *)
    ?(crossrate=0.80)            (* taux de cross over *)
    ?(elitesize=1)               (* taille de la population de l'élite *)
    ?(truerate=0.5)              (* taux d'éléments à vrai dans un chromosome *)
    ?(selectrate=1.0)            (* taux de selection - diminue la taille de la population si < 1.0 *) 
    ?(isviable =(fun _ -> true)) (* fonction déterminant les conditions de viabilité d'un chromosome *)
    ?(nbest = 1)                 (* nombre de meilleures solutions retournées. *)  
    ~fitness                     (* fonction de fitness *) 
    chrsize                      (* taille du chomosome *)
    popsize                      (* taille de la population supplémentaire à la population initiale *) 
    =  
  let pop0 = seed@(genpop popsize chrsize truerate) in 
  let popfinal = (nest (fun pop -> 
      let popsel = select pop elitesize fitness isviable selectrate in
      let crosspopsel = crossover popsel crossrate in 
      mutate crosspopsel mutaterate) pop0 gennumber) in
  let (bestsols,_) = separate nbest (sortbyfun fitness (List.filter isviable popfinal)) in
  bestsols ;; 