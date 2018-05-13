
(*DEFINITION DU TYPE*)

type aut = {alphabet: char list; etats: int list; initiaux: int list; finals: int list;

  transitions: (int * char * int) list};;









(** EXEMPLES D'AUTOMATES **)

(*reconnait les "fausses" écritures binaires des multiples de 3*)

let tri_bin = {alphabet = [`0`; `1`];
    etats = [0; 1; 2];
    initiaux = [0];
    finals = [0];
    transitions = [(0, `0`, 0); (0, `1`, 1); (1, `1`, 0); (1, `0`, 2); (2, `1`, 2); (2, `0`, 1)]};;

(*reconnait les vraies écritures binaires*)
let vrai_bin = {alphabet = [`0`; `1`];
    etats = [0; 1; 2];
    initiaux = [0];
    finals = [1; 2];
    transitions = [(0, `0`, 1); (0, `1`, 2); (2, `1`, 2); (2, `0`, 2)]};;
(*reconnait les vraies écritures binaires des multiples de 3*)

let vrai_tri_bin = {alphabet = [`0`; `1`];
    etats = [0; 1; 2; 3; 4; 5];
    initiaux = [0];
    finals = [1; 4];
    transitions = [(0, `0`, 1); (0, `1`, 2); (1, `0`, 5); (1, `1`, 5);
                   (2, `0`, 3); (2, `1`, 4); (3, `0`, 2); (3, `1`, 3);
                   (4, `0`, 4); (4, `1`, 2); (5, `0`, 5); (5, `1`, 5);]};;

let M1 =
  {alphabet = [`0`; `1`];
   etats = [3; 1; 2];
   initiaux = [1];   finals = [3];
   transitions = [(3, `0`, 1); (2, `1`, 1); (2, `1`, 3); (1, `0`, 2); (2, `1`, 2); (2, `0`, 1)]};;

let M2 =
  {alphabet = [`2`; `1`;`0`];
   etats = [1; 3; 0];
   initiaux = [3];   finals = [0];
   transitions = [(0, `2`, 3); (1, `1`, 1); (1, `1`, 0); (1, `2`, 0); (3, `1`, 1); (3, `2`, 1); (1, `0`, 1); (3, `0`, 1) ]};;

let M3 =
  {alphabet = [`a`; `1`];
   etats = [3; 1; 2];
   initiaux = [1];   finals = [3];
   transitions = [(3, `a`, 2); (2, `1`, 1); (1, `1`, 3); (1, `a`, 2); (2, `1`, 1); (2, `a`, 1)]};;

let M267 =
  {alphabet = [`a`; `b`];
   etats = [0; 1; 2; 3; 4];
   initiaux = [0];   finals = [0];
   transitions = [(0, `a`, 1); (0, `a`, 4); (1, `b`, 0); (1, `b`, 2);
                  (2, `a`, 1); (2, `a`, 3); (3, `a`, 2); (3, `a`, 4);
                  (4, `b`, 0); (4, `b`, 3)]};;

let M268a =
  {alphabet = [`a`; `b`];
   etats = [0; 1; 2; 3];
   initiaux = [0];   finals = [3];
   transitions = [(0, `a`, 0); (0, `b`, 0); (0, `a`, 1); (1, `b`, 2); (2, `a`, 3)]};;

let M268b =
  {alphabet = [`a`; `b`];
   etats = [0; 1; 2; 3];
   initiaux = [0]; finals = [3];
   transitions = [(0, `a`, 1); (0, `b`, 0); (1, `a`, 1); (1, `b`, 2); (2, `a`, 3); (2, `b`, 0); (3, `a`, 1); (3, `b`, 2)]};;

let M254 =
  {alphabet = [`0`; `1`];
   etats = [0; 1];
   initiaux = [0];   finals = [1];
   transitions = [(0, `1`, 1); (1, `0`, 1); (1, `1`, 1)]};;

let Mvide =
  {alphabet = [`a`; `b`];
   etats = [0; 1; 2];
   initiaux = [0];   finals = [2];
   transitions = [(0, `a`, 0); (0, `b`, 0); (0, `a`, 1); (1, `b`, 0);
                  (1, `a`, 1); (2, `a`, 2); (2, `b`, 2); (2, `b`, 0)]};;











(********************************** OPERATIONS SUR LES LISTES **********************************) 




(* Renvoie le maximum d'une liste d'entiers. Si la liste est vide, on renvoie 0 *)
(* COMPLEXITE : linéaire en la longueur de la liste *) 


let max_liste l = 
let rec maxl = fun 
 [] a -> a 
|(t::q) a -> maxl q (max t a) in
if l=[] then 0 else maxl l(hd(l)) ;;  



(* Renvoie le produit cartésien de deux listes   *)
 (* COMPLEXITE : en n*m, longueur des deux listes *)


let prod_cart A B =
let rec produit = fun 
 [] _ -> [] 
| (_::q) [] -> produit q B
| (t::q) (a::b) -> (t,a)::(produit (t::q) b ) in 
produit A B  ;;




(* Teste si un élément appartient déjà à une liste donnée *)
(* COMPLEXITE : linéaire *)


let rec appartient a= function
 [] -> false 
|(t::q) -> t=a || appartient a q ;;








(* Donne le complementaire d'une liste par rapport à une grosse liste qui la contient *)
(* COMPLEXITE : n*m, longueur des deux listes *)


let rec complement = fun 

  [] f -> []
| (a::b) f when appartient a f  -> complement b f 
| (a::b) f -> a::(complement b f) ;;



(* Effectue la réunion de deux listes *)
(* COMPLEXITE : n*m, longueur des deux listes *)


let rec reunion = fun 

 [] l -> l
|(t::q) l when appartient t l -> reunion q l 
|(t::q) l -> t::(reunion q l) ;;



(* Intersection de deux listes *)
(* COMPLEXITE : n*m, longueur des deux listes *)


let rec inter = fun 
  [] l -> [] 
| (t::q) l when appartient t l -> t::(inter q l)
| (t::q) l -> inter q l ;;




(* Enlève les éléments répétés d'une liste *) 
(* COMPLEXITE : n*(n-1)/2 *)


let rec liste_purge = fun 
  [] ->[]
| (t::q) when appartient t q -> liste_purge q
| (t::q) -> t::(liste_purge q) ;;





(* Transforme une liste en liste de liste *)
(* COMPLEXITE : Linéaire *)


let rec listlist_of_list = fun
 [] -> [] 
|(t::q) -> [t]::(listlist_of_list q) ;;




(* Génère une liste de longueur p avec les entiers naturels en ordre croissant *) 
(* COMPLEXITE : Linéaire *)

let génère_liste p = 
let rec aux = function 
   0 -> [0]
  |n->n::(aux(n-1)) in 
rev (aux p) ;; 





(* Fonction qui dit si deux listes sont égales en contenu à l'ordre près *) 
 (* COMPLEXITE : Linéaire *)


let liste_égales l1 l2 = 
if (list_length l1) <> (list_length l2) then false else
let rec teste = fun 
[] -> [] 
| (t::q)  -> (appartient t l2)::(teste q) in
let VRAI = teste l1 in 
if appartient false VRAI then false else true ;;  



(* Réécriture de map. La fonction map est présente de base, mais pour être sur de sa complexité, réécrivons la. *) 
(* COMPLEXITE : (cout de la fonction)*n. La plupart du temps linéaire *)

let rec map f = function 
 [] -> [] 
|(t::q) -> (f t)::(map f q) ;;  



(* Cette fonction va remplacer le @ qui peut dégénérer en quadratique. C'est un gain en complexité appréciable *) 
(* Remarque : la fonction rev (miroir) se fait en temps constant en fait, et il est inutile de la réecrire ! *) 
(* Complexité : Linéaire *) 



let concat_liste l1 l2 = 
let rec aux = fun 
 [] l -> l 
|(t::q) l -> aux q (t::l) in  
aux (rev l1) l2 ;;



(* TRI DE LISTES *) 


(* Tri fusion. Complexité n*ln(n).  *)
(* Le tri par insertions peut dégénérer en n², tout sera déjà bien assez complexe, essayons de gagner de la complexité où on peut *) 
(* Distribue la liste l en deux tas comme aux cartes *)

let separe l = 
let rec aux = fun 
 [] l0 l1 _ -> (l0,l1)
|(t::q) l0 l1 0 -> aux q (t::l0) l1 1 
|(t::q) l0 l1 1 -> aux q l0 (t::l1) 0  in 
aux l [] [] 0 ;; 

(* Fusionne deux listes préalablement triées *) 

let fusion l1 l2 = 
let rec aux L = function 
  l when L=[] -> l
 |[] -> L
 |(t::q) when hd(L)<t ->  hd(L)::(aux (tl(L)) (t::q))
 |(t::q) ->  t::(aux L q)  in 
aux l1 l2 ;;




let rec tri_fus = function 
  l when list_length l <2 -> l 
 |l -> let (l1,l2)= separe l in fusion (tri_fus(l1)) (tri_fus (l2)) ;; 




(********************************** RENOMMER **********************************)

(* Lors de l'algorithme de déterminisation par exemple, on sera amené à considérer un état comme une liste d'états. 
Il va falloir alors renommer *) 



(* Prend une liste L, la réindice en prenant pour premier terme i *)
(* Et renvoie l'image de x dans la nouvelle liste *)



 (* Si L=[2;3;5;1] et i=1, (renomme L x 1) transforme L en [1;2;3;4] : (renomme L 5 1) renvoie 3 *)
(* Complexité : Longueur de L *) 

let renomme L x i = 
let compteur = ref i and l = ref L in
 while hd(!l)<>x do 
l:=tl(!l); compteur:=(!compteur)+1 done; !compteur ;;


(* Permet de renommer un automate *) 
(* Si n est le nombre d'états : 
COMPLEXITE : 5*n *) 

let renomme_aut M i =
 {alphabet= M.alphabet;
  etats=map (fun x-> renomme (M.etats) x i) M.etats;
  initiaux=map (fun x-> renomme (M.etats) x i) M.initiaux;
  finals=map (fun x-> renomme (M.etats) x i) M.finals;
  transitions=map (fun (p,a,q)->((renomme (M.etats) p i),a,(renomme (M.etats) q i))) M.transitions};;






(************************** Automates à transitions instantanées : Worshall (DM7) ********************************)





(*DEFINITION DU TYPE*)

type go = {sommets: int list;
 fleches: (int * int) list};;

(* Exemple *) 

let G1 = {sommets = [0; 1; 2; 3; 4; 5];
         fleches = [(0, 1); (0, 2); (1, 5); (1, 5);
                   (2,3); (2,  4); (3, 2); (3, 3);
                   (4,  4); (4, 2); (5,  5); (5, 5);]};;

(* Extrait les transitions instantanées d'un automate *)  


let rec extrait_trans = function 
   [] -> []
  |(a,`_`,b)::q -> (a,b)::(extrait_trans q) 
  |(_::q)  -> extrait_trans q ;;


let rec transA_of_transG = function 
   [] -> [] 
  |((a,b)::q) -> (a,`_`,b)::(transA_of_transG q) ;;


let go_of_aut M = 
{sommets = M.etats ; fleches = (extrait_trans (M.transitions))};;

let aut_of_go G = {alphabet= []; etats= G.sommets ; initiaux= []; finals= [];

  transitions= transA_of_transG G.fleches};;

let copy_matrix N = 
if N = [||] then [||] else
let M = make_matrix (vect_length N) (vect_length N.(0)) (N.(0).(0)) in
for i=0 to (vect_length N-1) do 
  for j=0 to (vect_length N.(0) -1) do 
    M.(i).(j)<-N.(i).(j) 
  done; 
done; 
M;;  

(* Attention, tous mes automates auxquels j'applique l'algorithme de Roy Worshall doivent être canoniques. la liste d'état commence à 0 *) 

(* Algorithme de Roy Worshall
Je choisis de manipuler une matrice de booléens. *) 

let Worshall M = 
let n= list_length(M.etats) in let G=go_of_aut (renomme_aut M 0) and N = make_matrix n n false in 
for i=0 to n-1 do  (* Ces deux boucles initialisent nos matrices *) 
  for j=0 to n-1 do       
    N.(i).(j)<-  (i=j) || (appartient (i,j) G.fleches)   (* On initialise avec les transitions instantanées pur sucre, et si i=j *) 
  done;
done; 
(* Maintenant on applique l'algorithme du cours 2-10-5*)
for k=0 to (n-1) do 
  let Nk1=copy_matrix N in(*copie mat copie la matrice N, sans tomber dans le piège de l'adressage ! *) 
  for i=0 to n-1 do 
    for j=0 to n-1 do 
      N.(i).(j)<- Nk1.(i).(j) || (Nk1.(i).(k) && Nk1.(k).(j)) 
    done; 
  done; 
done; 
N;;

 
(* Donne la cloture instantanée d'un état. Je renomme à partir de 0 pour avoir ma matrice qui commence bien à 0 *) 

let clot1 p M = 
let pp = renomme (M.etats) p 0 in
let N=Worshall (renomme_aut M 0) in 
let l= ref [] in 
for i=0 to (vect_length N.(pp) -1) do 
  if N.(pp).(i) then l:=i::(!l) done;
rev(!l) ;; 


 



(* Donne la cloture instantanée d'une liste d'états *) 


let cloture l M = 
let rec aux = function 
   [] -> [] 
  |(t::q) -> concat_liste (clot1 t M) (aux q) in 
tri_fus (liste_purge (aux l)) ;; 










(********************************** MOTS RECONNUS **********************************)


(* Pour une liste d'états et en prononçant une lettre, donne la liste d'états accessibles, si la liste est vide, on renvoie le vide*)


let lecture b M E = 
let T = M.transitions in
let rec lect a = fun
 [] E -> []   (* a est la lettre, E la liste d'états, ((t,q,r)::s) la fonction de transition *)
|((t,q,r)::s) E when q=a && (appartient t E) -> concat_liste (clot1 r M) (lect a s E)
|((t,q,r)::s)  E-> (lect a s E)    in
liste_purge( lect b T E ) ;;





(* Teste si un élément de la liste A est dans la liste B (intersection non nulle) *) 

let rec appartient_liste = fun
 [] B -> false 
| (t::q) B -> (appartient t B) || (appartient_liste q B) ;; 



(* Teste si un mot appartient au langage reconnu par un automate *)


let reco l M = 
let etats = ref (M.initiaux)  in 
for i=0 to (string_length l -1) do
etats:= (lecture (l.[i]) M (!etats)) done; 
appartient_liste (M.finals) (!etats) ;;








(********************************** TRANSPOSITION **********************************)

(* On inverse états initiaux et finals. On inverse aussi les transitions avec un coup de map. *) 
(* COMPLEXITE : Linéaire *) 

let transpose M = 
{ alphabet = M.alphabet ; etats = M.etats ; initiaux = M.finals ; 
finals = M.initiaux ; transitions = map (fun (p,a,q)->(q,a,p)) (M.transitions) };; 
















(********************************** LA REUNION DE DEUX AUTOMATES **********************************)



(* L'algorithme de réunion est trivial, il faut juste bien renommer tous les états *) 
(* COMPLEXITE : O(4m² + 4n²)  *) 


let reunion_aut M N =
 let m=(list_length M.etats) and n=(list_length N.etats) in
 {alphabet=(reunion (M.alphabet) (N.alphabet));
  etats= (génère_liste (n+m));                            
  initiaux= concat_liste (map (fun x->(renomme (M.etats) x 0)) (M.initiaux)) (map (fun x->(renomme (N.etats) x m)) (N.initiaux)) ;
  finals= concat_liste (map (fun x->(renomme (M.etats) x 0)) (M.finals)) (map (fun x->(renomme (N.etats) x m)) (N.finals)) ;
  transitions= concat_liste (map (fun (p,a,q) -> ((renomme (M.etats) p 0),a,(renomme (M.etats) q 0))) (M.transitions))
                            (map (fun (p,a,q) -> ((renomme (N.etats) p m),a,(renomme (N.etats) q m))) (N.transitions))} ;;






(********************************** INTERSECTION **********************************)







(* Cette fonction fabrique la nouvelle fonction de transition. Si deux transitions portent sur la prononciation d'une même lettre,
on obtient par produit cartésien une transition de l'intersection. *)
(* Complexité en |t1|*|t2| *) 




let transition_intersection t1 t2 L =
let rec trans_inter = fun (* En argument : t1 et t2 *)
  [] l -> []
| (t::q) [] -> trans_inter q t2 
| ((a,b,c)::q) ((d,e,f)::s) when b=e -> ((renomme L (a,d) 0),b,(renomme L (c,f) 0))::(trans_inter ((a,b,c)::q) s)
| l (r::s)  -> (trans_inter l s) in 
trans_inter t1 t2;;
 


(* La complexité dépend du nombre de transitions ET du nombre d'états au carré. Comme les transitions sont en général plus nombreuses, on va baser le calcul de complexité la dessus. 
si tm et tn sont le nombre de transitions, on a une complexité de O(tm*tn) *)

let intersection M N =
 let m=(list_length M.etats) and n=(list_length N.etats) and PROD_CART_MN_ETATS= prod_cart M.etats N.etats in (* Mise en facteur *)
 {alphabet= inter M.alphabet N.alphabet;
  etats= (génère_liste (list_length (PROD_CART_MN_ETATS)));
  initiaux= map (fun x->(renomme (PROD_CART_MN_ETATS) x 0)) (prod_cart M.initiaux N.initiaux);  (* On doit renommer tous les produits cartésiens *)
  finals= map (fun x->(renomme (PROD_CART_MN_ETATS) x 0)) (prod_cart M.finals N.finals);
  transitions= (transition_intersection M.transitions N.transitions (PROD_CART_MN_ETATS))};;












(********************************** COMPLETION **********************************)





(* Recherche les étiquettes (lettres) manquantes à partir d'un état *)
(* Complexité : A*T. A est le nombre de lettres, souvent petit. T le nombre de transitions *) 


let etiquettes_manquantes i A T= 

let rec et_présent = fun 
 j [] l -> []
|j (t::q) [] -> et_présent j q T 
|j (t::q) ((a,b,c)::p) when a=j && b=t -> b::( et_présent j (t::q) p) 
|j l (a::p) -> et_présent j l p in

complement A (et_présent i A T) ;;  




(* Fabrique les transitions manquantes vers la poubelle à partir des étiquettes manquantes *)
(* Complexité : Nombre de lettres *) 


let rec make_trans = fun
  i [] n -> []  (* i est l'état concerné, (t::q) est l'alphabet des lettres manquantes, n est l'état poubelle *)
| i (t::q) n -> (i,t,n)::(make_trans i q n) ;; 


(* Cette fonction permet de fermer la poubelle dans le cas ou p est l'état poubelle*)
(* Complexité : nombre de lettres *) 



let rec  fermeture_poubelle = fun
  p [] -> [] 
| p (t::q) -> (p,t,p)::(fermeture_poubelle p q) ;; 





(* Complexité : O (Nombre d'états * Nombre de lettres * nombre de transitions). Le reste est négligeable. *) 


let complete N = 
let M = (renomme_aut N 0) in 
let nb_etats = (list_length (M.etats)) in
let trans = ref (M.transitions) and  i = ref 0 in 
while !i <> (nb_etats) do 
let manque = etiquettes_manquantes !i (M.alphabet) (M.transitions) in
trans := concat_liste (make_trans !i (manque) (nb_etats)) (!trans) ; i:=!i+1 done; 

if M.transitions = !trans then M else 

{alphabet = M.alphabet ; etats = (rev(nb_etats::rev(M.etats))) ; initiaux = M.initiaux ; finals = M.finals ; transitions = concat_liste (!trans) (fermeture_poubelle (nb_etats) M.alphabet) } ;;





(********************************** DETERMINISATION ACCESSIBLE **********************************)















(* Fonction qui fait la liste des listes d'états accessibles à partir d'un seul état *) 
(* Complexité : A*E. Dans les exemples, ce sera 2*E *)


let etats_deterministes A E M =  (* A est l'alphabet ; E est la liste de liste d'états de départ : T la transition de l'automate *)
let rec lister = fun
   [] l -> [] 
  |(t::q) [] -> lister q E
  |(t::q) (a::b) -> (cloture (lecture t M a) M)::(lister (t::q) b) in

liste_purge(lister A E) ;;




(* Fonction qui fait la liste de toutes les listes d'états accessibles selon l'algorthme vu en cours *) 
(* Complexité : Assez difficile a évaluer du fait qu'on ne sait pas quand la boucle While s'arrete. 
Elle peut faire au maximum card(Alphabet)^(nombre d'états) tours. *) 


let tous_etats_deterministes A I M = (* A est l'alphabet ; I est l'état de départ (initial) : T la transition de l'automate *)
let II= [cloture I M] in   
let res=ref II in 
while not(liste_égales (liste_purge((concat_liste (etats_deterministes A (!res) M) (!res) ))) !res)  do 
res:= liste_purge(concat_liste (liste_purge (!res)) ((etats_deterministes A (!res) M)))  done; !res ;; 








(* Renvoie les fonctions de transitions de l'automate déterminisé. *) 
(* Complexité : A*E (par les appels récursifs)  * E² par les renomme. 
A*E^3 si on néglige les tri_fus *) 



let trans_det A M E =  (* A est l'alphabet ; T les transitions ; E la liste de liste d'états *)
let rec transi_det = fun (* Prend en argument l'alphabet et la liste de liste d'états *)
   [] l -> []
  |(a::b) [] -> transi_det b E 
  |(a::b) (e::f) -> ((renomme E e 0) ,a,(renomme E (cloture (lecture a M e) M)0))::(transi_det (a::b) f) in
transi_det A E ;;



(* Renvoie la liste d'états finals non renommés de l'automate deterministe *) 

let finals_det E f M = (* E est la liste de liste d'états accessibles ; f la liste d'états finals de l'automate que l'on souhaite déterminiser *)
let rec final = function
   [] -> [] 
  |(t::q) when (inter (cloture t M) f) = [] -> final q 
  |(t::q) -> t::(final q) in
final E ;;




(* Renvoie l'automate déterminisé et complet  *)



let da N =
let M = renomme_aut N 0 in 
let TOUS_ETATS_DETERMINISTE = (tous_etats_deterministes M.alphabet M.initiaux M) in (* mise en facteur *)
 {alphabet=M.alphabet ; 
etats= (génère_liste (list_length TOUS_ETATS_DETERMINISTE)) ;
initiaux= map (fun x->(renomme (TOUS_ETATS_DETERMINISTE) x 0)) [cloture M.initiaux M] ;
finals= map (fun x->(renomme (TOUS_ETATS_DETERMINISTE) x 0)) (finals_det TOUS_ETATS_DETERMINISTE M.finals M) ;
transitions= (trans_det M.alphabet M TOUS_ETATS_DETERMINISTE) } ;;







(********************************** COMPLEMENTARISATION **********************************)


let complementaire M =
let N=da M in  
{ alphabet = N.alphabet ; etats = N.etats ; initiaux = N.initiaux ; 
finals = (complement (N.etats) (N.finals)); transitions = N.transitions };; 





(********************************** DIFFERENCE SYMETRIQUE **********************************) 



let difference_symetrique M N =
 da(intersection (da(reunion_aut M N)) (complementaire  (da(intersection M N))));;









(********************************** POUR LES AUTOMATES QUI RECONNAISSENT LE VIDE **********************************)




(* Fonction qui détermine les états accessibles à partir d'une liste d'états *)


let rec lecture_états = fun 

 [] E -> []
| ((t,q,r)::s) E when appartient t E -> r::(lecture_états s E)
| ((t,q,r)::s) E -> lecture_états s E ;; 

  

(* Fonction qui reconnait les automates ne reconnaissant que le vide *)

let vide N =
let M = da(N) in  

let atteints = ref M.initiaux and encours= ref M.initiaux and res = ref true in

while !res && !encours<>[] do 
let L = lecture_états (M.transitions) (!encours) in 
if liste_égales (!atteints) (liste_purge ( concat_liste (!atteints) L)) then encours:=[] else (* on s'arrête si on n'atteint pas plus d'états *)
   if (inter (concat_liste (!atteints) L) M.finals) <> [] then res:=false else  (* on s'arrête si un état atteint est final, l'automate reconnait alors un mot au moins *)
        begin encours:=L ; atteints:=(liste_purge( concat_liste (!atteints) L)) end done; 
 
!res ;;





(* De même avec da *)



let vide2 N = let M = da N in M.finals=[] ;;





(********************************** FONCTION QUI TESTE SI DEUX AUTOMATES RECONNAISSENT LE MÊME LANGAGE **********************************) 




let equiv M N = if inter M.alphabet N.alphabet = [] then false else (* on traite le cas des alphabets disjoints à part, sinon cela renvoie true *)
if (vide M) = (vide N) = true then true (* A part aussi, le cas ou les deux automates reconnaissent le vide *)
else  vide (difference_symetrique M N) ;;





(* Nous ignorerons le cas ou l'utilisateur décide de tester l'equivalent de deux automates 
dont les deux alphabets se chevauchent. Il se passe des choses étranges et renvoie true la
plupart des cas. Comme c'est un cas vraiment exotique, il sera ignoré *)







(**************************  EMONDAGE  ***************************)









(* Fonction qui détermine les états accessibles à partir d'une liste d'états 
Donc si on l'applique aux états initiaux, alors on a les états accessibles *)


let rec lecture_étatsI = fun (* Premier argument : la liste de transitions. Le deuxieme est la liste d'états considérée *)
 [] E -> []
|((t,q,r)::s) E when appartient t E -> r::(lecture_étatsI s E)
|((t,q,r)::s) E -> lecture_étatsI s E ;; 



let etats_accessibles T I = 

let res = ref (tri_fus (liste_purge I)) in 

while (!res)<> tri_fus(liste_purge(concat_liste (lecture_étatsI T !res) (!res))) do  
res := tri_fus(liste_purge(concat_liste (!res) (lecture_étatsI T !res))) done ; !res ;; 




(* Fonction qui détermine les états co-accessibles à partir d'une liste d'états 
Donc si on l'applique aux états finaux, alors on a les états accessibles *)



let rec lecture_étatsF = fun 
 [] E -> []          (* Premier argument : la liste de transitions. Le deuxieme est la liste d'états considérée *)
|((t,q,r)::s) E when appartient r E -> t::(lecture_étatsF s E)
|((t,q,r)::s) E -> lecture_étatsF s E ;; 



let etats_coaccessibles T F = 

let res = ref (tri_fus (liste_purge F)) in 

while (!res)<>  tri_fus(liste_purge(concat_liste (lecture_étatsF T !res) (!res))) do 
res := tri_fus(liste_purge( concat_liste (!res) (lecture_étatsF T !res))) done ; !res ;; 



(* Fonction qui donne la liste des états utiles *)

let utiles T I F = inter (etats_accessibles T I) (etats_coaccessibles T F) ;;



(* Fonction qui enlève les transitions désormais inutiles *)


let rec emonde_transition = fun  (* Prend en argument la liste de transitions et la liste des états utiles *)
 [] Q -> [] 
|((t,s,r)::q) Q when (appartient t Q) && (appartient r Q) -> (t,s,r)::(emonde_transition q Q) 
|(a::q) Q -> emonde_transition q Q ;; 


let emonde M = 
let utile = utiles (M.transitions) (M.initiaux) (M.finals)  in
{ alphabet = M.alphabet ; etats = utile ; initiaux = inter M.initiaux utile ; finals = inter M.finals utile ; 
transitions = emonde_transition M.transitions utile } ;;



(* Donne l'automate canonique déterminsite émondé *) 

let de M = renomme_aut (emonde (da(M))) 0;;









(********************************************** Minimalisation ****************************************)






(* Programme qui renvoie l'automate canonique minimal *) 

let mini M = renomme_aut (de(transpose(de(transpose M)))) 0 ;;


(* Le point 2.6.7 du cours prouve que l'automate suivant M267 est minimal : minimal M267 renvoie bien M267. *)


let M0 =
  {alphabet = [`a`; `b`];
   etats = [0;1;2];
   initiaux = [0];   finals = [2];
   transitions = [(0, `b`, 0); (0, `a`, 1); (1, `a`, 1); (1, `b`, 2);
                  (2, `a`, 2); (2, `b`, 2)]};;


(* Le point 2.6.8 du cours indique que le langage L=(a+b)*aba a 4 résiduels : M268 reconnait ce langage et minimal M268 a bien 4 états *)

let M268 =
  {alphabet = [`a`; `b`];
   etats = [0; 1; 2; 3];
   initiaux = [0];   finals = [3];
   transitions = [(0, `a`, 0); (0, `b`, 0); (0, `a`, 1); (1, `b`, 2); (2, `a`, 3)]};;







(******************************** Création d'un automate à partir d'un langage ***************************)




(* Fonction qui renvoie la liste de transitions instantanées nécéssaire à la concaténation d'automates *) 



 let relier_inst F I = 
 (* prend en argument les états finals de de l'automate 1 et les états initiaux d'un automate 2 *)
let rec relie = fun 
 l [] -> []
|[] (t::q) -> relie F q
|(a::b) (t::q) -> (a,`_`,t)::(relie b (t::q)) in 
relie F I ;;



let concat_aut m n = 
let mm = list_length m.etats and nn= list_length n.etats in
let M = renomme_aut m 0 and N = renomme_aut n (mm) in  
{alphabet= reunion M.alphabet N.alphabet ; etats = génère_liste (mm+nn)  ; initiaux = M.initiaux ; finals = N.finals ; 
transitions = concat_liste (concat_liste (M.transitions) (N.transitions)) (relier_inst M.finals N.initiaux) } ;;




(* Fonction qui crée les transitions instantanées nécessaires à la "plus-isation " d'un automate *) 

let plus F I = (* Prend en argument les états initiaux et finals *) 
let rec trans = fun 
  _ [] -> []
|[] (_::b) -> trans F b 
|(a::b) (t::q) -> (a,`_`,t)::(trans b (t::q)) in 
trans F I ;; 


let plus_aut M = 
{alphabet= M.alphabet ; etats = M.etats ; initiaux =M.initiaux ; finals = M.finals ; 
transitions = concat_liste (M.transitions) (plus M.finals M.initiaux) } ;;



(* Pour faire l'étoile, on fait le plus et on rajoute Epsilon, en rajoutant un état initial et final relié à rien du tout  *) 

let etoile_aut M = 
let A = ( (max_liste M.etats)+1) in (* On prend un état non utilisé *) 
{alphabet= M.alphabet ; etats = A::M.etats ; initiaux =A::M.initiaux ; finals = A::M.finals ; 
transitions = concat_liste (M.transitions) (plus M.finals M.initiaux) } ;;


(* Le parcours d'arbre est évidemment mieux adapté au traitement de données. Voici le type exprat qui dénote les expressions. *) 


type exprat = 
 Vide 
|Epsilon
|Lettre of char 
|Somme of exprat*exprat
|Produit of exprat*exprat
|Etoile of exprat
|Plus of exprat ;;





(******************* String -> Exprat **********************)
(* Pour ne pas se casser la tête a taper une expression rationnelle, voici un programme magique qui transforme une expression rationnelle écrite en 
string (de manière raisonnable, (ie) sans erreurs de syntaxe) en exprat. *)


(* Le vide sera dénoté par #, le "plus" par °, l'étoile par *, epsilon par _, et la réunion par + *) 

let est_lettre a = not (appartient a [`)` ; `*` ; `+` ;  `_` ; `(` ; `#`;`°`]) ;;


let insere2pts m = 
let n=string_length m in 
if n=0 then "" else 
let rec aux = fun 
  t i _ when i=(n-1) -> t
 |t i j -> if est_lettre m.[i] then 
              if est_lettre m.[i+1] then aux ( (sub_string t 0 j)^(":")^(sub_string t j (string_length t -j))) (i+1) (j+2) else
              match m.[i+1] with 
                 `(` | `_` | `#` -> aux ( (sub_string t 0 j)^(":")^(sub_string t j (string_length t -j) )) (i+1) (j+2) 
                | _  -> aux t (i+1) (j+1) 
           else 
               if appartient (m.[i]) [`)` ;`*`; `_` ; `#`;`°`] then 
                   if est_lettre m.[i+1] then aux  ((sub_string t 0 j)^(":")^(sub_string t j (string_length t -j) )) (i+1) (j+2) else
                      match m.[i+1] with 
                        `(` | `#` | `_` ->  aux ( (sub_string t 0 j)^(":")^(sub_string t j (string_length t-j)) ) (i+1) (j+2) 
                       | _ -> aux t (i+1) (j+1)  
               else aux t (i+1) (j+1) in 
aux m 0 1 ;;






(* Il reste le problème du produit, donc de la concaténation. IL faut impérativement que la concaténation soit faite avec un : sinon le programme plante avec SyntaxError 
C'est le boulot du programme insere2pts, qui va insérer ":" la ou il faut. *)





exception SyntaxError;;


let exprat_of_string t=
let s= insere2pts t in 
let rec expression i j=
  match j-i with
  -1->raise SyntaxError
 |0->(match s.[i] with
      `(` | `)` | `:` | `+` | `*`| `°` -> raise SyntaxError  (* le : ca va être le produit, la concatènation *) 
     |`_`-> Epsilon
     |`#`-> Vide
     |c->Lettre c)
 |_->
  let pos=ref (-1) and numpar=ref 0 and alter=ref false and k=ref (i-1) in
  while !k<j & not !alter do
    incr k;
    match s.[!k] with
     `(`->incr numpar
    |`)`->decr numpar
    |`+`->if !numpar=0 then (alter:=true;pos:=!k)
    |`:`->if !numpar=0 then pos:=!k
    |_->()
  done;
  if !pos>=0 then (
  let e1,e2=expression i (!pos-1),expression (!pos+1) j in
  if !alter then Somme (e1,e2)
            else Produit (e1,e2)
  ) else
  if s.[j]=`*` then Etoile (expression i (j-1))
               else 
  if s.[j]=`°` then Plus (expression i (j-1))
               else
  if s.[i]=`(` & s.[j]=`)` then expression (i+1) (j-1)
                           else raise SyntaxError

in 
if s="" then Vide else
expression 0 (string_length s - 1);;





(* Sa réciproque. Bien plus simple. Les cas sont la pour gérer le surplus de parenthèses. Il peut en subsister quelques unes superflues.  *) 

let arg = function 
  Plus exp -> exp 
 |Etoile exp -> exp 
 |_ -> raise SyntaxError ;;




let rec string_of_exprat = function

 Vide -> "#"
|Epsilon -> "_" 
|Lettre a -> string_of_char(a)
|Somme (exp1,exp2) ->"("^(string_of_exprat exp1)^"+"^(string_of_exprat exp2)^")" (* Une somme de somme apporte des parenthèses a+b+_ -> (a+(b+_)) par exemple... *) 
|Produit (exp1,exp2) -> (string_of_exprat exp1)^(string_of_exprat exp2) 
|A ->  match arg(A) with 
                Lettre a -> if A= Etoile(arg(A)) then (string_of_char(a))^"*" else (string_of_char(a))^"°"
               |Epsilon -> "_" 
               |Vide -> "#"
               |Somme _ -> if A= Etoile(arg(A)) then (string_of_exprat (arg(A)))^"*" else (string_of_exprat (arg(A)))^"°"
               |_ ->  if A= Etoile(arg(A)) then "("^(string_of_exprat (arg(A)))^(")*") else "("^(string_of_exprat (arg(A)))^(")°")  ;;









(* Fonction qui extrait les caractères présents dans un type exprat : l'alphabet*) 

let alphabet f = 
let rec alp = function
  Vide -> []
| Epsilon -> [] 
| Lettre e -> [e]
| Produit (exp1,exp2) -> concat_liste (alp(exp1)) (alp(exp2))
| Somme (exp1,exp2) -> concat_liste (alp(exp1)) (alp(exp2))
| Etoile exp -> alp exp 
| Plus exp -> alp exp in
liste_purge (alp f) ;;  



(* Pour un exprat donne l'automate reconnaissant le langage que dénote exprat *) 


let automate_reconnaissant f = 
let alpha = alphabet f in 
let rec auto = function 
  Vide -> {alphabet= alpha ; etats = [0] ; initiaux = [0] ; finals = [];
 transitions = [] }
| Epsilon -> {alphabet= alpha ; etats = [0] ; initiaux = [0] ; finals = [0];
 transitions = [] }
| Lettre e -> {alphabet= alpha ; etats = [0;1] ; initiaux = [0] ; finals = [1];
 transitions = [(0,e,1)] }  
| Produit (exp1,exp2) -> concat_aut (auto exp1) (auto exp2)
| Somme (exp1,exp2) -> reunion_aut (auto exp1) (auto exp2) 
| Etoile exp -> etoile_aut (auto exp) 
| Plus exp -> plus_aut (auto exp) in 
mini(auto f) ;; 


(* Et maintenant, pour un string ! :) *) 


let auto_reco s = 
let f = exprat_of_string s in  automate_reconnaissant f ;;





(* On va essayer de simplifier des expressions rationnelles, de manière élémentaire. *) 
(* Il faut classer les machins par ordre de meilleure simplification *) 

let simplifie_1 ex = 
let rec aux = function
   Vide -> Vide 
  |Epsilon -> Epsilon 
  |Lettre a -> Lettre a 
  |Somme(Vide,e1)-> aux e1 
  |Somme(e1,Vide)-> aux e1
  |Somme(e1,e2) when e1=e2 -> aux e1 
  |Somme(Plus e1,Epsilon) -> Etoile(aux e1) 
  |Somme(Epsilon,Plus e1) -> Etoile(aux e1) 
  |Somme(e1,Plus(e2)) when e2=e1 -> aux(Plus e1)
  |Somme(Plus(e1),e2) when e2=e1 -> aux(Plus e1)
  |Somme(e1,e2) -> Somme(aux e1,aux e2)
  |Produit(Vide,_)-> Vide 
  |Produit(_,Vide)-> Vide
  |Produit(Epsilon,e1) -> aux e1
  |Produit(e1,Epsilon) -> aux e1
  |Produit(e1,Etoile(e2)) when e1=e2 -> aux (Plus(e1))
  |Produit(Etoile(e2),e1) when e1=e2 -> aux (Plus(e1))
  |Produit(e1,e2)-> Produit(aux e1,aux e2)
  |Etoile(Vide) -> Vide 
  |Etoile(Epsilon)-> Epsilon 
  |Etoile(Etoile e1) -> aux (Etoile e1) 
  |Etoile(Plus e1) -> aux (Etoile e1) 
  |Etoile e1 -> Etoile (aux e1)
  |Plus(Vide) -> Vide 
  |Plus(Epsilon)-> Epsilon 
  |Plus(Plus e1) -> aux (Plus e1)
  |Plus e1 -> Plus (aux e1) in 
let res = ref ex in 
let encours= ref true in
while (!encours) do 
  let A = aux (!res) in 
  encours:= (!res<>A); res:=A done; 
!res ;;






let simplifie2 ex = 
let rec aux = function 
   Vide -> Vide 
  |Epsilon -> Epsilon 
  |Lettre a -> Lettre a 
  |Somme(Somme(Epsilon,e1),Plus e2) when e1=e2 -> aux(Etoile e1)
  |Somme(Plus e1,Somme(Epsilon,e2)) when e1=e2 -> aux(Etoile e1)
  |Somme(e1,e2) -> Somme(aux e1,aux e2)
  |Produit(e1,Produit(Etoile e2,e3)) when e1=e2 -> aux (Produit(Plus(e1),e3))
  |Produit(Produit(Etoile e2,e3),e1) when e1=e2 -> aux (Produit(Plus(e1),e3))
  |Produit(Plus(e1),Somme(Epsilon,e2)) when e1=e2 ->aux (Plus e1)
  |Produit(Etoile(e1),Somme(Epsilon,e2)) when e1=e2 ->aux (Etoile e1)
  |Produit(e1,e2)-> Produit(aux e1,aux e2) 
  |Etoile(Somme(Epsilon,e1)) -> aux (Etoile e1)
  |Etoile(Somme(e1,Epsilon)) -> aux (Etoile e1)
  |Etoile e1 -> Etoile (aux e1)
  |Plus(Somme(Epsilon,e1)) -> aux (Etoile e1) 
  |Plus(Somme(e1,Epsilon)) -> aux (Etoile e1) 
  |Plus e1 -> Plus (aux e1) in 
let res = ref ex in 
let encours= ref true in
while (!encours) do 
  let A = aux (!res) in 
  encours:= (!res<>A); res:=A done; 
!res ;;












(********************** Afficher un Automate ***********************)
(* Repris avec l'aimable accord de Salim Nibbouche, modifié pour que ca colle avec mon typage. Cela me sert pour faire des tests. 
Merci à lui.  *) 



#open "graphics";;
open_graph "";;
clear_graph();;

let Y = 200;;	(* hauteur de l'automate par rapport au bas de la fenêtre *)
let X = 0;;		(* distance de l'automate par rapport au bord vertical gauche de la fenêtre *)
let R = 20;;	(* rayon des bulles *)
let D = 100;;	(* distance entre deux états *)
let D2 = 50;;	(* distance entre les niveaux des flèches *)
let D3 = 5;;	(* distance entre les flèches de même niveau *)
let T_f = 3;;	(* taille des flèches *)


(* afficher la flèche qui part de p à q étiquetée par une liste a de chaines de caractères *)
let aff_fleche p a q X Y =
	if a = []
	then ()
	else
	begin
		let v = (* v est la liste de points du polygone qu'on veut tracer pour avoir la flèche *)
		begin
			if 		p = q
			then		[| (p*D-R/4+X,Y+R) ; (p*D-R/4+X,Y+D2) ; (p*D+R/4+X,Y+D2) ; (p*D+R/4+X,Y+R) ; (p*D+R/4+T_f+X,Y+R+T_f) ; (p*D+R/4-T_f+X,Y+R+T_f) ; (p*D+R/4+X,Y+R) |]
			else if 	q = p + 1
			then		[| (p*D+R+X,Y+D3) ; (q*D-R+X,Y+D3) ; (q*D-R-T_f+X,Y+D3+T_f) ; (q*D-R-T_f+X,Y+D3-T_f) ; (q*D-R+X,Y+D3) |]
			else if 	q = p - 1
			then		[| (p*D-R+X,Y-D3) ; (q*D+R+X,Y-D3) ; (q*D+R+T_f+X,Y-D3+T_f) ; (q*D+R+T_f+X,Y-D3-T_f) ; (q*D+R+X,Y-D3) |]
			else if	q > p
			then		[| (p*D+R+X,Y+D3) ; (p*D+((q-p)*D)/3+X,Y+(q-p)*D2+(p-1)*D3) ; (q*D-((q-p)*D)/3+X,Y+(q-p)*D2+(p-1)*D3) ; (q*D-R+X,Y+D3) ; (q*D-R-T_f+X,Y+D3+T_f) ; (q*D-R-T_f+X,Y+D3-T_f) ; (q*D-R+X,Y+D3) |]
			else		[| (p*D-R+X,Y-D3) ; (p*D+((q-p)*D)/3+X,Y+(q-p)*D2-(q-1)*D3) ; (q*D-((q-p)*D)/3+X,Y+(q-p)*D2-(q-1)*D3) ; (q*D+R+X,Y-D3) ; (q*D+R+T_f+X,Y-D3+T_f) ; (q*D+R+T_f+X,Y-D3-T_f) ; (q*D+R+X,Y-D3) |]
		end
		and n = it_list (prefix+) (list_length a - 1) (map string_length a) (* n est le nombre de caractères requis pour afficher l'étiquette *)
		in
		(* on trace les lignes *)
		moveto (fst v.(0)) (snd v.(0));
		do_vect (function (x,y) -> lineto x y) v;
		moveto (((p+q)*D-8*n)/2+X) (snd v.(1)+(if q>=p then 1 else -14));
		(* on affiche l'étiquette *)
		draw_string (hd a);
		do_list draw_string (map (function x->concat[",";x]) (tl a));
	end;;


(* affiche un état p : une bulle étiquetée p *)
let aff_etat p X Y =
	draw_circle (p*D+X) Y R;
	moveto (p*D-4+X) (Y-4);
	draw_string (string_of_int p);;


(* on affiche une flèche en bas de l'état p sensé être initial *)
let aff_init p X Y =
	moveto (p*D+X) (Y-R-D2);
	do_vect (function (x,y) -> lineto x y) [|(p*D+X,Y-R-D2);(p*D+X,Y-R);(p*D-T_f+X,Y-R-T_f);(p*D+T_f+X,Y-R-T_f);(p*D+X,Y-R)|];;


(* ajoute une deuxième couche à la bulle de l'état p sensé être final *)
let aff_final p X Y =
	draw_circle (p*D+X) Y (R-3);;



(* on affiche l'automate M, centré en l'ordonnée Y, dont la distance au bord gauche de la fenêtre est X
	aff est une fonction qui à tout étiquette associe une chaine de caractère lui correspondant *)
let aff_aut N aff X Y=
        let M= renomme_aut N 1 in 
        let n = list_length (M.etats)  in
	(* on affiche les états *)
	for i = 1 to n
	do
		aff_etat i X Y;
	done;
	let m = make_matrix n n [] in (* m est la matrice telle que m.(i).(j) contienne l'ensemble des
	étiquettes des transitions partant de i à j *)
	let rec f = function (* f remplit m en prenant pour argument E *)
		[]					->	()
	|	(p,a,q)::lst	->	m.(p-1).(q-1) <- a::m.(p-1).(q-1); f lst
	in
	(* on remplit m *) 
	f M.transitions;
	(* on affiche les transitions *)
	for p = 0 to n-1
	do
		for q = 0 to n-1
		do
			aff_fleche (p+1) (tri_fus(map aff m.(p).(q))) (q+1) X Y;
		done;
	done;
	(* on procure leur qualité aux états initiaux *)
	do_list (function x->aff_init x X Y) M.initiaux;
	(* on procure leur qualité aux états finaux *)
	do_list (function x->aff_final x X Y) M.finals;;



(* afficher un automate aux positions par défaut *)
let print_aut M  = aff_aut M (string_of_char) X Y;;









(***************************** Langage  Préfixiels **********************************)


(* Les langages "(a+b)*" et "(0+Epsilon)(1+(10))*" sont préfixiels, les langages "(a+b)*(a+b)" et "0+(1(1+0)*)" ne le sont pas *)


(* Fonction qui prend en argument le langage en Exprat (* il faudra faire l'effort d'écrire le langage en exprat... *) 
Et annonce si ce langage est préfixiel ou non ! *) 


let est_préfixiel f = 
let M = automate_reconnaissant f  in 
tri_fus M.finals = tri_fus M.etats ;;  

let A = Produit(Somme(Lettre `0`,Epsilon),Etoile(Somme(Lettre `1`,Produit(Lettre `1`,Lettre `0`)))) ;;


(* EXEMPLE est_préfixiel A renvoie true  c'est (0+Epsilon)(1+(10))*  *) 








(****************************** Teste si un automate reconnait un langage infini ou non (DM5) **************************) 

(* On va prendre l'automate minimal associé à l'automate que l'on nous donne. Il est en particulier émondé et tous ses états sont utiles. *) 


(* Pour montrer qu'un automate reconnait un langage infini, il faut regarder si il y a une boucle sur un état utile (principe de pompage du lemme 
de l'étoile. Si il existe un état utile qui est accessible en n lettres (n>0) a partir de ce même état, alors il y a une boucle. *) 

(* voici les états accessibles à partir d'un état p en un coup*) 

let etats_accessibles1 p T = (* T sera la fonction de transition de l'automate *)
let rec aux =  function 
  []  -> [] 
 |(((pp,_,qq)::r)) when pp=p -> qq::(aux r) 
 | (_::r) -> aux r in 
tri_fus(liste_purge(aux T)) ;;   



(* Donne les états accessibles en un pas d'une liste l d'états *) 

let etats_accessiblesl1 l T = (* T sera la fonction de transition de l'automate *)
let rec aux = function 
   [] -> [] 
  |(t::q) -> concat_liste (etats_accessibles1 t T) (aux q) in 
tri_fus(liste_purge(aux l)) ;;


(* Donne les états accessibles (en plus d'un pas) d'un état. On teste ensuite si p est dans cette liste (on teste si on a une boucle). On l'appliquera à tous les états utiles *) 

let etats_accessibles p T = 
let res=ref (etats_accessibles1 p T) in 
while !res<>(etats_accessiblesl1 (!res) T) do 
res:=(etats_accessiblesl1 (!res) T) done; 
appartient p !res ;; 



(* On le fait pour tous les états utiles, c'est la dernière fonction ! *) 

let est_infini N =
let M=mini N in 
let T=M.transitions in 
let rec aux = function 
  [] -> false
 |(t::q) -> (etats_accessibles t T) or aux q in 
aux (M.etats) ;; 












(***************************** Langage reconnu par un automate *******************************)
(************************* Algorithme de Nougkton Yamada *************************************)


(* Attention mon automate doit étre numéroté de 0 à n, qui ne contient pas de epsilon transitions*)

(* Fonction qui fait la somme de tous les éléments d'une liste d'expressions rationnelles *) 

let rec som_exp = function 
  [] -> Vide 
 |[ex]-> ex 
 |[ex1;ex2]-> Somme(ex1,ex2) 
 |(a::q) ->  Somme(a, som_exp q) ;;





(* Fonction auxilière pour initialiser un état *) 

let exp_init M i j = 
let rec aux = function
  [] -> [] 
 |((ii,a,jj)::q) when i=ii&&j=jj -> (Lettre a)::aux q
 |(_::q) ->  aux q in 
som_exp(aux M.transitions) ;;





(* Ce premier algorithme fait l'initialisation ! *) 


let initial M = 
let n=list_length (M.etats) in
let V=make_matrix n n Vide in 
for i=0 to (n-1) do 
  for j=0 to (n-1) do 
    let A = exp_init M i j in 
    if i=j 
    then V.(i).(j)<-if A = Vide then Epsilon else Somme(Epsilon,A) 
    else V.(i).(j)<-A 
  done; 
done;  
V;;





(* L'algorithme en dur ! *) 
(* Cela marche dans la plupart des cas. La simplification n'est pas optimale.*) 

let lang_reco G = 
let M = renomme_aut  (mini G) 0 in   
if M.initiaux = [] then "" else  (* Prenons une précaution... *)
let n=list_length(M.etats) in 
let Nk1 =ref(initial M) and N= make_matrix n n Vide in (* La matrice précédente, la matrice "encours de remplissage" *) 
for k=0 to (n-1) do 
  for i=0 to (n-1) do 
    for j=0 to (n-1) do 
      N.(i).(j)<-Somme(!Nk1.(i).(j), Produit(!Nk1.(i).(k),Produit(Etoile(!Nk1.(k).(k)),!Nk1.(k).(j)))) 
    done; 
  done;
if k<n then Nk1:=copy_matrix N ; done; 
let l=ref [] in 
(* L'automate étant minimal, il n'a qu'un seul état initial *)
let i0=hd(M.initiaux)  in 
let rec aux = function (* collecte les expressions rationnelles finales. N.(i0).(j) ou j est final *)
   [] -> [] 
  |(t::q) -> N.(i0).(t)::(aux q) in 
string_of_exprat (simplifie2(simplifie_1(som_exp(aux (M.finals))))) ;; 






 (* La fonction simplifie, peut être sujette à modifications, elle est d'ailleurs pas optimale dans les simplifications. Ca fera l'affaire...  *) 






















