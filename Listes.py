__author__ = 'Julien'


#retourne le maximum d'une liste
#testé OK
def max_liste(l, res=0):
    if l==[]:
        return res
    return max(res,max(l))

#renvoie le produit cartésien de deux listes (toutes les combinaisons possibles
#testé OK
def prod_cart (A,B):
    def produit(l1,l2):
        if l1==[]:
            return []
        if l2==[]:
            return prod_cart(l1[1:],B)
        else:
            return [(l1[0],l2[0])]+produit(l1,l2[1:])
    return produit(A,B)


#print(prod_cart([1,2,3],[3,5]))


#donne le complementaire de l dans L
# OK
def complementaire(L,l):
    res=[]
    for e in L:
        if not(e in l):
            res.append(e)
    return res

#effectue la réunion de deux listes
#OK
def reunion (l1,l2):
    for e in l1:
        if not(e in l2):
            l2.append(e)
    return l2


#efectue l'intersection de deux listes
#OK
def inter(l1,l2):
    res=[]
    for e in l1:
        if e in l2:
            res.append(e)
    return res



#eneleve les doublons
#nickel
def liste_purge(l):
    try:
        return list(set(l))
    except:
        res=[]
        compt=0
        for e in l:
            compt+=1
            if not(e in l[compt:]):
                res.append(e)
        return res



#fait une liste de singletons plutot qu'une liste d'entiers
#Super
def listeliste_of_liste(l):
    return [[k] for k in l]


#retourne si deux listes sont egales en contenu
#OK
def liste_egales(l1,l2):
    if len(l1)!=len(l2):
        return False
    for e in l1:
        if not(e in l2):
            return False
    return True

#fonction map
#Non testée mais semble OK
def map (f,l):
    return [f(k) for k in l]


#fonction de renommage
#Si L=[2;3;5;1] et i=1, (renomme L x 1) transforme L en [1;2;3;4] : (renomme L 5 1) renvoie 3


def renomme(L,x,i):
    compt=i
    for e in L:
        if e!=x:
            compt+=1
        else:
            break
    return compt


def copy_matrix(M):
    N=[[False]*len(M[0]) for i in range(len(M))]
    for i in range(len(M[0])):
        for j in range(len(M)):
            N[i][j]=M[i][j]
    return N


#L est le produit cartesien entre les deux liste d'états t1 et t2 les deux fonctions de transition
#on fait le renommage en meme temps
def transition_intersection(L,t1,t2):
    res=[]
    for (deb1,lettre1,fin1) in t1:
        for(deb2,lettre2,fin2) in t2:
            if lettre1==lettre2:
                res.append((renomme(L,(deb1,deb2),0),lettre1,renomme(L,(fin1,fin2),0)))
    return res




#==== Completion
#obtiens les lettres non renseignées au départ d'un état
def get_lettres_manquantes(etat,alphabet,transitions):
    res=list(alphabet)
    for (debut,lettre,fin) in transitions:
        if (debut==etat) and (lettre in res):
            res.remove(lettre)
    return res

#crée les transitions manquantes vers un état poubelle
def creer_transition(etat,lettresmanquantes,poubelle):
    res=[]
    for lettre in lettresmanquantes:
        res.append((etat,lettre,poubelle))
    return res

#ferme la poubelle
def fermeture_poubelle(alphabet,poubelle):
    res=[]
    for lettre in alphabet:
        res.append((poubelle,lettre,poubelle))
    return res


#===================== Creation d'automates a partir d'une expression rationnelle
#cette fonction va créer des transitions instantanées entre les états de départs et les états de fins
def relier_inst(etats_depart,etats_fins):
    res=[]
    for depart in etats_depart:
        for fin in etats_fins:
            res.append((depart,"_",fin))
    return res

























