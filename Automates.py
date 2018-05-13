__author__ = 'Julien'
from Listes import *
from Exprat import *
from tkinter import *


class Automate(object):

    def __init__(self):
        pass

    def initialGet(self):
        return self.initial

    def etatsGet(self):
        return self.etats

    def finalGet(self):
        return self.final

    def transitionsGet(self):
        return self.transitions

    def alphabetGet(self):
        return self.alphabet


    def initialSet(self, init):
        self.initial=init

    def alphabetSet(self, alph):
        self.alphabet  =  alph

    def etatsSet(self, et):
        self.etats  =  et

    def finalSet(self, fin):
        self.final = fin

    def transitionsSet(self, transitions):
        self.transitions  =  transitions

    #renomme un automate a partie de l'etat 0
    def renomme_canonique(self):
        self.renomme_aut(0)

    #teste ca marche, les automates sont bien les memes
    #renomme un automate a partir de l'état i
    def renomme_aut(self,i):
        anciens_etats=self.etats
        self.etats=[renomme(anciens_etats,k,i) for k in anciens_etats]
        self.initial=[renomme(anciens_etats,k,i) for k in self.initial]
        self.final=[renomme(anciens_etats,k,i) for k in self.final]
        self.transitions=[(renomme(anciens_etats,debut,i),a,renomme(anciens_etats,fin,i)) for (debut,a,fin) in self.transitions]

    def print_aut(self):
        dico=self.__dict__
        txt="{"
        txt+= "alphabet : "+str(dico['alphabet'])+", "
        txt+= "etats : "+str(dico['etats'])+", "
        txt+="initial : "+str(dico['initial'])+", "
        txt+="final : "+str(dico["final"])+", "
        txt+="transitions : "+str(dico["transitions"])+'}'
        print(txt)


    #===== TRANSITIONS INSTANTANEES

    def trans_instantanees(self):
        res=[]
        for e in self.transitions:
            if e[1]=='_':
                res+=[(e[0],e[2])]
        return res


    #Algrothme de Roy Worshall pour traiter des clotures instantanées. Renvoie une matrice de booleens pour dire si accessible ou pas
    def worshall(self):
        self.renomme_canonique() #necessite d'etre canonique
        transitions_inst=self.trans_instantanees()
        n = len(self.etats)
        N=[[False]*n for i in range(n)]
        #initialisation de la matrice N resultat
        for i in range(n):
            for j in range(n):
                N[i][j]=(i==j) or (i,j)in transitions_inst

        for k in range(n):
            Nk=copy_matrix(N)
            for i in range(n):
                for j in range(n):
                    N[i][j]=Nk[i][j] or (Nk[i][k]and Nk[k][j])
        return N

    #donne la cloture instantanée d'une liste d'états
    #testé et ca marche
    def clot_inst(self,l):
        N=self.worshall()
        res=[]
        for e in l:
            ee=renomme(self.etats,e,0)
            for i in range(len(N[ee])):
                if N[ee][i]:
                    res+=[i]
        res=liste_purge(res)
        res.sort()
        return res

    #cloture instantanée d'un etat seul
    def clot_inst_etat(self,e):
        return self.clot_inst([e])


    #=============== MOTS RECONNUS

    #donne les etats atteints en ayant proconce une lettre a partir d'une liste d'états
    def lecture(self,etats,let):
        etats_suivants=[]
        for (debut,lettre,fin) in self.transitions:
            if lettre==let and debut in etats:
                etats_suivants+=self.clot_inst_etat(fin)
        return etats_suivants


    #on va verifier si un mot est reconnu par un automate
    #semble OK
    def reconnu(self,mot):
        etats=self.clot_inst(self.initial)
        for let in mot:
            etats_suivants=self.lecture(etats,let)
            etats_suivants=liste_purge(etats_suivants)
            etats_suivants.sort()
            etats=etats_suivants
        return (inter(etats,self.final)!=[])


    #============ TRANSPOSITION
    #transposition d'un automate
    #Ok ca marche
    def transposition(self):
        res=Automate()
        res.alphabetSet(self.alphabet)
        res.etatsSet(self.etats)
        res.initialSet(self.final)
        res.finalSet(self.initial)
        res.transitionsSet([(fin,lettre,debut) for (debut,lettre,fin) in self.transitions])
        return res


    #====== REUNION

    #reunion de deux automates
    #semble OK
    def reunion(self,other):
        self.renomme_canonique()
        other.renomme_canonique()
        m=len(self.etats)
        n=len(other.etats)
        res=Automate()
        res.alphabetSet(reunion(self.alphabet,other.alphabet))
        res.etatsSet(range(0,n+m))
        res.initialSet([renomme(self.etats,k,0) for k in self.initial]+[renomme(other.etats,k,m) for k in other.initial])
        res.finalSet([renomme(self.etats,k,0) for k in self.final]+[renomme(other.etats,k,m) for k in other.final])
        res.transitionsSet([(renomme(self.etats,debut,0),lettre,renomme(self.etats,fin,0)) for (debut,lettre,fin) in self.transitions ]
        +[(renomme(other.etats,debut,m),lettre,renomme(other.etats,fin,m)) for (debut,lettre,fin) in other.transitions])
        return res



    #============= INTERSECTION

    #Intersection de deux automates (brutal)
    #non testé
    def intersection(self,other):
        self.renomme_canonique()
        other.renomme_canonique()
        etats_prod_car=prod_cart(self.etats,other.etats)
        res = Automate()
        res.alphabetSet(inter(self.alphabet,other.alphabet))
        res.etatsSet(range(0,len(etats_prod_car)))
        res.initialSet([renomme(etats_prod_car,k,0) for k in prod_cart(self.initial,other.initial)])
        res.finalSet([renomme(etats_prod_car,k,0) for k in prod_cart(self.final,other.final)])
        res.transitionsSet(transition_intersection(etats_prod_car,self.transitions,other.transitions))
        return res


    #============= COMPLETION
    #completion d'un automate en créant un état poubelle
    #emble OK


    def completion(self):
        self.renomme_canonique()
        transitions_a_ajouter=[]
        etat_poubelle=len(self.etats)
        for e in self.etats:
            manquantes = get_lettres_manquantes(e,self.alphabet,self.transitions)
            transitions_a_ajouter+=creer_transition(e,manquantes,etat_poubelle)
        if (transitions_a_ajouter==[]):
            #alors rien a faire, on n'ajoute rien
            return self
        transitions_a_ajouter+=fermeture_poubelle(self.alphabet,etat_poubelle)
        res=Automate()
        res.alphabetSet(self.alphabet)
        res.etatsSet(self.etats+[etat_poubelle])
        res.initialSet(self.initial)
        res.finalSet(self.final)
        res.transitionsSet(self.transitions+transitions_a_ajouter)
        return res



    #========== DETERMINISATION ACCESSIBLE


    #Fonction qui fait la liste des listes d'états accessibles à partir d'un seul état
    def etats_deterministes(self,alphabet,liste_liste_etat_depart):
        res=[]
        for let in alphabet:
            for etats in liste_liste_etat_depart:
                res.append(self.clot_inst(self.lecture(etats,let)))
        return liste_purge(res)

    #Fonction qui fait la liste de toutes les listes d'états accessibles selon l'algorithme de determinisation
    #prions pour que ca marche
    def tous_etats_deterministes(self):
        initiaux= [self.clot_inst(self.initial)]
        res=initiaux
        while (not(liste_egales(liste_purge(res+self.etats_deterministes(self.alphabet,res)),res))):
            res=liste_purge((liste_purge(res)+self.etats_deterministes(self.alphabet,res)))
        return res

    #Renvoie les fonctions de transitions de l'automate déterminisé.

    def trans_det(self,alphabet,liste_liste_etats):
        res=[]
        for let in alphabet:
            for etats in liste_liste_etats:
                res.append((renomme(liste_liste_etats,etats,0),let,renomme(liste_liste_etats,self.clot_inst(self.lecture(etats,let)),0)))
        return res

    #renvoie la liste d'états finals non renommée de l'automate deterministe


    def final_det(self,liste_liste_etats):
        res=[]
        for etats in liste_liste_etats:
            if inter(self.clot_inst(etats),self.final):
                res.append(etats)
        return res


    #algorithme de determinisation accessible
    #testé et approuvé
    def da(self):
        self.renomme_canonique()
        tous_etats_deterministes=self.tous_etats_deterministes()
        res=Automate()
        res.alphabetSet(self.alphabet)
        res.etatsSet(range(0,len(tous_etats_deterministes)))
        res.initialSet([renomme(tous_etats_deterministes,self.clot_inst(self.initial),0)])
        res.finalSet([renomme(tous_etats_deterministes,k,0) for k in self.final_det(tous_etats_deterministes)])
        res.transitionsSet(self.trans_det(self.alphabet,tous_etats_deterministes))
        return res



    #=============== Complementarisation
    #donne le lange complementaire reconnu par l'automate
    def complementaire(self):
        res=self.da()
        res.finalSet(complementaire(res.etats,res.final))
        return res

    #============== Différence symétrique

    #difference symetrique entre deux automates
    def difference_symetrique(self,other):
        return (self.reunion(other).da().intersection(self.intersection(other).da().complementaire())).da()

    #============reconnaitre le vide

    def reco_vide(self):
        return self.da().final==[]

    #Dit si deux automates reconnaissent le meme langage
    #ca semble marcher mais c'est un peu long a la compilation
    #usage deconseillé
    def reco_meme_langage(self,other):
        if not(liste_egales(self.alphabet,other.alphabet)):
            print("Cas ignoré : les deux automates ne se basent pas sur le meme alphabet")
            return False
        if (self.reco_vide() and other.reco_vide()):
            return True
        return self.difference_symetrique(other).reco_vide()



    #========= EMONDAGE

    #Fonction qui détermine la liste des etats accessibles
    def etats_accessibles(self):
        #cette sous fonction donne la liste des etats accessibles a partir d'une liste d'états
        def etat_accessible(l):
            res=[]
            for (debut,lettre,fin) in self.transitions:
                if debut in l:
                    res.append(fin)
            return res

        res=self.initial
        res.sort()
        while(not(liste_egales(res,liste_purge(etat_accessible(res)+res)))):
            res=liste_purge(etat_accessible(res)+res)
            res.sort()
        return res

    #Fonction qui détermine la liste des etats coaccessibles
    def etats_coaccessibles(self):
        #cette sous fonction donne la liste des etats coaccessibles a partir d'une liste d'états
        def etat_coaccessible(l):
            res=[]
            for (debut,lettre,fin) in self.transitions:
                if fin in l:
                    res.append(debut)
            return res
        res=self.final
        res.sort()
        while(not(liste_egales(res,liste_purge(etat_coaccessible(res)+res)))):
            res=liste_purge(etat_coaccessible(res)+res)
            res.sort()
        return res

    #les états utiles sont les états accessibles et co accessibles
    def etats_utiles(self):
        return inter(self.etats_accessibles(),self.etats_coaccessibles())

    def transitions_utiles(self,utiles):
        res=[]
        for(debut,let,fin) in self.transitions:
            if (debut in utiles) and (fin in utiles):
                res.append((debut,let,fin))
        return res

    #emonde un automate
    #semble OK
    def emonde(self):
        utiles=self.etats_utiles()
        res=Automate()
        res.alphabetSet(self.alphabet)
        res.etatsSet(utiles)
        res.initialSet(inter(utiles,self.initial))
        res.finalSet(inter(utiles,self.final))
        res.transitionsSet(self.transitions_utiles(utiles))
        return res

    # Donne l'automate canonique deterministe émonde
    #Semble OK car mini ok
    def de(self):
        auto=self.da().emonde()
        auto.renomme_canonique()
        return auto


    #================== AUTOMATE MINIMAL
    #testé et semble bien marcher :)
    def mini(self):
        auto=self.transposition().de().transposition().de()
        auto.renomme_canonique()
        return auto

    #================== CREATION A PARTIR D'UN LANGAGE

    #concatène deux automates en reliant instantanément les états finaux du premier aux initiaux du second
    def concat_auto(self,other):
        n1=len(self.etats)
        n2=len(other.etats)
        self.renomme_canonique()
        other.renomme_aut(n1)
        res= Automate()
        res.alphabetSet(reunion(self.alphabet,other.alphabet))
        res.etatsSet(range(0,n1+n2))
        res.initialSet(self.initial)
        res.finalSet((other.final))
        res.transitionsSet(self.transitions+other.transitions+relier_inst(self.final,other.initial))
        return res

    # plus-ise un automate en reliant instantanément ses états finaux à ses initiaux
    def plus_auto(self):
        self.renomme_canonique()
        res= Automate()
        res.alphabetSet(self.alphabet)
        res.etatsSet(self.etats)
        res.initialSet(self.initial)
        res.finalSet(self.final)
        res.transitionsSet(self.transitions+relier_inst(self.final,self.initial))
        return res

    #On plus-ise, puis on rajoute epsilon (un état relié a rien qui est final et initial a la fois)
    def etoile_auto(self):
        res = self.plus_auto()
        etat_epsilon=max_liste(res.etats)+1
        res.etatsSet(res.etats +[etat_epsilon])
        res.initialSet(res.initial +[etat_epsilon])
        res.finalSet(res.final +[etat_epsilon])
        return res



    #=================== LANGAGE RECONNU PAR UN AUTOMATE
    #Algorithme de Nougton Yamada
    #applicable qu'a un automate numéroté a partir de 0 canonique
    #on le fera donc pour les automates minimaux

    #initialisation pour un état
    def exp_init(self,i,j):
        res=[]
        for(debut,let,fin)in self.transitions:
            if(debut==i and fin==j):
                res.append(Exprat("Lettre",let))
        return som_exp(res)

    #initialise la matrice
    def initialise_Yamada(self):
        n=len(self.etats)
        M=[[Exprat("Vide")]*n for i in range(n)]
        for i in range(n):
            for j in range(n):
                A = self.exp_init(i,j)
                if i==j:
                    if A.type=="Vide":
                        M[i][j]=Exprat("Epsilon")
                    else:
                        M[i][j]=Exprat("Somme",Exprat("Epsilon"),A)
                else:
                    M[i][j]=A
        return M

    #Fonctionne, aux simplifications près...
    def langage_reconnu(self):
        auto=self.mini()
        if auto.initial==[]:
            return ""
        n=len(auto.etats)
        Nk=auto.initialise_Yamada() #matrice precedente
        N=[[Exprat("Vide")]*n for i in range(n)] #matrice en cours de remplissage
        for k in range(n):
            for i in range(n):
                for j in range(n):
                    N[i][j]=Exprat("Somme",Nk[i][j],Exprat("Produit",Nk[i][k],Exprat("Produit",Exprat("Etoile",Nk[k][k]),Nk[k][j])))
            if k<n:
                Nk=copy_matrix(N)
        #on termine l'algorithme en recuperant tous les N[i][j] ou (i,j) sont dans initiaux*finaux
        #remarque : auto est minimal donc n'a qu'un seul etat initial
        l=[]
        i0=auto.initial[0]
        for e in auto.final:
            if N[i0][e].type!="Vide" and not(appartient_expr(N[i0][e],l)):
                l.append(N[i0][e])
        l=som_exp(l)
        l=simplifie1(l)
        #l=simplifie2(l)
        return string_of_exprat(l)

    #dessine moi un automate
    def dessine_auto(self):
        global can
        Y=150 # Distance automate par rapport au haut de la fenetre
        X=70 #distance par rapport au bord gauche de la fenetre
        R=15 #rayon des bulles
        D=60 #distances entre états
        D2=30 #distance entre les niveaux de fleche
        D3=5 #Distance entre fleches de meme niveau
        T_f=3 #taille des fleches
        self.renomme_canonique()
        #pour afficher la fleche qui part de p à q avec une liste a de lettres
        def aff_fleche(p,a,q,X,Y):
            if a==[]:
                return
            if p==q:
                v=[(p*D-R+X,Y+R), (p*D-R+X,Y+D2), (p*D+R+X,Y+D2),(p*D+R+X,Y+R) , (p*D+R+T_f+X,Y+R+T_f) , (p*D+R-T_f+X,Y+R+T_f) , (p*D+R+X,Y+R) ]
            elif (p+1)==q:
                v=[(p*D+R+X,Y+D3),(q*D-R+X,Y+D3),(q*D-R-T_f+X,Y+D3+T_f),(q*D-R-T_f+X,Y+D3-T_f),(q*D-R+X,Y+D3)]
            elif (p-1)==q:
                v=[(p*D-R+X,Y-D3) , (q*D+R+X,Y-D3) , (q*D+R+T_f+X,Y-D3+T_f), (q*D+R+T_f+X,Y-D3-T_f), (q*D+R+X,Y-D3)]
            elif (q>p):
                v=[(p*D+R+X,Y+D3) , (p*D+((q-p)*D)/3+X,Y+(q-p)*D2+(p-1)*D3) , (q*D-((q-p)*D)/3+X,Y+(q-p)*D2+(p-1)*D3) , (q*D-R+X,Y+D3) , (q*D-R-T_f+X,Y+D3+T_f) , (q*D-R-T_f+X,Y+D3-T_f) , (q*D-R+X,Y+D3)]
            else:
                v=[(p*D-R+X,Y-D3) , (p*D+((q-p)*D)/3+X,Y+(q-p)*D2-(q-1)*D3) , (q*D-((q-p)*D)/3+X,Y+(q-p)*D2-(q-1)*D3) , (q*D+R+X,Y-D3) , (q*D+R+T_f+X,Y-D3+T_f) , (q*D+R+T_f+X,Y-D3-T_f) , (q*D+R+X,Y-D3)]
            n=len(a) #nombre de caractères a afficher
            for i in range(0,len(v)-1):
                (x0,y0)=v[i]
                (x1,y1)=v[i+1]
                can.create_line(x0,y0,x1,y1)
            etiquette=",".join(a)
            x=((p+q)*D-8*n)/2+X
            y=v[1][1]
            if(q>=p):
                y+=1
            else:
                y-=14
            can.create_text(x+len(a)*4,y+5,text=etiquette)
            return


        #pour afficher un etat p
        def aff_etat (p,X,Y):
            can.create_oval(p*D+X-R,Y-R,p*D+X+R,Y+R, outline="black")
            can.create_text(p*D+X, Y, text=str(p))
            return

        #affiche une fleche a côté de l'état initial
        def aff_init(p,X,Y):
            v=[(p*D+X,Y-R-D2),(p*D+X,Y-R),(p*D-T_f+X,Y-R-T_f),(p*D+T_f+X,Y-R-T_f),(p*D+X,Y-R)]
            for i in range(0,len(v)-1):
                (x0,y0)=v[i]
                (x1,y1)=v[i+1]
                can.create_line(x0,y0,x1,y1)
            return

        #affiche un double cercle pour les etats finaux
        def aff_final (p,X,Y):
            can.create_oval(p*D+X-(R-3),Y-(R-3),p*D+X+R-3,Y+R-3, outline="black")
            return
        #affiche l'automate
        def aff_aut(X,Y):
            #on dessine les etats
            n=len(self.etats)
            for e in self.etats:
                aff_etat(e,X,Y)
            M=[[0]*n for i in range(0,n)]
            for i in range(n):
                for j in range(n):
                    M[i][j]=list()
            #M[i][j] contient l'ensemble des etiquettes allant de i a j
            for (debut,let,fin) in self.transitions:
                M[debut][fin]+=[let]
            #on affiche les transitions
            for i in range(n):
                for j in range(n):
                    aff_fleche(i,M[i][j],j,X,Y)
            #on traites les états initiaux
            for e in self.initial:
                aff_init(e,X,Y)
            #on traite les états finaux
            for e in self.final:
                aff_final(e,X,Y)
            return
        aff_aut(X,Y)
        return









#================Creation d'automates canoniques


#automate qui reconnait le vide etant donne un alphabet donne possible

def auto_vide(alphabet=[]):
    res=Automate()
    res.alphabetSet(alphabet)
    res.etatsSet([])
    res.initialSet([])
    res.finalSet([])
    res.transitionsSet([])
    return res


#automate qui reconnait epsilon etant donne un alphabet possible

def auto_epsilon(alphabet=[]):
    res=Automate()
    res.alphabetSet(alphabet)
    res.etatsSet([0])
    res.initialSet([0])
    res.finalSet([0])
    res.transitionsSet([])
    return res



#automate qui reconnait une lettre etant donne un alphabet qui doit contenir cette lettre

def auto_lettre(lettre, alphabet=[]):
    if len(lettre)!=1:
        print("erreur : la lettre entrée doit etre un caractere")
        return
    if not lettre in alphabet:
        alphabet.append(lettre)
    res=Automate()
    res.alphabetSet(alphabet)
    res.etatsSet([0,1])
    res.initialSet([0])
    res.finalSet([1])
    res.transitionsSet([(0,lettre,1)])
    return res



#voici un programme qui a partir d'une expression rationnelle donne un automate qui le reconnait
def automate_reconnaissant_expr(expr):
    alpha=get_alphabet(expr)
    def construction(exp):
        if exp.type=="Vide":
            return auto_vide(alpha)
        if exp.type=="Epsilon":
            return auto_epsilon(alpha)
        if exp.type=="Lettre":
            return auto_lettre(exp.arg1,alpha)
        if exp.type=="Produit":
            return construction(exp.arg1).concat_auto(construction(exp.arg2))
        if exp.type=="Somme":
            return construction(exp.arg1).reunion(construction(exp.arg2))
        if exp.type=="Etoile":
            return construction(exp.arg1).etoile_auto()
        if exp.type=="Plus":
            return construction(exp.arg1).plus_auto()
        else:
            print("Erreur inattendue")
            return auto_vide(alpha)
    #je choisis d'envoyer l'automate minimal
    return construction(expr).mini()

#========================== SAINT GRAAL
#Voici le programme qui permet, a partir d'une expression rationnelle écrite en string, d'obtenir l'automate minimal qui la reconnait
def automate_reconnaissant_str(str):
    return automate_reconnaissant_expr(exprat_of_string(str))



#petite application interactive de dessin d'automates :)


def traitement(event):
    global entree,consigne,can,chang,auto
    expression=entree.get()
    entree.option_clear()
    try:
        auto=automate_reconnaissant_str(expression)
        can.delete("all")
        auto.dessine_auto()
    except:
        consigne.destroy()
        consigne=Label(text="Mauvaise expression rationnelle. Réessayez")
        consigne.grid(row=2,column=0)

def restart():
    global can,auto
    auto=auto_vide()
    can.delete("all")


def complet():
    global auto,can
    try:
        auto=auto.completion()
        can.delete("all")
        auto.dessine_auto()
    except:
        pass

def minim():
    global auto,can
    try:
        auto=auto.mini()
        can.delete("all")
        auto.dessine_auto()
    except:
        pass

def determin_acc():
    global auto
    auto.da()
    can.delete("all")
    auto.dessine_auto()

def get_lang():
    global auto
    langa=auto.langage_reconnu()
    can.create_text(650,350,text=langa)



#ajoute un etat
def ajoute_etat():
    global auto
    if auto.etats==[]:
        A=0
    else:
        A=max_liste(auto.etats)+1
    auto.etats.append(A)
    can.delete("all")
    auto.dessine_auto()
    return


def retire_etat():
    global ret,auto
    if auto.etats==[]:
        return
    else:
        etat=max_liste(auto.etats)
    try:
        auto.etats.remove(etat)
    except:
        pass
    try:
        auto.initial.remove(etat)
    except:
        pass
    try:
        auto.final.remove(etat)
    except:
        pass
    aretirer=list()
    for (debut,lettre,fin) in auto.transitions:
        if (debut==etat) or (fin==etat):
            aretirer.append((debut,lettre,fin))
    for e in aretirer:
        auto.transitions.remove(e)
    can.delete("all")
    auto.dessine_auto()





#ajoute un etat initial
def ajoute_etat_initial(event):
    global auto,entreeIAdd
    try:
        etat=int(entreeIAdd.get())
    except:
        return
    if (etat in auto.initial):
        return
    if (etat in auto.etats):
        auto.initial.append(etat)
        can.delete("all")
        auto.dessine_auto()

#retire un etat initial
def retire_etat_initial(event):
    global auto,entreeIRet
    try:
        etat=int(entreeIRet.get())
    except:
        return
    try:
        auto.initial.remove(etat)
    except:
        return
    can.delete("all")
    auto.dessine_auto()


#ajoute un etat final
def ajoute_etat_final(event):
    global auto,entreeFAdd
    try:
        etat=int(entreeFAdd.get())
    except:
        return
    if (etat in auto.final):
        return
    if (etat in auto.etats):
        auto.final.append(etat)
        can.delete("all")
        auto.dessine_auto()

#retire un etat final
def retire_etat_final(event):
    global auto,entreeFRet
    try:
        etat=int(entreeFRet.get())
    except:
        return
    try:
        auto.final.remove(etat)
    except:
        return
    can.delete("all")
    auto.dessine_auto()



def ajoute_trans(event):
    global auto,entreeTAdd
    try:
        t=entreeTAdd.get().split(",")
        depart=int(t[0])
        lettre=t[1]
        arrivee=int(t[2])
    except:
        return
    if (not(depart in auto.etats)) or  (not(arrivee in auto.etats)) or ((depart,lettre,arrivee) in auto.transitions):
        return
    if not (lettre in auto.alphabet):
        auto.alphabet.append(lettre)
    auto.transitions.append((depart,lettre,arrivee))
    can.delete("all")
    auto.dessine_auto()


def retire_trans(event):
    global auto,entreeTRet
    try:
        t=entreeTAdd.get().split(",")
        depart=int(t[0])
        lettre=t[1]
        arrivee=int(t[2])
    except:
        return
    try:
        auto.transitions.remove((depart,lettre,arrivee))
    except:
        return
    can.delete("all")
    auto.dessine_auto()






def ferme_menu():
    global consigne,entree,compl,da,mini,lang,ajou,ret,consigneIAdd,consigneIRet,entreeIAdd,entreeIRet,consigneFAdd,consigneFRet,entreeFAdd,entreeFRet,consigneTAdd,consigneTRet,entreeTAdd,entreeTRet
    try:
        consigne.destroy()
        entree.destroy()
    except:
        pass
    try:
        compl.destroy()
        da.destroy()
        mini.destroy()
        lang.destroy()
    except:
        pass
    try:
        ajou.destroy()
        ret.destroy()
        consigneIAdd.destroy()
        consigneIRet.destroy()
        entreeIAdd.destroy()
        entreeIRet.destroy()
        consigneFAdd.destroy()
        consigneFRet.destroy()
        entreeFAdd.destroy()
        entreeFRet.destroy()
        consigneTAdd.destroy()
        consigneTRet.destroy()
        entreeTAdd.destroy()
        entreeTRet.destroy()
    except:
        pass



def setER():
    global entree,consigne
    ferme_menu()
    consigne=Label(text="Entrez une expression rationnelle valide et appuyez sur entrée")
    consigne.grid(row=2,column=0)
    entree = Entry(fen)
    entree.bind("<Return>", traitement)
    entree.grid(row=2,column=1)


def setOp():
    global compl,mini,da,lang
    ferme_menu()
    compl=Button(fen,text="Complete l'automate",command=complet)
    compl.grid(row=2,column=0)
    mini=Button(fen,text="Minimise l'automate",command=minim)
    mini.grid(row=2,column=1)
    da=Button(fen,text="Determinise l'automate",command=determin_acc)
    da.grid(row=2,column=2)
    lang=Button(fen,text="Donne le langage reconnu par l'automate",command=get_lang)
    lang.grid(row=2,column=3)


def setDes():
    global ajou,ret,consigneIAdd,consigneIRet,entreeIAdd,entreeIRet,consigneFAdd,consigneFRet,entreeFAdd,entreeFRet,consigneTAdd,consigneTRet,entreeTAdd,entreeTRet
    #dessiner a la main un automate
    ferme_menu()
    ajou=Button(fen,text="ajoute un etat",command=ajoute_etat)
    ajou.grid(row=2,column=0,columnspan=2)
    ret=Button(fen,text="retire un etat",command=retire_etat)
    ret.grid(row=2,column=2,columnspan=2)
    consigneIAdd=Label(text="Ajoutez un etat initial et appuyez sur entree : ")
    consigneIAdd.grid(row=3,column=0)
    entreeIAdd = Entry(fen)
    entreeIAdd.bind("<Return>", ajoute_etat_initial)
    entreeIAdd.grid(row=3,column=1)
    consigneIRet=Label(text="Choisissez un EI a retirer :")
    consigneIRet.grid(row=3,column=2)
    entreeIRet = Entry(fen)
    entreeIRet.bind("<Return>", retire_etat_initial)
    entreeIRet.grid(row=3,column=3)
    consigneFAdd=Label(text="Ajoutez un etat final et appuyez sur entree :")
    consigneFAdd.grid(row=4,column=0)
    entreeFAdd = Entry(fen)
    entreeFAdd.bind("<Return>", ajoute_etat_final)
    entreeFAdd.grid(row=4,column=1)
    consigneFRet=Label(text="Choisissez un etat final à retirer :")
    consigneFRet.grid(row=4,column=2)
    entreeFRet = Entry(fen)
    entreeFRet.bind("<Return>", retire_etat_final)
    entreeFRet.grid(row=4,column=3)
    consigneTAdd=Label(text="Ajoutez une transition : depart,lettre,arrivee et appuyez sur entree :")
    consigneTAdd.grid(row=5,column=0)
    entreeTAdd = Entry(fen)
    entreeTAdd.bind("<Return>", ajoute_trans)
    entreeTAdd.grid(row=5,column=1)
    consigneTRet=Label(text="choisissez une transition à retirer :")
    consigneTRet.grid(row=5,column=2)
    entreeTRet = Entry(fen)
    entreeTRet.bind("<Return>", retire_trans)
    entreeTRet.grid(row=5,column=3)



fen=Tk()
fen.title("Dessine-moi un automate")
can=Canvas(fen,width=1300,height=400,bg="white")
can.grid(row=0,column=0,columnspan=5)
ajER=Button(fen,text="Créer un automate via une ER",command=setER)
ajER.grid(row=1,column=0)
ajDes=Button(fen,text="Créer un automate à la main",command=setDes)
ajDes.grid(row=1,column=1)
Op=Button(fen,text="Opérations sur les automates",command=setOp)
Op.grid(row=1,column=2)
res=Button(fen,text="Efface l'automate",command=restart)
res.grid(row=1,column=3)
qui=Button(fen,text="Quitter",command=fen.destroy)
qui.grid(row=1,column=4)
restart()
fen.mainloop()




