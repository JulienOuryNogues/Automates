__author__ = 'Julien'

from Listes import *

class Exprat:
        def __init__(self, type, arg1=None, arg2=None):
            #type could be : Etoile, Plus, Somme, Produit, Lettre, Vide, Epsilon
            self.type = type
            self.arg1 = arg1
            self.arg2 = arg2
        def __repr__(self):
            if self.type=="Produit" or self.type=="Somme":
                args = self.type, self.arg1, self.arg2
                return '({0} {1}, {2})'.format(*args)
            elif self.type=="Etoile" or self.type=="Plus" or self.type=="Lettre":
                args = self.type, self.arg1
                return '({0} {1})'.format(*args)
            else:
                return '({0})'.format(self.type)




#Le vide sera dénoté par #, le "plus" par °, l'étoile par *, epsilon par _, et la réunion par +
def est_lettre(a) :
    return (not a in [')' , '*' , '+' ,  '_' , '(' , '#','°'])

#OK
#Il reste le problème du produit, donc de la concaténation. IL faut impérativement que la concaténation soit faite avec un : sinon le programme plante avec SyntaxError
#C'est le boulot du programme insere2pts, qui va insérer ":" la ou il faut.
def insere2pts(string):
    n=len(string)
    if n==0:
        return ""
    def aux(t,i,j):
        if i==(n-1):
            return t
        else:
            if est_lettre(string[i]):
                if est_lettre(string[i+1]) or string[i+1]=="_" or string[i+1]=="#" or string[i+1]=="(":
                    return(aux(t[0:j]+":"+t[j:], i+1,j+2))
                else:
                    return aux(t,i+1,j+1)
            else:
                if string[i] in [')' , '*' ,  '_' , '#','°']:
                    if est_lettre(string[i+1]) or string[i+1]=="_" or string[i+1]=="#" or string[i+1]=="(":
                        return(aux(t[0:j]+":"+t[j:], i+1,j+2))
                    else:
                        return aux(t,i+1,j+1)
                else:
                    return aux(t,i+1,j+1)
    return aux(string,0,1)


#lourde fonction qui transforme les strings en expressions rationnelles
#Nickel !! :D

def exprat_of_string(string):
    s=insere2pts(string)
    def traitement(i,j):
        if (j-i<0):
            print("Erreur de syntaxe")
            return
        elif (j-i==0):
            if(s[i] in [')','*','+','(' ,'°',':']):
                print("Erreur de syntaxe")
                return
            elif s[i]=="_":
                return Exprat("Epsilon")
            elif s[i]=="#":
                return Exprat("Vide")
            else:
                return Exprat("Lettre",s[i])
        else:
            #on va essayer d'encercler une expression a laquelle on peut appliquer la recursion
            #typiquement on peut chercher des parenthèses ouvrantes/fermantes
            pos=-1
            numpar=0
            alter=False
            k=i-1
            while(k<j) and not(alter):
                k+=1
                if s[k]=="(":
                    numpar+=1
                elif s[k]==")":
                    numpar-=1
                elif s[k]=="+":
                    if (numpar==0):
                        alter=True #on dit si c'est un plus
                        pos=k
                elif s[k]==":":
                    if (numpar==0):
                        pos=k
                else:
                    pass
            if pos>=0:
                (e1,e2)=traitement(i,pos-1),traitement(pos+1,j)
                if alter:
                    return Exprat("Somme",e1,e2)
                else:
                    return Exprat("Produit",e1,e2)
            else:
                if s[j]=="*":
                    return Exprat("Etoile",traitement(i,j-1))
                elif s[j]=="°":
                    return Exprat("Plus",traitement(i,j-1))
                elif s[i]=="(" and s[j]==")":
                    return traitement(i+1,j-1)
                else:
                    print("Syntax error")
                    return
    if s=="":
        return Exprat("Vide")
    return traitement(0,len(s)-1)

#Sa réciproque
#satisfaisant
def string_of_exprat(expression):
    if expression.type=="Vide":
        return "#"
    if expression.type=="Epsilon":
        return "_"
    if expression.type=="Lettre":
        return expression.arg1
    if expression.type=="Somme":
        return "("+string_of_exprat(expression.arg1)+"+"+string_of_exprat(expression.arg2)+")"
    if expression.type=="Produit":
        return "("+string_of_exprat(expression.arg1)+string_of_exprat(expression.arg2)+")"
    if expression.type=="Etoile":
        arg=expression.arg1.type
        if arg=="Vide":
            return "#"
        if arg=="Epsilon":
            return "_"
        if arg=="Lettre":
            return expression.arg1.arg1+"*"
        if arg=="Somme":
            return string_of_exprat(expression.arg1)+"*"
        else:
            return "("+string_of_exprat(expression.arg1)+")*"
    if expression.type=="Plus":
        arg=expression.arg1.type
        if arg=="Vide":
            return "#"
        if arg=="Epsilon":
            return "_"
        if arg=="Lettre":
            return expression.arg1.arg1+"°"
        if arg=="Somme":
            return string_of_exprat(expression.arg1)+"°"
        else:
            return "("+string_of_exprat(expression.arg1)+")°"

#a partir d'une expression rationnelle, j'obtiens l'alphabet qu'elle utilise
def get_alphabet(expr):
    def aux(expr):
        if expr.type=="Vide" or expr.type=="Epsilon":
            return []
        if expr.type=="Lettre":
            return [expr.arg1]
        if expr.type=="Etoile" or expr.type=="Plus":
            return get_alphabet(expr.arg1)
        else: #Somme ou produit
            return get_alphabet(expr.arg1)+get_alphabet(expr.arg2)
    res=liste_purge(aux(expr))
    res.sort()
    return res


#==================== LANGAGE RECONNU PAR UN AUTOMATE


#Fonction qui va renvoyer l'expression rationnelle qui connote la somme des expressions rationnelles d'une liste

def som_exp(l):
    if l==[]:
        return Exprat("Vide")
    if len(l)==1:
        return l[0]
    if len(l)==2:
        return Exprat("Somme",l[0],l[1])
    else:
        return Exprat("Somme",l[0],som_exp(l[1:]))



def egalite_expr(exp1,exp2):
    if exp1==None and exp2==None:
        return True
    if exp1.type!=exp2.type:
        return False
    if exp1.type=="Lettre":
        return exp1.arg1==exp2.arg1
    return ((egalite_expr(exp1.arg1,exp2.arg1)and egalite_expr(exp1.arg2,exp2.arg2))or
                                       (egalite_expr(exp1.arg1,exp2.arg2)and egalite_expr(exp1.arg2,exp2.arg1)))


def appartient_expr(exp,liste):
    for e in liste:
        if egalite_expr(e,exp):
            return True
    return False


#==================== des fonctions random de simplification pour rendre les expressions plus lisibles


def simplifie1(expr):
    def aux(exp):
        if exp.type=="Vide" or exp.type=="Epsilon" or exp.type=="Lettre":
            return exp
        if exp.type=="Somme":
            if exp.arg1.type=="Vide":
                return aux(exp.arg2)
            if exp.arg2.type=="Vide":
                return aux(exp.arg1)
            if egalite_expr(exp.arg1,exp.arg2):
                return aux(exp.arg1)
            if exp.arg1.type=="Plus" and exp.arg2.type=="Epsilon":
                return aux(Exprat("Etoile",exp.arg1.arg1))
            if exp.arg2.type=="Plus" and exp.arg1.type=="Epsilon":
                return aux(Exprat("Etoile",exp.arg2.arg1))
            else:
                return Exprat("Somme", aux (exp.arg1), aux(exp.arg2))
        if exp.type=="Produit":
            if exp.arg1.type=="Vide" or exp.arg2.type=="Vide":
                return Exprat("Vide")
            if exp.arg1.type=="Epsilon":
                return aux(exp.arg2)
            if exp.arg2.type=="Epsilon":
                return aux(exp.arg1)
            if exp.arg1.type=="Etoile":
                if egalite_expr(exp.arg1.arg1,exp.arg2):
                    aux (Exprat("Plus",exp.arg2))
            if exp.arg2.type=="Etoile":
                if egalite_expr(exp.arg2.arg1,exp.arg1):
                    aux (Exprat("Plus",exp.arg1))
            else:
                return Exprat("Produit", aux(exp.arg1),aux(exp.arg2))
        if exp.type=="Etoile":
            if exp.arg1.type=="Vide":
                return Exprat("Vide")
            if exp.arg1.type=="Epsilon":
                return Exprat("Epsilon")
            if exp.arg1.type=="Etoile":
                return aux(exp.arg1)
            if exp.arg1.type=="Plus":
                return Exprat("Etoile",exp.arg1.arg1)
            else:
                return Exprat("Etoile",aux(exp.arg1))
        if exp.type=="Plus":
            if exp.arg1.type=="Vide":
                return Exprat("Vide")
            if exp.arg1.type=="Epsilon":
                return Exprat("Epsilon")
            if exp.arg1.type=="Etoile":
                return aux(exp.arg1)
            if exp.arg1.type=="Plus":
                return Exprat("Plus",exp.arg1.arg1)
            else:
                return Exprat("Plus",aux(exp.arg1))
    encours=True
    res=expr
    for i in range(10):
        res=aux(res)
    return res



#Experimental

def simplifie2(expr):
    def aux(exp):
        print(exp)
        if exp.type=="Vide" or exp.type=="Epsilon" or exp.type=="Lettre":
            return exp
        if exp.type=="Somme":
            try:
                if(exp.arg1.type=="Somme") and(exp.arg2.type=="Plus"):
                    if (exp.arg1.arg1.type=="Epsilon") and egalite_expr(exp.arg1.arg2,exp.arg2.arg1):
                        return aux(Exprat("Etoile"),exp.arg2.arg1)
                if(exp.arg2.type=="Somme") and(exp.arg1.type=="Plus"):
                    if (exp.arg2.arg1.type=="Epsilon") and egalite_expr(exp.arg2.arg2,exp.arg1.arg1):
                        aux(Exprat("Etoile"),exp.arg1.arg1)
            except:
                pass
            else:
                return Exprat("Somme", aux (exp.arg1), aux(exp.arg2))
        if exp.type=="Produit":
            try:
                if(exp.arg2.type=="Produit"):
                    if exp.arg2.arg1.type=="Etoile":
                        if egalite_expr(exp.arg1,exp.arg2.arg1.arg1):
                            return aux (Exprat("Produit",Exprat("Plus",exp.arg1),exp.arg2.arg2))
                if(exp.arg1.type=="Produit"):
                    if exp.arg1.arg1.type=="Etoile":
                        if egalite_expr(exp.arg2,exp.arg1.arg1.arg1):
                            return aux(Exprat("Produit",Exprat("Plus",exp.arg2),exp.arg1.arg2))
            except:
                pass
            else:
                return Exprat("Produit", aux(exp.arg1),aux(exp.arg2))
        if exp.type=="Etoile":
            if(exp.arg1.type=="Somme"):
                if(exp.arg1.arg1.type=="Epsilon"):
                    return aux(Exprat("Etoile",exp.arg1.arg2))
            if(exp.arg1.type=="Somme"):
                if(exp.arg1.arg2.type=="Epsilon"):
                    return aux(Exprat("Etoile",exp.arg1.arg1))
            else:
                return Exprat("Etoile",aux(exp.arg1))
        if exp.type=="Plus":
            if(exp.arg1.type=="Somme"):
                if(exp.arg1.arg1.type=="Epsilon"):
                    return aux(Exprat("Etoile",exp.arg1.arg2))
            if(exp.arg1.type=="Somme"):
                if(exp.arg1.arg2.type=="Epsilon"):
                    return aux(Exprat("Etoile",exp.arg1.arg1))
            else:
                return Exprat("Plus",aux(exp.arg1))
    res=expr
    for i in range(10):
        res=aux(res)
    return res


