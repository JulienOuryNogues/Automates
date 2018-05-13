from Automates import *



#============== Exemples d'automate ==========================

auto1=Automate()
auto1.alphabetSet(['0','1'])
auto1.etatsSet([3,1,2])
auto1.initialSet([1])
auto1.finalSet([3])
auto1.transitionsSet([(3, '0', 1), (2, '1', 1), (2, '1', 3), (1, '0', 2), (2, '1', 2), (2, '0', 1)])
auto1.print_aut()



auto2=Automate()
auto2.alphabetSet(['0','1','2'])
auto2.etatsSet([3,1,0])
auto2.initialSet([3])
auto2.finalSet([0])
auto2.transitionsSet([(0, '2', 3), (1, '1', 1), (1, '1', 0), (1, '2', 0), (3, '1', 1), (3, '2', 1),(1, '0', 1),(3, '0', 1)])
auto2.print_aut()





auto3=Automate()
auto3.alphabetSet(['a','b'])
auto3.etatsSet([0,1,2,3])
auto3.initialSet([0])
auto3.finalSet([3])
auto3.transitionsSet([(0, 'a', 0), (0, 'b', 0), (0, 'a', 1), (1, 'b', 2), (2, 'a', 3)])
auto3.print_aut()


auto4=Automate()
auto4.alphabetSet(['0','1'])
auto4.etatsSet([0,1])
auto4.initialSet([0])
auto4.finalSet([1])
auto4.transitionsSet([(0, '1', 1), (1, '0', 1), (1, '1', 1)])
auto4.print_aut()



auto5=Automate()
auto5.alphabetSet(['a','b'])
auto5.etatsSet([0,1,2,3])
auto5.initialSet([0])
auto5.finalSet([0])
auto5.transitionsSet([(0, '_', 1), (1, 'a', 2), (1, 'b', 3), (1, 'a', 2), (2, '_', 0), (3, 'b', 3), (3, 'a', 2)])
auto5.print_aut()


automini=Automate()
automini.alphabetSet(['a','b'])
automini.etatsSet([0,1,2])
automini.initialSet([0])
automini.finalSet([2])
automini.transitionsSet([(0, 'b', 0), (0, 'a', 1), (1, 'a', 1), (1, 'b', 2), (2, 'a', 2), (2, 'b', 2)])
automini.print_aut()


#print(auto5.clot_inst([3]))
#print(auto5.clot_inst([0,2]))


#auto=auto1.reunion(auto2)

#auto.print_aut()


#auto=auto1.da()
#auto.print_aut()


#auto=auto5.da()
#auto.print_aut()


#print(auto5.reco_vide())


#print(auto1.reco_meme_langage(auto1))

#print(auto2.reco_meme_langage(auto2))
#print(auto3.reco_meme_langage(auto3))
#print(auto4.reco_meme_langage(auto4))


#auto=automini.mini()
#auto.print_aut()
#on obtient bien les memes automates