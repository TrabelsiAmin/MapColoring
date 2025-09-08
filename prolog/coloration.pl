:- use_module(library(clpfd)).
:- consult(io).

colorier(Sommets, Solution) :-
    length(Sommets, N),
    length(Solution, N),
    Solution ins 1..4,
    ajouter_contraintes(Sommets, Solution),
    labeling([ff], Solution).

ajouter_contraintes(Sommets, Couleurs) :-
    findall((I, J), (adjacent(X, Y),
                     nth0(I, Sommets, X),
                     nth0(J, Sommets, Y),
                     I \= J), Contraintes),
    maplist(ajouter_inegalite(Couleurs), Contraintes).

ajouter_inegalite(Couleurs, (I, J)) :-
    nth0(I, Couleurs, Ci),
    nth0(J, Couleurs, Cj),
    Ci #\= Cj.
