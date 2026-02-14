% Graph Coloring Solver using Constraint Logic Programming (CLP)
% This module provides constraint-based coloring of graph vertices

:- use_module(library(clpfd)).
:- consult(io).

% colorier(+Vertices, -Solution)
% Finds a valid coloring of the graph vertices.
% Each vertex is assigned a color from the domain [1..4]
% Adjacent vertices must have different colors.
colorier(Sommets, Solution) :-
    length(Sommets, N),
    length(Solution, N),
    Solution ins 1..4,  % Domain: colors 1 to 4
    ajouter_contraintes(Sommets, Solution),
    labeling([ff], Solution).  % Find a solution using first-fail heuristic

% ajouter_contraintes(+Vertices, +Colors)
% Adds inequality constraints for all adjacent vertex pairs.
ajouter_contraintes(Sommets, Couleurs) :-
    findall((I, J), (adjacent(X, Y),
                     nth0(I, Sommets, X),
                     nth0(J, Sommets, Y),
                     I \= J), Contraintes),
    maplist(ajouter_inegalite(Couleurs), Contraintes).

% ajouter_inegalite(+Colors, +Pair)
% Adds a constraint that two adjacent vertices have different colors
ajouter_inegalite(Couleurs, (I, J)) :-
    nth0(I, Couleurs, Ci),
    nth0(J, Couleurs, Cj),
    Ci #\= Cj.  % Constraint: colors must be different

