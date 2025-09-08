:- dynamic adjacent/2.

charger_carte(Fichier, Sommets) :-
    open(Fichier, read, S),
    lire_lignes(S),
    close(S),
    findall(X, (adjacent(X, _); adjacent(_, X)), Tous),
    sort(Tous, Sommets).

lire_lignes(S) :-
    read_line_to_string(S, L),
    ( L == end_of_file -> true ;
        split_string(L, " ", "", [A,B]),
        atom_string(X, A), atom_string(Y, B),
        assertz(adjacent(X, Y)),
        assertz(adjacent(Y, X)),  % graphe non orienté
        lire_lignes(S)
    ).

sauvegarder_solution(Fichier, [], []) :- open(Fichier, write, F), close(F).
sauvegarder_solution(Fichier, Sommets, Couleurs) :-
    open(Fichier, write, F),
    sauvegarder(F, Sommets, Couleurs),
    close(F).

sauvegarder(_, [], []).
sauvegarder(F, [S|Ss], [C|Cs]) :-
    format(F, "~w ~w~n", [S, C]),
    sauvegarder(F, Ss, Cs).
