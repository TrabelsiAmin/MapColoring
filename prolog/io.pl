% Graph I/O Module
% Handles reading graph files, managing adjacency facts, and writing solutions

:- dynamic adjacent/2.

% charger_carte(+File, -Vertices)
% Loads a graph from a file and extracts all unique vertices.
% File format: each line contains "node1 node2" representing an edge.
charger_carte(Fichier, Sommets) :-
    open(Fichier, read, S),
    lire_lignes(S),
    close(S),
    % Extract all unique vertices
    findall(X, (adjacent(X, _); adjacent(_, X)), Tous),
    sort(Tous, Sommets).

% lire_lignes(+Stream)
% Recursively reads lines from a stream and asserts adjacent/2 facts.
% The graph is treated as undirected (both directions are asserted).
lire_lignes(S) :-
    read_line_to_string(S, L),
    ( L == end_of_file -> true ;
        split_string(L, " ", "", [A,B]),
        atom_string(X, A), atom_string(Y, B),
        assertz(adjacent(X, Y)),  % Add edge X -> Y
        assertz(adjacent(Y, X)),  % Add edge Y -> X (undirected graph)
        lire_lignes(S)  % Continue reading
    ).

% sauvegarder_solution(+File, +Vertices, +Colors)
% Saves the coloring solution to a file.
% File format: each line contains "vertex color_number"
sauvegarder_solution(Fichier, [], []) :- 
    open(Fichier, write, F), close(F).
sauvegarder_solution(Fichier, Sommets, Couleurs) :-
    open(Fichier, write, F),
    sauvegarder(F, Sommets, Couleurs),
    close(F).

% sauvegarder(+Stream, +Vertices, +Colors)
% Helper predicate to recursively write vertex-color pairs.
sauvegarder(_, [], []).
sauvegarder(F, [S|Ss], [C|Cs]) :-
    format(F, "~w ~w~n", [S, C]),
    sauvegarder(F, Ss, Cs).
