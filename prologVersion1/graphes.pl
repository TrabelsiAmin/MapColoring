/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Graph definition 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
edge(a,b).% edge(b,a).
edge(b,e).
edge(a,c).
edge(c,d).
edge(e,d).
edge(d,f).
edge(d,g).
edge(g,a).%%Cycle

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
list operations 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%% 1- Remove Duplicates 
remove_duplicates(List, Unique) :-
    remove_duplicates(List, [], Unique).

remove_duplicates([], Acc, Acc).
remove_duplicates([H|T], Acc, Unique) :-
    member(H, Acc),
    remove_duplicates(T, Acc, Unique).

remove_duplicates([H|T], Acc, Unique) :-
    \+ member(H, Acc),
    remove_duplicates(T, [H|Acc], Unique). 

%% 2- Sepecial Remove Duplicates, keep first occurrence of each element
removeAll(_,[],[]).
removeAll(H,[H|T],T1):-removeAll(H,T,T1).
removeAll(H,[X|T],[X|L]):- H\=X, removeAll(H,T,L). 

sremove_duplicates([],[]).
sremove_duplicates([H|T],[H|L]):-removeAll(H,T,L1),sremove_duplicates(L1,L).


%% 3- List Difference (using find all)
%% it is possible to use the built-in predicate subtract/3
list_difference(List1, List2, Difference) :-
    findall(X, (member(X, List1), \+ member(X, List2)), Difference).

%% 4-Print a list Using maplist/2 with write/1
print_list(List) :-
    write('['),
    %% maplist(write, List),
    maplist(format(' ~w'), List),
    write(']').
/* Other possible solution 
print_list([]) :- 
    write('[]').
print_list([H|T]) :-
    write('['), write(H), print_rest(T), write(']').

print_rest([]).
print_rest([H|T]) :-
    write(', '), write(H), print_rest(T).
*/

%% 5-Inverse a list, with terminal recursivity
%% inv(L,I) true if I is the result of list inversion applied to L

inv(L,R):-inv(L,[],R).
inv([],Acc,Acc).
inv([H|T],Acc,R):-inv(T,[H|Acc],R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set operations
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
setOf(List,Set):-sremove_duplicates(List,Set).
setUnion(L1,L2,S):-append(L1,L2,A), setOf(A,S).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find all reachable elements from a given node list, can use either DFS
or BFS- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
- - - - - */
reachableNodes(Algo,Node,Result):-
    Algo=dfs,reachableDFS([Node],[],IResult), inv(IResult,Result).
reachableNodes(Algo,Node,Result):-
    Algo=bfs,reachableBFS([Node],[],IResult), inv(IResult,Result).
			
reachableDFS([],Result,Result).
reachableDFS([X|ToVisit],Visited,R):-
    not(member(X,Visited)),
    format('reachable DFS : '), print_list([X|ToVisit]),write(' '),
    format('Visited '),print_list(Visited),write('\n'),
    findall(Y, edge(X,Y), Children),
    setUnion(Children,ToVisit,NewToVisit),
    reachableDFS(NewToVisit,[X|Visited],R).
reachableDFS([X|ToVisit],Visited,R):-
    member(X,Visited),
    reachableDFS(ToVisit,Visited,R).

reachableBFS([],Visited,Visited).
reachableBFS([X|ToVisit],Visited,R):-
    not(member(X,Visited)),
    findall(Y, edge(X,Y), Children),
    setUnion(ToVisit, Children, NewToVisit),
    reachableBFS(NewToVisit,[X|Visited],R).
reachableBFS([X|ToVisit],Visited,R):-
    member(X,Visited),
    reachableBFS(ToVisit,Visited,R).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
- - - Find a path from source to destination, to avoid cycles, need to
keep track of visited nodes 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
% Fonctionne correctment si le graphe n'admet pas de cycle
path_no_cycle(End,End,[End]).
path_no_cycle(Start,End,[Start|Path]):-
    edge(Start,Next),
    path_no_cycle(Next,End,Path).
    
%path(X,Y,Visited,Path). true si Path est un chemin de X à Y dans le graphe et 
%Visited est l'ensemble des noeuds visités
%% La liste visited set à couper les branches rendondantes, au début
%% elle contient le noeud de départ 
path(Start,End,Path):-
    path(Start,End,[Start],Path).

path(End,End,_,[End]).
path(Start,End,Visited,[Start|Path]):-
    edge(Start,Next),
    not(member(Next,Visited)),
    path(Next,End,[Next|Visited],Path).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Find path using either DFS or BFS reachability
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
remove_if(_, [], []):-!.
remove_if(Pred, [H|T], Result) :-
    call(Pred, H), !, remove_if(Pred, T, Result).      
remove_if(Pred, [H|T], [H|Result]) :-
    not(call(Pred, H)), !, remove_if(Pred, T, Result). 

removeAll2(_, [], []):-!.
removeAll2(_-Y, [_-Y|T], Result) :-
    removeAll2(Pred, T, Result).      
removeAll2(_-Y, [X-Z|T], [X-Z|Result]) :-Z\=Y,
    removeAll2(_-Y, T, Result).      

sremove_duplicates2([],[]).
sremove_duplicates2([H|T],[H|L]):-removeAll2(H,T,L1),sremove_duplicates2(L1,L).


%% setUnion2 constructs the list for DFS and BFS
%% First removes redundancy, then appends
setUnion2(Parent,S1,S2,U):-append(S1,S2,S3), sremove_duplicates2(S3,U).

reconstruct_path(Goal,List,[Goal]):-
    member(Goal-Goal,List),!.

reconstruct_path(Goal,List,[Goal|Path]):-
    select(Parent-Goal,List,Rest),!,
    reconstruct_path(Parent,Rest,Path).
    

findPathDFS(Start,Goal,Path):-
    findPathDFS([Start-Start],Goal,[],IPath),
    reconstruct_path(Goal,IPath,RPath),
    inv(RPath,Path).

findPathDFS([X|_],Goal,Result,[X|Result]):-
    X=_-Goal, %Nouvelle Condition d'arret
    print_list([X|Result]).

findPathDFS([X|ToVisit],Goal,Visited,R):-
    not(member(X,Visited)),
    format('reachable DFS : X=~w, ToVisit=',X), print_list(ToVisit),write(' '),
    format('Visited '),print_list(Visited),write('\n'),
    X=P-N,
    findall(N-C, edge(N,C), Children),
    format('Children'),print_list(Children),write('\n'),

    setUnion2(N,Children,ToVisit,NewToVisit),
    findPathDFS(NewToVisit,Goal,[X|Visited],R).

findPathDFS([X|ToVisit],Goal,Visited,R):-
    member(X,Visited),
    findPathDFS(ToVisit,Goal,Visited,R).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BFS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

findPathBFS(Start,Goal,Path):-
    findPathBFS([Start-Start],Goal,[],IPath),
    reconstruct_path(Goal,IPath,RPath),
    inv(RPath,Path).

findPathBFS([_-Goal|_],Goal,Result,[_-Goal|Result]).

findPathBFS([P-N|ToVisit],Goal,Visited,R):-
    not(member(X,Visited)),
    findall(N-C, edge(N,C), Children),
    append(ToVisit,Children,NewToVisit),
    findPathDFS(NewToVisit,Goal,[P-N|Visited],R).
findPathBFS([_-N|ToVisit],Goal,Visited,R):-
    member(_-N,Visited),
    findPathDFS(ToVisit,Goal,Visited,R).

%% Détection de cycle
cycle(X,[X|C]):-edge(X,Y),path(Y,X,C).

%% Trouver le nombre de cycles dans un graphe
cycles(N):-findall(C, cycle(_,C),L),length(L,N).


