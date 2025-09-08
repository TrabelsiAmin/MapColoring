:- use_module(library(pce)).
:- use_module(graphes).

run_gui :-
    new(D, dialog('Reachable Nodes Viewer')),

    new(StartNode, text_item(start_node)),
    send(D, append, StartNode),

    new(ResultLabel, label(result, 'Result here...')),
    send(D, append, ResultLabel),

    send(D, append,
        button('Find DFS Reachable', message(@prolog,
            show_reachable, StartNode?selection, ResultLabel))),

    send(D, open),
    send(D, enlarge, size(300, 100)).

show_reachable(StartAtom, Label) :-
    atom_string(Start, StartAtom),
    reachableNodes(dfs, Start, List),
    atomic_list_concat(List, ', ', Text),
    send(Label, selection, Text).

:- initialization(run_gui).
