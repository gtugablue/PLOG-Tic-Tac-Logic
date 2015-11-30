:- use_module(library(clpfd)).

tictaclogic(B, S) :-
        board(B, S, S),
        list_board_vars(B, L),
        domain(L, 1, 2),
        labeling([], L).

board([], _, 0) :- !.
board([H | T], Width, Height) :-
        length(H, Width),
        Height1 is Height - 1,
        board(T, Width, Height1).

test_board(B, 3) :-
        B = [[0, 0, 0],
             [0, 0, 0],
             [0, 0, 0]].

list_board_vars([], []).
list_board_vars([Bh | Bt], L) :-
        append(L1, Bh, L),
        list_board_vars(Bt, L1).