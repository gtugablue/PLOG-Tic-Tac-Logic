:- use_module(library(clpfd)).

tictaclogic(B, S) :- board(B, S, S).

board([], _, 0) :- !.
board([H | T], Width, Height) :-
        length(H, Width),
        Height1 is Height - 1,
        board(T, Width, Height1).