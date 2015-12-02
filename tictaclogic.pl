:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- now(Timestamp),
   setrand(Timestamp).

:- ensure_loaded('visualization.pl').

tictaclogic(B, S) :-
        board(B, S, S),
        list_board_vars(B, L),
        all_different_lists(B),
        transpose(B, B1),
        all_different_lists(B1),
        domain(L, 1, 2),
        labeling([], L).

all_different_lists([]).
all_different_lists([H | T]) :-
        all_different_lists(H, T),
        all_different_lists(T).
all_different_lists(_, []).
all_different_lists(L, [H | T]) :-
        different_lists(L, H),
        all_different_lists(L, T).

different_lists([L1h | L1t], [L2h | L2t]) :-
        L1h #= L2h,
        different_lists(L1t, L2t).
different_lists([L1h | _], [L2h | _]) :- L1h #\= L2h.

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

%sel([H | T], H, T).
%sel(Vars, Selected, Rest) :- random_select(Selected, Vars, Rest), var(Selected).
%sel(Vars,Selected,Rest):- length(Vars, N), random(0, N, R), R1 is R+1, element(R1, Vars, Selected), var(Selected), list_delete(Vars, R, Rest).

%list_delete([_X|L1],0, L1).
%list_delete([X|L1], I, [X|L2]):- I > 0, I1 is I - 1, list_delete(L1, I1, L2).