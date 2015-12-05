:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- now(Timestamp),
   setrand(Timestamp).

:- ensure_loaded('visualization.pl').

cross(1).
circle(2).

tictaclogic(Width, Height) :-
        Mw is Width mod 2,
        Mw = 0, % Width must be even
        Mh is Height mod 2,
        Mh = 0, % Height must be even
        solver(B1, Width, Height, []),
        write('First solution: '), nl, print_board(B1).

%replace(+N, +X, +L1, -L2)
% Replaces the Nth member of L1 by X and instanciates L2 to the result.
replace(N, X, L1, L2) :-
        length(L3, N),
        append(L3, [_ | T], L1),
        append(L3, [X | T], L2).

board_clear_piece(Board, [X, Y], New_board) :-
        nth0(Y, Board, Line),
        replace(X, _, Line, New_line),
        replace(Y, New_line, Board, New_board).

solver(B, Width, Height, LabelingParams) :-
        empty_board(B, Width, Height),
        list_board_vars(B, L),
        domain(L, 1, 2),
        
        % Restrictions
        Nw is Width div 2,
        Nh is Height div 2,
        transpose(B, B1),
        cross(X),
        circle(O),
        no_more_than_two_consecutive(B),
        no_more_than_two_consecutive(B1),
        same_number(B, Nw, X, O),
        same_number(B1, Nh, X, O),
        all_different_lists(B),
        all_different_lists(B1),

        % Labeling
        labeling(LabelingParams, L).

same_number([], _, _, _).
same_number([H | T], N, X, O) :-
        global_cardinality(H, [X-N, O-N]),
        same_number(T, N, X, O).

all_different_lists([]).
all_different_lists([H | T]) :-
        all_different_lists(H, T),
        all_different_lists(T).
all_different_lists(_, []).
all_different_lists(L, [H | T]) :-
        different_lists(L, H, Bs),
        sum(Bs, #\=, 0),
        all_different_lists(L, T).

different_lists([], [], []).
different_lists([L1h | L1t], [L2h | L2t], [B | Bs]) :-
        (L1h #\= L2h) #<=> B,
        different_lists(L1t, L2t, Bs).

empty_board([], _, 0) :- !.
empty_board([H | T], Width, Height) :-
        length(H, Width),
        Height1 is Height - 1,
        empty_board(T, Width, Height1).

list_board_vars([], []).
list_board_vars([Bh | Bt], L) :-
        append(Bh, L1, L),
        list_board_vars(Bt, L1).

no_more_than_two_consecutive([]).
no_more_than_two_consecutive([H | T]) :-
        no_more_than_two_consecutive_aux(H),
        no_more_than_two_consecutive(T).
no_more_than_two_consecutive_aux([_, _]).
no_more_than_two_consecutive_aux([H, H2, H3 | T]) :-
        H #\= H2 #\/ H #\= H3,
        no_more_than_two_consecutive_aux([H2, H3 | T]).

%sel([H | T], H, T).
sel(Vars, Selected, Rest) :- random_select(Selected, Vars, Rest), var(Selected).

test_board(B, 6, 6) :-
        cross(X),
        circle(O),
        B = [[_, _, X, _, _, _],
             [_, _, X, _, _, _],
             [X, _, _, _, _, X],
             [_, _, O, _, _, _],
             [_, X, _, _, _, X],
             [O, _, _, _, O, _]].

test_board2(B, 6, 6) :-
        cross(X),
        circle(O),
        B = [[_, _, _, _, _, _],
             [X, _, _, _, _, X],
             [_, _, _, X, _, _],
             [O, _, _, X, _, _],
             [_, O, _, _, _, _],
             [_, O, _, _, X, _]].

test_board(B, 8, 8) :-
        cross(X),
        circle(O),
        B = [[_, _, _, X, _, X, _, _],
             [_, X, _, _, _, _, _, _],
             [_, _, _, _, X, X, _, X],
             [_, _, O, _, _, _, O, _],
             [X, _, _, _, X, _, _, _],
             [_, _, _, O, _, _, X, X],
             [_, X, _, _, _, _, _, _],
             [_, _, O, _, _, _, O, _]].

test_board(B, 10, 10) :-
        cross(X),
        circle(O),
        B = [[_, _, X, _, _, _, _, X, _, _],
             [_, _, _, _, _, _, _, _, X, _],
             [_, O, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, O, _, O, _],
             [_, O, X, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, O, _, _, _, _, _],
             [_, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, _, _, _, _, _, _],
             [_, _, _, _, O, _, _, _, _, _]].