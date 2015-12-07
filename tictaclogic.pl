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
        write('Generating board...'), nl,
        statistics(walltime, [InitTime|_]),
        generate_board(B, Width, Height),
        statistics(walltime, [GenTime|_]),
        Delta1 is GenTime - InitTime,
        write('Board to be solved: '), nl, print_board(B), nl,
        write('Board generated in '), write_time(Delta1), nl, nl,
        solver(B, Width, Height, []),
        statistics(walltime, [SolveTime|_]),
        write('Solution: '), nl, print_board(B),
        Delta2 is SolveTime-GenTime,
        nl, write('Board solved in '), write_time(Delta2), nl, nl.

generate_board(B, Width, Height) :-
        solver(B1, Width, Height, [variable(sel)]),
        board_remove_pieces(B1, Width, Height, B).

board_remove_pieces(B, Width, Height, Result) :-
        print_board(B),
        board_nonempty_coords(B, Width, Height, NonEmpty),
        random_select(Coords, NonEmpty, Rest),
        board_remove_piece(B, Coords, B1),
        board_copy(B1, B2),
        findall(B2, solver(B2, Width, Height, []), L),
        board_remove_pieces_aux(B, B1, L, Width, Height, Rest, Result).
board_remove_pieces_aux(_, B1, L, Width, Height, _, Result) :-
        length(L, 1), !,
        board_remove_pieces(B1, Width, Height, Result).
board_remove_pieces_aux(B, _, _, _, _, [], B) :- !.
board_remove_pieces_aux(B, _, _, Width, Height, NonEmpty, Result) :-
        random_select(Coords, NonEmpty, Rest),
        board_remove_piece(B, Coords, B1),
        board_copy(B1, B2),
        findall(B2, solver(B2, Width, Height, []), L),
        board_remove_pieces_aux_aux(B, B1, L, Width, Height, Rest, Result).
board_remove_pieces_aux_aux(_, B1, L, Width, Height, _, Result) :-
        length(L, 1), !,
        board_remove_pieces(B1, Width, Height, Result).
board_remove_pieces_aux_aux(B, B1, L, Width, Height, [_ | T], Result) :- board_remove_pieces_aux(B, B1, L, Width, Height, T, Result).

%replace(+N, +X, +L1, -L2)
% Replaces the Nth member of L1 by X and instanciates L2 to the result.
replace(N, X, L1, L2) :-
        length(L3, N),
        append(L3, [_ | T], L1),
        append(L3, [X | T], L2).
              
board_xy(Board, [X, Y], Cell) :-
        nth0(Y, Board, Line),
        nth0(X, Line, Cell).

board_copy([], []) :- !.
board_copy([H1 | T1], [H2 | T2]) :-
        board_copy_aux(H1, H2),
        board_copy(T1, T2).
board_copy_aux([], []) :- !.
board_copy_aux([H1 | T1], [_ | T2]) :-
        var(H1), !,
        board_copy_aux(T1, T2).
board_copy_aux([H1 | T1], [H1 | T2]) :- board_copy_aux(T1, T2).

board_nonempty_coords(Board, Width, Height, NonEmpty) :- board_nonempty_coords_aux(Board, NonEmpty, Width, Height, [0, 0]).
board_nonempty_coords_aux([], _, _, Height, [0, Height]).
board_nonempty_coords_aux([Bh | Bt], NonEmpty, Width, Height, [0, Y]) :-
        board_nonempty_coords_aux_aux(Bh, NonEmpty1, Width, Height, [0, Y]),
        append(NonEmpty1, NonEmpty2, NonEmpty),
        Y1 is Y + 1,
        board_nonempty_coords_aux(Bt, NonEmpty2, Width, Height, [0, Y1]).
board_nonempty_coords_aux_aux([], [], Width, _, [Width, _]) :- !.
board_nonempty_coords_aux_aux([Rh | Rt], [[X, Y] | Et], Width, Height, [X, Y]) :-
        nonvar(Rh), !,
        X1 is X + 1,
        board_nonempty_coords_aux_aux(Rt, Et, Width, Height, [X1, Y]).
board_nonempty_coords_aux_aux([_ | Rt], NonEmpty, Width, Height, [X, Y]) :-
        X1 is X + 1,
        board_nonempty_coords_aux_aux(Rt, NonEmpty, Width, Height, [X1, Y]).

board_random_coords(Width, Height, [X, Y]) :-
        Total_size is Width * Height,
        random(0, Total_size, R),
        Div is R div Width,
        Mod is R mod Width,
        X = Div,
        Y = Mod.

board_remove_piece(Board, [X, Y], New_board) :-
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