% Visualization

piece_to_ascii(P, ' ') :- var(P), !.
piece_to_ascii(0, ' ').
piece_to_ascii(1, 'X').
piece_to_ascii(2, 'O').

print_board([Line]) :-
        print_line(Line),
        length(Line, Size),
        print_separating_line(Size).
print_board([Line1, Line2 |Rest]) :-
        print_line(Line1),
        print_board([Line2 | Rest]).
print_board([]).

print_line(Line) :-
        length(Line, Size),
        print_separating_line(Size),
        print_line_cells(Line).

print_separating_line(Size) :-
        Size > 0, !,
        print_separating_line_aux,
        S1 is Size - 1,
        print_separating_line(S1).
print_separating_line(0) :- write('-'), nl.

print_separating_line_aux :- write('----').

print_line_cells([Cell|Rest]) :-
        write('|'),
        print_cell(Cell),
        print_line_cells(Rest).
print_line_cells([]) :- write('|'), nl.

print_cell(Cell) :-
        piece_to_ascii(Cell, P),
        write(' '), write(P), write(' ').