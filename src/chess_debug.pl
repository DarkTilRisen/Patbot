:- module(chess_debug, [write_board/1]).
:- use_module(chess_operations).
% DEBUGGING code for writing out bords in utf8

write_square(nil) :- write('·').
write_square(piece(b, king)) :- write('\u2654').
write_square(piece(b, queen)) :- write('\u2655').
write_square(piece(b, rook)) :- write('\u2656').
write_square(piece(b, bishop)) :- write('\u2657').
write_square(piece(b, knight)) :- write('\u2658').
write_square(piece(b, pawn)) :- write('\u2659').
write_square(piece(w, king)) :- write('\u265A').
write_square(piece(w, queen)) :- write('\u265B').
write_square(piece(w, rook)) :- write('\u265C').
write_square(piece(w, bishop)) :- write('\u265D').
write_square(piece(w, knight)) :- write('\u265E').
write_square(piece(w, pawn)) :- write('\u265F').

write_row(row(S1, S2, S3, S4, S5, S6, S7, S8)) :-
    write(' '), write_square(S1), 
    write(' '), write_square(S2), 
    write(' '), write_square(S3), 
    write(' '), write_square(S4), 
    write(' '), write_square(S5), 
    write(' '), write_square(S6), 
    write(' '), write_square(S7), 
    write(' '), write_square(S8), 
    write(' ').

write_board(board(R1, R2, R3, R4, R5, R6, R7, R8)) :-
    nl, 
    write_row(R8), write(8), nl, 
    write_row(R7), write(7), nl, 
    write_row(R6), write(6), nl, 
    write_row(R5), write(5), nl, 
    write_row(R4), write(4), nl, 
    write_row(R3), write(3), nl, 
    write_row(R2), write(2), nl, 
    write_row(R1), write(1), nl, 
    write(' a b c d e f g h'), 
    nl.

write_board(Config) :-
    get_board(Config, Board), 
    write_board(Board).

% vim: set sw=4 ts=4 et :
