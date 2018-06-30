:- begin_tests(chess_io).
:- set_prolog_flag(double_quotes, chars).
:- use_module(test_chess_util).
:- use_module('../src/chess_operations.pl').
:- use_module('../src/chess_io').


/**
* Test the is piece function mapping a char to a piece.
*/
test(is_piece, [forall((piece_type(T), color(C)))]) :-
    nth1(Index, [pawn/b, knight/b, bishop/b, rook/b, queen/b, king/b, 
                pawn/w, knight/w, bishop/w, rook/w, queen/w, king/w], T/C), 
    nth1(Index, "pnbrqkPNBRQK", Letter), 
    chess_io:is_piece(Letter, piece(C, T)), !.

/**
* Test the piece parsing function.
*/
test(piece, [forall((piece_type(T), color(C)))]) :-
    nth1(Index, [pawn/b, knight/b, bishop/b, rook/b, queen/b, king/b, 
                pawn/w, knight/w, bishop/w, rook/w, queen/w, king/w], T/C), 
    nth1(Index, "pnbrqkPNBRQK", Letter), 
    phrase(chess_io:piece([piece(C, T) | X] - X), [Letter]), !.
/**
* Test building a list containing nil's for a certain length.
*/
test(build_empty, [forall(between(1, 8, L))]) :-
    chess_io:build_empty(L, List - []), 
    length(List, L), 
    forall(member(X, List), X=nil), !.

/**
* Test parse empty squares.
*/
test(empty, [forall(nth1(L, "12345678", Char))]) :-
    phrase(chess_io:empty(List - []), [Char]), 
    length(List, L), 
    forall(member(X, List), X=nil), !.
/**
* Test parse emtpy squares.
*/
test(enpassant) :-
    phrase(chess_io:en_passant(nil), "-"), !.

test(enpassant, forall((nth1(C, "abcdefgh", CChar), member(R/RChar, [3/'3', 6/'6'])))) :-
    phrase(chess_io:en_passant(R/C), [CChar, RChar]).

to_wk(false, ''). to_wk(true, 'K').
to_wq(false, ''). to_wq(true, 'Q').
to_bk(false, ''). to_bk(true, 'k').
to_bq(false, ''). to_bq(true, 'q').

/**
* Test parsing castling options.
*/
test(castle) :-
    phrase(chess_io:castle(castle(false, false, false, false)), "-"), !.

test(castle, forall(maplist(bool, [ _, _, _, _], [A, B, C, D]))) :-
    Castle =.. [ castle | [A, B, C, D]], 
    to_wk(A, A2), to_wq(B, B2), 
    to_bk(C, C2), to_bq(D , D2), 
    exclude(=(''), [A2, B2, C2, D2], L), 
    phrase(chess_io:castle(Castle), L), !.



fenweight(C, Val) :- nth1(Val, "12345678", C), !.
fenweight(C, 1) :- member(C, "pnbrqkPNBRQK").
valid_fen_row(Fen, N) :- maplist(fenweight, Fen, NumberList), sum_list(NumberList, X), X=N.
valid_square(nil).
valid_square(piece(C, P)) :- color(C), piece_type(P).
/**
*
* Test all valid sub rows of length 3.
*/
test(pieces, forall(phrase(chess_io:pieces([A, B, C]-[]), Fen))) :-
    valid_fen_row(Fen, 3), 
    forall(member(Square, [A, B, C]), valid_square(Square)), 
    !.

:- end_tests(chess_io).

% vim: set sw=4 ts=4 et :
