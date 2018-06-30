:- module(chess_io, [fen_io/2]).
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).

/**
* Convert input arguments to a internal prolog term or back.
* @arg In The fen input arguments.
* @arg Out The internal prolog term.
*/
fen_io('DRAW', 'DRAW') :- !.
fen_io(In, Out) :-
    var(Out), 
    arg_to_fen(In, Chars), 
    phrase(fen(Out), Chars).

fen_io(In, Out) :-
    nonvar(Out), 
    phrase(fen(Out), Chars), 
    arg_to_fen(In, Chars).

/**
* Parse FEN using DGC.
*
* Usage phrase(fen_config(X), "FENSTRING").
* The inverse operation and generation are also supported.
*
*/
fen(fen_config(Board, Turn, Castle, Passant, Half, Full)) -->
    board(Board), space, 
    turn(Turn), space, 
    castle(Castle), space, 
    en_passant(Passant), space, 
    [ Half ], space, [ Full ].

% Parse the prolog board.
board(board(R1, R2, R3, R4, R5, R6, R7, R8)) --> col(8, [ R8, R7, R6, R5, R4, R3, R2, R1 ]).

% Parse prolog columns.
col(1, [ H ]) --> row(H).
col(L , [ H|T ]) --> {succ(L2, L)}, row(H), forwardslash, col(L2, T).

% Parse a prolog row.
row(row(C1, C2, C3, C4, C5, C6, C7, C8)) --> pieces([ C1, C2, C3, C4, C5, C6, C7, C8 ] - []).

/**
* Parse prolog pieces or empty squares.
* 2 numbers in a FEN representation should never follow eachother!!!.
*/
pieces(Front - Back) --> empty(Front - Back).
pieces(Front - Back) --> empty(Front - Temp1), piece(Temp1 - Temp2), pieces(Temp2 - Back).

/**
* Build an empty list (list containing nil elements).
* @arg Length The length of the list to build.
* @arg List The list containing nil elements.
*/
build_empty(1, [ nil | X ] - X).
build_empty(L1, [ nil | Front ] - Back) :-  L1 < 9, succ(L2, L1), build_empty(L2, Front-Back).

/**
* Parse empty squares.
*/
empty(X - X) --> "".
empty(X) --> [ N ], { nth1(L, "12345678", N), build_empty(L, X) }.

/**
* Parse a single piece.
*/
piece([ P | E ] - E) --> [ C ], { is_piece(C, P) }.

/**
* Parse the turn.
*/
turn(b) --> "b".
turn(w) --> "w".

/**
* Parse column id's
*/
column_id(Alpha, Num) :- nth1(Num, "abcdefgh", Alpha).

/**
* Parse enpassant options.
*/
en_passant(nil) --> "-".
en_passant(3/C) --> [ Alpha ], "3", {column_id(Alpha, C)}.
en_passant(6/C) --> [ Alpha ], "6", {column_id(Alpha, C)}.

/**
* Parse castling options.
*/
castle(castle(false, false, false, false)) --> "-", !.
castle(castle(WK, WQ, BK, BQ)) -->
    castle('K', WK), 
    castle('Q', WQ), 
    castle('k', BK), 
    castle('q', BQ).

castle(Char, true) --> [Char].
castle(_, false) --> "".

/**
* Find the piece corresponding to a letter.
*
* @arg Char The Char representing the piece.
* @arg Piece The term representing the piece.
*/
is_piece('k', piece(b, king)).
is_piece('q', piece(b, queen)).
is_piece('r', piece(b, rook)).
is_piece('b', piece(b, bishop)).
is_piece('n', piece(b, knight)).
is_piece('p', piece(b, pawn)).
is_piece('K', piece(w, king)).
is_piece('Q', piece(w, queen)).
is_piece('R', piece(w, rook)).
is_piece('B', piece(w, bishop)).
is_piece('N', piece(w, knight)).
is_piece('P', piece(w, pawn)).

forwardslash --> "/".
space --> " ".

% UGLY CONVERSIONS yuk, ieuw, you're fired!
% DO NOT READ THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.
arg_to_fen([ H, F ], [ HN, ' ', FN ]) :- atom_number(H, HN), atom_number(F, FN).

arg_to_fen([ H|T ], L) :-
    var(L), !, 
    atom_chars(H, C), 
    arg_to_fen(T, T2), 
    append(C, [ ' '| T2 ], L).

arg_to_fen(A, L) :-
    nonvar(L), 
    append(L2, [ H, ' ', F ], L), 
    atom_chars(Temp, L2), 
    atomic_list_concat([H, ' ', F], Temp2), 
    atomic_concat(Temp, Temp2, A).

% vim: set sw=4 ts=4 et :
