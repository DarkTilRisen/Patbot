:- module(test_chess_util, [piece_type/1, color/1, bool/2, empty_config/1, random_pieces/5, random_color/1, random_position/1, equal/2, filter_valid_me/3]).
:- use_module('../src/chess_operations.pl').
:- use_module('../src/chess_rules.pl').


%%%%%%%%%%%% DISCLAIMER UGLY UTIL FUNCTIONS USED IN TESTS %%%%%%%%%%%%%%%

bool(_, false). bool(_, true).
piece_type(X) :- member(X, [pawn, knight, bishop, rook, queen, king]).
color(w).
color(b).


empty_config(fen_config(
    board(
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil), 
        row(nil, nil, nil, nil, nil, nil, nil, nil)
), w, castle(false, false, false, false), nil, 0, 1)).

% Taken from stackoverflow https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(Src, N, L) :- findall(E, (nth1(I, Src, E), I =< N), L).


% Hack for lambda I don't know how to fix this elegantly.
square(Config, Pos, Piece, NewConfig) :- chess_operations:set_square(Config, Pos, Piece, NewConfig).

% Put random pieces on the board.
random_pieces(Config, Colors, Pieces, Amount, NewConfig) :-
    findall(Empty, chess_operations:get_square(Config, Empty, nil), EmptySquares), 
    random_permutation(EmptySquares, RandomEmptySquares), 
    take(RandomEmptySquares, Amount, Squares), 
    % yes, I can still use folds. I love them.
    foldl(([Pos, CurrConfig, Out] >>
        (random_select(Color, Colors, _), 
        random_select(Piece, Pieces, _), 
        square(CurrConfig, Pos, piece(Color, Piece), Out)))
    , Squares, Config, NewConfig).

%select a random position
random_position(R/C) :- random(1, 9, R), random(1, 9, C).

%select a random color
random_color(C) :- random_select(C, [w, b], _).

% Check if the two lists contain the same elements.
equal(List1, List2) :-
    list_to_set(List1, Set1), list_to_set(List2, Set2), 
    length(Set1, L), 
    length(Set2, L), 
    subtract(Set1, Set2, []), subtract(Set2, Set1, []).

% filter out invalid coordinates and coordinates that already have a piece from the current player.
filter_valid_me(List1, Config, List3) :-
    include(chess_operations:all_coordinates, List1, List2), 
    exclude(chess_operations:is_mine(Config), List2, List3).

% vim: set sw=4 ts=4 et :
