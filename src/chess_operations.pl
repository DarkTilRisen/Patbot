:- module(chess_operations, [get_board/2, get_turn/2, get_castle/2, set_castle/3, get_enpassant/2, set_enpassant/3, get_half_count/2, get_full_count/2, set_square/4, get_square/3, is_empty/2, is_mine/2, set_turn/3, other_player/2 , all_coordinates/1, add_positions/3, update_half_count/4, update_full_count/2, set_half_count/3]).


/**
* Get the current board
*
* @arg Config The game configuration.
* @arg Board The board in the configuration.
*/
get_board(fen_config(Board, _, _, _, _, _), Board).

/**
* Set a new board in the config.
*
* @arg Config The current game configuration.
* @arg Board The new board.
* @arg NewConfig The new game configuration.
*/
set_board(fen_config(_, T, C, E, H, F), B, fen_config(B, T, C, E, H, F)).

/**
* Get the turn from the config.
*
* @arg Config The current game configuration.
* @arg Turn The turn in the configuration.
*/
get_turn(fen_config(_, Turn, _, _, _, _), Turn).

/**
* Set the turn in the config.
*
* @arg Config The current game configuration.
* @arg Turn The new Turn.
* @arg NewConfig The new game configuration.
*/
set_turn(fen_config(B, _, C, E, H, F), T, fen_config(B, T, C, E, H, F)).

/**
* Get the castle options from the game configuration.
*
* @arg Config The current game configuration.
* @arg Castle The castle options in the configuration.
*/
get_castle(fen_config(_, _, Castle, _, _, _), Castle).

/**
* Set the castling options in the config.
*
* @arg Config The current game configuration.
* @arg Castle The new Castle options.
* @arg NewConfig The new game configuration.
*/
set_castle(fen_config(B, T, _, E, H, F), C, fen_config(B, T, C, E, H, F)).

/**
* Get the enpasant options from the config.
*
* @arg Config The current game configuration.
* @arg EnPassant The enpassant options in the configuration.
*/
get_enpassant(fen_config(_, _, _, Passant, _, _), Passant).

/**
* Set the enpassant options in the config.
*
* @arg Config The current game configuration.
* @arg Turn The new Turn.
* @arg NewConfig The new game configuration.
*/
set_enpassant(fen_config(B, T, C, _, H, F), E, fen_config(B, T, C, E, H, F)).

/**
* Get the half count from the config.
*
* @arg Config The current game configuration.
* @arg HalfCount The half count in the configuration.
*/
get_half_count(fen_config(_, _, _, _, Half, _), Half).

/**
* Set the halfcount in the config.
*
* @arg Config The current game configuration.
* @arg HalfCount The new halfcount.
* @arg NewConfig The new game configuration.
*/
set_half_count(fen_config(B, T, C, E, _, F), H, fen_config(B, T, C, E, H, F)).

/**
* Get the full count from the config.
*
* @arg Config The current game configuration.
* @arg FullCount The full count in the configuration.
*/
get_full_count(fen_config(_, _, _, _, _, Full), Full).

/**
* Set the fullcount in the config.
*
* @arg Config The current game configuration.
* @arg FullCount The new Castle options.
* @arg NewConfig The new game configuration.
*/
set_full_count(fen_config(B, T, C, E, H, _), F, fen_config(B, T, C, E, H, F)).

/**
* Set the row in a board.
*
* @arg Board The current boardstate.
* @arg Row The row index.
* @arg Row The new row in a board.
* @arg NewBoard The new boardstate.
*/
set_row(board(_, _2, _3, _4, _5, _6, _7, _8), 1 , _1, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _, _3, _4, _5, _6, _7, _8), 2 , _2, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _, _4, _5, _6, _7, _8), 3 , _3, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _3, _, _5, _6, _7, _8), 4 , _4, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _3, _4, _, _6, _7, _8), 5 , _5, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _3, _4, _5, _, _7, _8), 6 , _6, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _3, _4, _5, _6, _, _8), 7 , _7, board(_1, _2, _3, _4, _5, _6, _7, _8)).
set_row(board(_1, _2, _3, _4, _5, _6, _7, _), 8 , _8, board(_1, _2, _3, _4, _5, _6, _7, _8)).

/**
* Set a square in a row.
*
* @arg Row The current game configuration.
* @arg Col The column index.
* @arg Square The new square in a row.
* @arg NewRow The new game configuration.
*/
set_col(row(_, _2, _3, _4, _5, _6, _7, _8), 1 , _1, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _, _3, _4, _5, _6, _7, _8), 2 , _2, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _, _4, _5, _6, _7, _8), 3 , _3, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _3, _, _5, _6, _7, _8), 4 , _4, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _3, _4, _, _6, _7, _8), 5 , _5, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _3, _4, _5, _, _7, _8), 6 , _6, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _3, _4, _5, _6, _, _8), 7 , _7, row(_1, _2, _3, _4, _5, _6, _7, _8)).
set_col(row(_1, _2, _3, _4, _5, _6, _7, _), 8 , _8, row(_1, _2, _3, _4, _5, _6, _7, _8)).


/**
* Set a square in a game configuration at a certain position.
*
* @arg Config The current game configuration.
* @arg Position containing row and column index
* @arg Square The new square (either nil or a piece).
* @arg NewConfig The new game configuration.
*/
set_square(Config, R/C, Square, NewConfig) :-
    get_board(Config, Board), 
    arg(R, Board, Row), 
    set_col(Row, C, Square, NewRow), 
    set_row(Board, R, NewRow, NewBoard), 
    set_board(Config, NewBoard, NewConfig).

/**
* Get a square in the game configuration.
*
* @arg Config The current game configuration.
* @arg Position containing row and column index
* @arg Square The new square in the game configuration.
*/
get_square(Config, R/C, Square) :- 
    get_board(Config, Board), 
    arg(R, Board, Row), 
    arg(C, Row, Square).
/**
* Check if the square on a certain position belongs to the current player.
*
* @arg Config The current game configuration.
* @arg Position The position to examine.
*/
is_mine(Config, R/C) :- get_turn(Config, Color), get_square(Config, R/C, piece(Color, _)).

/**
* Check if a square on a certain position is empty (contains nil).
* @arg Config The curreng game configuration.
* @arg Position The position to examine.
*/
is_empty(Config, R/C) :- get_square(Config, R/C, nil).

/**
* The oposite color.
*
* @arg Color The color.
* @arg OpositeColor The oposite color.
*/
other_player(b, w).
other_player(w, b).

/**
* Update the fullcount after black has played a move.
* fullcount = fullcount + 1
*
* @arg Config The game configuration without the fullcount updated.
* @arg NewConfig The configuration with the fullcount updated.
*/
update_full_count(Config, Config) :- get_turn(Config, w).
update_full_count(Config, NewConfig) :-
    get_turn(Config, b), 
    get_full_count(Config, Count), 
    NewCount is Count + 1, 
    set_full_count(Config, NewCount, NewConfig).
/**
* Rest the half count to 0.
*
* @arg Config The current game state.
* @arg NewConfig The game state with the halfcount set to 0.
*/
reset_half_count(Config, NewConfig) :- set_half_count(Config, 0, NewConfig).

/**
* Update the half count.
*
* @arg Config The current game configuration.
* @arg NewConfig The next game configuration.
*/
update_half_count(Config, NewConfig) :-
    get_half_count(Config, Count), 
    NewCount is Count + 1, 
    set_half_count(Config, NewCount, NewConfig).

/**
* Smart update of the halfcount.
* halfcount = halfcount +1 if no capture or pawn move.
* otherwise halfcount = 0
* @arg Config The old configuration before the move.
* @arg Square1 Old square.
* @arg Square2 New Square.
* @arg NewConfig The configuration with the half count intelligently updated.
*/
update_half_count(Config, Square, nil, NewConfig) :-
    Square \= piece(_, pawn), !, 
    update_half_count(Config, NewConfig).

update_half_count(Config, _, _, NewConfig) :-
    reset_half_count(Config, NewConfig).

/**
* Valid coordinates in a chess game.
* Can be used to check or generate.
* R in 1..8
* C in 1..8
*
* @arg Pos The coordinate to check or generate.
*/
all_coordinates(R/C) :- between(1, 8, R), between(1, 8, C).

/**
* Add to coordinates
* @arg Coordinate1 The first coordinate.
* @arg Coordinate2 The second coordinate.
* @arg NewCoordinate The addition of the 2 coordinates.
*/
add_positions(R1/C1, R2/C2, R3/C3) :- R3 is R1 + R2, C3 is C1 + C2.

% vim: set sw=4 ts=4 et :
