:- module(chess_rules, [options/2, options_no_check/2, dir/3, is_not_check/2]).
:- use_module(chess_operations).
:- use_module(chess_debug).


/**
* Generate the next possible configurations and also check if the king is in check.
* @arg Config The current configuration.
* @arg NewConfig The next configuration.
*/
options(Config, NewConfig) :-
    get_turn(Config, Color), 
    options_no_check(Config, NewConfig), 
    is_not_check(NewConfig, Color).

/**
* Generate the next possible congfigurations but do not check if the king is in check.
*
* @arg Config The current configuration.
* @arg NewConfig The next configuration.
*/
options_no_check(Config, NewConfig) :-
    get_turn(Config, Color), % Get the current turn.
    other_player(Color, Color2), % Get the other player.
    get_square(Config, Pos1, piece(Color, T)), % Iterate over all squares of the current player.
    update_board(Config, Pos1, piece(Color, T), Pos2, Temp0), % update the board.
    get_square(Config, Pos2, Square2), % Get the square of the updated position.
    update_castle(Temp0, Temp3), % Update the castling options
    update_half_count(Temp3, piece(Color, T), Square2, Temp4), % update the halfcount intellently.
    update_full_count(Temp4, Temp5), % Update the full count.
    set_turn(Temp5, Color2, NewConfig). % switch the turns.

/**
* Move a piece to a certain valid position.
*
* @arg Config The current configuration.
* @arg Pos1 The position of the current piece.
* @arg Pos2 The position the piece should move to.
* @arg Piece The piece on Pos1.
* @arg NewConfig The new configuration with the piece moved.
*/
move(Config, Pos1, Pos2, Piece, NewConfig) :-
    all_coordinates(Pos2), 
    \+ is_mine(Config, Pos2), 
    set_square(Config, Pos1, nil, Temp), 
    set_square(Temp, Pos2, Piece, NewConfig).

dir(knight, Pos, 1) :- member(Pos, [(-2)/(-1), (-1)/(-2), 1/(-2), 2/(-1), (-2)/1, (-1)/2, 1/2, 2/1]).
dir(bishop, Pos, 8) :- member(Pos, [(-1)/1, 1/1, (-1)/(-1), 1/(-1)]).
dir(rook, Pos, 8) :- member(Pos, [0/1, 1/0, 0/(-1), (-1)/0]).
dir(queen, Pos, 8) :- dir(bishop, Pos, _); dir(rook, Pos, _).
dir(king, Pos, 1) :- dir(queen, Pos, _).

/**
* Generate all possible moves with there new configuration per piece.
*
* @arg Config The current configuration
* @arg Pos1 The coordinate of the piece to move.
* @arg Piece The piece on the coordinate.
* @arg Pos2 The position that was moved to.
* @arg Config2 The new configuration with the piece moved.
*/

% update pawn movement.
update_board(Config, Pos1, piece(C, pawn), Pos2, Config2) :-
    !, 
    (pawn(w, 2, 8, 1/0, [1/1, 1/(-1)], Config, Pos1, C, Pos2, Config2)
    ;
    pawn(b, 7, 1, (-1)/0, [(-1)/1, (-1)/(-1)], Config, Pos1, C, Pos2, Config2)).

% update king movement.
update_board(Config, Pos1, piece(C, king), Pos2, Config2) :-
    castle_options(Config, Pos1, C, Pos2, Temp), % castling.
    set_enpassant(Temp, nil, Config2).

% update bishop rook and queen movement.
update_board(Config, Pos1, piece(C, Piece), Pos3, NewConfig) :-
    dir(Piece, Dir, Range), 
    add_positions(Pos1, Dir, Pos2),
    all_coordinates(Pos2),
    movement(Config, Dir, Range , Pos2, Options), 
    member(Pos3, Options), 
    move(Config, Pos1, Pos3, piece(C, Piece), Temp), 
    set_enpassant(Temp, nil, NewConfig).

movement(_, _, 0, _, []) :- !.
movement(Config, _, _, Pos, [Pos]) :- \+ is_empty(Config, Pos), !.

movement(Config, Dir, Range, Pos1, [ Pos1 | Moves1]) :-
    add_positions(Pos1, Dir, Pos2), 
    NewRange is Range - 1, 
    movement(Config, Dir, NewRange, Pos2, Moves1).
/**
* Generate all possible moves for a pawn in an overengineered fashion :).
*
* Note every pawn rule has a different prolog rule!
* The pawn rules are 
* 1) normall
* 2) attack
* 3) skip row
* 4) enpassant
*
* @arg Color The color of the pawn for which these rules apply.
* @arg BaseRow The starting row of the pawn.
* @arg PromoteRow The row at which the pawn promotes.
* @arg MoveDir Direction the pawn moves in.
* @arg TakeDirs List of directions the pawn can capture.
* @arg Config The current configuration
* @arg Pos1 The current position of the pawn.
* @arg Color The color of the pawn.
* @arg Pos2 The new position of the pawn.
* @arg Config2 The new game configuration with the pawn moved.
*/

% Pawn normall movement.
pawn(Color, _, PromoteRow, MoveDir, _ , Config, Pos1, Color, Pos2, Config2) :-
    add_positions(Pos1, MoveDir, Pos2), 
    is_empty(Config, Pos2), 
    promote(PromoteRow, Pos2, Type), 
    move(Config, Pos1, Pos2, piece(Color, Type), Temp), 
    set_enpassant(Temp, nil, Config2).

% Pawn attack movement.
pawn(Color, _, PromoteRow, _ , TakeDirs , Config, Pos1, Color, Pos2, Config2) :-
    member(Dir, TakeDirs), 
    add_positions(Pos1, Dir, Pos2), 
    \+ is_empty(Config, Pos2), 
    promote(PromoteRow, Pos2, Type), 
    move(Config, Pos1, Pos2, piece(Color, Type), Temp), 
    set_enpassant(Temp, nil, Config2).

% Pawn skip row at base position.
pawn(Color, BaseRow, _, MoveDir , _ , Config, BaseRow/C, Color, Pos3, Config2) :-
    add_positions(BaseRow/C, MoveDir, Pos2), 
    is_empty(Config, Pos2), 
    add_positions(Pos2, MoveDir, Pos3), 
    is_empty(Config, Pos3), 
    move(Config, BaseRow/C, Pos3, piece(Color, pawn), Temp), 
    set_enpassant(Temp, Pos2, Config2).

% Pawn enpassant.
pawn(Color, _, _, MoveDir , TakeDirs , Config, Pos1, Color, EnpassantPos, Config2) :-
    get_enpassant(Config, EnpassantPos), 
    member(Dir, TakeDirs), 
    add_positions(Pos1, Dir, EnpassantPos), 
    is_empty(Config, EnpassantPos), 
    reverseDir(MoveDir, OtherMoveDir), 
    add_positions(EnpassantPos, OtherMoveDir, SlayPawnPos), 
    move(Config, Pos1, EnpassantPos, piece(Color, pawn), Temp1), 
    set_square(Temp1, SlayPawnPos, nil, Temp2), 
    set_enpassant(Temp2, nil, Config2).

/**
* promote a piece to a knight, bishop, rook or queen if the piece has reached it's promotion row.
* 
* @arg PromoteRow The row on which to promote a piece.
* @arg Position The position of the piece.
* @arg Type The type of piece to which the pawn can promote.
*/
promote(PromoteRow, Row/_, pawn) :- PromoteRow \= Row.
promote(Row, Row/_, Piece) :- member(Piece, [knight, bishop, rook, queen]).

/**
* Get the castle opstions for a given color.
*
* @arg Color The player color.
* @arg Config The game configuration.
* @arg Castle The castle options for the given color.
*/
get_current_castle(w, Config, (WK, WQ)) :- get_castle(Config, castle(WK, WQ, _, _)).
get_current_castle(b, Config, (BK, BQ)) :- get_castle(Config, castle(_, _, BK, BQ)).

/**
* Generate the castle opstions for the 2 colors.
* @arg Config The current configuration
* @arg Pos1 The position of the current players king.
* @arg Color The color of the current configuration.
* @arg Pos2 The new Position of the king.
* @arg Config2 The new Configuration with the king and rook moved.
*/
castle_options(Config, Pos1, Color, Pos2, Config2) :-
    get_current_castle(Color, Config, CastleOptions), 
    (
        castle_options([6, 7], [5, 6], 5, 7, 8, 6, (true, _ ), 
                       Config, Pos1, piece(Color, king), CastleOptions, Pos2, Config2)
    ;
        castle_options([2, 3, 4], [5, 4], 5, 3, 1, 4, ( _, true ), 
                       Config, Pos1, piece(Color, king), CastleOptions, Pos2, Config2)
    ).

castle_options(Empty, NotAttacked, OldK, NewK, OldR, NewR, Sides, 
               Config, R/OldK, piece(C, king), Sides, R/NewK, NewConfig) :-
    forall(member(C2, Empty), is_empty(Config, R/C2)), 
    set_square(Config, R/OldK, nil, Temp1), 
    forall(
        (
            member(C2, NotAttacked), 
            other_player(C, OtherColor), 
            set_square(Config, R/C2, piece(king, C), T1), 
            set_turn(T1, OtherColor, Temp), 
            options_no_check(Temp, Temp2)
        ), 
        get_square(Temp2, R/C2, piece(king, C))
    ), 
    set_square(Temp1, R/OldR, nil, Temp2), 
    set_square(Temp2, R/NewK, piece(C, king), Temp3), 
    set_square(Temp3, R/NewR, piece(C, rook), NewConfig).

/**
* Reverse the signs of a coordinate.
*
* @arg Coordinate The initial coordinate.
* @arg ReversedCoordinate The coordinate with the signs flipped.
*/
reverseDir(R/C, R2/C2) :- R2 is R * (-1), C2 is C * (-1).


% THIS IS JUST MOVED IN TEST MODE, because this is really slow and in a real engine you should never test this explicitly.
is_not_check(Config, Color) :- pos_not_attacked(Config, _, piece(Color, king)).

/**
* Check if the piece on the given position is not attacked.
*
* @arg Config The current configuration.
* @arg The current position.
* @arg The piece on the position.
*/
pos_not_attacked(Config, Pos, Piece) :-
    forall(options_no_check(Config, NewConfig), get_square(NewConfig, Pos, Piece)).

/**
* Update the castle options.
*
* @arg Config The current configuration.
* @arg NewConfig The configuration with the castle options updated.
*/
update_castle(Config, NewConfig) :-
    get_castle(Config, castle(A1, B1, C1, D1)), 
    check_castling(Config, 1/8, 1/5, w, A1, A2), 
    check_castling(Config, 1/1, 1/5, w, B1, B2), 
    check_castling(Config, 8/8, 8/5, b, C1, C2), 
    check_castling(Config, 8/1, 8/5, b, D1, D2), 
    set_castle(Config, castle(A2, B2, C2, D2), NewConfig).

/**
* Check if castling is still possible.
* Check if rook and king ar moved and if castling was possible in the previous configuration.
*
* @arg Config The previous configuration with the piece already moved.
* @arg RookPosition Position the rook should be in for castling to be valid.
* @arg KingPosition.Position the king should be in for castling to be valid.
* @arg Color The color for which we are currently checking a castling option.
* @arg OldCastle Old castling options (most be true for the new castlig options to be true).
* @arg NewCastle New options (true if castling is still possible else false).
*/
check_castling(Config, RookPosition, KingPosition, Color, true, true) :-
    get_square(Config, RookPosition, piece(Color, rook)), 
    get_square(Config, KingPosition, piece(Color, king)), !.

check_castling(_, _, _, _, _, false).

% vim: set sw=4 ts=4 et :
