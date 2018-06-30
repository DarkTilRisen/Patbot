:- module(chess_evaluation, [evaluate/3]).
:- use_module(chess_operations).
:- use_module(chess_debug).

/**
* Calculate the value estimation of a bord.
*
* @arg Me The color of the player to optimize.
* @arg GameState The current state of the game.
* @arg Val The estimated value of the GameState.
*/
evaluate(Me, GameState, Val) :-
    findall(Val, 
        (
            get_square(GameState, Pos, piece(PieceColor, Type)), 
            value(Me, PieceColor, Type, Pos, Val)
        ), List1), 
    sum_list(List1, Val).

/**
* Calculate the value estimation of a piece in a certain position.
*
* @arg Me The color of the player to optimize.
* @arg Other The color of the piece to value.
* @arg Type The type of piece eg: pawn, knight...
* @arg Pos The current position of the piece.
* @arg Val The value for the piece in the current gamestate.
*/
value(Me, PieceColor, Type, Pos, Val) :-
    value(Type, Val1), 
    translate_table(Pos, PieceColor, Pos2), 
    position(Type, Pos2, Val2), 
    ProtoVal is Val1 + Val2, 
    sign(Me, PieceColor, ProtoVal, Val).

/**
* Calculate the sign of the value.
* The sign is non altered if the color equal to the color to optimize.
* The sign is switched if the color is equal to the other player.
* @arg Me The player to optimize.
* @arg PieceColor The color to whom this piece belongs.
* @arg CurrentVal The currentvalue
* @arg ModifiedVal The AlteredValue
*/
sign(Me, Me, Val, Val) :- !.
sign(_, _, Val, -(Val)).

/**
* Find the default score of a piece.
* This scoring system satisfies the following equation.
* bishop > knight > 3 * pawn
* bishop + knight = rook + 1.5 * pawn
* queen + pawn = 2 * rook
*
* @arg pieceType The type of the piece.
* @arg score The default score of the piece.
*/
value(pawn, 100).
value(knight, 320).
value(bishop, 330).
value(rook, 500).
value(queen, 900).
value(king, 20000).

/**
* Translate the tables who are defined in function of black pieces.
* @arg coordinate Original board coordinate.
* @arg color Color of the piece.
* @arg tranlatedCoordinate Translated boad coordinate.
*/
translate_table(R/C, b, R/C) :- !.
translate_table(R/C, w, R2/C) :- R2 is 9 - R.

% Pawn's placement scores.
position(pawn, R/C, Val) :-
  nth1(
    R, 
    [[0, 0, 0, 0, 0, 0, 0, 0], 
    [50, 50, 50, 50, 50, 50, 50, 50], 
    [10, 10, 20, 30, 30, 20, 10, 10], 
    [ 5, 5, 10, 25, 25, 10, 5, 5], 
    [ 0, 0, 0, 20, 20, 0, 0, 0], 
    [ 5, -5, -10, 0, 0, -10, -5, 5], 
    [ 5, 10, 10, -20, -20, 10, 10, 5], 
    [ 0, 0, 0, 0, 0, 0, 0, 0]], 
    Row
  ), 
  nth1(C, Row, Val), !.

% Knight's placement scores.
position(knight, R/C, Val) :-
  nth1(
    R, 
    [[-50, -40, -30, -30, -30, -30, -40, -50], 
    [ -40, -20, 0, 0, 0, 0, -20, -40], 
    [ -30, 0, 10, 15, 15, 10, 0, -30], 
    [ -30, 5, 15, 20, 20, 15, 5, -30], 
    [ -30, 0, 15, 20, 20, 15, 0, -30], 
    [ -30, 5, 10, 15, 15, 10, 5, -30], 
    [ -40, -20, 0, 5, 5, 0, -20, -40], 
    [ -50, -40, -30, -30, -30, -30, -40, -50]], 
    Row
  ), 
  nth1(C, Row, Val), !.

% Bishop's placement scores.
position(bishop, R/C, Val) :-
  nth1(
    R, 
    [[-20, -10, -10, -10, -10, -10, -10, -20], 
    [ -10, 0, 0, 0, 0, 0, 0, -10], 
    [ -10, 0, 5, 10, 10, 5, 0, -10], 
    [ -10, 5, 5, 10, 10, 5, 5, -10], 
    [ -10, 0, 10, 10, 10, 10, 0, -10], 
    [ -10, 10, 10, 10, 10, 10, 10, -10], 
    [ -10, 5, 0, 0, 0, 0, 5, -10], 
    [ -20, -10, -10, -10, -10, -10, -10, -20]], 
    Row
  ), 
  nth1(C, Row, Val), !.

% Rook's placement scores.
position(rook, R/C, Val) :-
  nth1(
    R, 
    [[0, 0, 0, 0, 0, 0, 0, 0], 
    [ 5, 10, 10, 10, 10, 10, 10, 5], 
    [-5, 0, 0, 0, 0, 0, 0, -5], 
    [-5, 0, 0, 0, 0, 0, 0, -5], 
    [-5, 0, 0, 0, 0, 0, 0, -5], 
    [-5, 0, 0, 0, 0, 0, 0, -5], 
    [-5, 0, 0, 0, 0, 0, 0, -5], 
    [ 0, 0, 0, 5, 5, 0, 0, 0]], 
    Row
  ), 
  nth1(C, Row, Val), !.

%Queen's placement scores.
position(queen, R/C, Val) :-
  nth1(
    R, 
    [[-20, -10, -10, -5, -5, -10, -10, -20], 
    [ -10, 0, 0, 0, 0, 0, 0, -10], 
    [ -10, 0, 5, 5, 5, 5, 0, -10], 
    [  -5, 0, 5, 5, 5, 5, 0, -5], 
    [   0, 0, 5, 5, 5, 5, 0, -5], 
    [ -10, 5, 5, 5, 5, 5, 0, -10], 
    [ -10, 0, 5, 0, 0, 0, 0, -10], 
    [ -20, -10, -10, -5, -5, -10, -10, -20]], 
    Row
  ), 
  nth1(C, Row, Val), !.

%King's placement scores.
position(king, R/C, Val) :-
  nth1(
    R, 
    [[-30, -40, -40, -50, -50, -40, -40, -30], 
    [ -30, -40, -40, -50, -50, -40, -40, -30], 
    [ -30, -40, -40, -50, -50, -40, -40, -30], 
    [ -30, -40, -40, -50, -50, -40, -40, -30], 
    [ -20, -30, -30, -40, -40, -30, -30, -20], 
    [ -10, -20, -20, -20, -20, -20, -20, -10], 
    [  20, 20, 0, 0, 0, 0, 20, 20], 
    [  20, 30, 10, 0, 0, 10, 30, 20]], 
    Row
  ), 
  nth1(C, Row, Val), !.

% vim: set sw=4 ts=4 et :
