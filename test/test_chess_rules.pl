:- begin_tests(chess_rules).
:- use_module('../src/chess_rules.pl').
:- use_module('../src/chess_operations.pl').
:- use_module('../src/chess_debug.pl').
:- use_module('../src/chess_io').
:- use_module(test_fen_db).
:- use_module(test_chess_util).

update_wk(true, Config, NewConfig) :- set_square(Config, 1/8, piece(w, rook), NewConfig).
update_wk(false, Config, Config).
update_wq(true, Config, NewConfig) :- set_square(Config, 1/1, piece(w, rook), NewConfig).
update_wq(false, Config, Config).
update_bk(true, Config, NewConfig) :- set_square(Config, 8/8, piece(b, rook), NewConfig).
update_bk(false, Config, Config).
update_bq(true, Config, NewConfig) :- set_square(Config, 8/1, piece(b, rook), NewConfig).
update_bq(false, Config, Config).

and(false, false, false). and(false, true, false).
and(true, false, false). and(true, true, true).

/**
* Test the updating of the castling options.
*/
test(update_castle, [forall((maplist(and, [ A, B, C, D ], List2, After)))]):-
    CastleBegin =.. [ castle | List2 ], 
    CastleAfter =.. [ castle | After ], 
    empty_config(Config), 
    set_castle(Config, CastleBegin, Config2), 
    set_square(Config2, 1/5, piece(w, king), Config2A), 
    set_square(Config2A, 8/5, piece(b, king), Config3), 
    update_wk(A, Config3, Config4), 
    update_wq(B, Config4, Config5), 
    update_bk(C, Config5, Config6), 
    update_bq(D, Config6, Config7), 
    chess_rules:update_castle(Config7, Config8), 
    get_castle(Config8, CastleAfter).
/**
* Test the promotion help function
*/
test(promote, [forall((member(Row, [1, 8]), all_coordinates(R/C)))]) :-
    findall(Type, chess_rules:promote(Row, R/C, Type), List), 
    (Row = R ->
        member(knight, List), 
        member(bishop, List), 
        member(rook, List), 
        member(queen, List), 
        \+ member(pawn, List), 
        \+ member(king, List)
    ;
        List = [ pawn ]
    ), !.

/**
* The folowing rules are simplified rules for the pieces
* used to test if the normall movement of the pieces is correct.
* To test edgecases we will do different hardcoded tests.
*/

pawn_pos(Conf, w, R1/C, R2/C) :- succ(R1, R2), chess_operations:is_empty(Conf, R2/C).
pawn_pos(Conf, b, R1/C, R2/C) :- succ(R2, R1), chess_operations:is_empty(Conf, R2/C).
pawn_pos(Conf, w, R1/C1, R2/C2) :- succ(R1, R2), succ(C1, C2), \+ chess_operations:is_empty(Conf, R2/C2).
pawn_pos(Conf, b, R1/C1, R2/C2) :- succ(R2, R1), succ(C1, C2), \+ chess_operations:is_empty(Conf, R2/C2).
pawn_pos(Conf, w, R1/C1, R2/C2) :- succ(R1, R2), succ(C2, C1), \+ chess_operations:is_empty(Conf, R2/C2).
pawn_pos(Conf, b, R1/C1, R2/C2) :- succ(R2, R1), succ(C2, C1), \+ chess_operations:is_empty(Conf, R2/C2).
pawn_pos(Conf, w, 2/C, 4/C) :- chess_operations:is_empty(Conf, 3/C), chess_operations:is_empty(Conf, 4/C).
pawn_pos(Conf, b, 7/C, 5/C) :- chess_operations:is_empty(Conf, 6/C), chess_operations:is_empty(Conf, 5/C).

knight_pos(_, _, Pos1, Pos2) :- 
    maplist(
        chess_operations:add_positions(Pos1), 
        [ 1/2, (-1)/2, 1/(-2), (-1)/(-2), 2/1, 2/(-1), (-2)/(1), (-2)/(-1)], 
        L1
    ), member(Pos2, L1).

king_pos(_, _, Pos1, Pos2) :- 
    maplist(
        chess_operations:add_positions(Pos1), 
        [ 1/0, 1/1, (-1)/0, (-1)/(-1), (-1)/1, 1/(-1), (0)/(1), (0)/(-1)], 
        L1
    ), member(Pos2, L1).

keep_moving(Conf, _, [ Pos ], Pos) :- \+ chess_operations:is_empty(Conf, Pos), ! .

keep_moving(Conf, DR/DC, [ R/C | History ], R/C) :-
    R2 is R + DR, C2 is C + DC, 
    keep_moving(Conf, DR/DC, History, R2/C2).

keep_moving_start(Conf, DR/DC, History, R/C) :-
    R2 is R + DR, C2 is C + DC, 
    keep_moving(Conf, DR/DC, History, R2/C2).

bishop_pos(Conf, _, Pos1, Pos2) :-
    (
        keep_moving_start(Conf, 1/1, Moves, Pos1);
        keep_moving_start(Conf, 1/(-1), Moves, Pos1);
        keep_moving_start(Conf, (-1)/1, Moves, Pos1);
        keep_moving_start(Conf, (-1)/(-1), Moves, Pos1)
    ), member(Pos2, Moves).

rook_pos(Conf, _, Pos1, Pos2) :-
    (
        keep_moving_start(Conf, 0/1, Moves, Pos1);
        keep_moving_start(Conf, 0/(-1), Moves, Pos1);
        keep_moving_start(Conf, (-1)/0, Moves, Pos1);
        keep_moving_start(Conf, 1/0, Moves, Pos1)
    ), member(Pos2, Moves).
queen_pos(Conf, C, Pos1, Pos2) :- rook_pos(Conf, C, Pos1, Pos2) ; bishop_pos(Conf, C, Pos1, Pos2).

goal_piece((Goal, Piece)) :- 
    member(
        (Goal, Piece), 
        [
            (pawn_pos, pawn), 
            (knight_pos, knight), 
            (bishop_pos, bishop), 
            (rook_pos, rook), 
            (queen_pos, queen), 
            (king_pos, king)
        ]
    ).

%%%%%%%%% RANDOM TESTS %%%%%%%%%%

/**
*
* Create an empty bord.
* Place a test piece on the bord.
* Fill the board with other pieces.
* Check if all the moves of the piece are valid.
*/
random_run(Validator, Type) :-
    empty_config(EmptyConfig), 
    random_position(Pos), random_color(C), 
    set_square(EmptyConfig, Pos, piece(C, Type), Config), 
    random(0, 20, Amount), % place a random amount of pieces on the board.
    random_pieces(Config, [w, b], [ pawn, knight, bishop, queen, king ], Amount, NewConfig), 
    findall(Pos2, call(Validator, NewConfig, C, Pos, Pos2), T), 
    filter_valid_me(T, NewConfig, Coordinates1), 
    findall(Pos2, chess_rules:update_board(NewConfig, Pos, piece(C, Type), Pos2, _), Coordinates2), 
    equal(Coordinates1, Coordinates2).

/**
* Generate 1000 random bords for each piece and check if their default movement is correct.
*/
test(update_board, [forall((between(0, 1000, _), goal_piece((Goal, Piece))))]) :- random_run(Goal, Piece).

%%%%%%%%%%% HARDCODED TESTS FOR EDGECASES ENPASSANT CHECK ETC %%%%%%%%%%%%%

/**
* Convert FenAtom to a board configuration.
*/
fenatom_to_board(FenAtom, Config) :-
    atomic_list_concat(Args, ' ', FenAtom), 
    chess_io:fen_io(Args, Config).

/**
* Check if the moves generated by the move generator equal the actual moves in the test database.
*/
test(options, [forall(fens(X))]) :-
    fen(ex, X, FenAtom), fenatom_to_board(FenAtom, Config), 
    findall(Option, chess_rules:options(Config, Option), Options1), 
    findall(Option, (fen(sol, X, Option)), Temp), 
    maplist(fenatom_to_board, Temp, Options2), 
    equal(Options1, Options2), !.



:- end_tests(chess_rules).

% vim: set sw=4 ts=4 et :
