#!/usr/bin/env swipl
:- initialization(main, main).
:- use_module(chess_io).
:- use_module(chess_operations).
:- use_module(chess_rules).
:- use_module(chess_engine).
:- use_module(chess_debug).

main(Args) :-
    length(Args, 6), 
    fen_io(Args, Game), 
    engine(Game, NewConfig), 
    fen_io(Next, NewConfig), 
    write(Next).

main(Args) :-
    length(Args, 7), 
    append(Arg, ['TEST'], Args), 
    fen_io(Arg, Game), 
    forall(
        (
            options(Game, NewConfig), 
            fen_io(FenString, NewConfig)
        ), (
            write(FenString), 
            nl
        )
    ).

% vim: set sw=4 ts=4 ft=prolog et :
