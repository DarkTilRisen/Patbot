:- module(chess_engine, [engine/2]).
:- use_module(chess_alpha_beta).
:- use_module(chess_rules).
:- use_module(chess_operations).


/**
* Try to find the best next board with alpha-beta pruning.
*
* @arg Config The board configuration.
* @arg NewConfig The next board configuration.
*/

% force draw on the 150th move.
% I don't care if I can put my opponent in mate at this time.
engine(Config, 'DRAW') :- get_half_count(Config, L), L >= 149, !.

engine(Config, NewConfig) :-
    % Get the current Player and let the engine optimize moves for this color.
    alpha_beta(Config, NewConfig, _), ! . % Start alpha-beta pruning.

engine(_, 'DRAW').
% vim: set sw=4 ts=4 et :
