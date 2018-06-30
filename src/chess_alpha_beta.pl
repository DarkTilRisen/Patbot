:- module(chess_alpha_beta, [alpha_beta/3]).
:- use_module(chess_evaluation).
:- use_module(chess_rules).
:- use_module(chess_operations).

/**
* Find the next move with alpha beta pruning.
*
* @arg Me The color of the player to optimize.
* @arg Config The current configuration.
* @arg BestConfig The best next configuration according to the engine.
*/
alpha_beta(Config, BestConfig, BestVal) :-
    get_turn(Config, Me), 
    bagof(NextConfig, options(Config, NextConfig), NextConfigList), 
    random_permutation(NextConfigList, RandomNextConfigList), 
    find_depth(RandomNextConfigList, Depth), 
    boundedbest(Me, Depth, RandomNextConfigList, -(inf), inf, BestConfig, BestVal), !.
/**
* Recursive alpha beta pruning entry point.
*
* @arg Me The color of the player to optimize.
* @arg Depth The depth decrement count.
* @arg Config The current configuration.
* @arg Alpha The Lower bound in alpha-beta pruning.
* @arg Beta The upper bound in alpha-beta pruning.
* @arg BestConfig The Best config the engine could find.
* @arg BestVal The estimated value of the best board.
*/
alpha_beta(Me, Depth, Config, Alpha, Beta, BestConfig, BestVal) :-
    succ(NewDepth, Depth), % this fails silently on -1 so we don't go below 0

    % don't seek past checkmates
    get_square(Config, _, piece(w, king)), get_square(Config, _, piece(b, king)), 
    % this may include positions where we put ourself in check but that's catched by the king's high value
    bagof(NextConfig, options_no_check(Config, NextConfig), NextConfigList), 
    % calculate best next move
    boundedbest(Me, NewDepth, NextConfigList, Alpha, Beta, BestConfig, BestVal), !.

alpha_beta(Me, _, Config, _, _, _, BestVal) :- evaluate(Me, Config, BestVal). %estimate the evaluation.

/**
* Recursively call alpha beta on a new depth and bound the game tree if possible.
*
* @arg Me The current player to optimize.
* @arg Depth The depth decrement count of the tree.
* @arg ConfigurationList The list of all configurations at the current depth.
* @arg Alpha The lower bound.
* @arg Beta The upper bound.
* @arg BestConfig The best config found from the subtrees.
* @arg Bestval The estimated value of the bestConfig.
*/
boundedbest(Me, Depth, [Config|ConfigList], Alpha, Beta, BestConfig, BestVal) :-
    alpha_beta(Me, Depth, Config, Alpha, Beta, _, Val), 
    goodenough(Me, Depth, ConfigList, Alpha, Beta, Config, Val, BestConfig, BestVal).

/**
* Check if bounding is possible or update the bounds en recursively call boundedbest. 
* 
* @arg Me The current player to optimize.
* @arg Depth The depth decrement count of the tree.
* @arg ConfigurationList The list of all configurations at the current depth.
* @arg Alpha The lower bound.
* @arg Beta The upper bound.
* @arg Val The value of the current node.
* @arg BestConfig The best config found from the subtrees.
* @arg Bestval The estimated value of the bestConfig.
*/
goodenough(_, _, [], _, _, Config, Val, Config, Val).
goodenough(Me, _, _, Alpha, Beta, Config, Val, Config, Val) :-
    \+ get_turn(Config, Me), Val > Beta, !
    ;
    get_turn(Config, Me), Val < Alpha, !.

goodenough(Me, Depth, ConfigList, Alpha, Beta, Config, Val, BestConfig, BestVal) :-
    newbounds(Me, Alpha, Beta, Config, Val, NewAlpha, NewBeta), 
    boundedbest(Me, Depth, ConfigList, NewAlpha, NewBeta, Config1, Val1), 
    betterOf(Me, Config, Val, Config1, Val1, BestConfig, BestVal).


/**
* Changes the bound according to the alpha-beta pruning scheme.
*
* @arg Me The color of the player to optimize.
* @arg Alpha The Lower bound in alpha-beta pruning.
* @arg Beta The upper bound in alpha-beta pruning.
* @arg Config The current configuration.
* @arg Val The current estimated board value.
* @arg NewAlpha The new alpha bound.
* @arg NewBeta The new beta bound.
*/
newbounds(Me, Alpha, Beta, Config, Val, Val, Beta) :- \+ get_turn(Config, Me), Val > Alpha, !.
newbounds(Me, Alpha, Beta, Config, Val, Alpha, Val) :- get_turn(Config, Me), Val < Beta, !.
newbounds(_, Alpha, Beta, _, _, Alpha, Beta).

/**
* Maximize or minimize the game configuration value according to which player has to move.
* 
* @arg Me The color of the player to optimize.
* @arg Config0 The first game configuration.
* @arg Val0 The value estimated for game configuration 0.
* @arg Config1 The second game configuration.
* @arg Val1 The value estimated for game configuration 1.
* @arg BestConfig The best game configuration in the current node in the game tree.
* @arg BestVal The best value for the player in the current node in the game tree.
*/
betterOf(Me, Config0, Val0, _, Val1, Config0, Val0) :-
    % If after the move we want to judge it's the other's player's turn. that means the move is
    % ours. we want to maximize value
    get_turn(Config0, Me), Val0 < Val1, !
    ;
    % the other player wants to maximize value
    \+ get_turn(Config0, Me), Val0 > Val1, !.

betterOf(_, _, _, Config1, Val1, Config1, Val1).

/**
* Estimate the depth according to the length of the root's children.
*
* @arg NextConfigList The list of the first configurations.
* @arg Depth The chosen depth.
*/
find_depth(NextConfigList, Depth) :- length(NextConfigList, L), depth(L, Depth).
depth(L, 3) :- L < 15, !.
depth(_, 2).

% vim: set sw=4 ts=4 et :
