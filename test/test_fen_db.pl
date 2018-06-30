:- module(test_fen_db, [ fens/1, fen/3 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              HARDCODED CASES HEYO            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fens(X) :- between(1, 20, X).

% just a normal random fen position
fen(ex, 1, 'rnbqkbnr/pp1ppppp/2p5/8/8/3P3N/PPP1PPPP/RNBQKB1R b KQkq - 0 2').

fen(sol, 1, 'r1bqkbnr/pp1ppppp/n1p5/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnb1kbnr/pp1ppppp/1qp5/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnb1kbnr/pp1ppppp/2p5/q7/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnb1kbnr/ppqppppp/2p5/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnbqkb1r/pp1ppppp/2p2n2/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnbqkb1r/pp1ppppp/2p4n/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 1 3').
fen(sol, 1, 'rnbqkbnr/1p1ppppp/2p5/p7/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq a6 0 3').
fen(sol, 1, 'rnbqkbnr/1p1ppppp/p1p5/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/p2ppppp/1pp5/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/p2ppppp/2p5/1p6/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq b6 0 3').
fen(sol, 1, 'rnbqkbnr/pp1p1ppp/2p1p3/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/pp1p1ppp/2p5/4p3/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq e6 0 3').
fen(sol, 1, 'rnbqkbnr/pp1pp1pp/2p2p2/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/pp1pp1pp/2p5/5p2/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq f6 0 3').
fen(sol, 1, 'rnbqkbnr/pp1ppp1p/2p3p1/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/pp1ppp1p/2p5/6p1/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq g6 0 3').
fen(sol, 1, 'rnbqkbnr/pp1pppp1/2p4p/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/pp1pppp1/2p5/7p/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq h6 0 3').
fen(sol, 1, 'rnbqkbnr/pp1ppppp/8/2p5/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').
fen(sol, 1, 'rnbqkbnr/pp2pppp/2p5/3p4/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq d6 0 3').
fen(sol, 1, 'rnbqkbnr/pp2pppp/2pp4/8/8/3P3N/PPP1PPPP/RNBQKB1R w KQkq - 0 3').

% queens cannot move because of checked position

fen(ex, 2, 'rnb1kbnr/ppppqppp/8/8/8/8/PPPPQPPP/RNB1KBNR w KQkq - 2 2').

fen(sol, 2, 'rnb1kbnr/ppppQppp/8/8/8/8/PPPP1PPP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/4Q3/8/8/8/PPPP1PPP/RNB1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/4Q3/8/8/PPPP1PPP/RNB1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/1P6/8/P1PPQPPP/RNB1KBNR b KQkq b3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/2P5/8/PP1PQPPP/RNB1KBNR b KQkq c3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/3P4/8/PPP1QPPP/RNB1KBNR b KQkq d3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/4Q3/8/PPPP1PPP/RNB1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/5P2/8/PPPPQ1PP/RNB1KBNR b KQkq f3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/6P1/8/PPPPQP1P/RNB1KBNR b KQkq g3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/7P/8/PPPPQPP1/RNB1KBNR b KQkq h3 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/1P6/P1PPQPPP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/2N5/PPPPQPPP/R1B1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/2P5/PP1PQPPP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/3P4/PPP1QPPP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/4Q3/PPPP1PPP/RNB1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/5N2/PPPPQPPP/RNB1KB1R b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/5P2/PPPPQ1PP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/6P1/PPPPQP1P/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/7N/PPPPQPPP/RNB1KB1R b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/7P/PPPPQPP1/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/8/PPPPQPPP/RNBK1BNR b kq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/N7/PPPPQPPP/R1B1KBNR b KQkq - 3 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/8/P7/1PPPQPPP/RNB1KBNR b KQkq - 0 2').
fen(sol, 2, 'rnb1kbnr/ppppqppp/8/8/P7/8/1PPPQPPP/RNB1KBNR b KQkq a3 0 2').

%black king pat
fen(ex, 3, '7k/5Q2/8/8/8/8/K7/8 b - - 10 50').

%black king stuck
fen(ex, 4, '7k/5Q2/8/8/8/8/K7/8 w - - 10 50').
fen(sol, 4, '4Q2k/8/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '5Q1k/8/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '6Qk/8/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/1Q6/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/2Q5/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/3Q4/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/4Q3/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/5Q2/8/8/8/1K6/8/8 b - - 11 50').
fen(sol, 4, '7k/5Q2/8/8/8/8/1K6/8 b - - 11 50').
fen(sol, 4, '7k/5Q2/8/8/8/8/8/1K6 b - - 11 50').
fen(sol, 4, '7k/5Q2/8/8/8/8/8/K7 b - - 11 50').
fen(sol, 4, '7k/5Q2/8/8/8/K7/8/8 b - - 11 50').
fen(sol, 4, '7k/6Q1/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/7Q/8/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/4Q3/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/5Q2/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/6Q1/8/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/3Q4/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/5Q2/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/7Q/8/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/2Q5/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/5Q2/8/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/8/1Q6/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/8/5Q2/K7/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/8/8/K4Q2/8 b - - 11 50').
fen(sol, 4, '7k/8/8/8/8/8/K7/5Q2 b - - 11 50').
fen(sol, 4, '7k/Q7/8/8/8/8/K7/8 b - - 11 50').

%black can castle kingside
fen(ex, 5, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 5, 'r1bqk2r/ppp1ppbp/2np1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'r1bqk2r/ppp1ppbp/n2p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'r1bqk2r/pppnppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rn1qk2r/ppp1ppbp/3p1np1/5b2/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rn1qk2r/ppp1ppbp/3p1np1/8/2PPP1b1/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rn1qk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N1b/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rn1qk2r/ppp1ppbp/3pbnp1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rn1qk2r/pppbppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnb1k2r/pppqppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbq1k1r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQ - 1 6').
fen(sol, 5, 'rnbq1rk1/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQ - 1 6').
fen(sol, 5, 'rnbq3r/pppkppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQ - 1 6').
fen(sol, 5, 'rnbqk1nr/ppp1ppbp/3p2p1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqk1r1/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQq - 1 6').
fen(sol, 5, 'rnbqk2r/1pp1ppbp/3p1np1/p7/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq a6 0 6').
fen(sol, 5, 'rnbqk2r/1pp1ppbp/p2p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/p1p1ppbp/1p1p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/p1p1ppbp/3p1np1/1p6/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq b6 0 6').
fen(sol, 5, 'rnbqk2r/pp2ppbp/2pp1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/pp2ppbp/3p1np1/2p5/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq c6 0 6').
fen(sol, 5, 'rnbqk2r/ppp1pp1p/3p1npb/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqk2r/ppp1ppb1/3p1np1/7p/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq h6 0 6').
fen(sol, 5, 'rnbqk2r/ppp1ppb1/3p1npp/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/3p1n2/6p1/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/3p2p1/3n4/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/3p2p1/7n/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/3p2p1/8/2PPP1n1/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/3p2p1/8/2PPn3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/ppp1ppbp/5np1/3p4/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/ppp2pbp/3p1np1/4p3/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq e6 0 6').
fen(sol, 5, 'rnbqk2r/ppp2pbp/3ppnp1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 6').
fen(sol, 5, 'rnbqk2r/pppnppbp/3p2p1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqkb1r/ppp1pp1p/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 1 6').
fen(sol, 5, 'rnbqkr2/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQq - 1 6').

% white can castle kingside.
fen(ex, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK2R w KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/1N6/2PPP3/5N2/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/2P5/3PP3/2N2N2/PP2BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/3N4/2PPP3/5N2/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/3P4/2P1P3/2N2N2/PP2BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/4N3/2PPP3/2N5/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/4P3/2PP4/2N2N2/PP2BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/6B1/2PPP3/2N2N2/PP2BPPP/R2QK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/6N1/2PPP3/2N5/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/1PPPP3/2N2N2/P3BPPP/R1BQK2R b KQkq b3 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP1P1/2N2N2/PP2BP1P/R1BQK2R b KQkq g3 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP2N/2N5/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP2P/2N2N2/PP2BPP1/R1BQK2R b KQkq h3 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/1PN2N2/P3BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/1QN2N2/PP2BPPP/R1B1K2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N1BN2/PP2BPPP/R2QK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N1P/PP2BPP1/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP1BBPPP/R2QK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP1KBPPP/R1BQ3R b kq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP1QBPPP/R1B1K2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/1RBQK2R b Kkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQ1K1R b kq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQ1RK1 b kq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQK1R1 b Qkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP2BPPP/R1BQKR2 b Qkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PP3PPP/R1BQKB1R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2N2/PPQ1BPPP/R1B1K2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N2NP1/PP2BP1P/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N5/PP1NBPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N5/PP2BPPP/R1BQK1NR b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2NB1N2/PP3PPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2NQ1N2/PP2BPPP/R1B1K2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/5N2/PP2BPPP/RNBQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/P1N2N2/1P2BPPP/R1BQK2R b KQkq - 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/2PPPB2/2N2N2/PP2BPPP/R2QK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/N1PPP3/5N2/PP2BPPP/R1BQK2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/P1PPP3/2N2N2/1P2BPPP/R1BQK2R b KQkq a3 0 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1np1/8/Q1PPP3/2N2N2/PP2BPPP/R1B1K2R b KQkq - 1 5').
fen(sol, 6, 'rnbqk2r/ppp1ppbp/3p1npB/8/2PPP3/2N2N2/PP2BPPP/R2QK2R b KQkq - 1 5').

% black check
fen(ex, 7, 'rnbqk2r/ppp1ppbp/3p1Np1/8/2PPP3/2N5/PP2BPPP/R1BQK2R b KQkq - 4 10').
fen(sol, 7, 'rnbq1k1r/ppp1ppbp/3p1Np1/8/2PPP3/2N5/PP2BPPP/R1BQK2R w KQ - 5 11').
fen(sol, 7, 'rnbqk2r/ppp1pp1p/3p1bp1/8/2PPP3/2N5/PP2BPPP/R1BQK2R w KQkq - 0 11').
fen(sol, 7, 'rnbqk2r/ppp2pbp/3p1pp1/8/2PPP3/2N5/PP2BPPP/R1BQK2R w KQkq - 0 11').


% white can't castle because of check on path.
fen(ex, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP2BPbP/R1BQK2R w KQkq - 1 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/1N6/2PPP3/8/PP2BPbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/2P5/3PP3/2N5/PP2BPbP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/3N4/2PPP3/8/PP2BPbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/3P4/2P1P3/2N5/PP2BPbP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/4P3/2PP4/2N5/PP2BPbP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/6B1/2PPP3/2N5/PP2BPbP/R2QK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/7B/2PPP3/2N5/PP3PbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/1PPPP3/2N5/P3BPbP/R1BQK2R b KQkq b3 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP1B1/2N5/PP3PbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP2P/2N5/PP2BPb1/R1BQK2R b KQkq h3 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/1PN5/P3BPbP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/1QN5/PP2BPbP/R1B1K2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N1B3/PP2BPbP/R2QK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N2B2/PP3PbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N2P2/PP2B1bP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N4P/PP2BPb1/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP1BBPbP/R2QK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP1KBPbP/R1BQ3R b kq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP1QBPbP/R1B1K2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP2BPbP/1RBQK2R b Kkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP2BPbP/R1BQK1R1 b Qkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP2BPbP/R1BQKR2 b Qkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PP3PbP/R1BQKB1R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2N5/PPQ1BPbP/R1B1K2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2NB4/PP3PbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/2NQ4/PP2BPbP/R1B1K2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/8/PP2BPbP/RNBQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPP3/P1N5/1P2BPbP/R1BQK2R b KQkq - 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPPB2/2N5/PP2BPbP/R2QK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/2PPPP2/2N5/PP2B1bP/R1BQK2R b KQkq f3 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/N1PPP3/8/PP2BPbP/R1BQK2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/P1PPP3/2N5/1P2BPbP/R1BQK2R b KQkq a3 0 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6p1/8/Q1PPP3/2N5/PP2BPbP/R1B1K2R b KQkq - 2 10').
fen(sol, 8, 'rn1qk2r/ppp1pp1p/6pB/8/2PPP3/2N5/PP2BPbP/R2QK2R b KQkq - 2 10').

%enpassant possible for white
fen(ex, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 2 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/1B2Pp2/8/8/PPPP1PPP/RNBQK1NR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp1Q/8/8/PPPP1PPP/RNB1KBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/1P6/8/P1PP1PPP/RNBQKBNR b KQkq b3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/2B5/8/PPPP1PPP/RNBQK1NR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/2P5/8/PP1P1PPP/RNBQKBNR b KQkq c3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/3P4/8/PPP2PPP/RNBQKBNR b KQkq d3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/5P2/8/PPPP2PP/RNBQKBNR b KQkq f3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/6P1/8/PPPP1P1P/RNBQKBNR b KQkq g3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/6Q1/8/PPPP1PPP/RNB1KBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/7P/8/PPPP1PP1/RNBQKBNR b KQkq h3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/1P6/P1PP1PPP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/2N5/PPPP1PPP/R1BQKBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/2P5/PP1P1PPP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/3B4/PPPP1PPP/RNBQK1NR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/3P4/PPP2PPP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/5N2/PPPP1PPP/RNBQKB1R b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/5P2/PPPP2PP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/5Q2/PPPP1PPP/RNB1KBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/6P1/PPPP1P1P/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/7N/PPPP1PPP/RNBQKB1R b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/7P/PPPP1PP1/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/8/PPPPBPPP/RNBQK1NR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/8/PPPPNPPP/RNBQKB1R b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/8/PPPPQPPP/RNB1KBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/N7/PPPP1PPP/R1BQKBNR b KQkq - 3 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/8/P7/1PPP1PPP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4p3/4Pp2/P7/8/1PPP1PPP/RNBQKBNR b KQkq a3 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/4pP2/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 10').
fen(sol, 9, 'rnbqkbnr/pppp2pp/B3p3/4Pp2/8/8/PPPP1PPP/RNBQK1NR b KQkq - 3 10').

%white castle queen side
fen(ex, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 10').

fen(sol, 10, 'r3kbnr/pp2pppp/8/8/1P6/8/P3PPPP/R3KBNR b KQkq b3 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/4P3/8/PP3PPP/R3KBNR b KQkq e3 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/5P2/8/PP2P1PP/R3KBNR b KQkq f3 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/6P1/8/PP2PP1P/R3KBNR b KQkq g3 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/7P/8/PP2PPP1/R3KBNR b KQkq h3 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/1P6/P3PPPP/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/4P3/PP3PPP/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/5N2/PP2PPPP/R3KB1R b KQkq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/5P2/PP2P1PP/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/6P1/PP2PP1P/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/7N/PP2PPPP/R3KB1R b KQkq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/7P/PP2PPP1/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP1KPPPP/R4BNR b kq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/1R2KBNR b Kkq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/2KR1BNR b kq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/2R1KBNR b Kkq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/3RKBNR b Kkq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/R2K1BNR b kq - 1 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/8/P7/1P2PPPP/R3KBNR b KQkq - 0 10').
fen(sol, 10, 'r3kbnr/pp2pppp/8/8/P7/8/1P2PPPP/R3KBNR b KQkq a3 0 10').

% black castle queen side.
fen(ex, 11, 'r3kbnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR b KQkq - 0 10').

fen(sol, 11, '1r2kbnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQk - 1 11').
fen(sol, 11, '2kr1bnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQ - 1 11').
fen(sol, 11, '2r1kbnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQk - 1 11').
fen(sol, 11, '3rkbnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQk - 1 11').
fen(sol, 11, 'r2k1bnr/pp2pppp/8/8/8/8/PP2PPPP/R3KBNR w KQ - 1 11').
fen(sol, 11, 'r3kb1r/pp2pppp/5n2/8/8/8/PP2PPPP/R3KBNR w KQkq - 1 11').
fen(sol, 11, 'r3kb1r/pp2pppp/7n/8/8/8/PP2PPPP/R3KBNR w KQkq - 1 11').
fen(sol, 11, 'r3kbnr/1p2pppp/8/p7/8/8/PP2PPPP/R3KBNR w KQkq a6 0 11').
fen(sol, 11, 'r3kbnr/1p2pppp/p7/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/p3pppp/1p6/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/p3pppp/8/1p6/8/8/PP2PPPP/R3KBNR w KQkq b6 0 11').
fen(sol, 11, 'r3kbnr/pp2p1pp/5p2/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/pp2p1pp/8/5p2/8/8/PP2PPPP/R3KBNR w KQkq f6 0 11').
fen(sol, 11, 'r3kbnr/pp2pp1p/6p1/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/pp2pp1p/8/6p1/8/8/PP2PPPP/R3KBNR w KQkq g6 0 11').
fen(sol, 11, 'r3kbnr/pp2ppp1/7p/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/pp2ppp1/8/7p/8/8/PP2PPPP/R3KBNR w KQkq h6 0 11').
fen(sol, 11, 'r3kbnr/pp3ppp/4p3/8/8/8/PP2PPPP/R3KBNR w KQkq - 0 11').
fen(sol, 11, 'r3kbnr/pp3ppp/8/4p3/8/8/PP2PPPP/R3KBNR w KQkq e6 0 11').
fen(sol, 11, 'r4bnr/pp1kpppp/8/8/8/8/PP2PPPP/R3KBNR w KQ - 1 11').

% black mated.
fen(ex, 12, 'RR5K/8/8/k7/8/8/8/8 b - - 10 50').

% white mated
fen(ex, 13, 'rr5k/8/8/K7/8/8/8/8 w - - 10 50').

% white promotion
fen(ex, 14, '1k6/4PK2/8/8/8/8/8/8 w - - 15 50').
fen(sol, 14, '1k2B3/5K2/8/8/8/8/8/8 b - - 0 50').
fen(sol, 14, '1k2K3/4P3/8/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k2N3/5K2/8/8/8/8/8/8 b - - 0 50').
fen(sol, 14, '1k2Q3/5K2/8/8/8/8/8/8 b - - 0 50').
fen(sol, 14, '1k2R3/5K2/8/8/8/8/8/8 b - - 0 50').
fen(sol, 14, '1k3K2/4P3/8/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k4K1/4P3/8/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k6/4P1K1/8/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k6/4P3/4K3/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k6/4P3/5K2/8/8/8/8/8 b - - 16 50').
fen(sol, 14, '1k6/4P3/6K1/8/8/8/8/8 b - - 16 50').

% black promotion
fen(ex, 15, '8/8/8/8/8/8/4pk2/1K6 b - - 15 50').
fen(sol, 15, '8/8/8/8/8/4k3/4p3/1K6 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/5k2/4p3/1K6 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/6k1/4p3/1K6 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/8/4p1k1/1K6 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/8/4p3/1K2k3 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/8/4p3/1K3k2 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/8/4p3/1K4k1 w - - 16 51').
fen(sol, 15, '8/8/8/8/8/8/5k2/1K2b3 w - - 0 51').
fen(sol, 15, '8/8/8/8/8/8/5k2/1K2n3 w - - 0 51').
fen(sol, 15, '8/8/8/8/8/8/5k2/1K2q3 w - - 0 51').
fen(sol, 15, '8/8/8/8/8/8/5k2/1K2r3 w - - 0 51').

% black promote by capture.
fen(ex, 16, '8/8/8/8/8/4k3/4p3/1K1R1R2 b - - 10 50').
fen(sol, 16, '8/8/8/8/4k3/8/4p3/1K1R1R2 w - - 11 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1R1b2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1R1n2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1R1q2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1R1r2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1RbR2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1RnR2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1RqR2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1RrR2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1b1R2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1n1R2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1q1R2 w - - 0 51').
fen(sol, 16, '8/8/8/8/8/4k3/8/1K1r1R2 w - - 0 51').

% Black must sacrifice pawn.
fen(ex, 17, '8/8/8/8/8/4k3/4p3/1K1RRR2 b - - 10 50').
fen(sol, 17, '8/8/8/8/4k3/8/4p3/1K1RRR2 w - - 11 51').

% White promote by capture.

fen(ex, 18, '1k1r1r2/4P3/4K3/8/8/8/8/8 w - - 10 50').
fen(sol, 18, '1k1B1r2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1N1r2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1Q1r2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1R1r2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1r1B2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1r1N2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1r1Q2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1r1R2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1r1r2/4P3/8/4K3/8/8/8/8 b - - 11 50').
fen(sol, 18, '1k1rBr2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1rNr2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1rQr2/8/4K3/8/8/8/8/8 b - - 0 50').
fen(sol, 18, '1k1rRr2/8/4K3/8/8/8/8/8 b - - 0 50').

% White must sacrifice pawn.
fen(ex, 19, '1k1rrr2/4P3/4K3/8/8/8/8/8 w - - 10 50').
fen(sol, 19, '1k1rrr2/4P3/8/4K3/8/8/8/8 b - - 11 50').


% Random puzzle of the day (2018 June 22) on lichess.
fen(ex, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3Pq1/2RQK2R w K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppB/2n2n2/3p2B1/3P4/2N1PN1P/PP3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2B2/3p4/3P4/2NBPN1P/PP3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n1B/3p4/3P4/2NBPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/1B1p2B1/3P4/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/1N1p2B1/3P4/3BPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3N2B1/3P4/3BPN1P/PP3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p1BB1/3P4/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/1P1P4/2NBPN1P/P4Pq1/2RQK2R b K b3 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/2BP4/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P3N/2NBP2P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P3P/2NBPN2/PP3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/1PNBPN1P/P4Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/1QNBPN1P/PP3Pq1/2R1K2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2N1PN1P/PP2BPq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2N1PN1P/PP3Pq1/1BRQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2N1PN1P/PP3Pq1/2RQKB1R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2N1PN1P/PPB2Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBP2P/PP1N1Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBP2P/PP3Pq1/2RQK1NR b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBP2P/PP3PqN/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP1K1Pq1/2RQ3R b - - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP1Q1Pq1/2R1K2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP2KPq1/2RQ3R b - - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP2QPq1/2R1K2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3Pq1/1R1QK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3Pq1/2RQK1R1 b - - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3Pq1/2RQKR2 b - - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3Pq1/R2QK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PP3PqR/2RQK3 b - - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PPQ2Pq1/2R1K2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/2NBPN1P/PPR2Pq1/3QK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/3BPN1P/PP2NPq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/3BPN1P/PP3Pq1/1NRQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3P4/P1NBPN1P/1P3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3PB3/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3PN3/3BPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/3PP3/2NB1N1P/PP3Pq1/2RQK2R b K - 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/N2P4/3BPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/P2P4/2NBPN1P/1P3Pq1/2RQK2R b K a3 0 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p2B1/Q2P4/2NBPN1P/PP3Pq1/2R1K2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p4/3P1B2/2NBPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3p4/3P3B/2NBPN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2n2/3pN1B1/3P4/2NBP2P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/2n2nB1/3p2B1/3P4/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').
fen(sol, 20, 'r1b2rk1/ppp1bppp/B1n2n2/3p2B1/3P4/2N1PN1P/PP3Pq1/2RQK2R b K - 1 11').

% vim: set sw=4 ts=4 et :
