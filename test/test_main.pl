#!/usr/bin/env swipl
:- initialization(main, main).

main(_) :-
    TestFiles = ['test_chess_io.pl', 'test_chess_rules.pl'], 
    consult(TestFiles), 
    load_test_files(TestFiles), 
    show_coverage(run_tests), 
    halt(0).
