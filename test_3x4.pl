:- consult('taquin_ida_star.pl').
:- consult('util/print.pl').

dim(3, 4).
heuristic(nilsson).

puzzle([1,2,3,4,5,10,6,7,0,9,11,8], example).

:- puzzle(Puzzle, _), testPuzzle(Puzzle), nl. % example
