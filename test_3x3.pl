:- consult('taquin_ida_star.pl').
:- consult('util/print.pl').

dim(3, 3).
heuristic(nilsson).

puzzle([1,2,3,4,5,0,7,8,6], trivial).

puzzle([0,1,3,4,2,5,7,8,6], easy).
puzzle([8,1,3,4,0,2,7,6,5], easy).

puzzle([4,3,8,2,0,1,6,5,7], hard).

puzzle([6,4,7,8,5,0,3,2,1], hardest).
puzzle([8,6,7,2,5,4,3,0,1], hardest).

% test the following query.
%?- puzzle(Puzzle, Difficulty), testPuzzle(Puzzle), nl.
:- puzzle(Puzzle, _), testPuzzle(Puzzle), nl. % example