:- consult('taquin.pl').
:- consult('util/print.pl').

dim(3, 3).

puzzle([1,2,3,4,5,0,7,8,6], trivial). % 1 move

puzzle([0,1,3,4,2,5,7,8,6], easy). % 4 moves
puzzle([8,1,3,4,0,2,7,6,5], medium). % 14 moves

puzzle([4,3,8,2,0,1,6,5,7], hard). % 20 moves

puzzle([6,4,7,8,5,0,3,2,1], hardest). % 31 moves
puzzle([8,6,7,2,5,4,3,0,1], hardest). % 31 moves

% test using the following query.
%?- puzzle(Puzzle, Difficulty), testPuzzle(Puzzle, Algorithm).