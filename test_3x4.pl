:- consult('taquin.pl').
:- consult('util/test.pl').

dim(3, 4).

puzzle([1,2,3,4,5,10,6,7,0,9,11,8], easy). % 5 moves
puzzle([10,1,4,11,5,9,0,6,7,3,8,2], medium). % 28 moves
puzzle([11,0,4,1,8,2,6,7,10,5,3,9], random). % (634 greedy)