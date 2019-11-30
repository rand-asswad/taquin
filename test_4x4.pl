:- consult('taquin.pl').
:- consult('util/print.pl').

dim(4, 4).

puzzle([1,3,7,4,6,2,8,0,5,9,10,12,13,14,11,15], easy). % 10 moves
puzzle([1,3,7,4,5,6,2,12,0,10,8,15,13,9,14,11], amateur). % 20 moves
puzzle([3,5,8,11,2,1,4,0,14,7,10,12,6,13,9,15], hard). % 36 moves