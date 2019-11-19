:- consult('8puzzle.pl').

puzzle([0,1,3,4,2,5,7,8,6]).
puzzle([8,1,3,4,0,2,7,6,5]).

:- forall(puzzle(P), (solve(P, Path), writeln(P), writeln(Path), nl)), halt.