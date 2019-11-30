:- dynamic dim/2, move/3, solve/3.

printState(P) :- dim(3,3), format('~d  ~d  ~d\n~d  ~d  ~d\n~d  ~d  ~d\n', P).
printState(P) :- dim(3,4), format('~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n', P).
printState(P) :- dim(4,4), format('~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n', P).

:- consult(moves).

printSolution(State, []) :- printState(State).
printSolution(Puzzle, [S|Path]) :-
    printState(Puzzle), % initial state
    move(Puzzle, S, M), writeln(M), % print move
    printSolution(S, Path). % print remaining

testPuzzle(Puzzle, Algorithm) :-
    solve(Puzzle, Solution, Algorithm),
    reverse(Solution, Path),
    printSolution(Puzzle, Path),
    length(Path, N),
    format('# of moves: ~d\n', N).

testPuzzle(Puzzle) :- testPuzzle(Puzzle, astar).