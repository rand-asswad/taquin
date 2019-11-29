:- dynamic dim/2, solvePath/2.

printState(P) :- dim(3,3), format('~d  ~d  ~d\n~d  ~d  ~d\n~d  ~d  ~d\n', P).
printState(P) :- dim(3,4), format('~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n', P).
printState(P) :- dim(4,4), format('~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n~d  ~d  ~d  ~d\n', P).

:- consult(moves).

printSolution(Puzzle, []) :- printState(Puzzle).
printSolution(Puzzle, [M|Path]) :-
    printState(Puzzle), % initial state
    writeln(M), move(Puzzle, Next, M), % print move
    printSolution(Next, Path). % print remaining

testPuzzle(Puzzle) :-
    solvePath(Puzzle, Path),
    printSolution(Puzzle, Path),
    length(Path, N),
    format('# of moves: ~d', N).