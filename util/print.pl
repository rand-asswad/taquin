printState(P) :- format('~d  ~d  ~d\n~d  ~d  ~d\n~d  ~d  ~d\n', P).

:- consult(moves).

printSolution(Puzzle, []) :- printState(Puzzle).
printSolution(Puzzle, [M|Path]) :-
    printState(Puzzle), % initial state
    writeln(M), move(Puzzle, Next, M), % print move
    printSolution(Next, Path). % print remaining