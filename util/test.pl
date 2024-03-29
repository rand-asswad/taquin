/* module for testing and displaying 'taquin' puzzle
 * uses:
 * - dim/2
 * - move/3
 * - solve/3, solve/4
 * - setNth0/4
 * provides:
 * - printState/1
 * - printSolution/2
 * - testPuzzle/3, testPuzzle/2, testPuzzle/1
 * - debugPuzzle/3
 */

:- dynamic dim/2, move/3, solve/3, solve/4, setNth0/4.

removeZero(S0, S) :- nth0(Ind, S0, 0), setNth0(Ind, ' ', S0, S).
printState(S0) :- removeZero(S0, S), printState0(S).
printState0(S) :- dim(3,3), format('~a  ~a  ~a\n~a  ~a  ~a\n~a  ~a  ~a\n', S).
printState0(S) :- dim(3,4), format('~a  ~a  ~a  ~a\n~a  ~a  ~a  ~a\n~a  ~a  ~a  ~a\n', S).
printState0(S) :- dim(4,4), format('~a  ~a  ~a  ~a\n~a  ~a  ~a  ~a\n~a  ~a  ~a  ~a\n~a  ~a  ~a  ~a\n', S).

printSolution(State, []) :- printState(State).
printSolution(Puzzle, [S|Path]) :-
    printState(Puzzle), % initial state
    move(Puzzle, S, M), writeln(M), % print move
    printSolution(S, Path). % print remaining

testPuzzle(Puzzle, Algorithm, H) :-
    solve(Puzzle, Solution, Algorithm, H),
    reverse(Solution, Path),
    printSolution(Puzzle, Path),
    length(Path, N),
    format('# of moves: ~d\n', N).

testPuzzle(Puzzle, Algorithm) :-
    solve(Puzzle, Solution, Algorithm),
    reverse(Solution, Path),
    printSolution(Puzzle, Path),
    length(Path, N),
    format('# of moves: ~d\n', N).

testPuzzle(Puzzle) :- testPuzzle(Puzzle, astar).

debugPuzzle(Puzzle, Algorithm, H) :-
    statistics(walltime, _),
    solve(Puzzle, Path, Algorithm, H),
    statistics(walltime, [_, ExecutionTime]),
    length(Path, N),
    write('Time elapsed: '), write(ExecutionTime), write(' ms'), nl,
    write('Path length: '), write(N), nl.