/* module provides core predicates for 'taquin' puzzle
 * uses:
 * - dim/2: defines puzzle dimensions (NbRows, NbCols)
 * - 'utils/moves.pl': provides move/3
 * provides:
 * - goal/1: defines goal state
 * - randomPuzzle/2: creates random puzzle
 */
:- dynamic dim/2.
:- consult('util/moves.pl').
:- consult('util/heuristic.pl').

% goal/1 predicate defines goal state
% uses auxiliary predicates
% makeGoal/2, makeGoal/3 
goal(State) :-
    dim(Rows, Cols),
    N is Rows * Cols,
    makeGoal(N, State).

% makeGoal/2: alias for calling makeGoal/3
makeGoal(N, State) :- makeGoal(N, 1, State).
% makeGoal(N, Start, List)
% makes an incrementing list of size N
% start at Start
makeGoal(1, _, [0]) :- !. % stop condition
makeGoal(N, Count, [Count|T]) :-
    C is Count + 1, % increment C
    M is N - 1, % decrement Size
    makeGoal(M, C, T). % call sublist

% randomPuzzle/2
% creates a random puzzle at most N steps
% away fom goal state

% path/2 gets path from list of states
path([_], []) :- !.
path([A,B|T], [D|Path]) :- move(A, B, D), !, path([B|T], Path).