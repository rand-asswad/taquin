/* module for calculating hamming distance
 * used predicate goal/1 that defines goal state
 */
:- dynamic goal/1.

% hamming/3 hamming distance between two states
hamming([], [], 0) :- !.
hamming([A|T1], [B|T2], N) :-
    dif(A, B), % A and B are different
    dif(A, 0), % exclude 0
    !, % prevent backtracking 
    hamming(T1, T2, M), % hamming distance for substates
    N is M + 1. % increment distance
hamming([_|T1], [_|T2], N) :- hamming(T1, T2, N). % no increment

% hamming/2: alias for calculating hamming/3 from goal
hamming(State, Dist) :- goal(Goal), hamming(State, Goal, Dist).