:- use_module(library(lists)).

/* goal(L) : the winning state */
goal([1,2,3,
      4,5,6,
      7,8,0]).


/* move(Before, After, Direction) :
 * After is the result of moving one tile
 * from the Before state
 * in the direction D
 */
/* moves.pl module
 * provides moves(Before, After, Direction)
 */

/* left(Before, After) :
 * After is the result of moving one tile left
 * from the Before state
 */
left([0,A,B, C,D,E, F,G,H], [A,0,B, C,D,E, F,G,H]).
left([A,0,B, C,D,E, F,G,H], [A,B,0, C,D,E, F,G,H]).
left([A,B,C, 0,D,E, F,G,H], [A,B,C, D,0,E, F,G,H]).
left([A,B,C, D,0,E, F,G,H], [A,B,C, D,E,0, F,G,H]).
left([A,B,C, D,E,F, 0,G,H], [A,B,C, D,E,F, G,0,H]).
left([A,B,C, D,E,F, G,0,H], [A,B,C, D,E,F, G,H,0]).

/* right(Before, After) :
 * After is the result of moving one tile right
 * from the Before state
 */
right(Before, After) :- left(After, Before).

/* up(Before, After) :
 * After is the result of moving one tile up
 * from the Before state
 */
up([0,A,B, C,D,E, F,G,H], [C,A,B, 0,D,E, F,G,H]).
up([A,0,B, C,D,E, F,G,H], [A,D,B, C,0,E, F,G,H]).
up([A,B,0, C,D,E, F,G,H], [A,B,E, C,D,0, F,G,H]).
up([A,B,C, 0,D,E, F,G,H], [A,B,C, F,D,E, 0,G,H]).
up([A,B,C, D,0,E, F,G,H], [A,B,C, D,G,E, F,0,H]).
up([A,B,C, D,E,0, F,G,H], [A,B,C, D,E,H, F,G,0]).

/* down(Before, After) :
 * After is the result of moving one tile down
 * from the Before state
 */
down(Before, After) :- up(After, Before).


/* move(Before, After, Direction) :
 * After is the result of moving one tile
 * from the Before state
 * in the direction D
 */
move(Before, After, left) :- left(Before, After).
move(Before, After, right) :- right(Before, After).
move(Before, After, up) :- up(Before, After).
move(Before, After, down) :- down(Before, After).


/* naive_solve(InitialPuzzle, Path) :
 * Path is the ordered directions to solve the puzzle
 * naive_solve(InitialPuzzle, Path, Visited) :
 * Visited stores the visited states to avoid inifinite loops
 * however this can loop a lot in order to find a path
 * 'naive' is in the name!
 */
naive_solve(Initial, Path) :- naive_solve(Initial, Path, []).

naive_solve(Initial, [], _) :- goal(Initial).
naive_solve(Initial, [Direction|Path], Visited) :-
      move(Initial, State, Direction),
      \+member(State, Visited), % State is not a member of Visited
      naive_solve(State, Path, [State|Visited]). % finds path from new state

/* manhattan(State1, State2, Dist)
 * the manhattan distance between two states
 */

/* the Manhattan distance */
%manhattan(State, State, 0).
%manhattan(S1, S2, 1) :- move(S1, S2, _).
%manhattan(S1, S2, N) :- move(S1, T, _), manhattan(T, S2, M), N is M + 1.

% manhattan distance between two indices
manh(Ind1, Ind2, D) :-
      arrayInd(Ind1, X1, Y1), arrayInd(Ind2, X2, Y2),
      diff(X1, X2, Dx), diff(Y1, Y2, Dy),
      D is Dx + Dy.
% return X and Y of a given 1D index (or vice versa)
arrayInd(Ind, X, Y) :- X is mod(Ind, 3), Y is div(Ind, 3).
% absolute difference of two numbers
diff(A, B, Diff) :- D is A - B, Diff is abs(D).

% pos(E, L, Ind)
% position (index) Ind of an element E in a list L
pos(E, [E|_], 0).
pos(E, [_|T], Ind) :- pos(E, T, I), Ind is I + 1.

% manhattan distance of the element at position Ind in L1 from L2
distInd(Ind, L1, L2, D) :- pos(E, L1, Ind), pos(E, L2, Ind2), manh(Ind, Ind2, D).

% cumulative manhattan distance of elements up to Ind
distCumul(0, L1, L2, D) :- distInd(0, L1, L2, D).
distCumul(Ind, L1, L2, D) :- distCumul(Prev, L1, L2, Dc), distInd(Ind, L1, L2, Di), Ind is Prev + 1, D is Di + Dc.

% manhattan distance between two lists
manhattan(L1, L2, D) :- distCumul(N, L1, L2, D), length(L1, M), M is N + 1.

manhattan(State, Dist) :- goal(Goal), manhattan(State, Goal, Dist).

/* hamming(State1, State2, Dist)
 * the hamming distance between two states
 */
/* the Hamming function
 * number of tiles in different positions
 */

hamming([], [], 0).
hamming([H|T1], [H|T2], N) :- hamming(T1, T2, N).
hamming([_|T1], [_|T2], N) :- hamming(T1, T2, M), N is M + 1.
hamming(State, Dist) :- goal(Goal), hamming(State, Goal, Dist).

/* heuristic function */
h(State, H) :- manhattan(State, Manh), hamming(State, Ham), H is Manh + 3 * Ham.

/* smart_move(Initial, State)
 * choose State that minimises heuristic function
 */
