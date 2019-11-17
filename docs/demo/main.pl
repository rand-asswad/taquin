
% import lists since tau-prolog is stupid
:- use_module(library(lists)).

% define dif/2
dif(X, Y) :- nonvar(X), nonvar(Y), X \== Y.

% program starts here
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
naive_solve(Initial, Path) :- naive_solve(Initial, [], [], Path).

naive_solve(Initial, _, Path, Path) :- goal(Initial).
naive_solve(Initial, Visited, P, Path) :-
      move(Initial, State, Direction),
      \+member(State, Visited), % State is not a member of Visited
      append(P, [Direction], PD),
      naive_solve(State, [State|Visited], PD, Path). % finds path from new state

/* manhattan(State1, State2, Dist)
 * the manhattan distance between two states
 */

/* the Manhattan distance */
%manhattan(State, State, 0).
%manhattan(S1, S2, 1) :- move(S1, S2, _).
%manhattan(S1, S2, N) :- move(S1, T, _), manhattan(T, S2, M), N is M + 1.

% manhattan distance between two indices
manh(Ind1, Ind2, D) :-
      arrayInd(Ind1, X1, Y1), arrayInd(Ind2, X2, Y2), % switch to coordinates
      diff(X1, X2, Dx), diff(Y1, Y2, Dy), % calculate coordinates difference
      D is Dx + Dy. % add differences

% return X and Y of a given 1D index (or vice versa)
arrayInd(Ind, X, Y) :- X is mod(Ind, 3), Y is Ind // 3.

% absolute difference of two numbers
diff(A, B, Diff) :- D is A - B, Diff is abs(D).

% pos(E, L, Ind)
% position (index) Ind of first occurence of an element E in a list L
pos(E, [E|_], 0).
pos(E, [_|T], Ind) :- pos(E, T, I), Ind is I + 1.

% manhattan distance of the element at position Ind in L1 from L2
distInd(Ind, L1, _, 0) :- pos(0, L1, Ind). % exclude zero
distInd(Ind, L1, L2, D) :-
      pos(E, L1, Ind), % fetch element at Ind from L1
      pos(E, L2, Ind2), % find its index in L2
      manh(Ind, Ind2, D). % calculate distance between the indices

% cumulative manhattan distance of elements up to Ind
distCumul(0, L1, L2, D) :- distInd(0, L1, L2, D).
distCumul(Ind, L1, L2, D) :-
      distCumul(Prev, L1, L2, Dc), % cumul dist up to previous index
      distInd(Ind, L1, L2, Di), % distance of current index
      Ind is Prev + 1, % define previous
      D is Di + Dc. % add distances

% manhattan distance between two lists
manhattan(L1, L2, D) :-
      distCumul(N, L1, L2, D), % calculate cumul distance up to length(L1)
      length(L1, M), % get length(L1)
      M is N + 1. % exclude last index since we start from 0

manhattan(State, Dist) :- goal(Goal), manhattan(State, Goal, Dist), !.

/* hamming(State1, State2, Dist)
 * the hamming distance between two states
 */
/* the Hamming function
 * number of tiles in wrong positions
 */

hamming([], [], 0).
hamming([H|T1], [H|T2], N) :- hamming(T1, T2, N).
hamming([0|T1], [_|T2], N) :- hamming(T1, T2, N). % exclude zero
hamming([A|T1], [B|T2], N) :- dif(A, B), hamming(T1, T2, M), N is M + 1.
hamming(State, Dist) :- goal(Goal), hamming(State, Goal, Dist), !.

/* heuristic functions */
h(State, H, manhattan) :- manhattan(State, H).
h(State, H, hamming) :- hamming(State, H).
h(State, H, nilsson) :- manhattan(State, Manh), hamming(State, Ham), H is Manh + 3 * Ham.

% choose default.
h(State, H) :- h(State, H, hamming).

/* smart_move(Initial, State)
 * choose State that minimises heuristic function
 */

neighborCost(State1, State2, Cost) :- move(State1, State2, _), h(State2, Cost).

neighborStates(State, Neighbors) :- findall(After, move(State, After, _), Neighbors).

%cheapest(List, State)
cheapest([State], State).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac =< Bc, cheapest([A|T], M).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac > Bc, cheapest([B|T], M).

%bestNeighbor(State, Neighbor)
bestNeighbor(State, Neighbor) :- neighborStates(State, List), cheapest(List, Neighbor).

%bestMove(State, Neighbor, Visited)
bestMove(State, Next, Visited) :-
      findall(Neighbor, (move(State, Neighbor, _), \+member(Neighbor, Visited)), Neighbors),
      cheapest(Neighbors, Next).

% stores path
solve(Initial, Path) :- solve(Initial, [], [], Path), !.
solve(Initial, _, States, States) :- goal(Initial).
solve(Initial, Visited, P, Path) :-
      bestMove(Initial, State, Visited),
      move(Initial, State, Direction),
      append(P, [Direction], PD),
      solve(State, [State|Visited], PD, Path).

% stores states
%solve(Initial, Path) :- solve(Initial, [], Path).
%solve(Initial, Path, Path) :- goal(Initial).
%solve(Initial, Visited, Path) :-
%      bestMove(Initial, State, Visited),
%      solve(State, [State|Visited], Path).