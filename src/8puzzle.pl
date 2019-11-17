/* goal(L) : the winning state */
goal([1,2,3,
      4,5,6,
      7,8,0]).


/* move(Before, After, Direction) :
 * After is the result of moving one tile
 * from the Before state
 * in the direction D
 */
:- consult('moves.pl').


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
:- consult('manhattan.pl').
manhattan(State, Dist) :- goal(Goal), manhattan(State, Goal, Dist).

/* hamming(State1, State2, Dist)
 * the hamming distance between two states
 */
:- consult('hamming.pl').
hamming(State, Dist) :- goal(Goal), hamming(State, Goal, Dist).

/* heuristic function */
%h(State, H) :- manhattan(State, Manh), hamming(State, Ham), H is Manh + 3 * Ham.
h(State, H) :- hamming(State, H).

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
solve(Initial, Path) :- solve(Initial, [], [], Path).
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