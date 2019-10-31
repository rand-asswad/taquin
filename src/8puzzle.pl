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
h(State, H) :- manhattan(State, Manh), hamming(State, Ham), H is Manh + 3 * Ham.

/* smart_move(Initial, State)
 * choose State that minimises heuristic function
 */
