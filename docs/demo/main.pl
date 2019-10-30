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
