/* main program for solving 'taquin' puzzle
 * uses:
 * - 'taquin_dfs.pl': memory-less dfs-based algorithms
 * - 'taquin_astar.pl': A* algorithm
 * provides:
 * - solve/4: solve using given algorithm and heuristic
 * - solve/3: solve using given algorithm with the best
 *   heuristic for the algorithm (recommended)
 */
:- consult('taquin_dfs.pl').
:- consult('taquin_astar.pl').

% solve/4: solver wrapper
solve(Puzzle, Solution, Algorithm, Heuristic) :-
    retractall(heuristic(_)), % reset heuristic
    assert(heuristic(Heuristic)), % use given heuristic
    call(Algorithm, Puzzle, Solution), % calls heuristic
    !. % only accept one solution

% solve/3 default heuristics
solve(Puzzle, Solution, dfs) :- dfs(Puzzle, Solution), !.
solve(Puzzle, Solution, iddfs) :- solve(Puzzle, Solution, iddfs, manhattan).
solve(Puzzle, Solution, greedy) :- solve(Puzzle, Solution, greedy, m3h).
solve(Puzzle, Solution, astar) :- solve(Puzzle, Solution, astar, manhattan).