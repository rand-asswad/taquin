:- consult('taquin_astar.pl').
:- consult('taquin_dfs.pl').

% solve(Puzzle, Solution, Algorithm)
solve(Puzzle, Solution, dfs) :- dfs(Puzzle, Solution).
solve(Puzzle, Solution, greedy) :-
    % h(n) = manh(n) + 3 hamming(n)
    setheuristic(m3h),
    greedy(Puzzle, Solution).
solve(Puzzle, Solution, astar) :-
    % heuristic needs to be admissible
    % admissible heuristics: manhattan or hamming
    setheuristic(manhattan),
    astar(Puzzle, Solution).

% setheuristic/1 sets unique value of dynamic heuristic
setheuristic(H) :- retractall(heuristic(_)), assert(heuristic(H)).