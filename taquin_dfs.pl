/* module for solving 'taquin' puzzle using basic
 * graph search algorithms
 * uses:
 * - dim/2: defines puzzle dimensions (NbRows, NbCols)
 * - heuristic/1: defines heuristic function for greedy
 * - 'util/core.pl': for core puzzle definition predicates
 * provides:
 * - dfs/2: Depth-First Search algorithm
 * - iddfs/2: Iterative Deepening DFS algorithm
 * - greedy/2: greedy graph search
 */
:- dynamic dim/2, heuristic/1.
:- consult('util/core.pl').

% Depth-First Search algorithm (simple prolog tree span)
% with no optimality guaranty
dfs(Initial, States) :- dfs(Initial, [], States).
dfs(Initial, States, States) :- goal(Initial).
dfs(Initial, Visited, States) :-
    move(Initial, State, _), % try a move
    \+member(State, Visited), % make sure new state hasn't been visited
    dfs(State, [State|Visited], States). % find path from new state

% Iterative deepening DFS
% iddfs/2: iterative deepening search starting at depth h(initial)
% an optimal solution is guaranteed for an admissible heuristic
iddfs(Initial, Path) :- h(Initial, H), iddfs(Initial, Path, H), !.
iddfs(Initial, Path, Depth) :- iddfs(Initial, [], Path, Depth).
iddfs(Initial, Path, H) :- H1 is H + 1, iddfs(Initial, Path, H1).

% iddfs/4: iterative deepening search for paths
% of maximum depth D
iddfs(State, Path, Path, _) :- goal(State).
iddfs(State, Visited, Solution, D) :-
    D > 0, % non-zero depth
    move(State, S, _), % possible move
    \+member(S, Visited), % unvisited move
    D1 is D - 1, % decrement depth
    iddfs(S, [S|Visited], Solution, D1). % path from new state


% cheapest/2 finds the state with minimum
% heuristic function from list of states
cheapest([State], State).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac =< Bc, cheapest([A|T], M).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac > Bc, cheapest([B|T], M).

% bestMove/3 finds the unvisited neighbor state
% with minimum heuristic function
bestMove(State, Visited, Next) :-
    % construct list of unvisited neighbor states
    findall(
        Neighbor,
        (move(State, Neighbor, _), \+member(Neighbor, Visited)),
        Neighbors
    ),
    % pick neighbor with minimum heuristic function
    cheapest(Neighbors, Next).

% greedy/2: greedy path search using any heuristic 
% admissible or not with no optimality guaranty
greedy(Initial, States) :- greedy(Initial, [], States).
greedy(Initial, States, States) :- goal(Initial).
greedy(Initial, Visited, AllStates) :-
    bestMove(Initial, Visited, State), % find best move from Initial
    greedy(State, [State|Visited], AllStates). % find list of states from new state