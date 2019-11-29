/* module for solving 'taquin' puzzle using A* algorithm
 * used predicates:
 * dim/2 defines puzzle dimensions (NbRows, nbCols)
 * heuristic/1 defines heuristic function for A*
 */
:- dynamic dim/2, heuristic/1.

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

% module moves provides move/3 predicate
% move(Before, After, Direction):
% After is the result of moving one tile
% from the Before state
% in the direction D
:- consult('util/moves.pl').


% Depth-First Search algorithm (simple prolog tree span)
% dfs/2: alias for calling dfs/3
dfs(Initial, States) :- dfs(Initial, [], States).
dfs(Initial, States, States) :- goal(Initial), !.
dfs(Initial, Visited, States) :-
    move(Initial, State, _), % try a move
    \+member(State, Visited), % make sure new state hasn't been visited
    append(Visited, [State], NewPath), % add new state to list of visited
    dfs(State, NewPath, States). % find path from new state

% provides manhattan/2 predicate
% manhattan(S, D):
% calculates manhattan distance D
% from state S to goal state
:- consult('util/manhattan.pl').

% provides hamming/2 predicate
% hamming(S, D):
% calculates hamming distance D
% from state S to goal state
:- consult('util/hamming.pl').

% h/3: alias for calling heuristic functions
% h(S, H, D):
% D is the output of heuristic function H at state S
h(State, manhattan, H) :- manhattan(State, H).
h(State, hamming, H) :- hamming(State, H).
h(State, nilsson, H) :-
    manhattan(State, Manh),
    hamming(State, Ham),
    H is Manh + 3 * Ham.

% h/2 alias for calling heuristic function of choice
h(State, H) :- heuristic(Choice), !, h(State, Choice, H), !.

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

% solve/2: alias for calling solve/3
% solve(State, List): finds list of states to goal
% state from given State 
solve(Initial, States) :- solve(Initial, [], States), !.
solve(Initial, States, States) :- goal(Initial).
solve(Initial, Visited, AllStates) :-
    bestMove(Initial, Visited, State), % find best move from Initial
    append(Visited, [State], NewPath), % add new state to list of visited
    solve(State, NewPath, AllStates). % find list of states from new state

% path/2 gets path from list of states
path([_], []) :- !.
path([A,B|T], [D|Path]) :- move(A, B, D), !, path([B|T], Path).

% solvePath/2: alias for finding path from initial state
solvePath(Initial, Path) :-
    solve(Initial, States), % find list of states to goal
    path([Initial|States], Path). % create path from list

%node(State, Path, Heuristic)
priorityQueue(>, node(_, Path1, H1), node(_, Path2, H2)) :-
    length(Path1, G1), F1 is G1 + H1,
    length(Path2, G2), F2 is G2 + H2,
    F1 >= F2, !.
priorityQueue(<, node(_, _, _), node(_, _, _)).
sortQueue(List, Sorted) :- predsort(priorityQueue, List, Sorted).

updateQueue(Node, [], [Node]) :- !.
updateQueue(node(S1,P1,H1), [node(S2,P2,H2)|Q], [node(S1,P1,H1),node(S2,P2,H2)|Q]) :-
    length(P1, G1), F1 is G1 + H1,
    length(P2, G2), F2 is G2 + H2,
    F1 =< F2, !.
updateQueue(node(S1,P1,H1), [node(S2,P2,H2)|Q0], [node(S2,P2,H2)|Q]) :-
    updateQueue(node(S1,P1,H1), Q0, Q).

a_star(Initial, Path) :-
    h(Initial, H),
    a_star_search([node(Initial, [], H)], [], Path),
    !.

% a_star_search(NodeQueue, Path, Solution)
a_star_search([node(Goal, Path, _)|_], _, Path) :- goal(Goal).
a_star_search([node(S, P, H)|Queue], Visited, Solution) :-
    findall(Neighbor, move(S, Neighbor, _), Sneighbors),
    nodeChildren(node(S, P, H), Sneighbors, Visited, Children),
    append(Children, Queue, NewQueue),
    sortQueue(NewQueue, SortedQueue),
    a_star_search(SortedQueue, [S|Visited], Solution).

% nodeChildren(ParentNode, CandidateChildren, Visited, Children)
nodeChildren(_, [], _, []) :- !.
nodeChildren(node(Parent, Path, ParentH), [Child|Others], Visited, [node(Child, [Child|Path], H)|Children]) :-
    \+member(Child, Visited),
    h(Child, H),
    nodeChildren(node(Parent, Path, ParentH), Others, [Child|Visited], Children),
    !.
nodeChildren(Node, [_|Others], Visited, Children) :-
    nodeChildren(Node, Others, Visited, Children),
    !.

% testing
dim(3, 3).
heuristic(manhattan).

puzzle([1,2,3,4,5,0,7,8,6], trivial).
puzzle([0,1,3,4,2,5,7,8,6], easy).
puzzle([6,4,7,8,5,0,3,2,1], hard).
puzzle([8,6,7,2,5,4,3,0,1], hardest).