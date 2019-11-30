/* module for solving 'taquin' puzzle using A* algorithm
 * uses:
 * - dim/2: defines puzzle dimensions (NbRows, NbCols)
 * - heuristic/1: defines heuristic function for greedy
 * - 'util/core.pl': for core puzzle definition predicates
 * provides:
 * - astar/2: Depth-First Search algorithm
 */
:- dynamic dim/2, heuristic/1.
:- consult('util/core.pl').

/* Utils for A* algorithm
 * - node/3 (term) a node in the A* graph is defined by:
 *   + State: the tile configurations as a list
 *   + Path: the list of states to get to current state
 *   + Cost function value at node (n): f(n) = g(n) + h(n)
 *     g(n) cost to reach the node = length of Path
 *     h(n) heuristic function (underestimate of true cost to reach goal)
 * - priority queue: list of unexplored nodes in assending order
 *   with respect to the cost function f(n)
 */

% pushQueue/3: adds node to priority queue
% pushQueue(Node, Queue, NewQueue)
pushQueue(Node, [], [Node]) :- !.
pushQueue(node(S1,P1,F1), [node(S2,P2,F2)|Q], [node(S1,P1,F1),node(S2,P2,F2)|Q]) :- F1 =< F2, !.
pushQueue(node(S1,P1,F1), [node(S2,P2,F2)|Q0], [node(S2,P2,F2)|Q]) :-
    pushQueue(node(S1,P1,F1), Q0, Q).

% appendQueue/3: adds a list of nodes to priority queue
% appendQueue(NodeList, Queue, NewQueue)
appendQueue([], Q, Q).
appendQueue([Node|T], Q0, Q) :- pushQueue(Node, Q0, Q1), appendQueue(T, Q1, Q).

astar(Initial, Path) :-
    h(Initial, H), % cost of initial state: f(n) = h(n)
    astar_search([node(Initial, [], H)], [], Path), % run astar search
    !.

% astar_search(NodeQueue, Path, Solution)
astar_search([node(Goal, Path, _)|_], _, Path) :- goal(Goal).
astar_search([node(S, P, H)|Queue], Visited, Solution) :-
    findall(Neighbor, move(S, Neighbor, _), Sneighbors), % find neighbor states
    nodeChildren(node(S, P, H), Sneighbors, Visited, Children), % expand children
    appendQueue(Children, Queue, SortedQueue), % add children to queue
    astar_search(SortedQueue, [S|Visited], Solution).

% nodeChildren(ParentNode, CandidateChildren, Visited, Children)
% expand children of a node
nodeChildren(_, [], _, []) :- !. % no candidates no children
nodeChildren(node(Parent, Path, ParentF), [Child|Others], Visited,
        [node(Child, [Child|Path], F)|Children]) :-
    \+member(Child, Visited), % child node state has not been visited
    length(Path, G1), h(Child, H), F is G1 + 1 + H, % f(child node)
    nodeChildren(node(Parent, Path, ParentF), Others, [Child|Visited], Children),
    !.
nodeChildren(Node, [_|Others], Visited, Children) :- % child node has ben visited
    nodeChildren(Node, Others, Visited, Children), % try other children
    !.