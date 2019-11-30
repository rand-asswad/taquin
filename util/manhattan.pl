/* module for calculating manhattan distance
 * uses:
 * - dim/2 defines puzzle dimensions (NbRows, NbCols)
 * - goal/1 defines goal state
 * provides:
 * - manhattan/2: manhattan heuristic function
 */
:- dynamic goal/1, dim/2.

% manh/3: manhattan distance between two flat indices
manh(Ind1, Ind2, D) :-
    coord(Ind1, X1, Y1), coord(Ind2, X2, Y2), % switch to coord
    diff(X1, X2, Dx), diff(Y1, Y2, Dy), % calculate coord difference
    D is Dx + Dy. % add differences

% coord/3: finds XY coordinates of a given flat index
coord(Ind, X, Y) :-
    dim(Rows, Cols),
    X is mod(Ind, Cols),
    Y is div(Ind, Cols),
    Y < Rows.

% diff/3: absolute difference of two numbers
diff(A, B, Diff) :- D is A - B, Diff is abs(D).

% distInd(Ind, L1, L2, D)
% D is manhattan distance of the element at Ind in L1 from L2
distInd(Ind, L1, _, 0) :- nth0(Ind, L1, 0). % exclude zero
distInd(Ind, L1, L2, D) :-
    nth0(Ind,  L1, E), % fetch element at Ind from L1
    nth0(Ind2, L2, E), % find its index in L2
    manh(Ind, Ind2, D). % calculate distance between the indices

% distCumul(Ind, L1, L2, D)
% D is cumulative manhattan distance of elements
% from L1 up to index Ind, from L2
distCumul(0, L1, L2, D) :- distInd(0, L1, L2, D).
distCumul(Ind, L1, L2, D) :-
    distCumul(Prev, L1, L2, Dc), % cumul dist up to previous index
    distInd(Ind, L1, L2, Di), % distance of current index
    Ind is Prev + 1, % define previous
    D is Di + Dc. % add distances

% manhattan/3: distance between two lists
manhattan(L1, L2, D) :-
    distCumul(N, L1, L2, D), % cumul distance up to last index in L1
    length(L1, M), % get length(L1)
    M is N + 1, % exclude last index (because indices start at 0)
    !. % cut backtracking when an answer is found

% manhattan/2: alias for calculating manhattan/3 from goal
manhattan(State, D) :- goal(G), manhattan(State, G, D).