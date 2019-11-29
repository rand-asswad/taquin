/* module for calculating hamming distance
 * used predicate dim/2 defines puzzle dimensions (NbRows, nbCols)
 */
:- dynamic dim/2.

% move(Before, After, Direction) :
% After is the result of moving one tile
% from the Before state
% in the direction D
move(Before, After, left) :- left(Before, After).
move(Before, After, right) :- right(Before, After).
move(Before, After, up) :- up(Before, After).
move(Before, After, down) :- down(Before, After).

left(State, After) :-
    dim(_, Cols), % get number of columns
    nth0(Ind, State, 0), % get index of 0
    X is mod(Ind, Cols), % get X coordinate
    X < Cols - 1, % 0 has a tile to its right
    Ind1 is Ind + 1, % index of right tile
    swap(Ind, Ind1, State, After), % swap tiles
    !. % only accept one state

right(State, After) :-
    dim(_, Cols), % get number of columns
    nth0(Ind, State, 0), % get index of 0
    X is mod(Ind, Cols), % get X coordinate
    X > 0, % 0 has a tile to its left
    Ind1 is Ind - 1, % index of left tile
    swap(Ind, Ind1, State, After), % swap tiles
    !. % only accept one state

up(State, After) :-
    dim(Rows, Cols), % get dimensions
    nth0(Ind, State, 0), % get index of 0
    Y is div(Ind, Cols), % get Y coordinate
    Y < Rows - 1, % 0 has a tile below
    Ind1 is Ind + Cols, % index of tile below
    swap(Ind, Ind1, State, After), % swap tiles
    !. % only accept one state

down(State, After) :-
    dim(_, Cols), % get number of columns
    nth0(Ind, State, 0), % get index of 0
    Y is div(Ind, Cols), % get Y coordinate
    Y > 0, % 0 has a tile above
    Ind1 is Ind - Cols, % index of tile above
    swap(Ind, Ind1, State, After), % swap tiles
    !. % only accept one state

% swap/4 swaps elements from list
% at indices Ind1 and Ind2
% returns new list
swap(Ind1, Ind2, List, NewList) :-
    nth0(Ind1, List, E1),
    nth0(Ind2, List, E2),
    setNth0(Ind1, E2, List, Tmp),
    setNth0(Ind2, E1, Tmp, NewList).

% setNth0(Index, Element, List, NewList)
% set element from list at given index
% returns new list
setNth0(_, _, [], []) :- !. % do nothing if empty lists
setNth0(0, E, [_|T], [E|T]) :- !. % set first element
setNth0(Ind, E, [H|Tail], [H|NewTail]) :-
    Ind > 0, % inner index
    NewInd is Ind - 1, % decremnt index
    setNth0(NewInd, E, Tail, NewTail). % treat inner list