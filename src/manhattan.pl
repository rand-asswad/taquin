
/* the Manhattan distance */
%manhattan(State, State, 0).
%manhattan(S1, S2, 1) :- move(S1, S2, _).
%manhattan(S1, S2, N) :- move(S1, T, _), manhattan(T, S2, M), N is M + 1.

% manhattan distance between two indices
manh(Ind1, Ind2, D) :-
      arrayInd(Ind1, X1, Y1), arrayInd(Ind2, X2, Y2),
      diff(X1, X2, Dx), diff(Y1, Y2, Dy),
      D is Dx + Dy.
% return X and Y of a given 1D index (or vice versa)
arrayInd(Ind, X, Y) :- X is mod(Ind, 3), Y is div(Ind, 3).
% absolute difference of two numbers
diff(A, B, Diff) :- D is A - B, Diff is abs(D).

% pos(E, L, Ind)
% position (index) Ind of an element E in a list L
pos(E, [E|_], 0).
pos(E, [_|T], Ind) :- pos(E, T, I), Ind is I + 1.

% manhattan distance of the element at position Ind in L1 from L2
distInd(Ind, L1, L2, D) :- pos(E, L1, Ind), pos(E, L2, Ind2), manh(Ind, Ind2, D).

% cumulative manhattan distance of elements up to Ind
distCumul(0, L1, L2, D) :- distInd(0, L1, L2, D).
distCumul(Ind, L1, L2, D) :- distCumul(Prev, L1, L2, Dc), distInd(Ind, L1, L2, Di), Ind is Prev + 1, D is Di + Dc.

% manhattan distance between two lists
manhattan(L1, L2, D) :- distCumul(N, L1, L2, D), length(L1, M), M is N + 1.
