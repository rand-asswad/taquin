/* the Hamming function
 * number of tiles in wrong positions
 */

hamming([], [], 0).
hamming([H|T1], [H|T2], N) :- hamming(T1, T2, N).
hamming([0|T1], [_|T2], N) :- hamming(T1, T2, N). % exclude zero
hamming([A|T1], [B|T2], N) :- dif(A, B), hamming(T1, T2, M), N is M + 1.