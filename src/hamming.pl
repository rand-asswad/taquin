/* the Hamming function
 * number of tiles in different positions
 */

hamming([], [], 0).
hamming([H|T1], [H|T2], N) :- hamming(T1, T2, N).
hamming([_|T1], [_|T2], N) :- hamming(T1, T2, M), N is M + 1.