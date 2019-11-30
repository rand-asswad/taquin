# Taquin (8-puzzle)

The famous sliding tiles game implemented in **Prolog**
using A* graph search algorithm for N×M size puzzles.

![](docs/img/cover_img.png)

# Usage

The code is implemented for [SWI Prolog](https://www.swi-prolog.org/) environment.

Run the following command in your shell (provided you have SWI Prolog installed):
```sh
swipl test_3x3.pl
```
You can also try `test_3x4.pl` or `test_4x4.pl`.

Then run in your prolog console the following query
```prolog
puzzle(Puzzle, Difficulty), testPuzzle(Puzzle, Algorithm).
```
that solves example puzzles of varied difficulties with different algorithms.

# Search Algorithms

- **Depth-First Search (DFS):** a simple implementation of DFS using prolog's search trees,
  there is no optimality guaranty.
- **Greedy Search:** a simple greedy algorithms that chooses each step by minimizing a
  heuristic (admissible or not), there is no optimality guaranty but the algorithm
  is fast and finds a solution for all 3x3 puzzles with the `m3h` heuristic.
- **A\* Search:** a rigourous A* implementation using a priority queue that is guarantied
  to find an optimal solution with an admissible heuristic *manhattan* or *hamming*.

The code is well-documented and readable,
for more details the report is in [docs/book.pdf](https://rand-asswad.github.io/taquin)
written in French (as the project is part of Masters program at INSA Rouen).

# To-Do List

- Implement IDA\*
- Implement random puzzle generator
- Implement comparison (time and solution optimality-wise)

