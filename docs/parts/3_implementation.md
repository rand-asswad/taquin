# Implémentation

## Modèle du programme

Nous avons implémenté notre programme sous l'enivronement
[SWI Prolog](https://www.swi-prolog.org/) qui est complet, libre,
bien documenté, et permet de construire des programmes modulaires.

Notre programme donc est modulaire et réutilisable.
```
taquin
├── util
│   ├── core.pl....................modèle principale du jeu
│   ├── hamming.pl.................définition de l'heuristique hamming
│   ├── heuristic.pl...............wrapper pour les heuristiques
│   ├── manhattan.pl...............définition de l'heuristique manhattan
│   ├── moves.pl...................définition des adjacents
│   └── test.pl....................outils pour tester le programme
├── taquin_astar.pl................algorithme A*
├── taquin_dfs.pl..................algorithme du type DFS
├── taquin.pl......................wrapper principal
├── test_3x3.pl....................exemples de puzzles 3×3
├── test_3x4.pl....................exemples de puzzles 3×4
├── test_4x4.pl....................exemples de puzzles 4×4
└── README.md......................petit guide d'utilisation
```

Nous avons implémenté le jeu taquin $n\times m$ à l'aide du prédicat
`dim/2` qui prend en argument le nombre de lignes
et le nombre des colonnes.
La base de connaissance doit contenir une et seulement une dimension.

Nous fournissons dans le module `util/core` le prédicat `goal/1` qui
définit l'état objectif pour la dimension prédéfinie dans
la base de connaissance. Le module `core` dépend des modules
`util/moves` et `util/heuristic`.

Le module `util/moves` définit l'adjacence à l'aide du prédicat `move/3`
```prolog
move(Before, After, Direction)
```
qui est vrai si en glissant une tuile de l'état `Before` dans la 
direction `Direction` l'état `After` est obtenu.

Le module `util/heuristic` fournit le prédicat `h/2` qui calcule
la fonction heuristique d'un état donné, la fonction heuristique
à utiliser doit être définie dans la base de connaissance à l'aide
du prédicat `heuristic/1` qui prend l'une des valeurs `manhattan`,
`hamming` et `m3h`.
Ce module dépend des modules `util/manhattan` et `util/hamming`.

Le module `taquin_dfs` fournit les prédicats des algorithmes
basés sur le principe DFS, et le module `taquin_astar` fournit
le prédicat de l'algorithme A\*. Ces modules dépendent du
module `util/core` (qui inclut les modules nécessaires).

Le module `taquin` fournit un *wrapper* pour tous
les algorithmes implémentés dans les prédicats `solve/3` et `solve/4`.

Utilisation:
```prolog
solve(+Puzzle, -Solution, +Algorithm, +Heuristic).
```
- `+Puzzle`: une configuration initiale donnée
- `-Solution`: un chemin (liste des états) du but jusqu'à `Puzzle`
- `+Algorithm`: l'algorithme à utiliser (`dfs`, `iddfs`, `greedy` ou `astar`)
- `+Heuristic`: heuristique à utiliser (`manhattan`, `hamming` ou `m3h`)

Le prédicat `solve/3` choisit lui-même l'heuristique en fonction
de l'algorithme choisi (recommandé).

## Algorithmes

### Depth-First Search

La définition du prédicat `dfs` est similaire à celle présentée
dans l'explication de l'agorithme en y rajoutant la contrainte
`\+member(E, Chemin)` afin d'éviter les problèmes dont nous
avons parlé.

```prolog
% dfs(Etat, CheminEnCours, CheminFinal)
dfs(Initial, Path, Path) :- goal(Initial).      % condition d'arrêt
dfs(Initial, Visited, Path) :-
    move(Initial, State, _),                    % un mouvement possible
    \+member(State, Visited),                   % l'état n'existe pas dans le chemin
    dfs(State, [State|Visited], Path).          % reste du chemin

% dfs/2: wrapper pour dfs/3 avec chemin vide
dfs(Initial, Path) :- dfs(Initial, [], Path).
```

### Iterative Deepening DFS

La définition d'une itération de ID-DFS est similaire
à celle de DFS, en y rajoutant la contrainte de profondeur.

```prolog
% iddfs(Etat, CheminEnCours, CheminFinal, Profondeur)
iddfs(Initial, Path, Path, _) :- goal(Initial). % condition d'arrêt
iddfs(Initial, Visited, Path, Depth) :-
    Depth > 0,                                  % profondeur non nul
    move(Initial, State, _),                    % un mouvement possible
    \+member(State, Visited),                   % l'état n'existe pas dans le chemin
    D1 is Depth - 1,                            % décrementer le profondeur
    iddfs(State, [State|Visited], Path, D1).    % reste du chemin
```

Il reste d'initialiser la boucle avec un profondeur maximal
sous-estimé par une heuristique admissible
```prolog
% iddfs/2 initialise la boucle avec prondeur max = h(état initial)
iddfs(Initial, Path) :- h(Initial, H), iddfs(Initial, Path, H), !.

% iddfs/3 wrapper d'une itération de iddfs de profondeur donné
iddfs(Initial, Path, Depth) :- iddfs(Initial, [], Path, Depth).
iddfs(Initial, Path, H) :- H1 is H + 1, iddfs(Initial, Path, H1).
```

### Algorithme Greedy

La définition de l'algorithme Greedy est similaire à celui
de DFS, en prenant le meilleur mouvement (localement) non visité.

```prolog
% greedy(Etat, CheminEnCours, CheminFinal)
greedy(Initial, Path, Path) :- goal(Initial).   % condition d'arrêt
greedy(Initial, Visited, Path) :-
    bestMove(Initial, Visited, State),          % le meilleur mouvement
    gredy(State, [State|Visited], Path).        % reste du chemin

% greedy/2: wrapper pour greedy/3 avec chemin vide
greedy(Initial, Path) :- greedy(Initial, [], Path).
```

Le prédicat `greedy/3` se définit grace aux prédicats auxiliaires

```prolog
% cheapest/2 trouve l'état avec l'heuristique la plus petite
cheapest([State], State).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac =< Bc, cheapest([A|T], M).
cheapest([A, B|T], M) :- h(A, Ac), h(B, Bc), Ac > Bc, cheapest([B|T], M).

% bestMove/3 trouve le voisin non-visité le moins cher
bestMove(State, Visited, Next) :-
    % construire la liste des voisins non-visités
    findall(
        Neighbor,
        (move(State, Neighbor, _), \+member(Neighbor, Visited)),
        Neighbors
    ),
    % en choisir le voisin qui minimise l'heuristique
    cheapest(Neighbors, Next).
```

On voit que contairement à `dfs/3` qui contient la clause `move(Initial, State, _)`
permettant d'obtenir `State` dans toutes les directions possibles, `greedy/3`
contient la clause `bestMove(Initial, Visited, State)` qui choisit toujours
le même `State` même si le chemin ne mène pas au but, l'algorithme
peut donc simplement échouer s'il se trouve dans un état sans nouveaux voisins.

### Algorithme A\*

L'algorithme A\* est le plus complexe parmi les algorithmes présentés,
contrairement aux autres algorithmes, il garde une liste des candidats
qui ne cesse d'augmenter jusqu'à ce qu'un chemin est trouvé.

Les algorithmes de type DFS gardent également une liste de candidats
implicite: **une pile**, c'est-à-dire une liste LIFO (last in first out).
En revanche, avec l'algorithme A\* il faut gérer la liste des candidats
explicitement, afin d'optimiser le temps de calcul nous utilisons un
**Priority Queue** (file de priorité), qui est une liste d'éléments
ordonnés dans l'ordre (croissant ou décroissant) par rapport à
une fonction de priorité.

Notre fonction de priorité est $f(n)=g(n)+h(n)$ que nous souhaitons
minimiser, notre Priority Queue est donc dans l'ordre croissant
par rapport à $f$.

Tout d'abord, nous définissons le terme `node/3` qui représente
un nœud du graphe de résolution de A\*.
Le terme `node(S, P, F)` se définit par:

- **Etat `S`:** un configuration du puzzle.
- **Chemin `P`:** le chemin pour arriver à `S`.
- **Coût `F`:** la fonction $f$ évaluée en `S` par rapport au chemin `P`.

Au lieu d'avoir à trier la liste, nous avons implémenté les prédicats
`pushQueue/3` et `appendQueue/3` qui permettent d'insérer des éléments
dans le Queue dans le bon placement directement.

```prolog
% pushQueue(+Node, +Queue, -NewQueue)
% permet de rajouter un nœud au Queue
pushQueue(Node, [], [Node]) :- !. % queue null
pushQueue(node(S1,P1,F1), [node(S2,P2,F2)|Q], [node(S1,P1,F1),node(S2,P2,F2)|Q]) :-
    F1 =< F2,   % le nouveau nœud est le moins cher
    !.          % arrêter la recherche
pushQueue(node(S1,P1,F1), [node(S2,P2,F2)|Q0], [node(S2,P2,F2)|Q]) :-
    pushQueue(node(S1,P1,F1), Q0, Q).

% appendQueue(NodeList, Queue, NewQueue)
% permet de rajouter une liste de nœuds non-ordonnés au Queue
appendQueue([], Q, Q).          % liste nulle
appendQueue([Node|T], Q0, Q) :-
    pushQueue(Node, Q0, Q1),    % rajouter le premier
    appendQueue(T, Q1, Q).      % rajouter le reste
```

Chaque itération de A\* consiste à générer les enfants du nœud prioritaire
et de les rajouter au Queue.

```prolog
% astar_search(NodeQueue, Path, Solution)
astar_search([node(Goal, Path, _)|_], _, Path) :- goal(Goal).
astar_search([node(S, P, H)|Queue], Visited, Solution) :-
    % générer les voisins de S
    findall(Neighbor, move(S, Neighbor, _), Sneighbors),
    % générer une liste des enfants
    nodeChildren(node(S, P, H), Sneighbors, Visited, Children),
    % rajouter les enfants au Queue
    appendQueue(Children, Queue, SortedQueue),
    % recommencer l'itération avec le nouveau Queue
    astar_search(SortedQueue, [S|Visited], Solution).
```

Le prédicat `nodeChildren/4` permet de générer les enfants d'un
nœud à partir de la liste de ses voisins.

```prolog
% nodeChildren(ParentNode, CandidateChildren, Visited, Children)
nodeChildren(_, [], _, []) :- !. % pas de candidats
nodeChildren(node(Parent, Path, ParentF), [Child|Others], Visited,
        [node(Child, [Child|Path], F)|Children]) :-
    % l'état n'existe pas dans le chemin
    \+member(Child, Visited),
    % calculer le coût de l'enfant
    length(Path, G1), h(Child, H), F is G1 + 1 + H,
    % générer les enfants pour le reste des voisins
    nodeChildren(node(Parent, Path, ParentF), Others, [Child|Visited], Children),
    !. % ignorer la règle suivante et passer aux autres candidats
nodeChildren(Node, [_|Others], Visited, Children) :-    % l'état existe dans le chemin
    nodeChildren(Node, Others, Visited, Children).      % passer aux autres candidats
```

Il ne reste que de lancer l'algorithme avec le queue contenant
l'état initial dont le coût est $f(n_0)=h(n_0)$.

```prolog
astar(Initial, Path) :-
    h(Initial, H),                                          % évaluer f(n) = h(n)
    astar_search([node(Initial, [], H)], [Initial], Path).  % lancer A*
```

## Utilisation du programme

Nous avons fourni le module `util/test` qui permet de tester le programme,
le module définit les prédicats

- `printState/1`: affiche un état comme une grille
- `printSolution/2`: affiche un chemin 
- `testPuzzle/3, testPuzzle/2, testPuzzle/1`: wrapper pour résoudre et
  afficher la solution (les 2^ème^ et 3^ème^ arguments sont optionnels)
- `debugPuzzle/3`: affiche le temps de résolution et longueur du chemin

Nous fournissons quelques exemples de puzzles dans les fichiers
`test_3x3.pl`, `test_3x4.pl` et `test_4x4.pl`,
à l'aide de term `puzzle(Puzzle, Difficulty)`.

Pour utiliser notre programme, lancer l'un de nos programmes tests
```sh
$ swipl test_3x3.pl
```
et lancer la requête suivante:
```prolog
% Algorithm = dfs, iddfs, greedy ou astar
% Heuristic = manhattan, hamming ou m3h
?- puzzle(Puzzle, Difficulty),
   Algorithm = astar, Heuristic = manhattan,
   testPuzzle(Puzzle, Algorithm, Heuristic).
```

## Quelques résultats

Nous avons testé nos algorithmes sur quelques exemples représentatifs,
voici les résultats trouvés pour les puzzles de taille $3\times3$.

\begin{table}[H]
\centering
\begin{tabular}{V{3}cV{3}cV{3}c|c|cV{3}c|c|cV{3}}
\hlineB{3}
\multirow{2}{*}{\'Etat} & \multirow{2}{*}{Difficulté} & \multicolumn{3}{cV{3}}{Algorithme} & \multicolumn{3}{cV{3}}{Heuristique} \\\cline{3-8}
                        &                             & Greedy & ID-DFS & A*               & Manh. & Ham. & M3H \\
\hlineB{3}
\state{1&2&3\\4&5& \\7&8&6} & triviale  & \makecell{$t=13$ms\\$N=1$}    & \makecell{$t=3$ms\\$N=1$}         & \makecell{$t=9$ms\\$N=1$}       &  1 & 1 &  4 \\\hline
\state{ &1&3\\4&2&5\\7&8&6} & facile    & \makecell{$t=18$ms\\$N=4$}    & \makecell{$t=6$ms\\$N=4$}         & \makecell{$t=19$ms\\$N=4$}      &  4 & 4 & 16 \\\hline
\state{8&1&3\\4& &2\\7&6&5} & moyenne   & \makecell{$t=96$ms\\$N=34$}   & \makecell{$t=237$ms\\$N=14$}      & \makecell{$t=210$ms\\$N=14$}    & 10 & 5 & 25 \\\hline
\state{4&3&8\\2& &1\\6&5&7} & difficile & \makecell{$t=418$ms\\$N=176$} & \makecell{$t=6~916$ms\\$N=20$}    & \makecell{$t=587$ms\\$N=20$}    & 16 & 8 & 40 \\\hline
\state{6&4&7\\8&5& \\3&2&1} & maximale  & \makecell{$t=108$ms\\$N=45$}  & \makecell{$t=9~233$ms\\$N^*=41$}  & \makecell{$t=27~005$ms\\$N=31$} & 21 & 7 & 42 \\\hline
\state{8&6&7\\2&5&4\\3& &1} & maximale  & \makecell{$t=644$ms\\$N=243$} & \makecell{$t=23~124$ms\\$N^*=41$} & \makecell{$t=21~933$ms\\$N=31$} & 21 & 7 & 42 \\\hline
\hlineB{3}
\end{tabular}
\caption{Comparaison des algorithmes sur des taquin de taille $3\times3$}
\end{table}
<figure id="fig3">
  <img src="img/results.png">
</figure>

Les états classés de difficulté *maximale* sont les configurations
avec les chemins les plus longue pour un puzzle 3×3. [@reinefeld]

Les résultats du DFS sont absents du tableau car pour tous états
non triviaux l'algorithme ne trouve pas un chemin avant qu'on
perde notre patience.

Les solutions de Greedy sont calculé avec l'heuristique local `m3h`.
Les solutions de ID-DFS et A\* sont trouvé avec l'heuristique de Manhattan à part
ceux marquées avec un asterisk; elles sont trouvées avec `m3h` qui n'est pas admissible,
car l'algorithme met beaucoup de temps pour trouver le chemin avec Manhattan
et nous n'avons jamais réussi à obtenir un résultat.
Néanmoins, si on part directement de l'itération de profondeur 31, les chemins
optimaux sont trouvé en $237~514 \text{ms}\approx 4\text{min}$ et
$271~363\text{ms}\approx 4.5\text{min}$ respectivement.