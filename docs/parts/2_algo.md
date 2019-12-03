# Algorithmes

Ils existent de nombreux algorithmes pour la recherche
d'un chemin dans un graphe, nous avons implémenté
quelques algorithmes qui correspondent bien à notre
problème et qui s'adaptent bien aux principes
de la programmation logique.

## Depth-First Search (DFS)

L'algorithme de parcours en profondeur (DFS) est un algorithme
complet permettant de trouver un chemin dans un graphe.

Cette algorithme est le principe *inné* de **prolog**
de l'arbre de résolution.

L'implémentation de DFS dans un arbre se fait simplement par le code
```prolog
dfs(Etat, [Etat]) :- final(Etat).
dfs(E1, [E1|Chemin]) :-
    adjacent(E1, E2),
    dfs(E2, Chemin).
```
On obtient notre chemin par la requête
```prolog
?- dfs([EtatInitial], Chemin).
```

![L'ordre de parcours des nœuds dans l'algorithme DFS](img/dfs.png)

Néanmoins, cet algorithme ne fonctionne pas pour la plupart des
puzzles; il parcourt en profondeur donc il prendra toujours
le premier adjacent de `E1` jusqu'à ce que le dernier `E1`
n'a plus d'adjacents, et puis tentera le deuxième adjcent du `E1`
précedant, et ainsi suite...sauf que dans le jeu du taquin
*il y a toujours au moins 2 adjacents!* le programme
est donc infiniment récursif.

Si on suppose que le prédicat `adjacent/2` est défini par
```prolog
adjacent(A, B) :- adjacent(A, B, gauche).
adjacent(A, B) :- adjacent(A, B, droite).
adjacent(A, B) :- adjacent(A, B, haut).
adjacent(A, B) :- adjacent(A, B, bas).
```
L'arbre de résolution de prolog dépend de l'ordre
de définition des prédicats, il tentera les adjacents
dans l'ordre (gauche, droite, haut, bas) donc pour
l'état suivant qui est adjacent au but, il modulera
entre ces deux états jusqu'à ce qu'il na plus de mémoire.
```
1 2 3       1 2 3       1 2 3       1 2 3       1 2 3
4 5         4 5         4   5       4 5         4   5
7 8 6       7 8 6       7 8 6       7 8 6       7 8 6       ...
```

Il est donc nécessaire d'interdire de prendre un adjacent `E2` déjà visité.
Or, un nouveau problème s'introduit:
```
1 2 3       1 2 3       1 2 3       1 2 3       1 2 3
4 5         4 5           4 5       7 4 5       7 4 5
7 8 6       7 8 6       7 8 6         8 6       8   6       ...
```
Nous avons testé cet algorithme avec cette configuration,
il fait 27 mouvements afin d'arriver au but !
Théoriquement, il trouvera toujours un chemin, mais en pratique
pour la plupart des configurations le overflow arrive avant
de trouver une solution.

D'où la nécessité de prendre des décisions informées,
nous allons ainsi introduire la notion d'**heuristique**.

## Heuristiques

Une **fonction heuristique** sur un graphe est une fonction
$h:E\rightarrow\N$ où $E$ est l'espace d'états du problème,
$h(n)$ représente le coût estimé pour arriver au but
à partir du nœud $n$, on appelle *coût* d'un chemin
la longueur d'un chemin.

On appelle **heuristique admissible** une heuristique $h$ telle que
$$ \forall n\in E, h(n) \leq h^*(n) $$
où $h^*(n)$ est le vrai coût minimal pour arriver au but
à partir du nœud $n$ (dite l'*heuristique parfaite*).

Trivialement, l'heuristique nulle est admissible mais
elle ne rajoute aucune valeur à la résolution du graphe.

[@wiki:h]

Nous allons introduire deux heuristiques qu'on a utilisé dans nos algorithmes.

### Distance de Hamming

La distance de Hamming est définie pour deux listes (ou mots) de même
taille par le nombre de valeurs qui diffèrent entre ces deux listes.

Soit mathématiquement, avec $A$ l'ensemble des atoms (ou alphabet)
\begin{align*}
d_{\text{Hamming}}: A^n \times A^n &\rightarrow \N\\
(x, y) &\mapsto \sum_{i=1}^n (1-\delta_{x[i],y[i]}) =
\begin{cases}
0 &\text{si } x[i] = y[i]\\
1 &\text{sinon}
\end{cases}
\end{align*}

Dans notre contexte, on ne prend pas en compte de la case vide
dans ce calcul.

L'heuristique de Hamming est donc la distance de Hamming entre
le tableau du nœud $n$ et le nœud final.
Cette heuristique est admissible.

### Distance de Manhattan

La distance de Manhattan est la distance $L_1$ dans les espaces de Banach.
Soit $V$ un espace de Banach de dimension $n$.
\begin{align*}
d_1: V \times V &\rightarrow \R_+\\
(x, y) &\mapsto \sum_{i=1}^{\dim V} |x_i - y_i|
\end{align*}

Dans notre contexte, la distance $d_1$ est définie sur
$V=\{1,\ldots,n\}\times\{1,\ldots,m\}$, soit $I=\{1,\ldots,nm-1\}$,
on définit la fonction de position d'une tuile dans un état $e\in E$
\begin{align*}
\textrm{pos}_e: I &\rightarrow V\\
p &\mapsto (i, j)
\end{align*}
La distance de Manhattan entre deux états se donne donc par
\begin{align*}
d_{\text{Manhattan}}: E \times E &\rightarrow \N\\
(u, v) &\mapsto \sum_{p\in I} d_1\left(\textrm{pos}_u(p), \textrm{pos}_v(p)\right)
\end{align*}

De même, l'heuristique de Manhattan est la distance entre
le nœud $n$ et le nœud final et ne prend pas compte de la case vide.
Cette heuristique est admissible.

\begin{table}[H]
\centering
\begin{tabular}{V{3}c|c|cV{3}c|c|cV{3}}
\hlineB{3}
\multicolumn{3}{V{3}cV{3}}{\'Etat $n$} & \multicolumn{3}{cV{3}}{Final} \\
\hlineB{3}
8 & 1 & 3 & 1 & 2 & 3 \\ \hline
4 &   & 2 & 4 & 5 & 6 \\ \hline
7 & 6 & 5 & 7 & 8 &   \\
\hlineB{3}
\end{tabular}~%
\begin{tabular}{V{3}cV{3}c|c|c|c|c|c|c|cV{3}c|c|c|c|c|c|c|cV{3}cV{3}}\hlineB{3}
Heuristique & \multicolumn{8}{cV{3}}{Hamming} & \multicolumn{8}{cV{3}}{Manhattan} & $h^*$ \\\hlineB{3}
Tuile    & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8  &  1 & 2 & 3 & 4 & 5 & 6 & 7 & 8  & \\\hline
Distance & 1 & 1 & 0 & 0 & 1 & 1 & 0 & 1  &  1 & 2 & 0 & 0 & 2 & 2 & 0 & 3  & \\\hline
Total    & \multicolumn{8}{cV{3}}{5}      & \multicolumn{8}{cV{3}}{10}      & 14 \\\hlineB{3}
\end{tabular}

\caption{Heuristiques de Hamming et de Manhattan}
\end{table}
<figure id="fig1">
  <img src="img/heuristics.png">
</figure>

## Algorithme Greedy

Dans le cas du jeu du taquin, l'algorithme de Greedy est
un algorithme d'optimisation locale;
lorsqu'il faut faire un choix parmi une liste d'adjacents
il prend l'adjacent qui minimise la fonction coût.

L'algorithme n'est pas toujours optimale,
dans notre implémentation nous avons obtenue les meilleurs
résultats pour l'heuristique définie par

$$ h(n) := \textrm{Manhattan}(n) + 3\cdot\textrm{Hamming}(n) $$

Cette heuristique n'est pas admissible car elle vaut
pour un état $n$ adjacent au but
$$h(n) = (1) + 3(1) = 4 > 1 = h^*(n)$$
mais l'admissiblité n'est pas importante pour cet algorithme.

L'algorithme trouve une solution en quelques secondes
pour toutes les configurations du taquin de taille $3\times3$,
la plupart de ses solutions ne sont pas optimales.

Pour les tailles plus grandes, l'algorithme prends plus de temps
et ne trouve pas toujours une solution car ses choix locaux
sont définitifs (il n'y a pas d'alternatif dans le backtracking).

## Iterative Deepening DFS (ID-DFS)

L'algorithme ID-DFS est une variante du DFS, il effectue
une recherche en profondeur DFS itérativement pour un
profondeur maximal donnée $d$, et l'incrémente succéssivement
en commençant par $d=0$ jusqu'à ce qu'il trouve une solution. [@wiki:iddfs]

Classiquement, $d=0$ à la première itération mais cela est
loin d'être optimale, nous proposons donc de commencer par
une sous-estimation du longeur du chemin,
$d=h(n_\text{initial})$ est une sous-estimation si $h$
est une heuristique admissible.

### Compléxité {-}

- Compléxité en temps: $O(b^d)$
- Compléxité spaciale: $O(d)$

où $d$ est le profondeur, et $b$ est le facteur de branchement.

L'algorithme trouve en quelques secondes des solutions optimales
pour toutes les configurations de taille $3\times3$,
et trouve rarement des solution pour les tailles plus grandes.

Ceci est dû au fait qu'une fois $d$ est grand, ID-DFS est presque
comme DFS et a les mêmes problèmes.

## Algorithme A\*

L'algorithme A\* est complet, optimal et efficace.
C'est un algorithme *à mémoire*, qui a une compléxité
spaciale importante.

A\* utilise la fonction d'évaluation $f$ défini par
$$ f(n) = g(n) + h(n) $$
où

- $f(n)$ le coût total estimé au nœud $n$
- $g(n)$ le vrai coût pour arriver au nœud $n$ à partir du nœud initial
- $h(n)$ le coût estimé pour arriver au but à partir du nœud $n$

Il garde une liste des candidats, et en choisit
succéssivement celui qui minimise le coût $f$ et rajoute
tous ses états adjacents (non visité) à la liste des candidats.

\begin{figure}
\centering
\includegraphics{img/astar_tree.png}
\caption{Une partie de la graphe de résolution A${}^*$\\
\textit{source:} \href{https://www.cs.princeton.edu/courses/archive/spring18/cos226/assignments/8puzzle/index.html}%
{Princeton University - Computer Science Course COS226}}
\end{figure}
<figure id="fig2"><img src="img/astar_tree.png"></figure>

Les solutions obtenues sont optimale si l'heuristique est
admissible, les meilleurs résultats sont obtenus pour la distance
de Manhattan. [@wiki:astar]

Pour les configurations faciles les solutions sont obtenues instantanément,
mais pour les configurations difficiles l'algorithme peut prendre plus
de temps que les algorithmes précédents.
Néanmoins, pour les tailles plus grande que $3\times3$, A\* est le
meilleur algorithme pour trouver une solution.