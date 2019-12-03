# Introduction

## Le jeu du taquin

Le jeu du taquin est un puzzle
qui a été créé vers 1870 et a attiré l'intérêt de
nombreux mathématiciens pour sa valeur en tant
qu'un problème combinatoire.

Le jeu est composé de $n\cdot m-1$ petits carreaux
numérotés à partir de $1$ qui glissent dans un cadre
du format $n\times m$ laissant une case vide permettant
de modifier la configuration des carreaux.
Le jeu consiste à remettre dans l'ordre ces cases
à partir d'une configuration initiale quelconque.

Le jeu souvent connu dans les formats $3\times 3$ ou
$4\times 4$, d'où l'appélation anglophone *8-puzzle*
ou *15-puzzle* respectivement.

Le Rubik's Cube est condéré comme l'un des descendants du taquin.
[@wiki:taquin]

## Complexité

Le jeu du taquin est le problème le plus grand de son type
qui peut être résolu complètement (trouver toutes les solutions
du problème à partir d'un puzzle solvable).
Il est simplement défini mais le problème est **NP-difficile**.
[@reinefeld]

Le problème est grand combinatoirement est exige une résolution
guidée afin d'atteindre avant d'épuiser les résources
(temps et mémoire).

L'espace d'état est de taille $\frac{(n\cdot m)!}{2}$ ce qui fait
$181 440$ pour la variante $3\times3$.

En effet, il existe $(n\cdot m)!$ permutations des tuiles,
une permutation est solvable, l'argument de parité a été présenté
dans [@johnson] afin de montrer que la moitié des configurations
initiales de ne peut jamais pas atteindre l'état but en définissant
une fonction invariante de mouvement des tuiles qui définit
deux classes d'équivalence d'états *accessibles* et *non-accessibles*.

## Le but du projet

Le but du projet est de résoudre un problème réel à l'aide
de la programmation logique.
Notre choix s'est portée le jeu du taquin car il est intéressant
en tant qu'un problème d'Intelligence Artificielle,
et s'adapte bien à la programmation logique.

Dans ce projet on présentera ce problème mathématiquement
et proposons des algorithmes de résolutions variées,
une implémentation en **prolog** de ces algorithmes, et finalement
une étude des résultats.

## Formulation du problème

Le problème est le mieux représenté par un graphe connexe.
On défini l'espace d'état $E$ par toutes les configurations solvables
du problème.

![Une partie du graphe du taquin](img/tree.png)

On considère deux états adjacents si on peut passer de l'un vers
l'autre en glissant une seule tuile voisine vers la case vide.
La case vide a au moins 2 tuiles voisines (si elle est dans un coin),
et au plus 4 (gauche, droite, haut, bas).

On cherche idéalement le chemin le plus court pour arriver au but
à partir de l'état initiale, ceci consiste un problème classique
de recherche de chemins dans un graphe.