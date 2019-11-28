---
title: Le jeu du taquin
subtitle: Projet de contraintes et programmation logique
institute: Département Génie Mathématique
lang: fr-FR
---

# Le jeu

![](img/cover_img.png)

## Principe

- Se joue sur un carré $(n,n)$.
- A chaque état : déplacement d'une tuile d'une case
- 4 possibilités maximum : droite, gauche, bas, haut

## Complexité

- Le problème est NP-difficile
- L'espace d'état est de taille $n^2!$.
- Pour $n=3$, la taille est $362 880$.

# Algorithme

## Depth-First Search (DFS)

- Parcours en profondeur d'un arbre (récursif)
- On marque les sommets visités pour ne pas boucler
- Algorithme Complet : on trouve toujours une solution 

## Problèmes

- Celle-ci est loin d'être optimale tout le temps
- Overflow probable
- exemple:

```
1 2 3       1 2 3       1 2 3       1 2 3
4 5 0       4 0 5       4 5 0       4 5 6
7 8 6       7 8 6       7 8 6       7 8 0
```

## Algorithme A*

- On utilise la destination (connue) pour chercher dans cette direction
- Gestion d'une file d'attente avec priorités
- Algorithme également complet avec une faible complexité
- La solution trouvée est toujours une des meilleurs si l'heuristique est bien adaptée

## Distance de Manhattan

- **Par tuile:** distance associée à la norme 1
- **Total:** la somme des distances de toutes les tuiles
- Ne prend pas en compte la case nulle

## Distance de Hamming

- Distance comparant le nombre de valeurs qui diffèrent entre deux suites
- Ne prend pas en compte la case nulle

## Heuristique

```
 8  1  3        1  2  3 
 4     2        4  5  6 
 7  6  5        7  8    
 
 initial          goal  
```

```
 1  2  3  4  5  6  7  8    1  2  3  4  5  6  7  8
 ----------------------    ----------------------
 1  1  0  0  1  1  0  1    1  2  0  0  2  2  0  3
 
    Hamming = 5 + 0          Manhattan = 10 + 0
```

$$ h(n) := \textrm{Manhattan}(n) + 3\cdot\textrm{Hamming}(n) $$

# Implémentation

## Code

```prolog
%solve/2 alias for solve/3
solve(Initial, Path) :- solve(Initial, [], Path).

%solve(Initial, Visited, FinalPath)
solve(Initial, Path, Path) :- goal(Initial).
solve(Initial, Visited, Path) :-
      bestMove(Initial, State, Visited),
      solve(State, [State|Visited], Path).
```

---

```prolog
%bestMove(State, Next, VisitedStates)
bestMove(State, Next, Visited) :-
      findall(Neighbor, (
          move(State, Neighbor, _),
          \+member(Neighbor, Visited)
      ), Neighbors),
      cheapest(Neighbors, Next).

%cheapest(List, State)
cheapest([State], State).
cheapest([A, B|T], M) :-
    h(A, Ac), h(B, Bc),
    Ac =< Bc, cheapest([A|T], M).
cheapest([A, B|T], M) :-
    h(A, Ac), h(B, Bc),
    Ac > Bc, cheapest([B|T], M).
```

## Démo

# Merci pour votre attention !