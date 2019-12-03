# Conclusion

## Analyse des résultats

Nous avons trouvé conformément à nos attentes que l'algorithme
A\* est le mieux adapté à ce problème.
Les autres algorithmes peuvent être plus rapides et/ou chanceux
dans les cas simples, mais dans les cas les plus complexes
et surtout en dimensions plus grandes, A\* est le plus
efficace et le meilleur candidat pour trouver une solution
en temps raisonnable.

Néanmoins, l'optimisation locale de Greedy le rend un candidat
respectable pour trouver un chemin rapidement, ce que nous
avons présenté pendant notre soutenance.
En effet, pour l'une des configurations 3×4 que nous avons testé
Greedy était le seul à nous donner une réponse (en temps raisonnable).

## Développements possibles du projet

### Algorithme IDA\*

Malgré la lenteur de l'algorithme ID-DFS, son idée reste très intéressante.
Sa variante **Iterative Deepening A\* (IDA\*)**[@korf] emprunte la fonction
coût de A\* $f(n)=g(n)+h(n)$ et donne des solutions plus rapidement
que A\* et en économisant beaucoup de mémoire par rapport à ce dernier.
Il sera certainement plus intéressant d'étudier et d'implémenter
l'algorithme IDA\*.

### Mouvements

Nous avons implémentés une grille de taille dynamique,
ce qui rend le projet plus intéressant.
Néanmoins, chaque appelle au prédicat `move/3` nécessite
un calcul non-négligeable dans les cas complexes,
il est ainsi important de définir les mouvements *à la main*
pour les tailles 3×3 et 4×4.

### Portabilité

Les algorithmes utilisés dans ce projet peuvent s'appliquer
à la résolutions de différents problèmes (e.g. Rubik's Cube, Flowshop, etc).
Il sera certainement intéressant d'adapter l'implémentation
afin d'avoir un code compatibles avec plusieurs instances
de problèmes de graphe.

## Apport personnel

Ce projet a été une excellente opportunité pour approfondir
notre connaissance en IA et notre maîtrise de la
programmation logique.

\vfill
# Références {-}