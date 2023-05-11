# TRSwapping
Implémentation d'un Target Record Swapping sur R

L'algorithme implémenté est disponible ici: [https://github.com/julienjamme/Algorithme-de-TRSwapping]()


## TODO

- Tests de cohérence des résultats: ! prendre en compte le moment de l'appariement de la paire
  - appartenance des paires aux mêmes catégories de similarités
  - appartenance des paires à la même entité géo supérieure
  - divergence des paires sur l'entité géographique

- Profilage code
- gestion mémoire => placer des gc() qd ca peut être utile
- Tests performances sur données de tests
- Tests performances sur données réelles

- Parallélisation sur les différents niveaux géo sup 

- Alternative avec calcul de distances entre les groupes
