# QCM Chapitre IV - Algorithmes de Tri

*Questions sur les algorithmes de tri et leurs caracteristiques*

---

## Question 1
Quel est l'objectif principal du tri ?

- a) Reduire la taille des donnees
- b) Organiser les donnees selon un ordre determine
- c) Supprimer les doublons
- d) Compresser les fichiers

<details><summary>Reponse</summary>b) Organiser les donnees selon un ordre determine - Le tri permet de classer les elements selon une cle</details>

---

## Question 2
Quelle est la complexite minimale theorique d'un tri par comparaison ?

- a) O(N)
- b) O(N log N)
- c) O(N²)
- d) O(log N)

<details><summary>Reponse</summary>b) O(N log N) - C'est la borne inferieure prouvee mathematiquement pour les tris par comparaison</details>

---

## Question 3
Quel est le principe du tri par selection ?

- a) Inserer chaque element a sa place
- b) Echanger les elements adjacents
- c) Trouver le minimum et le placer au debut
- d) Diviser le tableau en deux

<details><summary>Reponse</summary>c) Trouver le minimum et le placer au debut - On repete pour la partie non triee</details>

---

## Question 4
Quelle est la complexite du tri par selection dans TOUS les cas ?

- a) O(N)
- b) O(N log N)
- c) O(N²)
- d) O(log N)

<details><summary>Reponse</summary>c) O(N²) - Meme sur un tableau deja trie, on fait toujours N(N-1)/2 comparaisons</details>

---

## Question 5
A quelle activite le tri par insertion est-il souvent compare ?

- a) Ranger des livres
- b) Trier des cartes a jouer
- c) Empiler des assiettes
- d) Classer des photos

<details><summary>Reponse</summary>b) Trier des cartes a jouer - On insere chaque carte a sa place dans la main deja triee</details>

---

## Question 6
Quelle est la complexite du tri par insertion dans le MEILLEUR cas ?

- a) O(1)
- b) O(N)
- c) O(N log N)
- d) O(N²)

<details><summary>Reponse</summary>b) O(N) - Quand le tableau est deja trie, on fait seulement N-1 comparaisons</details>

---

## Question 7
Dans le tri a bulles, que "remonte" vers la fin a chaque passe ?

- a) Le plus petit element
- b) Le plus grand element
- c) Un element aleatoire
- d) Le premier element

<details><summary>Reponse</summary>b) Le plus grand element - Il "flotte" vers la fin comme une bulle</details>

---

## Question 8
Quelle amelioration permet d'arreter le tri a bulles si le tableau est deja trie ?

- a) Utiliser un pivot
- b) Detecter l'absence de permutation
- c) Compter les elements
- d) Diviser le tableau

<details><summary>Reponse</summary>b) Detecter l'absence de permutation - Si aucun echange, le tableau est trie</details>

---

## Question 9
Qu'est-ce que le tri Shaker ?

- a) Un tri qui melange les donnees
- b) Un tri a bulles bidirectionnel
- c) Un tri par selection ameliore
- d) Un tri recursif

<details><summary>Reponse</summary>b) Un tri a bulles bidirectionnel - Alternance de parcours gauche-droite et droite-gauche</details>

---

## Question 10
Quelle condition est necessaire pour le tri par denombrement ?

- a) Tableau deja partiellement trie
- b) Elements de type chaine
- c) Valeurs entieres dans un intervalle connu
- d) Nombre pair d'elements

<details><summary>Reponse</summary>c) Valeurs entieres dans un intervalle connu - On doit pouvoir compter les occurrences</details>

---

## Question 11
Quelle est la complexite du tri par denombrement ?

- a) O(N²)
- b) O(N log N)
- c) O(N + P) ou P est la plage de valeurs
- d) O(P²)

<details><summary>Reponse</summary>c) O(N + P) ou P est la plage de valeurs - Lineaire si P est petit par rapport a N</details>

---

## Question 12
Quel paradigme utilise le tri par fusion ?

- a) Programmation dynamique
- b) Diviser pour regner
- c) Glouton
- d) Backtracking

<details><summary>Reponse</summary>b) Diviser pour regner - On divise, trie recursivement, puis fusionne</details>

---

## Question 13
Quelle est la complexite du tri par fusion dans TOUS les cas ?

- a) O(N)
- b) O(N log N)
- c) O(N²)
- d) Depend des donnees

<details><summary>Reponse</summary>b) O(N log N) - Toujours, c'est un avantage du tri fusion</details>

---

## Question 14
Quel est l'inconvenient principal du tri par fusion ?

- a) Complexite O(N²)
- b) Necessite de memoire supplementaire O(N)
- c) Non stable
- d) Ne fonctionne pas sur les grands tableaux

<details><summary>Reponse</summary>b) Necessite de memoire supplementaire O(N) - Pour stocker les sous-tableaux fusionnes</details>

---

## Question 15
Dans le tri rapide, qu'est-ce qu'un pivot ?

- a) Le premier element
- b) L'element median
- c) L'element servant a partitionner
- d) Le dernier element trie

<details><summary>Reponse</summary>c) L'element servant a partitionner - Les elements sont repartis autour du pivot</details>

---

## Question 16
Quelle est la complexite du tri rapide dans le PIRE cas ?

- a) O(N)
- b) O(N log N)
- c) O(N²)
- d) O(log N)

<details><summary>Reponse</summary>c) O(N²) - Quand le pivot est toujours le min ou max (tableau deja trie)</details>

---

## Question 17
**ERREUR A DETECTER** : Le tri rapide est toujours plus rapide que le tri par fusion.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Le tri rapide peut etre O(N²) dans le pire cas, alors que fusion est toujours O(N log N)</details>

---

## Question 18
Qu'est-ce qu'un tri stable ?

- a) Un tri qui ne plante jamais
- b) Un tri qui preserve l'ordre relatif des elements egaux
- c) Un tri a complexite constante
- d) Un tri qui utilise peu de memoire

<details><summary>Reponse</summary>b) Un tri qui preserve l'ordre relatif des elements egaux - Important pour les tris multicriteres</details>

---

## Question 19
Lequel de ces tris est stable ?

- a) Selection
- b) Rapide
- c) Insertion
- d) Tous sont stables

<details><summary>Reponse</summary>c) Insertion - Selection et Rapide peuvent inverser l'ordre d'elements egaux</details>

---

## Question 20
Qu'est-ce qu'un tri en place ?

- a) Un tri qui ne modifie pas le tableau
- b) Un tri utilisant O(1) memoire supplementaire
- c) Un tri effectue sur place de travail
- d) Un tri sans comparaison

<details><summary>Reponse</summary>b) Un tri utilisant O(1) memoire supplementaire - Le tri se fait dans le tableau original</details>

---

## Question 21
Lequel de ces tris n'est PAS en place ?

- a) Selection
- b) Insertion
- c) Fusion
- d) Rapide

<details><summary>Reponse</summary>c) Fusion - Il necessite O(N) memoire supplementaire</details>

---

## Question 22
Pour un petit tableau (N < 50), quel tri est souvent recommande ?

- a) Tri rapide
- b) Tri fusion
- c) Tri insertion
- d) Tri par denombrement

<details><summary>Reponse</summary>c) Tri insertion - Simple, peu de overhead, efficace sur petits tableaux</details>

---

## Question 23
Pour un tableau presque trie, quel tri est le plus efficace ?

- a) Selection
- b) Insertion
- c) Rapide
- d) Denombrement

<details><summary>Reponse</summary>b) Insertion - Complexite O(N) sur un tableau presque trie</details>

---

## Question 24
**ERREUR A DETECTER** : Le tri par selection est un bon choix quand le tableau est presque trie.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Selection fait toujours O(N²) comparaisons, meme sur un tableau trie</details>

---

## Question 25
En JCL, quelle instruction est utilisee pour trier un fichier ?

- a) ORDER
- b) SORT
- c) ARRANGE
- d) CLASSIFY

<details><summary>Reponse</summary>b) SORT - SORT FIELDS definit les cles de tri</details>

---

## Question 26
Que signifie "A" dans SORT FIELDS=(1,10,CH,A) ?

- a) Alphabetique
- b) Ascending (croissant)
- c) Avant
- d) All

<details><summary>Reponse</summary>b) Ascending (croissant) - D serait pour Descending (decroissant)</details>

---

## Question 27
Quelle est la difference entre tri interne et tri externe ?

- a) Interne = ascendant, externe = descendant
- b) Interne = en RAM, externe = sur fichiers
- c) Interne = stable, externe = instable
- d) Interne = recursif, externe = iteratif

<details><summary>Reponse</summary>b) Interne = en RAM, externe = sur fichiers - Le tri externe gere les gros volumes</details>

---

## Question 28
Combien de comparaisons fait le tri par selection pour N elements ?

- a) N
- b) N - 1
- c) N(N-1)/2
- d) N log N

<details><summary>Reponse</summary>c) N(N-1)/2 - (N-1) + (N-2) + ... + 1 comparaisons</details>

---

## Question 29
Dans le tri Gnome, que fait l'algorithme quand deux elements adjacents sont mal ordonnes ?

- a) Il avance
- b) Il echange et recule
- c) Il s'arrete
- d) Il passe au suivant

<details><summary>Reponse</summary>b) Il echange et recule - Comme un gnome qui recule pour verifier</details>

---

## Question 30
**ERREUR A DETECTER** : Le tri par denombrement est toujours plus efficace que le tri rapide.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Seulement si l'intervalle de valeurs P est petit par rapport a N</details>

---

## Question 31
Quel tri utilise la technique du pivot et du partitionnement ?

- a) Fusion
- b) Selection
- c) Rapide
- d) Insertion

<details><summary>Reponse</summary>c) Rapide - Le pivot separe le tableau en deux parties</details>

---

## Question 32
Dans le tri par fusion, la fonction de fusion de deux tableaux tries a une complexite de :

- a) O(1)
- b) O(N + M) ou N et M sont les tailles
- c) O(N × M)
- d) O(log N)

<details><summary>Reponse</summary>b) O(N + M) ou N et M sont les tailles - On parcourt chaque element une fois</details>

---

## Question 33
Quel tri est particulierement adapte aux listes chainees ?

- a) Selection
- b) Rapide
- c) Fusion
- d) Denombrement

<details><summary>Reponse</summary>c) Fusion - La fusion de listes chainees se fait en O(1) memoire supplementaire</details>

---

## Question 34
En COBOL, quelle clause permet de trier par plusieurs cles ?

- a) MULTIPLE KEYS
- b) ON ASCENDING/DESCENDING KEY (plusieurs fois)
- c) SORT-KEYS
- d) MULTI-SORT

<details><summary>Reponse</summary>b) ON ASCENDING/DESCENDING KEY (plusieurs fois) - On peut specifier plusieurs cles</details>

---

## Question 35
**ERREUR A DETECTER** : Le tri rapide est stable et preserve l'ordre des elements egaux.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Le tri rapide n'est PAS stable, il peut inverser l'ordre d'elements egaux</details>

---

*Total : 35 questions - Formation Algorithmique M2i*
