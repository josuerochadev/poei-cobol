# QCM Chapitre V - Complexite Algorithmique

*Questions sur la notation Big-O et l'analyse de complexite*

---

## Question 1
Que mesure la complexite temporelle ?

- a) Le temps en secondes
- b) Le nombre d'operations en fonction de la taille des donnees
- c) La vitesse du processeur
- d) La taille du programme

<details><summary>Reponse</summary>b) Le nombre d'operations en fonction de la taille des donnees - On compte les operations, pas le temps reel</details>

---

## Question 2
Que signifie O(n) ?

- a) Exactement n operations
- b) De l'ordre de n operations (proportionnel a n)
- c) Maximum n operations
- d) Minimum n operations

<details><summary>Reponse</summary>b) De l'ordre de n operations (proportionnel a n) - La notation Big-O decrit le comportement asymptotique</details>

---

## Question 3
Quelle est la simplification de O(3n + 5) ?

- a) O(3n + 5)
- b) O(3n)
- c) O(n)
- d) O(5)

<details><summary>Reponse</summary>c) O(n) - On ignore les constantes multiplicatives et les termes constants</details>

---

## Question 4
Quelle est la simplification de O(n² + 100n + 50) ?

- a) O(n² + 100n)
- b) O(100n)
- c) O(n²)
- d) O(50)

<details><summary>Reponse</summary>c) O(n²) - On garde uniquement le terme dominant</details>

---

## Question 5
Quel ordre de complexite est le plus rapide ?

- a) O(n)
- b) O(log n)
- c) O(n²)
- d) O(n log n)

<details><summary>Reponse</summary>b) O(log n) - C'est le plus petit apres O(1)</details>

---

## Question 6
Quelle est la hierarchie correcte (du plus rapide au plus lent) ?

- a) O(n) < O(log n) < O(n²)
- b) O(log n) < O(n) < O(n²)
- c) O(n²) < O(n) < O(log n)
- d) O(log n) < O(n²) < O(n)

<details><summary>Reponse</summary>b) O(log n) < O(n) < O(n²) - Logarithmique < Lineaire < Quadratique</details>

---

## Question 7
Quelle complexite correspond a l'acces direct dans un tableau (T[i]) ?

- a) O(n)
- b) O(log n)
- c) O(1)
- d) O(n²)

<details><summary>Reponse</summary>c) O(1) - L'acces par indice est en temps constant</details>

---

## Question 8
Quelle complexite correspond a la recherche dichotomique ?

- a) O(n)
- b) O(log n)
- c) O(1)
- d) O(n²)

<details><summary>Reponse</summary>b) O(log n) - On divise l'espace de recherche par 2 a chaque iteration</details>

---

## Question 9
Quelle est la complexite de deux boucles imbriquees de 1 a n ?

- a) O(n)
- b) O(2n)
- c) O(n²)
- d) O(n + n)

<details><summary>Reponse</summary>c) O(n²) - n iterations × n iterations = n² operations</details>

---

## Question 10
Pour n = 1000, combien d'operations approximativement pour O(n²) ?

- a) 1 000
- b) 10 000
- c) 1 000 000
- d) 2 000

<details><summary>Reponse</summary>c) 1 000 000 - 1000² = 1 000 000 operations</details>

---

## Question 11
**ERREUR A DETECTER** : O(2n) est plus lent que O(n).

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - O(2n) = O(n), les constantes sont ignorees</details>

---

## Question 12
Quelle est la complexite du tri par fusion ?

- a) O(n)
- b) O(n log n)
- c) O(n²)
- d) O(log n)

<details><summary>Reponse</summary>b) O(n log n) - C'est la complexite optimale pour un tri par comparaison</details>

---

## Question 13
Quelle est la complexite du tri par selection ?

- a) O(n)
- b) O(n log n)
- c) O(n²)
- d) O(log n)

<details><summary>Reponse</summary>c) O(n²) - Deux boucles imbriquees, meme dans le meilleur cas</details>

---

## Question 14
Que mesure la complexite spatiale ?

- a) La taille du code source
- b) La memoire supplementaire utilisee
- c) L'espace disque
- d) La bande passante

<details><summary>Reponse</summary>b) La memoire supplementaire utilisee - Hors donnees d'entree</details>

---

## Question 15
Quelle est la complexite spatiale d'un tri en place ?

- a) O(n)
- b) O(n²)
- c) O(1)
- d) O(log n)

<details><summary>Reponse</summary>c) O(1) - Un tri en place n'utilise qu'une memoire constante supplementaire</details>

---

## Question 16
Quelle est la complexite spatiale du tri par fusion ?

- a) O(1)
- b) O(log n)
- c) O(n)
- d) O(n²)

<details><summary>Reponse</summary>c) O(n) - Il necessite un tableau supplementaire de taille n</details>

---

## Question 17
Dans quel cas la recherche sequentielle est-elle O(1) ?

- a) Tableau trie
- b) Element en premiere position
- c) Element absent
- d) Jamais

<details><summary>Reponse</summary>b) Element en premiere position - C'est le meilleur cas</details>

---

## Question 18
Quelle est la complexite de Fibonacci recursif naif ?

- a) O(n)
- b) O(n²)
- c) O(2^n)
- d) O(log n)

<details><summary>Reponse</summary>c) O(2^n) - Chaque appel genere deux appels, croissance exponentielle</details>

---

## Question 19
**ERREUR A DETECTER** : Un algorithme O(n²) est toujours plus lent qu'un algorithme O(n log n).

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Pour de petites valeurs de n, O(n²) peut etre plus rapide (constantes ignorees)</details>

---

## Question 20
Pour un fichier de 1 million d'enregistrements, quel tri est acceptable ?

- a) Tri selection O(n²)
- b) Tri bulles O(n²)
- c) Tri fusion O(n log n)
- d) Aucun

<details><summary>Reponse</summary>c) Tri fusion O(n log n) - Les tris O(n²) prendraient trop de temps</details>

---

## Question 21
Combien d'operations pour log₂(1024) ?

- a) 5
- b) 10
- c) 100
- d) 512

<details><summary>Reponse</summary>b) 10 - Car 2^10 = 1024</details>

---

## Question 22
Quelle boucle a une complexite O(log n) ?

- a) `Pour i de 1 a n faire`
- b) `Tant que i > 0 faire i := i - 1`
- c) `Tant que i > 0 faire i := i DIV 2`
- d) `Pour i de 1 a n*n faire`

<details><summary>Reponse</summary>c) `Tant que i > 0 faire i := i DIV 2` - On divise par 2 a chaque iteration</details>

---

## Question 23
Quelle est la complexite de ce code ?
```
Pour i de 1 a n faire
    Pour j de 1 a i faire
        x := x + 1
    Fpour
Fpour
```

- a) O(n)
- b) O(n log n)
- c) O(n²)
- d) O(n³)

<details><summary>Reponse</summary>c) O(n²) - 1+2+3+...+n = n(n+1)/2 = O(n²)</details>

---

## Question 24
La memoisation permet de :

- a) Reduire la memoire utilisee
- b) Echanger de la memoire contre du temps
- c) Augmenter la complexite
- d) Simplifier le code

<details><summary>Reponse</summary>b) Echanger de la memoire contre du temps - On stocke les resultats pour eviter les recalculs</details>

---

## Question 25
Quelle est la complexite spatiale d'une fonction recursive avec n appels ?

- a) O(1)
- b) O(log n)
- c) O(n)
- d) O(n²)

<details><summary>Reponse</summary>c) O(n) - Chaque appel utilise de l'espace sur la pile</details>

---

## Question 26
Pour n = 20, combien vaut approximativement 2^n ?

- a) 40
- b) 400
- c) 1 000
- d) 1 000 000

<details><summary>Reponse</summary>d) 1 000 000 - 2^20 = 1 048 576 ≈ 1 million</details>

---

## Question 27
**ERREUR A DETECTER** : O(n!) est utilisable pour n = 100.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - 100! est astronomiquement grand, totalement inutilisable</details>

---

## Question 28
Quel cas analyse-t-on generalement avec Big-O ?

- a) Le meilleur cas
- b) Le cas moyen
- c) Le pire cas
- d) Le cas aleatoire

<details><summary>Reponse</summary>c) Le pire cas - Pour garantir une borne superieure</details>

---

## Question 29
Si un algorithme fait n² comparaisons et n echanges, quelle est sa complexite ?

- a) O(n² + n)
- b) O(n²)
- c) O(n)
- d) O(2n²)

<details><summary>Reponse</summary>b) O(n²) - On garde le terme dominant : n² > n</details>

---

## Question 30
Pourquoi le SORT JCL est-il efficace pour de gros fichiers ?

- a) Il utilise un algorithme O(n²)
- b) Il utilise un algorithme O(n log n) optimise
- c) Il ignore certains enregistrements
- d) Il utilise la recursivite

<details><summary>Reponse</summary>b) Il utilise un algorithme O(n log n) optimise - Merge sort externe optimise pour les gros volumes</details>

---

*Total : 30 questions - Formation Algorithmique M2i*
