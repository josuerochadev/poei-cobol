# QCM Chapitre III - Recursivite

*Questions sur la recursivite et les algorithmes recursifs*

---

## Question 1
Qu'est-ce qu'une fonction recursive ?

- a) Une fonction qui utilise des boucles
- b) Une fonction qui s'appelle elle-meme
- c) Une fonction qui retourne toujours le meme resultat
- d) Une fonction sans parametres

<details><summary>Reponse</summary>b) Une fonction qui s'appelle elle-meme - C'est la definition de la recursivite</details>

---

## Question 2
Quelle est la valeur de 0! (factorielle de 0) ?

- a) 0
- b) 1
- c) Indefini
- d) -1

<details><summary>Reponse</summary>b) 1 - Par definition mathematique, 0! = 1 (cas de base)</details>

---

## Question 3
Quel element est INDISPENSABLE dans toute fonction recursive ?

- a) Une boucle Pour
- b) Un cas de base (condition d'arret)
- c) Deux appels recursifs
- d) Un tableau

<details><summary>Reponse</summary>b) Un cas de base (condition d'arret) - Sans lui, la recursion est infinie</details>

---

## Question 4
Que se passe-t-il si une fonction recursive n'a pas de cas de base ?

- a) Elle retourne 0
- b) Elle s'execute une seule fois
- c) Elle boucle indefiniment (stack overflow)
- d) Elle retourne NIL

<details><summary>Reponse</summary>c) Elle boucle indefiniment (stack overflow) - Les appels s'empilent jusqu'a debordement</details>

---

## Question 5
Pour calculer Factorielle(4), combien d'appels recursifs sont effectues ?

- a) 3
- b) 4
- c) 5
- d) 24

<details><summary>Reponse</summary>b) 4 - Fact(4) → Fact(3) → Fact(2) → Fact(1) → Fact(0) = 4 appels + le cas de base</details>

---

## Question 6
Quelle est la formule de recurrence de la factorielle ?

- a) n! = n + (n-1)!
- b) n! = n × (n-1)!
- c) n! = (n-1)! / n
- d) n! = n × n!

<details><summary>Reponse</summary>b) n! = n × (n-1)! - On multiplie n par la factorielle du predecesseur</details>

---

## Question 7
Qu'est-ce que la recursivite terminale ?

- a) La recursion qui se termine toujours
- b) L'appel recursif est la derniere operation
- c) La recursion avec un seul cas de base
- d) La recursion sans retour de valeur

<details><summary>Reponse</summary>b) L'appel recursif est la derniere operation - Rien n'est fait apres l'appel recursif</details>

---

## Question 8
Quel est l'avantage de la recursivite terminale ?

- a) Plus facile a lire
- b) Peut etre optimisee par le compilateur (pas de pile)
- c) Plus rapide a ecrire
- d) Permet d'avoir plusieurs cas de base

<details><summary>Reponse</summary>b) Peut etre optimisee par le compilateur (pas de pile) - Le compilateur peut la transformer en boucle</details>

---

## Question 9
La fonction Fibonacci naive a une complexite de :

- a) O(N)
- b) O(log N)
- c) O(2^N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(2^N) - Chaque appel genere deux appels, croissance exponentielle</details>

---

## Question 10
Qu'est-ce que la recursivite croisee ?

- a) Une fonction qui s'appelle deux fois
- b) Deux fonctions qui s'appellent mutuellement
- c) Une recursion avec plusieurs cas de base
- d) Une recursion dans une boucle

<details><summary>Reponse</summary>b) Deux fonctions qui s'appellent mutuellement - Ex: Pair(n) appelle Impair(n-1)</details>

---

## Question 11
**ERREUR A DETECTER** : La fonction suivante est correcte :
```
Fonction F(n) : entier
Debut
    Retourner F(n-1)
Fin
```

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Il manque le cas de base, cette fonction boucle indefiniment</details>

---

## Question 12
Dans l'algorithme PGCD(a, b) d'Euclide, quel est le cas de base ?

- a) a = 0
- b) b = 0
- c) a MOD b = 0
- d) a = b

<details><summary>Reponse</summary>c) a MOD b = 0 - Quand le reste est nul, b est le PGCD</details>

---

## Question 13
La puissance rapide a^n a une complexite de :

- a) O(N)
- b) O(log N)
- c) O(N^2)
- d) O(1)

<details><summary>Reponse</summary>b) O(log N) - On divise n par 2 a chaque etape</details>

---

## Question 14
Pour calculer a^8 avec la puissance rapide, combien de multiplications ?

- a) 8
- b) 7
- c) 4
- d) 3

<details><summary>Reponse</summary>d) 3 - a^8 = ((a^2)^2)^2 = 3 mises au carre</details>

---

## Question 15
Quelle structure de donnees est implicitement utilisee par la recursivite ?

- a) Une file
- b) Une pile
- c) Un tableau
- d) Une liste chainee

<details><summary>Reponse</summary>b) Une pile - La pile d'appels (call stack) stocke les contextes</details>

---

## Question 16
**ERREUR A DETECTER** : Cette fonction progresse correctement vers le cas de base :
```
Fonction F(n) : entier
Debut
    Si n = 0 Alors Retourner 1
    Sinon Retourner F(n)
    Fsi
Fin
```

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - L'appel F(n) ne modifie pas n, donc ne progresse pas vers 0</details>

---

## Question 17
Quand doit-on eviter la recursivite ?

- a) Toujours
- b) Quand une solution iterative simple existe
- c) Quand on manipule des arbres
- d) Quand le probleme se decompose en sous-problemes

<details><summary>Reponse</summary>b) Quand une solution iterative simple existe - L'iteration est souvent plus efficace</details>

---

## Question 18
La memoisation permet de :

- a) Utiliser moins de memoire
- b) Eviter les recalculs des memes sous-problemes
- c) Accelerer les boucles
- d) Convertir en iteratif

<details><summary>Reponse</summary>b) Eviter les recalculs des memes sous-problemes - On stocke les resultats deja calcules</details>

---

## Question 19
Pour la somme d'un tableau de N elements en recursif, quelle est la complexite ?

- a) O(1)
- b) O(log N)
- c) O(N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(N) - On parcourt tous les elements une fois</details>

---

## Question 20
Qu'est-ce que la recursivite multiple ?

- a) Une fonction avec plusieurs parametres
- b) Une fonction avec plusieurs appels recursifs
- c) Une fonction appelee plusieurs fois
- d) Une fonction avec plusieurs retours

<details><summary>Reponse</summary>b) Une fonction avec plusieurs appels recursifs - Ex: Fibonacci avec deux appels</details>

---

## Question 21
Dans la recherche dichotomique recursive, quel est le cas de base ?

- a) Tableau non trie
- b) Element trouve OU bornes croisees
- c) Milieu du tableau
- d) Premier element

<details><summary>Reponse</summary>b) Element trouve OU bornes croisees - On arrete si trouve ou si d > f</details>

---

## Question 22
Le principe "diviser pour regner" utilise :

- a) L'iteration uniquement
- b) La recursivite pour decomposer le probleme
- c) Des tableaux uniquement
- d) La recherche sequentielle

<details><summary>Reponse</summary>b) La recursivite pour decomposer le probleme - On divise en sous-problemes plus petits</details>

---

## Question 23
**ERREUR A DETECTER** : Toute fonction recursive peut etre convertie en fonction iterative.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>a) Vrai - C'est correct, toute recursion peut devenir iteration (avec pile explicite si besoin)</details>

---

## Question 24
Quel est le risque principal d'une recursion trop profonde ?

- a) Resultat incorrect
- b) Debordement de pile (stack overflow)
- c) Perte de donnees
- d) Compilation impossible

<details><summary>Reponse</summary>b) Debordement de pile (stack overflow) - La pile d'appels a une taille limitee</details>

---

## Question 25
Les trois criteres indispensables d'un algorithme recursif sont :

- a) Boucle, condition, affectation
- b) Formule de recurrence, condition d'arret, regle de combinaison
- c) Entree, traitement, sortie
- d) Declaration, initialisation, utilisation

<details><summary>Reponse</summary>b) Formule de recurrence, condition d'arret, regle de combinaison - Les 3 elements essentiels</details>

---

## Question 26
Dans Fibonacci(5), combien de fois Fibonacci(2) est-il calcule (version naive) ?

- a) 1 fois
- b) 2 fois
- c) 3 fois
- d) 5 fois

<details><summary>Reponse</summary>c) 3 fois - C'est le probleme des recalculs inutiles sans memoisation</details>

---

## Question 27
Quelle analogie illustre bien la recursivite ?

- a) Une boucle
- b) Les poupees russes
- c) Un tableau
- d) Une file d'attente

<details><summary>Reponse</summary>b) Les poupees russes - Chaque poupee contient une version plus petite d'elle-meme</details>

---

## Question 28
En COBOL, pour declarer un programme recursif, on utilise :

- a) RECURSIVE DIVISION
- b) PROGRAM-ID. xxx RECURSIVE.
- c) CALL RECURSIVE
- d) PERFORM RECURSIVE

<details><summary>Reponse</summary>b) PROGRAM-ID. xxx RECURSIVE. - Le mot-cle RECURSIVE dans l'identification</details>

---

## Question 29
La recursivite imbriquee signifie :

- a) Plusieurs cas de base
- b) L'argument de l'appel recursif contient un appel recursif
- c) Une recursion dans une boucle
- d) Deux fonctions qui s'appellent

<details><summary>Reponse</summary>b) L'argument de l'appel recursif contient un appel recursif - Ex: F(n, F(n-1))</details>

---

## Question 30
Pour transformer une recursion non-terminale en terminale, on utilise :

- a) Une boucle
- b) Un accumulateur
- c) Un tableau
- d) Une pile

<details><summary>Reponse</summary>b) Un accumulateur - On passe le resultat partiel en parametre</details>

---

*Total : 30 questions - Formation Algorithmique M2i*
