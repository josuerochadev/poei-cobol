# QCM Chapitre II (Partie 3) - Piles et Files (TAD)

*Questions sur les piles, files et types abstraits de donnees*

---

## Question 1
Que signifie LIFO ?

- a) Last In - First Out
- b) Last In - Final Output
- c) Linear Input - First Output
- d) List In - File Out

<details><summary>Reponse</summary>a) Last In - First Out - Le dernier element entre est le premier a sortir</details>

---

## Question 2
Que signifie FIFO ?

- a) Final In - First Out
- b) First In - First Out
- c) File In - File Out
- d) First Input - Final Output

<details><summary>Reponse</summary>b) First In - First Out - Le premier element entre est le premier a sortir</details>

---

## Question 3
Quelle structure de donnees utilise le principe LIFO ?

- a) File
- b) Tableau
- c) Pile
- d) Liste chainee

<details><summary>Reponse</summary>c) Pile - La pile (stack) fonctionne en LIFO</details>

---

## Question 4
Dans une pile, ou se font les insertions et suppressions ?

- a) A la base uniquement
- b) Au sommet uniquement
- c) A la base pour insertion, au sommet pour suppression
- d) N'importe ou

<details><summary>Reponse</summary>b) Au sommet uniquement - Toutes les operations se font au sommet (top)</details>

---

## Question 5
Comment s'appelle l'operation d'ajout dans une pile ?

- a) Enfiler
- b) Inserer
- c) Empiler (Push)
- d) Ajouter

<details><summary>Reponse</summary>c) Empiler (Push) - L'element est "empile" au sommet</details>

---

## Question 6
Comment s'appelle l'operation de retrait dans une pile ?

- a) Defiler
- b) Depiler (Pop)
- c) Retirer
- d) Extraire

<details><summary>Reponse</summary>b) Depiler (Pop) - L'element au sommet est "depile"</details>

---

## Question 7
Quelle est une application typique des piles ?

- a) File d'impression
- b) Gestion des appels recursifs
- c) Ordonnancement Round Robin
- d) Buffer de communication

<details><summary>Reponse</summary>b) Gestion des appels recursifs - La pile d'execution stocke les contextes d'appel</details>

---

## Question 8
Le bouton "Retour" d'un navigateur Web utilise :

- a) Une file
- b) Une pile
- c) Un tableau
- d) Une liste doublement chainee

<details><summary>Reponse</summary>b) Une pile - L'historique est gere en LIFO (derniere page visitee = premiere accessible)</details>

---

## Question 9
Dans une file, ou se fait l'insertion ?

- a) A l'avant
- b) A l'arriere
- c) Au milieu
- d) N'importe ou

<details><summary>Reponse</summary>b) A l'arriere - Les nouveaux elements arrivent a la fin de la file</details>

---

## Question 10
Dans une file, ou se fait la suppression ?

- a) A l'avant
- b) A l'arriere
- c) Au milieu
- d) N'importe ou

<details><summary>Reponse</summary>a) A l'avant - Les elements sortent par le debut de la file</details>

---

## Question 11
Comment s'appelle l'operation d'ajout dans une file ?

- a) Empiler
- b) Enfiler (Enqueue)
- c) Inserer
- d) Pusher

<details><summary>Reponse</summary>b) Enfiler (Enqueue) - L'element rejoint la file a l'arriere</details>

---

## Question 12
Quelle est une application typique des files ?

- a) Fonction Annuler (Undo)
- b) Parcours en profondeur (DFS)
- c) File d'attente d'impression
- d) Evaluation d'expressions postfixees

<details><summary>Reponse</summary>c) File d'attente d'impression - Les travaux sont traites dans l'ordre d'arrivee (FIFO)</details>

---

## Question 13
Dans une pile implementee par tableau, que represente `top` ?

- a) La taille maximale
- b) L'indice du sommet
- c) Le nombre d'elements supprimes
- d) L'indice de la base

<details><summary>Reponse</summary>b) L'indice du sommet - top pointe vers le dernier element empile</details>

---

## Question 14
Une pile est vide quand :

- a) top = MAX
- b) top = 1
- c) top = 0
- d) top = -1

<details><summary>Reponse</summary>c) top = 0 - Aucun element n'est dans la pile</details>

---

## Question 15
**ERREUR A DETECTER** : On peut depiler d'une pile vide sans probleme.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Depiler une pile vide est une erreur (precondition non respectee)</details>

---

## Question 16
Pourquoi utilise-t-on un tableau circulaire pour une file ?

- a) Pour accelerer les operations
- b) Pour eviter le gaspillage de memoire
- c) Pour permettre le tri
- d) Pour stocker plus d'elements

<details><summary>Reponse</summary>b) Pour eviter le gaspillage de memoire - Sans circularite, l'espace libere au debut serait perdu</details>

---

## Question 17
Dans un tableau circulaire, apres l'indice MAX on passe a :

- a) 0
- b) 1
- c) MAX + 1
- d) -1

<details><summary>Reponse</summary>b) 1 - Les indices "bouclent" de MAX vers 1</details>

---

## Question 18
Quelle est la complexite de l'operation Empiler dans une pile (tableau) ?

- a) O(N)
- b) O(log N)
- c) O(1)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(1) - L'empilage est en temps constant</details>

---

## Question 19
Quelle est la complexite de l'operation Enfiler dans une file dynamique (liste F/L) ?

- a) O(N)
- b) O(log N)
- c) O(1)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(1) - Grace au pointeur last, l'insertion en queue est en temps constant</details>

---

## Question 20
Dans une pile implementee par liste chainee, l'empilage correspond a :

- a) Une insertion en queue
- b) Une insertion en tete
- c) Une insertion au milieu
- d) Un parcours complet

<details><summary>Reponse</summary>b) Une insertion en tete - Le sommet de la pile est la tete de la liste</details>

---

## Question 21
Qu'est-ce qu'un TAD (Type Abstrait de Donnees) ?

- a) Un type de variable
- b) Une specification d'operations sans details d'implementation
- c) Un langage de programmation
- d) Un algorithme de tri

<details><summary>Reponse</summary>b) Une specification d'operations sans details d'implementation - Le TAD definit le "quoi" pas le "comment"</details>

---

## Question 22
L'operation `Sommet(P)` sur une pile :

- a) Retire et retourne l'element au sommet
- b) Retourne l'element au sommet sans le retirer
- c) Ajoute un element au sommet
- d) Vide la pile

<details><summary>Reponse</summary>b) Retourne l'element au sommet sans le retirer - Contrairement a Depiler qui retire l'element</details>

---

## Question 23
**ERREUR A DETECTER** : Une file dynamique necessite obligatoirement un tableau circulaire.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Une file dynamique utilise une liste chainee avec pointeurs first/last, pas un tableau</details>

---

## Question 24
Le parcours en largeur (BFS) utilise :

- a) Une pile
- b) Une file
- c) Un tableau trie
- d) Une liste doublement chainee

<details><summary>Reponse</summary>b) Une file - BFS explore niveau par niveau, ce qui necessite FIFO</details>

---

## Question 25
Le parcours en profondeur (DFS) utilise :

- a) Une pile
- b) Une file
- c) Un tableau trie
- d) Un arbre binaire

<details><summary>Reponse</summary>a) Une pile - DFS explore en profondeur d'abord, ce qui necessite LIFO</details>

---

## Question 26
Si on empile A, B, C puis on depile deux fois, que reste-t-il au sommet ?

- a) A
- b) B
- c) C
- d) Pile vide

<details><summary>Reponse</summary>a) A - On depile C puis B, il reste A au sommet</details>

---

## Question 27
Si on enfile A, B, C puis on defile deux fois, que reste-t-il en tete ?

- a) A
- b) B
- c) C
- d) File vide

<details><summary>Reponse</summary>c) C - On defile A puis B, il reste C en tete</details>

---

## Question 28
Quel est l'avantage de l'implementation dynamique d'une pile ?

- a) Plus rapide
- b) Pas de limite de taille
- c) Moins de memoire utilisee
- d) Plus simple a coder

<details><summary>Reponse</summary>b) Pas de limite de taille - La taille s'adapte dynamiquement (sauf limite memoire)</details>

---

## Question 29
**ERREUR A DETECTER** : Dans une file implementee par liste chainee, defiler est en O(N).

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Defiler est en O(1) car on supprime en tete (acces direct via first)</details>

---

## Question 30
Quelle precondition doit etre verifiee avant d'empiler ?

- a) La pile doit etre vide
- b) La pile ne doit pas etre pleine
- c) La pile doit contenir au moins un element
- d) Aucune precondition

<details><summary>Reponse</summary>b) La pile ne doit pas etre pleine - Sinon debordement (stack overflow)</details>

---

*Total : 30 questions - Formation Algorithmique M2i*
