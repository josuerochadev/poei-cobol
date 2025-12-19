# QCM Chapitre II (Partie 2) - Pointeurs et Listes Chainees

*Questions sur les pointeurs, la gestion dynamique et les listes chainees*

---

## Question 1
Qu'est-ce qu'un pointeur ?

- a) Une variable qui contient une valeur entiere
- b) Une variable qui contient l'adresse d'une autre variable
- c) Une fonction de recherche
- d) Un type de boucle

<details><summary>Reponse</summary>b) Une variable qui contient l'adresse d'une autre variable - Le pointeur "pointe" vers un emplacement memoire</details>

---

## Question 2
Si X = 150 et ^X = 654521, que represente 654521 ?

- a) La valeur de X
- b) L'adresse memoire de X
- c) Le type de X
- d) L'indice de X

<details><summary>Reponse</summary>b) L'adresse memoire de X - La notation ^X represente l'adresse de la variable X</details>

---

## Question 3
Comment accede-t-on a la valeur pointee par un pointeur P ?

- a) P
- b) ^P
- c) P^
- d) *P

<details><summary>Reponse</summary>c) P^ - Le symbole ^ apres le pointeur permet le dereferencement (acces au pointe)</details>

---

## Question 4
Que represente la valeur NIL ?

- a) Zero
- b) Une erreur
- c) Un pointeur qui ne pointe vers rien
- d) La fin d'un tableau

<details><summary>Reponse</summary>c) Un pointeur qui ne pointe vers rien - NIL (ou NULL) indique l'absence de reference</details>

---

## Question 5
Quelle instruction alloue de la memoire dynamiquement ?

- a) Liberer(P)
- b) Creer(P)
- c) P := NIL
- d) P^ := 0

<details><summary>Reponse</summary>b) Creer(P) - Cette instruction alloue une zone memoire et fait pointer P vers elle</details>

---

## Question 6
**ERREUR A DETECTER** : L'instruction `P := 10` est valide pour un pointeur vers entier.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - On ne peut pas affecter une valeur directement a un pointeur. Il faut utiliser P^ := 10</details>

---

## Question 7
Qu'est-ce qu'une fuite memoire ?

- a) Une zone memoire allouee mais plus referencee
- b) Une erreur de syntaxe
- c) Un pointeur vers NIL
- d) Une variable non initialisee

<details><summary>Reponse</summary>a) Une zone memoire allouee mais plus referencee - La memoire reste occupee mais inaccessible</details>

---

## Question 8
Dans une liste simplement chainee, chaque noeud contient :

- a) Uniquement une valeur
- b) Une valeur et un pointeur vers le suivant
- c) Une valeur et deux pointeurs
- d) Uniquement un pointeur

<details><summary>Reponse</summary>b) Une valeur et un pointeur vers le suivant - C'est la structure de base d'une liste simple</details>

---

## Question 9
Quelle est la complexite de l'insertion en tete d'une liste chainee ?

- a) O(1)
- b) O(N)
- c) O(log N)
- d) O(N^2)

<details><summary>Reponse</summary>a) O(1) - L'insertion en tete ne necessite pas de parcourir la liste</details>

---

## Question 10
Quelle est la complexite de l'insertion en queue d'une liste simplement chainee (sans pointeur last) ?

- a) O(1)
- b) O(N)
- c) O(log N)
- d) O(N^2)

<details><summary>Reponse</summary>b) O(N) - Il faut parcourir toute la liste pour trouver le dernier element</details>

---

## Question 11
Une liste chainee F/L (First/Last) permet :

- a) Un acces direct par indice
- b) Une insertion en queue en O(1)
- c) Une recherche en O(log N)
- d) Un tri automatique

<details><summary>Reponse</summary>b) Une insertion en queue en O(1) - Le pointeur last permet d'acceder directement au dernier element</details>

---

## Question 12
Dans une liste doublement chainee, chaque noeud contient :

- a) Un pointeur vers le suivant uniquement
- b) Deux valeurs
- c) Une valeur, un pointeur pred et un pointeur next
- d) Une valeur et un tableau de pointeurs

<details><summary>Reponse</summary>c) Une valeur, un pointeur pred et un pointeur next - Permet la navigation bidirectionnelle</details>

---

## Question 13
Quel est l'avantage principal d'une liste doublement chainee ?

- a) Utilise moins de memoire
- b) Acces direct par indice
- c) Navigation avant ET arriere
- d) Recherche plus rapide

<details><summary>Reponse</summary>c) Navigation avant ET arriere - Les deux pointeurs permettent de parcourir dans les deux sens</details>

---

## Question 14
**ERREUR A DETECTER** : Apres `Liberer(P)`, on peut toujours acceder a `P^` en toute securite.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Acceder a une zone liberee cause un comportement indefini (pointeur pendant)</details>

---

## Question 15
Pour eviter un pointeur pendant apres liberation, on doit :

- a) Ne jamais liberer de memoire
- b) Mettre le pointeur a NIL apres liberation
- c) Doubler la taille allouee
- d) Utiliser un tableau

<details><summary>Reponse</summary>b) Mettre le pointeur a NIL apres liberation - Cela permet de detecter que le pointeur est invalide</details>

---

## Question 16
Dans l'insertion en tete, quel est l'ordre correct des operations ?

- a) L := P puis P^.next := L
- b) P^.next := L puis L := P
- c) Liberer(L) puis Creer(P)
- d) L^.next := P puis P := L

<details><summary>Reponse</summary>b) P^.next := L puis L := P - Le nouveau noeud pointe vers l'ancien premier, puis devient la tete</details>

---

## Question 17
Comment teste-t-on si une liste chainee L est vide ?

- a) L = 0
- b) L^ = NIL
- c) L = NIL
- d) L^.next = NIL

<details><summary>Reponse</summary>c) L = NIL - Une liste vide n'a pas de premier element, donc L ne pointe vers rien</details>

---

## Question 18
Quelle est la complexite de la recherche dans une liste chainee non triee ?

- a) O(1)
- b) O(log N)
- c) O(N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(N) - Dans le pire cas, il faut parcourir tous les elements</details>

---

## Question 19
Pour supprimer un element au milieu d'une liste simple, on a besoin :

- a) Uniquement du noeud a supprimer
- b) Du noeud a supprimer et de son predecesseur
- c) Du noeud a supprimer et de son successeur
- d) De parcourir toute la liste deux fois

<details><summary>Reponse</summary>b) Du noeud a supprimer et de son predecesseur - Le predecesseur doit pointer vers le successeur du noeud supprime</details>

---

## Question 20
**ERREUR A DETECTER** : Dans une liste doublement chainee, la suppression d'un noeud ne necessite jamais de connaitre le predecesseur.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>a) Vrai - Grace au pointeur pred, on a directement acces au predecesseur depuis le noeud lui-meme</details>

---

## Question 21
Si P et Q sont des pointeurs et on fait `Q := P`, alors :

- a) Les valeurs pointees sont copiees
- b) Q et P pointent vers la meme zone memoire
- c) P devient NIL
- d) Une nouvelle zone est allouee

<details><summary>Reponse</summary>b) Q et P pointent vers la meme zone memoire - C'est une copie d'adresse, pas de valeur</details>

---

## Question 22
Quel est l'inconvenient principal des listes chainees par rapport aux tableaux ?

- a) Taille fixe
- b) Pas d'acces direct par indice (O(N) au lieu de O(1))
- c) Impossible d'inserer des elements
- d) Ne peut stocker que des entiers

<details><summary>Reponse</summary>b) Pas d'acces direct par indice (O(N) au lieu de O(1)) - Il faut parcourir la liste pour atteindre un element</details>

---

## Question 23
Dans une structure `Liste_FL`, que contient le champ `last` ?

- a) Le dernier element insere
- b) Un pointeur vers le dernier noeud
- c) La taille de la liste
- d) La valeur maximale

<details><summary>Reponse</summary>b) Un pointeur vers le dernier noeud - Permet l'insertion en queue en O(1)</details>

---

## Question 24
Pour parcourir une liste chainee, on utilise typiquement :

- a) Une boucle Pour avec indice
- b) Une boucle TantQue avec test sur NIL
- c) Une recursion obligatoire
- d) Un acces direct

<details><summary>Reponse</summary>b) Une boucle TantQue avec test sur NIL - On avance de noeud en noeud jusqu'a atteindre NIL</details>

---

## Question 25
Quelle bonne pratique permet d'eviter les fuites memoire ?

- a) Ne jamais utiliser de pointeurs
- b) Liberer systematiquement la memoire apres usage
- c) Allouer plus de memoire que necessaire
- d) Utiliser uniquement des variables globales

<details><summary>Reponse</summary>b) Liberer systematiquement la memoire apres usage - Chaque Creer() doit avoir son Liberer() correspondant</details>

---

*Total : 25 questions - Formation Algorithmique M2i*
