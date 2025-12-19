# QCM Chapitre II - Structures de Donnees

*Questions sur les tableaux, enregistrements et algorithmes de recherche*

---

## Question 1
Quelle est la caracteristique principale d'un tableau unidimensionnel ?

- a) Les elements peuvent etre de types differents
- b) Les elements sont stockes de maniere contigue en memoire
- c) La taille peut varier dynamiquement
- d) L'acces se fait uniquement de maniere sequentielle

<details><summary>Reponse</summary>b) Les elements sont stockes de maniere contigue en memoire - C'est ce qui permet un acces direct par indice en O(1)</details>

---

## Question 2
Pour un tableau `T : tableau [1..10] de entier`, quel est l'indice du dernier element ?

- a) 0
- b) 9
- c) 10
- d) 11

<details><summary>Reponse</summary>c) 10 - L'indice va de 1 (borne inferieure) a 10 (borne superieure)</details>

---

## Question 3
Dans un tableau extensible, que represente N si `T : tableau [1..MAX]` ?

- a) La taille maximale du tableau
- b) Le nombre d'elements reellement utilises
- c) L'indice du premier element
- d) La valeur du dernier element

<details><summary>Reponse</summary>b) Le nombre d'elements reellement utilises - N est la taille logique, toujours <= MAX</details>

---

## Question 4
Pour inverser un tableau de N elements, combien d'echanges faut-il effectuer ?

- a) N
- b) N - 1
- c) N / 2
- d) N * 2

<details><summary>Reponse</summary>c) N / 2 - On echange les elements symetriques, chaque echange traite 2 elements</details>

---

## Question 5
Quelle est la complexite de la recherche dichotomique ?

- a) O(1)
- b) O(N)
- c) O(log N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(log N) - A chaque iteration, l'espace de recherche est divise par 2</details>

---

## Question 6
La recherche dichotomique necessite que le tableau soit :

- a) De taille paire
- b) Trie
- c) Compose d'entiers positifs
- d) Completement rempli

<details><summary>Reponse</summary>b) Trie - Sans tri, on ne peut pas savoir dans quelle moitie chercher</details>

---

## Question 7
Dans la recherche dichotomique, si `E[m] < x`, que fait-on ?

- a) On cherche dans la moitie gauche (f := m - 1)
- b) On cherche dans la moitie droite (d := m + 1)
- c) On arrete la recherche
- d) On recommence depuis le debut

<details><summary>Reponse</summary>b) On cherche dans la moitie droite (d := m + 1) - Si E[m] < x, alors x est plus grand et se trouve a droite</details>

---

## Question 8
Pour ajouter un element x a la fin d'un tableau non ordonne de taille N :

- a) `T[N] := x`
- b) `N := N + 1` puis `T[N] := x`
- c) `T[N] := x` puis `N := N + 1`
- d) `T[1] := x`

<details><summary>Reponse</summary>b) N := N + 1 puis T[N] := x - On incremente d'abord la taille puis on place l'element</details>

---

## Question 9
Quelle est la complexite de l'ajout dans un tableau ordonne (avec maintien de l'ordre) ?

- a) O(1)
- b) O(log N)
- c) O(N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(N) - Il faut decaler les elements pour inserer au bon endroit</details>

---

## Question 10
Pour supprimer un element d'un tableau NON ordonne en O(1), on :

- a) Decale tous les elements suivants
- b) Remplace l'element par le dernier et decremente N
- c) Met l'element a 0
- d) C'est impossible en O(1)

<details><summary>Reponse</summary>b) Remplace l'element par le dernier et decremente N - Cette astuce evite le decalage</details>

---

## Question 11
**ERREUR A DETECTER** : Dans un tableau ordonne, la suppression d'un element se fait en O(1).

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Dans un tableau ordonne, il faut decaler les elements, ce qui est en O(N)</details>

---

## Question 12
Comment accede-t-on a l'element ligne 3, colonne 5 d'une matrice T ?

- a) `T[3][5]`
- b) `T[3, 5]`
- c) `T[5, 3]`
- d) `T(3)(5)`

<details><summary>Reponse</summary>b) T[3, 5] - En notation algorithmique, on utilise une seule paire de crochets avec virgule</details>

---

## Question 13
Un enregistrement permet de :

- a) Stocker uniquement des entiers
- b) Regrouper des donnees de types differents
- c) Creer un tableau dynamique
- d) Effectuer des calculs mathematiques

<details><summary>Reponse</summary>b) Regrouper des donnees de types differents - Contrairement aux tableaux qui sont homogenes</details>

---

## Question 14
Pour acceder au champ "jour" d'un enregistrement Date dans une variable D :

- a) `D(jour)`
- b) `D[jour]`
- c) `D.jour`
- d) `jour.D`

<details><summary>Reponse</summary>c) D.jour - La notation point permet d'acceder aux champs d'un enregistrement</details>

---

## Question 15
Si E est de type Etudiant avec un champ `dn` de type Date, comment acceder a l'annee de naissance ?

- a) `E.an`
- b) `E.dn.an`
- c) `E[dn][an]`
- d) `an.dn.E`

<details><summary>Reponse</summary>b) E.dn.an - On navigue dans les structures imbriquees avec des points successifs</details>

---

## Question 16
Quelle primitive retourne la longueur d'une chaine de caracteres ?

- a) `Pos(ch)`
- b) `Long(ch)`
- c) `Taille(ch)`
- d) `Len(ch)`

<details><summary>Reponse</summary>b) Long(ch) - Retourne le nombre de caracteres de la chaine</details>

---

## Question 17
La recherche sequentielle dans un tableau non ordonne a une complexite de :

- a) O(1)
- b) O(log N)
- c) O(N)
- d) O(N^2)

<details><summary>Reponse</summary>c) O(N) - Dans le pire cas, on doit parcourir tout le tableau</details>

---

## Question 18
**ERREUR A DETECTER** : La recherche dichotomique fonctionne sur n'importe quel tableau.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Elle ne fonctionne QUE sur les tableaux tries</details>

---

## Question 19
Dans l'algorithme de recherche de chaine naive, que fait `i := i - j + 2` ?

- a) Avance d'une position
- b) Retourne au debut du texte
- c) Retourne a la position suivant le debut du dernier essai
- d) Passe au caractere suivant du motif

<details><summary>Reponse</summary>c) Retourne a la position suivant le debut du dernier essai - Pour recommencer la comparaison decalee d'un caractere</details>

---

## Question 20
Pour un tableau de 1000 elements tries, combien de comparaisons au maximum pour une recherche dichotomique ?

- a) 1000
- b) 500
- c) 100
- d) 10

<details><summary>Reponse</summary>d) 10 - log2(1000) ≈ 10 (2^10 = 1024)</details>

---

## Question 21
Quel est l'equivalent COBOL de `T : tableau [1..10] de entier` ?

- a) `01 T PIC 9(10).`
- b) `01 T OCCURS 10 TIMES PIC 9(4).`
- c) `01 T VALUE 10.`
- d) `01 T ARRAY(10).`

<details><summary>Reponse</summary>b) 01 T OCCURS 10 TIMES PIC 9(4). - OCCURS definit la repetition en COBOL</details>

---

## Question 22
Dans la recherche sequentielle version "premiere occurrence", pourquoi utilise-t-on un drapeau booleen ?

- a) Pour compter les occurrences
- b) Pour arreter apres la premiere correspondance trouvee
- c) Pour inverser le tableau
- d) Pour trier le tableau

<details><summary>Reponse</summary>b) Pour arreter apres la premiere correspondance trouvee - Le drapeau passe a faux des qu'on trouve, empechant les mises a jour suivantes</details>

---

## Question 23
**ERREUR A DETECTER** : L'ajout d'un element dans un tableau non ordonne necessite de decaler les elements existants.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Dans un tableau NON ordonne, on ajoute simplement a la fin en O(1)</details>

---

## Question 24
Pour un tableau ordonne, quelle operation est la plus efficace ?

- a) Ajout
- b) Suppression
- c) Recherche
- d) Inversion

<details><summary>Reponse</summary>c) Recherche - Grace a la dichotomie en O(log N), contre O(N) pour ajout/suppression</details>

---

## Question 25
Comment se calcule l'indice milieu dans la recherche dichotomique ?

- a) `m := (d - f) / 2`
- b) `m := d + f`
- c) `m := (d + f) DIV 2`
- d) `m := N / 2`

<details><summary>Reponse</summary>c) m := (d + f) DIV 2 - Division entiere de la somme des bornes</details>

---

*Total : 25 questions - Formation Algorithmique M2i*
