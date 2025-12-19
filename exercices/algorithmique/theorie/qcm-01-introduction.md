# QCM Chapitre I - Introduction a l'Algorithmique

*Questions sur les concepts fondamentaux de l'algorithmique*

---

## Question 1
Quel est le resultat de l'operation `7 MOD 3` ?

- a) 2
- b) 1
- c) 3
- d) 0

<details><summary>Reponse</summary>b) 1 - Le MOD retourne le reste de la division entiere (7 = 3 × 2 + 1)</details>

---

## Question 2
Quelle structure de boucle garantit au moins une execution du bloc d'instructions ?

- a) Pour...finPour
- b) TantQue...finTantQue
- c) Repeter...Jusqu'a
- d) Si...finSi

<details><summary>Reponse</summary>c) Repeter...Jusqu'a - Le test est effectue APRES l'execution, donc le bloc s'execute au moins une fois</details>

---

## Question 3
Pour calculer une factorielle, quelle valeur initiale doit avoir l'accumulateur ?

- a) 0
- b) 1
- c) n
- d) -1

<details><summary>Reponse</summary>b) 1 - C'est l'element neutre de la multiplication (0 donnerait toujours 0)</details>

---

## Question 4
Quelle est la valeur de `5^0` (5 a la puissance 0) ?

- a) 0
- b) 5
- c) 1
- d) Indefini

<details><summary>Reponse</summary>c) 1 - Par definition mathematique, tout nombre (sauf 0) eleve a la puissance 0 vaut 1</details>

---

## Question 5
Pour detecter si un nombre est pair, on utilise :

- a) `n DIV 2 = 0`
- b) `n MOD 2 = 0`
- c) `n / 2 = 0`
- d) `n * 2 = n`

<details><summary>Reponse</summary>b) n MOD 2 = 0 - Si le reste de la division par 2 est 0, le nombre est pair</details>

---

## Question 6
**ERREUR A DETECTER** : La boucle `Pour i de 1 a 0 Faire` s'execute une fois.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Une boucle Pour avec debut > fin ne s'execute jamais (0 iterations)</details>

---

## Question 7
Quel est le resultat de `10 DIV 3` ?

- a) 3.33
- b) 3
- c) 1
- d) 4

<details><summary>Reponse</summary>b) 3 - DIV effectue une division ENTIERE (partie entiere de 10/3)</details>

---

## Question 8
Pour initialiser un minimum absolu, on utilise generalement :

- a) 0
- b) -1
- c) Le premier element lu
- d) +infini

<details><summary>Reponse</summary>d) +infini (ou c si on lit le premier element) - Tout nombre sera inferieur a +infini</details>

---

## Question 9
Quelle est la somme des 3 premiers nombres impairs (1 + 3 + 5) ?

- a) 8
- b) 9
- c) 15
- d) 6

<details><summary>Reponse</summary>b) 9 - La somme des n premiers impairs = n^2 (ici 3^2 = 9)</details>

---

## Question 10
Dans l'algorithme du PGCD par soustractions, si A = 48 et B = 18, quelle est la premiere operation ?

- a) B := B - A
- b) A := A - B
- c) A := A / B
- d) B := A MOD B

<details><summary>Reponse</summary>b) A := A - B (car A > B), donc A devient 48 - 18 = 30</details>

---

## Question 11
**ERREUR A DETECTER** : `TantQue x > 0 Faire x := x - 1 finTantQue` avec x = 0 s'execute une fois.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - TantQue teste la condition AVANT l'execution. Si x = 0, la condition est fausse donc aucune execution</details>

---

## Question 12
Quel operateur logique verifie que deux conditions sont vraies simultanement ?

- a) OU
- b) ET
- c) NON
- d) XOR

<details><summary>Reponse</summary>b) ET - Retourne vrai uniquement si les deux conditions sont vraies</details>

---

## Question 13
Pour compter les parentheses dans une expression, on utilise un compteur. Si on rencontre '(' :

- a) On decremente le compteur
- b) On incremente le compteur
- c) On remet le compteur a 0
- d) On multiplie le compteur par 2

<details><summary>Reponse</summary>b) On incremente le compteur (+1 pour '(', -1 pour ')')</details>

---

## Question 14
**ERREUR A DETECTER** : L'expression `(3 + 5)) * 2` est valide car elle contient le meme nombre de parentheses ouvrantes et fermantes.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Il y a 1 '(' et 2 ')'. Meme avec un nombre egal, l'ordre compte : on ne peut pas fermer avant d'ouvrir</details>

---

## Question 15
Quelle est la valeur de 4! (factorielle de 4) ?

- a) 4
- b) 10
- c) 24
- d) 16

<details><summary>Reponse</summary>c) 24 - 4! = 1 × 2 × 3 × 4 = 24</details>

---

*Total : 15 questions - Formation Algorithmique M2i*
