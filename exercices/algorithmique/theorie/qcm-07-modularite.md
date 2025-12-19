# QCM Chapitre VII - Modularite et Sous-programmes

*Questions sur la conception modulaire, procedures, fonctions et parametres*

---

## Question 1
Qu'est-ce que la modularite en programmation ?

- a) Ecrire du code en une seule ligne
- b) Decomposer un programme en sous-programmes independants
- c) Utiliser uniquement des variables globales
- d) Eviter les boucles

<details><summary>Reponse</summary>b) Decomposer un programme en sous-programmes independants - Diviser pour mieux regner</details>

---

## Question 2
Quel est un avantage de la modularite ?

- a) Code plus long
- b) Plus difficile a maintenir
- c) Reutilisation des modules
- d) Execution plus lente

<details><summary>Reponse</summary>c) Reutilisation des modules - Un module peut etre appele plusieurs fois</details>

---

## Question 3
Quelle est la difference entre une procedure et une fonction ?

- a) Aucune difference
- b) Une fonction retourne une valeur, pas une procedure
- c) Une procedure a des parametres, pas une fonction
- d) Une fonction est plus rapide

<details><summary>Reponse</summary>b) Une fonction retourne une valeur, pas une procedure - Distinction fondamentale</details>

---

## Question 4
Comment appelle-t-on les parametres dans la definition d'un sous-programme ?

- a) Parametres effectifs
- b) Parametres formels
- c) Parametres reels
- d) Parametres virtuels

<details><summary>Reponse</summary>b) Parametres formels - Les parametres dans la declaration</details>

---

## Question 5
Comment appelle-t-on les valeurs passees lors de l'appel ?

- a) Parametres formels
- b) Parametres effectifs
- c) Parametres nominaux
- d) Parametres abstraits

<details><summary>Reponse</summary>b) Parametres effectifs - Les valeurs reelles a l'appel</details>

---

## Question 6
Dans le passage par valeur, que recoit le sous-programme ?

- a) L'adresse de la variable
- b) Une copie de la valeur
- c) Le nom de la variable
- d) Un pointeur

<details><summary>Reponse</summary>b) Une copie de la valeur - La variable originale n'est pas modifiee</details>

---

## Question 7
Dans le passage par reference, que recoit le sous-programme ?

- a) Une copie de la valeur
- b) L'adresse de la variable
- c) Le type de la variable
- d) Rien

<details><summary>Reponse</summary>b) L'adresse de la variable - Permet de modifier l'original</details>

---

## Question 8
Si `x = 5` et on appelle `Double(x)` par valeur, que vaut x apres l'appel ?

- a) 10
- b) 5
- c) 0
- d) Erreur

<details><summary>Reponse</summary>b) 5 - Le passage par valeur ne modifie pas l'original</details>

---

## Question 9
Si `x = 5` et on appelle `Double(x)` par reference, que vaut x apres l'appel (si Double multiplie par 2) ?

- a) 5
- b) 10
- c) 0
- d) Erreur

<details><summary>Reponse</summary>b) 10 - Le passage par reference modifie l'original</details>

---

## Question 10
Quelle notation indique un passage par reference ?

- a) `x : entier`
- b) `var x : entier`
- c) `ref x : entier`
- d) `&x : entier`

<details><summary>Reponse</summary>b) var x : entier - Le mot-cle "var" indique la reference</details>

---

## Question 11
Quand utiliser le passage par reference ?

- a) Pour les parametres en entree uniquement
- b) Pour les parametres en sortie ou entree/sortie
- c) Jamais
- d) Toujours

<details><summary>Reponse</summary>b) Pour les parametres en sortie ou entree/sortie - Quand on veut modifier la variable</details>

---

## Question 12
Qu'est-ce qu'une variable locale ?

- a) Une variable accessible partout
- b) Une variable declaree dans un sous-programme
- c) Une variable sans type
- d) Une variable constante

<details><summary>Reponse</summary>b) Une variable declaree dans un sous-programme - Accessible uniquement dans ce sous-programme</details>

---

## Question 13
Qu'est-ce qu'une variable globale ?

- a) Une variable declaree dans un sous-programme
- b) Une variable accessible depuis tout le programme
- c) Une variable en majuscules
- d) Une variable tres grande

<details><summary>Reponse</summary>b) Une variable accessible depuis tout le programme - Declaree hors des sous-programmes</details>

---

## Question 14
**ERREUR A DETECTER** : Les variables globales sont recommandees car elles simplifient le code.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Les globales creent des effets de bord et rendent le code difficile a maintenir</details>

---

## Question 15
Qu'est-ce que le masquage de variable ?

- a) Cacher une variable a l'utilisateur
- b) Une variable locale portant le meme nom qu'une globale
- c) Supprimer une variable
- d) Renommer une variable

<details><summary>Reponse</summary>b) Une variable locale portant le meme nom qu'une globale - La locale "cache" la globale</details>

---

## Question 16
Qu'est-ce que la conception Top-Down ?

- a) Commencer par les details
- b) Partir du probleme global et decomposer
- c) Coder sans planifier
- d) Tester avant de coder

<details><summary>Reponse</summary>b) Partir du probleme global et decomposer - Decomposition descendante</details>

---

## Question 17
Qu'est-ce que la "cohesion forte" d'un module ?

- a) Le module fait beaucoup de choses
- b) Le module fait UNE seule chose bien definie
- c) Le module est tres long
- d) Le module utilise beaucoup de globales

<details><summary>Reponse</summary>b) Le module fait UNE seule chose bien definie - Principe de responsabilite unique</details>

---

## Question 18
Qu'est-ce que le "couplage faible" entre modules ?

- a) Les modules dependent fortement les uns des autres
- b) Les modules sont independants
- c) Les modules partagent des variables globales
- d) Les modules ont le meme nom

<details><summary>Reponse</summary>b) Les modules sont independants - Modifier un module n'affecte pas les autres</details>

---

## Question 19
Quelle taille est recommandee pour un module ?

- a) 1-5 lignes
- b) 20-50 lignes
- c) 500-1000 lignes
- d) Pas de limite

<details><summary>Reponse</summary>b) 20-50 lignes - Maximum 100, sinon decomposer</details>

---

## Question 20
En COBOL, quelle instruction appelle un paragraphe interne ?

- a) CALL
- b) PERFORM
- c) INVOKE
- d) EXECUTE

<details><summary>Reponse</summary>b) PERFORM - Pour les procedures internes</details>

---

## Question 21
En COBOL, quelle instruction appelle un programme externe ?

- a) PERFORM
- b) CALL
- c) RUN
- d) LINK

<details><summary>Reponse</summary>b) CALL - Pour les sous-programmes externes</details>

---

## Question 22
En COBOL, que signifie `BY REFERENCE` ?

- a) Passage par valeur
- b) Passage par reference (adresse)
- c) Passage par nom
- d) Pas de passage

<details><summary>Reponse</summary>b) Passage par reference (adresse) - C'est le mode par defaut en COBOL</details>

---

## Question 23
En COBOL, que signifie `BY CONTENT` ?

- a) Passage par reference
- b) Passage par valeur (copie)
- c) Passage par fichier
- d) Pas de passage

<details><summary>Reponse</summary>b) Passage par valeur (copie) - Le sous-programme recoit une copie</details>

---

## Question 24
Quelle notation indique un parametre en entree dans la documentation ?

- a) (S)
- b) (E)
- c) (E/S)
- d) (IN)

<details><summary>Reponse</summary>b) (E) - E pour Entree</details>

---

## Question 25
**ERREUR A DETECTER** : Une fonction peut modifier plusieurs variables et ne rien retourner.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Une fonction retourne UNE valeur, c'est une procedure qui ne retourne rien</details>

---

## Question 26
Quel est l'avantage de documenter les sous-programmes ?

- a) Le code s'execute plus vite
- b) Facilite la maintenance et la comprehension
- c) Reduit la taille du code
- d) Aucun avantage

<details><summary>Reponse</summary>b) Facilite la maintenance et la comprehension - Essentiel pour le travail d'equipe</details>

---

## Question 27
Comment passe-t-on un tableau en parametre ?

- a) Toujours par valeur
- b) Toujours par reference
- c) Jamais
- d) Par nom

<details><summary>Reponse</summary>b) Toujours par reference - Pour eviter la copie couteuse</details>

---

## Question 28
Qu'est-ce que l'abstraction en programmation modulaire ?

- a) Montrer tous les details
- b) Cacher les details d'implementation
- c) Supprimer du code
- d) Ajouter des commentaires

<details><summary>Reponse</summary>b) Cacher les details d'implementation - L'utilisateur voit l'interface, pas le code interne</details>

---

## Question 29
Pourquoi preferer les parametres aux variables globales ?

- a) Plus rapide
- b) Plus explicite, moins d'effets de bord
- c) Plus court
- d) Obligatoire

<details><summary>Reponse</summary>b) Plus explicite, moins d'effets de bord - On voit clairement ce qui entre et sort</details>

---

## Question 30
**ERREUR A DETECTER** : Un sous-programme peut avoir autant de parametres qu'on veut sans limite pratique.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Trop de parametres rend le code difficile a utiliser (5-7 max recommandes)</details>

---

*Total : 30 questions - Formation Algorithmique M2i*
