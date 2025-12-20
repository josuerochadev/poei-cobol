# QCM - Chapitre VI : Gestion des Tables

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Définition et déclaration (Questions 1-8)

### Question 1
Comment appelle-t-on un tableau en COBOL ?

- [ ] A) Array
- [ ] B) Table
- [ ] C) Vector
- [ ] D) Matrix

<details>
<summary>Réponse</summary>

**B** - En COBOL, les tableaux sont appelés **Tables** et se déclarent avec la clause OCCURS.

</details>

---

### Question 2
Quelle clause permet de déclarer une table en COBOL ?

- [ ] A) ARRAY
- [ ] B) TABLE
- [ ] C) OCCURS
- [ ] D) DIMENSION

<details>
<summary>Réponse</summary>

**C** - La clause `OCCURS n TIMES` permet de déclarer une table en multipliant un élément n fois.

</details>

---

### Question 3
À quel niveau peut-on utiliser la clause OCCURS ?

- [ ] A) Niveau 01
- [ ] B) Niveaux 02 à 49
- [ ] C) Niveau 77
- [ ] D) Tous les niveaux

<details>
<summary>Réponse</summary>

**B** - OCCURS est autorisé uniquement aux niveaux 02 à 49. Il est interdit aux niveaux 01 et 77.

</details>

---

### Question 4
Peut-on utiliser VALUE directement sur un élément avec OCCURS ?

- [ ] A) Oui, toujours
- [ ] B) Non, c'est interdit
- [ ] C) Seulement pour les alphanumériques
- [ ] D) Seulement avec INDEXED BY

<details>
<summary>Réponse</summary>

**B** - VALUE est interdit directement sur un OCCURS. On utilise REDEFINES pour initialiser une table.

</details>

---

### Question 5
Quelle est la déclaration correcte d'une table de 12 mois ?

- [ ] A) `01 MOIS OCCURS 12 TIMES PIC X(10).`
- [ ] B) `05 MOIS OCCURS 12 TIMES PIC X(10).`
- [ ] C) `05 MOIS PIC X(10) OCCURS 12.`
- [ ] D) B et C sont valides

<details>
<summary>Réponse</summary>

**D** - Les deux syntaxes sont valides. OCCURS peut être avant ou après le PIC.

</details>

---

### Question 6
Comment accéder au 5ème élément d'une table PRODUIT ?

- [ ] A) `PRODUIT[5]`
- [ ] B) `PRODUIT(5)`
- [ ] C) `PRODUIT.5`
- [ ] D) `PRODUIT{5}`

<details>
<summary>Réponse</summary>

**B** - En COBOL, les indices sont entre parenthèses : `PRODUIT(5)`.

</details>

---

### Question 7
Quel est l'indice du premier élément d'une table COBOL ?

- [ ] A) 0
- [ ] B) 1
- [ ] C) Dépend de la déclaration
- [ ] D) -1

<details>
<summary>Réponse</summary>

**B** - En COBOL, les indices commencent toujours à 1 (contrairement à C/Java qui commencent à 0).

</details>

---

### Question 8
Comment initialiser une table avec les noms des mois ?

- [ ] A) Avec VALUE sur chaque OCCURS
- [ ] B) Avec REDEFINES et des FILLER avec VALUE
- [ ] C) Avec INITIALIZE
- [ ] D) C'est impossible

<details>
<summary>Réponse</summary>

**B** - On déclare une structure avec des FILLER ayant des VALUE, puis on la redéfinit comme table avec OCCURS.

</details>

---

## Section 2 : Tables multidimensionnelles (Questions 9-13)

### Question 9
Comment accéder à un élément d'une table 2D en COBOL ?

- [ ] A) `TABLE[i][j]`
- [ ] B) `TABLE(i,j)`
- [ ] C) `TABLE(i)(j)`
- [ ] D) `TABLE.i.j`

<details>
<summary>Réponse</summary>

**B** - Les indices multiples sont séparés par des virgules : `TABLE(i,j)`.

</details>

---

### Question 10
Dans une table 2D, quel indice représente la dimension externe ?

- [ ] A) Le premier
- [ ] B) Le deuxième
- [ ] C) Le dernier
- [ ] D) Aucun

<details>
<summary>Réponse</summary>

**A** - Le premier indice correspond à la table externe (englobante), le second à la table interne (imbriquée).

</details>

---

### Question 11
Comment déclarer une table 2D de 4 régions avec 4 trimestres chacune ?

- [ ] A) `05 VENTES OCCURS 4 TIMES OCCURS 4 TIMES PIC 9(6).`
- [ ] B) `05 REGION OCCURS 4 TIMES. 10 TRIM OCCURS 4 TIMES PIC 9(6).`
- [ ] C) `05 VENTES(4,4) PIC 9(6).`
- [ ] D) Impossible en COBOL

<details>
<summary>Réponse</summary>

**B** - On imbrique un OCCURS dans un autre avec des niveaux différents.

</details>

---

### Question 12
Combien d'indices faut-il pour accéder à un élément d'une table 3D ?

- [ ] A) 1
- [ ] B) 2
- [ ] C) 3
- [ ] D) Dépend de la structure

<details>
<summary>Réponse</summary>

**C** - Une table 3D nécessite 3 indices : `ELEMENT(i, j, k)`.

</details>

---

### Question 13
Dans `VAR3(2,4)` sur une table 2D, que représente le "4" ?

- [ ] A) Le 4ème élément de la table externe
- [ ] B) Le 4ème élément de la table interne du 2ème élément externe
- [ ] C) La taille de la table
- [ ] D) Un indice invalide

<details>
<summary>Réponse</summary>

**B** - Le premier indice (2) sélectionne dans la table externe, le second (4) sélectionne dans la table interne.

</details>

---

## Section 3 : DEPENDING ON (Questions 14-16)

### Question 14
À quoi sert la clause DEPENDING ON ?

- [ ] A) Trier la table
- [ ] B) Définir une table à taille variable
- [ ] C) Indexer la table
- [ ] D) Initialiser la table

<details>
<summary>Réponse</summary>

**B** - `OCCURS 1 TO n TIMES DEPENDING ON var` crée une table dont la taille logique varie selon var.

</details>

---

### Question 15
Où doit être déclarée la variable de contrôle DEPENDING ON ?

- [ ] A) Après la table
- [ ] B) Avant la table
- [ ] C) Dans la PROCEDURE DIVISION
- [ ] D) Peu importe

<details>
<summary>Réponse</summary>

**B** - La variable de contrôle doit être déclarée AVANT la table dans la DATA DIVISION.

</details>

---

### Question 16
Si `OCCURS 1 TO 50 DEPENDING ON NB`, combien de mémoire est allouée ?

- [ ] A) Pour 1 élément
- [ ] B) Pour NB éléments
- [ ] C) Pour 50 éléments (maximum)
- [ ] D) Dynamiquement selon NB

<details>
<summary>Réponse</summary>

**C** - La mémoire est allouée pour le maximum (50), mais seuls NB éléments sont considérés logiquement.

</details>

---

## Section 4 : Indice vs Index (Questions 17-21)

### Question 17
Quelle est la différence principale entre un indice et un index ?

- [ ] A) Aucune différence
- [ ] B) L'indice est une variable PIC 9, l'index contient un déplacement en octets
- [ ] C) L'index est plus lent
- [ ] D) L'indice est déclaré avec INDEXED BY

<details>
<summary>Réponse</summary>

**B** - L'indice est un numéro (1, 2, 3...) qu'il faut convertir. L'index contient directement le déplacement en octets (plus performant).

</details>

---

### Question 18
Comment déclarer un index sur une table ?

- [ ] A) `01 IDX PIC 9(4).`
- [ ] B) `05 ELEM OCCURS 10 TIMES INDEXED BY IDX.`
- [ ] C) `INDEX IDX FOR ELEM.`
- [ ] D) `DEFINE INDEX IDX.`

<details>
<summary>Réponse</summary>

**B** - L'index est déclaré avec `INDEXED BY nom-index` dans la clause OCCURS.

</details>

---

### Question 19
Comment incrémenter un index de 1 ?

- [ ] A) `ADD 1 TO IDX`
- [ ] B) `IDX = IDX + 1`
- [ ] C) `SET IDX UP BY 1`
- [ ] D) `COMPUTE IDX = IDX + 1`

<details>
<summary>Réponse</summary>

**C** - Les index se manipulent UNIQUEMENT avec l'instruction SET. ADD/COMPUTE ne fonctionnent pas.

</details>

---

### Question 20
Comment initialiser un index à 1 ?

- [ ] A) `MOVE 1 TO IDX`
- [ ] B) `SET IDX TO 1`
- [ ] C) `IDX = 1`
- [ ] D) `INITIALIZE IDX`

<details>
<summary>Réponse</summary>

**B** - `SET IDX TO 1` initialise l'index. MOVE ne fonctionne pas avec les index.

</details>

---

### Question 21
Comment afficher la position d'un index ?

- [ ] A) `DISPLAY IDX`
- [ ] B) Copier dans une variable puis afficher : `SET WS-I TO IDX` puis `DISPLAY WS-I`
- [ ] C) `PRINT IDX`
- [ ] D) Impossible

<details>
<summary>Réponse</summary>

**B** - On ne peut pas afficher directement un index. Il faut d'abord le copier dans une variable numérique avec SET.

</details>

---

## Section 5 : SEARCH et SET (Questions 22-25)

### Question 22
Quelle est la différence entre SEARCH et SEARCH ALL ?

- [ ] A) Aucune différence
- [ ] B) SEARCH = séquentielle O(n), SEARCH ALL = binaire O(log n)
- [ ] C) SEARCH ALL est plus lent
- [ ] D) SEARCH ALL ne nécessite pas de tri

<details>
<summary>Réponse</summary>

**B** - SEARCH fait une recherche séquentielle. SEARCH ALL fait une recherche binaire (plus rapide mais nécessite une table triée avec KEY).

</details>

---

### Question 23
Que faut-il faire avant d'utiliser SEARCH (non ALL) ?

- [ ] A) Trier la table
- [ ] B) Initialiser l'index à 1 avec SET
- [ ] C) Déclarer une clé
- [ ] D) Rien de particulier

<details>
<summary>Réponse</summary>

**B** - Avant SEARCH, il faut positionner l'index au début avec `SET idx TO 1`.

</details>

---

### Question 24
Que fait `SET IDX DOWN BY 2` ?

- [ ] A) Initialise IDX à 2
- [ ] B) Recule de 2 positions dans la table
- [ ] C) Divise IDX par 2
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**B** - `SET idx DOWN BY n` recule de n positions (décrémente).

</details>

---

### Question 25
Quel prérequis pour utiliser SEARCH ALL ?

- [ ] A) Table non triée
- [ ] B) Table triée + clause KEY dans OCCURS
- [ ] C) Indice au lieu d'index
- [ ] D) Table à taille variable

<details>
<summary>Réponse</summary>

**B** - SEARCH ALL (recherche binaire) nécessite une table triée et la clause `ASCENDING/DESCENDING KEY IS` dans la déclaration OCCURS.

</details>

---

## Résumé des scores

| Score | Niveau |
|-------|--------|
| 23-25 | Excellent - Maîtrise complète |
| 18-22 | Bien - Quelques révisions mineures |
| 13-17 | Moyen - Relire le chapitre |
| < 13 | Insuffisant - Revoir le cours en détail |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [QCM Chapitre V](qcm-05-traitement-conditionnel.md) | [QCM Chapitre VII](qcm-07-gestion-fichiers.md) |

---
*Formation COBOL - M2i Formation*
