# QCM - Chapitre II : Interface ISPF et Commandes de Base

## Instructions

- **20 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

> **Note** : Pour approfondir ISPF, consultez également le [QCM ISPF du module z/OS-TSO](../../zos-tso/theorie/).

---

## Section 1 : Présentation d'ISPF (Questions 1-5)

### Question 1
Que signifie l'acronyme ISPF ?

- [ ] A) Interactive System Programming Facility
- [ ] B) Interactive System Productivity Facility
- [ ] C) Integrated System Production Facility
- [ ] D) Internal System Processing Facility

<details>
<summary>Réponse</summary>

**B** - ISPF = **I**nteractive **S**ystem **P**roductivity **F**acility.

</details>

---

### Question 2
Quelle option du menu principal ISPF permet d'éditer un programme COBOL ?

- [ ] A) Option 0 - Settings
- [ ] B) Option 1 - View
- [ ] C) Option 2 - Edit
- [ ] D) Option 3 - Utilities

<details>
<summary>Réponse</summary>

**C** - L'option 2 (Edit) permet de créer et modifier le code source.

</details>

---

### Question 3
Quelle option permet de voir les résultats des jobs soumis ?

- [ ] A) Option 3 - Utilities
- [ ] B) Option S - SDSF
- [ ] C) Option X - Exit
- [ ] D) Option 1 - View

<details>
<summary>Réponse</summary>

**B** - SDSF (Spool Display and Search Facility) permet de consulter les résultats des jobs.

</details>

---

### Question 4
Qu'est-ce qu'un PDS en environnement mainframe ?

- [ ] A) Program Data System
- [ ] B) Partitioned Data Set
- [ ] C) Primary Data Storage
- [ ] D) Parallel Data Structure

<details>
<summary>Réponse</summary>

**B** - PDS = Partitioned Data Set. C'est l'équivalent d'un dossier contenant des membres (fichiers).

</details>

---

### Question 5
Quelle touche de fonction permet de quitter/retourner en arrière dans ISPF ?

- [ ] A) F1
- [ ] B) F2
- [ ] C) F3
- [ ] D) F7

<details>
<summary>Réponse</summary>

**C** - F3 = Exit/End, permet de quitter l'écran courant et revenir en arrière.

</details>

---

## Section 2 : Commandes ligne de l'éditeur (Questions 6-12)

### Question 6
Quelle commande ligne permet d'insérer 5 nouvelles lignes ?

- [ ] A) `A5`
- [ ] B) `I5`
- [ ] C) `N5`
- [ ] D) `+5`

<details>
<summary>Réponse</summary>

**B** - `I5` insère 5 lignes vides après la ligne courante. I = Insert.

</details>

---

### Question 7
Quelle commande ligne permet de supprimer une ligne ?

- [ ] A) `S`
- [ ] B) `X`
- [ ] C) `D`
- [ ] D) `R`

<details>
<summary>Réponse</summary>

**C** - `D` = Delete, supprime la ligne. `D3` supprime 3 lignes.

</details>

---

### Question 8
Pour copier un bloc de lignes, quelles commandes utilise-t-on ?

- [ ] A) `C` au début et `C` à la fin du bloc
- [ ] B) `CC` au début et `CC` à la fin du bloc
- [ ] C) `COPY` sur la première ligne
- [ ] D) `CB` au début et `CE` à la fin

<details>
<summary>Réponse</summary>

**B** - On utilise `CC` sur la première ligne du bloc et `CC` sur la dernière ligne pour délimiter le bloc à copier.

</details>

---

### Question 9
Après avoir marqué des lignes avec `CC...CC`, où faut-il indiquer la destination ?

- [ ] A) Avec la commande `TO` sur la ligne de destination
- [ ] B) Avec `A` (After) ou `B` (Before) sur la ligne de destination
- [ ] C) Avec `D` (Destination) sur la ligne cible
- [ ] D) La copie se fait automatiquement à la fin du fichier

<details>
<summary>Réponse</summary>

**B** - On utilise `A` pour insérer après la ligne ou `B` pour insérer avant la ligne de destination.

</details>

---

### Question 10
Quelle commande ligne permet de dupliquer une ligne 4 fois ?

- [ ] A) `C4`
- [ ] B) `D4`
- [ ] C) `R4`
- [ ] D) `M4`

<details>
<summary>Réponse</summary>

**C** - `R4` = Repeat 4 fois. La ligne sera dupliquée 4 fois (donc 5 lignes identiques au total).

</details>

---

### Question 11
Quelle est la différence entre `M` et `C` ?

- [ ] A) `M` = Move (déplace), `C` = Copy (copie)
- [ ] B) `M` = Multiple, `C` = Clear
- [ ] C) `M` = Modify, `C` = Change
- [ ] D) Il n'y a pas de différence

<details>
<summary>Réponse</summary>

**A** - `M` déplace la ligne (elle disparaît de son emplacement d'origine), `C` la copie (l'original reste en place).

</details>

---

### Question 12
Que fait la commande ligne `O` (Overlay) ?

- [ ] A) Ouvre un nouveau fichier
- [ ] B) Copie par superposition sur une autre ligne
- [ ] C) Supprime les caractères en trop
- [ ] D) Affiche les options disponibles

<details>
<summary>Réponse</summary>

**B** - `O` (Overlay) permet de copier une ligne en la superposant sur une autre, les espaces de la source ne remplaçant pas les caractères de la destination.

</details>

---

## Section 3 : Commandes primaires (Questions 13-16)

### Question 13
Quelle commande permet de soumettre un JCL pour exécution ?

- [ ] A) `RUN`
- [ ] B) `EXEC`
- [ ] C) `SUB` ou `SUBMIT`
- [ ] D) `GO`

<details>
<summary>Réponse</summary>

**C** - `SUB` ou `SUBMIT` soumet le JCL pour exécution sur le système.

</details>

---

### Question 14
Quelle commande primaire permet d'effacer les messages d'avertissement affichés ?

- [ ] A) `CLEAR`
- [ ] B) `RESET`
- [ ] C) `REFRESH`
- [ ] D) `CLEAN`

<details>
<summary>Réponse</summary>

**B** - `RESET` réinitialise l'affichage et efface les messages Warning.

</details>

---

### Question 15
Comment rechercher le texte "DISPLAY" dans un fichier ?

- [ ] A) `SEARCH 'DISPLAY'`
- [ ] B) `FIND 'DISPLAY'`
- [ ] C) `LOOK 'DISPLAY'`
- [ ] D) `F5 DISPLAY`

<details>
<summary>Réponse</summary>

**B** - `FIND 'texte'` permet de rechercher une chaîne dans le fichier.

</details>

---

### Question 16
Quelle commande affiche la règle des colonnes dans l'éditeur ?

- [ ] A) `RULER`
- [ ] B) `COLUMNS`
- [ ] C) `COLS`
- [ ] D) `NUMBERS`

<details>
<summary>Réponse</summary>

**C** - `COLS` affiche une ligne avec les numéros de colonnes pour faciliter le positionnement du code COBOL.

</details>

---

## Section 4 : Compilation et exécution (Questions 17-20)

### Question 17
Quel Return Code indique une compilation réussie sans erreur ni warning ?

- [ ] A) RC = -1
- [ ] B) RC = 0
- [ ] C) RC = 4
- [ ] D) RC = 8

<details>
<summary>Réponse</summary>

**B** - RC = 0 indique un succès complet. RC = 4 indique des warnings, RC = 8 ou plus indique des erreurs.

</details>

---

### Question 18
Dans SDSF, quelle commande permet d'afficher la file des sorties (Output Queue) ?

- [ ] A) `Q`
- [ ] B) `O`
- [ ] C) `OUT`
- [ ] D) `LIST`

<details>
<summary>Réponse</summary>

**B** - `O` affiche l'Output Queue où se trouvent les résultats des jobs terminés.

</details>

---

### Question 19
Dans un JCL de compilation COBOL, que spécifie la carte `//COBOL.SYSIN DD` ?

- [ ] A) Le fichier de sortie des erreurs
- [ ] B) Le programme source à compiler
- [ ] C) La bibliothèque des utilitaires
- [ ] D) Le fichier exécutable généré

<details>
<summary>Réponse</summary>

**B** - `COBOL.SYSIN` pointe vers le dataset contenant le programme source COBOL à compiler.

</details>

---

### Question 20
Quelle est la différence entre `FORM1011.COBOL.SOURCE` et `FORM1011.COBOL.LOAD` ?

- [ ] A) SOURCE contient le code source, LOAD contient les exécutables
- [ ] B) SOURCE est en lecture seule, LOAD est en écriture
- [ ] C) SOURCE utilise RECFM=U, LOAD utilise RECFM=FB
- [ ] D) Il n'y a pas de différence, ce sont des alias

<details>
<summary>Réponse</summary>

**A** - SOURCE contient le code source COBOL (RECFM=FB, LRECL=80), LOAD contient les programmes exécutables compilés (RECFM=U).

</details>

---

## Questions Bonus : Syntaxe COBOL

### Question Bonus 1
Comment inclure une apostrophe dans une chaîne délimitée par apostrophes ?

- [ ] A) `\'`
- [ ] B) `''` (apostrophe doublée)
- [ ] C) `\''`
- [ ] D) Ce n'est pas possible

<details>
<summary>Réponse</summary>

**B** - On double l'apostrophe : `'J''AI DIT'` affiche `J'AI DIT`.

</details>

---

### Question Bonus 2
Que représente le `V` dans `PIC 9(5)V99` ?

- [ ] A) Variable
- [ ] B) Virgule décimale implicite (Virtual)
- [ ] C) Valeur
- [ ] D) Vérification

<details>
<summary>Réponse</summary>

**B** - `V` représente la virgule décimale implicite (virtuelle). Elle n'occupe pas d'espace en mémoire mais indique la position du point décimal.

</details>

---

### Question Bonus 3
Que fait le caractère `Z` dans une clause PICTURE d'édition ?

- [ ] A) Ajoute des zéros à gauche
- [ ] B) Supprime les zéros non significatifs (remplace par des espaces)
- [ ] C) Zone réservée
- [ ] D) Zéro obligatoire

<details>
<summary>Réponse</summary>

**B** - `Z` supprime les zéros non significatifs et les remplace par des espaces lors de l'affichage.

</details>

---

## Résumé des scores

| Score | Niveau |
|-------|--------|
| 20-23 | Excellent - Maîtrise complète |
| 15-19 | Bien - Quelques révisions mineures |
| 10-14 | Moyen - Relire le chapitre |
| < 10 | Insuffisant - Revoir le cours en détail |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [QCM Chapitre I](qcm-01-structure-programme.md) | [QCM Chapitre III](qcm-03-declaration-variables.md) |

---
*Formation COBOL - M2i Formation*
