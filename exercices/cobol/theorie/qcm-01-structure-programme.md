# QCM - Chapitre I : Structure d'un Programme COBOL

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Historique et contexte (Questions 1-5)

### Question 1
Quelle organisation a développé COBOL en 1959 ?

- [ ] A) IBM
- [ ] B) ANSI
- [ ] C) CODASYL (Conference on Data Systems Language)
- [ ] D) Le Département de la Défense US (DoD)

<details>
<summary>Réponse</summary>

**C et D** - COBOL a été développé par CODASYL sous l'impulsion du DoD (Département de la Défense US).

</details>

---

### Question 2
Que signifie l'acronyme COBOL ?

- [ ] A) Common Business Oriented Language
- [ ] B) Computer Business Organized Language
- [ ] C) Common Binary Oriented Logic
- [ ] D) Compiled Business Object Language

<details>
<summary>Réponse</summary>

**A** - COBOL = **C**ommon **B**usiness **O**riented **L**anguage (Langage Commun Orienté Affaires).

</details>

---

### Question 3
Quelle version de COBOL a introduit la programmation structurée avec END-IF et END-PERFORM ?

- [ ] A) COBOL-74
- [ ] B) COBOL-85
- [ ] C) COBOL-2002
- [ ] D) COBOL-61

<details>
<summary>Réponse</summary>

**B** - COBOL-85 a introduit la programmation structurée avec les terminateurs explicites (END-IF, END-PERFORM, END-EVALUATE).

</details>

---

### Question 4
Quel pourcentage des transactions ATM mondiales utilise COBOL ?

- [ ] A) 50%
- [ ] B) 75%
- [ ] C) 95%
- [ ] D) 100%

<details>
<summary>Réponse</summary>

**C** - Environ 95% des transactions ATM dans le monde sont traitées par des programmes COBOL.

</details>

---

### Question 5
Quel événement de 2020-2021 a mis en lumière l'importance de COBOL dans les administrations américaines ?

- [ ] A) Une cyberattaque majeure
- [ ] B) La pandémie COVID-19 et les systèmes de chômage
- [ ] C) Une panne du système bancaire
- [ ] D) Le bug de l'an 2000

<details>
<summary>Réponse</summary>

**B** - Lors de la pandémie COVID-19, les États de l'Iowa, New Jersey, Kansas et Oklahoma ont fait appel à des développeurs COBOL pour maintenir leurs systèmes de gestion du chômage.

</details>

---

## Section 2 : Les 4 DIVISIONS (Questions 6-12)

### Question 6
Combien de DIVISIONS composent un programme COBOL ?

- [ ] A) 2
- [ ] B) 3
- [ ] C) 4
- [ ] D) 5

<details>
<summary>Réponse</summary>

**C** - Un programme COBOL comporte 4 DIVISIONS : IDENTIFICATION, ENVIRONMENT, DATA et PROCEDURE.

</details>

---

### Question 7
Dans quel ordre doivent apparaître les DIVISIONS ?

- [ ] A) PROCEDURE → DATA → ENVIRONMENT → IDENTIFICATION
- [ ] B) IDENTIFICATION → ENVIRONMENT → DATA → PROCEDURE
- [ ] C) DATA → IDENTIFICATION → ENVIRONMENT → PROCEDURE
- [ ] D) L'ordre n'a pas d'importance

<details>
<summary>Réponse</summary>

**B** - L'ordre est obligatoire : IDENTIFICATION → ENVIRONMENT → DATA → PROCEDURE.

</details>

---

### Question 8
Quelle DIVISION est obligatoire dans un programme COBOL ?

- [ ] A) ENVIRONMENT DIVISION uniquement
- [ ] B) DATA DIVISION uniquement
- [ ] C) IDENTIFICATION DIVISION uniquement
- [ ] D) Toutes les DIVISIONS sont obligatoires

<details>
<summary>Réponse</summary>

**C** - Seule l'IDENTIFICATION DIVISION est obligatoire. Les autres peuvent être omises si non nécessaires.

</details>

---

### Question 9
Quel paragraphe est obligatoire dans l'IDENTIFICATION DIVISION ?

- [ ] A) AUTHOR
- [ ] B) PROGRAM-ID
- [ ] C) DATE-WRITTEN
- [ ] D) INSTALLATION

<details>
<summary>Réponse</summary>

**B** - PROGRAM-ID est le seul paragraphe obligatoire de l'IDENTIFICATION DIVISION.

</details>

---

### Question 10
Quelle est la longueur maximale d'un PROGRAM-ID ?

- [ ] A) 8 caractères
- [ ] B) 20 caractères
- [ ] C) 30 caractères
- [ ] D) Illimitée

<details>
<summary>Réponse</summary>

**C** - Le PROGRAM-ID peut contenir de 1 à 30 caractères.

</details>

---

### Question 11
Quelle section de l'ENVIRONMENT DIVISION permet de définir `DECIMAL-POINT IS COMMA` ?

- [ ] A) INPUT-OUTPUT SECTION
- [ ] B) FILE-CONTROL
- [ ] C) CONFIGURATION SECTION / SPECIAL-NAMES
- [ ] D) I-O-CONTROL

<details>
<summary>Réponse</summary>

**C** - `DECIMAL-POINT IS COMMA` se définit dans SPECIAL-NAMES de la CONFIGURATION SECTION.

</details>

---

### Question 12
Quelles sont les 4 sections possibles de la DATA DIVISION ?

- [ ] A) FILE, WORKING-STORAGE, LOCAL-STORAGE, LINKAGE
- [ ] B) FILE, VARIABLE, CONSTANT, LINKAGE
- [ ] C) INPUT, OUTPUT, WORKING, LINKAGE
- [ ] D) FILE, WORKING-STORAGE, SCREEN, REPORT

<details>
<summary>Réponse</summary>

**A** - Les 4 sections sont : FILE SECTION, WORKING-STORAGE SECTION, LOCAL-STORAGE SECTION et LINKAGE SECTION.

</details>

---

## Section 3 : Règles de codage (Questions 13-19)

### Question 13
Quelles sont les colonnes de la Zone A ?

- [ ] A) 1-6
- [ ] B) 7
- [ ] C) 8-11
- [ ] D) 12-72

<details>
<summary>Réponse</summary>

**C** - La Zone A correspond aux colonnes 8 à 11.

</details>

---

### Question 14
Quel caractère en colonne 7 indique une ligne de commentaire ?

- [ ] A) #
- [ ] B) //
- [ ] C) *
- [ ] D) --

<details>
<summary>Réponse</summary>

**C** - L'astérisque (*) en colonne 7 indique une ligne de commentaire.

</details>

---

### Question 15
Quel caractère en colonne 7 permet de continuer une chaîne sur la ligne suivante ?

- [ ] A) +
- [ ] B) -
- [ ] C) &
- [ ] D) _

<details>
<summary>Réponse</summary>

**B** - Le tiret (-) en colonne 7 indique une continuation de ligne.

</details>

---

### Question 16
Quels éléments doivent obligatoirement commencer en Zone A (colonne 8) ?

- [ ] A) Les noms de DIVISION et SECTION
- [ ] B) Les noms de paragraphes
- [ ] C) Les niveaux 01 et 77
- [ ] D) Toutes les réponses ci-dessus

<details>
<summary>Réponse</summary>

**D** - Les DIVISIONS, SECTIONS, paragraphes et niveaux 01/77 doivent commencer en Zone A.

</details>

---

### Question 17
À quoi servent les colonnes 73-80 ?

- [ ] A) Aux instructions COBOL
- [ ] B) Aux commentaires
- [ ] C) Elles sont ignorées (réservées au système)
- [ ] D) À la numérotation des lignes

<details>
<summary>Réponse</summary>

**C** - Les colonnes 73-80 sont réservées au système et ignorées par le compilateur.

</details>

---

### Question 18
Quel caractère en colonne 7 active une ligne de debug ?

- [ ] A) *
- [ ] B) D ou d
- [ ] C) #
- [ ] D) ?

<details>
<summary>Réponse</summary>

**B** - Le caractère D (ou d) en colonne 7 marque une ligne de debug, activée avec l'option de compilation appropriée.

</details>

---

### Question 19
Quel caractère en colonne 7 provoque un saut de page à l'impression ?

- [ ] A) *
- [ ] B) -
- [ ] C) /
- [ ] D) P

<details>
<summary>Réponse</summary>

**C** - Le slash (/) en colonne 7 provoque un saut de page lors de l'impression du listing.

</details>

---

## Section 4 : Nommage et littéraux (Questions 20-25)

### Question 20
Quelle est la longueur maximale d'un nom de variable COBOL ?

- [ ] A) 8 caractères
- [ ] B) 16 caractères
- [ ] C) 30 caractères
- [ ] D) 255 caractères

<details>
<summary>Réponse</summary>

**C** - Un nom de variable COBOL peut contenir de 1 à 30 caractères.

</details>

---

### Question 21
Lequel de ces noms de variable est INVALIDE ?

- [ ] A) CLIENT-NOM
- [ ] B) TOTAL_2024
- [ ] C) WS-MONTANT
- [ ] D) LIGNE-01

<details>
<summary>Réponse</summary>

**B** - TOTAL_2024 est invalide car COBOL n'accepte pas le caractère underscore (_). Seuls les tirets (-) sont autorisés.

</details>

---

### Question 22
Quelle constante figurative représente la valeur zéro ?

- [ ] A) NULL
- [ ] B) ZEROS ou ZEROES
- [ ] C) EMPTY
- [ ] D) BLANK

<details>
<summary>Réponse</summary>

**B** - ZERO, ZEROS ou ZEROES représentent la valeur zéro en COBOL.

</details>

---

### Question 23
Quelle est la longueur maximale d'un littéral alphanumérique ?

- [ ] A) 80 caractères
- [ ] B) 120 caractères
- [ ] C) 160 caractères
- [ ] D) 255 caractères

<details>
<summary>Réponse</summary>

**C** - Un littéral alphanumérique peut contenir jusqu'à 160 caractères.

</details>

---

### Question 24
Comment inclure une apostrophe dans une chaîne délimitée par apostrophes ?

- [ ] A) \'
- [ ] B) ''
- [ ] C) /'
- [ ] D) Ce n'est pas possible

<details>
<summary>Réponse</summary>

**B** - On double l'apostrophe : `'L''apostrophe est doublée'`

</details>

---

### Question 25
Quelle instruction termine un programme COBOL principal (appelant) ?

- [ ] A) END PROGRAM
- [ ] B) EXIT PROGRAM
- [ ] C) STOP RUN
- [ ] D) TERMINATE

<details>
<summary>Réponse</summary>

**C** - `STOP RUN` termine un programme principal. `EXIT PROGRAM` est utilisé pour les sous-programmes (programmes appelés).

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
| - | [QCM Chapitre II](qcm-02-ispf-commandes.md) |

---
*Formation COBOL - M2i Formation*
