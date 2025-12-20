# QCM - Chapitre XII : Fichier d'Impression COBOL

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Concepts fondamentaux d'édition (Questions 1-5)

### Question 1
Qu'est-ce que l'édition en COBOL ?

- [ ] A) La compilation du code source
- [ ] B) La transformation de données brutes en données lisibles pour l'impression
- [ ] C) La modification du code source
- [ ] D) La suppression des erreurs de syntaxe

<details>
<summary>Réponse</summary>

**B** - L'édition consiste à transformer des données brutes en données lisibles pour l'impression ou l'affichage (insertion de séparateurs, suppression de zéros, formatage).

</details>

---

### Question 2
Les éléments édités peuvent-ils être utilisés comme opérandes dans un calcul ?

- [ ] A) Oui, sans restriction
- [ ] B) Non, ils ne peuvent être que destinations (après GIVING)
- [ ] C) Oui, mais uniquement avec COMPUTE
- [ ] D) Oui, mais uniquement en lecture

<details>
<summary>Réponse</summary>

**B** - Les éléments édités ne peuvent pas être utilisés comme **opérandes** dans un calcul. Ils peuvent uniquement être utilisés comme **destination** (à droite du mot GIVING).

</details>

---

### Question 3
Quelle clause FD est typique pour un fichier d'impression ?

- [ ] A) LABEL RECORD IS STANDARD
- [ ] B) LABEL RECORD IS OMITTED
- [ ] C) LABEL RECORD IS EXTERNAL
- [ ] D) LABEL RECORD IS INTERNAL

<details>
<summary>Réponse</summary>

**B** - Pour les fichiers d'impression, on utilise `LABEL RECORD IS OMITTED` car les fichiers d'impression n'ont pas d'étiquettes.

</details>

---

### Question 4
Quelle est la largeur standard d'une ligne d'impression ?

- [ ] A) 80 caractères
- [ ] B) 100 caractères
- [ ] C) 132 caractères
- [ ] D) 256 caractères

<details>
<summary>Réponse</summary>

**C** - La largeur standard pour une imprimante ligne est de **132 caractères**.

</details>

---

### Question 5
Quelles sont les deux grandes méthodes d'édition en COBOL ?

- [ ] A) Édition d'affichage et d'impression
- [ ] B) Édition d'insertion et édition de suppression/remplacement
- [ ] C) Édition numérique et alphabétique
- [ ] D) Édition fixe et variable

<details>
<summary>Réponse</summary>

**B** - Les deux méthodes sont l'**édition d'insertion** (simple, spéciale, fixe, flottante) et l'**édition de suppression et remplacement** (Z, *).

</details>

---

## Section 2 : Caractères et symboles d'édition (Questions 6-10)

### Question 6
Quel caractère d'édition insère un espace à sa position ?

- [ ] A) `S`
- [ ] B) `B`
- [ ] C) `X`
- [ ] D) `0`

<details>
<summary>Réponse</summary>

**B** - Le caractère `B` (Blank) insère un **espace** à la position où il apparaît dans la clause PICTURE.

</details>

---

### Question 7
Quel caractère utilise-t-on pour formater les dates avec des séparateurs ?

- [ ] A) `-`
- [ ] B) `B`
- [ ] C) `/`
- [ ] D) `.`

<details>
<summary>Réponse</summary>

**C** - Le caractère `/` (barre oblique) est principalement utilisé pour représenter des valeurs de date (ex: `99/99/9999`).

</details>

---

### Question 8
Que signifient les symboles CR et DB ?

- [ ] A) Caractère Retour et Débit Base
- [ ] B) Crédit et Débit (affichés si valeur négative)
- [ ] C) Code Retour et Data Block
- [ ] D) Caractère Réservé et Donnée Binaire

<details>
<summary>Réponse</summary>

**B** - `CR` (Crédit) et `DB` (Débit) apparaissent à droite de la valeur uniquement si elle est **négative**. Si positive, deux espaces sont affichés.

</details>

---

### Question 9
Quelle clause permet de remplir un élément d'espaces si sa valeur est zéro ?

- [ ] A) ZERO FILL
- [ ] B) BLANK WHEN ZERO
- [ ] C) SPACE IF ZERO
- [ ] D) EMPTY ON ZERO

<details>
<summary>Réponse</summary>

**B** - La clause `BLANK WHEN ZERO` définit l'élément entier sur des blancs si l'élément contient une valeur nulle.

</details>

---

### Question 10
Quel caractère d'édition insère un zéro à sa position ?

- [ ] A) `Z`
- [ ] B) `9`
- [ ] C) `0`
- [ ] D) `*`

<details>
<summary>Réponse</summary>

**C** - Le caractère `0` insère un **zéro** à la position où il apparaît dans la clause PICTURE.

</details>

---

## Section 3 : Insertion simple et spéciale (Questions 11-15)

### Question 11
Quel sera le résultat de `PIC 99/99/9999` avec la valeur 15122023 ?

- [ ] A) 15-12-2023
- [ ] B) 15/12/2023
- [ ] C) 1512/2023
- [ ] D) 15.12.2023

<details>
<summary>Réponse</summary>

**B** - La clause `99/99/9999` insère des barres obliques aux positions indiquées, donnant **15/12/2023**.

</details>

---

### Question 12
Quel sera le résultat de `PIC 99B999` avec la valeur 12345 ?

- [ ] A) 12345
- [ ] B) 12 345
- [ ] C) 123 45
- [ ] D) 1 2345

<details>
<summary>Réponse</summary>

**B** - La clause `99B999` insère un espace après les deux premiers chiffres, donnant **12 345**.

</details>

---

### Question 13
Que représente le point (.) dans une clause PICTURE d'édition ?

- [ ] A) Un séparateur de milliers
- [ ] B) Un caractère d'insertion simple
- [ ] C) Le séparateur décimal réel
- [ ] D) Un caractère de remplacement

<details>
<summary>Réponse</summary>

**C** - Le point (`.`) est le symbole d'insertion spéciale qui représente la **virgule décimale réelle** pour l'alignement.

</details>

---

### Question 14
Peut-on avoir à la fois un point décimal (.) et un symbole V dans la même PICTURE ?

- [ ] A) Oui, c'est obligatoire
- [ ] B) Non, c'est mutuellement exclusif
- [ ] C) Oui, mais seulement pour les PIC numériques
- [ ] D) Oui, si on utilise DECIMAL-POINT IS COMMA

<details>
<summary>Réponse</summary>

**B** - Soit le point décimal réel (`.`), soit le symbole `V` comme point décimal supposé, mais **pas les deux** dans une chaîne PICTURE.

</details>

---

### Question 15
À quoi sert la clause DECIMAL-POINT IS COMMA ?

- [ ] A) Afficher les nombres avec des virgules
- [ ] B) Inverser les rôles du point et de la virgule
- [ ] C) Supprimer les décimales
- [ ] D) Arrondir les valeurs décimales

<details>
<summary>Réponse</summary>

**B** - `DECIMAL-POINT IS COMMA` inverse les rôles : la virgule devient le séparateur décimal et le point devient le séparateur de milliers.

</details>

---

## Section 4 : Insertion fixe et flottante (Questions 16-21)

### Question 16
Quelle est la différence entre le signe fixe (-) et le signe flottant (----) ?

- [ ] A) Aucune différence
- [ ] B) Le signe fixe est toujours à la même position, le flottant se déplace près du premier chiffre
- [ ] C) Le signe fixe est pour les positifs, le flottant pour les négatifs
- [ ] D) Le signe fixe affiche toujours le signe, le flottant jamais

<details>
<summary>Réponse</summary>

**B** - Le signe **fixe** reste à une position déterminée. Le signe **flottant** se déplace pour se positionner juste avant le premier chiffre significatif.

</details>

---

### Question 17
Avec PIC -99999,99, quel sera le résultat pour une valeur positive de 12345.67 ?

- [ ] A) +12345,67
- [ ] B) -12345,67
- [ ] C)  12345,67 (espace au lieu du -)
- [ ] D) 12345,67-

<details>
<summary>Réponse</summary>

**C** - Avec le signe moins (`-`) fixe, si la valeur est **positive**, un **espace** remplace le signe moins.

</details>

---

### Question 18
Avec PIC +99999,99, quel sera le résultat pour une valeur négative de -12345.67 ?

- [ ] A) +12345,67
- [ ] B) -12345,67
- [ ] C)  12345,67
- [ ] D) Erreur

<details>
<summary>Réponse</summary>

**B** - Avec le signe plus (`+`) fixe, si la valeur est **négative**, le signe **moins** (`-`) est affiché.

</details>

---

### Question 19
Combien de symboles minimum faut-il pour une insertion flottante ?

- [ ] A) Un seul
- [ ] B) Au moins deux
- [ ] C) Au moins trois
- [ ] D) Exactement quatre

<details>
<summary>Réponse</summary>

**B** - L'édition par insertion flottante nécessite une chaîne d'**au moins deux** symboles d'insertion flottante.

</details>

---

### Question 20
Avec PIC $$$$$$$9,99, quel sera le résultat pour la valeur 12.34 ?

- [ ] A) $00012,34
- [ ] B) $$$$$12,34
- [ ] C)      $12,34
- [ ] D) 12,34$

<details>
<summary>Réponse</summary>

**C** - Avec l'insertion flottante, le symbole `$` se positionne juste avant le premier chiffre significatif, et les positions à gauche sont remplies d'espaces.

</details>

---

### Question 21
Quel sera le résultat de PIC $9999.99CR pour la valeur -123.45 ?

- [ ] A) $0123.45
- [ ] B) $0123.45CR
- [ ] C) -$123.45
- [ ] D) $123.45-

<details>
<summary>Réponse</summary>

**B** - Le symbole `$` est fixe en début, et `CR` (Crédit) apparaît à droite car la valeur est **négative**.

</details>

---

## Section 5 : Suppression de zéros et instruction WRITE (Questions 22-25)

### Question 22
Quelle est la différence entre Z et * pour la suppression des zéros ?

- [ ] A) Z remplace par des espaces, * remplace par des astérisques
- [ ] B) Z supprime les zéros, * les conserve
- [ ] C) Z est pour les entiers, * pour les décimaux
- [ ] D) Aucune différence

<details>
<summary>Réponse</summary>

**A** - `Z` remplace les zéros non significatifs par des **espaces**, `*` les remplace par des **astérisques** (protection des chèques).

</details>

---

### Question 23
Quel sera le résultat de PIC ZZZZ.ZZ pour la valeur 0000.00 ?

- [ ] A) 0000.00
- [ ] B) ****.**
- [ ] C) (tout en espaces)
- [ ] D) .00

<details>
<summary>Réponse</summary>

**C** - Avec `PIC ZZZZ.ZZ` (tout en Z, y compris après le point), si la valeur est zéro, l'élément entier contient des **espaces**.

</details>

---

### Question 24
Quelle clause WRITE permet de faire un saut de page avant l'écriture ?

- [ ] A) BEFORE ADVANCING PAGE
- [ ] B) AFTER ADVANCING PAGE
- [ ] C) SKIP PAGE
- [ ] D) PAGE BREAK

<details>
<summary>Réponse</summary>

**B** - `AFTER ADVANCING PAGE` effectue un saut de page **avant** l'écriture de la ligne.

</details>

---

### Question 25
Pour quel usage utilise-t-on typiquement le caractère * dans l'édition ?

- [ ] A) Pour les nombres négatifs
- [ ] B) Pour la protection des montants sur les chèques
- [ ] C) Pour les calculs
- [ ] D) Pour les dates

<details>
<summary>Réponse</summary>

**B** - Le caractère `*` est utilisé pour la **protection des montants** sur les chèques, empêchant l'ajout de chiffres à gauche.

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
| [QCM Chapitre XI](qcm-11-tri-interne.md) | [Module CICS](../../cics/) |

---
*Formation COBOL - M2i Formation*
