# Chapitre XII - Fichier d'Impression COBOL

Ce chapitre traite des fichiers d'impression et des caractères d'édition pour la mise en forme des résultats.

---

## XII-1 Définition d'édition COBOL

### Concept d'édition

L'**édition** en COBOL consiste à transformer des données brutes en données lisibles pour l'impression ou l'affichage :

- Insertion de séparateurs (virgules, points, espaces)
- Suppression des zéros non significatifs
- Ajout de symboles monétaires
- Formatage des signes (+/-)
- Mise en forme des dates et montants

```
Donnée brute     →    Donnée éditée
00012345         →    123,45
-00005000        →    -50,00 €
20231215         →    15/12/2023
```

### Déclaration d'un fichier d'impression

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-IMPRESSION
               ASSIGN TO 'ETAT.LST'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-IMPRESSION
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORD IS OMITTED.
       01  LIGNE-IMPRESSION     PIC X(132).
```

**Caractéristiques :**
- Largeur standard : **132 caractères** (imprimante ligne)
- `LABEL RECORD IS OMITTED` : pas d'étiquettes (fichier d'impression)
- Organisation SEQUENTIAL

### Instruction WRITE pour impression

```cobol
      * Écriture simple
       WRITE LIGNE-IMPRESSION FROM WS-LIGNE-DETAIL

      * Avec saut de page
       WRITE LIGNE-IMPRESSION FROM WS-TITRE
           AFTER ADVANCING PAGE

      * Avec saut de lignes
       WRITE LIGNE-IMPRESSION FROM WS-DETAIL
           AFTER ADVANCING 1 LINE

       WRITE LIGNE-IMPRESSION FROM WS-SOUS-TOTAL
           AFTER ADVANCING 2 LINES
```

| Clause | Effet |
|--------|-------|
| `AFTER ADVANCING PAGE` | Saut de page avant écriture |
| `AFTER ADVANCING n LINES` | n sauts de ligne avant écriture |
| `BEFORE ADVANCING n LINES` | n sauts de ligne après écriture |

---

## XII-2 Caractères d'édition

### Tableau des caractères d'édition

| Caractère | Nom | Usage |
|-----------|-----|-------|
| `9` | Chiffre | Position de chiffre |
| `Z` | Suppression zéro | Remplace zéros par espaces |
| `*` | Astérisque | Remplace zéros par astérisques |
| `.` | Point | Séparateur décimal (ou virgule) |
| `,` | Virgule | Séparateur de milliers |
| `+` | Plus | Signe + ou - selon valeur |
| `-` | Moins | Signe - si négatif, espace sinon |
| `$` | Dollar | Symbole monétaire |
| `€` | Euro | Symbole monétaire |
| `CR` | Crédit | "CR" si négatif |
| `DB` | Débit | "DB" si négatif |
| `B` | Blanc | Insertion d'espace |
| `/` | Barre | Insertion de / |
| `0` | Zéro | Insertion de 0 |

---

## XII-3 Édition par insertion simple

### Insertion de caractères fixes

Les caractères `B`, `0`, `/` sont insérés à leur position exacte.

#### Insertion de blancs (B)

```cobol
       01  WS-NUMERO           PIC 9(9).
       01  WS-NUMERO-EDIT      PIC 999B999B999.

       MOVE 123456789 TO WS-NUMERO
       MOVE WS-NUMERO TO WS-NUMERO-EDIT
      * Résultat : "123 456 789"
```

#### Insertion de barres (/)

```cobol
       01  WS-DATE             PIC 9(8).
       01  WS-DATE-EDIT        PIC 99/99/9999.

       MOVE 15122023 TO WS-DATE
       MOVE WS-DATE TO WS-DATE-EDIT
      * Résultat : "15/12/2023"
```

#### Insertion de zéros (0)

```cobol
       01  WS-CODE             PIC 999.
       01  WS-CODE-EDIT        PIC 9900999.

       MOVE 123 TO WS-CODE
       MOVE WS-CODE TO WS-CODE-EDIT
      * Résultat : "1200123"
```

### Exemples combinés

```cobol
       01  WS-TEL              PIC 9(10).
       01  WS-TEL-EDIT         PIC 99B99B99B99B99.

       MOVE 0612345678 TO WS-TEL
       MOVE WS-TEL TO WS-TEL-EDIT
      * Résultat : "06 12 34 56 78"

       01  WS-SIRET            PIC 9(14).
       01  WS-SIRET-EDIT       PIC 999B999B999B99999.

       MOVE 12345678901234 TO WS-SIRET
       MOVE WS-SIRET TO WS-SIRET-EDIT
      * Résultat : "123 456 789 01234"
```

---

## XII-4 Édition par insertion spéciale

### Point décimal (.) et Virgule (,)

Le **point** représente le séparateur décimal. La **virgule** sépare les milliers.

**Note :** Avec `DECIMAL-POINT IS COMMA`, les rôles sont inversés.

```cobol
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       01  WS-MONTANT          PIC 9(7)V99.
       01  WS-MONTANT-EDIT     PIC 9.999.999,99.

       MOVE 1234567.89 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "1.234.567,89"
```

#### Sans DECIMAL-POINT IS COMMA

```cobol
       01  WS-MONTANT          PIC 9(7)V99.
       01  WS-MONTANT-EDIT     PIC 9,999,999.99.

       MOVE 1234567.89 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "1,234,567.89"
```

---

## XII-5 Édition par insertion fixe

### Signe fixe (+, -)

Le signe est placé à une position fixe (début ou fin).

#### Signe moins (-) fixe

```cobol
       01  WS-VALEUR           PIC S9(5)V99.
       01  WS-VALEUR-EDIT      PIC -99999,99.

       MOVE -12345.67 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT
      * Résultat : "-12345,67"

       MOVE 12345.67 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT
      * Résultat : " 12345,67"  (espace à la place du -)
```

#### Signe plus (+) fixe

```cobol
       01  WS-VALEUR           PIC S9(5)V99.
       01  WS-VALEUR-EDIT      PIC +99999,99.

       MOVE -12345.67 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT
      * Résultat : "-12345,67"

       MOVE 12345.67 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT
      * Résultat : "+12345,67"
```

#### Signe en fin

```cobol
       01  WS-VALEUR-EDIT      PIC 99999,99-.

       MOVE -12345.67 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT
      * Résultat : "12345,67-"
```

### Symbole monétaire fixe

```cobol
       01  WS-PRIX             PIC 9(5)V99.
       01  WS-PRIX-EDIT        PIC $99999,99.

       MOVE 1250.00 TO WS-PRIX
       MOVE WS-PRIX TO WS-PRIX-EDIT
      * Résultat : "$01250,00"
```

### CR et DB (Crédit/Débit)

```cobol
       01  WS-SOLDE            PIC S9(7)V99.
       01  WS-SOLDE-CR         PIC 9999999,99CR.
       01  WS-SOLDE-DB         PIC 9999999,99DB.

       MOVE -5000.00 TO WS-SOLDE
       MOVE WS-SOLDE TO WS-SOLDE-CR
      * Résultat : "0005000,00CR"

       MOVE WS-SOLDE TO WS-SOLDE-DB
      * Résultat : "0005000,00DB"

       MOVE 5000.00 TO WS-SOLDE
       MOVE WS-SOLDE TO WS-SOLDE-CR
      * Résultat : "0005000,00  " (espaces à la place de CR)
```

---

## XII-6 Édition par insertion flottante

### Concept de flottant

Un symbole **flottant** se déplace pour se positionner juste avant le premier chiffre significatif.

### Symbole monétaire flottant ($, €)

```cobol
       01  WS-MONTANT          PIC 9(7)V99.
       01  WS-MONTANT-EDIT     PIC $$$$$$$9,99.

       MOVE 1234.56 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "   $1234,56"

       MOVE 12.34 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "     $12,34"
```

#### Avec Euro (€)

```cobol
       CURRENCY SIGN IS "E" WITH PICTURE SYMBOL "€".

       01  WS-PRIX-EDIT        PIC €€€€€€9,99.

       MOVE 999.99 TO WS-PRIX
       MOVE WS-PRIX TO WS-PRIX-EDIT
      * Résultat : "   €999,99"
```

### Signe flottant (+, -)

```cobol
       01  WS-VALEUR           PIC S9(6)V99.
       01  WS-VALEUR-EDIT-PLUS PIC ++++++9,99.
       01  WS-VALEUR-EDIT-MOINS PIC ------9,99.

       MOVE -123.45 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT-PLUS
      * Résultat : "   -123,45"

       MOVE 123.45 TO WS-VALEUR
       MOVE WS-VALEUR TO WS-VALEUR-EDIT-PLUS
      * Résultat : "   +123,45"

       MOVE WS-VALEUR TO WS-VALEUR-EDIT-MOINS
      * Résultat : "    123,45" (espace car positif)
```

### Représentation graphique

```
Valeur source : 00012345 (PIC S9(8)V99 = 123,45)

PIC $$$$$$$9,99  →  "    $123,45"
                     ↑↑↑↑
                     Espaces (zéros supprimés)
                         ↑
                         $ flottant

PIC ++++++9,99   →  "   +123,45"  (valeur positive)
PIC ++++++9,99   →  "   -123,45"  (valeur négative)
```

---

## XII-7 Suppression des zéros (Z)

### Caractère Z

Le `Z` remplace les zéros non significatifs par des **espaces**.

```cobol
       01  WS-NOMBRE           PIC 9(8).
       01  WS-NOMBRE-EDIT      PIC ZZZZZZ99.

       MOVE 00001234 TO WS-NOMBRE
       MOVE WS-NOMBRE TO WS-NOMBRE-EDIT
      * Résultat : "    1234"

       MOVE 00000012 TO WS-NOMBRE
       MOVE WS-NOMBRE TO WS-NOMBRE-EDIT
      * Résultat : "      12"
```

### Z avec décimales

```cobol
       01  WS-MONTANT          PIC 9(6)V99.
       01  WS-MONTANT-EDIT     PIC ZZZZZZ9,99.

       MOVE 001234.56 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "  1234,56"

       MOVE 000000.05 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "     0,05"
```

### Tout en Z (y compris décimales)

```cobol
       01  WS-MONTANT-EDIT     PIC ZZZZZZ,ZZ.

       MOVE 000000.00 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-EDIT
      * Résultat : "         " (tout en espaces si zéro)
```

---

## XII-8 Remplacement par astérisques (*)

### Protection des montants

Le `*` remplace les zéros non significatifs par des **astérisques** (protection des chèques).

```cobol
       01  WS-MONTANT          PIC 9(8)V99.
       01  WS-MONTANT-CHECK    PIC ********9,99.

       MOVE 00001234.56 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-CHECK
      * Résultat : "****1234,56"

       MOVE 00000012.00 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-MONTANT-CHECK
      * Résultat : "******12,00"
```

### Combinaison avec symbole monétaire

```cobol
       01  WS-CHEQUE-EDIT      PIC $***,***,**9.99.

       MOVE 12345.67 TO WS-MONTANT
       MOVE WS-MONTANT TO WS-CHEQUE-EDIT
      * Résultat : "$***,*12,345.67"
```

---

## XII-9 Tableau récapitulatif

### Éditions les plus courantes

| PIC source | PIC édition | Valeur | Résultat |
|------------|-------------|--------|----------|
| `9(6)` | `ZZZ.ZZ9` | 001234 | "  1.234" |
| `9(6)V99` | `ZZZ.ZZ9,99` | 001234.56 | "  1.234,56" |
| `S9(6)V99` | `-ZZZ.ZZ9,99` | -1234.56 | "-  1.234,56" |
| `S9(6)V99` | `ZZZ.ZZ9,99-` | -1234.56 | "  1.234,56-" |
| `S9(6)V99` | `+ZZZ.ZZ9,99` | 1234.56 | "+  1.234,56" |
| `9(8)V99` | `$$$.$$$,$$9.99` | 12345.67 | "   $12,345.67" |
| `9(8)V99` | `***,***,**9.99` | 12345.67 | "***,*12,345.67" |
| `9(8)` | `99/99/9999` | 15122023 | "15/12/2023" |
| `9(10)` | `99B99B99B99B99` | 0612345678 | "06 12 34 56 78" |

### Différences Z vs * vs $

| Caractère | Remplace zéros par | Usage |
|-----------|-------------------|-------|
| `Z` | Espaces | Affichage standard |
| `*` | Astérisques | Protection (chèques) |
| `$` ou `€` | Se positionne avant 1er chiffre | Symbole monétaire flottant |

---

## XII-10 Exemple complet - État d'impression

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPRESSION-FACTURE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-FACTURE ASSIGN TO 'FACTURE.LST'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  F-FACTURE
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORD IS OMITTED.
       01  LIGNE-FACTURE       PIC X(132).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Lignes d'impression
      *----------------------------------------------------------------*
       01  WS-LIGNE-TITRE.
           05  FILLER          PIC X(50) VALUE SPACES.
           05  FILLER          PIC X(30) VALUE 'FACTURE CLIENT'.
           05  FILLER          PIC X(52) VALUE SPACES.

       01  WS-LIGNE-SEPARATEUR.
           05  FILLER          PIC X(132) VALUE ALL '='.

       01  WS-LIGNE-EN-TETE.
           05  FILLER          PIC X(10) VALUE 'CODE'.
           05  FILLER          PIC X(25) VALUE 'DESIGNATION'.
           05  FILLER          PIC X(15) VALUE 'QTE'.
           05  FILLER          PIC X(20) VALUE 'PRIX UNIT.'.
           05  FILLER          PIC X(25) VALUE 'MONTANT HT'.
           05  FILLER          PIC X(37) VALUE SPACES.

       01  WS-LIGNE-DETAIL.
           05  WS-DET-CODE     PIC X(8).
           05  FILLER          PIC X(2) VALUE SPACES.
           05  WS-DET-DESIGN   PIC X(20).
           05  FILLER          PIC X(5) VALUE SPACES.
           05  WS-DET-QTE      PIC ZZ.ZZ9.
           05  FILLER          PIC X(5) VALUE SPACES.
           05  WS-DET-PU       PIC ZZZ.ZZ9,99.
           05  FILLER          PIC X(3) VALUE ' E '.
           05  WS-DET-MONTANT  PIC ZZZ.ZZZ.ZZ9,99.
           05  FILLER          PIC X(3) VALUE ' E'.
           05  FILLER          PIC X(55) VALUE SPACES.

       01  WS-LIGNE-TOTAL-HT.
           05  FILLER          PIC X(60) VALUE SPACES.
           05  FILLER          PIC X(20) VALUE 'TOTAL HT :'.
           05  WS-TOT-HT       PIC ZZZ.ZZZ.ZZ9,99.
           05  FILLER          PIC X(5) VALUE ' EUR'.
           05  FILLER          PIC X(32) VALUE SPACES.

       01  WS-LIGNE-TVA.
           05  FILLER          PIC X(60) VALUE SPACES.
           05  FILLER          PIC X(20) VALUE 'TVA 20% :'.
           05  WS-TOT-TVA      PIC ZZZ.ZZZ.ZZ9,99.
           05  FILLER          PIC X(5) VALUE ' EUR'.
           05  FILLER          PIC X(32) VALUE SPACES.

       01  WS-LIGNE-TOTAL-TTC.
           05  FILLER          PIC X(60) VALUE SPACES.
           05  FILLER          PIC X(20) VALUE 'TOTAL TTC :'.
           05  WS-TOT-TTC      PIC ZZZ.ZZZ.ZZ9,99.
           05  FILLER          PIC X(5) VALUE ' EUR'.
           05  FILLER          PIC X(32) VALUE SPACES.

      *----------------------------------------------------------------*
      * Zones de calcul
      *----------------------------------------------------------------*
       01  WS-TOTAL-HT         PIC 9(10)V99 VALUE 0.
       01  WS-TVA              PIC 9(10)V99 VALUE 0.
       01  WS-TOTAL-TTC        PIC 9(10)V99 VALUE 0.
       01  WS-MONTANT-LIGNE    PIC 9(10)V99 VALUE 0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           OPEN OUTPUT F-FACTURE

      *    En-tête
           WRITE LIGNE-FACTURE FROM WS-LIGNE-SEPARATEUR
               AFTER ADVANCING PAGE
           WRITE LIGNE-FACTURE FROM WS-LIGNE-TITRE
               AFTER ADVANCING 1 LINE
           WRITE LIGNE-FACTURE FROM WS-LIGNE-SEPARATEUR
               AFTER ADVANCING 1 LINE
           MOVE SPACES TO LIGNE-FACTURE
           WRITE LIGNE-FACTURE
               AFTER ADVANCING 1 LINE
           WRITE LIGNE-FACTURE FROM WS-LIGNE-EN-TETE
               AFTER ADVANCING 1 LINE
           MOVE ALL '-' TO LIGNE-FACTURE
           WRITE LIGNE-FACTURE
               AFTER ADVANCING 1 LINE

      *    Lignes de détail
           PERFORM 1000-ECRIRE-DETAIL-1
           PERFORM 1000-ECRIRE-DETAIL-2
           PERFORM 1000-ECRIRE-DETAIL-3

      *    Totaux
           MOVE SPACES TO LIGNE-FACTURE
           WRITE LIGNE-FACTURE
               AFTER ADVANCING 1 LINE
           MOVE ALL '-' TO LIGNE-FACTURE
           WRITE LIGNE-FACTURE
               AFTER ADVANCING 1 LINE

           MOVE WS-TOTAL-HT TO WS-TOT-HT
           WRITE LIGNE-FACTURE FROM WS-LIGNE-TOTAL-HT
               AFTER ADVANCING 1 LINE

           COMPUTE WS-TVA = WS-TOTAL-HT * 0.20
           MOVE WS-TVA TO WS-TOT-TVA
           WRITE LIGNE-FACTURE FROM WS-LIGNE-TVA
               AFTER ADVANCING 1 LINE

           COMPUTE WS-TOTAL-TTC = WS-TOTAL-HT + WS-TVA
           MOVE WS-TOTAL-TTC TO WS-TOT-TTC
           WRITE LIGNE-FACTURE FROM WS-LIGNE-TOTAL-TTC
               AFTER ADVANCING 1 LINE

           WRITE LIGNE-FACTURE FROM WS-LIGNE-SEPARATEUR
               AFTER ADVANCING 2 LINES

           CLOSE F-FACTURE
           DISPLAY 'Facture generee : FACTURE.LST'
           STOP RUN.

       1000-ECRIRE-DETAIL-1.
           MOVE 'ART001' TO WS-DET-CODE
           MOVE 'Clavier USB' TO WS-DET-DESIGN
           MOVE 10 TO WS-DET-QTE
           MOVE 25.99 TO WS-DET-PU
           COMPUTE WS-MONTANT-LIGNE = 10 * 25.99
           MOVE WS-MONTANT-LIGNE TO WS-DET-MONTANT
           ADD WS-MONTANT-LIGNE TO WS-TOTAL-HT
           WRITE LIGNE-FACTURE FROM WS-LIGNE-DETAIL
               AFTER ADVANCING 1 LINE.

       1000-ECRIRE-DETAIL-2.
           MOVE 'ART002' TO WS-DET-CODE
           MOVE 'Souris optique' TO WS-DET-DESIGN
           MOVE 10 TO WS-DET-QTE
           MOVE 15.50 TO WS-DET-PU
           COMPUTE WS-MONTANT-LIGNE = 10 * 15.50
           MOVE WS-MONTANT-LIGNE TO WS-DET-MONTANT
           ADD WS-MONTANT-LIGNE TO WS-TOTAL-HT
           WRITE LIGNE-FACTURE FROM WS-LIGNE-DETAIL
               AFTER ADVANCING 1 LINE.

       1000-ECRIRE-DETAIL-3.
           MOVE 'ART003' TO WS-DET-CODE
           MOVE 'Ecran 24 pouces' TO WS-DET-DESIGN
           MOVE 5 TO WS-DET-QTE
           MOVE 199.99 TO WS-DET-PU
           COMPUTE WS-MONTANT-LIGNE = 5 * 199.99
           MOVE WS-MONTANT-LIGNE TO WS-DET-MONTANT
           ADD WS-MONTANT-LIGNE TO WS-TOTAL-HT
           WRITE LIGNE-FACTURE FROM WS-LIGNE-DETAIL
               AFTER ADVANCING 1 LINE.
```

---

## Résumé

| Type d'édition | Caractères | Exemple |
|----------------|------------|---------|
| Insertion simple | `B`, `/`, `0` | `99/99/9999` → date |
| Insertion décimale | `.`, `,` | `999.999,99` → montant |
| Signe fixe | `+`, `-`, `CR`, `DB` | `-999,99` ou `999,99CR` |
| Signe flottant | `++++`, `----` | `+++9,99` → signe devant |
| Symbole fixe | `$`, `€` | `$999,99` |
| Symbole flottant | `$$$$`, `€€€€` | `$$$9,99` → $ devant chiffres |
| Suppression zéros | `Z` | `ZZZ9` → espaces |
| Astérisques | `*` | `***9` → protection |
