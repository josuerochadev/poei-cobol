# Exercices COBOL

## Organisation

Les exercices sont organisés par chapitre du cours.

**Convention de nommage :** `CXX-SUJET.cbl` où XX = numéro de chapitre

## Chapitres

### Chapitre 02 - ISPF et premiers programmes

| Programme | Description |
|-----------|-------------|
| `C02-DISPLAY.cbl` | DISPLAY, chaînes de caractères, continuation, debug |
| `C02-DECIMAL.cbl` | Nombres décimaux (V), signés (S), édition |

### Chapitre 03 - Déclaration des variables

| Programme | Description |
|-----------|-------------|
| `C03-NIVEAU88.cbl` | Niveau 88 (conditions), niveau 66 (RENAMES) |
| `C03-REDEFINES.cbl` | INITIALIZE, édition flottante (++), REDEFINES |

### Chapitre 04 - Opérations sur les données

| Programme | Description |
|-----------|-------------|
| `C04-ARITHM.cbl` | ADD, SUBTRACT, MULTIPLY, DIVIDE, ON SIZE ERROR |
| `C04-FACTURE.cbl` | COMPUTE, calcul facture avec TVA et remises |

### Chapitre 05 - Traitement conditionnel

| Programme | Description |
|-----------|-------------|
| `C05-EVALUATE.cbl` | EVALUATE avec ALSO (notes examen + stage) |
| `C05-PERFORM.cbl` | PERFORM VARYING (boucle simple avec somme) |
| `C05-BOUCLES.cbl` | PERFORM avec 3 boucles imbriquées |

### Chapitre 06 - Gestion des Tables

| Programme | Description |
|-----------|-------------|
| `C06-TINDICE.cbl` | Indice d'adressage des tables (subscript) |
| `C06-TINDEX.cbl` | Instruction SET sur les index |
| `C06-BANQUE01.cbl` | TD Banque - Déclaration des 5 tables |
| `C06-BANQUE02.cbl` | TD Banque - Chargement buffer et dispatch |
| `C06-BANQUE03.cbl` | TD Banque - Fichiers E/S (SEQUENTIAL → INDEXED) |

### Chapitre 07 - Gestion des Fichiers

| Programme | Description |
|-----------|-------------|
| `C07-EMPLOYE-WRITE.cbl` | Écriture fichier SEQUENTIAL (OUTPUT) |
| `C07-EMPLOYE-READ.cbl` | Lecture fichier SEQUENTIAL (INPUT) |
| `C07-EMPLOYE-EXTEND.cbl` | Ajout en fin de fichier (EXTEND) |

## Compilation

```bash
# Se placer dans le dossier du chapitre
cd chapitre-06

# Compiler
cobc -x C06-TINDICE.cbl -o C06-TINDICE

# Exécuter
./C06-TINDICE
```
