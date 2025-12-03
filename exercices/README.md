# Exercices COBOL

## Organisation

Les exercices sont organisés par chapitre du cours.

## Chapitres

### Chapitre 02 - ISPF et premiers programmes

| Programme | Description |
|-----------|-------------|
| `PG01CH01.cbl` | DISPLAY, chaînes de caractères, continuation, debug |
| `PG02CH01.cbl` | Nombres décimaux (V), signés (S), édition |

### Chapitre 03 - Déclaration des variables

| Programme | Description |
|-----------|-------------|
| `PG03CH03.cbl` | Niveau 88 (conditions), niveau 66 (RENAMES) |
| `PG04CH03.cbl` | INITIALIZE, édition flottante (++), REDEFINES |

### Chapitre 04 - Opérations sur les données

| Programme | Description |
|-----------|-------------|
| `PG05CH04.cbl` | ADD, SUBTRACT, MULTIPLY, DIVIDE, ON SIZE ERROR |
| `PG06CH04.cbl` | COMPUTE, calcul facture avec TVA et remises |

## Compilation

```bash
# Se placer dans le dossier du chapitre
cd chapitre-03

# Compiler
cobc -x PG03CH03.cbl -o PG03CH03

# Exécuter
./PG03CH03
```
