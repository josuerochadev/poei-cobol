# Exercices COBOL - Chapitre VI

## Thème : Gestion des Tables

Ce chapitre couvre la manipulation des tableaux (OCCURS) en COBOL.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C06-TINDICE.cbl` | Tables avec indices (subscripts) |
| `C06-TINDEX.cbl` | Tables avec index (SET, SEARCH) |
| `C06-BANQUE01.cbl` | Application bancaire - Partie 1 |
| `C06-BANQUE02.cbl` | Application bancaire - Partie 2 |
| `C06-BANQUE03.cbl` | Application bancaire - Partie 3 |

## Compilation et exécution

```bash
# Compiler un programme
cobc -x C06-TINDICE.cbl -o C06-TINDICE

# Exécuter
./C06-TINDICE
```

## Ordre d'exécution recommandé

1. `C06-TINDICE.cbl` - Comprendre les indices
2. `C06-TINDEX.cbl` - Comprendre les index
3. `C06-BANQUE01.cbl` → `C06-BANQUE02.cbl` → `C06-BANQUE03.cbl`

## Concepts abordés

- Clause OCCURS (tableaux)
- Indices vs Index
- Instruction SET pour index
- SEARCH et SEARCH ALL
- Tables à plusieurs dimensions
- OCCURS DEPENDING ON

## Prérequis

- [Chapitre VI - Gestion des Tables](../../../cours/cobol/06-gestion-tables.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V](../chapitre-05/README.md) | [Chapitre VII](../chapitre-07/README.md) |
