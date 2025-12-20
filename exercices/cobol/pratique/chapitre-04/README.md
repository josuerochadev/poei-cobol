# Exercices COBOL - Chapitre IV

## Thème : Opérations sur les données

Ce chapitre couvre les opérations arithmétiques en COBOL.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C04-ARITHM.cbl` | Instructions ADD, SUBTRACT, MULTIPLY, DIVIDE |
| `C04-FACTURE.cbl` | Calcul complet d'une facture avec TVA |

## Compilation et exécution

```bash
# Compiler un programme
cobc -x C04-ARITHM.cbl -o C04-ARITHM

# Exécuter
./C04-ARITHM
```

## Concepts abordés

- Instructions arithmétiques (ADD, SUBTRACT, MULTIPLY, DIVIDE)
- Instruction COMPUTE
- Gestion des arrondis (ROUNDED)
- Gestion des dépassements (ON SIZE ERROR)
- Calculs avec décimales

## Prérequis

- [Chapitre IV - Opérations sur les données](../../../cours/cobol/04-operations-donnees.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III](../chapitre-03/README.md) | [Chapitre V](../chapitre-05/README.md) |
