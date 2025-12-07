# Exercices COBOL - Chapitre II

## Thème : Interface ISPF et commandes de base

Ce chapitre introduit les premiers programmes COBOL avec affichage console.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C02-DISPLAY.cbl` | Instruction DISPLAY - Affichage de texte |
| `C02-DECIMAL.cbl` | Manipulation de nombres décimaux |

## Compilation et exécution

```bash
# Compiler un programme
cobc -x C02-DISPLAY.cbl -o C02-DISPLAY

# Exécuter
./C02-DISPLAY
```

## Concepts abordés

- Structure de base d'un programme COBOL
- Instruction `DISPLAY`
- Variables numériques et alphanumériques
- Clauses PICTURE de base

## Prérequis

- [Chapitre II - Interface ISPF et commandes](../../../cours/cobol/02-ispf-commandes.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre III](../chapitre-03/README.md) |
