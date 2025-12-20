# Exercices COBOL - Chapitre V

## Thème : Traitement conditionnel

Ce chapitre couvre les structures de contrôle et les boucles.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C05-EVALUATE.cbl` | Instruction EVALUATE (switch/case) |
| `C05-PERFORM.cbl` | Instruction PERFORM et ses variantes |
| `C05-BOUCLES.cbl` | Boucles PERFORM UNTIL et PERFORM VARYING |

## Compilation et exécution

```bash
# Compiler un programme
cobc -x C05-EVALUATE.cbl -o C05-EVALUATE

# Exécuter
./C05-EVALUATE
```

## Concepts abordés

- Instruction IF / ELSE / END-IF
- Instruction EVALUATE (WHEN, OTHER)
- PERFORM simple et PERFORM THRU
- PERFORM UNTIL (boucle conditionnelle)
- PERFORM VARYING (boucle avec compteur)
- Conditions composées (AND, OR, NOT)

## Prérequis

- [Chapitre V - Traitement conditionnel](../../../cours/cobol/05-traitement-conditionnel.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IV](../chapitre-04/README.md) | [Chapitre VI](../chapitre-06/README.md) |
