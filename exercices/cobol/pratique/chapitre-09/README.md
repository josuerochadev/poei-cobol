# Exercices COBOL - Chapitre IX

## Thème : Programmes et Sous-programmes

Ce chapitre couvre la modularisation avec CALL et sous-programmes.

## Fichiers

### Programmes principaux

| Programme | Description |
|-----------|-------------|
| `C09-APPELANT.cbl` | Programme principal appelant |
| `C09-BYREF-DEMO.cbl` | Démonstration BY REFERENCE vs BY CONTENT |
| `C09-PERSREV.cbl` | Programme principal calcul revenus |

### Sous-programmes

| Programme | Description |
|-----------|-------------|
| `C09-CALCUL.cbl` | Sous-programme de calcul |
| `C09-MODIF.cbl` | Sous-programme de modification |
| `C09-TRIEUR.cbl` | Sous-programme de tri |
| `C09-VALID.cbl` | Sous-programme de validation |
| `C09-CALREV.cbl` | Sous-programme calcul revenus |

## Compilation et exécution

```bash
# Compiler le sous-programme en module
cobc -c C09-CALCUL.cbl

# Compiler le programme principal avec le sous-programme
cobc -x C09-APPELANT.cbl C09-CALCUL.o -o C09-APPELANT

# Exécuter
./C09-APPELANT
```

## Ordre de compilation

1. Compiler d'abord les sous-programmes (`cobc -c`)
2. Puis compiler le programme principal en liant les modules

**Exemple complet :**
```bash
cobc -c C09-CALREV.cbl
cobc -x C09-PERSREV.cbl C09-CALREV.o -o C09-PERSREV
./C09-PERSREV
```

## Concepts abordés

- Instruction CALL
- LINKAGE SECTION
- BY REFERENCE vs BY CONTENT vs BY VALUE
- RETURNING
- Passage de paramètres
- CANCEL (déchargement)

## Prérequis

- [Chapitre IX - Programmes et Sous-programmes](../../../cours/cobol/09-programmes-sous-programmes.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VIII](../chapitre-08/README.md) | [Chapitre X](../chapitre-10/README.md) |
