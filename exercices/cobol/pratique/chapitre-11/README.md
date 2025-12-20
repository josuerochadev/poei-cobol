# Exercices COBOL - Chapitre XI

## Thème : Tri Interne (SORT / MERGE)

Ce chapitre couvre le tri et la fusion de fichiers en COBOL.

## Fichiers

### Exercices de base

| Programme | Description |
|-----------|-------------|
| `C11-SORT-SIMPLE.cbl` | SORT basique avec USING/GIVING |
| `C11-SORT-INPUT.cbl` | SORT avec INPUT PROCEDURE |
| `C11-SORT-OUTPUT.cbl` | SORT avec OUTPUT PROCEDURE |
| `C11-SORT-COMPLET.cbl` | SORT avec INPUT et OUTPUT PROCEDURE |
| `C11-MERGE.cbl` | Fusion de fichiers triés |

### Application Client

| Programme | Description |
|-----------|-------------|
| `C11-CLIENT-CREATE.cbl` | Création fichier clients test |
| `C11-CLIENT-TRI-ASC.cbl` | Tri ascendant |
| `C11-CLIENT-TRI-DESC.cbl` | Tri descendant |
| `C11-CLIENT-TRI-PROC.cbl` | Tri avec procédures |
| `C11-CLIENT-SPLIT.cbl` | Éclatement en sous-fichiers |
| `C11-CLIENT-MERGE.cbl` | Fusion des sous-fichiers |

## Compilation et exécution

```bash
# Compiler
cobc -x C11-SORT-SIMPLE.cbl -o C11-SORT-SIMPLE

# Exécuter
./C11-SORT-SIMPLE
```

## Ordre d'exécution recommandé

**Exercices de base :**
1. `C11-SORT-SIMPLE` - Comprendre SORT basique
2. `C11-SORT-INPUT` - INPUT PROCEDURE
3. `C11-SORT-OUTPUT` - OUTPUT PROCEDURE
4. `C11-SORT-COMPLET` - Combinaison
5. `C11-MERGE` - Fusion

**Application Client :**
1. `C11-CLIENT-CREATE` - Crée le fichier
2. `C11-CLIENT-TRI-ASC` - Tri ascendant
3. `C11-CLIENT-TRI-DESC` - Tri descendant
4. `C11-CLIENT-SPLIT` - Éclate en 2 fichiers
5. `C11-CLIENT-MERGE` - Fusionne les fichiers

## Concepts abordés

- SORT ... USING ... GIVING
- INPUT PROCEDURE / OUTPUT PROCEDURE
- RELEASE / RETURN
- ASCENDING / DESCENDING KEY
- MERGE pour fusion de fichiers triés
- SD (Sort Description)

## Prérequis

- [Chapitre XI - Tri Interne](../../../cours/cobol/11-tri-interne.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre X](../chapitre-10/README.md) | [Chapitre XII](../chapitre-12/README.md) |
