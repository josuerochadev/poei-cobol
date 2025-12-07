# Exercices COBOL - Chapitre VII

## Thème : Gestion des Fichiers

Ce chapitre introduit la manipulation des fichiers séquentiels.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C07-EMPLOYE-WRITE.cbl` | Création d'un fichier séquentiel |
| `C07-EMPLOYE-READ.cbl` | Lecture d'un fichier séquentiel |
| `C07-EMPLOYE-EXTEND.cbl` | Ajout en fin de fichier (EXTEND) |

## Compilation et exécution

```bash
# Compiler
cobc -x C07-EMPLOYE-WRITE.cbl -o C07-EMPLOYE-WRITE

# Exécuter dans l'ordre
./C07-EMPLOYE-WRITE    # Crée le fichier
./C07-EMPLOYE-READ     # Lit le fichier
./C07-EMPLOYE-EXTEND   # Ajoute des enregistrements
```

## Ordre d'exécution obligatoire

1. `C07-EMPLOYE-WRITE.cbl` - Crée le fichier EMPLOYE.dat
2. `C07-EMPLOYE-READ.cbl` - Lit et affiche le contenu
3. `C07-EMPLOYE-EXTEND.cbl` - Ajoute des enregistrements

## Concepts abordés

- SELECT ... ASSIGN TO
- OPEN OUTPUT / INPUT / EXTEND
- WRITE, READ, CLOSE
- FILE STATUS
- Gestion de fin de fichier (AT END)

## Prérequis

- [Chapitre VII - Gestion des Fichiers](../../../cours/cobol/07-gestion-fichiers.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI](../chapitre-06/README.md) | [Chapitre VIII](../chapitre-08/README.md) |
