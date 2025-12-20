# Exercices COBOL - Chapitre VIII

## Thème : Opérations E/S sur les Fichiers

Ce chapitre approfondit les opérations sur fichiers indexés et relatifs.

## Fichiers

### Série EMPL (Fichiers indexés KSDS)

| Programme | Description |
|-----------|-------------|
| `C08-EMPL-WRITE.cbl` | Création fichier indexé employés |
| `C08-EMPL-PRINT.cbl` | Lecture et affichage |

### Série RELEVE (Fichiers indexés)

| Programme | Description |
|-----------|-------------|
| `C08-RELEVE-INIT.cbl` | Initialisation fichier relevés |
| `C08-RELEVE.cbl` | Gestion des relevés bancaires |

### Série RRDS (Fichiers relatifs)

| Programme | Description |
|-----------|-------------|
| `C08-RRDS-WRITE.cbl` | Création fichier relatif |
| `C08-RRDS-READ.cbl` | Lecture par numéro relatif |
| `C08-RRDS-ADD.cbl` | Ajout d'enregistrements |
| `C08-RRDS-REWRITE.cbl` | Mise à jour |
| `C08-RRDS-DELETE.cbl` | Suppression |
| `C08-RRDS-LIST.cbl` | Liste complète |

## Compilation et exécution

```bash
# Compiler
cobc -x C08-RRDS-WRITE.cbl -o C08-RRDS-WRITE

# Exécuter
./C08-RRDS-WRITE
```

## Ordre d'exécution recommandé

**Série RRDS :**
1. `C08-RRDS-WRITE` - Crée le fichier
2. `C08-RRDS-LIST` - Affiche le contenu
3. `C08-RRDS-READ` - Lecture par clé
4. `C08-RRDS-ADD` - Ajout
5. `C08-RRDS-REWRITE` - Modification
6. `C08-RRDS-DELETE` - Suppression

## Concepts abordés

- Fichiers INDEXED (KSDS)
- Fichiers RELATIVE (RRDS)
- READ avec clé, START, READ NEXT
- REWRITE, DELETE
- INVALID KEY / NOT INVALID KEY
- FILE STATUS codes

## Prérequis

- [Chapitre VIII - Opérations E/S](../../../cours/cobol/08-operations-es.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VII](../chapitre-07/README.md) | [Chapitre IX](../chapitre-09/README.md) |
