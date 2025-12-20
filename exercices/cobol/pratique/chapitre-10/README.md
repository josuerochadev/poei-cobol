# Exercices COBOL - Chapitre X

## Thème : Traitement des Fichiers (Synthèse)

Ce chapitre présente des cas pratiques avancés de gestion de fichiers.

## Fichiers

| Programme | Description |
|-----------|-------------|
| `C10-PERS-CREATE.cbl` | Création fichier indexé PERSONNEL |
| `C10-PERS-LIST.cbl` | Liste séquentielle du fichier |
| `C10-PERS-ADD.cbl` | Ajout d'enregistrements |
| `C10-PERS-UPDATE.cbl` | Mise à jour d'enregistrements |
| `C10-PERS-START.cbl` | Positionnement avec START |
| `C10-PERS-BYSS.cbl` | Lecture par sous-ensemble |
| `C10-ALTKEY.cbl` | Utilisation de clés alternatives |
| `C10-OPTIONAL.cbl` | Fichiers OPTIONAL |

## Compilation et exécution

```bash
# Compiler
cobc -x C10-PERS-CREATE.cbl -o C10-PERS-CREATE

# Exécuter
./C10-PERS-CREATE
```

## Ordre d'exécution recommandé

1. `C10-PERS-CREATE` - Crée le fichier PERSONNEL.dat
2. `C10-PERS-LIST` - Affiche le contenu
3. `C10-PERS-ADD` - Ajoute des enregistrements
4. `C10-PERS-UPDATE` - Modifie des enregistrements
5. `C10-PERS-START` - Positionnement et lecture
6. `C10-PERS-BYSS` - Lecture par plage de clés
7. `C10-ALTKEY` - Accès par clé alternative
8. `C10-OPTIONAL` - Gestion fichiers optionnels

## Concepts abordés

- ALTERNATE RECORD KEY
- START avec conditions (=, >, >=, <, <=)
- READ NEXT / READ PREVIOUS
- OPTIONAL dans SELECT
- FILE STATUS avancé
- Gestion des erreurs fichiers

## Prérequis

- [Chapitre X - Traitement des Fichiers](../../../cours/cobol/10-traitement-fichiers.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IX](../chapitre-09/README.md) | [Chapitre XI](../chapitre-11/README.md) |
