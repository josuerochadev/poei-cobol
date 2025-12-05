# Exercices - Formation Mainframe

Ce dossier contient les exercices pratiques organisés par module.

## Modules disponibles

| Module | Description | Exercices |
|--------|-------------|-----------|
| [Z/OS et TSO](zos-tso/) | Exercices TSO, commandes système | 14 exercices |
| [COBOL](cobol/) | Exercices COBOL (chapitres 02 à 12) | 50+ exercices |
| [CICS](cics/) | Exercices CICS (à venir) | - |

## Organisation

Chaque module suit la numérotation des chapitres du cours correspondant.

```
exercices/
├── zos-tso/
│   └── 03-exercices-tso.md   # Commandes TSO
│
├── cobol/
│   ├── chapitre-02/    # DISPLAY, chaînes
│   ├── chapitre-03/    # Variables, PICTURE
│   ├── chapitre-04/    # Opérations arithmétiques
│   ├── chapitre-05/    # Conditions, boucles
│   ├── chapitre-06/    # Tables (OCCURS)
│   ├── chapitre-07/    # Fichiers séquentiels
│   ├── chapitre-08/    # VSAM (KSDS, ESDS, RRDS)
│   ├── chapitre-09/    # Sous-programmes (CALL)
│   ├── chapitre-10/    # Fichiers avancés (KSDS)
│   ├── chapitre-11/    # Tri interne (SORT/MERGE)
│   └── chapitre-12/    # Impression, édition
│
└── cics/
    └── (à venir)
```

## Compilation

```bash
# Compiler un exercice
cobc -x programme.cbl -o programme

# Exécuter
./programme
```
