# Exercices - Formation Mainframe

Ce dossier contient les exercices pratiques organisés par module.

## Modules disponibles

| Module | Description | Exercices |
|--------|-------------|-----------|
| [COBOL](cobol/) | Exercices COBOL (chapitres 02 à 09) | 40+ exercices |
| [CICS](cics/) | Exercices CICS (à venir) | - |

## Organisation

Chaque module suit la numérotation des chapitres du cours correspondant.

```
exercices/
├── cobol/
│   ├── chapitre-02/    # DISPLAY, chaînes
│   ├── chapitre-03/    # Variables, PICTURE
│   ├── chapitre-04/    # Opérations arithmétiques
│   ├── chapitre-05/    # Conditions, boucles
│   ├── chapitre-06/    # Tables (OCCURS)
│   ├── chapitre-07/    # Fichiers séquentiels
│   ├── chapitre-08/    # VSAM (KSDS, ESDS, RRDS)
│   └── chapitre-09/    # Sous-programmes (CALL)
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
