# Exercices - Formation Mainframe

Ce dossier contient les exercices pratiques organisés par module.

## Modules disponibles

| Module | Description | Exercices |
|--------|-------------|-----------|
| [Z/OS et TSO](zos-tso/) | Exercices TSO, commandes système | 14 exercices |
| [JCL](jcl/) | Exercices JCL (chapitres 02 à 05) | 24 exercices |
| [VSAM](vsam/) | QCM + TP IDCAMS + Exercices pratiques | 40 exercices |
| [Algorithmique](algorithmique/) | QCM + Exercices pratiques | 24 exercices |
| [COBOL](cobol/) | Exercices COBOL (chapitres 02 à 12) | 61 programmes |
| [CICS](cics/) | QCM + TP pratiques + TP Gestion des Credits | 93 exercices |
| [DB2/SQL](db2/) | QCM + TP SQL + DDL/DML | 144 exercices |

## Organisation

Chaque module suit la numérotation des chapitres du cours correspondant.

```
exercices/
├── zos-tso/
│   └── 03-exercices-tso.md   # Commandes TSO
│
├── jcl/
│   ├── chapitre-02/    # Fichiers et paramètres
│   ├── chapitre-03/    # Procédures JCL
│   ├── chapitre-04/    # Utilitaires (IEBGENER, IDCAMS, SORT)
│   └── chapitre-05/    # Travaux pratiques + QCM
│
├── vsam/
│   ├── theorie/        # QCM (16 questions)
│   ├── pratique/       # Exercices IDCAMS par chapitre
│   └── tp/             # TP IDCAMS formateur
│
├── algorithmique/
│   ├── theorie/        # QCM (15 questions)
│   └── pratique/       # 9 exercices algorithmiques
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
├── cics/
│   ├── theorie/              # QCM (82 questions)
│   ├── pratique/             # Exercices BMS, COBOL, JCL (10 exercices)
│   ├── tp-gestion-credits/   # TP complet (transaction CRED)
│   └── fil-rouge/            # Projet fil rouge (a venir)
│
└── db2/                      # Exercices DB2/SQL
    ├── theorie/              # QCM (71 questions)
    ├── tp/                   # TP SQL (56 exercices)
    ├── pratique/             # DDL/DML (17 exercices)
    └── fil-rouge/            # Projet fil rouge (a venir)
```

## Compilation

```bash
# Compiler un exercice
cobc -x programme.cbl -o programme

# Exécuter
./programme
```
