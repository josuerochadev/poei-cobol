<div align="center">

# Formation POEI Mainframe COBOL

**Support de formation complet pour le developpement mainframe : z/OS, JCL, VSAM, COBOL, DB2/SQL, CICS.**

![COBOL](https://img.shields.io/badge/COBOL_85-005CA5?style=flat&logo=ibm&logoColor=white)
![z/OS](https://img.shields.io/badge/z%2FOS-054ADA?style=flat&logo=ibm&logoColor=white)
![JCL](https://img.shields.io/badge/JCL-333333?style=flat)
![VSAM](https://img.shields.io/badge/VSAM-6C6C6C?style=flat)
![DB2](https://img.shields.io/badge/DB2-0F62FE?style=flat&logo=ibm&logoColor=white)
![CICS](https://img.shields.io/badge/CICS_TS-1F70C1?style=flat&logo=ibm&logoColor=white)
![GnuCOBOL](https://img.shields.io/badge/GnuCOBOL-4E9A06?style=flat)

[Projet fil rouge](https://github.com/josuerochadev/fil-rouge-mainframe) · [Portfolio](https://josuerocha.dev)

</div>

---

## A propos

Depot de suivi de ma formation POEI Developpeur Mainframe COBOL (M2i Formation, Strasbourg, octobre 2025 - janvier 2026). Ce repository regroupe l'ensemble des cours reformules, exercices pratiques, QCM et travaux pratiques realises durant les 3 mois de formation, avant ma prise de poste en tant qu'ingenieur d'etudes mainframe chez CELAD.

Les projets fil rouge (batch, DB2, CICS) sont dans un [repo dedie](https://github.com/josuerochadev/fil-rouge-mainframe) avec une demo web deployable.

## Contenu

- 7 modules de cours (56 chapitres en markdown)
- 117 programmes COBOL, 139 JCL, 19 maps BMS, 23 scripts SQL
- Exercices theoriques (QCM) et pratiques par chapitre
- Configuration emulateur Hercules TK5
- Scripts utilitaires (compilation, audit)

## Stack technique

| Categorie | Outils |
|-----------|--------|
| Langage | COBOL 85 |
| Systeme | z/OS, TSO/ISPF |
| Jobs | JCL (Job Control Language) |
| Fichiers | VSAM KSDS, fichiers sequentiels |
| Base de donnees | DB2/SQL |
| Transactionnel | CICS TS, BMS |
| Emulateur | Hercules TK5 |
| Compilation locale | GnuCOBOL 3.2.0 |

## Modules de formation

### z/OS et TSO/ISPF

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Presentation generale de z/OS | oui | - |
| II | Fonctionnement de z/OS | oui | - |
| III | TSO (Time Sharing Option) | oui | oui |
| IV | ISPF/PDF | oui | - |
| V | Architecture z/OS | oui | - |

### JCL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Cartes JOB, EXEC et DD | oui | - |
| II | Fichiers speciaux et parametres | oui | oui |
| III | Procedures JCL | oui | oui |
| IV | Utilitaires (IEBGENER, IDCAMS, SORT) | oui | oui |
| V | Travaux pratiques (QCM + TP) | - | oui |

### Algorithmique

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Introduction a l'algorithmique | oui | QCM + 9 ex |
| II.1 | Structures de donnees (tableaux, enregistrements) | oui | QCM + 16 ex |
| II.2 | Pointeurs et listes chainees | oui | QCM + 3 ex |
| II.3 | Piles et files (LIFO/FIFO) | oui | QCM + 4 ex |
| III | Recursivite | oui | QCM + 8 ex |
| IV | Algorithmes de tri | oui | QCM |
| V | Complexite algorithmique | oui | QCM |
| VI | Algorithmes sur fichiers | oui | QCM |
| VII | Modularite | oui | QCM |

### COBOL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Structure d'un programme COBOL | oui | - |
| II | Interface ISPF et commandes | oui | oui |
| III | Declaration des variables | oui | oui |
| IV | Operations sur les donnees | oui | oui |
| V | Traitement conditionnel | oui | oui |
| VI | Gestion des tables | oui | oui |
| VII | Gestion des fichiers | oui | oui |
| VIII | Operations E/S sur les fichiers | oui | oui |
| IX | Programmes et sous-programmes | oui | oui |
| X | Traitement des fichiers | oui | oui |
| XI | Tri interne (SORT/MERGE) | oui | oui |
| XII | Fichiers d'impression et edition | oui | oui |

### CICS

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Presentation generale | oui | QCM |
| II | Organisation du systeme | oui | QCM |
| III | SGBD IMS | oui | QCM |
| IV | Architecture multicouches | oui | QCM |
| V | Couche de presentation | oui | QCM |
| VI | Couche de traitement | oui | QCM |
| VII | Couche des donnees | oui | - |
| VIII | Travaux pratiques | oui | TP |
| IX | Architecture et transactions TSI | oui | QCM |

### DB2/SQL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Fondamentaux des bases de donnees | oui | QCM |
| II | Architecture DB2 | oui | QCM |
| III | Modelisation des donnees | oui | QCM |
| IV | Modele relationnel | oui | QCM |
| V | Types de donnees et DB2I | oui | QCM |
| VI | SQL DDL (CREATE, ALTER, DROP) | oui | TP |
| VII | SQL DML (INSERT, UPDATE, DELETE) | oui | TP |
| VIII | SQL SELECT et jointures | oui | TP |
| IX | Agregations et sous-requetes | oui | TP |
| X | Embedded SQL COBOL | oui | Fil rouge |

## Demarrer

### Prerequis

- [GnuCOBOL](https://gnucobol.sourceforge.io/) 3.2.0 ou superieur
- [Hercules TK5](http://wotho.ethz.ch/tk4-/) pour l'emulation z/OS (optionnel)

### Compilation et execution

```bash
# Compiler un programme
cobc -x programme.cbl -o programme

# Compiler avec mode debug (active les lignes D en colonne 7)
cobc -x -fdebugging-line programme.cbl -o programme

# Executer
./programme
```

## Architecture

```text
poei-cobol/
├── cours/                  Supports de cours par module (56 chapitres)
│   ├── zos-tso/            z/OS, TSO/ISPF (5 chapitres)
│   ├── jcl/                Job Control Language (4 chapitres)
│   ├── vsam/               Virtual Storage Access Method
│   ├── algorithmique/      Algorithmique (9 chapitres)
│   ├── cobol/              COBOL (12 chapitres)
│   ├── cics/               CICS transactionnel (9 chapitres)
│   └── db2/                DB2/SQL (10 chapitres)
├── exercices/              Exercices par module
│   ├── cobol/              84 programmes COBOL par chapitre
│   ├── cics/               Exercices BMS, COBOL, JCL + fil rouge
│   ├── db2/                QCM, TP SQL, fil rouge COBOL-DB2
│   ├── jcl/                QCM + TP
│   ├── vsam/               TP IDCAMS, KSDS, ESDS, RRDS
│   └── algorithmique/      40 exercices + 9 QCM
├── hercules/               Configuration emulateur z/OS
│   ├── jcl/                JCL pour Hercules
│   ├── proclib/            Procedures cataloguees
│   └── data/               Fichiers VSAM et sequentiels
├── docs/                   Memos et documentation
├── exemples/               Exemples de code commentes
└── utils/                  Scripts utilitaires (compilation, audit)
```

---

Construit par **[Josue Rocha](https://josuerocha.dev)** · [LinkedIn](https://linkedin.com/in/josuerocha) · [GitHub](https://github.com/josuerochadev)
