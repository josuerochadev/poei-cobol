# Formation COBOL - POEI

Dépôt de suivi de ma formation POEI Développeur Mainframe COBOL dispensée par M2i Formation à Strasbourg (octobre 2025 – janvier 2026).
Technologies couvertes : z/OS, TSO/ISPF, JCL, VSAM, COBOL, DB2/SQL, CICS
Ce repository documente ma progression et regroupe les exercices, TP et projets réalisés durant la formation.

## Structure du projet

```
poei-cobol/
├── cours/                  # Cours par module
│   ├── zos-tso/            # Module Z/OS et TSO/ISPF (5 chapitres)
│   │   ├── 01-presentation-zos.md
│   │   ├── 02-fonctionnement-zos.md
│   │   ├── 03-tso.md
│   │   ├── 04-ispf.md
│   │   └── 05-architecture-zos.md
│   │
│   ├── jcl/                # Module JCL (4 chapitres)
│   │   ├── 01-cartes-job-exec-dd.md
│   │   ├── 02-fichiers-parametres.md
│   │   ├── 03-procedures.md
│   │   └── 04-utilitaires.md
│   │
│   ├── cobol/              # Module COBOL (12 chapitres)
│   │   ├── 01-structure-programme.md
│   │   ├── ...
│   │   └── 12-fichier-impression.md
│   │
│   ├── cics/               # Module CICS (8 chapitres)
│   │   ├── 01-presentation-generale.md
│   │   ├── 02-organisation-systeme.md
│   │   ├── 03-sgbd-ims.md
│   │   ├── 04-architecture-multicouches.md
│   │   ├── 05-couche-presentation.md
│   │   ├── 06-couche-traitement.md
│   │   ├── 07-couche-donnees.md
│   │   └── 08-travaux-pratiques.md
│   │
│   └── db2/                # Module DB2/SQL (10 chapitres)
│       ├── 01-fondamentaux-bd.md
│       ├── 02-architecture-db2.md
│       ├── 03-modelisation.md
│       ├── 04-modele-relationnel.md
│       ├── 05-types-db2i.md
│       ├── 06-sql-ddl.md
│       ├── 07-sql-dml.md
│       ├── 08-sql-select.md
│       ├── 09-sql-avance.md
│       └── 10-embedded-sql.md
│
├── exercices/              # Exercices par module
│   ├── zos-tso/            # Exercices TSO/ISPF
│   │
│   ├── jcl/                # Exercices JCL (24 fichiers .jcl)
│   │   ├── chapitre-02/
│   │   ├── chapitre-03/
│   │   ├── chapitre-04/
│   │   └── chapitre-05/    # Travaux pratiques + QCM
│   │
│   ├── cobol/              # Exercices COBOL (61 programmes)
│   │   ├── chapitre-02/
│   │   ├── ...
│   │   └── chapitre-12/
│   │
│   └── cics/               # Exercices CICS (1 TP)
│       └── tp-gestion-credits/  # TP complet architecture 3 tiers
│
├── projets/                # Projets complets
│   └── fil-rouge/          # Projet fil rouge (21 exercices)
│
├── hercules/               # Configuration Hercules/z/OS
├── docs/                   # Documentation et mémos
├── exemples/               # Exemples de code commentés
└── utils/                  # Scripts utilitaires
```

## Modules de formation

### Module Z/OS et TSO/ISPF

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Présentation générale de Z/OS | ✅ | - |
| II | Fonctionnement de Z/OS | ✅ | - |
| III | TSO (Time Sharing Option) | ✅ | ✅ |
| IV | ISPF/PDF | ✅ | - |
| V | Architecture Z/OS | ✅ | - |

### Module JCL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Cartes JOB, EXEC et DD | ✅ | - |
| II | Fichiers spéciaux et paramètres | ✅ | ✅ |
| III | Procédures JCL | ✅ | ✅ |
| IV | Utilitaires (IEBGENER, IDCAMS, SORT...) | ✅ | ✅ |
| V | Travaux pratiques (QCM + TPs) | - | ✅ |

### Module COBOL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Structure d'un programme COBOL | ✅ | - |
| II | Interface ISPF et commandes | ✅ | ✅ |
| III | Déclaration des variables | ✅ | ✅ |
| IV | Opérations sur les données | ✅ | ✅ |
| V | Traitement conditionnel | ✅ | ✅ |
| VI | Gestion des Tables | ✅ | ✅ |
| VII | Gestion des Fichiers | ✅ | ✅ |
| VIII | Opérations E/S sur les Fichiers | ✅ | ✅ |
| IX | Programmes et sous-programmes | ✅ | ✅ |
| X | Traitement des Fichiers | ✅ | ✅ |
| XI | Tri Interne (SORT/MERGE) | ✅ | ✅ |
| XII | Fichiers d'impression et édition | ✅ | ✅ |

### Module CICS

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Présentation générale | ✅ | - |
| II | Organisation du système | ✅ | - |
| III | SGBD IMS | ✅ | - |
| IV | Architecture Multicouches | ✅ | - |
| V | Couche de Présentation | ✅ | - |
| VI | Couche de Traitement | ✅ | - |
| VII | Couche des Données | ✅ | - |
| VIII | Travaux Pratiques | ✅ | ✅ |

### Module DB2/SQL

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Fondamentaux des bases de données | ✅ | - |
| II | Architecture DB2 | ✅ | - |
| III | Modélisation des données | ✅ | - |
| IV | Modèle relationnel | ✅ | - |
| V | Types de données et DB2I | ✅ | - |
| VI | SQL DDL (CREATE, ALTER, DROP) | ✅ | - |
| VII | SQL DML (INSERT, UPDATE, DELETE) | ✅ | - |
| VIII | SQL SELECT et Jointures | ✅ | - |
| IX | Agrégations et Sous-requêtes | ✅ | - |
| X | Embedded SQL COBOL | ✅ | - |

## Environnement

- **Compilateur** : GnuCOBOL 3.2.0
- **OS** : macOS
- **Émulateur** : Hercules (pour JCL/z/OS)

## Compilation et exécution

```bash
# Compiler un programme
cobc -x programme.cbl -o programme

# Compiler avec mode debug
cobc -x -fdebugging-line programme.cbl -o programme

# Exécuter
./programme
```

## Ressources

- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Course](https://www.tutorialspoint.com/cobol/index.htm)
