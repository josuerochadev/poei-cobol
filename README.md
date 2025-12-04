# Formation COBOL - POEI

Dépôt de suivi de ma formation POEI Développeur Mainframe COBOL dispensée par M2i Formation à Strasbourg (octobre 2025 – janvier 2026).
Technologies couvertes : z/OS, TSO/ISPF, JCL, VSAM, COBOL, DB2/SQL, CICS
Ce repository documente ma progression et regroupe les exercices, TP et projets réalisés durant la formation.

## Structure du projet

```
poei-cobol/
├── cours/                  # Cours par module
│   ├── cobol/              # Module COBOL (9 chapitres)
│   │   ├── 01-structure-programme.md
│   │   ├── 02-ispf-commandes.md
│   │   ├── ...
│   │   └── 09-programmes-sous-programmes.md
│   │
│   └── cics/               # Module CICS (3 chapitres)
│       ├── 01-presentation-generale.md
│       ├── 02-organisation-systeme.md
│       └── 03-sgbd-ims.md
│
├── exercices/              # Exercices par module
│   ├── cobol/              # Exercices COBOL
│   │   ├── chapitre-02/
│   │   ├── ...
│   │   └── chapitre-09/
│   │
│   └── cics/               # Exercices CICS (à venir)
│
├── projets/                # Projets complets
│   └── fil-rouge/          # Projet fil rouge
│
├── hercules/               # Configuration Hercules/z/OS
├── docs/                   # Documentation et mémos
├── exemples/               # Exemples de code commentés
└── utils/                  # Scripts utilitaires
```

## Modules de formation

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
| VIII | Opérations E/S avancées (VSAM) | ✅ | ✅ |
| IX | Programmes et sous-programmes | ✅ | ✅ |

### Module CICS

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Présentation générale | ✅ | - |
| II | Organisation du système | ✅ | - |
| III | SGBD IMS | ✅ | - |

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
