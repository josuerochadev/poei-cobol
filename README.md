# Formation COBOL - POEI

Dépôt de suivi de ma formation POEI Développeur Mainframe COBOL dispensée par M2i Formation à Strasbourg (octobre 2025 – janvier 2026).
Technologies couvertes : z/OS, TSO/ISPF, JCL, VSAM, COBOL, DB2/SQL, CICS
Ce repository documente ma progression et regroupe les exercices, TP et projets réalisés durant la formation.

## Structure du projet

```
poei-cobol/
├── cours/              # Fiches de révision par chapitre
│   ├── 01-structure-programme.md
│   ├── 02-ispf-commandes.md
│   ├── 03-declaration-variables.md
│   ├── 04-operations-donnees.md
│   └── 05-traitement-conditionnel.md
│
├── exercices/          # Programmes COBOL par chapitre
│   ├── chapitre-02/    # DISPLAY, chaînes, debug
│   ├── chapitre-03/    # Variables, PICTURE, REDEFINES
│   ├── chapitre-04/    # MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
│   ├── chapitre-05/    # IF, EVALUATE, PERFORM
│   ├── chapitre-06/    # Tables (OCCURS), indices, index, TD Banque
│   ├── chapitre-07/    # Fichiers SEQUENTIAL (READ, WRITE, EXTEND)
│   └── chapitre-08/    # Fichiers E/S avancés, RRDS (RELATIVE)
│
├── projets/            # Projets complets
│   └── fil-rouge/      # Projet fil rouge (21 exercices)
│
├── docs/               # Documentation et mémos
├── exemples/           # Exemples de code commentés
└── utils/              # Scripts utilitaires
```

## Chapitres couverts

| Chapitre | Sujet | Cours | Exercices |
|----------|-------|-------|-----------|
| I | Structure d'un programme COBOL | ✅ | - |
| II | Interface ISPF et commandes | ✅ | C02-DISPLAY, C02-DECIMAL |
| III | Déclaration des variables | ✅ | C03-NIVEAU88, C03-REDEFINES |
| IV | Opérations sur les données | ✅ | C04-ARITHM, C04-FACTURE |
| V | Traitement conditionnel | ✅ | C05-EVALUATE, C05-PERFORM, C05-BOUCLES |
| VI | Gestion des Tables | ✅ | C06-TINDICE, C06-TINDEX, C06-BANQUE* |
| VII | Gestion des Fichiers | ✅ | C07-EMPLOYE-WRITE/READ/EXTEND |
| VIII | Opérations E/S avancées | ✅ | C08-EMPL-*, C08-RRDS-* |

## Environnement

- **Compilateur** : GnuCOBOL 3.2.0
- **OS** : macOS

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
