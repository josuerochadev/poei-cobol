# Formation COBOL - POEI

Répertoire de formation COBOL, JCL et technologies Mainframe.

## Structure du projet

```
poei-cobol/
├── cours/              # Fiches de révision par chapitre
│   ├── 01-structure-programme.md
│   ├── 02-ispf-commandes.md
│   ├── 03-declaration-variables.md
│   └── 04-operations-donnees.md
│
├── exercices/          # Programmes COBOL par chapitre
│   ├── chapitre-02/    # DISPLAY, chaînes, debug
│   ├── chapitre-03/    # Variables, PICTURE, REDEFINES
│   └── chapitre-04/    # MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
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
| II | Interface ISPF et commandes | ✅ | PG01CH01, PG02CH01 |
| III | Déclaration des variables | ✅ | PG03CH03, PG04CH03 |
| IV | Opérations sur les données | ✅ | PG05CH04, PG06CH04 |

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
