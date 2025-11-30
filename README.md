# Formation COBOL - POEI

Répertoire de formation COBOL, JCL et technologies Mainframe.

## Structure du projet

```
poei-cobol/
├── 01-bases-cobol/     # Concepts de base COBOL
├── 02-exercices/       # Exercices pratiques
├── 03-jcl/            # Job Control Language
├── 04-mainframe/      # Concepts mainframe (CICS, DB2, VSAM, etc.)
├── 05-projets/        # Projets plus complets
├── docs/              # Documentation et notes de cours
├── exemples/          # Exemples de code commentés
└── utils/             # Scripts utilitaires
```

## Environnement

- **Compilateur** : GNU COBOL 3.2.0
- **OS** : macOS

## Compilation et exécution

### Compiler un programme COBOL
```bash
cobc -x nom_programme.cbl
```

### Compiler avec options de débogage
```bash
cobc -x -debug -g nom_programme.cbl
```

### Exécuter
```bash
./nom_programme
```

## Ressources

- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Course](https://www.tutorialspoint.com/cobol/index.htm)

## Notes

Ce répertoire contient tous les exercices, exemples et projets réalisés pendant la formation.
