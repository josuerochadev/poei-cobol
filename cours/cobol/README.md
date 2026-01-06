# Module COBOL - Common Business Oriented Language

## Introduction

COBOL est un langage de programmation créé en 1959 pour les applications de gestion d'entreprise. Malgré son âge, il reste omniprésent dans les systèmes critiques (banques, assurances, administrations) traitant des milliards de transactions quotidiennes.

Ce module couvre les fondamentaux du langage COBOL jusqu'aux techniques avancées de gestion de fichiers et de modularisation.

## Prérequis

- Connaissance de base de l'environnement mainframe (module Z/OS-TSO)
- Familiarité avec ISPF pour l'édition de code
- Notions de JCL pour la compilation et l'exécution

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

### Fondamentaux (Chapitres I-VI)
- Comprendre la structure d'un programme COBOL (4 divisions)
- Déclarer et manipuler des variables (PICTURE, niveaux, édition)
- Effectuer des opérations arithmétiques et logiques
- Utiliser les structures de contrôle (IF, EVALUATE, PERFORM)
- Gérer des tableaux avec OCCURS, SEARCH et indices

### Gestion de fichiers (Chapitres VII-X)
- Manipuler les trois organisations de fichiers (SEQUENTIAL, INDEXED, RELATIVE)
- Utiliser les opérations E/S (OPEN, READ, WRITE, REWRITE, DELETE, CLOSE)
- Gérer les erreurs via FILE STATUS
- Comprendre l'intégration avec VSAM et le JCL mainframe

### Techniques avancées (Chapitres XI-XII)
- Trier et fusionner des fichiers avec SORT/MERGE
- Créer des programmes modulaires (CALL, sous-programmes)
- Produire des états d'impression formatés

## Parcours de formation

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        MODULE COBOL - 12 CHAPITRES                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  PARTIE 1 : FONDAMENTAUX                                                    │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐                     │
│  │  Ch.I        │   │  Ch.II       │   │  Ch.III      │                     │
│  │  Structure   │──►│  ISPF &      │──►│  Variables   │                     │
│  │  Programme   │   │  Commandes   │   │  & PICTURE   │                     │
│  └──────────────┘   └──────────────┘   └──────────────┘                     │
│         │                                     │                             │
│         ▼                                     ▼                             │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐                     │
│  │  Ch.IV       │   │  Ch.V        │   │  Ch.VI       │                     │
│  │  Opérations  │──►│  Conditions  │──►│  Tables &    │                     │
│  │  Données     │   │  & Boucles   │   │  OCCURS      │                     │
│  └──────────────┘   └──────────────┘   └──────────────┘                     │
│                                               │                             │
├───────────────────────────────────────────────┼─────────────────────────────┤
│                                               ▼                             │
│  PARTIE 2 : FICHIERS                   ┌──────────────┐                     │
│  ┌──────────────┐   ┌──────────────┐   │  Ch.VII      │                     │
│  │  Ch.X        │◄──│  Ch.VIII     │◄──│  Gestion     │                     │
│  │  Synthèse    │   │  Opérations  │   │  Fichiers    │                     │
│  │  Fichiers    │   │  E/S         │   │              │                     │
│  └──────────────┘   └──────────────┘   └──────────────┘                     │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  PARTIE 3 : AVANCÉ                                                          │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐                     │
│  │  Ch.IX       │   │  Ch.XI       │   │  Ch.XII      │                     │
│  │  Sous-       │   │  SORT &      │   │  Fichiers    │                     │
│  │  Programmes  │   │  MERGE       │   │  Impression  │                     │
│  └──────────────┘   └──────────────┘   └──────────────┘                     │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Table des matières

### Partie 1 : Fondamentaux

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 01 | [Structure d'un programme](01-structure-programme.md) | 4 divisions, règles de codage, zones |
| 02 | [Interface ISPF et commandes](02-ispf-commandes.md) | Éditeur ISPF, commandes ligne, compilation |
| 03 | [Déclaration des variables](03-declaration-variables.md) | Niveaux, PICTURE, VALUE, REDEFINES, édition |
| 04 | [Opérations sur les données](04-operations-donnees.md) | MOVE, ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE |
| 05 | [Traitement conditionnel](05-traitement-conditionnel.md) | IF, EVALUATE, PERFORM, GO TO, boucles |
| 06 | [Gestion des Tables](06-gestion-tables.md) | OCCURS, indices, index, SET, SEARCH |

### Partie 2 : Gestion des Fichiers

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 07 | [Gestion des Fichiers](07-gestion-fichiers.md) | SEQUENTIAL, INDEXED, RELATIVE, FILE STATUS |
| 08 | [Opérations E/S](08-operations-es.md) | OPEN, READ, WRITE, REWRITE, DELETE, START, CLOSE |
| 10 | [Traitement des Fichiers](10-traitement-fichiers.md) | Synthèse mainframe, OPTIONAL, VSAM, AIX/PATH, JCL |

### Partie 3 : Techniques Avancées

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 09 | [Programmes et Sous-programmes](09-programmes-sous-programmes.md) | CALL, LINKAGE SECTION, BY REFERENCE/CONTENT |
| 11 | [Tri Interne](11-tri-interne.md) | SORT, MERGE, INPUT/OUTPUT PROCEDURE |
| 12 | [Fichier d'impression](12-fichier-impression.md) | Édition, caractères Z, *, signes, symboles |

## Progression

- [x] Chapitre I - Structure d'un programme COBOL
- [x] Chapitre II - Interface ISPF et commandes de base
- [x] Chapitre III - Déclaration des variables
- [x] Chapitre IV - Opérations sur les données
- [x] Chapitre V - Traitement conditionnel
- [x] Chapitre VI - Gestion des Tables
- [x] Chapitre VII - Gestion des Fichiers
- [x] Chapitre VIII - Opérations E/S sur les Fichiers
- [x] Chapitre IX - Programmes et Sous-programmes
- [x] Chapitre X - Traitement des Fichiers (synthèse)
- [x] Chapitre XI - Tri Interne (SORT / MERGE)
- [x] Chapitre XII - Fichier d'impression

## Aller plus loin

### Documentation officielle IBM

| Ressource | Description |
|-----------|-------------|
| [Enterprise COBOL for z/OS - Language Reference](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=cobol-language-reference) | Manuel de référence du langage |
| [Enterprise COBOL for z/OS - Programming Guide](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=cobol-programming-guide) | Guide de programmation |
| [COBOL Performance Tuning](https://www.ibm.com/docs/en/cobol-zos/6.4?topic=guide-performance-tuning) | Optimisation des programmes |

### GnuCOBOL (environnement local)

| Ressource | Description |
|-----------|-------------|
| [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides.html) | Guide officiel GnuCOBOL |
| [GnuCOBOL FAQ](https://gnucobol.sourceforge.io/faq/index.html) | Questions fréquentes |
| [GnuCOBOL Quick Reference](https://gnucobol.sourceforge.io/HTML/gnucobqr.html) | Référence rapide |

### Cours en ligne

| Plateforme | Cours | Niveau |
|------------|-------|--------|
| [IBM Skills](https://www.ibm.com/training/collection/cobol) | COBOL Programming | Tous niveaux |
| [Coursera - IBM](https://www.coursera.org/learn/cobol-programming) | Introduction to COBOL | Débutant |
| [Udemy](https://www.udemy.com/course/mainframe-the-complete-cobol-course-from-beginner-to-expert/) | Complete COBOL Course | Tous niveaux |
| [LinkedIn Learning](https://www.linkedin.com/learning/learning-cobol) | Learning COBOL | Débutant |

### Tutoriels gratuits

- [TutorialsPoint - COBOL](https://www.tutorialspoint.com/cobol/index.htm) - Tutoriel complet avec exemples
- [Mainframe Gurukul - COBOL](https://www.mainframegurukul.com/tutorials/programming/cobol/cobol-tutorial.html) - Cours détaillé
- [IBMMainframer - COBOL](https://www.ibmmainframer.com/cobol-tutorial/) - Tutoriel orienté mainframe
- [COBOL Café](https://www.codeproject.com/Articles/5293574/Learn-COBOL-in-One-Day) - Introduction rapide

### Livres recommandés

| Titre | Auteur | Description |
|-------|--------|-------------|
| *Murach's Mainframe COBOL* | Mike Murach | Référence pédagogique complète |
| *Beginning COBOL for Programmers* | Michael Coughlan | Pour les développeurs venant d'autres langages |
| *COBOL for the 21st Century* | Nancy Stern | Manuel universitaire classique |

### Outils et IDE

| Outil | Description |
|-------|-------------|
| [VS Code + COBOL extension](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol) | Coloration syntaxique et snippets |
| [IBM Z Open Editor](https://marketplace.visualstudio.com/items?itemName=IBM.zopeneditor) | Extension IBM pour VS Code |
| [OpenCobolIDE](https://pypi.org/project/OpenCobolIDE/) | IDE dédié à GnuCOBOL |

### Communautés

- [Stack Overflow - COBOL](https://stackoverflow.com/questions/tagged/cobol) - Questions/réponses
- [Reddit r/cobol](https://www.reddit.com/r/cobol/) - Communauté COBOL
- [IBM Z Community](https://community.ibm.com/community/user/ibmz-and-linuxone) - Forum IBM officiel
- [Open Mainframe Project](https://www.openmainframeproject.org/) - Communauté open source mainframe

### Environnements de pratique

- [IBM Z Trial](https://www.ibm.com/z/trials) - Accès gratuit à un environnement z/OS réel
- [Hercules + TK4-](http://wotho.ethz.ch/tk4-/) - MVS 3.8j gratuit pour pratiquer
- [COBOL Technical Foundation](https://www.ibm.com/community/z/open-mainframe-project/cobol/) - Ressources IBM/Open Mainframe

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [Algorithmique](../algorithmique/README.md) | [CICS](../cics/README.md) |

---
*Formation COBOL - M2i Formation*
