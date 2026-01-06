# Module JCL (Job Control Language)

## Présentation

Ce module couvre le **Job Control Language (JCL)**, le langage de contrôle des travaux sur z/OS. Le JCL est essentiel pour soumettre et exécuter des programmes batch sur mainframe.

## Chapitres disponibles

| Fichier | Chapitre | Contenu |
|---------|----------|---------|
| [01-cartes-job-exec-dd.md](01-cartes-job-exec-dd.md) | I | Cartes JOB, EXEC, DD - Structure et paramètres |
| [02-fichiers-parametres.md](02-fichiers-parametres.md) | II | Concaténation, fichiers PS/PO, temporaires, références |
| [03-procedures.md](03-procedures.md) | III | Procédures in-stream, cataloguées, imbriquées, paramètres |
| [04-utilitaires.md](04-utilitaires.md) | IV | Utilitaires système (IEBGENER, IEBCOPY, IDCAMS, SORT) |

**Travaux Pratiques :** [Exercices JCL](../../exercices/jcl/tp/README.md) - QCM et TP de synthèse

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

- Comprendre la structure et la syntaxe du JCL
- Écrire des cartes JOB, EXEC et DD
- Définir des fichiers avec les paramètres DSN, DISP, DCB, SPACE
- Utiliser les procédures cataloguées et in-stream
- Gérer les bibliothèques (JOBLIB, STEPLIB, JCLLIB)
- Utiliser les utilitaires système courants

## Progression

- [x] Chapitre I - Cartes JOB, EXEC, DD
- [x] Chapitre II - Fichiers spéciaux et paramètres
- [x] Chapitre III - Procédures
- [x] Chapitre IV - Utilitaires
- [x] Chapitre V - Travaux pratiques (dans exercices/)

## Prérequis

- Module Z/OS et TSO/ISPF (recommandé)
- Compréhension de l'environnement mainframe
- Notions de base sur les datasets

## Structure d'un JCL

```
┌─────────────────────────────────────────────────────────────────┐
│                    STRUCTURE JCL DE BASE                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //JOBNAME  JOB  ...            ◄── Carte JOB (1 par job)     │
│   //*                                                           │
│   //STEP1    EXEC PGM=...        ◄── Carte EXEC (1+ par job)   │
│   //INPUT    DD   DSN=...        ◄── Carte DD (0+ par step)    │
│   //OUTPUT   DD   DSN=...                                       │
│   //*                                                           │
│   //STEP2    EXEC PGM=...                                       │
│   //...                                                         │
│   //                                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Lien avec les autres modules

```
┌─────────────────────────────────────────────────────────────────┐
│                PARCOURS DE FORMATION                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ┌─────────────┐                                               │
│   │   Z/OS      │  Prerequis                                    │
│   │  TSO/ISPF   │                                               │
│   └──────┬──────┘                                               │
│          │                                                      │
│          ▼                                                      │
│   ┌─────────────┐                                               │
│   │    JCL      │  ◄── VOUS ÊTES ICI                           │
│   │             │                                               │
│   └──────┬──────┘                                               │
│          │                                                      │
│          ▼                                                      │
│   ┌─────────────┐                                               │
│   │   COBOL     │  Langage de programmation                     │
│   │             │                                               │
│   └──────┬──────┘                                               │
│          │                                                      │
│          ▼                                                      │
│   ┌─────────────┐                                               │
│   │   CICS      │  Transactionnel temps réel                    │
│   │             │                                               │
│   └─────────────┘                                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Aller plus loin

### Documentation officielle IBM

| Ressource | Description |
|-----------|-------------|
| [z/OS MVS JCL Reference](https://www.ibm.com/docs/en/zos/2.5.0?topic=mvs-zos-jcl-reference) | Manuel de référence JCL complet |
| [z/OS MVS JCL User's Guide](https://www.ibm.com/docs/en/zos/2.5.0?topic=mvs-zos-jcl-users-guide) | Guide utilisateur JCL |
| [z/OS Utilities Reference](https://www.ibm.com/docs/en/zos/2.5.0?topic=dfsms-zos-utilities) | Référence des utilitaires (IEBGENER, IEBCOPY, etc.) |
| [DFSORT Application Programming Guide](https://www.ibm.com/docs/en/zos/2.5.0?topic=dfsort) | Guide complet DFSORT/ICETOOL |
| [IDCAMS Reference](https://www.ibm.com/docs/en/zos/2.5.0?topic=dfsms-zos-idcams-access-method-services) | Commandes IDCAMS pour VSAM |

### Cours en ligne

| Plateforme | Cours | Niveau |
|------------|-------|--------|
| [Coursera - IBM](https://www.coursera.org/learn/introduction-to-jcl) | Introduction to z/OS JCL | Débutant |
| [Udemy](https://www.udemy.com/course/jcl-tutorial-for-beginners/) | JCL Tutorial for Beginners | Débutant |
| [Udemy](https://www.udemy.com/course/mainframe-the-complete-jcl-course/) | Mainframe: The Complete JCL Course | Tous niveaux |
| [Interskill](https://interskill.com/) | Mainframe Specialist - JCL Foundations | Débutant |
| [LinkedIn Learning](https://www.linkedin.com/learning/) | Mainframe JCL Essentials | Intermédiaire |

### Tutoriels gratuits

- [IBMMainframer.com - JCL Tutorial](https://www.ibmmainframer.com/jcl-tutorial/) - Tutoriel JCL complet avec exemples
- [TutorialBrain - JCL](https://www.tutorialbrain.com/mainframe/jcl_tutorial/) - Guide détaillé JCL
- [Mainframe Gurukul - JCL](https://www.mainframegurukul.com/tutorials/programming/jcl/jcl-tutorial.html) - Cours JCL avec exercices
- [TechGuru99 - JCL](https://www.guru99.com/jcl-job-control-language-tutorial.html) - JCL Tutorial for Beginners

### Outils et utilitaires

| Outil | Description |
|-------|-------------|
| [JCL Checker](https://www.file-aid.com/) | Vérificateur de syntaxe JCL |
| [DFSORT Tricks](https://www.dfsort.com/) | Astuces et exemples DFSORT/ICETOOL |
| [CBTTape](http://www.cbttape.org/) | Collection d'utilitaires mainframe gratuits |

### Références rapides

- [JCL Quick Reference Card](https://www.ibm.com/docs/en/zos/2.5.0?topic=reference-quick) - Carte de référence rapide IBM
- [SORT Tricks](https://www.dfsort.com/tricks/) - 100+ astuces DFSORT par Frank Yaeger (IBM)
- [IDCAMS Commands Cheat Sheet](https://www.ibmmainframer.com/idcams-tutorial/) - Aide-mémoire IDCAMS

### Communautés

- [IBM Z Community](https://community.ibm.com/community/user/ibmz-and-linuxone) - Forum officiel IBM
- [Planet Mainframe](https://planetmainframe.com/) - Blog et actualités mainframe
- [Stack Overflow - JCL](https://stackoverflow.com/questions/tagged/jcl) - Questions/réponses JCL
- [r/mainframe](https://www.reddit.com/r/mainframe/) - Subreddit dédié au mainframe

### Environnement de pratique

- [IBM Z Trial](https://www.ibm.com/z/trials) - Accès gratuit à un environnement z/OS réel
- [Hercules Emulator + TK4-](http://wotho.ethz.ch/tk4-/) - MVS 3.8j avec JES2 pour pratiquer le JCL

---

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [Z/OS TSO/ISPF](../zos-tso/README.md) | [VSAM](../vsam/README.md) |

---
*Formation JCL - M2i Formation*
