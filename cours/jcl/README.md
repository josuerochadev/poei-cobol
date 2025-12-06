# Module JCL (Job Control Language)

## Presentation

Ce module couvre le **Job Control Language (JCL)**, le langage de controle des travaux sur z/OS. Le JCL est essentiel pour soumettre et executer des programmes batch sur mainframe.

## Chapitres disponibles

| Fichier | Chapitre | Contenu |
|---------|----------|---------|
| `01-cartes-job-exec-dd.md` | I | Cartes JOB, EXEC, DD - Structure et parametres |
| `02-fichiers-parametres.md` | II | Concatenation, fichiers PS/PO, temporaires, references |
| `03-procedures.md` | III | Procedures cataloguees et in-stream *(a venir)* |
| `04-utilitaires.md` | IV | Utilitaires systeme (IEBGENER, IEBCOPY, IDCAMS...) *(a venir)* |
| `05-travaux-pratiques.md` | V | Exercices pratiques JCL *(a venir)* |

## Objectifs pedagogiques

A l'issue de ce module, vous serez capable de :

- Comprendre la structure et la syntaxe du JCL
- Ecrire des cartes JOB, EXEC et DD
- Definir des fichiers avec les parametres DSN, DISP, DCB, SPACE
- Utiliser les procedures cataloguees et in-stream
- Gerer les bibliotheques (JOBLIB, STEPLIB, JCLLIB)
- Utiliser les utilitaires systeme courants

## Progression

- [x] Chapitre I - Cartes JOB, EXEC, DD
- [x] Chapitre II - Fichiers speciaux et parametres
- [ ] Chapitre III - Procedures
- [ ] Chapitre IV - Utilitaires
- [ ] Chapitre V - Travaux pratiques

## Prerequis

- Module Z/OS et TSO/ISPF (recommande)
- Comprehension de l'environnement mainframe
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
│   │    JCL      │  ◄── VOUS ETES ICI                           │
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
│   │   CICS      │  Transactionnel temps reel                    │
│   │             │                                               │
│   └─────────────┘                                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```
