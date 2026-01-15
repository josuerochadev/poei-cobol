# Partie 0 : Preparation de l'environnement

[< Introduction](00-introduction.md) | [Partie 1 : Affichage >](02-partie-1-affichage.md)

---

## Exercice 0 : Creation des Libraries

### Enonce

Ce travail necessite la creation de trois Library pour stocker les membres a creer au cours de sa realisation. Les Library a definir doivent porter le nom sous la forme suivante :
- **ROCHA.CICS.SOURCE** : Programmes COBOL et JCL
- **ROCHA.CICS.LINK** : Programmes objets (apres compilation)
- **ROCHA.CICS.LOAD** : Programmes executables (apres link-edit)

### Mon travail

Avant de commencer le developpement des programmes CICS, j'ai cree les trois libraries necessaires via ISPF option 3.2 (Data Set Utility). Ces libraries sont des PDS (Partitioned Data Sets) qui contiendront tous les membres du projet.

**Choix des caracteristiques :**
- **Organisation** : PO (Partitioned Organization) pour stocker plusieurs membres
- **Format d'enregistrement** : FB (Fixed Block) avec LRECL=80 pour les sources
- **Taille** : 10 tracks primaires, 5 secondaires (suffisant pour le projet)
- **Directory blocks** : 10 blocs pour l'index des membres

### Resolution

**Methode 1 : Via ISPF 3.2 (Data Set Utility)**

```
Option ===> 3.2

DATA SET UTILITY

A - Allocate new data set

Data Set Name: ROCHA.CICS.SOURCE

Allocation Parameters:
  Management class  . .
  Storage class . . . .
  Volume serial . . . .
  Device type . . . . .
  Data class  . . . . .
  Space units . . . . . TRACK
  Primary quantity  . . 10
  Secondary quantity  . 5
  Directory blocks  . . 10
  Record format . . . . FB
  Record length . . . . 80
  Block size  . . . . . 27920
  Data set name type  . PDS
```

Repeter l'operation pour `ROCHA.CICS.LINK` et `ROCHA.CICS.LOAD`.

**Methode 2 : Via JCL (IEFBR14)**

```jcl
//CREATLIB JOB (ACCT),'CREATE LIBRARIES',CLASS=A,MSGCLASS=X
//*****************************************************************
//* CREATION DES LIBRARIES POUR LE PROJET CICS
//*****************************************************************
//*
//STEP1    EXEC PGM=IEFBR14
//*
//* LIBRARY SOURCE (PROGRAMMES COBOL, BMS, JCL)
//SOURCE   DD DSN=ROCHA.CICS.SOURCE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920),
//            UNIT=SYSDA
//*
//* LIBRARY LINK (PROGRAMMES OBJETS)
//LINK     DD DSN=ROCHA.CICS.LINK,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920),
//            UNIT=SYSDA
//*
//* LIBRARY LOAD (PROGRAMMES EXECUTABLES)
//LOAD     DD DSN=ROCHA.CICS.LOAD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(20,10,10)),
//            DCB=(RECFM=U,BLKSIZE=27998),
//            UNIT=SYSDA
```

> **Note** : La library LOAD utilise RECFM=U (Undefined) car elle contient des modules executables (load modules) et non du texte source.

**Verification des libraries creees :**

```
Option ===> 3.4

DSLIST - Data Sets Matching ROCHA.CICS

Command - Enter "/" to select action
-------------------------------------------------------------------------------
         ROCHA.CICS.LINK
         ROCHA.CICS.LOAD
         ROCHA.CICS.SOURCE
```

### Structure des libraries

| Library | Contenu | RECFM | LRECL |
|---------|---------|-------|-------|
| ROCHA.CICS.SOURCE | Programmes COBOL (.cbl), MAPs BMS (.bms), JCL, Copybooks | FB | 80 |
| ROCHA.CICS.LINK | Modules objets apres compilation | FB | 80 |
| ROCHA.CICS.LOAD | Modules executables (load modules) | U | - |

### Membres crees dans SOURCE

Au cours du projet, les membres suivants ont ete crees dans `ROCHA.CICS.SOURCE` :

**Programmes COBOL :**

| Membre | Transaction | Description |
|--------|-------------|-------------|
| PRGCLIA | AFFI | Affichage client (READ) |
| PRGAJT | AJOU | Ajout client (WRITE) |
| PRGMAJ | MAJO | Mise a jour client (REWRITE) |
| PRGSUP | SUPP | Suppression client (DELETE) |
| PRGLGEN | LGEN | Liste generique (STARTBR/READNEXT) |
| PRGSTAT | STAT | Statistiques par region |

**MAPs BMS :**

| Membre | Programme | Description |
|--------|-----------|-------------|
| CLIAFF | PRGCLIA | Ecran affichage client |
| CLIAJT | PRGAJT | Ecran ajout client |
| CLIMAJ | PRGMAJ | Ecran mise a jour client |
| CLISUP | PRGSUP | Ecran suppression client |
| CLISTAT | PRGSTAT | Ecran statistiques |

**JCL :**

| Membre | Usage | Exercice |
|--------|-------|----------|
| DEFVSAM | Definition cluster VSAM | Ex 1 |
| LOADVSAM | Chargement donnees initiales | Ex 1 |
| ASMCLAF | Assemblage MAP CLIAFF | Ex 2 |
| CMPCLAF | Compilation PRGCLIA | Ex 3 |
| ASMAJT | Assemblage MAP CLIAJT | Ex 6 |
| CMPAJT | Compilation PRGAJT | Ex 7 |

> **Note sur les copybooks** : Les copybooks pour les MAPs BMS sont generes automatiquement lors de l'assemblage avec l'option `TYPE=DSECT`. Ils contiennent les structures de donnees avec les suffixes :
> - `I` : Zone input (donnees recues de l'ecran)
> - `O` : Zone output (donnees a envoyer)
> - `L` : Longueur du champ saisi
> - `A` : Attribut du champ (couleur, intensite, etc.)

---

[< Introduction](00-introduction.md) | [Partie 1 : Affichage >](02-partie-1-affichage.md)
