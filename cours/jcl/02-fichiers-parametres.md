# Chapitre II - Les fichiers speciaux et les parametres

## Introduction

Ce chapitre presente les differentes manieres de definir et manipuler les fichiers en JCL :

- **Concatenation** - Combiner plusieurs datasets en un seul flux logique
- **Fichiers partitionnes et sequentiels** - Comprendre les organisations PS et PO
- **Fichiers temporaires** - Creer des fichiers de travail ephemeres
- **Backward references** - Referencer des datasets definis precedemment

---

## II-1 Concatenation des Data Sets

### II-1-1 Principe de la concatenation

La **concatenation** permet de traiter plusieurs datasets comme un seul fichier logique. Les datasets sont lus sequentiellement dans l'ordre de declaration.

```
┌─────────────────────────────────────────────────────────────────┐
│                    CONCATENATION DE DATASETS                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //INPUT  DD DSN=FICHIER1,DISP=SHR    ──┐                     │
│   //       DD DSN=FICHIER2,DISP=SHR      ├── Lus comme UN seul │
│   //       DD DSN=FICHIER3,DISP=SHR    ──┘    fichier          │
│                                                                 │
│   Le programme recoit:                                          │
│   ┌──────────────┐                                              │
│   │  FICHIER1    │  ◄── Lu en premier                          │
│   ├──────────────┤                                              │
│   │  FICHIER2    │  ◄── Lu ensuite                             │
│   ├──────────────┤                                              │
│   │  FICHIER3    │  ◄── Lu en dernier                          │
│   └──────────────┘                                              │
│                                                                 │
│   Le passage d'un fichier a l'autre est TRANSPARENT            │
│   pour le programme                                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-1-2 Syntaxe de la concatenation

```jcl
//ddname   DD DSN=premier.dataset,DISP=SHR
//         DD DSN=deuxieme.dataset,DISP=SHR
//         DD DSN=troisieme.dataset,DISP=SHR
```

**Regles importantes :**

| Regle | Description |
|-------|-------------|
| DDname | Uniquement sur la premiere carte DD |
| Ordre | Les fichiers sont lus dans l'ordre de declaration |
| Limite | Maximum 255 datasets concatenes |
| DCB | Caracteristiques du premier fichier utilisees par defaut |
| DISP | Chaque dataset a sa propre disposition |

### II-1-3 Concatenation et DCB

```
┌─────────────────────────────────────────────────────────────────┐
│                    DCB ET CONCATENATION                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   REGLE : Le premier dataset determine les caracteristiques     │
│                                                                 │
│   //INPUT DD DSN=FILE1,DISP=SHR,DCB=(RECFM=FB,LRECL=80)        │
│   //      DD DSN=FILE2,DISP=SHR                                │
│   //      DD DSN=FILE3,DISP=SHR                                │
│                                                                 │
│   Les fichiers FILE2 et FILE3 doivent etre compatibles :       │
│   • Meme RECFM (ou compatible)                                 │
│   • LRECL <= LRECL du premier                                  │
│   • BLKSIZE <= BLKSIZE du premier                              │
│                                                                 │
│   ATTENTION : Si un fichier suivant a un LRECL plus grand,     │
│   les donnees seront TRONQUEES !                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Gestion des BLKSIZE differents :**

```jcl
//* Probleme : FILE2 a un BLKSIZE plus grand
//INPUT  DD DSN=FILE1,DISP=SHR             BLKSIZE=800
//       DD DSN=FILE2,DISP=SHR             BLKSIZE=8000  <- Erreur!

//* Solution 1 : Mettre le fichier avec le plus grand BLKSIZE en premier
//INPUT  DD DSN=FILE2,DISP=SHR             BLKSIZE=8000
//       DD DSN=FILE1,DISP=SHR             BLKSIZE=800   <- OK

//* Solution 2 : Specifier le BLKSIZE maximum sur la premiere DD
//INPUT  DD DSN=FILE1,DISP=SHR,DCB=BLKSIZE=8000
//       DD DSN=FILE2,DISP=SHR
```

### II-1-4 Cas d'utilisation de la concatenation

```
┌─────────────────────────────────────────────────────────────────┐
│              CAS D'UTILISATION CONCATENATION                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. BIBLIOTHEQUES DE PROGRAMMES (STEPLIB/JOBLIB)               │
│   ───────────────────────────────────────────────               │
│   //STEPLIB DD DSN=USER.LOADLIB,DISP=SHR                       │
│   //        DD DSN=PROD.LOADLIB,DISP=SHR                       │
│   //        DD DSN=SYS1.LINKLIB,DISP=SHR                       │
│   Recherche sequentielle du programme                          │
│                                                                 │
│   2. FICHIERS DE DONNEES MENSUELS                               │
│   ───────────────────────────────                               │
│   //VENTES  DD DSN=DATA.VENTES.JAN,DISP=SHR                    │
│   //        DD DSN=DATA.VENTES.FEV,DISP=SHR                    │
│   //        DD DSN=DATA.VENTES.MAR,DISP=SHR                    │
│   Traitement du trimestre                                       │
│                                                                 │
│   3. BIBLIOTHEQUES DE COPIES/INCLUDE                            │
│   ─────────────────────────────────                             │
│   //SYSLIB  DD DSN=USER.COPYLIB,DISP=SHR                       │
│   //        DD DSN=PROD.COPYLIB,DISP=SHR                       │
│   Recherche des COPY dans l'ordre                              │
│                                                                 │
│   4. FICHIERS DE LOG/HISTORIQUE                                 │
│   ─────────────────────────────                                 │
│   //HISTO   DD DSN=LOG.ANNEE.2023,DISP=SHR                     │
│   //        DD DSN=LOG.ANNEE.2024,DISP=SHR                     │
│   Analyse multi-annees                                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-1-5 Exemples pratiques de concatenation

**Exemple 1 : Concatenation simple pour lecture**

```jcl
//MYJOB    JOB ...
//*
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=FTEST.DATA.PART1,DISP=SHR
//         DD DSN=FTEST.DATA.PART2,DISP=SHR
//         DD DSN=FTEST.DATA.PART3,DISP=SHR
//OUTPUT   DD DSN=FTEST.DATA.MERGED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

**Exemple 2 : Concatenation avec IEBGENER**

```jcl
//* Copie de fichiers concatenes vers un seul fichier
//COPYJOB  JOB ...
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.FILE.A,DISP=SHR
//         DD DSN=FTEST.FILE.B,DISP=SHR
//         DD DSN=FTEST.FILE.C,DISP=SHR
//SYSUT2   DD DSN=FTEST.FILE.MERGED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(100,20),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

**Exemple 3 : Concatenation de bibliotheques**

```jcl
//COMPILE  JOB ...
//*
//COB      EXEC PGM=IGYCRCTL,PARM='LIST,MAP'
//STEPLIB  DD DSN=IGY.V6R4M0.SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=FTEST.COPYLIB,DISP=SHR
//         DD DSN=PROD.COPYLIB,DISP=SHR
//         DD DSN=SYS1.COPYLIB,DISP=SHR
//SYSIN    DD DSN=FTEST.SOURCE(MYPROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&OBJMOD,DISP=(NEW,PASS),
//            SPACE=(TRK,(10,5))
```

---

## II-2 Les fichiers partitionnes et sequentiels

### II-2-1 Organisation des datasets

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES D'ORGANISATION                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PS - PHYSICAL SEQUENTIAL (Sequentiel)                         │
│   ──────────────────────────────────────                        │
│   • Enregistrements stockes sequentiellement                   │
│   • Acces du debut a la fin                                    │
│   • Utilisations : fichiers de donnees, logs, rapports         │
│                                                                 │
│   ┌─────────────────────────────────────────┐                  │
│   │ Enr1 │ Enr2 │ Enr3 │ Enr4 │ ... │ EnrN │                  │
│   └─────────────────────────────────────────┘                  │
│                                                                 │
│   PO - PARTITIONED ORGANIZATION (PDS)                           │
│   ───────────────────────────────────                           │
│   • Ensemble de membres (fichiers) dans un repertoire          │
│   • Chaque membre est un fichier sequentiel                    │
│   • Utilisations : bibliotheques source, COPYLIB, LOADLIB      │
│                                                                 │
│   ┌──────────────────────────────────────┐                     │
│   │  REPERTOIRE          │    DONNEES    │                     │
│   │  ┌─────────────────┐ │ ┌───────────┐ │                     │
│   │  │ MEMBER1 -> addr │ │ │  MEMBER1  │ │                     │
│   │  │ MEMBER2 -> addr │ │ │  MEMBER2  │ │                     │
│   │  │ MEMBER3 -> addr │ │ │  MEMBER3  │ │                     │
│   │  └─────────────────┘ │ └───────────┘ │                     │
│   └──────────────────────────────────────┘                     │
│                                                                 │
│   PDSE - PDS EXTENDED                                           │
│   ──────────────────                                           │
│   • Version amelioree du PDS                                   │
│   • Pas besoin de compression                                  │
│   • Meilleure gestion de l'espace                              │
│   • DSNTYPE=LIBRARY                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-2-2 Creation d'un fichier sequentiel (PS)

```jcl
//* Creation d'un fichier sequentiel
//CREATE   EXEC PGM=IEFBR14
//NEWFILE  DD DSN=FTEST.DATA.SEQFILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PS),
//            UNIT=SYSDA
```

**Parametres pour fichiers sequentiels :**

| Parametre | Description | Exemple |
|-----------|-------------|---------|
| DSORG=PS | Organisation sequentielle | `DCB=(...,DSORG=PS)` |
| RECFM | Format enregistrement | FB, VB, F, V |
| LRECL | Longueur enregistrement | 80, 133, etc. |
| BLKSIZE | Taille de bloc | 0 = optimise |
| SPACE | Allocation primaire/secondaire | (TRK,(10,5)) |

### II-2-3 Creation d'un fichier partitionne (PDS)

```jcl
//* Creation d'un PDS (bibliotheque)
//CREATE   EXEC PGM=IEFBR14
//NEWPDS   DD DSN=FTEST.SOURCE.COBOL,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10,20),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    SPACE POUR PDS                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SPACE=(TRK,(primaire,secondaire,directory))                   │
│                   │          │          │                       │
│                   │          │          └── Blocs pour          │
│                   │          │              le repertoire       │
│                   │          │              (obligatoire PDS)   │
│                   │          │                                  │
│                   │          └── Extensions                     │
│                   │                                             │
│                   └── Espace initial                            │
│                                                                 │
│   Le nombre de blocs de repertoire determine le nombre         │
│   maximum de membres :                                          │
│   • 1 bloc = environ 5 membres                                 │
│   • 20 blocs = environ 100 membres                             │
│                                                                 │
│   Exemples:                                                     │
│   SPACE=(TRK,(50,10,10))   Petit PDS (~50 membres)             │
│   SPACE=(CYL,(5,1,50))     PDS moyen (~250 membres)            │
│   SPACE=(CYL,(20,5,200))   Grand PDS (~1000 membres)           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-2-4 Creation d'un PDSE (PDS Extended)

```jcl
//* Creation d'un PDSE
//CREATE   EXEC PGM=IEFBR14
//NEWPDSE  DD DSN=FTEST.SOURCE.COBOLV2,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            DSNTYPE=LIBRARY,
//            UNIT=SYSDA
```

**Differences PDS vs PDSE :**

| Caracteristique | PDS | PDSE |
|-----------------|-----|------|
| Directory | Taille fixe (SPACE) | Dynamique |
| Compression | Necessaire (IEBCOPY) | Automatique |
| Espace libere | Non recupere | Recupere automatiquement |
| Membres | ~16000 max | Illimite pratiquement |
| Partage | Limite | Ameliore |
| DSNTYPE | (defaut) | LIBRARY |

### II-2-5 Acces aux membres d'un PDS

```
┌─────────────────────────────────────────────────────────────────┐
│                    ACCES AUX MEMBRES PDS                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ACCES A UN MEMBRE SPECIFIQUE                                  │
│   ────────────────────────────                                  │
│   //INPUT DD DSN=FTEST.SOURCE(MYPROG),DISP=SHR                 │
│                         ────────                                │
│                         Nom du membre entre parentheses         │
│                                                                 │
│   ACCES A TOUTE LA BIBLIOTHEQUE                                 │
│   ─────────────────────────────                                 │
│   //SYSLIB DD DSN=FTEST.SOURCE,DISP=SHR                        │
│   Le programme peut alors parcourir tous les membres           │
│                                                                 │
│   CREATION D'UN NOUVEAU MEMBRE                                  │
│   ────────────────────────────                                  │
│   //OUTPUT DD DSN=FTEST.SOURCE(NEWPROG),                       │
│   //          DISP=(NEW,CATLG)                                  │
│   DISP=NEW pour un nouveau membre, meme si PDS existe          │
│                                                                 │
│   REMPLACEMENT D'UN MEMBRE                                      │
│   ────────────────────────                                      │
│   //OUTPUT DD DSN=FTEST.SOURCE(OLDPROG),                       │
│   //          DISP=SHR                                          │
│   DISP=SHR ou OLD pour remplacer un membre existant            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-2-6 Exemples pratiques

**Exemple 1 : Copie de membre PDS vers fichier sequentiel**

```jcl
//COPYMEM  JOB ...
//*
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.SOURCE(MYPROG),DISP=SHR
//SYSUT2   DD DSN=FTEST.BACKUP.MYPROG,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

**Exemple 2 : Copie fichier sequentiel vers membre PDS**

```jcl
//ADDMEM   JOB ...
//*
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.INPUT.DATA,DISP=SHR
//SYSUT2   DD DSN=FTEST.PDS.LIB(NEWMEM),DISP=SHR
```

**Exemple 3 : Liste des membres d'un PDS avec IEHLIST**

```jcl
//LISTPDS  JOB ...
//*
//STEP1    EXEC PGM=IEHLIST
//SYSPRINT DD SYSOUT=*
//DD1      DD DSN=FTEST.SOURCE,DISP=SHR
//SYSIN    DD *
  LISTPDS DSNAME=FTEST.SOURCE,VOL=3390=volser
/*
```

---

## II-3 Les fichiers Temporaires

### II-3-1 Concept des fichiers temporaires

```
┌─────────────────────────────────────────────────────────────────┐
│                    FICHIERS TEMPORAIRES                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Un fichier TEMPORAIRE :                                        │
│   • Existe uniquement pendant la duree du JOB                  │
│   • Est automatiquement supprime a la fin du JOB               │
│   • N'est PAS catalogue                                        │
│   • Peut etre passe entre steps avec DISP=(,PASS)              │
│                                                                 │
│   Utilisations courantes :                                      │
│   • Fichiers intermediaires entre steps                        │
│   • Fichiers de tri temporaires                                │
│   • Modules objets avant link-edit                             │
│   • Donnees de travail                                         │
│                                                                 │
│   AVANTAGES :                                                   │
│   ✓ Pas de gestion manuelle de suppression                     │
│   ✓ Pas de conflit de nom entre jobs concurrents               │
│   ✓ Nom genere automatiquement                                 │
│   ✓ Espace libere automatiquement                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-3-2 Syntaxes des fichiers temporaires

**Methode 1 : DSN=&&nom**

```jcl
//STEP1    EXEC PGM=PROG1
//TEMPFILE DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

**Methode 2 : Sans DSN (nom genere par systeme)**

```jcl
//STEP1    EXEC PGM=PROG1
//TEMPFILE DD DISP=(NEW,PASS),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

**Methode 3 : UNIT=VIO (Virtual I/O - tres rapide)**

```jcl
//STEP1    EXEC PGM=PROG1
//TEMPFILE DD DSN=&&VIOFILE,
//            DISP=(NEW,PASS),
//            UNIT=VIO,
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

### II-3-3 Cycle de vie d'un fichier temporaire

```
┌─────────────────────────────────────────────────────────────────┐
│                CYCLE DE VIE FICHIER TEMPORAIRE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //STEP1  EXEC PGM=PROG1                                       │
│   //OUT    DD DSN=&&TEMP,DISP=(NEW,PASS),SPACE=...             │
│                              │                                  │
│                              └── Creation + Passage             │
│                                                                 │
│   //STEP2  EXEC PGM=PROG2                                       │
│   //IN     DD DSN=&&TEMP,DISP=(OLD,PASS)                       │
│                              │                                  │
│                              └── Lecture + Passage              │
│                                                                 │
│   //STEP3  EXEC PGM=PROG3                                       │
│   //IN     DD DSN=&&TEMP,DISP=(OLD,DELETE)                     │
│                              │                                  │
│                              └── Lecture + Suppression          │
│                                                                 │
│   ═══════════════════════════════════════════════════════════  │
│                                                                 │
│   DISP pour fichiers temporaires :                              │
│   ────────────────────────────────                              │
│   (NEW,PASS)    Creer et passer au step suivant                │
│   (OLD,PASS)    Utiliser et passer au step suivant             │
│   (OLD,DELETE)  Utiliser et supprimer                          │
│   (MOD,PASS)    Ajouter et passer                              │
│                                                                 │
│   Note : A la fin du JOB, tous les fichiers temporaires        │
│   sont automatiquement supprimes, meme si PASS                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-3-4 Exemples pratiques

**Exemple 1 : Compilation et link-edit**

```jcl
//COMPILE  JOB ...
//*
//* STEP 1 : Compilation - cree un module objet temporaire
//COB      EXEC PGM=IGYCRCTL
//SYSLIN   DD DSN=&&OBJMOD,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSIN    DD DSN=FTEST.SOURCE(MYPROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//* STEP 2 : Link-edit - utilise le module objet temporaire
//LKED     EXEC PGM=IEWL,COND=(8,LT,COB)
//SYSLIN   DD DSN=&&OBJMOD,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=FTEST.LOADLIB(MYPROG),DISP=SHR
//SYSPRINT DD SYSOUT=*
```

**Exemple 2 : Tri avec fichier intermediaire**

```jcl
//SORTJOB  JOB ...
//*
//* STEP 1 : Extraction des donnees
//EXTRACT  EXEC PGM=MYPROG
//INPUT    DD DSN=FTEST.MASTER.FILE,DISP=SHR
//OUTPUT   DD DSN=&&EXTRACT,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(5,2)),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//*
//* STEP 2 : Tri des donnees extraites
//SORT     EXEC PGM=SORT
//SORTIN   DD DSN=&&EXTRACT,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=FTEST.SORTED.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//SYSOUT   DD SYSOUT=*
```

**Exemple 3 : Multiple fichiers temporaires**

```jcl
//MULTITMP JOB ...
//*
//STEP1    EXEC PGM=PROG1
//OUT1     DD DSN=&&TEMP1,DISP=(NEW,PASS),SPACE=(TRK,(5,1))
//OUT2     DD DSN=&&TEMP2,DISP=(NEW,PASS),SPACE=(TRK,(5,1))
//*
//STEP2    EXEC PGM=PROG2
//IN1      DD DSN=&&TEMP1,DISP=(OLD,PASS)
//IN2      DD DSN=&&TEMP2,DISP=(OLD,PASS)
//OUT      DD DSN=&&MERGED,DISP=(NEW,PASS),SPACE=(TRK,(10,2))
//*
//STEP3    EXEC PGM=PROG3
//INPUT    DD DSN=&&MERGED,DISP=(OLD,DELETE)
//OUTPUT   DD DSN=FTEST.FINAL.DATA,DISP=(NEW,CATLG),...
```

### II-3-5 VIO (Virtual I/O)

```
┌─────────────────────────────────────────────────────────────────┐
│                    UNIT=VIO                                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   VIO = Virtual I/O - Stockage en memoire paginee              │
│                                                                 │
│   AVANTAGES :                                                   │
│   • Tres rapide (pas d'I/O disque)                             │
│   • Ideal pour petits fichiers temporaires                     │
│   • Reduit la charge I/O                                       │
│                                                                 │
│   LIMITATIONS :                                                 │
│   • Limite a quelques cylindres (depend config systeme)        │
│   • Ne peut pas etre passe entre jobs                          │
│   • Pas pour les gros fichiers                                 │
│                                                                 │
│   Exemple:                                                      │
│   //VIOFILE DD DSN=&&VIO,                                       │
│   //           DISP=(NEW,PASS),                                 │
│   //           UNIT=VIO,                                        │
│   //           SPACE=(TRK,(5,1)),                               │
│   //           DCB=(RECFM=FB,LRECL=80)                          │
│                                                                 │
│   Utilisation recommandee:                                      │
│   • Fichiers de controle                                       │
│   • Petits fichiers de travail                                 │
│   • Messages temporaires                                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-4 Backward References (References arrieres)

### II-4-1 Concept des references arrieres

```
┌─────────────────────────────────────────────────────────────────┐
│                    BACKWARD REFERENCES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Une REFERENCE ARRIERE permet de :                             │
│   • Acceder a un dataset defini dans un step precedent         │
│   • Copier les caracteristiques (DCB) d'un autre DD            │
│   • Utiliser le meme volume qu'un autre dataset                │
│                                                                 │
│   SYNTAXE GENERALE :                                            │
│   ─────────────────                                            │
│   *.stepname.ddname                Dans le meme job            │
│   *.procstep.stepname.ddname       Dans une procedure          │
│                                                                 │
│   TYPES DE REFERENCES :                                         │
│   ─────────────────────                                        │
│   DSN=*.STEP1.DDNAME      Reference au nom du dataset          │
│   VOL=REF=*.STEP1.DDNAME  Reference au volume                  │
│   DCB=*.STEP1.DDNAME      Reference aux caracteristiques       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-4-2 Reference au DSN

```jcl
//* Reference au nom du dataset
//STEP1    EXEC PGM=PROG1
//OUTPUT   DD DSN=&&TEMPFILE,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//*
//STEP2    EXEC PGM=PROG2
//INPUT    DD DSN=*.STEP1.OUTPUT,
//            DISP=(OLD,DELETE)
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    DSN=*.stepname.ddname                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Signification : Utiliser le meme DSN que celui defini        │
│                   dans la DD 'ddname' du step 'stepname'       │
│                                                                 │
│   Exemple:                                                      │
│   //STEP1  ...                                                  │
│   //OUT    DD DSN=FTEST.DATA,DISP=(NEW,PASS)                   │
│   //STEP2  ...                                                  │
│   //IN     DD DSN=*.STEP1.OUT,DISP=(OLD,DELETE)                │
│            ─────────────────                                    │
│            Equivaut a DSN=FTEST.DATA                           │
│                                                                 │
│   AVANTAGES :                                                   │
│   • Evite de repeter le nom du dataset                         │
│   • Maintient la coherence entre steps                         │
│   • Obligatoire pour fichiers temporaires (&&)                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-4-3 Reference au DCB

```jcl
//* Reference aux caracteristiques DCB
//STEP1    EXEC PGM=PROG1
//INPUT    DD DSN=FTEST.MASTER.FILE,DISP=SHR
//OUTPUT   DD DSN=FTEST.OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(100,20),RLSE),
//            DCB=*.INPUT
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    DCB=*.ddname                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Signification : Copier les caracteristiques DCB de           │
│                   l'autre DD dans ce meme step                 │
│                                                                 │
│   //INPUT  DD DSN=FTEST.IN,DISP=SHR                            │
│   //       (DCB lu depuis le fichier: RECFM=FB,LRECL=100)      │
│   //OUTPUT DD DSN=FTEST.OUT,                                    │
│   //          DISP=(NEW,CATLG),                                 │
│   //          DCB=*.INPUT                                       │
│            ───────────                                          │
│            Copie RECFM=FB,LRECL=100                            │
│                                                                 │
│   Reference depuis un autre step:                               │
│   //STEP2  ...                                                  │
│   //OUT2   DD DSN=...,DCB=*.STEP1.INPUT                        │
│                                                                 │
│   UTILISATION :                                                 │
│   • Creer un fichier avec memes caracteristiques              │
│   • Assurer la compatibilite entre fichiers                    │
│   • Eviter de re-specifier tous les parametres                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-4-4 Reference au volume

```jcl
//* Reference au volume
//STEP1    EXEC PGM=PROG1
//FILE1    DD DSN=FTEST.DATA1,DISP=SHR
//FILE2    DD DSN=FTEST.DATA2,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.FILE1,
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    VOL=REF=*.ddname                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Signification : Placer le nouveau dataset sur le meme        │
│                   volume que le dataset reference              │
│                                                                 │
│   //FILE1 DD DSN=FTEST.DATA1,DISP=SHR   (sur volume PROD01)    │
│   //FILE2 DD DSN=FTEST.DATA2,                                   │
│   //         VOL=REF=*.FILE1            (ira sur PROD01)       │
│                                                                 │
│   UTILISATION :                                                 │
│   • Regrouper des fichiers lies sur le meme volume            │
│   • Optimiser les acces I/O                                    │
│   • Faciliter la gestion des volumes                           │
│                                                                 │
│   Variantes:                                                    │
│   VOL=REF=dsname             Reference a un dataset catalogue  │
│   VOL=REF=*.stepname.ddname  Reference dans le meme job        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-4-5 References dans les procedures

```
┌─────────────────────────────────────────────────────────────────┐
│                REFERENCES DANS LES PROCEDURES                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Dans une procedure, la syntaxe est:                           │
│   *.procstep.stepname.ddname                                    │
│                                                                 │
│   Exemple:                                                      │
│   //MYJOB  JOB ...                                              │
│   //CALL1  EXEC MYPROC      ◄── procstep = CALL1               │
│                                                                 │
│   Contenu de MYPROC:                                            │
│   //MYPROC PROC                                                 │
│   //STEP1  EXEC PGM=PROG1   ◄── stepname = STEP1               │
│   //OUT    DD DSN=&&TEMP,DISP=(NEW,PASS)                       │
│   //STEP2  EXEC PGM=PROG2                                       │
│   //IN     DD DSN=*.STEP1.OUT,DISP=(OLD,DELETE)                │
│            ──────────────── Reference interne a la proc        │
│                                                                 │
│   Depuis le JCL appelant:                                       │
│   //STEPX  EXEC PGM=PROGX                                       │
│   //INPUT  DD DSN=*.CALL1.STEP1.OUT,...                        │
│            ────────────────────────                             │
│            procstep.stepname.ddname                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### II-4-6 Exemples complets

**Exemple 1 : Chaine de traitement avec references**

```jcl
//CHAINJOB JOB ...
//*
//* STEP 1 : Extraction
//EXTRACT  EXEC PGM=EXTRACT
//INPUT    DD DSN=FTEST.MASTER,DISP=SHR
//OUTPUT   DD DSN=&&EXTRACT,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(10,5)),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=0)
//*
//* STEP 2 : Transformation (reference DSN et DCB)
//TRANSF   EXEC PGM=TRANSFRM
//INPUT    DD DSN=*.EXTRACT.OUTPUT,
//            DISP=(OLD,PASS)
//OUTPUT   DD DSN=&&TRANSF,
//            DISP=(NEW,PASS),
//            SPACE=(CYL,(10,5)),
//            DCB=*.INPUT
//*
//* STEP 3 : Chargement final
//LOAD     EXEC PGM=LOADPGM
//INPUT    DD DSN=*.TRANSF.OUTPUT,
//            DISP=(OLD,DELETE)
//OUTPUT   DD DSN=FTEST.TARGET,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(20,10),RLSE),
//            DCB=*.INPUT
```

**Exemple 2 : Copie avec memes caracteristiques**

```jcl
//COPYJOB  JOB ...
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.SOURCE.DATA,DISP=SHR
//SYSUT2   DD DSN=FTEST.BACKUP.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.SYSUT1,
//            SPACE=(TRK,(100,20),RLSE),
//            DCB=*.SYSUT1
```

**Exemple 3 : Creation de fichiers associes sur meme volume**

```jcl
//CREATEJB JOB ...
//*
//CREATE   EXEC PGM=IEFBR14
//MASTER   DD DSN=FTEST.VSAM.MASTER,DISP=SHR
//INDEX    DD DSN=FTEST.VSAM.INDEX,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.MASTER,
//            SPACE=(CYL,(1,1)),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0)
//BACKUP   DD DSN=FTEST.VSAM.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            VOL=REF=*.MASTER,
//            SPACE=(CYL,(5,2)),
//            DCB=*.INDEX
```

---

## Synthese

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLES DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CONCATENATION                                                 │
│   ─────────────                                                │
│   • Combine plusieurs datasets en un flux logique              │
│   • DDname uniquement sur premiere DD                          │
│   • DCB du premier fichier utilise par defaut                  │
│   • Maximum 255 datasets                                       │
│                                                                 │
│   FICHIERS PS ET PO                                             │
│   ─────────────────                                            │
│   • PS = Sequentiel (DSORG=PS)                                 │
│   • PO = Partitionne/PDS (DSORG=PO)                            │
│   • PDSE = PDS Extended (DSNTYPE=LIBRARY)                      │
│   • Acces membre : DSN=pds.name(MEMBER)                        │
│                                                                 │
│   FICHIERS TEMPORAIRES                                          │
│   ────────────────────                                         │
│   • DSN=&&nom ou sans DSN                                      │
│   • Supprimes automatiquement en fin de job                    │
│   • DISP=(NEW,PASS) pour creer et passer                       │
│   • UNIT=VIO pour stockage en memoire                          │
│                                                                 │
│   BACKWARD REFERENCES                                           │
│   ───────────────────                                          │
│   • DSN=*.step.dd    Reference au nom                          │
│   • DCB=*.step.dd    Reference aux caracteristiques            │
│   • VOL=REF=*.step.dd Reference au volume                      │
│   • *.procstep.step.dd pour procedures                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-memoire

```
┌─────────────────────────────────────────────────────────────────┐
│               AIDE-MEMOIRE FICHIERS SPECIAUX                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CONCATENATION                                                 │
│   ─────────────────────────────────────────────────────────    │
│   //INPUT DD DSN=FILE1,DISP=SHR                                │
│   //      DD DSN=FILE2,DISP=SHR     (pas de DDname)            │
│   //      DD DSN=FILE3,DISP=SHR                                │
│                                                                 │
│   CREATION FICHIERS                                             │
│   ─────────────────────────────────────────────────────────    │
│   Sequentiel: SPACE=(TRK,(10,5))                               │
│   PDS:        SPACE=(TRK,(50,10,20))  ◄── directory blocks     │
│   PDSE:       SPACE=(TRK,(50,10)),DSNTYPE=LIBRARY              │
│                                                                 │
│   FICHIERS TEMPORAIRES                                          │
│   ─────────────────────────────────────────────────────────    │
│   DSN=&&TEMP,DISP=(NEW,PASS)     Creation                      │
│   DSN=&&TEMP,DISP=(OLD,PASS)     Utilisation                   │
│   DSN=&&TEMP,DISP=(OLD,DELETE)   Derniere utilisation          │
│   UNIT=VIO                        Memoire (rapide)             │
│                                                                 │
│   BACKWARD REFERENCES                                           │
│   ─────────────────────────────────────────────────────────    │
│   DSN=*.STEP1.DDNAME             Nom du dataset                │
│   DCB=*.STEP1.DDNAME             Caracteristiques              │
│   VOL=REF=*.STEP1.DDNAME         Volume                        │
│   *.PROCSTEP.STEP.DD             Dans procedure                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Chapitre I - Cartes JOB, EXEC, DD](01-cartes-job-exec-dd.md) | [Chapitre III - Les procedures](03-procedures.md) |
