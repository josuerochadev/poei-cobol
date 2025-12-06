# Exercices JCL - Chapitre III
## Les procedures

Ces exercices permettent de pratiquer les procedures in-stream, cataloguees, imbriquees et le parametrage symbolique.

---

## Exercice 1 : Procedure In-Stream pour chargement ESDS

### Objectif
Definir une procedure in-stream qui charge un Data Set sequentiel (ESDS) avec des donnees definies dans le JOB appelant.

### Instructions
1. Creer une procedure in-stream nommee `LOADPROC`
2. La procedure doit utiliser IEBGENER pour copier des donnees
3. Utiliser un parametre symbolique `&OUTDSN` pour le nom du fichier de sortie
4. Les donnees seront fournies via un override du DD SYSUT1 dans le JCL appelant
5. Appeler la procedure avec le dataset `FTEST.ESDS.AAAA`

### Solution

```jcl
//FTESTEX1 JOB (ACCT),'EXERCICE 1 - PROC INSTREAM',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 1 : PROCEDURE IN-STREAM POUR CHARGEMENT ESDS
//* ============================================================
//*
//* ----------------------------------------------------------
//* DEFINITION DE LA PROCEDURE IN-STREAM
//* ----------------------------------------------------------
//LOADPROC PROC OUTDSN=
//*
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL DE LA PROCEDURE AVEC DONNEES IN-STREAM
//* ----------------------------------------------------------
//CALL1    EXEC LOADPROC,OUTDSN=FTEST.ESDS.AAAA
//*
//* Override du DD SYSUT1 pour fournir les donnees
//*
//LOAD.SYSUT1 DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01A  JOB (ACCT),'EX1 PROC INSTREAM',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* DEFINITION DE LA PROCEDURE IN-STREAM
//LOADPROC PROC OUTDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            VOL=SER=PUB001,
//            UNIT=3390
//         PEND
//*
//* APPEL DE LA PROCEDURE
//CALL1    EXEC LOADPROC,OUTDSN=HERC01.ESDS.AAAA
//LOAD.SYSUT1 DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//
```

### Points cles
- `//LOADPROC PROC OUTDSN=` : declaration avec parametre sans defaut
- `//         PEND` : fin obligatoire pour procedure in-stream
- `//LOAD.SYSUT1 DD *` : override du DD pour fournir les donnees (stepname.ddname)
- Les donnees in-stream ne peuvent pas etre dans la procedure elle-meme

### Resultat attendu
- Le job doit se terminer avec RC=0
- Le dataset `FTEST.ESDS.AAAA` doit contenir les 3 enregistrements
- Le listing JES doit montrer l'expansion de la procedure (lignes ++)

---

## Exercice 2 : Deux procedures cataloguees

### Objectif
Creer deux procedures cataloguees et les appeler successivement dans un JCL.

### Procedures a creer

**Procedure 1 : LOADVSAM** - Charge un fichier ESDS
- Parametre : `&VSAMDSN` (nom du fichier VSAM/ESDS)
- Utilise IDCAMS pour definir et charger

**Procedure 2 : COPYSEQ** - Copie un fichier sequentiel
- Parametres : `&INDSN` (source), `&OUTDSN` (cible)
- Utilise IEBGENER

### Note pour environnement de formation
En l'absence de PROCLIB, nous simulons avec des procedures in-stream. En production, ces procedures seraient stockees dans une bibliotheque PROCLIB.

### Solution (simulation avec in-stream)

```jcl
//FTESTEX2 JOB (ACCT),'EXERCICE 2 - DEUX PROCS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 2 : DEUX PROCEDURES APPELEES SUCCESSIVEMENT
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURE 1 : LOADESDS - Chargement fichier sequentiel
//* En production, serait dans USER.PROCLIB(LOADESDS)
//* ----------------------------------------------------------
//LOADESDS PROC ESDSDSN=
//*
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* PROCEDURE 2 : COPYSEQ - Copie fichier sequentiel
//* En production, serait dans USER.PROCLIB(COPYSEQ)
//* ----------------------------------------------------------
//COPYSEQ  PROC INDSN=,OUTDSN=
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL PROCEDURE 1 : Charger FTEST.ESDS.AAAA
//* ----------------------------------------------------------
//STEP010  EXEC LOADESDS,ESDSDSN=FTEST.ESDS.AAAA
//LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//*
//* ----------------------------------------------------------
//* APPEL PROCEDURE 2 : Copier vers FTEST.TRI.SEQ
//* ----------------------------------------------------------
//STEP020  EXEC COPYSEQ,
//              INDSN=FTEST.ESDS.AAAA,
//              OUTDSN=FTEST.TRI.SEQ
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01B  JOB (ACCT),'EX2 DEUX PROCS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* PROCEDURE 1 : LOADESDS
//LOADESDS PROC ESDSDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//* PROCEDURE 2 : COPYSEQ
//COPYSEQ  PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//* APPEL PROCEDURE 1
//STEP010  EXEC LOADESDS,ESDSDSN=HERC01.ESDS.AAAA
//LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//*
//* APPEL PROCEDURE 2
//STEP020  EXEC COPYSEQ,
//              INDSN=HERC01.ESDS.AAAA,
//              OUTDSN=HERC01.TRI.SEQ
//
```

### Verification
- STEP010 doit creer `FTEST.ESDS.AAAA` avec 3 enregistrements
- STEP020 doit creer `FTEST.TRI.SEQ` avec le meme contenu
- Les deux procedures sont expandues dans le listing

---

## Exercice 3 : Procedures imbriquees

### Objectif
Modifier l'exercice 2 pour que les deux procedures soient imbriquees : une procedure principale appelle les deux autres.

### Instructions
1. Creer une procedure `FULLPROC` qui appelle `LOADESDS` puis `COPYSEQ`
2. Utiliser le parametrage pour passer les noms de fichiers
3. Le JCL appelant ne fait qu'un seul EXEC

### Solution

```jcl
//FTESTEX3 JOB (ACCT),'EXERCICE 3 - PROC IMBRIQUEES',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 3 : PROCEDURES IMBRIQUEES
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 2 : LOADESDS
//* ----------------------------------------------------------
//LOADESDS PROC ESDSDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 2 : COPYSEQ
//* ----------------------------------------------------------
//COPYSEQ  PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 1 : FULLPROC (appelle les deux autres)
//* ----------------------------------------------------------
//FULLPROC PROC SRCDSN=,TGTDSN=
//*
//* Etape 1 : Charger le fichier source
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//*
//* Etape 2 : Copier vers fichier cible
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL UNIQUE DE LA PROCEDURE PRINCIPALE
//* ----------------------------------------------------------
//PROCESS  EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.AAAA,
//              TGTDSN=FTEST.TRI.SEQ
//*
//* Override pour fournir les donnees au step LOAD imbrique
//* Notation : procstep.step.ddname
//*
//LOADSTEP.LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01C  JOB (ACCT),'EX3 IMBRIQUEES',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* PROCEDURE NIVEAU 2 : LOADESDS
//LOADESDS PROC ESDSDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//* PROCEDURE NIVEAU 2 : COPYSEQ
//COPYSEQ  PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//* PROCEDURE NIVEAU 1 : FULLPROC
//FULLPROC PROC SRCDSN=,TGTDSN=
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* APPEL UNIQUE
//PROCESS  EXEC FULLPROC,
//              SRCDSN=HERC01.ESDS.AAAA,
//              TGTDSN=HERC01.TRI.SEQ
//LOADSTEP.LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//
```

### Points cles
- `//FULLPROC PROC` appelle `LOADESDS` et `COPYSEQ`
- Les parametres `&SRCDSN` et `&TGTDSN` sont propages aux sous-procedures
- Override imbrique : `//LOADSTEP.LOAD.SYSUT1` = procstep.step.ddname
- Un seul EXEC dans le JCL appelant execute tout le traitement

---

## Exercice 4 : Parametrage symbolique multiple

### Objectif
Utiliser le parametrage symbolique pour executer les memes procedures sur differents jeux de donnees :
- `FTEST.ESDS.BBBB`
- `FTEST.ESDS.CCCC`

### Instructions
1. Reprendre la procedure `FULLPROC` de l'exercice 3
2. L'appeler deux fois avec des parametres differents
3. Chaque appel cree et copie un fichier different

### Solution

```jcl
//FTESTEX4 JOB (ACCT),'EXERCICE 4 - PARAM SYMBOLIQUES',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 4 : PARAMETRAGE SYMBOLIQUE MULTIPLE
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURES (memes que exercice 3)
//* ----------------------------------------------------------
//LOADESDS PROC ESDSDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//COPYSEQ  PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//         PEND
//*
//FULLPROC PROC SRCDSN=,TGTDSN=
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* ----------------------------------------------------------
//* PREMIER APPEL : FTEST.ESDS.BBBB -> FTEST.TRI.BBBB
//* ----------------------------------------------------------
//PROCBB   EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.BBBB,
//              TGTDSN=FTEST.TRI.BBBB
//LOADSTEP.LOAD.SYSUT1 DD *
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-01
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-02
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-03
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-04
/*
//*
//* ----------------------------------------------------------
//* DEUXIEME APPEL : FTEST.ESDS.CCCC -> FTEST.TRI.CCCC
//* ----------------------------------------------------------
//PROCCC   EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.CCCC,
//              TGTDSN=FTEST.TRI.CCCC
//LOADSTEP.LOAD.SYSUT1 DD *
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-01
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-02
/*
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01D  JOB (ACCT),'EX4 MULTI PARAM',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* PROCEDURES
//LOADESDS PROC ESDSDSN=
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//COPYSEQ  PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,UNIT=3390
//         PEND
//*
//FULLPROC PROC SRCDSN=,TGTDSN=
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* PREMIER APPEL : BBBB
//PROCBB   EXEC FULLPROC,
//              SRCDSN=HERC01.ESDS.BBBB,
//              TGTDSN=HERC01.TRI.BBBB
//LOADSTEP.LOAD.SYSUT1 DD *
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-01
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-02
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-03
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-04
/*
//*
//* DEUXIEME APPEL : CCCC
//PROCCC   EXEC FULLPROC,
//              SRCDSN=HERC01.ESDS.CCCC,
//              TGTDSN=HERC01.TRI.CCCC
//LOADSTEP.LOAD.SYSUT1 DD *
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-01
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-02
/*
//
```

### Points cles
- La meme procedure `FULLPROC` est appelee deux fois
- Les parametres `SRCDSN` et `TGTDSN` different a chaque appel
- Chaque appel cree ses propres fichiers
- Le code est reutilisable et facile a maintenir

### Verification
Apres execution, les fichiers suivants doivent exister :
- `FTEST.ESDS.BBBB` (4 enregistrements)
- `FTEST.TRI.BBBB` (4 enregistrements, copie)
- `FTEST.ESDS.CCCC` (2 enregistrements)
- `FTEST.TRI.CCCC` (2 enregistrements, copie)

---

## Resume des concepts pratiques

```
┌─────────────────────────────────────────────────────────────────┐
│                    RESUME DES EXERCICES                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   EXERCICE 1 - Procedure In-Stream                              │
│   • PROC...PEND pour definir dans le JCL                       │
│   • Override stepname.ddname pour donnees in-stream            │
│   • Parametres symboliques &param                              │
│                                                                 │
│   EXERCICE 2 - Deux procedures successives                      │
│   • Appels EXEC independants                                   │
│   • Chaque procedure a ses propres parametres                  │
│   • Ordre d'execution sequentiel                               │
│                                                                 │
│   EXERCICE 3 - Procedures imbriquees                            │
│   • Procedure qui appelle d'autres procedures                  │
│   • Propagation des parametres                                 │
│   • Override imbrique : procstep.step.ddname                   │
│                                                                 │
│   EXERCICE 4 - Parametrage multiple                             │
│   • Meme procedure, differents parametres                      │
│   • Reutilisabilite du code                                    │
│   • Traitement de plusieurs jeux de donnees                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Nettoyage apres exercices

```jcl
//CLEANUP  JOB (ACCT),'NETTOYAGE',CLASS=A,MSGCLASS=A
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.ESDS.AAAA
  DELETE FTEST.TRI.SEQ
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.TRI.BBBB
  DELETE FTEST.ESDS.CCCC
  DELETE FTEST.TRI.CCCC
  SET MAXCC=0
/*
//
```

Pour Hercules/TK4-, remplacer `FTEST` par `HERC01`.
