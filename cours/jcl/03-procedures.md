# Chapitre III - Les procedures

## Introduction

Ce chapitre est consacre a l'etude des **procedures JCL** et des **parametres symboliques**. Les procedures permettent de reutiliser du code JCL et de standardiser les traitements batch.

Sujets couverts :
- Les procedures cataloguees
- Les ordres PROC et PEND
- Les parametres symboliques
- Appel et modification d'une procedure
- Le listing d'execution
- Les procedures imbriquees

---

## III-1 Les procedures

### III-1-1 Definition

```
┌─────────────────────────────────────────────────────────────────┐
│                    QU'EST-CE QU'UNE PROCEDURE ?                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Une PROCEDURE est un ensemble d'instructions JCL              │
│   reutilisables stockees sous un nom.                          │
│                                                                 │
│   AVANTAGES :                                                   │
│   ────────────                                                 │
│   ✓ Reutilisation : ecrire une fois, utiliser partout         │
│   ✓ Maintenance : modification centralisee                     │
│   ✓ Standardisation : memes traitements pour tous             │
│   ✓ Fiabilite : code teste et valide                          │
│   ✓ Simplicite : JCL appelant plus court                      │
│   ✓ Parametrage : adaptation via symboles                     │
│                                                                 │
│   EXEMPLE D'UTILISATION :                                       │
│   ───────────────────────                                      │
│   Une procedure de compilation COBOL peut etre utilisee        │
│   par tous les developpeurs avec des parametres differents     │
│   (nom de programme, options de compilation, etc.)             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-1-2 Restrictions dans la definition d'une procedure

```
┌─────────────────────────────────────────────────────────────────┐
│                    RESTRICTIONS DES PROCEDURES                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   UNE PROCEDURE NE PEUT PAS CONTENIR :                          │
│   ─────────────────────────────────────                        │
│   ✗ Carte JOB                                                  │
│   ✗ Carte JOBLIB (mais STEPLIB est autorise)                   │
│   ✗ Carte JCLLIB                                               │
│   ✗ Carte JES2/JES3 (/*ROUTE, /*PRIORITY, etc.)               │
│   ✗ Delimiteur // (null statement) en fin                     │
│   ✗ Donnees in-stream (DD * ou DD DATA)                        │
│     (sauf si DDNAME est utilise)                               │
│                                                                 │
│   UNE PROCEDURE PEUT CONTENIR :                                 │
│   ─────────────────────────────                                │
│   ✓ Carte PROC (obligatoire pour in-stream)                    │
│   ✓ Cartes EXEC                                                │
│   ✓ Cartes DD                                                  │
│   ✓ Cartes IF/THEN/ELSE/ENDIF                                  │
│   ✓ Cartes SET                                                 │
│   ✓ Carte PEND (obligatoire pour in-stream)                    │
│   ✓ Commentaires (//*                                          │
│   ✓ Parametres symboliques (&param)                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-1-3 Syntaxe : Definition d'une procedure

#### Procedure In-Stream

```jcl
//procname PROC [param1=default1,param2=default2,...]
//*
//* Corps de la procedure
//*
//stepname EXEC PGM=program
//ddname   DD   ...
//*
//         PEND
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCEDURE IN-STREAM                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CARACTERISTIQUES :                                            │
│   ──────────────────                                           │
│   • Definie dans le JCL lui-meme                               │
│   • Placee AVANT le premier EXEC du JCL                        │
│   • Se termine par PEND                                        │
│   • Visible uniquement dans ce JCL                             │
│   • Utile pour des tests ou procedures temporaires             │
│                                                                 │
│   STRUCTURE :                                                   │
│   ───────────                                                  │
│   //MYJOB   JOB ...                                             │
│   //*                                                           │
│   //MYPROC  PROC PARAM1=,PARAM2=DEFAULT    ◄── Definition      │
│   //STEP1   EXEC PGM=...                                        │
│   //DD1     DD   DSN=&PARAM1,...                               │
│   //        PEND                            ◄── Fin procedure  │
│   //*                                                           │
│   //CALL1   EXEC MYPROC,PARAM1=VALUE        ◄── Appel          │
│   //                                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Procedure Cataloguee

```jcl
//* Contenu du membre MYPROC dans la PROCLIB
//MYPROC   PROC PARAM1=,PARAM2=DEFAULT
//*
//* Corps de la procedure
//*
//STEP1    EXEC PGM=program
//DD1      DD   DSN=&PARAM1,DISP=SHR
//DD2      DD   DSN=&PARAM2,DISP=SHR
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCEDURE CATALOGUEE                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CARACTERISTIQUES :                                            │
│   ──────────────────                                           │
│   • Stockee dans une bibliotheque PROCLIB                      │
│   • Un membre par procedure                                    │
│   • Nom du membre = nom de la procedure                        │
│   • Pas de PEND (fin implicite)                               │
│   • Accessible par tous les JCL                                │
│   • Standard de l'entreprise                                   │
│                                                                 │
│   BIBLIOTHEQUES PROCLIB :                                       │
│   ────────────────────────                                     │
│   • SYS1.PROCLIB        (systeme IBM)                          │
│   • USER.PROCLIB        (utilisateur)                          │
│   • PROD.PROCLIB        (production)                           │
│                                                                 │
│   APPEL :                                                       │
│   ───────                                                      │
│   //MYJOB   JOB ...                                             │
│   //        JCLLIB ORDER=(USER.PROCLIB)                        │
│   //STEP1   EXEC MYPROC,PARAM1=VALUE                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-1-4 Regles de codage des procedures

```
┌─────────────────────────────────────────────────────────────────┐
│                    REGLES DE CODAGE                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. NOM DE PROCEDURE                                           │
│      • 1 a 8 caracteres alphanumeriques                        │
│      • Commence par une lettre ou @, #, $                      │
│      • Convention : prefixe par application                    │
│        Ex: PAIECOMP (PAIE + COMPilation)                       │
│                                                                 │
│   2. CARTE PROC                                                 │
│      • Obligatoire pour procedure in-stream                    │
│      • Optionnelle pour procedure cataloguee                   │
│      • Declare les parametres symboliques                      │
│                                                                 │
│   3. PARAMETRES SYMBOLIQUES                                     │
│      • Nom : &param (1-7 caracteres apres &)                   │
│      • Valeur par defaut : param=valeur                        │
│      • Sans defaut : param= (vide)                             │
│                                                                 │
│   4. NOMS DE STEPS                                              │
│      • Uniques dans la procedure                               │
│      • Utilisables pour references et conditions               │
│                                                                 │
│   5. CARTE PEND                                                 │
│      • Obligatoire pour in-stream                              │
│      • Inutile pour cataloguee                                 │
│      • // PEND ou //PEND ou //labelPEND                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-1-5 Assignation des bibliotheques dans les JCL

```
┌─────────────────────────────────────────────────────────────────┐
│                    ASSIGNATION DES PROCLIB                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   METHODE 1 : JCLLIB (recommandee)                              │
│   ─────────────────────────────────                            │
│   //MYJOB  JOB ...                                              │
│   //       JCLLIB ORDER=(USER.PROCLIB,                         │
│   //                     PROD.PROCLIB,                          │
│   //                     SYS1.PROCLIB)                          │
│   //STEP1  EXEC MYPROC                                          │
│                                                                 │
│   • Place apres JOB, avant premier EXEC                        │
│   • Jusqu'a 15 bibliotheques                                   │
│   • Recherche dans l'ordre specifie                            │
│   • Utilisable aussi pour INCLUDE                              │
│                                                                 │
│   METHODE 2 : Parametres systeme                                │
│   ──────────────────────────────                               │
│   • Configuration dans JES2/JES3                               │
│   • Bibliotheques par defaut du systeme                        │
│   • SYS1.PROCLIB toujours en dernier recours                   │
│                                                                 │
│   ORDRE DE RECHERCHE :                                          │
│   ────────────────────                                         │
│   1. Bibliotheques JCLLIB (si specifie)                        │
│   2. Bibliotheques systeme par defaut                          │
│   3. SYS1.PROCLIB                                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-1-6 Types de procedures

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES DE PROCEDURES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. PROCEDURE IN-STREAM                                        │
│   ──────────────────────                                       │
│   Definie dans le JCL, entre PROC et PEND                      │
│                                                                 │
│   //MYJOB  JOB ...                                              │
│   //MYPROC PROC                                                 │
│   //STEP1  EXEC PGM=IEFBR14                                     │
│   //       PEND                                                 │
│   //RUN    EXEC MYPROC                                          │
│                                                                 │
│   2. PROCEDURE CATALOGUEE                                       │
│   ────────────────────────                                     │
│   Stockee dans une bibliotheque PROCLIB                        │
│                                                                 │
│   //MYJOB  JOB ...                                              │
│   //       JCLLIB ORDER=USER.PROCLIB                           │
│   //RUN    EXEC MYPROC                                          │
│                                                                 │
│   3. PROCEDURE IMBRIQUEE                                        │
│   ──────────────────────                                       │
│   Une procedure qui appelle une autre procedure                │
│   Maximum 15 niveaux d'imbrication                             │
│                                                                 │
│   PROC1 appelle PROC2 qui appelle PROC3...                     │
│                                                                 │
│   COMPARAISON :                                                 │
│   ─────────────                                                │
│   │ Critere       │ In-Stream │ Cataloguee │                   │
│   │───────────────│───────────│────────────│                   │
│   │ Reutilisable  │ Non       │ Oui        │                   │
│   │ Modifiable    │ Facile    │ Controle   │                   │
│   │ Tests         │ Ideal     │ Production │                   │
│   │ PEND          │ Requis    │ Non        │                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-2 Les parametres symboliques

### III-2-1 Definition et syntaxe

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRES SYMBOLIQUES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Un PARAMETRE SYMBOLIQUE est une variable qui sera            │
│   remplacee par une valeur a l'execution.                      │
│                                                                 │
│   SYNTAXE :                                                     │
│   ─────────                                                    │
│   &nom         Parametre simple                                │
│   &nom.suite   Parametre suivi de texte                        │
│   &&nom        Fichier temporaire (pas un parametre)           │
│                                                                 │
│   DECLARATION DANS PROC :                                       │
│   ────────────────────────                                     │
│   //MYPROC PROC DSN=,             Sans valeur par defaut       │
│   //            ENV=TEST,          Avec valeur par defaut      │
│   //            SPACE='(TRK,(5,1))'  Valeur avec virgules      │
│                                                                 │
│   UTILISATION DANS PROC :                                       │
│   ────────────────────────                                     │
│   //DD1 DD DSN=&DSN,DISP=SHR                                   │
│   //DD2 DD DSN=DATA.&ENV..FILE,DISP=SHR                        │
│            Le point separe le symbole du texte suivant         │
│                                                                 │
│   PASSAGE A L'APPEL :                                           │
│   ────────────────────                                         │
│   //STEP1 EXEC MYPROC,DSN=PROD.DATA,ENV=PROD                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-2-2 Regles des parametres symboliques

```
┌─────────────────────────────────────────────────────────────────┐
│                    REGLES DES SYMBOLES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. NOM DU PARAMETRE                                           │
│      • 1 a 7 caracteres (apres &)                              │
│      • Commence par lettre ou @, #, $                          │
│      • Alphanumerique                                          │
│                                                                 │
│   2. VALEURS PAR DEFAUT                                         │
│      • Specifiees dans la carte PROC                           │
│      • Apostrophes si caracteres speciaux                      │
│      • Vide = valeur obligatoire a l'appel                     │
│                                                                 │
│   3. CONCATENATION AVEC TEXTE                                   │
│      • Utiliser le point (.) comme separateur                  │
│      • DSN=&HLQ..DATA resulte en HLQ.DATA                      │
│      • Le premier point appartient au symbole                  │
│      • Le second point est le separateur DSN                   │
│                                                                 │
│   4. VALEURS AVEC CARACTERES SPECIAUX                           │
│      • Entourer de guillemets simples                          │
│      • SPACE='(CYL,(5,1))'                                     │
│      • Pour apostrophe dans valeur : ''                        │
│                                                                 │
│   5. SYMBOLES SYSTEME                                           │
│      • &SYSUID   Userid du soumetteur                          │
│      • &LYYMMDD  Date AAAAMMJJ                                 │
│      • &SYSTIME  Heure HH.MM.SS                                │
│      • &SYSJOBNAME Nom du job                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-2-3 Exemples de parametres symboliques

```jcl
//* DEFINITION DE PROCEDURE AVEC PARAMETRES
//COPYPROC PROC INDSN=,
//              OUTDSN=,
//              INDISP=SHR,
//              OUTDISP='(NEW,CATLG,DELETE)',
//              SPACE='(TRK,(10,5),RLSE)'
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DSN=&INDSN,
//              DISP=&INDISP
//SYSUT2   DD   DSN=&OUTDSN,
//              DISP=&OUTDISP,
//              SPACE=&SPACE,
//              DCB=*.SYSUT1
//         PEND
//*
//* APPEL AVEC TOUS LES PARAMETRES
//CALL1    EXEC COPYPROC,
//              INDSN=PROD.INPUT.DATA,
//              OUTDSN=PROD.OUTPUT.DATA
//*
//* APPEL AVEC SURCHARGE DE PARAMETRES PAR DEFAUT
//CALL2    EXEC COPYPROC,
//              INDSN=TEST.INPUT.DATA,
//              OUTDSN=TEST.OUTPUT.DATA,
//              SPACE='(CYL,(1,1))'
```

**Exemple avec symboles systeme :**

```jcl
//LOGPROC  PROC ENV=TEST
//*
//CREATE   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
EXECUTION DU JOB &SYSJOBNAME LE &LYYMMDD A &SYSTIME
/*
//SYSUT2   DD DSN=&SYSUID..LOG.&ENV..D&LYYMMDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1))
//         PEND
```

---

## III-3 Appel d'une procedure

### III-3-1 Syntaxe d'appel

```
┌─────────────────────────────────────────────────────────────────┐
│                    APPEL D'UNE PROCEDURE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SYNTAXE DE BASE :                                             │
│   ─────────────────                                            │
│   //stepname EXEC procname[,param1=val1,param2=val2,...]       │
│                                                                 │
│   ou                                                            │
│                                                                 │
│   //stepname EXEC PROC=procname[,param1=val1,...]              │
│                                                                 │
│   EXEMPLES :                                                    │
│   ──────────                                                   │
│   //STEP1 EXEC MYPROC                                          │
│   //STEP1 EXEC MYPROC,DSN=DATA.FILE                            │
│   //STEP1 EXEC PROC=MYPROC,DSN=DATA.FILE                       │
│                                                                 │
│   Note : PROC= est optionnel si le nom ne correspond pas      │
│   a un nom de programme dans LINKLIB                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-3-2 Passage de parametres

```jcl
//* PROCEDURE
//COMPPROC PROC SRCLIB=,
//              SRCMEM=,
//              LOADLIB=,
//              COPYLIB=SYS1.COPYLIB,
//              OPTIONS='LIST,MAP'
//*
//COB      EXEC PGM=IGYCRCTL,PARM='&OPTIONS'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=&COPYLIB,DISP=SHR
//SYSIN    DD DSN=&SRCLIB(&SRCMEM),DISP=SHR
//SYSLIN   DD DSN=&&OBJ,DISP=(NEW,PASS)
//SYSPRINT DD SYSOUT=*
//*
//LKED     EXEC PGM=IEWL,COND=(8,LT,COB)
//SYSLIN   DD DSN=&&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=&LOADLIB(&SRCMEM),DISP=SHR
//SYSPRINT DD SYSOUT=*
//         PEND

//* APPEL
//MYJOB    JOB ...
//COMPILE  EXEC COMPPROC,
//              SRCLIB=FTEST.SOURCE,
//              SRCMEM=MYPROG,
//              LOADLIB=FTEST.LOADLIB,
//              COPYLIB=FTEST.COPYLIB
```

### III-3-3 Qualification des parametres pour EXEC dans procedure

```
┌─────────────────────────────────────────────────────────────────┐
│            PARAMETRES EXEC QUALIFIES PAR STEP                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Pour passer des parametres a un step specifique de la        │
│   procedure, utiliser la notation : param.stepname             │
│                                                                 │
│   PROCEDURE :                                                   │
│   //MYPROC PROC                                                 │
│   //STEP1  EXEC PGM=PROG1                                       │
│   //STEP2  EXEC PGM=PROG2                                       │
│   //       PEND                                                 │
│                                                                 │
│   APPEL AVEC PARAMETRES QUALIFIES :                             │
│   //CALL   EXEC MYPROC,                                         │
│   //            PARM.STEP1='PARAM1',                            │
│   //            PARM.STEP2='PARAM2',                            │
│   //            REGION.STEP1=4M,                                │
│   //            TIME.STEP2=5                                    │
│                                                                 │
│   PARAMETRES QUALIFIABLES :                                     │
│   • PARM.stepname                                              │
│   • REGION.stepname                                            │
│   • TIME.stepname                                              │
│   • COND.stepname                                              │
│   • ADDRSPC.stepname                                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-4 Modification d'une procedure

### III-4-1 Override des cartes DD

```
┌─────────────────────────────────────────────────────────────────┐
│                    OVERRIDE (SURCHARGE) DD                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   On peut modifier les cartes DD d'une procedure a l'appel     │
│   en utilisant la notation : stepname.ddname                   │
│                                                                 │
│   PROCEDURE :                                                   │
│   //MYPROC PROC                                                 │
│   //STEP1  EXEC PGM=MYPROG                                      │
│   //INPUT  DD DSN=DEFAULT.INPUT,DISP=SHR                       │
│   //OUTPUT DD DSN=DEFAULT.OUTPUT,DISP=(NEW,CATLG)              │
│   //       PEND                                                 │
│                                                                 │
│   OVERRIDE A L'APPEL :                                          │
│   //CALL   EXEC MYPROC                                          │
│   //STEP1.INPUT DD DSN=MY.INPUT,DISP=SHR                       │
│   //STEP1.OUTPUT DD DSN=MY.OUTPUT,DISP=(NEW,CATLG)             │
│   ───────────────                                               │
│   stepname.ddname                                               │
│                                                                 │
│   REGLES :                                                      │
│   • Le DD d'override remplace completement le DD original      │
│   • L'ordre des overrides n'est pas important                  │
│   • On peut aussi ajouter des DD qui n'existent pas            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-4-2 Ajout de cartes DD

```jcl
//* PROCEDURE SANS CERTAINS DD
//RUNPROC  PROC
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=DEFAULT.INPUT,DISP=SHR
//OUTPUT   DD SYSOUT=*
//         PEND

//* APPEL AVEC AJOUT DE DD
//CALL     EXEC RUNPROC
//STEP1.INPUT DD DSN=MY.INPUT,DISP=SHR
//STEP1.SYSUDUMP DD SYSOUT=*
//STEP1.CEEDUMP DD SYSOUT=*
```

### III-4-3 Concatenation avec override

```
┌─────────────────────────────────────────────────────────────────┐
│                    OVERRIDE AVEC CONCATENATION                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Pour ajouter des fichiers en concatenation :                  │
│                                                                 │
│   PROCEDURE :                                                   │
│   //MYPROC PROC                                                 │
│   //STEP1  EXEC PGM=IGYCRCTL                                    │
│   //SYSLIB DD DSN=SYS1.COPYLIB,DISP=SHR                        │
│   //       PEND                                                 │
│                                                                 │
│   APPEL AVEC CONCATENATION SUPPLEMENTAIRE :                     │
│   //CALL   EXEC MYPROC                                          │
│   //STEP1.SYSLIB DD                                             │
│   //             DD DSN=USER.COPYLIB,DISP=SHR                   │
│   //             DD DSN=PROD.COPYLIB,DISP=SHR                   │
│                                                                 │
│   La premiere DD vide conserve la DD de la procedure           │
│   Les suivantes sont ajoutees en concatenation                 │
│                                                                 │
│   RESULTAT :                                                    │
│   SYSLIB = SYS1.COPYLIB + USER.COPYLIB + PROD.COPYLIB          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-4-4 Exemple complet d'overrides

```jcl
//* PROCEDURE DE COMPILATION COBOL
//COBCLG   PROC SRCLIB=,
//              SRCMEM=,
//              LOADLIB=
//*
//* STEP COMPILE
//COB      EXEC PGM=IGYCRCTL,PARM='LIST,MAP'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSLIB   DD DSN=SYS1.COPYLIB,DISP=SHR
//SYSIN    DD DSN=&SRCLIB(&SRCMEM),DISP=SHR
//SYSLIN   DD DSN=&&OBJ,DISP=(NEW,PASS)
//SYSPRINT DD SYSOUT=*
//*
//* STEP LINKEDIT
//LKED     EXEC PGM=IEWL,COND=(8,LT,COB)
//SYSLIN   DD DSN=&&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=&LOADLIB(&SRCMEM),DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//* STEP GO (EXECUTION)
//GO       EXEC PGM=*.LKED.SYSLMOD,COND=(8,LT,LKED)
//STEPLIB  DD DSN=&LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//         PEND

//* APPEL AVEC OVERRIDES
//MYJOB    JOB ...
//         JCLLIB ORDER=USER.PROCLIB
//*
//COMPILE  EXEC COBCLG,
//              SRCLIB=FTEST.SOURCE,
//              SRCMEM=MYPROG,
//              LOADLIB=FTEST.LOADLIB,
//              PARM.COB='LIST,MAP,XREF',
//              REGION.GO=8M
//*
//* Override SYSLIB pour ajouter mes COPYLIB
//COB.SYSLIB DD
//           DD DSN=FTEST.COPYLIB,DISP=SHR
//           DD DSN=PROD.COPYLIB,DISP=SHR
//*
//* Override pour le step GO - ajouter mes fichiers
//GO.INPUT DD DSN=FTEST.INPUT.DATA,DISP=SHR
//GO.OUTPUT DD DSN=FTEST.OUTPUT.DATA,
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(10,5))
//
```

---

## III-5 Le listing d'execution

### III-5-1 Interpretation du listing

```
┌─────────────────────────────────────────────────────────────────┐
│                    LISTING D'EXECUTION                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Le listing JES montre l'expansion de la procedure :           │
│                                                                 │
│   XX = Instructions du JCL appelant                            │
│   ++ = Instructions generees par expansion procedure           │
│   // = Instructions originales de la procedure                 │
│                                                                 │
│   EXEMPLE DE LISTING :                                          │
│   ────────────────────                                         │
│                                                                 │
│   XXMYJOB   JOB (ACCT),'TEST',CLASS=A,MSGCLASS=X               │
│   XX        JCLLIB ORDER=USER.PROCLIB                          │
│   XXSTEP1   EXEC MYPROC,DSN=DATA.FILE                          │
│   ++STEP1   EXEC PGM=IEBGENER                                   │
│   ++SYSPRINT DD SYSOUT=*                                        │
│   ++SYSIN   DD DUMMY                                            │
│   ++SYSUT1  DD DSN=DATA.FILE,DISP=SHR                          │
│   ++SYSUT2  DD DSN=DATA.OUTPUT,...                             │
│   XXSTEP1.SYSUT1 DD DSN=MY.OVERRIDE,DISP=SHR                   │
│                                                                 │
│   Note : Les symboles sont remplaces dans les lignes ++        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-5-2 Messages JES et codes retour

```
┌─────────────────────────────────────────────────────────────────┐
│                    MESSAGES PROCEDURES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   MESSAGES D'ERREUR COURANTS :                                  │
│   ────────────────────────────                                 │
│   IEF653I SUBSTITUTION JCL - &param                            │
│   → Parametre symbolique remplace                              │
│                                                                 │
│   IEFC001I PROCEDURE procname WAS EXPANDED USING...            │
│   → Procedure trouvee et expandue                              │
│                                                                 │
│   IEFC032I PROCEDURE procname NOT FOUND                        │
│   → Procedure non trouvee dans PROCLIB                         │
│                                                                 │
│   IEFC034I UNIDENTIFIED KEYWORD                                │
│   → Parametre inconnu passe a la procedure                     │
│                                                                 │
│   IEF632I UNKNOWN KEYWORD IN EXEC STATEMENT PARAMETER          │
│   → Parametre invalide sur EXEC                                │
│                                                                 │
│   IEFC619I INVALID VALUE FOR SYMBOLIC param                    │
│   → Valeur invalide pour le parametre                          │
│                                                                 │
│   NIVEAU DE MESSAGES (MSGLEVEL) :                               │
│   ────────────────────────────────                             │
│   MSGLEVEL=(1,1) recommande pour voir l'expansion              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-6 Procedures imbriquees

### III-6-1 Concept

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCEDURES IMBRIQUEES                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Une procedure peut appeler une autre procedure.               │
│   Maximum 15 niveaux d'imbrication.                            │
│                                                                 │
│   SCHEMA :                                                      │
│   ────────                                                     │
│   JCL appelle PROC1                                             │
│       └── PROC1 appelle PROC2                                   │
│               └── PROC2 appelle PROC3                           │
│                       └── ...                                   │
│                                                                 │
│   REGLES :                                                      │
│   ────────                                                     │
│   • Maximum 15 niveaux                                         │
│   • Pas d'appel recursif (PROC1 ne peut pas appeler PROC1)     │
│   • Les parametres se propagent                                │
│   • Les overrides sont possibles a chaque niveau               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-6-2 Exemple de procedures imbriquees

```jcl
//* ============================================================
//* PROCEDURE NIVEAU 2 : COPIE SIMPLE (COPYFILE)
//* Stockee dans USER.PROCLIB(COPYFILE)
//* ============================================================
//COPYFILE PROC INDSN=,OUTDSN=
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=*.SYSUT1

//* ============================================================
//* PROCEDURE NIVEAU 1 : TRAITEMENT COMPLET (FULLPROC)
//* Appelle COPYFILE (imbrication)
//* Stockee dans USER.PROCLIB(FULLPROC)
//* ============================================================
//FULLPROC PROC SRCDSN=,
//              TGTDSN=,
//              BAKDSN=
//*
//* Etape 1 : Backup du fichier cible actuel
//BACKUP   EXEC COPYFILE,
//              INDSN=&TGTDSN,
//              OUTDSN=&BAKDSN
//*
//* Etape 2 : Copie du nouveau fichier
//COPY     EXEC COPYFILE,
//              INDSN=&SRCDSN,
//              OUTDSN=&TGTDSN

//* ============================================================
//* JCL APPELANT
//* ============================================================
//MYJOB    JOB (ACCT),'IMBRICATION',CLASS=A,MSGCLASS=X
//         JCLLIB ORDER=USER.PROCLIB
//*
//PROCESS  EXEC FULLPROC,
//              SRCDSN=FTEST.INPUT.NEW,
//              TGTDSN=FTEST.PROD.DATA,
//              BAKDSN=FTEST.BACKUP.DATA
//
```

### III-6-3 Override dans procedures imbriquees

```
┌─────────────────────────────────────────────────────────────────┐
│              OVERRIDE AVEC IMBRICATION                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Pour override dans une procedure appelee par une autre :      │
│   procstep.stepname.ddname                                      │
│                                                                 │
│   STRUCTURE :                                                   │
│   JCL                                                           │
│   └── EXEC FULLPROC (procstep = PROCESS)                       │
│       └── FULLPROC.BACKUP = EXEC COPYFILE                      │
│           └── COPYFILE.COPY = step interne                     │
│                                                                 │
│   OVERRIDE :                                                    │
│   //PROCESS  EXEC FULLPROC,...                                  │
│   //BACKUP.COPY.SYSUT1 DD DISP=OLD                             │
│   ───────────────────────                                       │
│   procstep.step.ddname                                          │
│                                                                 │
│   Ou si on veut overrider le step direct de FULLPROC :          │
│   //BACKUP.SYSUT2 DD DSN=ANOTHER.FILE,...                      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### III-6-4 Exemple complet avec imbrication

```jcl
//* ============================================================
//* JCL AVEC OVERRIDE DE PROCEDURE IMBRIQUEE
//* ============================================================
//FULLJOB  JOB (ACCT),'OVERRIDE IMBRIQUE',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1)
//         JCLLIB ORDER=USER.PROCLIB
//*
//* Appel de FULLPROC qui appelle COPYFILE
//*
//PROCESS  EXEC FULLPROC,
//              SRCDSN=FTEST.INPUT.NEW,
//              TGTDSN=FTEST.PROD.DATA,
//              BAKDSN=FTEST.BACKUP.DATA
//*
//* Override du step COPY dans la procedure COPYFILE
//* appelee par le step BACKUP de FULLPROC
//*
//BACKUP.COPY.SYSPRINT DD SYSOUT=X
//BACKUP.COPY.SYSUT2   DD SPACE=(CYL,(1,1),RLSE)
//*
//* Override du step COPY appele directement par FULLPROC
//*
//COPY.COPY.SYSUT2 DD SPACE=(CYL,(2,1),RLSE)
//
```

---

## III-7 Bonnes pratiques

```
┌─────────────────────────────────────────────────────────────────┐
│                    BONNES PRATIQUES PROCEDURES                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. NOMMAGE                                                    │
│      • Prefixe par application : PAIECOMP, STOCKINV            │
│      • Suffixe par fonction : xxxCOMP, xxxLKED, xxxGO          │
│      • Documentation dans le nom                               │
│                                                                 │
│   2. PARAMETRES                                                 │
│      • Valeurs par defaut sensees                              │
│      • Pas trop de parametres (max 10-15)                      │
│      • Noms explicites : SRCLIB pas SL                         │
│                                                                 │
│   3. DOCUMENTATION                                              │
│      • Commentaires en debut de procedure                      │
│      • Liste des parametres avec description                   │
│      • Exemples d'appel                                        │
│                                                                 │
│   4. GESTION                                                    │
│      • Versionner les procedures                               │
│      • Tester avant mise en production                         │
│      • Bibliotheques separees TEST/PROD                        │
│                                                                 │
│   5. SECURITE                                                   │
│      • Proteger les PROCLIB en production                      │
│      • Audit des modifications                                 │
│      • Backup des procedures                                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Synthese

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLES DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   TYPES DE PROCEDURES                                           │
│   ───────────────────                                          │
│   • In-stream : definie dans JCL, PROC...PEND                  │
│   • Cataloguee : stockee dans PROCLIB                          │
│   • Imbriquee : procedure appelant procedure                   │
│                                                                 │
│   PARAMETRES SYMBOLIQUES                                        │
│   ──────────────────────                                       │
│   • Declaration : PROC PARAM=default                           │
│   • Utilisation : &PARAM, &PARAM.suite                         │
│   • Passage : EXEC PROC,PARAM=valeur                           │
│   • Systeme : &SYSUID, &LYYMMDD, &SYSTIME                      │
│                                                                 │
│   APPEL ET MODIFICATION                                         │
│   ─────────────────────                                        │
│   • Appel : EXEC procname,param=valeur                         │
│   • Override DD : stepname.ddname DD ...                       │
│   • Override EXEC : PARM.stepname=valeur                       │
│   • Concatenation : DD vide puis DD supplementaires            │
│                                                                 │
│   JCLLIB                                                        │
│   ──────                                                       │
│   • Definit les bibliotheques de procedures                    │
│   • Apres JOB, avant premier EXEC                              │
│   • JCLLIB ORDER=(lib1,lib2,...)                               │
│                                                                 │
│   LISTING                                                       │
│   ───────                                                      │
│   • XX = JCL original                                          │
│   • ++ = Expansion procedure                                   │
│   • MSGLEVEL=(1,1) pour debug                                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-memoire

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MEMOIRE PROCEDURES                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DEFINITION IN-STREAM                                          │
│   ─────────────────────────────────────────────────────────    │
│   //MYPROC PROC PARAM1=,PARAM2=DEFAULT                         │
│   //STEP1  EXEC PGM=...                                         │
│   //DD1    DD DSN=&PARAM1,DISP=SHR                             │
│   //       PEND                                                 │
│                                                                 │
│   DEFINITION CATALOGUEE                                         │
│   ─────────────────────────────────────────────────────────    │
│   (Membre dans PROCLIB, pas de PEND)                           │
│   //MYPROC PROC PARAM1=                                         │
│   //STEP1  EXEC PGM=...                                         │
│   //DD1    DD DSN=&PARAM1,DISP=SHR                             │
│                                                                 │
│   APPEL                                                         │
│   ─────────────────────────────────────────────────────────    │
│   //       JCLLIB ORDER=USER.PROCLIB                           │
│   //CALL   EXEC MYPROC,PARAM1=DATA.FILE                        │
│                                                                 │
│   OVERRIDE DD                                                   │
│   ─────────────────────────────────────────────────────────    │
│   //CALL   EXEC MYPROC                                          │
│   //STEP1.DD1 DD DSN=OTHER.FILE,DISP=SHR                       │
│                                                                 │
│   OVERRIDE EXEC                                                 │
│   ─────────────────────────────────────────────────────────    │
│   //CALL   EXEC MYPROC,PARM.STEP1='OPTIONS'                    │
│                                                                 │
│   CONCATENATION OVERRIDE                                        │
│   ─────────────────────────────────────────────────────────    │
│   //STEP1.SYSLIB DD                                             │
│   //             DD DSN=MY.LIB,DISP=SHR                         │
│                                                                 │
│   IMBRICATION                                                   │
│   ─────────────────────────────────────────────────────────    │
│   //PROC1 appelle PROC2                                        │
│   Override: //CALL.STEP2.DD1 DD ...                            │
│             procstep.step.ddname                                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Chapitre II - Fichiers speciaux](02-fichiers-parametres.md) | [Chapitre IV - Les utilitaires](04-utilitaires.md) |
