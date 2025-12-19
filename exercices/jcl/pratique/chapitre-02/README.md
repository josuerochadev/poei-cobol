# Exercices JCL - Chapitre II
## Fichiers speciaux et parametres

Ces exercices permettent de pratiquer la creation de fichiers, l'utilisation de IEBGENER, les fichiers temporaires et la concatenation.

---

## Exercice 1 : Creation d'un Data Set avec donnees in-stream

### Objectif
Creer un dataset `FTEST.ESDS.AAAA` avec des enregistrements de 80 caracteres et le charger avec des donnees inline.

### Donnees a charger
```
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
```

### Instructions
1. Utiliser l'utilitaire IEBGENER
2. Le fichier doit avoir un format fixe bloque (FB) avec LRECL=80
3. Allouer suffisamment d'espace (quelques pistes suffisent)
4. Les donnees sont fournies en SYSIN via DD *

### Solution

```jcl
//FTESTEX1 JOB (ACCT),'EXERCICE 1',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 1 : CREATION DATASET AVEC DONNEES IN-STREAM
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//SYSUT2   DD DSN=FTEST.ESDS.AAAA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01A  JOB (ACCT),'EXERCICE 1',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//SYSUT2   DD DSN=HERC01.ESDS.AAAA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            VOL=SER=PUB001,
//            UNIT=3390
//
```

### Verification
Apres execution, verifier le dataset avec ISPF 3.4 :
- Le dataset `FTEST.ESDS.AAAA` doit exister
- Il doit contenir 3 enregistrements
- Browse (B) pour voir le contenu

---

## Exercice 2 : Copie de Data Set avec IEBGENER

### Objectif
Copier le dataset `FTEST.ESDS.AAAA` cree dans l'exercice 1 vers un nouveau dataset `FTEST.ESDS.BBBB`.

### Instructions
1. Utiliser IEBGENER pour la copie
2. Le fichier cible doit avoir les memes caracteristiques que la source
3. Utiliser une reference arriere (DCB=*) pour copier les caracteristiques

### Solution

```jcl
//FTESTEX2 JOB (ACCT),'EXERCICE 2',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 2 : COPIE DATASET AVEC IEBGENER
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01B  JOB (ACCT),'EXERCICE 2',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=HERC01.ESDS.AAAA,DISP=SHR
//SYSUT2   DD DSN=HERC01.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,
//            UNIT=3390
//
```

### Points cles
- `DISP=SHR` sur SYSUT1 car on lit un fichier existant
- `DCB=*.SYSUT1` copie les caracteristiques du fichier source
- Reference arriere evite de re-specifier RECFM, LRECL, BLKSIZE

### Verification
- Le dataset `FTEST.ESDS.BBBB` doit exister
- Son contenu doit etre identique a `FTEST.ESDS.AAAA`

---

## Exercice 3 : Utilisation d'un fichier temporaire

### Objectif
Copier `FTEST.ESDS.BBBB` vers un fichier temporaire `&&TEMP`, puis copier ce temporaire vers `FTEST.ESDS.CCCC`.

### Instructions
1. Premier step : copier ESDS.BBBB vers &&TEMP
2. Deuxieme step : copier &&TEMP vers ESDS.CCCC
3. Le fichier temporaire doit etre passe entre les steps (DISP=PASS)
4. Le fichier temporaire sera supprime automatiquement

### Solution

```jcl
//FTESTEX3 JOB (ACCT),'EXERCICE 3',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 3 : FICHIER TEMPORAIRE
//* ============================================================
//*
//* STEP 1 : Copie vers fichier temporaire
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(1,1)),
//            DCB=*.SYSUT1
//*
//* STEP 2 : Copie du temporaire vers fichier permanent
//*
//STEP020  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&&TEMP,
//            DISP=(OLD,DELETE)
//SYSUT2   DD DSN=FTEST.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01C  JOB (ACCT),'EXERCICE 3',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* STEP 1 : Copie vers fichier temporaire
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=HERC01.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(1,1)),
//            DCB=*.SYSUT1
//*
//* STEP 2 : Copie du temporaire vers fichier permanent
//STEP020  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&&TEMP,
//            DISP=(OLD,DELETE)
//SYSUT2   DD DSN=HERC01.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,
//            UNIT=3390
//
```

### Points cles
- `DSN=&&TEMP` : le double ampersand indique un fichier temporaire
- `DISP=(NEW,PASS)` : cree et passe au step suivant
- `DISP=(OLD,DELETE)` : utilise puis supprime
- Le temporaire n'apparait pas dans le catalogue
- Automatiquement supprime en fin de job

### Verification
- Le dataset `FTEST.ESDS.CCCC` doit exister
- Le fichier temporaire n'existe plus apres le job
- Verifier les messages JES pour voir la creation/suppression du temporaire

---

## Exercice 4 : Concatenation de Data Sets

### Objectif
Concatener les trois datasets `FTEST.ESDS.AAAA`, `FTEST.ESDS.BBBB` et `FTEST.ESDS.CCCC` dans un nouveau dataset `FTEST.ESDS.DDDD`.

### Instructions
1. Utiliser IEBGENER avec concatenation en entree (SYSUT1)
2. Le fichier de sortie contiendra 9 enregistrements (3 x 3)
3. Les fichiers sont lus dans l'ordre de declaration

### Solution

```jcl
//FTESTEX4 JOB (ACCT),'EXERCICE 4',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 4 : CONCATENATION DE DATASETS
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//* Concatenation des 3 fichiers en entree
//*
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//         DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//         DD DSN=FTEST.ESDS.CCCC,DISP=SHR
//*
//* Fichier de sortie concatene
//*
//SYSUT2   DD DSN=FTEST.ESDS.DDDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
```

### Adaptation Hercules/TK4-

```jcl
//HERC01D  JOB (ACCT),'EXERCICE 4',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//* Concatenation des 3 fichiers en entree
//SYSUT1   DD DSN=HERC01.ESDS.AAAA,DISP=SHR
//         DD DSN=HERC01.ESDS.BBBB,DISP=SHR
//         DD DSN=HERC01.ESDS.CCCC,DISP=SHR
//*
//* Fichier de sortie concatene
//SYSUT2   DD DSN=HERC01.ESDS.DDDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            VOL=SER=PUB001,
//            UNIT=3390
//
```

### Points cles
- Le DDname `SYSUT1` apparait uniquement sur la premiere DD
- Les DD suivantes n'ont pas de nom (concatenation)
- Les fichiers sont lus sequentiellement
- Le `DCB=*.SYSUT1` prend les caracteristiques du premier fichier

### Verification
- Le dataset `FTEST.ESDS.DDDD` doit exister
- Il doit contenir 9 enregistrements (3 de chaque fichier source)
- L'ordre doit etre : AAAA, BBBB, CCCC

---

## Exercice Bonus : JCL complet en un seul job

### Objectif
Realiser tous les exercices precedents dans un seul JCL multi-steps.

### Solution complete

```jcl
//FTESTALL JOB (ACCT),'EXERCICES JCL',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* JCL COMPLET - EXERCICES CHAPITRE II
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : Creation ESDS.AAAA avec donnees in-stream
//* ----------------------------------------------------------
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//SYSUT2   DD DSN=FTEST.ESDS.AAAA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 2 : Copie ESDS.AAAA vers ESDS.BBBB
//* ----------------------------------------------------------
//STEP020  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 3 : Copie ESDS.BBBB vers temporaire
//* ----------------------------------------------------------
//STEP030  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(1,1)),
//            DCB=*.SYSUT1
//*
//* ----------------------------------------------------------
//* STEP 4 : Copie temporaire vers ESDS.CCCC
//* ----------------------------------------------------------
//STEP040  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&&TEMP,DISP=(OLD,DELETE)
//SYSUT2   DD DSN=FTEST.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 5 : Concatenation vers ESDS.DDDD
//* ----------------------------------------------------------
//STEP050  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//         DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//         DD DSN=FTEST.ESDS.CCCC,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.DDDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
```

---

## Resume des concepts pratiques

```
┌─────────────────────────────────────────────────────────────────┐
│                    RESUME DES EXERCICES                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   EXERCICE 1 - Creation avec DD *                               │
│   • IEBGENER pour copier donnees inline vers dataset           │
│   • SYSUT1 DD * pour donnees in-stream                         │
│   • DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)                          │
│                                                                 │
│   EXERCICE 2 - Copie avec reference DCB                         │
│   • IEBGENER pour copie dataset a dataset                      │
│   • DCB=*.SYSUT1 pour copier les caracteristiques              │
│   • DISP=SHR pour lecture fichier existant                     │
│                                                                 │
│   EXERCICE 3 - Fichier temporaire                               │
│   • DSN=&&TEMP pour creer un temporaire                        │
│   • DISP=(NEW,PASS) puis DISP=(OLD,DELETE)                     │
│   • Suppression automatique en fin de job                      │
│                                                                 │
│   EXERCICE 4 - Concatenation                                    │
│   • DDname uniquement sur premiere DD                          │
│   • Fichiers lus sequentiellement                              │
│   • Resultat = fusion des contenus                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Nettoyage apres exercices

Pour supprimer les datasets crees :

```jcl
//CLEANUP  JOB (ACCT),'NETTOYAGE',CLASS=A,MSGCLASS=A
//*
//DELETE   EXEC PGM=IEFBR14
//DD1      DD DSN=FTEST.ESDS.AAAA,DISP=(OLD,DELETE)
//DD2      DD DSN=FTEST.ESDS.BBBB,DISP=(OLD,DELETE)
//DD3      DD DSN=FTEST.ESDS.CCCC,DISP=(OLD,DELETE)
//DD4      DD DSN=FTEST.ESDS.DDDD,DISP=(OLD,DELETE)
//
```

Ou avec IDCAMS :

```jcl
//CLEANUP  JOB (ACCT),'NETTOYAGE',CLASS=A,MSGCLASS=A
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.ESDS.AAAA
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.ESDS.CCCC
  DELETE FTEST.ESDS.DDDD
  SET MAXCC=0
/*
//
```
