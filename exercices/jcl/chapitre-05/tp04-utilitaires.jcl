//FTESTTP4 JOB (ACCT),'TP04 UTILITAIRES',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 4 : UTILISATION DES UTILITAIRES IBM
//* ============================================================
//* Objectif : Pratiquer les utilitaires IEFBR14, IEBGENER,
//*            IEBCOPY, IEBCOMPR et IDCAMS
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* ETAPE 0 : NETTOYAGE INITIAL
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.TP04.SOURCE
  DELETE FTEST.TP04.COPIE
  DELETE FTEST.TP04.PDS1
  DELETE FTEST.TP04.PDS2
  DELETE FTEST.TP04.VSAM CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//* ==========================================================
//* PARTIE 1 : IEFBR14 - CREATION DATASETS VIDES
//* ==========================================================
//*
//IEFBR14  EXEC PGM=IEFBR14
//* Creation d'un fichier sequentiel vide
//SEQVIDE  DD DSN=FTEST.TP04.SOURCE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//* Creation d'un PDS vide
//PDSVIDE1 DD DSN=FTEST.TP04.PDS1,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//PDSVIDE2 DD DSN=FTEST.TP04.PDS2,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//*
//* ==========================================================
//* PARTIE 2 : IEBGENER - CHARGEMENT ET COPIE
//* ==========================================================
//*
//* Chargement du fichier source avec des donnees
//LOADDATA EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
001DUPONT    JEAN      75001PARIS     0100000
002MARTIN    MARIE     69001LYON      0075000
003DURAND    PIERRE    13001MARSEILLE 0150000
004PETIT     SOPHIE    33001BORDEAUX  0050000
005BERNARD   PAUL      75008PARIS     0200000
/*
//SYSUT2   DD DSN=FTEST.TP04.SOURCE,DISP=MOD
//*
//* Copie vers un nouveau fichier
//COPYDATA EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.TP04.SOURCE,DISP=SHR
//SYSUT2   DD DSN=FTEST.TP04.COPIE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//*
//* ==========================================================
//* PARTIE 3 : IEBCOPY - GESTION PDS
//* ==========================================================
//*
//* Charger des membres dans PDS1
//LOADMB1  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
* MEMBRE A - CONFIGURATION
PARAM1=VALUE1
PARAM2=VALUE2
/*
//SYSUT2   DD DSN=FTEST.TP04.PDS1(MEMBREA),DISP=SHR
//*
//LOADMB2  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
* MEMBRE B - DONNEES
DATA LINE 1
DATA LINE 2
/*
//SYSUT2   DD DSN=FTEST.TP04.PDS1(MEMBREB),DISP=SHR
//*
//* Copier les membres de PDS1 vers PDS2 avec IEBCOPY
//COPYPDS  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=FTEST.TP04.PDS1,DISP=SHR
//OUTPDS   DD DSN=FTEST.TP04.PDS2,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
/*
//*
//* ==========================================================
//* PARTIE 4 : IEBCOMPR - COMPARAISON
//* ==========================================================
//*
//COMPARE  EXEC PGM=IEBCOMPR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=FTEST.TP04.SOURCE,DISP=SHR
//SYSUT2   DD DSN=FTEST.TP04.COPIE,DISP=SHR
//SYSIN    DD *
  COMPARE TYPORG=PS
/*
//*
//* ==========================================================
//* PARTIE 5 : IDCAMS - GESTION VSAM
//* ==========================================================
//*
//* Definition d'un KSDS
//DEFVSAM  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(FTEST.TP04.VSAM)               -
           INDEXED                             -
           KEYS(3 0)                           -
           RECORDSIZE(60 60)                   -
           TRACKS(5 2)                         -
           SHAREOPTIONS(2 3)                   -
         )                                     -
         DATA (                                -
           NAME(FTEST.TP04.VSAM.DATA)          -
         )                                     -
         INDEX (                               -
           NAME(FTEST.TP04.VSAM.INDEX)         -
         )
/*
//*
//* Chargement du VSAM depuis le fichier sequentiel
//LOADVSAM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.TP04.SOURCE,DISP=SHR
//OUTFILE  DD DSN=FTEST.TP04.VSAM,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//* Affichage du contenu VSAM
//PRINTVSM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.TP04.VSAM,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(INFILE) CHARACTER
/*
//*
//* Information catalogue
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(FTEST.TP04.*) ALL
/*
//
