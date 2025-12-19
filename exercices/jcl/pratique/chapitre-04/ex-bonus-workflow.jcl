//FTESTBON JOB (ACCT),'BONUS WORKFLOW COMPLET',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE BONUS : WORKFLOW COMPLET
//* ============================================================
//* Objectif : Combiner plusieurs utilitaires dans un workflow
//*            de traitement batch complet
//*
//* Scenario :
//* 1. Nettoyage des anciens fichiers
//* 2. Creation d'un fichier de donnees clients
//* 3. Tri des donnees par ville puis nom
//* 4. Separation Paris / Autres villes
//* 5. Creation VSAM et chargement
//* 6. Rapport final
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : NETTOYAGE COMPLET
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.BONUS.INPUT
  DELETE FTEST.BONUS.SORTED
  DELETE FTEST.BONUS.PARIS
  DELETE FTEST.BONUS.AUTRES
  DELETE FTEST.BONUS.VSAM CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* STEP 2 : CREATION FICHIER DE DONNEES
//* ----------------------------------------------------------
//CREATE   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
001DUPONT    JEAN      PARIS     7500150000
002MARTIN    MARIE     LYON      6900125000
003DURAND    PIERRE    MARSEILLE 1300175000
004PETIT     SOPHIE    BORDEAUX  3300130000
005BERNARD   PAUL      PARIS     7500845000
006THOMAS    CLAIRE    LYON      6900235000
007ROBERT    LUC       NANTES    4400020000
008RICHARD   ANNE      PARIS     7501655000
009MOREAU    JACQUES   TOULOUSE  3100040000
010SIMON     MARIE     PARIS     7500560000
/*
//SYSUT2   DD DSN=FTEST.BONUS.INPUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=50,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 3 : TRI PAR VILLE PUIS NOM
//* ----------------------------------------------------------
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.BONUS.INPUT,DISP=SHR
//SORTOUT  DD DSN=FTEST.BONUS.SORTED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* TRI PAR VILLE (POS 24-33) PUIS NOM (POS 4-13)
  SORT FIELDS=(24,10,CH,A,4,10,CH,A)
/*
//*
//* ----------------------------------------------------------
//* STEP 4 : SEPARATION PARIS / AUTRES (OUTFIL)
//* ----------------------------------------------------------
//SPLIT    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.BONUS.SORTED,DISP=SHR
//PARIS    DD DSN=FTEST.BONUS.PARIS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//AUTRES   DD DSN=FTEST.BONUS.AUTRES,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* SEPARER LES CLIENTS PARISIENS DES AUTRES
  SORT FIELDS=COPY
  OUTFIL FNAMES=PARIS,INCLUDE=(34,2,CH,EQ,C'75')
  OUTFIL FNAMES=AUTRES,SAVE
/*
//*
//* ----------------------------------------------------------
//* STEP 5 : CREATION VSAM KSDS
//* ----------------------------------------------------------
//DEFVSAM  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(FTEST.BONUS.VSAM)              -
           INDEXED                             -
           KEYS(3 0)                           -
           RECORDSIZE(50 50)                   -
           TRACKS(5 2)                         -
           SHAREOPTIONS(2 3)                   -
         )                                     -
         DATA (                                -
           NAME(FTEST.BONUS.VSAM.DATA)         -
         )                                     -
         INDEX (                               -
           NAME(FTEST.BONUS.VSAM.INDEX)        -
         )
/*
//*
//* ----------------------------------------------------------
//* STEP 6 : CHARGEMENT VSAM
//* ----------------------------------------------------------
//LOADVSAM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.BONUS.SORTED,DISP=SHR
//OUTFILE  DD DSN=FTEST.BONUS.VSAM,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//* ----------------------------------------------------------
//* STEP 7 : RAPPORT FINAL
//* ----------------------------------------------------------
//RAPPORT  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
========================================
      RAPPORT DE TRAITEMENT BATCH
========================================

FICHIERS CREES :
  - FTEST.BONUS.INPUT   : Donnees brutes
  - FTEST.BONUS.SORTED  : Donnees triees
  - FTEST.BONUS.PARIS   : Clients Paris
  - FTEST.BONUS.AUTRES  : Clients autres villes
  - FTEST.BONUS.VSAM    : KSDS charge

TRAITEMENT TERMINE AVEC SUCCES

========================================
/*
//SYSUT2   DD SYSOUT=*
//*
//* ----------------------------------------------------------
//* STEP 8 : AFFICHAGE FICHIERS CREES
//* ----------------------------------------------------------
//PRTPARIS EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.BONUS.PARIS,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=51)
//*
//PRTAUTRE EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.BONUS.AUTRES,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=51)
//*
//PRTVSAM  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.BONUS.VSAM,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(INFILE) CHARACTER
/*
//
