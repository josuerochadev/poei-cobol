//FTETP02  JOB (ACCT),'TP02 GDG',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  TP 02 : GESTION DES GENERATION DATA GROUPS                       *
//*  - Definition GDG                                                  *
//*  - Creation de generations                                         *
//*  - Rotation automatique                                            *
//*  - Concatenation                                                   *
//*********************************************************************
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.TP02.DAILY) GDG FORCE
  DELETE (FTEST.TP02.CONCAT) NONVSAM PURGE
  SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 1: DEFINITION DU GDG
//*-------------------------------------------------------------------*
//DEFGDG   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG ( -
    NAME(FTEST.TP02.DAILY) -
    LIMIT(3) -
    NOEMPTY -
    SCRATCH)

  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(FTEST.TP02.DAILY) ALL
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2: CREATION GENERATION 1
//*-------------------------------------------------------------------*
//GEN1     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250101 DONNEES DU JOUR 1 - LUNDI
20250101 TRANSACTION 001 - VENTE
20250101 TRANSACTION 002 - ACHAT
/*
//SYSUT2   DD DSN=FTEST.TP02.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 3: CREATION GENERATION 2
//*-------------------------------------------------------------------*
//GEN2     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250102 DONNEES DU JOUR 2 - MARDI
20250102 TRANSACTION 003 - VENTE
20250102 TRANSACTION 004 - RETOUR
/*
//SYSUT2   DD DSN=FTEST.TP02.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 4: CREATION GENERATION 3
//*-------------------------------------------------------------------*
//GEN3     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250103 DONNEES DU JOUR 3 - MERCREDI
20250103 TRANSACTION 005 - VENTE
20250103 TRANSACTION 006 - VENTE
20250103 TRANSACTION 007 - ACHAT
/*
//SYSUT2   DD DSN=FTEST.TP02.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 5: LISTER LES GENERATIONS (doit avoir G0001, G0002, G0003)
//*-------------------------------------------------------------------*
//LIST3    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(FTEST.TP02.DAILY) NAME
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 6: CREATION GENERATION 4 (rotation - G0001 supprimee)
//*-------------------------------------------------------------------*
//GEN4     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250104 DONNEES DU JOUR 4 - JEUDI
20250104 TRANSACTION 008 - VENTE
20250104 TRANSACTION 009 - RETOUR
/*
//SYSUT2   DD DSN=FTEST.TP02.DAILY(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 7: LISTER APRES ROTATION (doit avoir G0002, G0003, G0004)
//*-------------------------------------------------------------------*
//LIST4    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(FTEST.TP02.DAILY) NAME
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 8: CONCATENER TOUTES LES GENERATIONS
//*-------------------------------------------------------------------*
//CONCAT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ALLIN    DD DSN=FTEST.TP02.DAILY,DISP=SHR
//ALLOUT   DD DSN=FTEST.TP02.CONCAT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(ALLIN) OUTFILE(ALLOUT)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 9: AFFICHER LE RESULTAT DE LA CONCATENATION
//*-------------------------------------------------------------------*
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT INDATASET(FTEST.TP02.CONCAT) CHARACTER
/*
//
