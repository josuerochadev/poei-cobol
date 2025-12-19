//TESTGDG  JOB (ACCT),'TP GDG FORMATEUR',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  TP GDG - EXERCICE DU FORMATEUR                                   *
//*  Gestion TESTGDG.COMPTE.MENSUEL avec LIMIT(4)                     *
//*********************************************************************
//*
//*  Structure enregistrement (80 octets):
//*  Pos 01-06 : Code Client (cle)
//*  Pos 07-26 : Nom Client
//*  Pos 27-27 : Etat operation (D=Debit, C=Credit)
//*  Pos 28-37 : Solde (10 chiffres)
//*  Pos 38-45 : Date operation (AAAAMMJJ)
//*  Pos 46-80 : Reserve
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (TESTGDG.COMPTE.MENSUEL) GDG FORCE
  DELETE (TESTGDG.COMPTE.ESDS) CLUSTER PURGE
  DELETE (TESTGDG.COMPTE.KSDS) CLUSTER PURGE
  DELETE (TESTGDG.COMPTE.CONCAT) NONVSAM PURGE
  DELETE (TESTGDG.COMPTE.BACKUP) NONVSAM PURGE
  SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 1: DEFINITION DU GDG AVEC LIMIT(4)
//*-------------------------------------------------------------------*
//DEFGDG   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG ( -
    NAME(TESTGDG.COMPTE.MENSUEL) -
    LIMIT(4) -
    NOEMPTY -
    SCRATCH)

  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(TESTGDG.COMPTE.MENSUEL) ALL
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2: CREATION ESDS POUR CHARGEMENT INITIAL
//*-------------------------------------------------------------------*
//DEFESDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER ( -
    NAME(TESTGDG.COMPTE.ESDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    NONINDEXED -
    RECORDSIZE(80 80) -
    SHAREOPTIONS(1 3)) -
  DATA (NAME(TESTGDG.COMPTE.ESDS.DATA))
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 3: CHARGEMENT DONNEES DANS ESDS
//* Format: CodeClt|NomClient          |E|Solde     |Date    |Reserve
//*-------------------------------------------------------------------*
//LOADESDS EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INDATA   DD *
CLT001DUPONT JEAN          C0000150000202501010000000000000000000000000
CLT002MARTIN MARIE         D0000025000202501020000000000000000000000000
CLT003BERNARD PIERRE       C0000080000202501030000000000000000000000000
CLT004DURAND SOPHIE        D0000015000202501040000000000000000000000000
CLT005MOREAU PAUL          C0000200000202501050000000000000000000000000
/*
//SYSIN    DD *
  REPRO INFILE(INDATA) OUTDATASET(TESTGDG.COMPTE.ESDS)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 4: CREATION KSDS POUR CONVERSION
//*-------------------------------------------------------------------*
//DEFKSDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER ( -
    NAME(TESTGDG.COMPTE.KSDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    INDEXED -
    RECORDSIZE(80 80) -
    KEYS(6 0) -
    FREESPACE(15 10) -
    SHAREOPTIONS(2 3)) -
  DATA (NAME(TESTGDG.COMPTE.KSDS.DATA)) -
  INDEX (NAME(TESTGDG.COMPTE.KSDS.INDEX))
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 5: REPRO ESDS VERS KSDS
//*-------------------------------------------------------------------*
//REPRO    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO INDATASET(TESTGDG.COMPTE.ESDS) -
    OUTDATASET(TESTGDG.COMPTE.KSDS)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 6: CREATION GENERATION 1 (Janvier)
//*-------------------------------------------------------------------*
//GEN1     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250131 CLOTURE MENSUELLE JANVIER 2025
CLT001 DUPONT JEAN           SOLDE: +150000
CLT002 MARTIN MARIE          SOLDE: -025000
CLT003 BERNARD PIERRE        SOLDE: +080000
/*
//SYSUT2   DD DSN=TESTGDG.COMPTE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 7: CREATION GENERATION 2 (Fevrier)
//*-------------------------------------------------------------------*
//GEN2     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250228 CLOTURE MENSUELLE FEVRIER 2025
CLT001 DUPONT JEAN           SOLDE: +175000
CLT002 MARTIN MARIE          SOLDE: -018000
CLT004 DURAND SOPHIE         SOLDE: -012000
/*
//SYSUT2   DD DSN=TESTGDG.COMPTE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 8: CREATION GENERATION 3 (Mars)
//*-------------------------------------------------------------------*
//GEN3     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250331 CLOTURE MENSUELLE MARS 2025
CLT001 DUPONT JEAN           SOLDE: +180000
CLT003 BERNARD PIERRE        SOLDE: +095000
CLT005 MOREAU PAUL           SOLDE: +225000
/*
//SYSUT2   DD DSN=TESTGDG.COMPTE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 9: CREATION GENERATION 4 (Avril)
//*-------------------------------------------------------------------*
//GEN4     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250430 CLOTURE MENSUELLE AVRIL 2025
CLT002 MARTIN MARIE          SOLDE: -010000
CLT004 DURAND SOPHIE         SOLDE: +005000
CLT005 MOREAU PAUL           SOLDE: +250000
/*
//SYSUT2   DD DSN=TESTGDG.COMPTE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 10: LISTER 4 GENERATIONS (G0001-G0004)
//*-------------------------------------------------------------------*
//LIST4    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(TESTGDG.COMPTE.MENSUEL) NAME
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 11: CREATION GENERATION 5 (Mai) - ROTATION AUTOMATIQUE
//* Observation: G0001 sera supprimee (LIMIT=4)
//*-------------------------------------------------------------------*
//GEN5     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
20250531 CLOTURE MENSUELLE MAI 2025
CLT001 DUPONT JEAN           SOLDE: +200000
CLT003 BERNARD PIERRE        SOLDE: +100000
CLT005 MOREAU PAUL           SOLDE: +275000
/*
//SYSUT2   DD DSN=TESTGDG.COMPTE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
//*
//*-------------------------------------------------------------------*
//* ETAPE 12: LISTER APRES ROTATION (G0002-G0005)
//* G0001 (Janvier) a ete supprimee par la rotation
//*-------------------------------------------------------------------*
//LIST5    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(TESTGDG.COMPTE.MENSUEL) NAME
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 13: CONCATENER TOUTES LES GENERATIONS
//*-------------------------------------------------------------------*
//CONCAT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ALLIN    DD DSN=TESTGDG.COMPTE.MENSUEL,DISP=SHR
//ALLOUT   DD DSN=TESTGDG.COMPTE.CONCAT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(ALLIN) OUTFILE(ALLOUT)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 14: AFFICHER CONCATENATION
//*-------------------------------------------------------------------*
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT INDATASET(TESTGDG.COMPTE.CONCAT) CHARACTER
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 15: BACKUP DU KSDS
//*-------------------------------------------------------------------*
//BACKUP   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//BACKFILE DD DSN=TESTGDG.COMPTE.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
  REPRO INDATASET(TESTGDG.COMPTE.KSDS) -
    OUTFILE(BACKFILE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 16: LISTCAT FINAL - TOUS LES OBJETS
//*-------------------------------------------------------------------*
//FINAL    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(TESTGDG.COMPTE) NAME
/*
//
