//FTETP03  JOB (ACCT),'TP03 WORKFLOW',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  TP 03 : WORKFLOW BATCH COMPLET                                   *
//*  Simulation d'un traitement batch quotidien                       *
//*********************************************************************
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.TP03.MASTER) CLUSTER PURGE
  DELETE (FTEST.TP03.TRANS) CLUSTER PURGE
  DELETE (FTEST.TP03.EXPORT) NONVSAM PURGE
  DELETE (FTEST.TP03.ARCHIVE) GDG FORCE
  SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 1: DEFINITION DES FICHIERS
//*-------------------------------------------------------------------*
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Fichier MASTER - Comptes clients */
  DEFINE CLUSTER ( -
    NAME(FTEST.TP03.MASTER) -
    TRACKS(2 1) -
    VOLUMES(ZASYS1) -
    INDEXED -
    RECORDSIZE(80 80) -
    KEYS(10 0) -
    FREESPACE(20 10) -
    SHAREOPTIONS(2 3)) -
  DATA (NAME(FTEST.TP03.MASTER.DATA)) -
  INDEX (NAME(FTEST.TP03.MASTER.INDEX))

  /* Fichier TRANSACTIONS - Mouvements du jour */
  DEFINE CLUSTER ( -
    NAME(FTEST.TP03.TRANS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    NONINDEXED -
    RECORDSIZE(60 60) -
    SHAREOPTIONS(1 3)) -
  DATA (NAME(FTEST.TP03.TRANS.DATA))

  /* GDG pour archivage */
  DEFINE GDG ( -
    NAME(FTEST.TP03.ARCHIVE) -
    LIMIT(5) -
    NOEMPTY -
    SCRATCH)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2: CHARGEMENT INITIAL DU MASTER
//* Structure: NumCompte(10) | Nom(30) | Solde(10) | DateMAJ(8) | Reserve(22)
//*-------------------------------------------------------------------*
//LOADMSTR EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INMASTER DD *
0000000001DUPONT JEAN                   0000100000202501010000000000000000000000
0000000002MARTIN MARIE                  0000050000202501010000000000000000000000
0000000003BERNARD PIERRE                0000075000202501010000000000000000000000
0000000004DURAND SOPHIE                 0000025000202501010000000000000000000000
0000000005MOREAU PAUL                   0000150000202501010000000000000000000000
/*
//SYSIN    DD *
  REPRO INFILE(INMASTER) OUTDATASET(FTEST.TP03.MASTER)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 3: VERIFY DES FICHIERS (bonne pratique avant traitement)
//*-------------------------------------------------------------------*
//VERIFY   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  VERIFY DATASET(FTEST.TP03.MASTER)
  IF LASTCC <= 4 THEN SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 4: CHARGEMENT DES TRANSACTIONS DU JOUR
//* Structure: NumCompte(10) | Type(1) | Montant(10) | Date(8) | Libelle(31)
//*-------------------------------------------------------------------*
//LOADTRAN EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INTRANS  DD *
0000000001C0000005000202501150000VERSEMENT CHEQUE
0000000002D0000010000202501150000PRELEVEMENT EDF
0000000003C0000015000202501150000VIREMENT RECU
0000000001D0000002500202501150000RETRAIT DAB
0000000005C0000025000202501150000DEPOT ESPECES
/*
//SYSIN    DD *
  REPRO INFILE(INTRANS) OUTDATASET(FTEST.TP03.TRANS)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 5: AFFICHER L'ETAT AVANT TRAITEMENT
//*-------------------------------------------------------------------*
//BEFORE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Etat du fichier MASTER avant mise a jour */
  PRINT INDATASET(FTEST.TP03.MASTER) CHARACTER
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 6: STATISTIQUES LISTCAT
//*-------------------------------------------------------------------*
//STATS    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(FTEST.TP03.MASTER) ALL
  LISTCAT ENTRIES(FTEST.TP03.TRANS) ALL
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 7: EXPORT SAUVEGARDE DU MASTER
//*-------------------------------------------------------------------*
//EXPORT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//BACKUP   DD DSN=FTEST.TP03.EXPORT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,1)),
//            DCB=(RECFM=VB,LRECL=32760,BLKSIZE=0)
//SYSIN    DD *
  EXPORT FTEST.TP03.MASTER -
    OUTFILE(BACKUP) -
    TEMPORARY
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 8: ARCHIVAGE DANS GDG
//*-------------------------------------------------------------------*
//ARCHIVE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ARCOUT   DD DSN=FTEST.TP03.ARCHIVE(+1),
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
  REPRO INDATASET(FTEST.TP03.MASTER) -
    OUTFILE(ARCOUT)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 9: RAPPORT FINAL
//*-------------------------------------------------------------------*
//REPORT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Rapport des transactions du jour */
  PRINT INDATASET(FTEST.TP03.TRANS) CHARACTER

  /* Liste des objets crees */
  LISTCAT LEVEL(FTEST.TP03) NAME
/*
//
