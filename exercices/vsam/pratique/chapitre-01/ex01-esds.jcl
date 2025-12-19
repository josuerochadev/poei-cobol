//FTEST01  JOB (ACCT),'DEFINE ESDS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  EXERCICE 1 : CREATION D'UN CLUSTER ESDS                          *
//*  Objectif : Definir un Entry Sequenced Data Set                   *
//*********************************************************************
//*
//DEFESDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Suppression si existe (ignorer erreur) */
  DELETE (FTEST.VSAM.ESDS) CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0

  /* Definition du cluster ESDS */
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.ESDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NONINDEXED -
    RECORDSIZE(80 80) -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA ( -
    NAME(FTEST.VSAM.ESDS.DATA))

  /* Verification */
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(FTEST.VSAM.ESDS) ALL
/*
//
