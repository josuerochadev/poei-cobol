//FTEST02  JOB (ACCT),'DEFINE KSDS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  EXERCICE 2 : CREATION D'UN CLUSTER KSDS                          *
//*  Objectif : Definir un Key Sequenced Data Set                     *
//*********************************************************************
//*
//DEFKSDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Suppression si existe (ignorer erreur) */
  DELETE (FTEST.VSAM.KSDS) CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0

  /* Definition du cluster KSDS */
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.KSDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    INDEXED -
    RECORDSIZE(100 100) -
    KEYS(10 0) -
    FREESPACE(10 10) -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA ( -
    NAME(FTEST.VSAM.KSDS.DATA)) -
  INDEX ( -
    NAME(FTEST.VSAM.KSDS.INDEX))

  /* Verification */
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(FTEST.VSAM.KSDS) ALL
/*
//
