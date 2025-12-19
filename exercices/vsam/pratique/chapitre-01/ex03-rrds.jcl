//FTEST03  JOB (ACCT),'DEFINE RRDS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  EXERCICE 3 : CREATION D'UN CLUSTER RRDS                          *
//*  Objectif : Definir un Relative Record Data Set                   *
//*********************************************************************
//*
//DEFRRDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Suppression si existe (ignorer erreur) */
  DELETE (FTEST.VSAM.RRDS) CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0

  /* Definition du cluster RRDS */
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.RRDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NUMBERED -
    RECORDSIZE(50 50) -
    SHAREOPTIONS(1 3) -
    NOREUSE) -
  DATA ( -
    NAME(FTEST.VSAM.RRDS.DATA))

  /* Verification */
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(FTEST.VSAM.RRDS) ALL
/*
//
