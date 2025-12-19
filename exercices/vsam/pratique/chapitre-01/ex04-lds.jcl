//FTEST04  JOB (ACCT),'DEFINE LDS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  EXERCICE 4 : CREATION D'UN CLUSTER LDS                           *
//*  Objectif : Definir un Linear Data Set                            *
//*********************************************************************
//*
//DEFLDS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Suppression si existe (ignorer erreur) */
  DELETE (FTEST.VSAM.LDS) CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0

  /* Definition du cluster LDS */
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.LDS) -
    TRACKS(2 1) -
    VOLUMES(ZASYS1) -
    LINEAR -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA ( -
    NAME(FTEST.VSAM.LDS.DATA))

  /* Verification */
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(FTEST.VSAM.LDS) ALL
/*
//
