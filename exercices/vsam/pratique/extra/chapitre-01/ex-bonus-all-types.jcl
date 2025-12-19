//FTEST05  JOB (ACCT),'ALL VSAM TYPES',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  EXERCICE BONUS : CREATION DES 4 TYPES DE CLUSTERS                *
//*  Objectif : Creer ESDS, KSDS, RRDS et LDS en un seul job          *
//*********************************************************************
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Nettoyage prealable */
  DELETE (FTEST.VSAM.ESDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.KSDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.RRDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.LDS) CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /*-----------------------------------------------------------------*/
  /* ESDS - Entry Sequenced Data Set                                 */
  /*-----------------------------------------------------------------*/
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.ESDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NONINDEXED -
    RECORDSIZE(80 80) -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA (NAME(FTEST.VSAM.ESDS.DATA))

  IF LASTCC NE 0 THEN DO -
    SET MAXCC = 16 -
  END

  /*-----------------------------------------------------------------*/
  /* KSDS - Key Sequenced Data Set                                   */
  /*-----------------------------------------------------------------*/
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
  DATA (NAME(FTEST.VSAM.KSDS.DATA)) -
  INDEX (NAME(FTEST.VSAM.KSDS.INDEX))

  IF LASTCC NE 0 THEN DO -
    SET MAXCC = 16 -
  END

  /*-----------------------------------------------------------------*/
  /* RRDS - Relative Record Data Set                                 */
  /*-----------------------------------------------------------------*/
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.RRDS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NUMBERED -
    RECORDSIZE(50 50) -
    SHAREOPTIONS(1 3)) -
  DATA (NAME(FTEST.VSAM.RRDS.DATA))

  IF LASTCC NE 0 THEN DO -
    SET MAXCC = 16 -
  END

  /*-----------------------------------------------------------------*/
  /* LDS - Linear Data Set                                           */
  /*-----------------------------------------------------------------*/
  DEFINE CLUSTER ( -
    NAME(FTEST.VSAM.LDS) -
    TRACKS(2 1) -
    VOLUMES(ZASYS1) -
    LINEAR -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA (NAME(FTEST.VSAM.LDS.DATA))

  IF LASTCC NE 0 THEN DO -
    SET MAXCC = 16 -
  END
/*
//*
//LISTALL  EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Lister tous les clusters crees */
  LISTCAT LEVEL(FTEST.VSAM) ALL
/*
//
