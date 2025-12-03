//DEFVSAM  JOB (ACCT),'DEFINE VSAM',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Definition de fichiers VSAM avec IDCAMS
//*--------------------------------------------------------------------
//*
//*====================================================================
//* EXEMPLE 1 : KSDS (Key Sequenced Data Set) - Fichier indexe
//*====================================================================
//DEFKSDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.DATA.EMPLOYE.KSDS CLUSTER PURGE
  SET MAXCC = 0

  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.EMPLOYE.KSDS)               -
           INDEXED                                    -
           KEYS(6 0)                                  -
           RECORDSIZE(80 80)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (                                       -
           NAME(USER.DATA.EMPLOYE.KSDS.DATA)          -
           CISZ(4096)                                 -
           )                                          -
         INDEX (                                      -
           NAME(USER.DATA.EMPLOYE.KSDS.INDEX)         -
           )

  IF LASTCC = 0 THEN                                  -
    LISTCAT ENTRIES(USER.DATA.EMPLOYE.KSDS) ALL
/*
//*
//*====================================================================
//* EXEMPLE 2 : ESDS (Entry Sequenced Data Set) - Fichier sequentiel
//*====================================================================
//DEFESDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.DATA.EMPLOYE.ESDS CLUSTER PURGE
  SET MAXCC = 0

  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.EMPLOYE.ESDS)               -
           NONINDEXED                                 -
           RECORDSIZE(80 80)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (                                       -
           NAME(USER.DATA.EMPLOYE.ESDS.DATA)          -
           CISZ(4096)                                 -
           )
/*
//*
//*====================================================================
//* EXEMPLE 3 : RRDS (Relative Record Data Set) - Fichier relatif
//*====================================================================
//DEFRRDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.DATA.EMPLOYE.RRDS CLUSTER PURGE
  SET MAXCC = 0

  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.EMPLOYE.RRDS)               -
           NUMBERED                                   -
           RECORDSIZE(80 80)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (                                       -
           NAME(USER.DATA.EMPLOYE.RRDS.DATA)          -
           CISZ(4096)                                 -
           )

  IF LASTCC = 0 THEN                                  -
    LISTCAT ENTRIES(USER.DATA.EMPLOYE.RRDS) ALL
/*
