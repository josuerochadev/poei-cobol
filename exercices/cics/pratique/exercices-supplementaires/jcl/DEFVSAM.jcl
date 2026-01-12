//DEFVSAM  JOB (ACCT),'DEF VSAM BANQUE',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID
//*********************************************************************
//* Définition fichiers VSAM pour application bancaire CICS
//*********************************************************************
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.CICS.CLIENT CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0
  DELETE USER.CICS.COMPTE CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//*
//DEFCLI   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(USER.CICS.CLIENT)              -
           INDEXED                             -
           KEYS(6 0)                           -
           RECORDSIZE(126 126)                 -
           SHAREOPTIONS(2 3)                   -
           CYLINDERS(1 1)                      -
         )                                     -
         DATA (NAME(USER.CICS.CLIENT.DATA))    -
         INDEX (NAME(USER.CICS.CLIENT.INDEX))
/*
//*
//DEFCPT   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(USER.CICS.COMPTE)              -
           INDEXED                             -
           KEYS(11 0)                          -
           RECORDSIZE(56 56)                   -
           SHAREOPTIONS(2 3)                   -
           CYLINDERS(1 1)                      -
         )                                     -
         DATA (NAME(USER.CICS.COMPTE.DATA))    -
         INDEX (NAME(USER.CICS.COMPTE.INDEX))
/*
//
