//C08RELEV JOB (ACCT),'EDITION RELEVE BANCAIRE',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Edition Releve Bancaire - Exercice Synthese Chapitre 08
//*
//* Ce JCL execute :
//*   1. Definition des fichiers VSAM (IDCAMS)
//*   2. Chargement du fichier BUFFER
//*   3. Dispatch vers fichiers AGENCE, CLIENT, RIB, MVTC
//*--------------------------------------------------------------------
//*
//*====================================================================
//* ETAPE 1 : DEFINITION DES FICHIERS VSAM
//*====================================================================
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.DATA.AGENCE.KSDS CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.AGENCE.KSDS)                -
           INDEXED                                    -
           KEYS(7 0)                                  -
           RECORDSIZE(37 37)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (NAME(USER.DATA.AGENCE.KSDS.DATA))      -
         INDEX (NAME(USER.DATA.AGENCE.KSDS.INDEX))

  DELETE USER.DATA.CLIENT.KSDS CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.CLIENT.KSDS)                -
           INDEXED                                    -
           KEYS(5 0)                                  -
           RECORDSIZE(52 52)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (NAME(USER.DATA.CLIENT.KSDS.DATA))      -
         INDEX (NAME(USER.DATA.CLIENT.KSDS.INDEX))

  DELETE USER.DATA.RIB.KSDS CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.RIB.KSDS)                   -
           INDEXED                                    -
           KEYS(5 0)                                  -
           RECORDSIZE(48 48)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (NAME(USER.DATA.RIB.KSDS.DATA))         -
         INDEX (NAME(USER.DATA.RIB.KSDS.INDEX))

  DELETE USER.DATA.MVTC.ESDS CLUSTER PURGE
  SET MAXCC = 0
  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.MVTC.ESDS)                  -
           NONINDEXED                                 -
           RECORDSIZE(25 25)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (NAME(USER.DATA.MVTC.ESDS.DATA))
/*
//*
//*====================================================================
//* ETAPE 2 : CHARGEMENT FICHIER BUFFER (DONNEES DE TEST)
//*====================================================================
//STEP02   EXEC PGM=C08RELINIT,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//BUFFER   DD DSN=USER.DATA.BUFFER.SEQ,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(5,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 3 : EXECUTION PROGRAMME RELEVE BANCAIRE
//*====================================================================
//STEP03   EXEC PGM=C08RELEVE,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*--- Fichier source
//BUFFER   DD DSN=USER.DATA.BUFFER.SEQ,DISP=SHR
//*--- Fichiers de sortie
//AGENCE   DD DSN=USER.DATA.AGENCE.KSDS,DISP=SHR
//CLIENT   DD DSN=USER.DATA.CLIENT.KSDS,DISP=SHR
//RIB      DD DSN=USER.DATA.RIB.KSDS,DISP=SHR
//MVTC     DD DSN=USER.DATA.MVTC.ESDS,DISP=SHR
//*--- Sorties
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 4 : VERIFICATION - LISTCAT DES FICHIERS CREES
//*====================================================================
//STEP04   EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(USER.DATA.AGENCE.KSDS) ALL
  LISTCAT ENTRIES(USER.DATA.CLIENT.KSDS) ALL
  LISTCAT ENTRIES(USER.DATA.RIB.KSDS) ALL
  LISTCAT ENTRIES(USER.DATA.MVTC.ESDS) ALL
/*
//*
//*====================================================================
//* ETAPE 5 : IMPRESSION DU CONTENU DES FICHIERS
//*====================================================================
//STEP05   EXEC PGM=IDCAMS,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//AGENCE   DD DSN=USER.DATA.AGENCE.KSDS,DISP=SHR
//CLIENT   DD DSN=USER.DATA.CLIENT.KSDS,DISP=SHR
//RIB      DD DSN=USER.DATA.RIB.KSDS,DISP=SHR
//MVTC     DD DSN=USER.DATA.MVTC.ESDS,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(AGENCE) CHAR
  PRINT INFILE(CLIENT) CHAR
  PRINT INFILE(RIB) CHAR
  PRINT INFILE(MVTC) CHAR
/*
