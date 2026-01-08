//DEFPATH  JOB (AIXPATH),MSGLEVEL=(1,1),REGION=4M,MSGCLASS=A,
//             CLASS=A,NOTIFY=&SYSUID
//*================================================================*
//* JCL : DEFPATH - Definition AIX et PATH pour FCLIENT            *
//* AIX sur champ NOMCPT (position 8, longueur 10)                 *
//* Utilise par : PRGPATH (Chapitre VIII - Exercice 14)            *
//*================================================================*
//*
//*----------------------------------------------------------------*
//* ETAPE 1 : Suppression AIX et PATH existants (si existent)      *
//*----------------------------------------------------------------*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.CICS.FCLIENT.PATH -
         ALTERNATEINDEX
  SET MAXCC = 0
  DELETE FTEST.CICS.FCLIENT.AIX -
         ALTERNATEINDEX
  SET MAXCC = 0
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 2 : Definition de l'ALTERNATE INDEX (AIX)                *
//*           Cle alternative : NOMCPT (position 7, longueur 10)   *
//*           NONUNIQUEKEY : plusieurs enreg. peuvent avoir        *
//*                          le meme nom                           *
//*----------------------------------------------------------------*
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX ( -
         NAME(FTEST.CICS.FCLIENT.AIX) -
         RELATE(FTEST.CICS.FCLIENT.KSDS) -
         KEYS(10 7) -
         RECORDSIZE(23 200) -
         TRACKS(2 2) -
         VOLUMES(FDDBAS) -
         SHAREOPTIONS(2 3) -
         NONUNIQUEKEY -
         UPGRADE -
         ) -
         DATA ( -
         NAME(FTEST.CICS.FCLIENT.AIX.DATA) -
         ) -
         INDEX ( -
         NAME(FTEST.CICS.FCLIENT.AIX.INDEX) -
         )
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 3 : Construction de l'AIX (BLDINDEX)                     *
//*           Lecture du fichier de base et creation des entrees   *
//*           dans l'index alternatif                              *
//*----------------------------------------------------------------*
//STEP3    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  BLDINDEX -
         INDATASET(FTEST.CICS.FCLIENT.KSDS) -
         OUTDATASET(FTEST.CICS.FCLIENT.AIX)
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 4 : Definition du PATH                                   *
//*           Le PATH permet d'acceder au fichier de base          *
//*           via la cle alternative (NOMCPT)                      *
//*----------------------------------------------------------------*
//STEP4    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE PATH ( -
         NAME(FTEST.CICS.FCLIENT.PATH) -
         PATHENTRY(FTEST.CICS.FCLIENT.AIX) -
         )
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 5 : Verification - Listcat des objets crees              *
//*----------------------------------------------------------------*
//STEP5    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(FTEST.CICS.FCLIENT.KSDS) ALL
  LISTCAT ENTRIES(FTEST.CICS.FCLIENT.AIX) ALL
  LISTCAT ENTRIES(FTEST.CICS.FCLIENT.PATH) ALL
/*
//
