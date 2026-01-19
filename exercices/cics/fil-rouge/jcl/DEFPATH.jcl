//ROCHA19 JOB (ACCT),'DEF AIX REGION',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*================================================================*
//* JCL : DEFPATH - Definition AIX et PATH sur CODREG              *
//* AIX sur champ CODREG (offset 6, longueur 2)                    *
//* Utilise par : PRGSTAT (Exercice 19 - Statistiques par region)  *
//*================================================================*
//* Structure enregistrement CLIENT (80 octets) :                  *
//*   Position 1-6  : NUMCPT (cle primaire)                        *
//*   Position 7-8  : CODREG (cle alternative) <-- AIX             *
//*   Position 9-10 : NATCPT                                       *
//*   Position 11-20: NOM                                          *
//*   ...                                                          *
//*================================================================*
//*
//*----------------------------------------------------------------*
//* ETAPE 1 : Suppression AIX et PATH existants (si existent)      *
//*----------------------------------------------------------------*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE ROCHA.CICS.CLIENT.PATH -
         ALTERNATEINDEX
  SET MAXCC = 0
  DELETE ROCHA.CICS.CLIENT.AIX -
         ALTERNATEINDEX
  SET MAXCC = 0
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 2 : Definition de l'ALTERNATE INDEX (AIX)                *
//*           Cle alternative : CODREG (offset 6, longueur 2)      *
//*           NONUNIQUEKEY : plusieurs clients par region          *
//*----------------------------------------------------------------*
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX ( -
         NAME(ROCHA.CICS.CLIENT.AIX) -
         RELATE(ROCHA.CICS.CLIENT) -
         KEYS(2 6) -
         RECORDSIZE(14 200) -
         TRACKS(2 2) -
         VOLUMES(FDDBAS) -
         SHAREOPTIONS(2 3) -
         NONUNIQUEKEY -
         UPGRADE -
         ) -
         DATA ( -
         NAME(ROCHA.CICS.CLIENT.AIX.DATA) -
         ) -
         INDEX ( -
         NAME(ROCHA.CICS.CLIENT.AIX.INDEX) -
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
         INDATASET(ROCHA.CICS.CLIENT) -
         OUTDATASET(ROCHA.CICS.CLIENT.AIX)
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 4 : Definition du PATH                                   *
//*           Le PATH permet d'acceder au fichier de base          *
//*           via la cle alternative (CODREG)                      *
//*----------------------------------------------------------------*
//STEP4    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE PATH ( -
         NAME(ROCHA.CICS.CLIENT.PATH) -
         PATHENTRY(ROCHA.CICS.CLIENT.AIX) -
         )
/*
//*
//*----------------------------------------------------------------*
//* ETAPE 5 : Verification - Listcat des objets crees              *
//*----------------------------------------------------------------*
//STEP5    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(ROCHA.CICS.CLIENT) ALL
  LISTCAT ENTRIES(ROCHA.CICS.CLIENT.AIX) ALL
  LISTCAT ENTRIES(ROCHA.CICS.CLIENT.PATH) ALL
/*
//
