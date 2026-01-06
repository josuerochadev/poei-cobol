//DEFVSAM  JOB (ACCT),'DEF VSAM CH09',CLASS=A,MSGCLASS=X
//*****************************************************************
//* JCL : Definition fichier VSAM EMPLOYE avec Alternate Index    *
//* Chapitre IX - Exercices pratiques CICS                        *
//*****************************************************************
//*
//*------- SUPPRESSION SI EXISTE -----------------------------------
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE hlq.EMPLOYE.KSDS CLUSTER PURGE
  DELETE hlq.EMPLOYE.AIX.NOM ALTERNATEINDEX PURGE
  DELETE hlq.EMPLOYE.PATH.NOM PATH PURGE
  SET MAXCC=0
/*
//*
//*------- DEFINITION CLUSTER KSDS ---------------------------------
//DEFCLUS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                                  -
           NAME(hlq.EMPLOYE.KSDS)                   -
           INDEXED                                  -
           KEYS(6 0)                                -
           RECORDSIZE(80 80)                        -
           SHAREOPTIONS(2 3)                        -
           CYLINDERS(1 1)                           -
         )                                          -
         DATA (                                     -
           NAME(hlq.EMPLOYE.KSDS.DATA)              -
         )                                          -
         INDEX (                                    -
           NAME(hlq.EMPLOYE.KSDS.INDEX)             -
         )
/*
//*
//*------- DEFINITION ALTERNATE INDEX SUR NOM ----------------------
//DEFAIX   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX (                           -
           NAME(hlq.EMPLOYE.AIX.NOM)                -
           RELATE(hlq.EMPLOYE.KSDS)                 -
           KEYS(30 6)                               -
           RECORDSIZE(42 42)                        -
           NONUNIQUEKEY                             -
           UPGRADE                                  -
           SHAREOPTIONS(2 3)                        -
           CYLINDERS(1 1)                           -
         )                                          -
         DATA (                                     -
           NAME(hlq.EMPLOYE.AIX.NOM.DATA)           -
         )                                          -
         INDEX (                                    -
           NAME(hlq.EMPLOYE.AIX.NOM.INDEX)          -
         )
/*
//*
//*------- DEFINITION PATH -----------------------------------------
//DEFPATH  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE PATH (                                     -
           NAME(hlq.EMPLOYE.PATH.NOM)               -
           PATHENTRY(hlq.EMPLOYE.AIX.NOM)           -
         )
/*
//*
//*------- CHARGEMENT DONNEES INITIALES ----------------------------
//LOAD     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001DUPONT JEAN                     COMPTA    003500000N
EMP002MARTIN MARIE                    RH        004200000O
EMP003DURAND PIERRE                   IT        005500000N
EMP004PETIT SOPHIE                    COMPTA    003200000N
EMP005MOREAU PAUL                     DIRECTION 008000000O
EMP006LEROY ANNE                      IT        004800000N
EMP007ROUX PHILIPPE                   RH        003900000N
EMP008FOURNIER LUC                    COMPTA    003600000O
EMP009GIRARD CLAIRE                   IT        005100000N
EMP010BONNET MARC                     DIRECTION 007500000N
/*
//VSAM     DD DSN=hlq.EMPLOYE.KSDS,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(VSAM)
/*
//*
//*------- CONSTRUCTION ALTERNATE INDEX ----------------------------
//BLDAIX   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  BLDINDEX                                          -
           INDATASET(hlq.EMPLOYE.KSDS)              -
           OUTDATASET(hlq.EMPLOYE.AIX.NOM)
/*
//*
//*------- LISTCAT POUR VERIFICATION -------------------------------
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(hlq.EMPLOYE.*) ALL
/*
//
