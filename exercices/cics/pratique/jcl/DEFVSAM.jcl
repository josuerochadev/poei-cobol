//DEFVSAM  JOB (ACCT),'DEFINE VSAM FILES',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : DEFVSAM.jcl
//* Description : Definition des fichiers VSAM pour TP CICS
//* Fichiers crees :
//*   - FTEST.CICS.EMPLOYE (KSDS - Employes, LRECL=80)
//*   - FTEST.CICS.CREDEMP (KSDS - Credits employes, LRECL=80)
//*********************************************************************
//*
//*===================================================================*
//* ETAPE 1 : Suppression des fichiers existants
//*===================================================================*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.CICS.EMPLOYE CLUSTER PURGE
  SET MAXCC = 0
  DELETE FTEST.CICS.CREDEMP CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//*===================================================================*
//* ETAPE 2 : Definition du fichier EMPLOYE
//*===================================================================*
//DEFEMPL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER                                  -
         (NAME(FTEST.CICS.EMPLOYE)                -
          INDEXED                                 -
          RECORDSIZE(80 80)                       -
          KEYS(6 0)                               -
          VOLUMES(FDDBAS)                         -
          SHAREOPTIONS(2 3)                       -
          CYLINDERS(1 1))                         -
         DATA                                     -
         (NAME(FTEST.CICS.EMPLOYE.DATA))          -
         INDEX                                    -
         (NAME(FTEST.CICS.EMPLOYE.INDEX))
/*
//*
//*===================================================================*
//* ETAPE 3 : Definition du fichier CRE-EMP (Credits)
//*===================================================================*
//DEFCRED  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER                                  -
         (NAME(FTEST.CICS.CREDEMP)                -
          INDEXED                                 -
          RECORDSIZE(80 80)                       -
          KEYS(6 0)                               -
          VOLUMES(FDDBAS)                         -
          SHAREOPTIONS(2 3)                       -
          CYLINDERS(1 1))                         -
         DATA                                     -
         (NAME(FTEST.CICS.CREDEMP.DATA))          -
         INDEX                                    -
         (NAME(FTEST.CICS.CREDEMP.INDEX))
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
