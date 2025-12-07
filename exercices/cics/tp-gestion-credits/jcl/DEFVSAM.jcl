//DEFVSAM  JOB (ACCT),'DEFINE VSAM FILES',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : DEFVSAM.jcl
//* Description : Définition des fichiers VSAM pour le TP Crédits
//* Fichiers créés :
//*   - USER.CICS.EMPLOYE  (KSDS - Employés)
//*   - USER.CICS.CREDEMP  (KSDS - Crédits)
//*********************************************************************
//*
//* ÉTAPE 1 : Suppression des fichiers existants (si présents)
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.CICS.EMPLOYE CLUSTER PURGE
  SET MAXCC = 0
  DELETE USER.CICS.CREDEMP CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//* ÉTAPE 2 : Définition du fichier EMPLOYE
//*
//DEFEMPL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER                                  -
         (NAME(USER.CICS.EMPLOYE)                 -
          INDEXED                                 -
          RECORDSIZE(52 52)                       -
          KEYS(6 0)                               -
          SHAREOPTIONS(2 3)                       -
          CYLINDERS(1 1))                         -
         DATA                                     -
         (NAME(USER.CICS.EMPLOYE.DATA))           -
         INDEX                                    -
         (NAME(USER.CICS.EMPLOYE.INDEX))
/*
//*
//* ÉTAPE 3 : Définition du fichier CRE-EMP
//*
//DEFCRED  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER                                  -
         (NAME(USER.CICS.CREDEMP)                 -
          INDEXED                                 -
          RECORDSIZE(40 40)                       -
          KEYS(6 0)                               -
          SHAREOPTIONS(2 3)                       -
          CYLINDERS(1 1))                         -
         DATA                                     -
         (NAME(USER.CICS.CREDEMP.DATA))           -
         INDEX                                    -
         (NAME(USER.CICS.CREDEMP.INDEX))
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
