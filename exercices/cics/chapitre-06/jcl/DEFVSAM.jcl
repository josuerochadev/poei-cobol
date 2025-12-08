//DEFVSAM  JOB (ACCT),'DEFINE VSAM FILES',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : DEFVSAM.jcl
//* Description : Definition du fichier VSAM EMPLOYE pour TP CICS
//* Fichier cree :
//*   - FTEST.CICS.EMPLOYE (KSDS - Employes, LRECL=80)
//*********************************************************************
//*
//* ETAPE 1 : Suppression du fichier existant (si present)
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.CICS.EMPLOYE CLUSTER PURGE
  SET MAXCC = 0
/*
//*
//* ETAPE 2 : Definition du fichier EMPLOYE
//*
//DEFEMPL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER                                  -
         (NAME(FTEST.CICS.EMPLOYE)                -
          INDEXED                                 -
          RECORDSIZE(80 80)                       -
          KEYS(6 0)                               -
          VOLUMES(PUB001)                         -
          SHAREOPTIONS(2 3)                       -
          CYLINDERS(1 1))                         -
         DATA                                     -
         (NAME(FTEST.CICS.EMPLOYE.DATA))          -
         INDEX                                    -
         (NAME(FTEST.CICS.EMPLOYE.INDEX))
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
