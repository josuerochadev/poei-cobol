//FCLEAN1  JOB (ACCT),'CLEANUP CHAP 01',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  NETTOYAGE : SUPPRESSION DES CLUSTERS CHAPITRE 01                 *
//*********************************************************************
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Suppression de tous les clusters du chapitre 01 */
  DELETE (FTEST.VSAM.ESDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.KSDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.RRDS) CLUSTER PURGE
  DELETE (FTEST.VSAM.LDS) CLUSTER PURGE

  /* Ignorer les erreurs (fichiers deja supprimes) */
  SET MAXCC = 0
/*
//
