//FTESTCLP JOB (ACCT),'CLEANUP CHAPITRE 02',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* NETTOYAGE : SUPPRESSION DES DATASETS DU CHAPITRE 02
//* ============================================================
//* A executer apres les exercices pour nettoyer
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* ============================================================
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.ESDS.AAAA
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.ESDS.CCCC
  DELETE FTEST.ESDS.DDDD
  SET MAXCC=0
/*
//
