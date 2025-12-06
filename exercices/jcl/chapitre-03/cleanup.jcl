//FTESTCLP JOB (ACCT),'CLEANUP CHAPITRE 03',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* NETTOYAGE : SUPPRESSION DES DATASETS DU CHAPITRE 03
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
  DELETE FTEST.TRI.SEQ
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.TRI.BBBB
  DELETE FTEST.ESDS.CCCC
  DELETE FTEST.TRI.CCCC
  SET MAXCC=0
/*
//
