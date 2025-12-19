//FTESTCLP JOB (ACCT),'CLEANUP CHAPITRE 04',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* NETTOYAGE : SUPPRESSION DES DATASETS DU CHAPITRE 04
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
* ------------------------------------------------
* EXERCICE 1 : IEFBR14 ET IEBGENER
* ------------------------------------------------
  DELETE FTEST.UTIL.DATA

* ------------------------------------------------
* EXERCICE 2 : IEBCOPY PDS
* ------------------------------------------------
  DELETE FTEST.SOURCE.PDS
  DELETE FTEST.BACKUP.PDS
  DELETE FTEST.SELECT.PDS

* ------------------------------------------------
* EXERCICE 3 : SORT FILTRAGE
* ------------------------------------------------
  DELETE FTEST.UTIL.TRIENOM
  DELETE FTEST.UTIL.TRIECP
  DELETE FTEST.UTIL.PARIS
  DELETE FTEST.UTIL.EXTRAIT

* ------------------------------------------------
* EXERCICE 4 : IDCAMS VSAM
* ------------------------------------------------
  DELETE FTEST.VSAM.CLIENT CLUSTER PURGE

* ------------------------------------------------
* EXERCICE BONUS : WORKFLOW COMPLET
* ------------------------------------------------
  DELETE FTEST.BONUS.INPUT
  DELETE FTEST.BONUS.SORTED
  DELETE FTEST.BONUS.PARIS
  DELETE FTEST.BONUS.AUTRES
  DELETE FTEST.BONUS.VSAM CLUSTER PURGE

* ------------------------------------------------
* IGNORER ERREURS SI FICHIERS INEXISTANTS
* ------------------------------------------------
  SET MAXCC = 0
/*
//
