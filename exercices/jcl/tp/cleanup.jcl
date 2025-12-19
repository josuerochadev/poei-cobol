//FTESTCLP JOB (ACCT),'CLEANUP CHAPITRE 05',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* NETTOYAGE : SUPPRESSION DES DATASETS DU CHAPITRE 05
//* ============================================================
//* A executer apres les travaux pratiques pour nettoyer
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* ============================================================
//*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
* ------------------------------------------------
* TP 01 : DATASET SEQUENTIEL
* ------------------------------------------------
  DELETE FTEST.DATA.SEQFILE

* ------------------------------------------------
* TP 02 : DATASET PARTITIONNE
* ------------------------------------------------
  DELETE FTEST.TSOJCL.LIBTEST

* ------------------------------------------------
* TP 03 : CONCATENATION
* ------------------------------------------------
  DELETE FTEST.ESDS.AAAA
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.ESDS.CCCC

* ------------------------------------------------
* TP 04 : UTILITAIRES
* ------------------------------------------------
  DELETE FTEST.TP04.SOURCE
  DELETE FTEST.TP04.COPIE
  DELETE FTEST.TP04.PDS1
  DELETE FTEST.TP04.PDS2
  DELETE FTEST.TP04.VSAM CLUSTER PURGE

* ------------------------------------------------
* TP 05 : SORT
* ------------------------------------------------
  DELETE FTEST.TP05.INPUT
  DELETE FTEST.TP05.TRIENOM
  DELETE FTEST.TP05.TRIMULTI
  DELETE FTEST.TP05.PARIS
  DELETE FTEST.TP05.PROVINCE
  DELETE FTEST.TP05.EXTRAIT

* ------------------------------------------------
* IGNORER ERREURS SI FICHIERS INEXISTANTS
* ------------------------------------------------
  SET MAXCC = 0
/*
//
