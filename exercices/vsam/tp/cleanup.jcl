//FTECLNTP JOB (ACCT),'CLEANUP TP',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  NETTOYAGE : SUPPRESSION DE TOUS LES OBJETS TP                    *
//*********************************************************************
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /*-----------------------------------------------------------------*/
  /* TP 01 - CLIENTS                                                 */
  /*-----------------------------------------------------------------*/
  DELETE (TESTGDG.TP01.PATH.VILLE) PATH PURGE
  DELETE (TESTGDG.TP01.AIX.VILLE) ALTERNATEINDEX PURGE
  DELETE (TESTGDG.TP01.PATH.SOLDE) PATH PURGE
  DELETE (TESTGDG.TP01.AIX.SOLDE) ALTERNATEINDEX PURGE
  DELETE (TESTGDG.TP01.CLIENTS) CLUSTER PURGE
  DELETE (TESTGDG.TP01.BACKUP) NONVSAM PURGE

  /*-----------------------------------------------------------------*/
  /* TP GDG FORMATEUR - COMPTE.MENSUEL                               */
  /*-----------------------------------------------------------------*/
  DELETE (TESTGDG.COMPTE.MENSUEL) GDG FORCE
  DELETE (TESTGDG.COMPTE.ESDS) CLUSTER PURGE
  DELETE (TESTGDG.COMPTE.KSDS) CLUSTER PURGE
  DELETE (TESTGDG.COMPTE.CONCAT) NONVSAM PURGE
  DELETE (TESTGDG.COMPTE.BACKUP) NONVSAM PURGE

  /*-----------------------------------------------------------------*/
  /* TP 03 - WORKFLOW                                                */
  /*-----------------------------------------------------------------*/
  DELETE (TESTGDG.TP03.MASTER) CLUSTER PURGE
  DELETE (TESTGDG.TP03.TRANS) CLUSTER PURGE
  DELETE (TESTGDG.TP03.EXPORT) NONVSAM PURGE
  DELETE (TESTGDG.TP03.ARCHIVE) GDG FORCE

  /* Ignorer les erreurs */
  SET MAXCC = 0
/*
//
