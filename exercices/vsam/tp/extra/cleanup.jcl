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
  DELETE (FTEST.TP01.PATH.VILLE) PATH PURGE
  DELETE (FTEST.TP01.AIX.VILLE) ALTERNATEINDEX PURGE
  DELETE (FTEST.TP01.PATH.SOLDE) PATH PURGE
  DELETE (FTEST.TP01.AIX.SOLDE) ALTERNATEINDEX PURGE
  DELETE (FTEST.TP01.CLIENTS) CLUSTER PURGE
  DELETE (FTEST.TP01.BACKUP) NONVSAM PURGE

  /*-----------------------------------------------------------------*/
  /* TP 02 - GDG                                                     */
  /*-----------------------------------------------------------------*/
  DELETE (FTEST.TP02.DAILY) GDG FORCE
  DELETE (FTEST.TP02.CONCAT) NONVSAM PURGE

  /*-----------------------------------------------------------------*/
  /* TP 03 - WORKFLOW                                                */
  /*-----------------------------------------------------------------*/
  DELETE (FTEST.TP03.MASTER) CLUSTER PURGE
  DELETE (FTEST.TP03.TRANS) CLUSTER PURGE
  DELETE (FTEST.TP03.EXPORT) NONVSAM PURGE
  DELETE (FTEST.TP03.ARCHIVE) GDG FORCE

  /* Ignorer les erreurs */
  SET MAXCC = 0
/*
//
