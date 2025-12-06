//FTESTEX2 JOB (ACCT),'EX02 COPIE IEBGENER',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 2 : COPIE DATASET AVEC IEBGENER
//* ============================================================
//* Objectif : Copier FTEST.ESDS.AAAA vers FTEST.ESDS.BBBB
//* Utilise DCB=*.SYSUT1 pour copier les caracteristiques
//*
//* Prerequis : Executer ex01-creation-esds.jcl d'abord
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
