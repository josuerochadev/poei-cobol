//FTESTEX2 JOB (ACCT),'EX02 DEUX PROCS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 2 : DEUX PROCEDURES APPELEES SUCCESSIVEMENT
//* ============================================================
//* Objectif : Creer deux procedures et les appeler l'une
//*            apres l'autre dans le meme JCL
//*
//* Procedures :
//* - LOADESDS : charge un fichier avec donnees in-stream
//* - COPYSEQ  : copie un fichier vers un autre
//*
//* Note : En production, ces procedures seraient dans PROCLIB
//*        Ici on les definit en in-stream pour l'exercice
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURE 1 : LOADESDS - Chargement fichier sequentiel
//* ----------------------------------------------------------
//LOADESDS PROC ESDSDSN=
//*
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&ESDSDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* PROCEDURE 2 : COPYSEQ - Copie fichier sequentiel
//* ----------------------------------------------------------
//COPYSEQ  PROC INDSN=,OUTDSN=
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INDSN,DISP=SHR
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL PROCEDURE 1 : Charger FTEST.ESDS.AAAA
//* ----------------------------------------------------------
//STEP010  EXEC LOADESDS,ESDSDSN=FTEST.ESDS.AAAA
//LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//*
//* ----------------------------------------------------------
//* APPEL PROCEDURE 2 : Copier vers FTEST.TRI.SEQ
//* ----------------------------------------------------------
//STEP020  EXEC COPYSEQ,
//              INDSN=FTEST.ESDS.AAAA,
//              OUTDSN=FTEST.TRI.SEQ
//
