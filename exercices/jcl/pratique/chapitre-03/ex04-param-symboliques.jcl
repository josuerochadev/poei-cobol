//FTESTEX4 JOB (ACCT),'EX04 PARAM SYMBOLIQUES',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 4 : PARAMETRAGE SYMBOLIQUE MULTIPLE
//* ============================================================
//* Objectif : Utiliser la meme procedure avec differents
//*            parametres pour traiter BBBB et CCCC
//*
//* Concepts :
//* - Reutilisation de procedure
//* - Parametrage dynamique
//* - Un seul code, plusieurs jeux de donnees
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURES (memes que exercice 3)
//* ----------------------------------------------------------
//LOADESDS PROC ESDSDSN=
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
//COPYSEQ  PROC INDSN=,OUTDSN=
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
//FULLPROC PROC SRCDSN=,TGTDSN=
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* ----------------------------------------------------------
//* PREMIER APPEL : FTEST.ESDS.BBBB -> FTEST.TRI.BBBB
//* ----------------------------------------------------------
//PROCBB   EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.BBBB,
//              TGTDSN=FTEST.TRI.BBBB
//LOADSTEP.LOAD.SYSUT1 DD *
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-01
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-02
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-03
BBBBBBDONNEES-POUR-FICHIER-BBBB-LIGNE-04
/*
//*
//* ----------------------------------------------------------
//* DEUXIEME APPEL : FTEST.ESDS.CCCC -> FTEST.TRI.CCCC
//* ----------------------------------------------------------
//PROCCC   EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.CCCC,
//              TGTDSN=FTEST.TRI.CCCC
//LOADSTEP.LOAD.SYSUT1 DD *
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-01
CCCCCCDONNEES-POUR-FICHIER-CCCC-LIGNE-02
/*
//
