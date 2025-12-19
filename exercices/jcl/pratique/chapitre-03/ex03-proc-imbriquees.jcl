//FTESTEX3 JOB (ACCT),'EX03 PROC IMBRIQUEES',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 3 : PROCEDURES IMBRIQUEES
//* ============================================================
//* Objectif : Une procedure principale (FULLPROC) appelle
//*            les deux procedures de l'exercice 2
//*
//* Concepts :
//* - Imbrication de procedures
//* - Propagation des parametres
//* - Override imbrique : procstep.step.ddname
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 2 : LOADESDS
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
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 2 : COPYSEQ
//* ----------------------------------------------------------
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
//* ----------------------------------------------------------
//* PROCEDURE NIVEAU 1 : FULLPROC (appelle les deux autres)
//* ----------------------------------------------------------
//FULLPROC PROC SRCDSN=,TGTDSN=
//*
//* Etape 1 : Charger le fichier source
//LOADSTEP EXEC LOADESDS,ESDSDSN=&SRCDSN
//*
//* Etape 2 : Copier vers fichier cible
//COPYSTEP EXEC COPYSEQ,INDSN=&SRCDSN,OUTDSN=&TGTDSN
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL UNIQUE DE LA PROCEDURE PRINCIPALE
//* ----------------------------------------------------------
//PROCESS  EXEC FULLPROC,
//              SRCDSN=FTEST.ESDS.AAAA,
//              TGTDSN=FTEST.TRI.SEQ
//*
//* Override pour fournir les donnees au step LOAD imbrique
//* Notation : procstep.step.ddname
//*
//LOADSTEP.LOAD.SYSUT1 DD *
111111DONNEES-FICHIER-AAAA-LIGNE-01
222222DONNEES-FICHIER-AAAA-LIGNE-02
333333DONNEES-FICHIER-AAAA-LIGNE-03
/*
//
