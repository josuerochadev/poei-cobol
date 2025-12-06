//FTESTEX1 JOB (ACCT),'EX01 CREATION ESDS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 1 : CREATION DATASET AVEC DONNEES IN-STREAM
//* ============================================================
//* Objectif : Creer FTEST.ESDS.AAAA avec IEBGENER et DD *
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//SYSUT2   DD DSN=FTEST.ESDS.AAAA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//
