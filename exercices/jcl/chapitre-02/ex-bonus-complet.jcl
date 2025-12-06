//FTESTALL JOB (ACCT),'BONUS - JCL COMPLET',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE BONUS : JCL COMPLET - TOUS LES EXERCICES
//* ============================================================
//* Execute tous les exercices du chapitre 2 en un seul job
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390 sur chaque SYSUT2
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : Creation ESDS.AAAA avec donnees in-stream
//* ----------------------------------------------------------
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
//*
//* ----------------------------------------------------------
//* STEP 2 : Copie ESDS.AAAA vers ESDS.BBBB
//* ----------------------------------------------------------
//STEP020  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 3 : Copie ESDS.BBBB vers temporaire
//* ----------------------------------------------------------
//STEP030  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(1,1)),
//            DCB=*.SYSUT1
//*
//* ----------------------------------------------------------
//* STEP 4 : Copie temporaire vers ESDS.CCCC
//* ----------------------------------------------------------
//STEP040  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&&TEMP,DISP=(OLD,DELETE)
//SYSUT2   DD DSN=FTEST.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 5 : Concatenation vers ESDS.DDDD
//* ----------------------------------------------------------
//STEP050  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//         DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//         DD DSN=FTEST.ESDS.CCCC,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.DDDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
