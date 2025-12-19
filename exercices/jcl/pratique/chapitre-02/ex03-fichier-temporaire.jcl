//FTESTEX3 JOB (ACCT),'EX03 FICHIER TEMPORAIRE',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 3 : FICHIER TEMPORAIRE
//* ============================================================
//* Objectif : Copier ESDS.BBBB vers &&TEMP puis vers ESDS.CCCC
//*
//* Concepts :
//* - DSN=&&TEMP : fichier temporaire
//* - DISP=(NEW,PASS) : creer et passer au step suivant
//* - DISP=(OLD,DELETE) : utiliser puis supprimer
//*
//* Prerequis : Executer ex02-copie-iebgener.jcl d'abord
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390 sur SYSUT2 du STEP020
//* ============================================================
//*
//* STEP 1 : Copie vers fichier temporaire
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=&&TEMP,
//            DISP=(NEW,PASS),
//            SPACE=(TRK,(1,1)),
//            DCB=*.SYSUT1
//*
//* STEP 2 : Copie du temporaire vers fichier permanent
//*
//STEP020  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&&TEMP,
//            DISP=(OLD,DELETE)
//SYSUT2   DD DSN=FTEST.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
