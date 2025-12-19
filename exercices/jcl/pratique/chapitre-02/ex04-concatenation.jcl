//FTESTEX4 JOB (ACCT),'EX04 CONCATENATION',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 4 : CONCATENATION DE DATASETS
//* ============================================================
//* Objectif : Concatener AAAA + BBBB + CCCC vers DDDD
//*
//* Concepts :
//* - DDname uniquement sur premiere DD
//* - Fichiers lus sequentiellement
//* - Resultat = 9 enregistrements (3 x 3)
//*
//* Prerequis : Executer ex01, ex02, ex03 d'abord
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//* Concatenation des 3 fichiers en entree
//*
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//         DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//         DD DSN=FTEST.ESDS.CCCC,DISP=SHR
//*
//* Fichier de sortie concatene
//*
//SYSUT2   DD DSN=FTEST.ESDS.DDDD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
