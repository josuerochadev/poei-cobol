//FTESTEX1 JOB (ACCT),'EX01 PROC INSTREAM',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 1 : PROCEDURE IN-STREAM POUR CHARGEMENT ESDS
//* ============================================================
//* Objectif : Definir une procedure in-stream qui charge un
//*            dataset avec des donnees fournies via override
//*
//* Concepts :
//* - PROC...PEND : definition procedure in-stream
//* - &OUTDSN : parametre symbolique
//* - stepname.ddname : override pour fournir les donnees
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* DEFINITION DE LA PROCEDURE IN-STREAM
//* ----------------------------------------------------------
//LOADPROC PROC OUTDSN=
//*
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY
//SYSUT2   DD DSN=&OUTDSN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//         PEND
//*
//* ----------------------------------------------------------
//* APPEL DE LA PROCEDURE AVEC DONNEES IN-STREAM
//* ----------------------------------------------------------
//CALL1    EXEC LOADPROC,OUTDSN=FTEST.ESDS.AAAA
//*
//* Override du DD SYSUT1 pour fournir les donnees
//*
//LOAD.SYSUT1 DD *
111111AAAAAAAAAABBBBBBBBBB01012020ADRESSE1
222222CCCCCCCCCCDDDDDDDDDD01012021ADRESSE2
333333EEEEEEEEEFFFFFFFFFF01012023ADRESSE3
/*
//
