//RUNPGM   JOB (ACCT),'EXECUTE PROGRAM',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Execution d'un programme COBOL compile
//* Modifier PGMNAME selon le programme à exécuter
//*--------------------------------------------------------------------
//RUN      EXEC PGM=&PGMNAME
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//*
//* Runtime libraries
//         DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//*--------------------------------------------------------------------
//* Fichiers de donnees (adapter selon le programme)
//*--------------------------------------------------------------------
//* Exemple fichier sequentiel
//*SEQFILE  DD DSN=USER.DATA.SEQFILE,DISP=SHR
//*
//* Exemple fichier VSAM
//*VSAMFILE DD DSN=USER.DATA.VSAMFILE,DISP=SHR
//*
