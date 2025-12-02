//PJ10JCL4 JOB (COMPTE),'EXEC EDIT ACTIV',MSGLEVEL=(1,1),REGION=4M,
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID
//*====================================================================*
//* EXERCICE 10 - EXECUTION EDITION PAR ACTIVITE VIA AIX              *
//*====================================================================*
//RUN      EXEC PGM=PJ10ACT
//STEPLIB  DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR
//FCLIENT  DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR
//         DD DSN=ROCHA.PATH.AIX.ACTPROF,DISP=SHR
//FEDITION DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
/*
//
