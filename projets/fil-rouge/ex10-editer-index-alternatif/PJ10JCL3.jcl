//PJ10JCL3 JOB (COMPTE),'EXEC EDIT REGION',MSGLEVEL=(1,1),REGION=4M,
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID
//*====================================================================*
//* EXERCICE 10 - EXECUTION EDITION PAR REGION VIA AIX                *
//*====================================================================*
//RUN      EXEC PGM=PJ10REG
//STEPLIB  DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR
//FCLIENT  DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR
//FCLIENT1 DD DSN=ROCHA.PATH.AIX.REGION,DISP=SHR
//FEDITION DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
/*
//
