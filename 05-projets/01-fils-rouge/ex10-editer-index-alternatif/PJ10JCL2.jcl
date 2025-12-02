//PJ10JCL2 JOB (COMPTE),'COMPIL PJ10ACT',MSGLEVEL=(1,1),REGION=4M,
//             NOTIFY=&SYSUID
//*====================================================================*
//* EXERCICE 10 - COMPILATION PROGRAMME EDITION PAR ACTIVITE          *
//*====================================================================*
//COMPIL EXEC IGYWCL
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ10ACT),DISP=SHR
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ10ACT),DISP=SHR
/*
//
