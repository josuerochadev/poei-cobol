//PJ10JCL1 JOB (COMPTE),'COMPIL PJ10REG',MSGLEVEL=(1,1),REGION=4M,
//             NOTIFY=&SYSUID
//*====================================================================*
//* EXERCICE 10 - COMPILATION PROGRAMME EDITION PAR REGION            *
//*====================================================================*
//COMPIL EXEC IGYWCL
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ10REG),DISP=SHR
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ10REG),DISP=SHR
/*
//
