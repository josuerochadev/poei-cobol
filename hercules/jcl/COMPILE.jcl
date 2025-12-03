//COBCOMP  JOB (ACCT),'COMPILE COBOL',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Compilation d'un programme COBOL
//* Modifier le membre SRCMBR selon le programme à compiler
//*--------------------------------------------------------------------
//COBCOMP  EXEC PGM=IGYCRCTL,REGION=0M,
//         PARM='RENT,APOST,MAP,LIST,XREF'
//*
//STEPLIB  DD DSN=IGY.V6R3M0.SIGYCOMP,DISP=SHR
//*
//SYSIN    DD DSN=USER.COBOL.SOURCE(&SRCMBR),DISP=SHR
//*
//SYSLIB   DD DSN=USER.COBOL.COPYLIB,DISP=SHR
//*
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(3,3))
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//*
//*--------------------------------------------------------------------
//* LINK-EDIT
//*--------------------------------------------------------------------
//LKED     EXEC PGM=IEWL,COND=(8,LT,COBCOMP),
//         PARM='LIST,XREF,LET,MAP'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=USER.LOADLIB(&SRCMBR),DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
