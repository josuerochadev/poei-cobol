//COMPRUN  JOB (ACCT),'COMPILE AND RUN',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Compilation + Execution en une seule soumission
//* Modifier SRCMBR selon le programme
//*--------------------------------------------------------------------
//*
//*====================================================================
//* ETAPE 1 : COMPILATION COBOL
//*====================================================================
//COBCOMP  EXEC PGM=IGYCRCTL,REGION=0M,
//         PARM='RENT,APOST,MAP,XREF'
//STEPLIB  DD DSN=IGY.V6R3M0.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN=USER.COBOL.SOURCE(&SRCMBR),DISP=SHR
//SYSLIB   DD DSN=USER.COBOL.COPYLIB,DISP=SHR
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
//*====================================================================
//* ETAPE 2 : LINK-EDIT
//*====================================================================
//LKED     EXEC PGM=IEWL,COND=(8,LT,COBCOMP),
//         PARM='LIST,XREF,LET,MAP'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=&&GOSET(&SRCMBR),DISP=(MOD,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(10,10,1))
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//*
//*====================================================================
//* ETAPE 3 : EXECUTION
//*====================================================================
//GO       EXEC PGM=*.LKED.SYSLMOD,COND=(8,LT)
//STEPLIB  DD DSN=CEE.SCEERUN,DISP=SHR
//         DD DSN=CEE.SCEERUN2,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//*
//* Ajouter les DD pour les fichiers de donnees ici
//*
