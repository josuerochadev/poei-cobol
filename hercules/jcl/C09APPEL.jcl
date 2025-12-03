//C09APPEL JOB (ACCT),'PROGRAMMES ET SOUS-PROGRAMMES',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Exercices Chapitre IX - Programmes et Sous-Programmes
//*
//* Ce JCL compile et execute :
//*   1. Les sous-programmes (C09CALCUL, C09VALID, C09TRIEUR, C09MODIF)
//*   2. Les programmes appelants (C09APPEL, C09BYREF)
//*--------------------------------------------------------------------
//*
//*====================================================================
//* ETAPE 1 : COMPILATION DU SOUS-PROGRAMME C09CALCUL
//*====================================================================
//STEP01   EXEC IGYWCL
//COBOL.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09CALCUL.
      * ... (code du sous-programme)
/*
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09CALCUL),DISP=SHR
//*
//*====================================================================
//* ETAPE 2 : COMPILATION DU SOUS-PROGRAMME C09VALID
//*====================================================================
//STEP02   EXEC IGYWCL,COND=(0,NE)
//COBOL.SYSIN DD DSN=USER.SOURCE(C09VALID),DISP=SHR
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09VALID),DISP=SHR
//*
//*====================================================================
//* ETAPE 3 : COMPILATION DU SOUS-PROGRAMME C09TRIEUR
//*====================================================================
//STEP03   EXEC IGYWCL,COND=(0,NE)
//COBOL.SYSIN DD DSN=USER.SOURCE(C09TRIEUR),DISP=SHR
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09TRIEUR),DISP=SHR
//*
//*====================================================================
//* ETAPE 4 : COMPILATION DU SOUS-PROGRAMME C09MODIF
//*====================================================================
//STEP04   EXEC IGYWCL,COND=(0,NE)
//COBOL.SYSIN DD DSN=USER.SOURCE(C09MODIF),DISP=SHR
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09MODIF),DISP=SHR
//*
//*====================================================================
//* ETAPE 5 : COMPILATION PROGRAMME APPELANT C09APPEL
//*           (avec liaison des sous-programmes)
//*====================================================================
//STEP05   EXEC IGYWCL,COND=(0,NE)
//COBOL.SYSIN DD DSN=USER.SOURCE(C09APPEL),DISP=SHR
//LKED.SYSLIB DD DSN=USER.LOADLIB,DISP=SHR
//            DD DSN=CEE.SCEELKED,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE SYSLIB(C09CALCUL)
  INCLUDE SYSLIB(C09VALID)
  INCLUDE SYSLIB(C09TRIEUR)
  NAME C09APPEL(R)
/*
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09APPEL),DISP=SHR
//*
//*====================================================================
//* ETAPE 6 : COMPILATION PROGRAMME C09BYREF
//*====================================================================
//STEP06   EXEC IGYWCL,COND=(0,NE)
//COBOL.SYSIN DD DSN=USER.SOURCE(C09BYREF),DISP=SHR
//LKED.SYSLIB DD DSN=USER.LOADLIB,DISP=SHR
//            DD DSN=CEE.SCEELKED,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE SYSLIB(C09MODIF)
  NAME C09BYREF(R)
/*
//LKED.SYSLMOD DD DSN=USER.LOADLIB(C09BYREF),DISP=SHR
//*
//*====================================================================
//* ETAPE 7 : EXECUTION PROGRAMME PRINCIPAL
//*====================================================================
//STEP07   EXEC PGM=C09APPEL,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 8 : EXECUTION DEMO BY REFERENCE vs BY CONTENT
//*====================================================================
//STEP08   EXEC PGM=C09BYREF,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
