//COMPCRED JOB 'COMPILE PROGCRED',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//*********************************************************************
//* JCL : COMPCRED.jcl
//* Description : Compilation du programme CICS PROGCRED
//*
//* Procedure : DFHYITVL (Integrated CICS Translator)
//* - Translate : Convertit les EXEC CICS en COBOL
//* - Compile  : Compile le source COBOL
//* - Link-edit : Lie avec les modules CICS
//*
//* Entree  : FTEST.CICS.SOURCE(PROGCRED)
//* Sortie  : FTEST.CICS.LOAD(PROGCRED)
//*********************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//*
//* COMPILATION PROGCRED
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(PROGCRED),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PROGCRED(R)
/*
//*********************************************************************
//* PARAMETRES :
//* - DFHYITVL : Procedure integree CICS
//* - DFHELII  : Module d'interface CICS-COBOL
//* - PROGLIB  : Bibliotheque de sortie pour le load module
//*********************************************************************
//
