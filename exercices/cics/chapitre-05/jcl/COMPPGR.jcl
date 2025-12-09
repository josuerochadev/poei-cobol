//COMPPGR  JOB 'COMPPGR',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//*--------------------------------------------------------------------*
//*  JCL : Compilation d'un programme CICS COBOL                       *
//*  Usage : Compile et link-edit un programme CICS                    *
//*  Procedure : DFHYITVL (Integrated CICS Translator)                 *
//*--------------------------------------------------------------------*
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(PROGWRIT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PROGWRIT(R)
/*
//
