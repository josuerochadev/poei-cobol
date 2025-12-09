//ASSBLMAP JOB 'ASSEMBL',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//*--------------------------------------------------------------------*
//*  JCL : Assemblage d'une MAP BMS                                    *
//*  Usage : Assembler le source BMS MAPTEST                           *
//*--------------------------------------------------------------------*
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='FTEST.CICS.LOAD',
//          DSCTLIB='FTEST.CICS.LKED',
//          MAPNAME='MAPTEST',RMODE=24
//SYSPRINT DD SYSOUT=A
//SYSUT1   DD DSN=FTEST.CICS.SOURCE(MAPTEST),DISP=SHR
/*
//
