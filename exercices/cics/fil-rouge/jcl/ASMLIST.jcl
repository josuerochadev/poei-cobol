//ROCHA17A JOB (ACCT),'ASM CLILIST',CLASS=A,MSGCLASS=X,
//          NOTIFY=&SYSUID
//*********************************************************************
//*  JCL : ASMLIST
//*  FONCTION : Assemblage du BMS CLILIST (Liste Generique)
//*  MAPSET : CLILIST
//*  MAP : MAPLGEN
//*  TRANSACTION : LGEN
//*  FIL ROUGE CICS - EXERCICE 18
//*********************************************************************
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLILIST',RMODE=24
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLILIST),DISP=SHR
//*
//*********************************************************************
//*  SORTIE :
//*  - ROCHA.CICS.LOAD(CLILIST)  : Module load BMS
//*  - ROCHA.CICS.LINK(CLILIST)  : Copybook COBOL (DSECT)
//*********************************************************************
