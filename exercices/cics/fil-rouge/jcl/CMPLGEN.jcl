//ROCHA17C JOB (ACCT),'COMP PRGLGEN',CLASS=A,MSGCLASS=X,
//          NOTIFY=&SYSUID
//*********************************************************************
//*  JCL : CMPLGEN
//*  FONCTION : Compilation du programme COBOL PRGLGEN
//*  PROGRAMME : PRGLGEN (Liste Generique des Clients)
//*  TRANSACTION : LGEN
//*  FIL ROUGE CICS - EXERCICE 18
//*********************************************************************
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGLGEN),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGLGEN(R)
/*
//*
//*********************************************************************
//*  SORTIE :
//*  - ROCHA.CICS.LOAD(PRGLGEN)  : Module load executable
//*
//*  PREREQUIS :
//*  - Le BMS CLILIST doit etre assemble avant (ASMLIST.jcl)
//*  - Le copybook CLILIST doit exister dans ROCHA.CICS.LINK
//*********************************************************************
