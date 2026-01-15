//ROCHA06 JOB (ACCT),'COMPILE PRGAJT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 7
//* COMPILATION DU PROGRAMME COBOL-CICS PRGAJT (AJOUT CLIENT)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGAJT)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGAJT)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLIAJT)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLIAJT)
//*
//* Procedure : DFHYITVL (Integrated CICS Translator)
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//* COMPILATION COBOL-CICS
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGAJT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGAJT(R)
/*
//
