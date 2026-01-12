//ROCHA04 JOB (ACCT),'COMPILE PRGCLIA',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 3
//* COMPILATION DU PROGRAMME COBOL-CICS PRGCLIA (AFFICHAGE CLIENT)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGCLIA)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGCLIA)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLIAFF)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLIAFF)
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGCLIA),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGCLIA(R)
/*
//
