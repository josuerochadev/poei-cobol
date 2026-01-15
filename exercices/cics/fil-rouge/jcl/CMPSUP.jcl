//ROCHA13 JOB (ACCT),'COMPILE PRGSUP',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 13
//* COMPILATION DU PROGRAMME COBOL-CICS PRGSUP (SUPPRESSION CLIENT)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGSUP)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGSUP)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLISUP)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLISUP)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction (DFHPF3, DFHCLEAR, etc.)
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLISUP   : Structure MAP generee par l'assemblage BMS
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGSUP),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGSUP(R)
/*
//
