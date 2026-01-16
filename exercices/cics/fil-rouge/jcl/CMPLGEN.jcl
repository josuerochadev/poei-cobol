//ROCHA18 JOB (ACCT),'COMPILE PRGLGEN',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 18
//* COMPILATION DU PROGRAMME COBOL-CICS PRGLGEN (LISTE GENERIQUE)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGLGEN)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGLGEN)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLILIST)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLILIST)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction (DFHPF3, DFHCLEAR, etc.)
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLILIST  : Structure MAP generee par l'assemblage BMS
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGLGEN),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGLGEN(R)
/*
//
