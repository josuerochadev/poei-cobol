//ROCHA10 JOB (ACCT),'COMPILE PRGMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 10
//* COMPILATION DU PROGRAMME COBOL-CICS PRGMAJ (MISE A JOUR CLIENT)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGMAJ)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGMAJ)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLIMAJ)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLIMAJ)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction (DFHPF3, DFHCLEAR, etc.)
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, DFHBMUNN, etc.)
//*   - CLIMAJ   : Structure MAP generee par l'assemblage BMS
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGMAJ),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGMAJ(R)
/*
//
