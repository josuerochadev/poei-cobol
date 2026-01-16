//ROCHA17 JOB (ACCT),'COMPILE PRGDELG',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 17
//* COMPILATION DU PROGRAMME COBOL-CICS PRGDELG (SUPPRESSION GENERIQUE)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGDELG)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGDELG)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLIDEL)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLIDEL)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction (DFHPF3, DFHCLEAR, etc.)
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLIDEL   : Structure MAP generee par l'assemblage BMS
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGDELG),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGDELG(R)
/*
//
