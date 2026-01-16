//ROCHA19 JOB (ACCT),'COMPILE PRGSTAT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 19
//* COMPILATION DU PROGRAMME COBOL-CICS PRGSTAT (STATISTIQUES)
//*
//* Ce JCL compile le programme COBOL-CICS et genere :
//*   - Le module executable dans ROCHA.CICS.LOAD(PRGSTAT)
//*
//* Prerequis :
//*   - Source COBOL copie dans ROCHA.CICS.SOURCE(PRGSTAT)
//*   - Copybook BMS dans ROCHA.CICS.LINK(CLISTAT)
//*   - MAP assemblee dans ROCHA.CICS.LOAD(CLISTAT)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction (DFHPF3, DFHCLEAR, etc.)
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLISTAT  : Structure MAP generee par l'assemblage BMS
//*
//* Procedure : DFHYITVL (Integrated CICS Translator)
//*
//* FONCTIONNALITE :
//*   Programme de calcul des statistiques par region
//*   - Parcours complet du fichier FCLIENT
//*   - Comptage et somme par type (DB/CR)
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGSTAT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGSTAT(R)
/*
//
