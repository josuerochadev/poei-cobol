//COMPALL  JOB 'COMPILE ALL',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//*********************************************************************
//* JCL : COMPALL.jcl
//* Description : Compilation des 3 programmes CICS du TP Gestion Credits
//*
//* Procedure utilisee : DFHYITVL (Integrated CICS Translator)
//* Cette procedure effectue :
//*   - Translation des commandes EXEC CICS en COBOL
//*   - Compilation COBOL
//*   - Link-edit avec les modules CICS
//*
//* Programmes compiles :
//*   - CREDDAO  : Couche Donnees (Data Access Object)
//*   - CREDTRT  : Couche Traitement (Business Logic)
//*   - CREDPRES : Couche Presentation (User Interface)
//*
//* ORDRE DE COMPILATION IMPORTANT :
//* 1. CREDDAO   (appele par CREDTRT)
//* 2. CREDTRT   (appele par CREDPRES)
//* 3. CREDPRES  (point d'entree)
//*********************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//*
//*********************************************************************
//* ETAPE 1 : Compilation CREDDAO (Couche Donnees)
//*********************************************************************
//STEP1    EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(CREDDAO),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME CREDDAO(R)
/*
//*
//*********************************************************************
//* ETAPE 2 : Compilation CREDTRT (Couche Traitement)
//*********************************************************************
//STEP2    EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(CREDTRT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME CREDTRT(R)
/*
//*
//*********************************************************************
//* ETAPE 3 : Compilation CREDPRES (Couche Presentation)
//*********************************************************************
//STEP3    EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(CREDPRES),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME CREDPRES(R)
/*
//*********************************************************************
//* PARAMETRES CLES :
//*
//* DFHYITVL  : Procedure integree (Translate + Compile + Link)
//* INDEX     : HLQ des bibliotheques CICS
//* PROGLIB   : Bibliotheque de sortie pour les load modules
//* AD370HLQ  : HLQ du compilateur COBOL Enterprise (IGY420)
//* DSCTLIB   : Bibliotheque des copybooks (CREDSET, EMPLOYE, CREDEMP)
//* LE370HLQ  : HLQ du Language Environment
//* DFHELII   : Module d'interface CICS-COBOL (Exec Interface)
//*
//* Apres execution reussie :
//* - Les 3 programmes sont dans FTEST.CICS.LOAD
//*
//*********************************************************************
//
