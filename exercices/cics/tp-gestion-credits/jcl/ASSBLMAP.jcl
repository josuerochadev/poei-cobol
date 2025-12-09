//ASSBLMAP JOB 'ASSEMBL',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//*********************************************************************
//* JCL : ASSBLMAP.jcl
//* Description : Assemblage du MAPSET BMS CREDSET
//*
//* Procedure utilisee : DFHMAPS (fournie par CICS)
//* Cette procedure genere :
//*   - Le module objet (MAPLIB) pour l'execution
//*   - Le DSECT COBOL (DSCTLIB) pour le copybook
//*
//* Entree  : FTEST.CICS.SOURCE(CREDSET) - Source BMS
//* Sorties : FTEST.CICS.LOAD   - Module objet
//*           FTEST.CICS.LKED   - Copybook CREDSET.cpy
//*********************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//*
//* ASSEMBLAGE DU MAPSET CREDSET
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='FTEST.CICS.LOAD',
//          DSCTLIB='FTEST.CICS.LKED',
//          MAPNAME='CREDSET',RMODE=24
//SYSPRINT DD SYSOUT=A
//SYSUT1   DD DSN=FTEST.CICS.SOURCE(CREDSET),DISP=SHR
/*
//*********************************************************************
//* PARAMETRES CLES :
//*
//* DFHMAPS   : Procedure IBM CICS pour assembler les MAPs
//* INDEX     : HLQ des bibliotheques CICS (DFH510.CICS)
//* MAPLIB    : Bibliotheque de sortie pour le load module
//* DSCTLIB   : Bibliotheque pour le DSECT (copybook COBOL genere)
//* MAPNAME   : Nom du MAPSET a assembler
//* RMODE     : Mode de residence (24 = below the line)
//*
//* Apres execution reussie :
//* - Le module CREDSET est dans FTEST.CICS.LOAD
//* - Le copybook CREDSET.cpy est dans FTEST.CICS.LKED
//*
//*********************************************************************
//
