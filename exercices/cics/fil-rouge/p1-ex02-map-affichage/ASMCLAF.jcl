//ROCHA03 JOB (ACCT),'ASSEMBL BMS CLIAFF',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 2
//* ASSEMBLAGE DE LA MAP BMS CLIAFF (AFFICHAGE CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Prerequis :
//*   - Source BMS copie dans ROCHA.CICS.SOURCE(CLIAFF)
//*   - Libraries ROCHA.CICS.* existantes
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//* ASSEMBLAGE DE LA MAP BMS
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIAFF',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIAFF),DISP=SHR
/*
//
