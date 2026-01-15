//ROCHA12 JOB (ACCT),'ASSEMBL BMS CLISUP',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 12
//* ASSEMBLAGE DE LA MAP BMS CLISUP (SUPPRESSION CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Le copybook genere contiendra pour chaque champ :
//*   - NOMCPTx  ou x = I (input), O (output), L (longueur), A (attr)
//*   - CONFIRMI pour recevoir la confirmation O/N
//*
//* Prerequis :
//*   - Source BMS copie dans ROCHA.CICS.SOURCE(CLISUP)
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
//          MAPNAME='CLISUP',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLISUP),DISP=SHR
/*
//
