//ROCHA09 JOB (ACCT),'ASSEMBL BMS CLIMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 9
//* ASSEMBLAGE DE LA MAP BMS CLIMAJ (MISE A JOUR CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Le copybook genere contiendra pour chaque champ :
//*   - NOMCPTx  ou x = I (input), O (output), L (longueur), A (attr)
//*   - Le suffixe 'A' permet de modifier l'attribut dynamiquement
//*     Ex: MOVE DFHBMASK TO NUMCPTA (passe en ASKIP/protege)
//*
//* Prerequis :
//*   - Source BMS copie dans ROCHA.CICS.SOURCE(CLIMAJ)
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
//          MAPNAME='CLIMAJ',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIMAJ),DISP=SHR
/*
//
