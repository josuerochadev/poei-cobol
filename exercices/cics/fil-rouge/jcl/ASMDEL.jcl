//ROCHA09 JOB (ACCT),'ASSEMBL BMS CLIDEL',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 17
//* ASSEMBLAGE DE LA MAP BMS CLIDEL (SUPPRESSION GENERIQUE)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIDEL',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIDEL),DISP=SHR
/*
