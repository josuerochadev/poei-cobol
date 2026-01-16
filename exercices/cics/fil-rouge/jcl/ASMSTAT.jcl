//ROCHA11 JOB (ACCT),'ASSEMBL BMS CLISTAT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 19
//* ASSEMBLAGE DE LA MAP BMS CLISTAT (STATISTIQUES PAR REGION)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* FONCTIONNALITE :
//*   Ecran de saisie code region et affichage des statistiques
//*   - Nombre total de clients
//*   - Nombre et somme des debiteurs (DB)
//*   - Nombre et somme des crediteurs (CR)
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLISTAT',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLISTAT),DISP=SHR
/*
