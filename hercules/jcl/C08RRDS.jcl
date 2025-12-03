//C08RRDS  JOB (ACCT),'EXERCICE RRDS',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*--------------------------------------------------------------------
//* JCL : Exercice Chapitre 08 - Operations RRDS
//* Ce JCL execute les 5 etapes de l'exercice RRDS
//*--------------------------------------------------------------------
//*
//*====================================================================
//* ETAPE 0 : DEFINITION DU FICHIER RRDS
//*====================================================================
//STEP0    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.DATA.RRDS CLUSTER PURGE
  SET MAXCC = 0

  DEFINE CLUSTER (                                    -
           NAME(USER.DATA.RRDS)                       -
           NUMBERED                                   -
           RECORDSIZE(80 80)                          -
           TRACKS(5 5)                                -
           SHAREOPTIONS(2 3)                          -
           )                                          -
         DATA (                                       -
           NAME(USER.DATA.RRDS.DATA)                  -
           CISZ(4096)                                 -
           )
/*
//*
//*====================================================================
//* ETAPE 1 : CREATION FICHIER RRDS (10 enregistrements)
//*====================================================================
//STEP1    EXEC PGM=C08RRDWT,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 2 : LECTURE ENREGISTREMENT N°6
//*====================================================================
//STEP2    EXEC PGM=C08RRDRD,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 3 : ECRITURE ENREGISTREMENT N°13
//*====================================================================
//STEP3    EXEC PGM=C08RRDAD,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 4 : MODIFICATION ENREGISTREMENT N°4
//*====================================================================
//STEP4    EXEC PGM=C08RRDRW,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 5 : SUPPRESSION ENREGISTREMENT N°3
//*====================================================================
//STEP5    EXEC PGM=C08RRDDE,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
//*====================================================================
//* ETAPE 6 : VERIFICATION - LISTE COMPLETE
//*====================================================================
//STEP6    EXEC PGM=C08RRDLS,COND=(0,NE)
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//RRDSFILE DD DSN=USER.DATA.RRDS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//*
