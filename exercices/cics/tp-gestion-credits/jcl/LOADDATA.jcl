//LOADDATA JOB (ACCT),'LOAD DATA CREDITS',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID
//*********************************************************************
//* JCL     : LOADDATA
//* Auteur  : Formation CICS
//* Date    : 2024-01
//* 
//* Fonction: Chargement des données de test dans les fichiers VSAM
//*           EMPLOYE et CREDEMP
//*
//* Prérequis: 
//*   - Fichiers VSAM déjà définis (DEFVSAM.jcl)
//*   - Fichiers séquentiels de données disponibles
//*
//* Données chargées :
//*   - 6 employés (EMP001 à EMP006)
//*   - 4 crédits (pour EMP001, EMP003, EMP004, EMP006)
//*********************************************************************
//*
//*-------------------------------------------------------------------*
//* ETAPE 1 : Chargement fichier EMPLOYE
//*-------------------------------------------------------------------*
//LOADEMPL EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001MARTIN JEAN                   COMPTA    0350000Y
EMP002DUPONT MARIE                  INFO      0420000N
EMP003DURAND PIERRE                 RH        0380000Y
EMP004LEROY SOPHIE                  COMPTA    0320000Y
EMP005MOREAU PAUL                   INFO      0450000N
EMP006SIMON ANNE                    RH        0360000Y
/*
//OUTFILE  DD DSN=USER.CICS.EMPLOYE,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2 : Chargement fichier CREDEMP
//*-------------------------------------------------------------------*
//LOADCRED EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001PRET AUTO            01500000004500001260000
EMP003PRET IMMO            05000000008000004840000
EMP004PRET PERSO           00500000002500000025000
EMP006PRET ETUDES          00800000002000000680000
/*
//OUTFILE  DD DSN=USER.CICS.CREDEMP,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 3 : Vérification des données chargées
//*-------------------------------------------------------------------*
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//EMPLOYE  DD DSN=USER.CICS.EMPLOYE,DISP=SHR
//CREDEMP  DD DSN=USER.CICS.CREDEMP,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(EMPLOYE) COUNT(10)
  PRINT INFILE(CREDEMP) COUNT(10)
/*
//
