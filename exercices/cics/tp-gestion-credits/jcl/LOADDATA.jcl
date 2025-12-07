//LOADDATA JOB (ACCT),'LOAD VSAM DATA',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : LOADDATA.jcl
//* Description : Chargement des données de test dans les fichiers VSAM
//* Prérequis : Exécuter DEFVSAM.jcl avant ce JCL
//*********************************************************************
//*
//* ÉTAPE 1 : Chargement du fichier EMPLOYE (6 enregistrements)
//*
//LOADEMP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001MARTIN JEAN                  COMPTA    Y
EMP002DUPONT MARIE                 INFO      N
EMP003DURAND PIERRE                RH        Y
EMP004LEROY SOPHIE                 COMPTA    Y
EMP005MOREAU PAUL                  INFO      N
EMP006SIMON ANNE                   RH        Y
/*
//OUTFILE  DD DSN=USER.CICS.EMPLOYE,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//* ÉTAPE 2 : Chargement du fichier CRE-EMP (4 enregistrements)
//*
//LOADCRD  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001PRET AUTO
EMP003PRET IMMO
EMP004PRET PERSO
EMP006PRET ETUDES
/*
//OUTFILE  DD DSN=USER.CICS.CREDEMP,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*********************************************************************
//* NOTE : Les données ci-dessus sont simplifiées.
//* En production, utiliser un programme COBOL pour charger
//* les données avec les formats COMP-3 corrects.
//*
//* Données complètes (format lisible) :
//*
//* EMPLOYE :
//* EMP001 MARTIN JEAN        COMPTA     3500.00 Y
//* EMP002 DUPONT MARIE       INFO       4200.00 N
//* EMP003 DURAND PIERRE      RH         3800.00 Y
//* EMP004 LEROY SOPHIE       COMPTA     3200.00 Y
//* EMP005 MOREAU PAUL        INFO       4500.00 N
//* EMP006 SIMON ANNE         RH         3600.00 Y
//*
//* CRE-EMP :
//* EMP001 PRET AUTO     15000.00  450.00 12600.00
//* EMP003 PRET IMMO     50000.00  800.00 48400.00
//* EMP004 PRET PERSO     5000.00  250.00   250.00
//* EMP006 PRET ETUDES    8000.00  200.00  6800.00
//*********************************************************************
//*
//* ÉTAPE 3 : Vérification du chargement
//*
//VERIFY   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(USER.CICS.EMPLOYE) ALL
  LISTCAT ENTRIES(USER.CICS.CREDEMP) ALL
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
