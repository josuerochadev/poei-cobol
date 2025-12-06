//FTESTEX4 JOB (ACCT),'EX04 IDCAMS VSAM',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 4 : IDCAMS - GESTION VSAM
//* ============================================================
//* Objectif : Creer et manipuler un fichier VSAM KSDS
//*
//* Prerequis : Executer ex01-iefbr14-iebgener.jcl d'abord
//*             pour creer FTEST.UTIL.DATA
//*
//* Format fichier (LRECL=50) :
//* - Pos 01-03 : Code client (CLE VSAM)
//* - Pos 04-50 : Reste des donnees
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001 sur les DEFINE
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : SUPPRESSION DU CLUSTER EXISTANT
//* ----------------------------------------------------------
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.VSAM.CLIENT CLUSTER PURGE
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* STEP 2 : DEFINITION DU KSDS
//* ----------------------------------------------------------
//DEFINE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(FTEST.VSAM.CLIENT)             -
           INDEXED                             -
           KEYS(3 0)                           -
           RECORDSIZE(50 50)                   -
           TRACKS(5 2)                         -
           SHAREOPTIONS(2 3)                   -
           FREESPACE(10 10)                    -
         )                                     -
         DATA (                                -
           NAME(FTEST.VSAM.CLIENT.DATA)        -
         )                                     -
         INDEX (                               -
           NAME(FTEST.VSAM.CLIENT.INDEX)       -
         )
/*
//*
//* ----------------------------------------------------------
//* STEP 3 : CHARGEMENT DES DONNEES (REPRO)
//* ----------------------------------------------------------
//LOAD     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//OUTFILE  DD DSN=FTEST.VSAM.CLIENT,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
//*
//* ----------------------------------------------------------
//* STEP 4 : AFFICHAGE DES 5 PREMIERS ENREGISTREMENTS
//* ----------------------------------------------------------
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.VSAM.CLIENT,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(INFILE) CHARACTER COUNT(5)
/*
//*
//* ----------------------------------------------------------
//* STEP 5 : LISTCAT - INFORMATIONS CATALOGUE
//* ----------------------------------------------------------
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(FTEST.VSAM.CLIENT) ALL
/*
//*
//* ----------------------------------------------------------
//* STEP 6 : AFFICHAGE SELECTION PAR CLE
//* ----------------------------------------------------------
//PRINTSEL EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=FTEST.VSAM.CLIENT,DISP=SHR
//SYSIN    DD *
* AFFICHER ENREGISTREMENTS DE 003 A 006
  PRINT INFILE(INFILE) CHARACTER -
        FROMKEY(003) TOKEY(006)
/*
//
