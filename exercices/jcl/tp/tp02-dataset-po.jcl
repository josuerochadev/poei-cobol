//FTESTTP2 JOB (ACCT),'TP02 DATASET PO',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 2 : CREATION D'UN DATA SET PARTITIONNE (PO/PDS)
//* ============================================================
//* Objectif : Creer un PDS et y ajouter des membres
//*
//* Caracteristiques du dataset :
//* - Nom       : FTEST.TSOJCL.LIBTEST
//* - Type      : PO (Partitioned Organization)
//* - LRECL     : 80
//* - RECFM     : FB
//* - Espace    : 10 tracks primaires, 5 secondaires
//* - Directory : 10 blocs
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* ETAPE 1 : SUPPRESSION SI EXISTE
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.TSOJCL.LIBTEST
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* ETAPE 2 : CREATION DU PDS VIDE
//* ----------------------------------------------------------
//CREATE   EXEC PGM=IEFBR14
//PDSLIB   DD DSN=FTEST.TSOJCL.LIBTEST,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* ETAPE 3 : CREATION MEMBRE JIEBGENE
//* ----------------------------------------------------------
//* Ce membre contient un exemple de JCL IEBGENER
//*
//MEMBRE1  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
//COPYFILE JOB (ACCT),'EXEMPLE IEBGENER',
//             CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* EXEMPLE DE COPIE AVEC IEBGENER
//*
//COPY     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=fichier.source,DISP=SHR
//SYSUT2   DD DSN=fichier.dest,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
//
/*
//SYSUT2   DD DSN=FTEST.TSOJCL.LIBTEST(JIEBGENE),DISP=SHR
//*
//* ----------------------------------------------------------
//* ETAPE 4 : CREATION MEMBRE CONFIG
//* ----------------------------------------------------------
//MEMBRE2  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*================================================================*
*  MEMBRE CONFIG - PARAMETRES DE CONFIGURATION                    *
*================================================================*
PARAM.ENVIRONNEMENT=TEST
PARAM.DEBUG=OUI
PARAM.TRACE=NON
PARAM.MAXRECORDS=10000
*================================================================*
/*
//SYSUT2   DD DSN=FTEST.TSOJCL.LIBTEST(CONFIG),DISP=SHR
//*
//* ----------------------------------------------------------
//* ETAPE 5 : CREATION MEMBRE DONNEES
//* ----------------------------------------------------------
//MEMBRE3  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*================================================================*
*  MEMBRE DONNEES - FICHIER DE DONNEES TEST                       *
*================================================================*
001DUPONT    JEAN      75001PARIS
002MARTIN    MARIE     69001LYON
003DURAND    PIERRE    13001MARSEILLE
*================================================================*
/*
//SYSUT2   DD DSN=FTEST.TSOJCL.LIBTEST(DONNEES),DISP=SHR
//*
//* ----------------------------------------------------------
//* ETAPE 6 : LISTE DES MEMBRES DU PDS
//* ----------------------------------------------------------
//LISTPDS  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=FTEST.TSOJCL.LIBTEST,DISP=SHR
//SYSIN    DD DUMMY
//*
//* Note: IEBCOPY avec SYSIN DUMMY liste les membres dans SYSPRINT
//*
//
