//FTESTTP1 JOB (ACCT),'TP01 DATASET PS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 1 : CREATION D'UN DATA SET SEQUENTIEL (PS)
//* ============================================================
//* Objectif : Creer un dataset sequentiel et y charger des
//*            donnees avec IEBGENER
//*
//* Caracteristiques du dataset :
//* - Nom    : FTEST.DATA.SEQFILE
//* - Type   : PS (Physical Sequential)
//* - LRECL  : 80
//* - RECFM  : FB
//* - Espace : 5 tracks primaires, 2 secondaires
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
  DELETE FTEST.DATA.SEQFILE
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* ETAPE 2 : CREATION DU DATASET SEQUENTIEL VIDE
//* ----------------------------------------------------------
//* Methode 1 : Avec IEFBR14 (creation vide)
//*
//CREATE   EXEC PGM=IEFBR14
//SEQFILE  DD DSN=FTEST.DATA.SEQFILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PS),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* ETAPE 3 : CHARGEMENT DES DONNEES
//* ----------------------------------------------------------
//LOAD     EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*================================================================*
*  FICHIER SEQUENTIEL DE TEST - TP01                              *
*================================================================*
ENREGISTREMENT 001 - PREMIERE LIGNE DE DONNEES
ENREGISTREMENT 002 - DEUXIEME LIGNE DE DONNEES
ENREGISTREMENT 003 - TROISIEME LIGNE DE DONNEES
ENREGISTREMENT 004 - QUATRIEME LIGNE DE DONNEES
ENREGISTREMENT 005 - CINQUIEME LIGNE DE DONNEES
*================================================================*
*  FIN DU FICHIER                                                 *
*================================================================*
/*
//SYSUT2   DD DSN=FTEST.DATA.SEQFILE,DISP=MOD
//*
//* ----------------------------------------------------------
//* ETAPE 4 : VERIFICATION - AFFICHAGE DU CONTENU
//* ----------------------------------------------------------
//VERIFY   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.DATA.SEQFILE,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=81)
//*
//* ----------------------------------------------------------
//* ETAPE 5 : INFORMATION CATALOGUE
//* ----------------------------------------------------------
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(FTEST.DATA.SEQFILE) ALL
/*
//
