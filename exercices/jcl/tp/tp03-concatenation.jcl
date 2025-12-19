//FTESTTP3 JOB (ACCT),'TP03 CONCATENATION',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 3 : CONCATENATION DE DATA SETS
//* ============================================================
//* Objectif : Creer plusieurs datasets et les concatener
//*
//* Fichiers :
//* - FTEST.ESDS.AAAA : Premier fichier source
//* - FTEST.ESDS.BBBB : Deuxieme fichier source
//* - FTEST.ESDS.CCCC : Fichier resultat (concatenation)
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* ETAPE 1 : SUPPRESSION DES FICHIERS EXISTANTS
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.ESDS.AAAA
  DELETE FTEST.ESDS.BBBB
  DELETE FTEST.ESDS.CCCC
  SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* ETAPE 2 : CREATION DU PREMIER FICHIER (AAAA)
//* ----------------------------------------------------------
//CREAAAA  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*================================================================*
*  FICHIER AAAA - DEBUT                                           *
*================================================================*
AAAA-LIGNE-001-PREMIER-FICHIER-DONNEES
AAAA-LIGNE-002-PREMIER-FICHIER-DONNEES
AAAA-LIGNE-003-PREMIER-FICHIER-DONNEES
AAAA-LIGNE-004-PREMIER-FICHIER-DONNEES
AAAA-LIGNE-005-PREMIER-FICHIER-DONNEES
*================================================================*
*  FICHIER AAAA - FIN                                             *
*================================================================*
/*
//SYSUT2   DD DSN=FTEST.ESDS.AAAA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* ETAPE 3 : CREATION DU DEUXIEME FICHIER (BBBB)
//* ----------------------------------------------------------
//CREABBBB EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
*================================================================*
*  FICHIER BBBB - DEBUT                                           *
*================================================================*
BBBB-LIGNE-001-DEUXIEME-FICHIER-DONNEES
BBBB-LIGNE-002-DEUXIEME-FICHIER-DONNEES
BBBB-LIGNE-003-DEUXIEME-FICHIER-DONNEES
*================================================================*
*  FICHIER BBBB - FIN                                             *
*================================================================*
/*
//SYSUT2   DD DSN=FTEST.ESDS.BBBB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* ETAPE 4 : CONCATENATION VERS FICHIER CCCC
//* ----------------------------------------------------------
//* La concatenation est realisee en mettant plusieurs DD
//* sans nom (sauf la premiere) sous le meme ddname
//*
//CONCAT   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//         DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD DSN=FTEST.ESDS.CCCC,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* ETAPE 5 : AFFICHAGE DES TROIS FICHIERS
//* ----------------------------------------------------------
//PRINTAAA EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.AAAA,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=81)
//*
//PRINTBBB EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.BBBB,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=81)
//*
//PRINTCCC EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.ESDS.CCCC,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=81)
//*
//* ----------------------------------------------------------
//* RESULTAT ATTENDU :
//* FTEST.ESDS.CCCC contient toutes les lignes de AAAA
//* suivies de toutes les lignes de BBBB
//* ----------------------------------------------------------
//
