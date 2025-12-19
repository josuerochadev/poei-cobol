//FTESTEX1 JOB (ACCT),'EX01 IEFBR14 IEBGENER',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 1 : IEFBR14 ET IEBGENER
//* ============================================================
//* Objectif : Utiliser IEFBR14 et IEBGENER pour creer et
//*            charger des datasets
//*
//* Steps :
//* 1. DELETE  - Supprimer dataset existant (IDCAMS)
//* 2. CREATE  - Creer et charger donnees (IEBGENER)
//* 3. PRINT   - Afficher le contenu (IEBGENER)
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : SUPPRESSION CONDITIONNELLE
//* ----------------------------------------------------------
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.UTIL.DATA
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* STEP 2 : CREATION ET CHARGEMENT AVEC IEBGENER
//* ----------------------------------------------------------
//CREATE   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
001DUPONT    JEAN      PARIS     75001
002MARTIN    MARIE     LYON      69001
003DURAND    PIERRE    MARSEILLE 13001
004PETIT     SOPHIE    BORDEAUX  33001
005BERNARD   PAUL      PARIS     75008
006THOMAS    CLAIRE    LYON      69002
007ROBERT    LUC       NANTES    44000
008RICHARD   ANNE      PARIS     75016
/*
//SYSUT2   DD DSN=FTEST.UTIL.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=50,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 3 : AFFICHAGE DU CONTENU
//* ----------------------------------------------------------
//PRINT    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//SYSUT2   DD SYSOUT=*
//
