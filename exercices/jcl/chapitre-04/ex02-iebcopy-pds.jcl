//FTESTEX2 JOB (ACCT),'EX02 IEBCOPY PDS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 2 : IEBCOPY - GESTION PDS
//* ============================================================
//* Objectif : Maitriser la gestion des PDS avec IEBCOPY
//*
//* Steps :
//* 1. DELPDS   - Supprimer les PDS existants
//* 2. CREPDS   - Creer les PDS vides (IEFBR14)
//* 3. LOADMEM  - Charger des membres (IEBGENER)
//* 4. COPYPDS  - Copier le PDS complet (IEBCOPY)
//* 5. COPYSEL  - Copier membres selectifs (IEBCOPY SELECT)
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 1 : SUPPRESSION DES PDS EXISTANTS
//* ----------------------------------------------------------
//DELPDS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.SOURCE.PDS
  DELETE FTEST.BACKUP.PDS
  DELETE FTEST.SELECT.PDS
  SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* STEP 2 : CREATION DES PDS VIDES
//* ----------------------------------------------------------
//CREPDS   EXEC PGM=IEFBR14
//SOURCE   DD DSN=FTEST.SOURCE.PDS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//BACKUP   DD DSN=FTEST.BACKUP.PDS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//SELECT   DD DSN=FTEST.SELECT.PDS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,2,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
//*
//* ----------------------------------------------------------
//* STEP 3A : CHARGER MEMBRE1
//* ----------------------------------------------------------
//LOADMB1  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
* MEMBRE1 - FICHIER DE CONFIGURATION
* ==================================
PARAM1=VALEUR1
PARAM2=VALEUR2
PARAM3=VALEUR3
/*
//SYSUT2   DD DSN=FTEST.SOURCE.PDS(MEMBRE1),DISP=SHR
//*
//* ----------------------------------------------------------
//* STEP 3B : CHARGER MEMBRE2
//* ----------------------------------------------------------
//LOADMB2  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
* MEMBRE2 - FICHIER DE DONNEES
* ============================
DATA LIGNE 001
DATA LIGNE 002
DATA LIGNE 003
/*
//SYSUT2   DD DSN=FTEST.SOURCE.PDS(MEMBRE2),DISP=SHR
//*
//* ----------------------------------------------------------
//* STEP 3C : CHARGER MEMBRE3
//* ----------------------------------------------------------
//LOADMB3  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
* MEMBRE3 - FICHIER DE TEST
* =========================
TEST RECORD A
TEST RECORD B
TEST RECORD C
/*
//SYSUT2   DD DSN=FTEST.SOURCE.PDS(MEMBRE3),DISP=SHR
//*
//* ----------------------------------------------------------
//* STEP 4 : COPIER LE PDS COMPLET
//* ----------------------------------------------------------
//COPYPDS  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=FTEST.SOURCE.PDS,DISP=SHR
//OUTPDS   DD DSN=FTEST.BACKUP.PDS,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
/*
//*
//* ----------------------------------------------------------
//* STEP 5 : COPIER MEMBRES SELECTIFS (MEMBRE1 ET MEMBRE3)
//* ----------------------------------------------------------
//COPYSEL  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=FTEST.SOURCE.PDS,DISP=SHR
//OUTPDS   DD DSN=FTEST.SELECT.PDS,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  SELECT MEMBER=(MEMBRE1,MEMBRE3)
/*
//
