//FTETP01  JOB (ACCT),'TP01 CLIENTS',CLASS=A,MSGCLASS=A,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*********************************************************************
//*  TP 01 : GESTION COMPLETE D'UN FICHIER CLIENTS                    *
//*  - Creation KSDS                                                   *
//*  - Chargement donnees                                              *
//*  - AIX sur ville et solde                                          *
//*  - Export sauvegarde                                               *
//*********************************************************************
//*
//* STRUCTURE ENREGISTREMENT (100 octets):
//* Pos 01-10 : Numero client (cle primaire)
//* Pos 11-30 : Nom client
//* Pos 31-50 : Ville
//* Pos 51-60 : Code postal
//* Pos 61-70 : Telephone
//* Pos 71-80 : Solde (signe + montant)
//* Pos 81-100: Reserve
//*
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.TP01.CLIENTS) CLUSTER PURGE
  DELETE (FTEST.TP01.AIX.VILLE) ALTERNATEINDEX PURGE
  DELETE (FTEST.TP01.PATH.VILLE) PATH PURGE
  DELETE (FTEST.TP01.AIX.SOLDE) ALTERNATEINDEX PURGE
  DELETE (FTEST.TP01.PATH.SOLDE) PATH PURGE
  DELETE (FTEST.TP01.BACKUP) NONVSAM PURGE
  SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 1: CREATION DU KSDS CLIENTS
//*-------------------------------------------------------------------*
//DEFKSDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER ( -
    NAME(FTEST.TP01.CLIENTS) -
    TRACKS(2 1) -
    VOLUMES(ZASYS1) -
    INDEXED -
    RECORDSIZE(100 100) -
    KEYS(10 0) -
    FREESPACE(15 10) -
    SHAREOPTIONS(2 3)) -
  DATA (NAME(FTEST.TP01.CLIENTS.DATA)) -
  INDEX (NAME(FTEST.TP01.CLIENTS.INDEX))
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2: CHARGEMENT DES DONNEES CLIENTS
//*-------------------------------------------------------------------*
//LOADDATA EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INDATA   DD *
0000000001DUPONT JEAN         STRASBOURG          67000     0388123456+000150000
0000000002MARTIN MARIE        PARIS               75001     0145678901+000025000
0000000003BERNARD PIERRE      STRASBOURG          67100     0388234567+000080000
0000000004DURAND SOPHIE       LYON                69001     0472345678-000015000
0000000005MOREAU PAUL         STRASBOURG          67000     0388345678+000200000
0000000006PETIT CLAIRE        PARIS               75008     0142456789+000032000
0000000007ROUX JACQUES        LYON                69002     0478567890+000095000
0000000008LEROY ANNE          PARIS               75016     0140678901+000055000
0000000009SIMON MARC          MARSEILLE           13001     0491789012+000120000
0000000010LAURENT JULIE       BORDEAUX            33000     0556890123+000075000
/*
//SYSIN    DD *
  REPRO INFILE(INDATA) OUTDATASET(FTEST.TP01.CLIENTS)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 3: AIX SUR VILLE (doublons autorises)
//*-------------------------------------------------------------------*
//DEFAIXV  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX ( -
    NAME(FTEST.TP01.AIX.VILLE) -
    RELATE(FTEST.TP01.CLIENTS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    KEYS(20 30) -
    RECORDSIZE(40 500) -
    UPGRADE -
    NONUNIQUEKEY) -
  DATA (NAME(FTEST.TP01.AIX.VILLE.DATA)) -
  INDEX (NAME(FTEST.TP01.AIX.VILLE.INDEX))

  DEFINE PATH ( -
    NAME(FTEST.TP01.PATH.VILLE) -
    PATHENTRY(FTEST.TP01.AIX.VILLE))

  BLDINDEX -
    INDATASET(FTEST.TP01.CLIENTS) -
    OUTDATASET(FTEST.TP01.AIX.VILLE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 4: AIX SUR SOLDE
//*-------------------------------------------------------------------*
//DEFAIXS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALTERNATEINDEX ( -
    NAME(FTEST.TP01.AIX.SOLDE) -
    RELATE(FTEST.TP01.CLIENTS) -
    TRACKS(1 1) -
    VOLUMES(ZASYS1) -
    KEYS(10 70) -
    RECORDSIZE(30 300) -
    UPGRADE -
    NONUNIQUEKEY) -
  DATA (NAME(FTEST.TP01.AIX.SOLDE.DATA)) -
  INDEX (NAME(FTEST.TP01.AIX.SOLDE.INDEX))

  DEFINE PATH ( -
    NAME(FTEST.TP01.PATH.SOLDE) -
    PATHENTRY(FTEST.TP01.AIX.SOLDE))

  BLDINDEX -
    INDATASET(FTEST.TP01.CLIENTS) -
    OUTDATASET(FTEST.TP01.AIX.SOLDE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 5: AFFICHER LES STATISTIQUES
//*-------------------------------------------------------------------*
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(FTEST.TP01) ALL
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 6: EXPORT VERS FICHIER SEQUENTIEL
//*-------------------------------------------------------------------*
//BACKUP   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SEQFILE  DD DSN=FTEST.TP01.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(2,1)),
//            DCB=(LRECL=100,RECFM=FB,BLKSIZE=0)
//SYSIN    DD *
  REPRO INDATASET(FTEST.TP01.CLIENTS) -
    OUTFILE(SEQFILE)
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 7: AFFICHER LE CONTENU
//*-------------------------------------------------------------------*
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT INDATASET(FTEST.TP01.CLIENTS) CHARACTER
/*
//
