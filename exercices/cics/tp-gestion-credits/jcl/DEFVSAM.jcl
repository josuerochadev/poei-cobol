//DEFVSAM  JOB (ACCT),'DEF VSAM CREDITS',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID
//*********************************************************************
//* JCL     : DEFVSAM
//* Auteur  : Formation CICS
//* Date    : 2024-01
//* 
//* Fonction: Définition des fichiers VSAM pour l'application
//*           de gestion des crédits employés
//*
//* Fichiers créés :
//*   - USER.CICS.EMPLOYE  : Données employés (KSDS)
//*   - USER.CICS.CREDEMP  : Données crédits (KSDS)
//*
//* Prérequis: 
//*   - Espace disponible sur le volume
//*   - Droits IDCAMS
//*********************************************************************
//*
//*-------------------------------------------------------------------*
//* ETAPE 1 : Suppression fichiers existants (si présents)
//*-------------------------------------------------------------------*
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.CICS.EMPLOYE CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0
  DELETE USER.CICS.CREDEMP CLUSTER PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 2 : Définition fichier EMPLOYE (KSDS)
//*-------------------------------------------------------------------*
//DEFEMPL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(USER.CICS.EMPLOYE)             -
           INDEXED                             -
           KEYS(6 0)                           -
           RECORDSIZE(52 52)                   -
           SHAREOPTIONS(2 3)                   -
           FREESPACE(10 10)                    -
           CYLINDERS(1 1)                      -
         )                                     -
         DATA (                                -
           NAME(USER.CICS.EMPLOYE.DATA)        -
         )                                     -
         INDEX (                               -
           NAME(USER.CICS.EMPLOYE.INDEX)       -
         )
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 3 : Définition fichier CRE-EMP (KSDS)
//*-------------------------------------------------------------------*
//DEFCRED  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                             -
           NAME(USER.CICS.CREDEMP)             -
           INDEXED                             -
           KEYS(6 0)                           -
           RECORDSIZE(41 41)                   -
           SHAREOPTIONS(2 3)                   -
           FREESPACE(10 10)                    -
           CYLINDERS(1 1)                      -
         )                                     -
         DATA (                                -
           NAME(USER.CICS.CREDEMP.DATA)        -
         )                                     -
         INDEX (                               -
           NAME(USER.CICS.CREDEMP.INDEX)       -
         )
/*
//*
//*-------------------------------------------------------------------*
//* ETAPE 4 : Vérification (LISTCAT)
//*-------------------------------------------------------------------*
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(USER.CICS.EMPLOYE) ALL
  LISTCAT ENTRIES(USER.CICS.CREDEMP) ALL
/*
//
