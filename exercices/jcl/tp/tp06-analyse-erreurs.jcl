//FTESTTP6 JOB (ACCT),'TP06 ANALYSE ERREURS',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 6 : ANALYSE DES RESULTATS ET LECTURE DES ERREURS
//* ============================================================
//* Objectif : Apprendre a lire et interpreter les erreurs JCL
//*
//* Ce JCL contient plusieurs steps :
//* - Certains avec des erreurs volontaires (commentes)
//* - D'autres pour illustrer les codes retour
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* ============================================================
//*
//* ==========================================================
//* PARTIE 1 : CODES RETOUR NORMAUX
//* ==========================================================
//*
//* ----------------------------------------------------------
//* STEP OK : Code retour 0 (Succes)
//* ----------------------------------------------------------
//STEPOK   EXEC PGM=IEFBR14
//* Ce step retourne RC=0 (succes)
//*
//* ----------------------------------------------------------
//* STEP WARNING : Code retour 4 (Avertissement)
//* ----------------------------------------------------------
//* IEBGENER avec fichier vide retourne souvent RC=4
//STEPWARN EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DUMMY,DCB=(RECFM=FB,LRECL=80)
//SYSUT2   DD SYSOUT=*
//*
//* ----------------------------------------------------------
//* STEP AVEC RC=8 : IDCAMS DELETE fichier inexistant
//* ----------------------------------------------------------
//STEPRC8  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
* Tentative de suppression d'un fichier qui n'existe pas
  DELETE FTEST.FICHIER.INEXISTANT
/*
//*
//* ==========================================================
//* PARTIE 2 : GESTION DES ERREURS AVEC IF/THEN
//* ==========================================================
//*
//IFTEST   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.TEST.ERREUR
  IF LASTCC = 8 THEN DO
    SET MAXCC = 0
  END
/*
//*
//* ==========================================================
//* PARTIE 3 : EXEMPLES D'ERREURS COURANTES (COMMENTES)
//* ==========================================================
//*
//* Decommentez ces sections une par une pour voir les erreurs
//*
//* ----------------------------------------------------------
//* ERREUR 1 : Dataset inexistant (IEF212I)
//* ----------------------------------------------------------
//* Decommentez pour tester :
//*
//*//ERRDSNX  EXEC PGM=IEBGENER
//*//SYSPRINT DD SYSOUT=*
//*//SYSIN    DD DUMMY
//*//SYSUT1   DD DSN=FTEST.NEXISTE.PAS,DISP=SHR
//*//SYSUT2   DD SYSOUT=*
//*
//* Message attendu : IEF212I ... DATA SET NOT FOUND
//*
//* ----------------------------------------------------------
//* ERREUR 2 : Programme inexistant (S806)
//* ----------------------------------------------------------
//* Decommentez pour tester :
//*
//*//ERRPGM   EXEC PGM=PROGRAMM
//*
//* Message attendu : S806 ABEND - MODULE NOT FOUND
//*
//* ----------------------------------------------------------
//* ERREUR 3 : Erreur syntaxe JCL (JCL ERROR)
//* ----------------------------------------------------------
//* Decommentez pour tester :
//*
//*//ERRSYN   EXEC PGM=IEFBR14
//*//BADDD    DD DSN=FTEST.TEST,DISP=(NEW,CATLG)
//*//            SPACE=(TRK,5),UNIT=SYSDA
//*
//* Erreur : continuation incorrecte (pas de virgule)
//*
//* ----------------------------------------------------------
//* ERREUR 4 : Dataset deja existant
//* ----------------------------------------------------------
//* Decommentez pour tester (si le dataset existe deja) :
//*
//*//ERREXIST EXEC PGM=IEFBR14
//*//NEWDS    DD DSN=SYS1.PARMLIB,
//*//            DISP=(NEW,CATLG),
//*//            SPACE=(TRK,(1,1)),
//*//            UNIT=SYSDA
//*
//* Message attendu : IEF341I ... DUPLICATE DATA SET NAME
//*
//* ==========================================================
//* PARTIE 4 : UTILISATION DE TYPRUN
//* ==========================================================
//*
//* Pour verifier un JCL sans l'executer, ajoutez TYPRUN=SCAN
//* sur la carte JOB :
//*
//* //FTESTTP6 JOB (ACCT),'TP06 ANALYSE',TYPRUN=SCAN,...
//*
//* Le JCL sera verifie syntaxiquement mais pas execute.
//*
//* ==========================================================
//* PARTIE 5 : STEP FINAL - RAPPORT
//* ==========================================================
//*
//RAPPORT  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
================================================================
           RAPPORT TP06 - ANALYSE DES ERREURS
================================================================

CODES RETOUR OBSERVES DANS CE JOB :
-----------------------------------
STEPOK   : RC=0  (Succes normal)
STEPWARN : RC=4  (Warning - fichier vide)
STEPRC8  : RC=8  (Erreur - fichier inexistant)
IFTEST   : RC=0  (Erreur masquee par IF/THEN)

COMMENT ANALYSER UN JOB :
-------------------------
1. Consulter JESMSGLG pour les messages systeme
2. Consulter JESYSMSG pour les messages JES
3. Consulter SYSPRINT de chaque step pour les details
4. Verifier le MAXCC (Max Condition Code) du job

CODES RETOUR COURANTS :
-----------------------
RC=0  : Succes
RC=4  : Warning (traitement OK avec avertissement)
RC=8  : Erreur (traitement partiel ou echoue)
RC=12 : Erreur grave
RC=16 : Erreur fatale

================================================================
/*
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=81)
//
