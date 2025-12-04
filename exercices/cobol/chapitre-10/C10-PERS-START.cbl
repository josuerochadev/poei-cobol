       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSST.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-START
      * OBJET     : Lecture de l'enregistrement le plus proche
      *             - Affecter une cle inexistante
      *             - Utiliser START pour trouver le plus proche
      *             - Fonctionne sur cle primaire et secondaire
      * EXERCICE  : TP Chapitre X - Exercice 8
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PERSONNEL
               ASSIGN TO 'PERSONNEL.KSDS'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ENR-MATRICULE
               ALTERNATE RECORD KEY IS ENR-NUM-SS WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD F-PERSONNEL
           RECORDING MODE IS F
           RECORD CONTAINS 66 CHARACTERS.
       01 ENR-PERSONNEL.
          05 ENR-MATRICULE         PIC 9(6).
          05 ENR-NOM               PIC X(15).
          05 ENR-PRENOM            PIC X(15).
          05 ENR-SALAIRE           PIC 9(6).
          05 ENR-PRIMES            PIC 9(6).
          05 ENR-REVENU-ANNUEL     PIC 9(8).
          05 ENR-NUM-SS            PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX.
          88 WS-OK                 VALUE '00'.
          88 WS-NOT-FOUND          VALUE '23'.
          88 WS-EOF                VALUE '10'.

       01 WS-CHOIX                 PIC 9 VALUE 0.
       01 WS-CLE-RECHERCHE         PIC X(10).
       01 WS-CONTINUER             PIC X VALUE 'O'.
          88 CONTINUER             VALUE 'O' 'o'.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 8 : LECTURE ENREGISTREMENT LE PLUS     '
           DISPLAY '  PROCHE (START >= cle inexistante)               '
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN INPUT F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL NOT CONTINUER
               PERFORM 1000-MENU-RECHERCHE
               DISPLAY ' '
               DISPLAY 'Autre recherche ? (O/N) : '
               ACCEPT WS-CONTINUER
           END-PERFORM

           CLOSE F-PERSONNEL
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Menu de choix de la cle
      *----------------------------------------------------------------*
       1000-MENU-RECHERCHE.
           DISPLAY ' '
           DISPLAY '--- Type de cle ---'
           DISPLAY '1. Recherche par MATRICULE (cle primaire)'
           DISPLAY '2. Recherche par N° SS (cle secondaire)'
           DISPLAY 'Choix : '
           ACCEPT WS-CHOIX

           EVALUATE WS-CHOIX
               WHEN 1
                   PERFORM 2000-RECHERCHE-MATRICULE
               WHEN 2
                   PERFORM 3000-RECHERCHE-NUMSS
               WHEN OTHER
                   DISPLAY 'Choix invalide'
           END-EVALUATE.

      *----------------------------------------------------------------*
      * Recherche par matricule (cle primaire)
      *----------------------------------------------------------------*
       2000-RECHERCHE-MATRICULE.
           DISPLAY ' '
           DISPLAY 'Matricule a rechercher (6 chiffres) : '
           DISPLAY '(Essayez une valeur inexistante, ex: 000007)'
           ACCEPT WS-CLE-RECHERCHE

           MOVE WS-CLE-RECHERCHE TO ENR-MATRICULE

      *    D'abord, verifier si la cle exacte existe
           READ F-PERSONNEL KEY IS ENR-MATRICULE
               INVALID KEY
                   DISPLAY ' '
                   DISPLAY '*** Matricule ' WS-CLE-RECHERCHE
                           ' non trouve ***'
                   DISPLAY 'Recherche du plus proche (START >=)...'
                   PERFORM 2100-START-MATRICULE
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY '*** Matricule trouve exactement ***'
                   PERFORM 9100-AFFICHER-SALARIE
           END-READ.

      *----------------------------------------------------------------*
      * START sur cle primaire
      *----------------------------------------------------------------*
       2100-START-MATRICULE.
           MOVE WS-CLE-RECHERCHE TO ENR-MATRICULE

           START F-PERSONNEL KEY >= ENR-MATRICULE
               INVALID KEY
                   DISPLAY '*** Aucun matricule >= ' WS-CLE-RECHERCHE
                           ' ***'
               NOT INVALID KEY
                   READ F-PERSONNEL NEXT
                       AT END
                           DISPLAY '*** Fin de fichier ***'
                       NOT AT END
                           DISPLAY ' '
                           DISPLAY '--- Enregistrement le plus proche ---'
                           DISPLAY '(Premier matricule >= '
                                   WS-CLE-RECHERCHE ')'
                           PERFORM 9100-AFFICHER-SALARIE
                   END-READ
           END-START.

      *----------------------------------------------------------------*
      * Recherche par N° SS (cle secondaire)
      *----------------------------------------------------------------*
       3000-RECHERCHE-NUMSS.
           DISPLAY ' '
           DISPLAY 'N° SS a rechercher (10 chiffres) : '
           DISPLAY '(Essayez une valeur inexistante, ex: 1900000000)'
           ACCEPT WS-CLE-RECHERCHE

           MOVE WS-CLE-RECHERCHE TO ENR-NUM-SS

      *    D'abord, verifier si la cle exacte existe
           READ F-PERSONNEL KEY IS ENR-NUM-SS
               INVALID KEY
                   DISPLAY ' '
                   DISPLAY '*** N° SS ' WS-CLE-RECHERCHE
                           ' non trouve ***'
                   DISPLAY 'Recherche du plus proche (START >=)...'
                   PERFORM 3100-START-NUMSS
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY '*** N° SS trouve exactement ***'
                   PERFORM 9100-AFFICHER-SALARIE
           END-READ.

      *----------------------------------------------------------------*
      * START sur cle secondaire
      *----------------------------------------------------------------*
       3100-START-NUMSS.
           MOVE WS-CLE-RECHERCHE TO ENR-NUM-SS

           START F-PERSONNEL KEY >= ENR-NUM-SS
               INVALID KEY
                   DISPLAY '*** Aucun N° SS >= ' WS-CLE-RECHERCHE
                           ' ***'
               NOT INVALID KEY
                   READ F-PERSONNEL NEXT
                       AT END
                           DISPLAY '*** Fin de fichier ***'
                       NOT AT END
                           DISPLAY ' '
                           DISPLAY '--- Enregistrement le plus proche ---'
                           DISPLAY '(Premier N° SS >= '
                                   WS-CLE-RECHERCHE ')'
                           PERFORM 9100-AFFICHER-SALARIE
                   END-READ
           END-START.

      *----------------------------------------------------------------*
      * Affichage d'un salarie
      *----------------------------------------------------------------*
       9100-AFFICHER-SALARIE.
           DISPLAY 'Matricule : ' ENR-MATRICULE
           DISPLAY 'Nom       : ' ENR-NOM
           DISPLAY 'Prenom    : ' ENR-PRENOM
           DISPLAY 'N° SS     : ' ENR-NUM-SS
           DISPLAY 'Salaire   : ' ENR-SALAIRE.

