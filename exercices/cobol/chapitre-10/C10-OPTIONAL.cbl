       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10OPTIONAL.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-OPTIONAL
      * OBJET     : Démonstration de la clause OPTIONAL
      *             Le fichier CONFIG peut ne pas exister
      * EXERCICE  : Chapitre X - Traitement des fichiers
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * Fichier OPTIONAL : peut ne pas exister à l'ouverture
      *----------------------------------------------------------------*
           SELECT OPTIONAL F-CONFIG
               ASSIGN TO 'CONFIG.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-CFG.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Fichier de configuration (optionnel)
      *----------------------------------------------------------------*
       FD  F-CONFIG
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-CONFIG.
           05  CFG-PARAMETRE      PIC X(20).
           05  CFG-VALEUR         PIC X(30).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01  WS-STATUS-CFG          PIC XX.
           88 CFG-OK              VALUE '00'.
           88 CFG-OPTIONAL-EMPTY  VALUE '05'.
           88 CFG-NOT-FOUND       VALUE '35'.
           88 CFG-EOF             VALUE '10'.


      *----------------------------------------------------------------*
      * Variables de contrôle
      *----------------------------------------------------------------*
       01  WS-FIN-CONFIG          PIC X VALUE 'N'.
           88 FIN-CONFIG          VALUE 'O'.

       01  WS-CONFIG-EXISTE       PIC X VALUE 'N'.
           88 CONFIG-EXISTE       VALUE 'O'.

      *----------------------------------------------------------------*
      * Valeurs par défaut si pas de fichier CONFIG
      *----------------------------------------------------------------*
       01  WS-DEFAUT-PARAM1       PIC X(30) VALUE 'VALEUR-DEFAUT-1'.
       01  WS-DEFAUT-PARAM2       PIC X(30) VALUE 'VALEUR-DEFAUT-2'.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '     DEMONSTRATION CLAUSE OPTIONAL                 '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-CONFIG
           PERFORM 2000-AFFICHER-CONFIG
           PERFORM 3000-FERMER-FICHIERS

           DISPLAY ' '
           DISPLAY 'Fin du programme C10-OPTIONAL'
           STOP RUN.

      *----------------------------------------------------------------*
      * Ouverture du fichier CONFIG (OPTIONAL)
      *----------------------------------------------------------------*
       1000-OUVRIR-CONFIG.
           DISPLAY '--- Tentative ouverture fichier CONFIG ---'

           OPEN INPUT F-CONFIG

           EVALUATE TRUE
               WHEN CFG-OK
                   DISPLAY 'Fichier CONFIG trouve et ouvert'
                   SET CONFIG-EXISTE TO TRUE
               WHEN CFG-OPTIONAL-EMPTY
                   DISPLAY 'Fichier CONFIG non present (STATUS 05)'
                   DISPLAY '-> Utilisation des valeurs par defaut'
               WHEN CFG-NOT-FOUND
                   DISPLAY 'Fichier CONFIG non trouve (FILE STATUS 35)'
                   DISPLAY '-> Ce cas ne devrait pas arriver avec OPTIONAL'
               WHEN OTHER
                   DISPLAY 'Erreur ouverture CONFIG : ' WS-STATUS-CFG
           END-EVALUATE.

      *----------------------------------------------------------------*
      * Affichage de la configuration
      *----------------------------------------------------------------*
       2000-AFFICHER-CONFIG.
           DISPLAY ' '
           DISPLAY '--- Configuration active ---'

           IF CONFIG-EXISTE
               PERFORM 2100-LIRE-CONFIG
           ELSE
               DISPLAY 'PARAM1 : ' WS-DEFAUT-PARAM1
               DISPLAY 'PARAM2 : ' WS-DEFAUT-PARAM2
           END-IF.

      *----------------------------------------------------------------*
      * Lecture séquentielle du fichier CONFIG
      *----------------------------------------------------------------*
       2100-LIRE-CONFIG.
           PERFORM UNTIL FIN-CONFIG
               READ F-CONFIG
                   AT END
                       SET FIN-CONFIG TO TRUE
                   NOT AT END
                       DISPLAY CFG-PARAMETRE ' : ' CFG-VALEUR
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
      * Fermeture des fichiers
      *----------------------------------------------------------------*
       3000-FERMER-FICHIERS.
           IF CONFIG-EXISTE
               CLOSE F-CONFIG
           END-IF.

