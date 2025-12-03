       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSRD.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Lecture enregistrement N°6 dans fichier RRDS
      *
      * Organisation RELATIVE avec acces RANDOM
      * Lecture directe par numero d'enregistrement (RELATIVE KEY)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FRRDS ASSIGN TO 'RRDS.DAT'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS WS-REL-KEY
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  FRRDS
           RECORD CONTAINS 80 CHARACTERS.
       01  REC-RRDS.
           05  REC-ID             PIC X(06).
           05  REC-NOM            PIC X(20).
           05  REC-PRENOM         PIC X(20).
           05  REC-DATA           PIC X(34).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * FILE STATUS et RELATIVE KEY
      *----------------------------------------------------------------
       01  WS-FS                  PIC XX.
       01  WS-REL-KEY             PIC 9(4) VALUE 0.

      *----------------------------------------------------------------
      * Zone de travail
      *----------------------------------------------------------------
       01  WS-REC.
           05  WS-ID              PIC X(06).
           05  WS-NOM             PIC X(20).
           05  WS-PRENOM          PIC X(20).
           05  WS-DATA            PIC X(34).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Lecture RRDS - Enregistrement N.6'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-LIRE-ENR-6
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en INPUT (lecture)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN INPUT FRRDS

           EVALUATE WS-FS
               WHEN '00'
                   DISPLAY 'Fichier RRDS.DAT ouvert en lecture.'
               WHEN '35'
                   DISPLAY 'Erreur : Fichier RRDS.DAT non trouve !'
                   DISPLAY 'Executez d''abord C08-RRDS-WRITE'
                   STOP RUN
               WHEN OTHER
                   DISPLAY 'Erreur ouverture fichier : ' WS-FS
                   STOP RUN
           END-EVALUATE.

      *----------------------------------------------------------------
      * Lecture de l'enregistrement N°6
      *----------------------------------------------------------------
       2000-LIRE-ENR-6.
           DISPLAY ' '
           DISPLAY 'Lecture enregistrement numero 6...'
           DISPLAY ' '

      * Positionner la cle relative sur 6
           MOVE 6 TO WS-REL-KEY

      * Lecture directe
           READ FRRDS INTO WS-REC
               INVALID KEY
                   DISPLAY 'Erreur : Enregistrement 6 non trouve !'
                   DISPLAY 'File Status : ' WS-FS
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.6 trouve :'
                   DISPLAY '  ID      : ' WS-ID
                   DISPLAY '  NOM     : ' WS-NOM
                   DISPLAY '  PRENOM  : ' WS-PRENOM
                   DISPLAY '  DATA    : ' WS-DATA
           END-READ.

      *----------------------------------------------------------------
      * Fermeture
      *----------------------------------------------------------------
       3000-FERMER-FICHIER.
           CLOSE FRRDS

           IF WS-FS = '00'
               DISPLAY ' '
               DISPLAY 'Fichier ferme avec succes.'
           ELSE
               DISPLAY 'Erreur fermeture : ' WS-FS
           END-IF.
