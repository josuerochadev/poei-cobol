       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSDEL.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Suppression enregistrement N°3 dans fichier RRDS
      *
      * Organisation RELATIVE avec acces RANDOM
      * Suppression d'un enregistrement par DELETE
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

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Suppression RRDS - Enregistrement N.3'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-SUPPRIMER-ENR-3
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en I-O (lecture/ecriture)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN I-O FRRDS

           EVALUATE WS-FS
               WHEN '00'
                   DISPLAY 'Fichier RRDS.DAT ouvert en I-O.'
               WHEN '35'
                   DISPLAY 'Erreur : Fichier RRDS.DAT non trouve !'
                   DISPLAY 'Executez d''abord C08-RRDS-WRITE'
                   STOP RUN
               WHEN OTHER
                   DISPLAY 'Erreur ouverture fichier : ' WS-FS
                   STOP RUN
           END-EVALUATE.

      *----------------------------------------------------------------
      * Suppression de l'enregistrement N°3
      *----------------------------------------------------------------
       2000-SUPPRIMER-ENR-3.
           DISPLAY ' '

      * Positionner la cle relative sur 3
           MOVE 3 TO WS-REL-KEY

      * D'abord lire l'enregistrement pour afficher son contenu
           READ FRRDS
               INVALID KEY
                   DISPLAY 'Enregistrement 3 non trouve ou deja supp.'
                   DISPLAY 'File Status : ' WS-FS
                   STOP RUN
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.3 a supprimer :'
                   DISPLAY '  ID      : ' REC-ID
                   DISPLAY '  NOM     : ' REC-NOM
                   DISPLAY '  PRENOM  : ' REC-PRENOM
                   DISPLAY '  DATA    : ' REC-DATA
           END-READ

           DISPLAY ' '
           DISPLAY 'Suppression en cours...'
           DISPLAY ' '

      * Suppression
           DELETE FRRDS
               INVALID KEY
                   DISPLAY 'Erreur DELETE position 3 !'
                   DISPLAY 'File Status : ' WS-FS
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.3 supprime avec succes.'
           END-DELETE.

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
