       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSADD.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Ecriture enregistrement N°13 dans fichier RRDS
      *
      * Organisation RELATIVE avec acces RANDOM
      * Ajout d'un enregistrement a une position specifique
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
           DISPLAY '  Ecriture RRDS - Enregistrement N.13'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-ECRIRE-ENR-13
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
      * Ecriture de l'enregistrement N°13
      *----------------------------------------------------------------
       2000-ECRIRE-ENR-13.
           DISPLAY ' '
           DISPLAY 'Ecriture enregistrement numero 13...'
           DISPLAY ' '

      * Positionner la cle relative sur 13
           MOVE 13 TO WS-REL-KEY

      * Preparer les donnees
           MOVE '000013' TO REC-ID
           MOVE 'NOUVEAU' TO REC-NOM
           MOVE 'EMPLOYE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 13 - AJOUTE' TO REC-DATA

      * Ecriture a la position 13
           WRITE REC-RRDS
               INVALID KEY
                   DISPLAY 'Erreur ecriture position 13 !'
                   DISPLAY 'File Status : ' WS-FS
                   DISPLAY '(22 = enregistrement existe deja)'
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.13 ecrit avec succes :'
                   DISPLAY '  ID      : ' REC-ID
                   DISPLAY '  NOM     : ' REC-NOM
                   DISPLAY '  PRENOM  : ' REC-PRENOM
                   DISPLAY '  DATA    : ' REC-DATA
           END-WRITE.

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
