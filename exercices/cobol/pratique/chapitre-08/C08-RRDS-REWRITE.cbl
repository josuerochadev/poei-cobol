       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSRW.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Modification enregistrement N°4 dans fichier RRDS
      *
      * Organisation RELATIVE avec acces RANDOM
      * Lecture puis reecriture (REWRITE) de l'enregistrement
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
           DISPLAY '  Modification RRDS - Enregistrement N.4'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-MODIFIER-ENR-4
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
      * Modification de l'enregistrement N°4
      *----------------------------------------------------------------
       2000-MODIFIER-ENR-4.
           DISPLAY ' '

      * Positionner la cle relative sur 4
           MOVE 4 TO WS-REL-KEY

      * D'abord lire l'enregistrement
           READ FRRDS
               INVALID KEY
                   DISPLAY 'Erreur : Enregistrement 4 non trouve !'
                   DISPLAY 'File Status : ' WS-FS
                   STOP RUN
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.4 AVANT modification :'
                   DISPLAY '  ID      : ' REC-ID
                   DISPLAY '  NOM     : ' REC-NOM
                   DISPLAY '  PRENOM  : ' REC-PRENOM
                   DISPLAY '  DATA    : ' REC-DATA
           END-READ

           DISPLAY ' '
           DISPLAY 'Modification en cours...'
           DISPLAY ' '

      * Modifier les donnees (garder l'ID)
           MOVE 'BERNARD-MODIF' TO REC-NOM
           MOVE 'SOPHIE-MAJ' TO REC-PRENOM
           MOVE 'DONNEES MODIFIEES PAR REWRITE' TO REC-DATA

      * Reecriture
           REWRITE REC-RRDS
               INVALID KEY
                   DISPLAY 'Erreur REWRITE position 4 !'
                   DISPLAY 'File Status : ' WS-FS
               NOT INVALID KEY
                   DISPLAY 'Enregistrement N.4 APRES modification :'
                   DISPLAY '  ID      : ' REC-ID
                   DISPLAY '  NOM     : ' REC-NOM
                   DISPLAY '  PRENOM  : ' REC-PRENOM
                   DISPLAY '  DATA    : ' REC-DATA
           END-REWRITE.

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
