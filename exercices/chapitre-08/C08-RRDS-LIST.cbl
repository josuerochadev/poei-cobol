       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSLST.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Liste complete du fichier RRDS
      *
      * Organisation RELATIVE avec acces SEQUENTIAL pour parcourir
      * Affiche tous les enregistrements du fichier
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FRRDS ASSIGN TO 'RRDS.DAT'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS SEQUENTIAL
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
      * Indicateur fin de fichier
      *----------------------------------------------------------------
       01  WS-FIN-FICHIER         PIC 9 VALUE 0.
           88  FIN-FICHIER        VALUE 1.

      *----------------------------------------------------------------
      * Compteur
      *----------------------------------------------------------------
       01  WS-CPT                 PIC 9(3) VALUE 0.

      *----------------------------------------------------------------
      * Lignes d'affichage
      *----------------------------------------------------------------
       01  WS-SEP                 PIC X(60) VALUE ALL '-'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Liste complete du fichier RRDS'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-LISTER-FICHIER
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
      * Lecture sequentielle de tous les enregistrements
      *----------------------------------------------------------------
       2000-LISTER-FICHIER.
           DISPLAY ' '
           DISPLAY WS-SEP
           DISPLAY ' REC | ID     | NOM        | PRENOM     | DATA'
           DISPLAY WS-SEP

      * Premiere lecture
           READ FRRDS
               AT END SET FIN-FICHIER TO TRUE
           END-READ

      * Boucle de lecture
           PERFORM UNTIL FIN-FICHIER
               ADD 1 TO WS-CPT
               DISPLAY ' ' WS-REL-KEY ' | ' REC-ID ' | '
                   REC-NOM(1:10) ' | ' REC-PRENOM(1:10)
                   ' | ' REC-DATA(1:20)

               READ FRRDS
                   AT END SET FIN-FICHIER TO TRUE
               END-READ
           END-PERFORM

           DISPLAY WS-SEP
           DISPLAY ' '
           DISPLAY 'Total enregistrements lus : ' WS-CPT.

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
