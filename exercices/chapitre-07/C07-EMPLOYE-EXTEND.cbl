       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYE-EXTEND.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Ajout en fin de fichier EMPLOYE (EXTEND)
      *
      * Ce programme ajoute un employe a la fin du fichier
      * sans ecraser les enregistrements existants.
      * Organisation : SEQUENTIAL
      * Mode d'acces : SEQUENTIAL
      * Mode OPEN   : EXTEND
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYE ASSIGN TO 'EMPLOYE.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYE
           RECORDING MODE IS F
           RECORD CONTAINS 21 CHARACTERS.
       01  ENR-EMPLOYE.
           05  ID-EMPLOYE          PIC 9(3).
           05  NAME-EMPLOYE        PIC A(15).
           05  TITRE-EMPLOYE       PIC X(3).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * FILE STATUS
      *----------------------------------------------------------------
       01  WS-FS                   PIC XX.

      *----------------------------------------------------------------
      * Zone de travail
      *----------------------------------------------------------------
       01  WS-EMPLOYE.
           05  WS-ID               PIC 9(3).
           05  WS-NAME             PIC A(15).
           05  WS-TITRE            PIC X(3).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Ajout fichier EMPLOYE (EXTEND)'
           DISPLAY '=================================================='
           DISPLAY ' '

      * Ouverture en EXTEND (ajout en fin)
           OPEN EXTEND EMPLOYE

           EVALUATE WS-FS
               WHEN '00'
                   DISPLAY 'Fichier EMPLOYE.DAT ouvert en EXTEND.'
               WHEN '35'
                   DISPLAY 'Fichier non trouve, creation...'
                   CLOSE EMPLOYE
                   OPEN OUTPUT EMPLOYE
               WHEN OTHER
                   DISPLAY 'Erreur ouverture : ' WS-FS
                   STOP RUN
           END-EVALUATE

      * Ajout de l'employe (exemple de l'enonce)
           DISPLAY ' '
           DISPLAY 'Ajout d''un nouvel employe...'

           MOVE 560 TO ID-EMPLOYE
           MOVE 'JEAN' TO NAME-EMPLOYE
           MOVE '120' TO TITRE-EMPLOYE

           WRITE ENR-EMPLOYE
           END-WRITE

           IF WS-FS = '00'
               DISPLAY '  Ajoute : ' ID-EMPLOYE ' - '
                   NAME-EMPLOYE ' - ' TITRE-EMPLOYE
           ELSE
               DISPLAY '  Erreur ecriture : ' WS-FS
           END-IF

      * Fermeture
           CLOSE EMPLOYE

           DISPLAY ' '
           DISPLAY 'Fichier ferme.'
           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.
