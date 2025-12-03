       IDENTIFICATION DIVISION.
       PROGRAM-ID. C07-EMPWRITE.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Ecriture fichier EMPLOYE (SEQUENTIAL)
      *
      * Ce programme cree et remplit un fichier EMPLOYE
      * avec plusieurs enregistrements.
      * Organisation : SEQUENTIAL
      * Mode d'acces : SEQUENTIAL
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

      *----------------------------------------------------------------
      * Compteur
      *----------------------------------------------------------------
       01  WS-CPT-ECRITS           PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Ecriture fichier EMPLOYE (SEQUENTIAL)'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-ECRIRE-EMPLOYES
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en OUTPUT (creation/ecrasement)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN OUTPUT EMPLOYE

           IF WS-FS NOT = '00'
               DISPLAY 'Erreur ouverture fichier : ' WS-FS
               STOP RUN
           END-IF

           DISPLAY 'Fichier EMPLOYE.DAT ouvert en ecriture.'.

      *----------------------------------------------------------------
      * Ecriture de plusieurs employes
      *----------------------------------------------------------------
       2000-ECRIRE-EMPLOYES.
           DISPLAY ' '
           DISPLAY 'Ecriture des employes...'
           DISPLAY ' '

      * Employe 1
           MOVE 001 TO ID-EMPLOYE
           MOVE 'DUPONT' TO NAME-EMPLOYE
           MOVE 'DEV' TO TITRE-EMPLOYE
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 2
           MOVE 002 TO ID-EMPLOYE
           MOVE 'MARTIN' TO NAME-EMPLOYE
           MOVE 'MGR' TO TITRE-EMPLOYE
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 3
           MOVE 003 TO ID-EMPLOYE
           MOVE 'DURAND' TO NAME-EMPLOYE
           MOVE 'DBA' TO TITRE-EMPLOYE
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 4 (exemple de l'enonce)
           MOVE 560 TO ID-EMPLOYE
           MOVE 'JEAN' TO NAME-EMPLOYE
           MOVE '120' TO TITRE-EMPLOYE
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 5
           MOVE 999 TO ID-EMPLOYE
           MOVE 'BERNARD' TO NAME-EMPLOYE
           MOVE 'CEO' TO TITRE-EMPLOYE
           PERFORM 2100-ECRIRE-UN-EMPLOYE

           DISPLAY ' '
           DISPLAY 'Total employes ecrits : ' WS-CPT-ECRITS.

      *----------------------------------------------------------------
      * Ecriture d'un employe
      *----------------------------------------------------------------
       2100-ECRIRE-UN-EMPLOYE.
           WRITE ENR-EMPLOYE
           END-WRITE

           IF WS-FS = '00'
               ADD 1 TO WS-CPT-ECRITS
               DISPLAY '  Ecrit : ' ID-EMPLOYE ' - '
                   NAME-EMPLOYE ' - ' TITRE-EMPLOYE
           ELSE
               DISPLAY '  Erreur ecriture : ' WS-FS
           END-IF.

      *----------------------------------------------------------------
      * Fermeture
      *----------------------------------------------------------------
       3000-FERMER-FICHIER.
           CLOSE EMPLOYE

           IF WS-FS = '00'
               DISPLAY ' '
               DISPLAY 'Fichier ferme avec succes.'
           ELSE
               DISPLAY 'Erreur fermeture : ' WS-FS
           END-IF.
