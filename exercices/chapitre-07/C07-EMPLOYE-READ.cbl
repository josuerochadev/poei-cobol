       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYE-READ.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Lecture fichier EMPLOYE (SEQUENTIAL)
      *
      * Ce programme lit et affiche tous les enregistrements
      * du fichier EMPLOYE.
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
      * Indicateur fin de fichier
      *----------------------------------------------------------------
       01  WS-FIN-FICHIER          PIC 9 VALUE 0.
           88  FIN-FICHIER         VALUE 1.

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
       01  WS-CPT-LUS              PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Lecture fichier EMPLOYE (SEQUENTIAL)'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-LIRE-EMPLOYES
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en INPUT (lecture)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN INPUT EMPLOYE

           EVALUATE WS-FS
               WHEN '00'
                   DISPLAY 'Fichier EMPLOYE.DAT ouvert en lecture.'
               WHEN '35'
                   DISPLAY 'Erreur : Fichier EMPLOYE.DAT non trouve !'
                   DISPLAY 'Executez d''abord C07-EMPLOYE-WRITE'
                   STOP RUN
               WHEN OTHER
                   DISPLAY 'Erreur ouverture fichier : ' WS-FS
                   STOP RUN
           END-EVALUATE.

      *----------------------------------------------------------------
      * Lecture de tous les employes
      *----------------------------------------------------------------
       2000-LIRE-EMPLOYES.
           DISPLAY ' '
           DISPLAY '--------------------------------------'
           DISPLAY ' ID  | NOM             | TITRE'
           DISPLAY '--------------------------------------'

      * Premiere lecture
           READ EMPLOYE INTO WS-EMPLOYE
               AT END SET FIN-FICHIER TO TRUE
           END-READ

      * Boucle de lecture
           PERFORM UNTIL FIN-FICHIER
               ADD 1 TO WS-CPT-LUS
               DISPLAY ' ' ID-EMPLOYE ' | ' NAME-EMPLOYE
                   ' | ' TITRE-EMPLOYE

               READ EMPLOYE INTO WS-EMPLOYE
                   AT END SET FIN-FICHIER TO TRUE
               END-READ
           END-PERFORM

           DISPLAY '--------------------------------------'
           DISPLAY ' '
           DISPLAY 'Total employes lus : ' WS-CPT-LUS.

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
