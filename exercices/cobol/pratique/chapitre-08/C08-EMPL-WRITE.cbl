       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-EMPWRT.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Ecriture fichier EMPLOYE etendu
      *
      * Structure de l'enregistrement :
      * - ID-EMP     : 6 caracteres (identifiant)
      * - NOM-EMP    : 20 caracteres
      * - PRENOM-EMP : 20 caracteres
      * - ADR-EMP    : 40 caracteres (adresse)
      * - DEBIT-EMP  : 10 num (8 entiers + 2 decimales)
      * - CREDIT-EMP : 10 num (8 entiers + 2 decimales)
      * - SALAIRE-EMP: 10 num (8 entiers + 2 decimales)
      * Total : 6 + 20 + 20 + 40 + 10 + 10 + 10 = 116 caracteres
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEMPL ASSIGN TO 'EMPLOYE-EXT.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  FEMPL
           RECORDING MODE IS F
           RECORD CONTAINS 116 CHARACTERS.
       01  REC-EMPL.
           05  ID-EMP             PIC X(06).
           05  NOM-EMP            PIC X(20).
           05  PRENOM-EMP         PIC X(20).
           05  ADR-EMP            PIC X(40).
           05  DEBIT-EMP          PIC 9(8)V99.
           05  CREDIT-EMP         PIC 9(8)V99.
           05  SALAIRE-EMP        PIC 9(8)V99.

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * FILE STATUS
      *----------------------------------------------------------------
       01  WS-FS                  PIC XX.

      *----------------------------------------------------------------
      * Compteur
      *----------------------------------------------------------------
       01  WS-CPT-ECRITS          PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Creation fichier EMPLOYE etendu'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-ECRIRE-EMPLOYES
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en OUTPUT (creation)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN OUTPUT FEMPL

           IF WS-FS NOT = '00'
               DISPLAY 'Erreur ouverture fichier : ' WS-FS
               STOP RUN
           END-IF

           DISPLAY 'Fichier EMPLOYE-EXT.DAT ouvert en ecriture.'.

      *----------------------------------------------------------------
      * Ecriture de plusieurs employes
      *----------------------------------------------------------------
       2000-ECRIRE-EMPLOYES.
           DISPLAY ' '
           DISPLAY 'Ecriture des employes...'
           DISPLAY ' '

      * Employe 1
           MOVE '000001' TO ID-EMP
           MOVE 'DUPONT' TO NOM-EMP
           MOVE 'JEAN' TO PRENOM-EMP
           MOVE '12 RUE DE PARIS 75001 PARIS' TO ADR-EMP
           MOVE 1500.50 TO DEBIT-EMP
           MOVE 3200.75 TO CREDIT-EMP
           MOVE 2850.00 TO SALAIRE-EMP
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 2
           MOVE '000002' TO ID-EMP
           MOVE 'MARTIN' TO NOM-EMP
           MOVE 'MARIE' TO PRENOM-EMP
           MOVE '45 AVENUE VICTOR HUGO 69002 LYON' TO ADR-EMP
           MOVE 850.25 TO DEBIT-EMP
           MOVE 4500.00 TO CREDIT-EMP
           MOVE 3200.50 TO SALAIRE-EMP
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 3
           MOVE '000003' TO ID-EMP
           MOVE 'DURAND' TO NOM-EMP
           MOVE 'PIERRE' TO PRENOM-EMP
           MOVE '8 PLACE BELLECOUR 69001 LYON' TO ADR-EMP
           MOVE 2300.00 TO DEBIT-EMP
           MOVE 5600.80 TO CREDIT-EMP
           MOVE 4100.00 TO SALAIRE-EMP
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 4
           MOVE '000004' TO ID-EMP
           MOVE 'BERNARD' TO NOM-EMP
           MOVE 'SOPHIE' TO PRENOM-EMP
           MOVE '23 RUE GARIBALDI 13001 MARSEILLE' TO ADR-EMP
           MOVE 1200.00 TO DEBIT-EMP
           MOVE 2800.50 TO CREDIT-EMP
           MOVE 2500.00 TO SALAIRE-EMP
           PERFORM 2100-ECRIRE-UN-EMPLOYE

      * Employe 5
           MOVE '000005' TO ID-EMP
           MOVE 'PETIT' TO NOM-EMP
           MOVE 'LUCAS' TO PRENOM-EMP
           MOVE '67 BOULEVARD CARNOT 31000 TOULOUSE' TO ADR-EMP
           MOVE 3500.75 TO DEBIT-EMP
           MOVE 8200.25 TO CREDIT-EMP
           MOVE 5500.00 TO SALAIRE-EMP
           PERFORM 2100-ECRIRE-UN-EMPLOYE

           DISPLAY ' '
           DISPLAY 'Total employes ecrits : ' WS-CPT-ECRITS.

      *----------------------------------------------------------------
      * Ecriture d'un employe
      *----------------------------------------------------------------
       2100-ECRIRE-UN-EMPLOYE.
           WRITE REC-EMPL
           END-WRITE

           IF WS-FS = '00'
               ADD 1 TO WS-CPT-ECRITS
               DISPLAY '  Ecrit : ' ID-EMP ' ' NOM-EMP ' ' PRENOM-EMP
           ELSE
               DISPLAY '  Erreur ecriture : ' WS-FS
           END-IF.

      *----------------------------------------------------------------
      * Fermeture
      *----------------------------------------------------------------
       3000-FERMER-FICHIER.
           CLOSE FEMPL

           IF WS-FS = '00'
               DISPLAY ' '
               DISPLAY 'Fichier ferme avec succes.'
           ELSE
               DISPLAY 'Erreur fermeture : ' WS-FS
           END-IF.
