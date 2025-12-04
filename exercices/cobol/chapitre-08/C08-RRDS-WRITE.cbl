       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-RRDSWRT.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Creation fichier RRDS (Relative Record Data Set)
      *
      * Organisation RELATIVE avec acces SEQUENTIAL pour creation
      * Chargement de 10 enregistrements minimum
      * Structure : ID(6) + NOM(20) + PRENOM(20) + DATA(34) = 80 car.
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
      * Compteur
      *----------------------------------------------------------------
       01  WS-CPT                 PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Creation fichier RRDS - 10 enregistrements'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-ECRIRE-ENREGISTREMENTS
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en OUTPUT (creation)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN OUTPUT FRRDS

           IF WS-FS NOT = '00'
               DISPLAY 'Erreur ouverture fichier : ' WS-FS
               STOP RUN
           END-IF

           DISPLAY 'Fichier RRDS.DAT cree.'.

      *----------------------------------------------------------------
      * Ecriture des 10 enregistrements
      *----------------------------------------------------------------
       2000-ECRIRE-ENREGISTREMENTS.
           DISPLAY ' '
           DISPLAY 'Ecriture des enregistrements...'
           DISPLAY ' '

      * Enregistrement 1
           MOVE '000001' TO REC-ID
           MOVE 'DUPONT' TO REC-NOM
           MOVE 'JEAN' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 01' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 2
           MOVE '000002' TO REC-ID
           MOVE 'MARTIN' TO REC-NOM
           MOVE 'MARIE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 02' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 3
           MOVE '000003' TO REC-ID
           MOVE 'DURAND' TO REC-NOM
           MOVE 'PIERRE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 03' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 4
           MOVE '000004' TO REC-ID
           MOVE 'BERNARD' TO REC-NOM
           MOVE 'SOPHIE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 04' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 5
           MOVE '000005' TO REC-ID
           MOVE 'PETIT' TO REC-NOM
           MOVE 'LUCAS' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 05' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 6
           MOVE '000006' TO REC-ID
           MOVE 'MOREAU' TO REC-NOM
           MOVE 'CLAIRE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 06' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 7
           MOVE '000007' TO REC-ID
           MOVE 'LEFEBVRE' TO REC-NOM
           MOVE 'THOMAS' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 07' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 8
           MOVE '000008' TO REC-ID
           MOVE 'LEROY' TO REC-NOM
           MOVE 'JULIE' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 08' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 9
           MOVE '000009' TO REC-ID
           MOVE 'ROUX' TO REC-NOM
           MOVE 'NICOLAS' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 09' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

      * Enregistrement 10
           MOVE '000010' TO REC-ID
           MOVE 'FOURNIER' TO REC-NOM
           MOVE 'EMMA' TO REC-PRENOM
           MOVE 'DONNEES ENREGISTREMENT 10' TO REC-DATA
           PERFORM 2100-ECRIRE-UN-ENR

           DISPLAY ' '
           DISPLAY 'Total enregistrements ecrits : ' WS-CPT.

      *----------------------------------------------------------------
      * Ecriture d'un enregistrement
      *----------------------------------------------------------------
       2100-ECRIRE-UN-ENR.
           WRITE REC-RRDS
           END-WRITE

           IF WS-FS = '00'
               ADD 1 TO WS-CPT
               DISPLAY '  Ecrit #' WS-CPT ' : ' REC-ID ' '
                   REC-NOM ' ' REC-PRENOM
           ELSE
               DISPLAY '  Erreur ecriture : ' WS-FS
           END-IF.

      *----------------------------------------------------------------
      * Fermeture
      *----------------------------------------------------------------
       3000-FERMER-FICHIER.
           CLOSE FRRDS

           IF WS-FS = '00'
               DISPLAY ' '
               DISPLAY 'Fichier RRDS ferme avec succes.'
           ELSE
               DISPLAY 'Erreur fermeture : ' WS-FS
           END-IF.
