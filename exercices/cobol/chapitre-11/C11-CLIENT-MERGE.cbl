       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11CLIMRG.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-CLIENT-MERGE
      * OBJET     : Exercice 5 - Concatenation (MERGE) des 3 Data Sets
      *             - CLIENT.PS         (cle CLI-)
      *             - CLIENT-COURANT.PS (cle C-)
      *             - CLIENT-EPARGNE.PS (cle E-)
      *             Resultat : CLIENT-MERGE.PS (tous les clients)
      * NOTE      : Les 3 fichiers doivent etre PRE-TRIES sur leur cle
      * EXERCICE  : TP Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Fichier original (cle CLI-)
           SELECT F-CLIENT-ORIGINAL
               ASSIGN TO 'CLIENT-TRI-ASC.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-O.

      *    Fichier COURANT (cle C-)
           SELECT F-CLIENT-COURANT
               ASSIGN TO 'CLIENT-COURANT.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-C.

      *    Fichier EPARGNE (cle E-)
           SELECT F-CLIENT-EPARGNE
               ASSIGN TO 'CLIENT-EPARGNE.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

      *    Fichier de travail MERGE
           SELECT F-MERGE
               ASSIGN TO 'MERGE-CLIENT.TMP'.

      *    Fichier de sortie (concatenation)
           SELECT F-CLIENT-FUSION
               ASSIGN TO 'CLIENT-MERGE.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-F.

       DATA DIVISION.
       FILE SECTION.
       FD  F-CLIENT-ORIGINAL
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-ORIGINAL            PIC X(70).

       FD  F-CLIENT-COURANT
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-COURANT             PIC X(70).

       FD  F-CLIENT-EPARGNE
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-EPARGNE             PIC X(70).

       SD  F-MERGE.
       01  ENR-MERGE.
           05  MRG-ID              PIC X(8).
           05  MRG-NOM             PIC X(20).
           05  MRG-PRENOM          PIC X(15).
           05  MRG-SOLDE           PIC S9(10)V99.
           05  MRG-TYPE-COMPTE     PIC X(15).

       FD  F-CLIENT-FUSION
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-FUSION              PIC X(70).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-O             PIC XX.
       01  WS-STATUS-C             PIC XX.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-F             PIC XX.

       01  WS-FIN-MERGE            PIC X VALUE 'N'.
           88 FIN-MERGE            VALUE 'O'.

       01  WS-CPT                  PIC 999 VALUE 0.
       01  WS-ED-SOLDE             PIC -----.---.--9,99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 5 : MERGE (CONCATENATION) DES 3 DS     '
           DISPLAY '  - CLIENT-TRI-ASC.PS (cle CLI-)                  '
           DISPLAY '  - CLIENT-COURANT.PS (cle C-)                    '
           DISPLAY '  - CLIENT-EPARGNE.PS (cle E-)                    '
           DISPLAY '  Resultat : CLIENT-MERGE.PS                      '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Afficher les fichiers sources
           PERFORM 1000-AFFICHER-FICHIERS-SOURCES

      *    MERGE des 3 fichiers
           DISPLAY ' '
           DISPLAY '--- Execution du MERGE ---'
           MERGE F-MERGE
               ON ASCENDING KEY MRG-ID
               USING F-CLIENT-ORIGINAL
                     F-CLIENT-COURANT
                     F-CLIENT-EPARGNE
               GIVING F-CLIENT-FUSION

           DISPLAY 'MERGE termine avec succes'

      *    Afficher le resultat
           PERFORM 2000-AFFICHER-RESULTAT

           DISPLAY ' '
           DISPLAY 'Fichier fusionne : CLIENT-MERGE.PS'
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Afficher les fichiers sources
      *----------------------------------------------------------------*
       1000-AFFICHER-FICHIERS-SOURCES.
           DISPLAY '--- Fichier CLIENT-TRI-ASC.PS (original) ---'
           OPEN INPUT F-CLIENT-ORIGINAL
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-O = '10'
               READ F-CLIENT-ORIGINAL INTO ENR-MERGE
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       DISPLAY MRG-ID ' - ' MRG-NOM
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-ORIGINAL
           DISPLAY 'Total : ' WS-CPT ' enregistrements'

           DISPLAY ' '
           DISPLAY '--- Fichier CLIENT-COURANT.PS ---'
           OPEN INPUT F-CLIENT-COURANT
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-C = '10'
               READ F-CLIENT-COURANT INTO ENR-MERGE
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       DISPLAY MRG-ID ' - ' MRG-NOM
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-COURANT
           DISPLAY 'Total : ' WS-CPT ' enregistrements'

           DISPLAY ' '
           DISPLAY '--- Fichier CLIENT-EPARGNE.PS ---'
           OPEN INPUT F-CLIENT-EPARGNE
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-E = '10'
               READ F-CLIENT-EPARGNE INTO ENR-MERGE
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       DISPLAY MRG-ID ' - ' MRG-NOM
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-EPARGNE
           DISPLAY 'Total : ' WS-CPT ' enregistrements'.

      *----------------------------------------------------------------*
      * Afficher le fichier fusionne
      *----------------------------------------------------------------*
       2000-AFFICHER-RESULTAT.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  FICHIER FUSIONNE : CLIENT-MERGE.PS              '
           DISPLAY '=================================================='
           DISPLAY 'ID       NOM                  PRENOM          '
                   'SOLDE            TYPE'
           DISPLAY '-------- -------------------- --------------- '
                   '---------------- ---------------'

           OPEN INPUT F-CLIENT-FUSION
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-F = '10'
               READ F-CLIENT-FUSION INTO ENR-MERGE
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       MOVE MRG-SOLDE TO WS-ED-SOLDE
                       DISPLAY MRG-ID ' ' MRG-NOM ' ' MRG-PRENOM ' '
                               WS-ED-SOLDE ' ' MRG-TYPE-COMPTE
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-FUSION

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'TOTAL ENREGISTREMENTS FUSIONNES : ' WS-CPT
           DISPLAY '=================================================='.

