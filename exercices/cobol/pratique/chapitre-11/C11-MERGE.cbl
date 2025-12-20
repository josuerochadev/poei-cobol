       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11MERGE.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-MERGE
      * OBJET     : Demonstration MERGE (fusion de fichiers pre-tries)
      *             Fusion de 3 fichiers regionaux en un fichier unique
      * EXERCICE  : Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Fichiers regionaux (pre-tries par code client)
           SELECT F-PARIS
               ASSIGN TO 'CLIENTS-PARIS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-P.

           SELECT F-LYON
               ASSIGN TO 'CLIENTS-LYON.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-L.

           SELECT F-MARSEILLE
               ASSIGN TO 'CLIENTS-MARSEILLE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-M.

      *    Fichier de travail pour le MERGE
           SELECT F-MERGE
               ASSIGN TO 'MERGE-WORK.TMP'.

      *    Fichier de sortie (fusionne)
           SELECT F-NATIONAL
               ASSIGN TO 'CLIENTS-NATIONAL.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-N.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Fichiers regionaux (structure identique)
      *----------------------------------------------------------------*
       FD  F-PARIS
           RECORDING MODE IS F
           RECORD CONTAINS 60 CHARACTERS.
       01  ENR-PARIS               PIC X(60).

       FD  F-LYON
           RECORDING MODE IS F
           RECORD CONTAINS 60 CHARACTERS.
       01  ENR-LYON                PIC X(60).

       FD  F-MARSEILLE
           RECORDING MODE IS F
           RECORD CONTAINS 60 CHARACTERS.
       01  ENR-MARSEILLE           PIC X(60).

      *----------------------------------------------------------------*
      * Fichier de travail MERGE (SD)
      *----------------------------------------------------------------*
       SD  F-MERGE.
       01  ENR-MERGE.
           05  MRG-CODE            PIC X(8).
           05  MRG-NOM             PIC X(25).
           05  MRG-VILLE           PIC X(15).
           05  MRG-CA              PIC 9(10)V99.

      *----------------------------------------------------------------*
      * Fichier de sortie
      *----------------------------------------------------------------*
       FD  F-NATIONAL
           RECORDING MODE IS F
           RECORD CONTAINS 60 CHARACTERS.
       01  ENR-NATIONAL            PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-P             PIC XX.
       01  WS-STATUS-L             PIC XX.
       01  WS-STATUS-M             PIC XX.
       01  WS-STATUS-N             PIC XX.

       01  WS-FIN-MERGE            PIC X VALUE 'N'.
           88 FIN-MERGE            VALUE 'O'.

       01  WS-CPT                  PIC 9(3) VALUE 0.
       01  WS-ED-CA                PIC ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  DEMONSTRATION MERGE (FUSION DE FICHIERS)        '
           DISPLAY '  Fusion de 3 fichiers regionaux pre-tries        '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Creer les 3 fichiers regionaux (pre-tries)
           PERFORM 1000-CREER-FICHIERS-REGIONAUX

      *    Fusion avec MERGE
           DISPLAY ' '
           DISPLAY '--- Fusion des 3 fichiers par MERGE ---'
           MERGE F-MERGE
               ON ASCENDING KEY MRG-CODE
               USING F-PARIS F-LYON F-MARSEILLE
               GIVING F-NATIONAL

           DISPLAY 'Fusion terminee'

      *    Afficher le resultat
           PERFORM 2000-AFFICHER-RESULTAT

           DISPLAY ' '
           DISPLAY 'Fin du programme C11-MERGE'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation des fichiers regionaux PRE-TRIES par code client
      *----------------------------------------------------------------*
       1000-CREER-FICHIERS-REGIONAUX.
      *    PARIS (codes commencant par P)
           OPEN OUTPUT F-PARIS
           MOVE 'P0001   DUPONT                   PARIS          0000150000'
               TO ENR-PARIS
           WRITE ENR-PARIS
           MOVE 'P0003   MARTIN                   PARIS          0000250000'
               TO ENR-PARIS
           WRITE ENR-PARIS
           MOVE 'P0005   BERNARD                  PARIS          0000180000'
               TO ENR-PARIS
           WRITE ENR-PARIS
           CLOSE F-PARIS
           DISPLAY 'PARIS    : 3 clients (P0001, P0003, P0005)'

      *    LYON (codes commencant par L)
           OPEN OUTPUT F-LYON
           MOVE 'L0002   PETIT                    LYON           0000120000'
               TO ENR-LYON
           WRITE ENR-LYON
           MOVE 'L0004   DURAND                   LYON           0000200000'
               TO ENR-LYON
           WRITE ENR-LYON
           CLOSE F-LYON
           DISPLAY 'LYON     : 2 clients (L0002, L0004)'

      *    MARSEILLE (codes commencant par M)
           OPEN OUTPUT F-MARSEILLE
           MOVE 'M0001   LEROY                    MARSEILLE      0000300000'
               TO ENR-MARSEILLE
           WRITE ENR-MARSEILLE
           MOVE 'M0006   MOREAU                   MARSEILLE      0000175000'
               TO ENR-MARSEILLE
           WRITE ENR-MARSEILLE
           CLOSE F-MARSEILLE
           DISPLAY 'MARSEILLE: 2 clients (M0001, M0006)'.

      *----------------------------------------------------------------*
      * Afficher le fichier national fusionne
      *----------------------------------------------------------------*
       2000-AFFICHER-RESULTAT.
           DISPLAY ' '
           DISPLAY '--- Fichier NATIONAL fusionne (tri par code) ---'
           DISPLAY 'CODE     NOM                       VILLE           CA'
           DISPLAY '-------- ------------------------- --------------- --------'

           MOVE 0 TO WS-CPT
           OPEN INPUT F-NATIONAL
           PERFORM UNTIL WS-STATUS-N = '10'
               READ F-NATIONAL INTO ENR-MERGE
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       MOVE MRG-CA TO WS-ED-CA
                       DISPLAY MRG-CODE ' ' MRG-NOM ' ' MRG-VILLE ' '
                               WS-ED-CA
               END-READ
           END-PERFORM
           CLOSE F-NATIONAL

           DISPLAY ' '
           DISPLAY 'Total clients fusionnes : ' WS-CPT.

