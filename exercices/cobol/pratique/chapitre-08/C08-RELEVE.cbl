       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08RELEVE.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Edition Releve Bancaire
      *
      * Exercice de synthese - Chapitre VIII
      * Lecture d'un fichier BUFFER contenant differents types
      * d'enregistrements et dispatch vers fichiers INDEXED/SEQUENTIAL
      *
      * Types d'enregistrements :
      *   A = Agence   -> FAGENCE (INDEXED)
      *   C = Client   -> FCLT (INDEXED)
      *   R = RIB      -> FRIB (INDEXED)
      *   M = Mouvement-> FMVTC (SEQUENTIAL)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *--- Fichier source (BUFFER) - Sequential
           SELECT FBUFFER ASSIGN TO 'BUFFER.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-BUFFER.

      *--- Fichier AGENCE - Indexed
           SELECT FAGENCE ASSIGN TO 'AGENCE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CODE-TAG
               FILE STATUS IS FS-AGENCE.

      *--- Fichier CLIENT - Indexed
           SELECT FCLT ASSIGN TO 'CLIENT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CODE-TCLT
               FILE STATUS IS FS-CLT.

      *--- Fichier RIB - Indexed
           SELECT FRIB ASSIGN TO 'RIB.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CLT-TRIB
               FILE STATUS IS FS-RIB.

      *--- Fichier MOUVEMENTS - Sequential
           SELECT FMVTC ASSIGN TO 'MVTC.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-MVTC.

       DATA DIVISION.
       FILE SECTION.

      *----------------------------------------------------------------
      * FD FAGENCE - Fichier Agences
      *----------------------------------------------------------------
       FD  FAGENCE.
       01  ENR-TAG.
           10  CODE-TAG           PIC 9(7).
           10  LIBELLE-TAG        PIC X(30).

      *----------------------------------------------------------------
      * FD FCLT - Fichier Clients
      *----------------------------------------------------------------
       FD  FCLT.
       01  ENR-TCLT.
           10  CODE-TCLT          PIC 9(5).
           10  NOM-TCLT           PIC X(20).
           10  PRENOM-TCLT        PIC X(20).
           10  CODEAG-TCLT        PIC 9(7).

      *----------------------------------------------------------------
      * FD FRIB - Fichier RIB
      *----------------------------------------------------------------
       FD  FRIB.
       01  ENR-TRIB.
           10  CLT-TRIB           PIC 9(5).
           10  CPTE-TRIB          PIC X(23).
           10  DATE-TRIB          PIC 9(8).
           10  SOLDE-TRIB         PIC 9(9)V9(2).
           10  SENS-TRIB          PIC X.

      *----------------------------------------------------------------
      * FD FMVTC - Fichier Mouvements
      *----------------------------------------------------------------
       FD  FMVTC.
       01  ENR-TMVTC.
           10  CLT-TMVTC          PIC 9(5).
           10  DATE-TMVTC         PIC 9(8).
           10  MONT-TMVTC         PIC 9(9)V9(2).
           10  SENS-TMVTC         PIC X.

      *----------------------------------------------------------------
      * FD FBUFFER - Fichier source avec REDEFINES
      *----------------------------------------------------------------
       FD  FBUFFER.
       01  ENR-BUFFER.
           10  ID-TAB             PIC X.
           10  LIGNE-AG.
               15  CODE-AG        PIC 9(7).
               15  CODEAG REDEFINES CODE-AG.
                   20  CODEAG-6   PIC 9(6).
                   20  CODEAG-CLE PIC 9.
               15  LIBELLE-AG     PIC X(30).
               15  FILLER         PIC X(42).
           10  LIGNE-CLT REDEFINES LIGNE-AG.
               15  CODE-CLT       PIC 9(5).
               15  NOM-CLT        PIC X(20).
               15  PRENOM-CLT     PIC X(20).
               15  CODEAG-CLT     PIC 9(7).
               15  FILLER         PIC X(27).
           10  LIGNE-RIB REDEFINES LIGNE-AG.
               15  CLT-RIB        PIC 9(5).
               15  CPTE-RIB       PIC X(23).
               15  DATE-RIB       PIC 9(8).
               15  SOLDE-RIB      PIC 9(9)V9(2).
               15  SENS-RIB       PIC X.
               15  FILLER         PIC X(31).
           10  LIGNE-MVTC REDEFINES LIGNE-AG.
               15  CLT-MVTC       PIC 9(5).
               15  DATE-MVTC      PIC 9(8).
               15  MONT-MVTC      PIC 9(9)V9(2).
               15  SENS-MVTC      PIC X.
               15  FILLER         PIC X(54).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * FILE STATUS
      *----------------------------------------------------------------
       01  FS-BUFFER              PIC XX VALUE ZEROS.
       01  FS-AGENCE              PIC XX VALUE ZEROS.
       01  FS-CLT                 PIC XX VALUE ZEROS.
       01  FS-RIB                 PIC XX VALUE ZEROS.
       01  FS-MVTC                PIC XX VALUE ZEROS.

      *----------------------------------------------------------------
      * Indicateur fin de fichier
      *----------------------------------------------------------------
       01  WS-BUFFER-EOF          PIC X VALUE 'N'.
           88  FIN-BUFFER         VALUE 'O'.

      *----------------------------------------------------------------
      * Variables pour calcul cle agence
      *----------------------------------------------------------------
       01  WQUOT                  PIC 9(6).
       01  WRESTE                 PIC 9.
       01  CLE-AG                 PIC 9.

      *----------------------------------------------------------------
      * Compteurs
      *----------------------------------------------------------------
       01  WS-CPT-AGENCE          PIC 9(5) VALUE 0.
       01  WS-CPT-CLIENT          PIC 9(5) VALUE 0.
       01  WS-CPT-RIB             PIC 9(5) VALUE 0.
       01  WS-CPT-MVTC            PIC 9(5) VALUE 0.
       01  WS-CPT-ERREUR          PIC 9(5) VALUE 0.
       01  WS-CPT-TOTAL           PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
      *----------------------------------------------------------------
      * Programme principal
      *----------------------------------------------------------------
       PROG.
           PERFORM DEBUT
           PERFORM TRAITEMENT UNTIL FIN-BUFFER
           PERFORM FIN
           STOP RUN.

      *----------------------------------------------------------------
      * DEBUT - Ouverture des fichiers
      *----------------------------------------------------------------
       DEBUT.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Edition Releve Bancaire - Chargement fichiers'
           DISPLAY '=================================================='
           DISPLAY ' '

           MOVE 'N' TO WS-BUFFER-EOF
           OPEN INPUT FBUFFER
           OPEN OUTPUT FCLT FAGENCE FRIB FMVTC

           IF FS-BUFFER = '00' AND FS-AGENCE = '00'
              AND FS-CLT = '00' AND FS-RIB = '00'
              AND FS-MVTC = '00'
               DISPLAY 'Tous les fichiers ouverts avec succes.'
               DISPLAY ' '
               PERFORM LECTURE
           ELSE
               DISPLAY 'ERREUR OUVERTURE FICHIERS :'
               DISPLAY '  BUFFER : ' FS-BUFFER
               DISPLAY '  AGENCE : ' FS-AGENCE
               DISPLAY '  CLIENT : ' FS-CLT
               DISPLAY '  RIB    : ' FS-RIB
               DISPLAY '  MVTC   : ' FS-MVTC
               STOP RUN
           END-IF.

      *----------------------------------------------------------------
      * TRAITEMENT - Dispatch selon type d'enregistrement
      *----------------------------------------------------------------
       TRAITEMENT.
           ADD 1 TO WS-CPT-TOTAL

           EVALUATE ID-TAB
               WHEN 'A'
                   PERFORM TRAITER-AGENCE
               WHEN 'C'
                   PERFORM TRAITER-CLIENT
               WHEN 'R'
                   PERFORM TRAITER-RIB
               WHEN 'M'
                   PERFORM TRAITER-MVTC
               WHEN OTHER
                   DISPLAY 'Type inconnu : ' ID-TAB
                   ADD 1 TO WS-CPT-ERREUR
           END-EVALUATE

           PERFORM LECTURE.

      *----------------------------------------------------------------
      * FIN - Fermeture et statistiques
      *----------------------------------------------------------------
       FIN.
           CLOSE FBUFFER FAGENCE FCLT FMVTC FRIB

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  STATISTIQUES'
           DISPLAY '=================================================='
           DISPLAY '  Enregistrements lus    : ' WS-CPT-TOTAL
           DISPLAY '  Agences ecrites        : ' WS-CPT-AGENCE
           DISPLAY '  Clients ecrits         : ' WS-CPT-CLIENT
           DISPLAY '  RIB ecrits             : ' WS-CPT-RIB
           DISPLAY '  Mouvements ecrits      : ' WS-CPT-MVTC
           DISPLAY '  Erreurs                : ' WS-CPT-ERREUR
           DISPLAY '=================================================='
           DISPLAY ' '.

      *----------------------------------------------------------------
      * LECTURE - Lecture du fichier BUFFER
      *----------------------------------------------------------------
       LECTURE.
           READ FBUFFER
               AT END MOVE 'O' TO WS-BUFFER-EOF
           END-READ.

      *----------------------------------------------------------------
      * TRAITER-AGENCE - Validation cle et ecriture
      *----------------------------------------------------------------
       TRAITER-AGENCE.
      * Calcul de la cle de controle (modulo 7)
           DIVIDE CODEAG-6 BY 7 GIVING WQUOT REMAINDER WRESTE
           COMPUTE CLE-AG = 7 - WRESTE

           DISPLAY 'AGENCE: ' CODE-AG ' CLE CALC=' CLE-AG
               ' CLE BUFFER=' CODEAG-CLE

           IF CLE-AG NOT = CODEAG-CLE
               DISPLAY '  -> CODE AGENCE ERRONE: ' CODE-AG
               ADD 1 TO WS-CPT-ERREUR
           ELSE
               MOVE CODE-AG TO CODE-TAG
               MOVE LIBELLE-AG TO LIBELLE-TAG
               WRITE ENR-TAG
                   INVALID KEY
                       DISPLAY '  -> ERR. ECR. AGENCE: ' FS-AGENCE
                       ADD 1 TO WS-CPT-ERREUR
                   NOT INVALID KEY
                       DISPLAY '  -> ENR AGENCE OK: ' CODE-TAG
                           ' ' LIBELLE-TAG
                       ADD 1 TO WS-CPT-AGENCE
               END-WRITE
           END-IF.

      *----------------------------------------------------------------
      * TRAITER-CLIENT - Ecriture fichier client
      *----------------------------------------------------------------
       TRAITER-CLIENT.
           MOVE CODE-CLT TO CODE-TCLT
           MOVE NOM-CLT TO NOM-TCLT
           MOVE PRENOM-CLT TO PRENOM-TCLT
           MOVE CODEAG-CLT TO CODEAG-TCLT

           WRITE ENR-TCLT
               INVALID KEY
                   DISPLAY 'ERR. ECR. CLIENT: ' FS-CLT
                   ADD 1 TO WS-CPT-ERREUR
               NOT INVALID KEY
                   DISPLAY 'CLIENT OK: ' CODE-TCLT ' '
                       NOM-TCLT ' ' PRENOM-TCLT
                   ADD 1 TO WS-CPT-CLIENT
           END-WRITE.

      *----------------------------------------------------------------
      * TRAITER-RIB - Ecriture fichier RIB
      *----------------------------------------------------------------
       TRAITER-RIB.
           MOVE CLT-RIB TO CLT-TRIB
           MOVE CPTE-RIB TO CPTE-TRIB
           MOVE DATE-RIB TO DATE-TRIB
           MOVE SOLDE-RIB TO SOLDE-TRIB
           MOVE SENS-RIB TO SENS-TRIB

           WRITE ENR-TRIB
               INVALID KEY
                   DISPLAY 'ERR. ECR. RIB: ' FS-RIB
                   ADD 1 TO WS-CPT-ERREUR
               NOT INVALID KEY
                   DISPLAY 'RIB OK: ' CLT-TRIB ' ' CPTE-TRIB
                   ADD 1 TO WS-CPT-RIB
           END-WRITE.

      *----------------------------------------------------------------
      * TRAITER-MVTC - Ecriture fichier mouvements
      *----------------------------------------------------------------
       TRAITER-MVTC.
           MOVE CLT-MVTC TO CLT-TMVTC
           MOVE DATE-MVTC TO DATE-TMVTC
           MOVE MONT-MVTC TO MONT-TMVTC
           MOVE SENS-MVTC TO SENS-TMVTC

           WRITE ENR-TMVTC
           END-WRITE

           IF FS-MVTC = '00'
               DISPLAY 'MVTC OK: ' CLT-TMVTC ' ' DATE-TMVTC
                   ' ' MONT-TMVTC ' ' SENS-TMVTC
               ADD 1 TO WS-CPT-MVTC
           ELSE
               DISPLAY 'ERR. ECR. MVTC: ' FS-MVTC
               ADD 1 TO WS-CPT-ERREUR
           END-IF.
