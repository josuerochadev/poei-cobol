       IDENTIFICATION DIVISION.
       PROGRAM-ID. C06-BANQUE03.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * TD BANQUE VIRTUELLE - Exercice 4
      *
      * Lecture d'un fichier BUFFER (SEQUENTIAL/ESDS) et dispatch
      * vers 4 fichiers INDEXED selon le type d'enregistrement :
      *   'A' -> AGENCES.DAT
      *   'C' -> CLIENTS.DAT
      *   'R' -> RIB.DAT
      *   'M' -> MOUVEMENTS.DAT
      *
      * Validation code agence : cle = 7 - (code6 MOD 7)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *----------------------------------------------------------------
      * Fichier d'entree : BUFFER (SEQUENTIAL)
      *----------------------------------------------------------------
           SELECT F-BUFFER
               ASSIGN TO 'BUFFER.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-BUF.

      *----------------------------------------------------------------
      * Fichier de sortie : AGENCES (INDEXED)
      *----------------------------------------------------------------
           SELECT F-AGENCES
               ASSIGN TO 'AGENCES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FA-CODE
               FILE STATUS IS WS-STATUS-AG.

      *----------------------------------------------------------------
      * Fichier de sortie : CLIENTS (INDEXED)
      *----------------------------------------------------------------
           SELECT F-CLIENTS
               ASSIGN TO 'CLIENTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FC-CODE
               FILE STATUS IS WS-STATUS-CLT.

      *----------------------------------------------------------------
      * Fichier de sortie : RIB (INDEXED)
      *----------------------------------------------------------------
           SELECT F-RIB
               ASSIGN TO 'RIB.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FR-CLT
               ALTERNATE RECORD KEY IS FR-CPTE
               FILE STATUS IS WS-STATUS-RIB.

      *----------------------------------------------------------------
      * Fichier de sortie : MOUVEMENTS (INDEXED)
      *----------------------------------------------------------------
           SELECT F-MOUVEMENTS
               ASSIGN TO 'MOUVEMENTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FM-CLE
               FILE STATUS IS WS-STATUS-MVT.

       DATA DIVISION.
       FILE SECTION.

      *----------------------------------------------------------------
      * FD BUFFER - 55 octets (ID + donnees)
      *----------------------------------------------------------------
       FD  F-BUFFER
           RECORDING MODE IS F
           RECORD CONTAINS 55 CHARACTERS.
       01  ENR-BUFFER.
           05  BUF-ID              PIC X.
           05  BUF-DATA            PIC X(54).

      *----------------------------------------------------------------
      * FD AGENCES - 37 octets
      *----------------------------------------------------------------
       FD  F-AGENCES
           RECORDING MODE IS F
           RECORD CONTAINS 37 CHARACTERS.
       01  ENR-AGENCE.
           05  FA-CODE             PIC 9(7).
           05  FA-LIBELLE          PIC X(30).

      *----------------------------------------------------------------
      * FD CLIENTS - 54 octets
      *----------------------------------------------------------------
       FD  F-CLIENTS
           RECORDING MODE IS F
           RECORD CONTAINS 54 CHARACTERS.
       01  ENR-CLIENT.
           05  FC-CODE             PIC 9(7).
           05  FC-NOM              PIC X(20).
           05  FC-PRENOM           PIC X(20).
           05  FC-CODEAG           PIC 9(7).

      *----------------------------------------------------------------
      * FD RIB - 50 octets
      *----------------------------------------------------------------
       FD  F-RIB
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-RIB.
           05  FR-CLT              PIC 9(7).
           05  FR-CPTE             PIC X(23).
           05  FR-DATE             PIC 9(8).
           05  FR-SOLDE            PIC 9(9)V99.
           05  FR-SENS             PIC X.

      *----------------------------------------------------------------
      * FD MOUVEMENTS - 30 octets (cle composite)
      *----------------------------------------------------------------
       FD  F-MOUVEMENTS
           RECORDING MODE IS F
           RECORD CONTAINS 30 CHARACTERS.
       01  ENR-MOUVEMENT.
           05  FM-CLE.
               10  FM-CLT          PIC 9(7).
               10  FM-DATE         PIC 9(8).
               10  FM-SEQ          PIC 9(3).
           05  FM-MONT             PIC 9(9)V99.
           05  FM-SENS             PIC X.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * FILE STATUS
      *----------------------------------------------------------------
       01  WS-STATUS-BUF           PIC XX.
       01  WS-STATUS-AG            PIC XX.
       01  WS-STATUS-CLT           PIC XX.
       01  WS-STATUS-RIB           PIC XX.
       01  WS-STATUS-MVT           PIC XX.

      *----------------------------------------------------------------
      * Indicateurs
      *----------------------------------------------------------------
       01  WS-FIN-FICHIER          PIC 9 VALUE 0.
           88  FIN-FICHIER         VALUE 1.

      *----------------------------------------------------------------
      * Compteurs
      *----------------------------------------------------------------
       01  WS-CPT-LUS              PIC 9(4) VALUE 0.
       01  WS-CPT-AGENCES          PIC 9(4) VALUE 0.
       01  WS-CPT-AG-REJET         PIC 9(4) VALUE 0.
       01  WS-CPT-CLIENTS          PIC 9(4) VALUE 0.
       01  WS-CPT-RIB              PIC 9(4) VALUE 0.
       01  WS-CPT-MVT              PIC 9(4) VALUE 0.
       01  WS-CPT-IGNORES          PIC 9(4) VALUE 0.

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  WS-CODE6                PIC 9(6) VALUE 0.
       01  WS-CODE1                PIC 9 VALUE 0.
       01  WS-RESTE                PIC 9 VALUE 0.
       01  WS-CLE-CALC             PIC 9 VALUE 0.
       01  WS-CODE-VALIDE          PIC 9 VALUE 0.
           88  CODE-OK             VALUE 1.
           88  CODE-KO             VALUE 0.

      *----------------------------------------------------------------
      * Sequence pour cle mouvement
      *----------------------------------------------------------------
       01  WS-SEQ-MVT              PIC 9(3) VALUE 0.
       01  WS-LAST-CLT-MVT         PIC 9(7) VALUE 0.
       01  WS-LAST-DATE-MVT        PIC 9(8) VALUE 0.

      *----------------------------------------------------------------
      * Zones de travail pour extraction buffer
      *----------------------------------------------------------------
       01  WS-AGENCE.
           05  WS-AG-CODE          PIC 9(7).
           05  WS-AG-LIBELLE       PIC X(30).

       01  WS-CLIENT.
           05  WS-CLT-CODE         PIC 9(7).
           05  WS-CLT-NOM          PIC X(20).
           05  WS-CLT-PRENOM       PIC X(20).
           05  WS-CLT-CODEAG       PIC 9(7).

       01  WS-RIB.
           05  WS-RIB-CLT          PIC 9(7).
           05  WS-RIB-CPTE         PIC X(23).
           05  WS-RIB-DATE         PIC 9(8).
           05  WS-RIB-SOLDE        PIC 9(9)V99.
           05  WS-RIB-SENS         PIC X.

       01  WS-MVT.
           05  WS-MVT-CLT          PIC 9(7).
           05  WS-MVT-DATE         PIC 9(8).
           05  WS-MVT-MONT         PIC 9(9)V99.
           05  WS-MVT-SENS         PIC X.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  TD BANQUE VIRTUELLE - Fichiers E/S'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INITIALISATION
           PERFORM 2000-TRAITEMENT
           PERFORM 3000-FINALISATION

           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture des fichiers
      *----------------------------------------------------------------
       1000-INITIALISATION.
           DISPLAY 'Ouverture des fichiers...'

      * Fichier BUFFER en lecture
           OPEN INPUT F-BUFFER
           IF WS-STATUS-BUF NOT = '00'
               DISPLAY 'Erreur ouverture BUFFER : ' WS-STATUS-BUF
               STOP RUN
           END-IF

      * Fichiers de sortie en ecriture
           OPEN OUTPUT F-AGENCES
           IF WS-STATUS-AG NOT = '00'
               DISPLAY 'Erreur ouverture AGENCES : ' WS-STATUS-AG
               CLOSE F-BUFFER
               STOP RUN
           END-IF

           OPEN OUTPUT F-CLIENTS
           IF WS-STATUS-CLT NOT = '00'
               DISPLAY 'Erreur ouverture CLIENTS : ' WS-STATUS-CLT
               CLOSE F-BUFFER F-AGENCES
               STOP RUN
           END-IF

           OPEN OUTPUT F-RIB
           IF WS-STATUS-RIB NOT = '00'
               DISPLAY 'Erreur ouverture RIB : ' WS-STATUS-RIB
               CLOSE F-BUFFER F-AGENCES F-CLIENTS
               STOP RUN
           END-IF

           OPEN OUTPUT F-MOUVEMENTS
           IF WS-STATUS-MVT NOT = '00'
               DISPLAY 'Erreur ouverture MOUVEMENTS : ' WS-STATUS-MVT
               CLOSE F-BUFFER F-AGENCES F-CLIENTS F-RIB
               STOP RUN
           END-IF

           DISPLAY 'Fichiers ouverts avec succes.'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Lecture et dispatch
      *----------------------------------------------------------------
       2000-TRAITEMENT.
           DISPLAY 'Traitement du fichier BUFFER...'
           DISPLAY ' '

           READ F-BUFFER
               AT END SET FIN-FICHIER TO TRUE
           END-READ

           PERFORM UNTIL FIN-FICHIER
               ADD 1 TO WS-CPT-LUS
               PERFORM 2100-DISPATCHER-ENR

               READ F-BUFFER
                   AT END SET FIN-FICHIER TO TRUE
               END-READ
           END-PERFORM

           DISPLAY ' '
           DISPLAY 'Traitement termine.'.

      *----------------------------------------------------------------
      * Dispatch selon type d'enregistrement
      *----------------------------------------------------------------
       2100-DISPATCHER-ENR.
           EVALUATE BUF-ID
               WHEN 'A'
                   PERFORM 2200-TRAITER-AGENCE
               WHEN 'C'
                   PERFORM 2300-TRAITER-CLIENT
               WHEN 'R'
                   PERFORM 2400-TRAITER-RIB
               WHEN 'M'
                   PERFORM 2500-TRAITER-MVT
               WHEN OTHER
                   ADD 1 TO WS-CPT-IGNORES
                   DISPLAY '  [?] Type inconnu : ' BUF-ID
           END-EVALUATE.

      *----------------------------------------------------------------
      * Traitement AGENCE
      *----------------------------------------------------------------
       2200-TRAITER-AGENCE.
           MOVE BUF-DATA(1:37) TO WS-AGENCE

      * Validation code agence
           DIVIDE WS-AG-CODE BY 10 GIVING WS-CODE6
               REMAINDER WS-CODE1
           COMPUTE WS-RESTE = FUNCTION MOD(WS-CODE6, 7)
           COMPUTE WS-CLE-CALC = 7 - WS-RESTE

           IF WS-CLE-CALC = WS-CODE1
               SET CODE-OK TO TRUE
           ELSE
               SET CODE-KO TO TRUE
           END-IF

           IF CODE-OK
               MOVE WS-AG-CODE TO FA-CODE
               MOVE WS-AG-LIBELLE TO FA-LIBELLE
               WRITE ENR-AGENCE
                   INVALID KEY
                       DISPLAY '  [A] Doublon agence : ' FA-CODE
                   NOT INVALID KEY
                       ADD 1 TO WS-CPT-AGENCES
                       DISPLAY '  [A] Agence ecrite : ' FA-CODE
                           ' - ' FA-LIBELLE
               END-WRITE
           ELSE
               ADD 1 TO WS-CPT-AG-REJET
               DISPLAY '  [A] Agence REJETEE : ' WS-AG-CODE
                   ' (code invalide, cle attendue=' WS-CLE-CALC ')'
           END-IF.

      *----------------------------------------------------------------
      * Traitement CLIENT
      *----------------------------------------------------------------
       2300-TRAITER-CLIENT.
           MOVE BUF-DATA(1:54) TO WS-CLIENT

           MOVE WS-CLT-CODE TO FC-CODE
           MOVE WS-CLT-NOM TO FC-NOM
           MOVE WS-CLT-PRENOM TO FC-PRENOM
           MOVE WS-CLT-CODEAG TO FC-CODEAG

           WRITE ENR-CLIENT
               INVALID KEY
                   DISPLAY '  [C] Doublon client : ' FC-CODE
               NOT INVALID KEY
                   ADD 1 TO WS-CPT-CLIENTS
                   DISPLAY '  [C] Client ecrit : ' FC-CODE
                       ' - ' FC-NOM ' ' FC-PRENOM
           END-WRITE.

      *----------------------------------------------------------------
      * Traitement RIB
      *----------------------------------------------------------------
       2400-TRAITER-RIB.
           MOVE BUF-DATA(1:50) TO WS-RIB

           MOVE WS-RIB-CLT TO FR-CLT
           MOVE WS-RIB-CPTE TO FR-CPTE
           MOVE WS-RIB-DATE TO FR-DATE
           MOVE WS-RIB-SOLDE TO FR-SOLDE
           MOVE WS-RIB-SENS TO FR-SENS

           WRITE ENR-RIB
               INVALID KEY
                   DISPLAY '  [R] Doublon RIB client : ' FR-CLT
               NOT INVALID KEY
                   ADD 1 TO WS-CPT-RIB
                   DISPLAY '  [R] RIB ecrit : Client ' FR-CLT
                       ' - ' FR-CPTE
           END-WRITE.

      *----------------------------------------------------------------
      * Traitement MOUVEMENT
      *----------------------------------------------------------------
       2500-TRAITER-MVT.
           MOVE BUF-DATA(1:27) TO WS-MVT

      * Gestion sequence pour cle unique
           IF WS-MVT-CLT = WS-LAST-CLT-MVT
               AND WS-MVT-DATE = WS-LAST-DATE-MVT
               ADD 1 TO WS-SEQ-MVT
           ELSE
               MOVE 1 TO WS-SEQ-MVT
               MOVE WS-MVT-CLT TO WS-LAST-CLT-MVT
               MOVE WS-MVT-DATE TO WS-LAST-DATE-MVT
           END-IF

           MOVE WS-MVT-CLT TO FM-CLT
           MOVE WS-MVT-DATE TO FM-DATE
           MOVE WS-SEQ-MVT TO FM-SEQ
           MOVE WS-MVT-MONT TO FM-MONT
           MOVE WS-MVT-SENS TO FM-SENS

           WRITE ENR-MOUVEMENT
               INVALID KEY
                   DISPLAY '  [M] Doublon mouvement : '
                       FM-CLT '-' FM-DATE '-' FM-SEQ
               NOT INVALID KEY
                   ADD 1 TO WS-CPT-MVT
                   DISPLAY '  [M] Mouvement ecrit : Client ' FM-CLT
                       ' - Date ' FM-DATE ' - ' FM-SENS
           END-WRITE.

      *----------------------------------------------------------------
      * Fermeture et statistiques
      *----------------------------------------------------------------
       3000-FINALISATION.
           DISPLAY ' '
           DISPLAY '--------------------------------------------------'
           DISPLAY 'STATISTIQUES DU TRAITEMENT :'
           DISPLAY '--------------------------------------------------'
           DISPLAY 'Enregistrements lus       : ' WS-CPT-LUS
           DISPLAY ' '
           DISPLAY 'Agences ecrites           : ' WS-CPT-AGENCES
           DISPLAY 'Agences rejetees          : ' WS-CPT-AG-REJET
           DISPLAY 'Clients ecrits            : ' WS-CPT-CLIENTS
           DISPLAY 'RIB ecrits                : ' WS-CPT-RIB
           DISPLAY 'Mouvements ecrits         : ' WS-CPT-MVT
           DISPLAY 'Types inconnus ignores    : ' WS-CPT-IGNORES
           DISPLAY ' '

           CLOSE F-BUFFER
           CLOSE F-AGENCES
           CLOSE F-CLIENTS
           CLOSE F-RIB
           CLOSE F-MOUVEMENTS

           DISPLAY 'Fichiers fermes.'
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Fin du programme BANQUE03'
           DISPLAY '=================================================='.
