       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-READ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENT ASSIGN TO "data/CLIENT.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-CLI.

       DATA DIVISION.
       FILE SECTION.

       FD F-CLIENT.
       01 ENR-CLIENT.
           05 CLI-NUM-COMPTE       PIC 9(03).
           05 CLI-CODE-REGION      PIC 9(02).
           05 CLI-NATURE-COMPTE    PIC 9(02).
           05 CLI-NOM              PIC X(10).
           05 CLI-PRENOM           PIC X(10).
           05 CLI-DATE-NAIS        PIC 9(08).
           05 CLI-SEXE             PIC X(01).
           05 CLI-ACTIVITE         PIC 9(02).
           05 CLI-SITUATION        PIC X(01).
           05 CLI-ADRESSE          PIC X(10).
           05 CLI-SOLDE            PIC 9(10).
           05 CLI-POSITION         PIC X(02).

       WORKING-STORAGE SECTION.
       01 WS-FS-CLI                PIC X(02).
       01 WS-EOF                   PIC 9(01) VALUE 0.
       01 WS-CPT                   PIC 9(03) VALUE 0.
       01 WS-TOT-CR                PIC 9(12) VALUE 0.
       01 WS-TOT-DB                PIC 9(12) VALUE 0.
       01 WS-LIGNE-SEP             PIC X(60) VALUE ALL "=".

       PROCEDURE DIVISION.

       PRINCIPAL.
           DISPLAY WS-LIGNE-SEP
           DISPLAY "  TEST LECTURE FICHIER CLIENT"
           DISPLAY WS-LIGNE-SEP
           DISPLAY " "

           OPEN INPUT F-CLIENT
           IF WS-FS-CLI NOT = "00"
               DISPLAY "ERREUR OUVERTURE: " WS-FS-CLI
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = 1
               READ F-CLIENT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END PERFORM TRAITER-CLIENT
               END-READ
           END-PERFORM

           CLOSE F-CLIENT

           DISPLAY " "
           DISPLAY WS-LIGNE-SEP
           DISPLAY "  RESUME:"
           DISPLAY "  - Nombre clients: " WS-CPT
           DISPLAY "  - Total CR:       " WS-TOT-CR
           DISPLAY "  - Total DB:       " WS-TOT-DB
           DISPLAY WS-LIGNE-SEP

           STOP RUN.

       TRAITER-CLIENT.
           ADD 1 TO WS-CPT
           DISPLAY WS-CPT ". " CLI-NOM " " CLI-PRENOM
                   " | " CLI-ADRESSE
                   " | " CLI-SOLDE " " CLI-POSITION

           IF CLI-POSITION = "CR"
               ADD CLI-SOLDE TO WS-TOT-CR
           ELSE
               ADD CLI-SOLDE TO WS-TOT-DB
           END-IF.
