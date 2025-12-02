       IDENTIFICATION DIVISION.
       PROGRAM-ID. PJ10ACT.
      *---------------------------------------------------------
      * EDITION CLIENT VIA AIX ACTIVITE PROFESSIONNELLE
      * LECTURE SEQUENTIELLE PAR ACTIVITE - SAUT 2 LIGNES
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENT ASSIGN TO FCLIENT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CLI-NUM-COMPTE
               ALTERNATE RECORD KEY IS CLI-ACTIVITE
                   WITH DUPLICATES
               FILE STATUS IS WS-FS-CLI.

           SELECT F-EDITION ASSIGN TO FEDITION
               FILE STATUS IS WS-FS-EDI.

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
           05 CLI-SOLDE            PIC 9(10).
           05 CLI-POSITION         PIC X(02).
           05 FILLER               PIC X(29).

       FD F-EDITION.
       01 ENR-EDITION              PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FS-CLI                PIC X(02).
       01 WS-FS-EDI                PIC X(02).

       01 WS-EOF                   PIC 9(01) VALUE 0.

      * ZONE DE RUPTURE
       01 WS-ACTIVITE-PREC         PIC 9(02) VALUE 99.
       01 WS-PREM                  PIC 9(01) VALUE 1.
       01 WS-AVANCE                PIC 9(01) VALUE 1.

      * LIGNE D'EDITION
       01 WS-LIGNE-DETAIL.
           05 FILLER               PIC X(03) VALUE SPACES.
           05 WS-DET-NUM           PIC 9(03).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 WS-DET-REGION        PIC 9(02).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 WS-DET-NOM           PIC X(10).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 WS-DET-PRENOM        PIC X(10).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 WS-DET-ACTIVITE      PIC 9(02).
           05 FILLER               PIC X(02) VALUE SPACES.
           05 WS-DET-SOLDE         PIC Z(09)9.
           05 FILLER               PIC X(01) VALUE SPACES.
           05 WS-DET-POSITION      PIC X(02).
           05 FILLER               PIC X(27) VALUE SPACES.

       PROCEDURE DIVISION.

       PRINCIPAL.
           OPEN INPUT F-CLIENT
           OPEN OUTPUT F-EDITION
           MOVE 0 TO WS-EOF

      * POSITIONNER SUR L'AIX ACTIVITE
           START F-CLIENT KEY IS >= CLI-ACTIVITE

           PERFORM UNTIL WS-EOF = 1
               READ F-CLIENT NEXT
                   AT END MOVE 1 TO WS-EOF
                   NOT AT END
                       PERFORM GERER-RUPTURE
                       PERFORM ECRIRE-DETAIL
               END-READ
           END-PERFORM

           CLOSE F-CLIENT
           CLOSE F-EDITION
           STOP RUN.

       GERER-RUPTURE.
           MOVE 1 TO WS-AVANCE
      * RUPTURE ACTIVITE
           IF CLI-ACTIVITE NOT = WS-ACTIVITE-PREC
               IF WS-PREM = 0
                   MOVE 3 TO WS-AVANCE
               END-IF
               MOVE CLI-ACTIVITE TO WS-ACTIVITE-PREC
           END-IF
           MOVE 0 TO WS-PREM.

       ECRIRE-DETAIL.
           MOVE CLI-NUM-COMPTE  TO WS-DET-NUM
           MOVE CLI-CODE-REGION TO WS-DET-REGION
           MOVE CLI-NOM         TO WS-DET-NOM
           MOVE CLI-PRENOM      TO WS-DET-PRENOM
           MOVE CLI-ACTIVITE    TO WS-DET-ACTIVITE
           MOVE CLI-SOLDE       TO WS-DET-SOLDE
           MOVE CLI-POSITION    TO WS-DET-POSITION
           WRITE ENR-EDITION FROM WS-LIGNE-DETAIL
               AFTER ADVANCING WS-AVANCE LINES.
