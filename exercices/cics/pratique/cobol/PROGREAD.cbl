       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGREAD.
      *
      * Programme : PROGREAD - Lecture simple VSAM avec CICS
      * Transaction : READ
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)
      *

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-REC-KEY              PIC X(6).

       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).

       01  WS-MSG                  PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
           INITIALIZE WS-MSG.
      *    Cle a rechercher
           MOVE 'EMP001' TO WS-REC-KEY.

      *    Lecture VSAM
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               RESP(WS-RESP)
           END-EXEC.

      *    Afficher resultat
           IF WS-RESP = DFHRESP(NORMAL)
               STRING 'TROUVE: ' DELIMITED SIZE
                      EMP-NAME DELIMITED SIZE
                      INTO WS-MSG
               END-STRING
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(60)
                   ERASE
               END-EXEC
           ELSE
               MOVE 'EMPLOYE NON TROUVE OU ERREUR' TO WS-MSG
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(60)
                   ERASE
               END-EXEC
           END-IF.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
