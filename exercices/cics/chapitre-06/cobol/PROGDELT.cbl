       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGDELT.
      *
      * Programme : PROGDELT - Suppression simple VSAM avec CICS
      * Transaction : DELT
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)
      *

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.

       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).

       01  WS-REC-KEY              PIC X(6).
       01  WS-MSG                  PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
      *    Cle a supprimer (EMP099 cree par PROGWRIT)
           MOVE 'EMP099' TO WS-REC-KEY.

      *    Lecture avec verrouillage (UPDATE)
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               UPDATE
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR: EMP099 NON TROUVE' TO WS-MSG
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(60)
                   ERASE
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.

      *    Suppression
           EXEC CICS DELETE
               FILE('EMPLOYE')
               RESP(WS-RESP)
           END-EXEC.

      *    Afficher resultat
           INITIALIZE WS-MSG.
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'EMP099 SUPPRIME AVEC SUCCES' TO WS-MSG
           ELSE
               MOVE 'ERREUR DELETE' TO WS-MSG
           END-IF.

           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               LENGTH(60)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
