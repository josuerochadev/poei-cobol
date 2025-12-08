       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGWRIT.
      ******************************************************************
      * Programme : PROGWRIT - Ecriture simple VSAM avec CICS
      * Transaction : WRIT
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.

       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  FILLER              PIC X(24).

       01  WS-MSG                  PIC X(50).

       PROCEDURE DIVISION.

       0000-MAIN.
      *--- Preparer enregistrement a ecrire
           INITIALIZE WS-EMPLOYE.
           MOVE 'EMP099' TO EMP-ID.
           MOVE 'TEST EMPLOYE CICS'  TO EMP-NAME.
           MOVE 'TEST'     TO EMP-DEPT.
           MOVE 012345678  TO EMP-SALAIRE.
           MOVE 'N'        TO EMP-ETAT-CRED.

      *--- Ecriture VSAM
           EXEC CICS WRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RIDFLD(EMP-ID)
               RESP(WS-RESP)
           END-EXEC.

      *--- Afficher resultat
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT CREE: EMP099' TO WS-MSG
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(50)
                   ERASE
               END-EXEC
           ELSE
               IF WS-RESP = DFHRESP(DUPREC)
                   MOVE 'ERREUR: CLE EMP099 EXISTE DEJA' TO WS-MSG
               ELSE
                   MOVE 'ERREUR ECRITURE VSAM' TO WS-MSG
               END-IF
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(50)
                   ERASE
               END-EXEC
           END-IF.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
