       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGWRIT.
      *
      * Programme : PROGWRIT - Ecriture simple VSAM avec CICS
      * Transaction : WRIT
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

       01  WS-MSG                  PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
      *    Initialiser tout l'enregistrement a SPACES
           INITIALIZE WS-EMPLOYE REPLACING
               ALPHANUMERIC DATA BY SPACES
               NUMERIC DATA BY ZEROES.

      *    Remplir les champs
           MOVE 'EMP099' TO EMP-ID.
           MOVE 'TEST EMPLOYE CICS'  TO EMP-NAME.
           MOVE 'TEST'     TO EMP-DEPT.
           MOVE 012345678  TO EMP-SALAIRE.
           MOVE 'N'        TO EMP-ETAT-CRED.
           MOVE SPACES     TO EMP-FILLER.

      *    Ecriture VSAM
           EXEC CICS WRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RIDFLD(EMP-ID)
               RESP(WS-RESP)
           END-EXEC.

      *    Afficher resultat
           INITIALIZE WS-MSG.
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT CREE: EMP099' TO WS-MSG
           ELSE
               IF WS-RESP = DFHRESP(DUPREC)
                   MOVE 'ERREUR: CLE EMP099 EXISTE DEJA' TO WS-MSG
               ELSE
                   MOVE 'ERREUR ECRITURE VSAM' TO WS-MSG
               END-IF
           END-IF.

           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               LENGTH(60)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
