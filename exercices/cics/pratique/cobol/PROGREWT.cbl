       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGREWT.
      *
      * Programme : PROGREWT - Mise a jour simple VSAM avec CICS
      * Transaction : REWT
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
      *    Cle a modifier
           MOVE 'EMP001' TO WS-REC-KEY.

      *    Lecture avec verrouillage (UPDATE)
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               UPDATE
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR LECTURE - EMP001 NON TROUVE' TO WS-MSG
               EXEC CICS SEND TEXT
                   FROM(WS-MSG)
                   LENGTH(60)
                   ERASE
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.

      *    Modifier les donnees (garder le filler propre)
           MOVE 'DUPONT JEAN (MODIFIE)' TO EMP-NAME.
           MOVE 'DIRECTION'  TO EMP-DEPT.
           ADD 1000000 TO EMP-SALAIRE.
           MOVE SPACES TO EMP-FILLER.

      *    Reecriture
           EXEC CICS REWRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RESP(WS-RESP)
           END-EXEC.

      *    Afficher resultat
           INITIALIZE WS-MSG.
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'EMP001 MODIFIE AVEC SUCCES' TO WS-MSG
           ELSE
               MOVE 'ERREUR REWRITE' TO WS-MSG
           END-IF.

           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               LENGTH(60)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
