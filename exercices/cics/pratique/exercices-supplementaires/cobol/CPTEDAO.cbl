      ******************************************************************
      * Programme : CPTEDAO - Gestion Comptes (Données)
      * Fonction : Accès fichier VSAM COMPTE avec browse
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPTEDAO.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-DATA-BUFFER          PIC X(60).
       01  WS-CLE                  PIC X(11).

       01  WS-COMMAREA.
           05  DAO-ACTION          PIC X(4).
               88  DAO-READ        VALUE 'READ'.
               88  DAO-NEXT        VALUE 'NEXT'.
               88  DAO-PREV        VALUE 'PREV'.
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(11).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(60).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(90).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 0 TO DAO-RESP

           EVALUATE TRUE
               WHEN DAO-READ
                   PERFORM 1000-LIRE
               WHEN DAO-NEXT
                   PERFORM 2000-LIRE-SUIVANT
               WHEN DAO-PREV
                   PERFORM 3000-LIRE-PRECEDENT
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

       1000-LIRE.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

       2000-LIRE-SUIVANT.

           MOVE DAO-CLE TO WS-CLE

           EXEC CICS
               STARTBR FILE(DAO-FICHIER)
                       RIDFLD(WS-CLE)
                       GTEQ
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 20 TO DAO-RESP
               GO TO 2000-EXIT
           END-IF

           EXEC CICS
               READNEXT FILE(DAO-FICHIER)
                        INTO(WS-DATA-BUFFER)
                        RIDFLD(WS-CLE)
                        RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               EXEC CICS
                   READNEXT FILE(DAO-FICHIER)
                            INTO(WS-DATA-BUFFER)
                            RIDFLD(WS-CLE)
                            RESP(WS-RESP)
               END-EXEC
           END-IF

           EXEC CICS
               ENDBR FILE(DAO-FICHIER)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

       2000-EXIT.
           EXIT.

       3000-LIRE-PRECEDENT.

           MOVE DAO-CLE TO WS-CLE

           EXEC CICS
               STARTBR FILE(DAO-FICHIER)
                       RIDFLD(WS-CLE)
                       GTEQ
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 20 TO DAO-RESP
               GO TO 3000-EXIT
           END-IF

           EXEC CICS
               READPREV FILE(DAO-FICHIER)
                        INTO(WS-DATA-BUFFER)
                        RIDFLD(WS-CLE)
                        RESP(WS-RESP)
           END-EXEC

           EXEC CICS
               ENDBR FILE(DAO-FICHIER)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

       3000-EXIT.
           EXIT.

       9000-TRAITER-RESP.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO DAO-RESP
                   MOVE WS-DATA-BUFFER TO DAO-DATA
                   MOVE WS-CLE TO DAO-CLE
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO DAO-RESP
               WHEN DFHRESP(ENDFILE)
                   MOVE 20 TO DAO-RESP
               WHEN OTHER
                   MOVE 99 TO DAO-RESP
           END-EVALUATE.
