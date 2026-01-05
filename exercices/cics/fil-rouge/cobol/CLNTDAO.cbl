      ******************************************************************
      * Programme : CLNTDAO - Consultation Client (Données)
      * Fonction : Accès fichier VSAM CLIENT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTDAO.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-DATA-BUFFER          PIC X(130).

       01  WS-COMMAREA.
           05  DAO-ACTION          PIC X(4).
               88  DAO-READ        VALUE 'READ'.
               88  DAO-READ-UPD    VALUE 'UPDT'.
               88  DAO-REWRITE     VALUE 'REWT'.
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(11).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(130).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(160).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 0 TO DAO-RESP

           EVALUATE TRUE
               WHEN DAO-READ
                   PERFORM 1000-LIRE
               WHEN DAO-READ-UPD
                   PERFORM 1100-LIRE-POUR-MAJ
               WHEN DAO-REWRITE
                   PERFORM 2000-REECRIRE
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

       1100-LIRE-POUR-MAJ.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

       2000-REECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               REWRITE FILE(DAO-FICHIER)
                       FROM(WS-DATA-BUFFER)
                       RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

       9000-TRAITER-RESP.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO DAO-RESP
                   MOVE WS-DATA-BUFFER TO DAO-DATA
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO DAO-RESP
               WHEN DFHRESP(DUPREC)
                   MOVE 14 TO DAO-RESP
               WHEN OTHER
                   MOVE 99 TO DAO-RESP
           END-EVALUATE.
