       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGWRIT.
      *================================================================*
      * Programme : PRGWRIT - WRITE precede par READ (Chapitre VIII)   *
      * Transaction : TWRI                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Verification existence avant ecriture            *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPWRIT.

       01  WS-RESPCODE          PIC S9(8) COMP.
       01  WS-REC-LEN           PIC S9(4) COMP.
       01  WS-KEY-LEN           PIC S9(4) COMP.
       01  WS-REC-KEY           PIC 9(3).

       01  WS-REC-DATA.
           05  WS-CDECLT        PIC X(3).
           05  WS-CODREG        PIC 99.
           05  WS-NATCPT        PIC 99.
           05  WS-NOMCPT        PIC X(10).
           05  WS-PRNCPT        PIC X(10).
           05  WS-DTNCPT        PIC 9(8).
           05  WS-SEXCPT        PIC X(1).
           05  WS-APRCPT        PIC 9(2).
           05  WS-SOCCPT        PIC X(1).
           05  WS-ADRCPT        PIC X(10).
           05  WS-SLDCPT        PIC 9(10).
           05  WS-POSCPT        PIC X(02).
           05  FILLER           PIC X(19).

       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE 80  TO WS-REC-LEN.
           MOVE 000 TO WS-REC-KEY.
           MOVE 3   TO WS-KEY-LEN.

           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPWRIT') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPWRIT') MAPONLY FREEKB
           END-EXEC.

           EXEC CICS SEND MAP('MAP3')
               MAPSET('MAPWRIT') MAPONLY FREEKB
           END-EXEC.

           MOVE 'O' TO FLAGERRI.

           PERFORM UNTIL FLAGERRI = 'N'

               EXEC CICS RECEIVE MAP('MAP1')
                   MAPSET('MAPWRIT')
               END-EXEC

               MOVE CDECLTI TO WS-REC-KEY

               EXEC CICS READ FILE('FCLIENT') INTO(WS-REC-DATA)
                   LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
                   KEYLENGTH(WS-KEY-LEN) RESP(WS-RESPCODE)
               END-EXEC

               IF WS-RESPCODE = DFHRESP(NORMAL)
                   MOVE 'RECORD DOUBLE CONTINUEZ (O/N)' TO MSGINFO
                   EXEC CICS SEND MAP('MAP3')
                       MAPSET('MAPWRIT') DATAONLY FREEKB
                   END-EXEC
                   EXEC CICS RECEIVE MAP('MAP3')
                       MAPSET('MAPWRIT')
                   END-EXEC
                   EVALUATE TRUE
                       WHEN FLAGERRI = 'O'
                           MOVE ' ' TO CDECLTI
                       WHEN OTHER
                           MOVE 'N' TO FLAGERRI
                   END-EVALUATE
               ELSE
                   EXEC CICS RECEIVE MAP('MAP2')
                       MAPSET('MAPWRIT')
                   END-EXEC
                   MOVE CDECLTI TO WS-CDECLT
                   MOVE CODREGI TO WS-CODREG
                   MOVE NATCPTI TO WS-NATCPT
                   MOVE NOMCPTI TO WS-NOMCPT
                   MOVE PRNCPTI TO WS-PRNCPT
                   MOVE DTNCPTI TO WS-DTNCPT
                   MOVE SEXCPTI TO WS-SEXCPT
                   MOVE APRCPTI TO WS-APRCPT
                   MOVE SOCCPTI TO WS-SOCCPT
                   MOVE ADRCPTI TO WS-ADRCPT
                   MOVE SLDCPTI TO WS-SLDCPT
                   MOVE POSCPTI TO WS-POSCPT

                   EXEC CICS WRITE FILE('FCLIENT') FROM(WS-REC-DATA)
                       LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
                       KEYLENGTH(WS-KEY-LEN) RESP(WS-RESPCODE)
                   END-EXEC

                   MOVE 'NOUVELLE SAISIE       (O/N)' TO MSGINFO
                   EXEC CICS SEND MAP('MAP3')
                       MAPSET('MAPWRIT') DATAONLY FREEKB
                   END-EXEC
                   EXEC CICS RECEIVE MAP('MAP3')
                       MAPSET('MAPWRIT')
                   END-EXEC
                   EVALUATE TRUE
                       WHEN FLAGERRI = 'O'
                           MOVE ' ' TO CDECLTI
                       WHEN OTHER
                           MOVE 'N' TO FLAGERRI
                   END-EVALUATE
               END-IF

           END-PERFORM.

           EXEC CICS SEND CONTROL ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
