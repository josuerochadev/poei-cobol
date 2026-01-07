       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRWRSPL.
      *================================================================*
      * Programme : PRWRSPL - Ecriture simple VSAM (Chapitre VIII)     *
      * Transaction : TWRT                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Ecriture d'un nouvel enregistrement client       *
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

           EXEC CICS RECEIVE MAP('MAP1')
               MAPSET('MAPWRIT')
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPWRIT') MAPONLY FREEKB
           END-EXEC.

           EXEC CICS RECEIVE MAP('MAP2')
               MAPSET('MAPWRIT')
           END-EXEC.

           MOVE CDECLTI  TO WS-CDECLT WS-REC-KEY.
           MOVE CODREGI  TO WS-CODREG.
           MOVE NATCPTI  TO WS-NATCPT.
           MOVE NOMCPTI  TO WS-NOMCPT.
           MOVE PRNCPTI  TO WS-PRNCPT.
           MOVE DTNCPTI  TO WS-DTNCPT.
           MOVE SEXCPTI  TO WS-SEXCPT.
           MOVE APRCPTI  TO WS-APRCPT.
           MOVE SOCCPTI  TO WS-SOCCPT.
           MOVE ADRCPTI  TO WS-ADRCPT.
           MOVE SLDCPTI  TO WS-SLDCPT.
           MOVE POSCPTI  TO WS-POSCPT.

           EXEC CICS WRITE FILE('FCLIENT') FROM(WS-REC-DATA)
               LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
               KEYLENGTH(WS-KEY-LEN) RESP(WS-RESPCODE)
           END-EXEC.

           EXEC CICS SEND MAP('MAP3')
               MAPSET('MAPWRIT') MAPONLY FREEKB
           END-EXEC.

           EVALUATE TRUE
               WHEN WS-RESPCODE = DFHRESP(DUPREC)
                   MOVE 'ECRITURE RECORD KEY DOUBLE    ' TO MSGINFO
               WHEN WS-RESPCODE = DFHRESP(FILENOTFOUND)
                   MOVE 'ECRITURE FILE NOT EXIST       ' TO MSGINFO
               WHEN WS-RESPCODE = DFHRESP(IOERR)
                   MOVE 'ERREUR E/S SUR LE FICHIER     ' TO MSGINFO
               WHEN WS-RESPCODE = DFHRESP(INVREQ)
                   MOVE 'KEY RECORD ET RIDFLD DOIT IDEN' TO MSGINFO
               WHEN WS-RESPCODE = DFHRESP(NORMAL)
                   MOVE 'ECRITURE RECORD VALIDE        ' TO MSGINFO
           END-EVALUATE.

           EXEC CICS SEND MAP('MAP3')
               MAPSET('MAPWRIT') DATAONLY FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
