       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDELGEN.
      *================================================================*
      * Programme : PDELGEN - DELETE generique sans READ (Chap VIII)   *
      * Transaction : TDLG                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Suppression generique par cle partielle          *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MDELG.

       01  WS-REC-LEN           PIC S9(4) COMP.
       01  WS-KEY-LEN           PIC S9(4) COMP.
       01  WS-DEL-REC           PIC S9(4) COMP.
       01  WS-REC-KEY           PIC X(3).
       01  NBR-DELREC           PIC 9(3).

       01  WS-REC-DATA.
           05  WS-CDECLT        PIC X(3).
           05  FILLER           PIC X(77).

       PROCEDURE DIVISION.

       MAIN-PARA.

           EXEC CICS SEND MAP('MAPDEL')
               MAPSET('MDELG') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS RECEIVE MAP('MAPDEL')
               MAPSET('MDELG')
           END-EXEC.

           MOVE VALKEYI  TO WS-REC-KEY.
           MOVE LENKEYI  TO WS-KEY-LEN.

      *------- DELETE generique sans READ prealable -------------------
           EXEC CICS DELETE FILE('FCLIENT')
               RIDFLD(WS-REC-KEY) KEYLENGTH(WS-KEY-LEN)
               GENERIC NUMREC(WS-DEL-REC)
           END-EXEC.

           MOVE WS-DEL-REC TO NBR-DELREC.

           STRING '*** NOMBRE DE RECORD       : ' DELIMITED SIZE
                  NBR-DELREC DELIMITED SPACE
                  ' ***' DELIMITED SIZE
                  INTO NBRDELO
           END-STRING.

           EXEC CICS SEND MAP('MAPDEL')
               MAPSET('MDELG') DATAONLY FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
