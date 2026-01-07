       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGRGEN.
      *================================================================*
      * Programme : PRGRGEN - Lecture generique VSAM (Chapitre VIII)   *
      * Transaction : TGEN                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Lecture avec cle partielle (GENERIC)             *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPREAD.

       01  WS-REC-LEN            PIC S9(4) COMP.
       01  WS-KEY-LEN            PIC S9(4) COMP.
       01  WS-REC-KEY            PIC 9(3).

       01  WS-REC-DATA.
           05  WS-CDECLT         PIC X(3).
           05  WS-CODREG         PIC 99.
           05  WS-NATCPT         PIC 99.
           05  WS-NOMCPT         PIC X(10).
           05  WS-PRNCPT         PIC X(10).
           05  WS-DTNCPT         PIC 9(8).
           05  WS-SEXCPT         PIC X(1).
           05  WS-APRCPT         PIC 9(2).
           05  WS-SOCCPT         PIC X(1).
           05  WS-ADRCPT         PIC X(10).
           05  WS-SLDCPT         PIC 9(8)V9(2).
           05  WS-POSCPT         PIC X(02).
           05  FILLER            PIC X(19).

       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE 80  TO WS-REC-LEN.
           MOVE 005 TO WS-REC-KEY.
           MOVE 3   TO WS-KEY-LEN.

           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPREAD') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS RECEIVE MAP('MAP1')
               MAPSET('MAPREAD')
           END-EXEC.

           MOVE CDECLTI TO WS-REC-KEY.

           EXEC CICS READ FILE('FCLIENT') INTO(WS-REC-DATA)
               LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
               KEYLENGTH(2) GENERIC
           END-EXEC.

           MOVE WS-CDECLT TO CDECLTO.
           MOVE WS-CODREG TO CODREGO.
           MOVE WS-NATCPT TO NATCPTO.
           MOVE WS-NOMCPT TO NOMCPTO.
           MOVE WS-PRNCPT TO PRNCPTO.
           MOVE WS-DTNCPT TO DTNCPTO.
           MOVE WS-SEXCPT TO SEXCPTO.
           MOVE WS-APRCPT TO APRCPTO.
           MOVE WS-SOCCPT TO SOCCPTO.
           MOVE WS-ADRCPT TO ADRCPTO.
           MOVE WS-SLDCPT TO SLDCPTO.
           MOVE WS-POSCPT TO POSCPTO.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPREAD') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPREAD') DATAONLY FREEKB
           END-EXEC.

           EXEC CICS
               RETURN
           END-EXEC.

           STOP RUN.
