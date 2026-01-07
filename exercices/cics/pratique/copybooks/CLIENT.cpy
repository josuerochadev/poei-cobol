      *================================================================*
      * Copybook : CLIENT.cpy
      * Description : Structure enregistrement fichier CLIENT
      * Fichier : FCLIENT (KSDS, LRECL=80, CLE=3)
      * Utilise par : PRGREAD, PRGRGEN (Chapitre VIII)
      *================================================================*
       01  WS-REC-DATA.
           05  WS-CDECLT         PIC 9(3).
           05  WS-CODREG         PIC 99.
           05  WS-NATCPT         PIC 99.
           05  WS-NOMCPT         PIC X(10).
           05  WS-PRNCPT         PIC X(10).
           05  WS-DTNCPT         PIC 9(8).
           05  WS-SEXCPT         PIC X(1).
           05  WS-APRCPT         PIC 9(2).
           05  WS-SOCCPT         PIC X(1).
           05  WS-ADRCPT         PIC X(10).
           05  WS-SLDCPT         PIC 9(10).
           05  WS-POSCPT         PIC X(02).
           05  FILLER            PIC X(19).
      *================================================================*
