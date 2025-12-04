       IDENTIFICATION DIVISION.
       PROGRAM-ID. C03-REDEFINES.
       AUTHOR.     N GAIGI.
      *----------------------------------------------------------------
      * PROGRAMME INITIALIZE VARIABLE GROUPE
      *           CARACTERES D'EDITION
      *           LE REDEFINES (FORMATS DIFFERENTS)
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  IDENTITE.
           05 NOM         PIC X(15).
           05 PRENOM      PIC X(15).
           05 DTENAISSANCE.
              10  DN-AA PIC 9(4).
              10  DN-MM PIC 9(2).
              10  DN-JJ PIC 9(2).
           05 ADRESSE     PIC X(25).
       01  WS-MONT1       PIC S9(4).
       01  WS-MONT2       PIC S9(4).
       01  WS-MONT3       PIC S9(4).
       01  WS-MONT4       PIC S9(4).
      *-----------------------------------
      *       LE REDEFINES
      *-----------------------------------
       01  WS-DESCRIPTION.
           05  WS-DATE1 VALUE '20251110'.
               10 WS-YEAR PIC X(4).
               10 WS-MONTH PIC X(2).
               10 WS-DAY PIC X(2).
           05  WS-DATE2 REDEFINES WS-DATE1 PIC 9(8).
      *-----------------------------------
      *       LE CARACTERE MIXTE +
      *-----------------------------------
       01  WS-MONT1-E     PIC ++999.
       01  WS-MONT2-E     PIC +++99.
       01  WS-MONT3-E     PIC ++++9.
       01  WS-MONT4-E     PIC +++++.
       PROCEDURE DIVISION.
           PERFORM INIT.
           PERFORM TRAIT.
           PERFORM FIN.
       INIT.
           INITIALIZE IDENTITE.
           MOVE -1524 TO WS-MONT4.
           MOVE 524   TO WS-MONT3.
           MOVE -24   TO WS-MONT2.
           MOVE 4     TO WS-MONT1.
       TRAIT.
           DISPLAY 'LA VALEUR INITIALE D''IDENTITE EST:' IDENTITE.
           MOVE WS-MONT1 TO WS-MONT1-E.
           DISPLAY '  4 =====> ++999   ' WS-MONT1-E.
           MOVE WS-MONT2 TO WS-MONT2-E.
           DISPLAY '-24 =====> +++99   ' WS-MONT2-E.
           MOVE WS-MONT3 TO WS-MONT3-E.
           DISPLAY '524 =====> ++++9   ' WS-MONT3-E.
           MOVE WS-MONT4 TO WS-MONT4-E.
           DISPLAY '-1524 ===> +++++   ' WS-MONT4-E.
           DISPLAY "WS-DATE1 : "   WS-DATE1.
           DISPLAY "WS-DATE2 : "   WS-DATE2.
       FIN.
           STOP RUN.
