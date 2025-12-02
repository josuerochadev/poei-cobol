       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG01CH01.
       AUTHOR.     ROCHA.  
      * PROGRAMME D'AFFICHAGE (LE DISPLAY)
      *---------------------------------------------------------
      * Ce programme illustre :
      * - L'instruction DISPLAY
      * - Les chaines de caracteres (guillemets, apostrophes)
      * - La continuation de ligne (colonne 7 = tiret)
      * - Le mode DEBUGGING (ligne D en colonne 7)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC WITH DEBUGGING MODE.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WMESSAGE      PIC X(35).
       01  WMESSAGE2     PIC X(35).
       01  WMESSAGE3     PIC X(35).
       01  WMESSAGE4     PIC X(99).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INIT.
           PERFORM 2000-TRAIT.
           PERFORM 9000-FIN.
           STOP RUN.

       1000-INIT.
           MOVE 'BIENVENUE A LA FORMATION COBOL'   TO WMESSAGE.
           MOVE 'ON VA S"INITIER AU LGGE COBOL'    TO WMESSAGE2.
           MOVE 'ON S''AMELIORERA JOUR APRES JOUR' TO WMESSAGE3.
           MOVE 'AU 10EME JOUR DE LA FORMATION, NOUS SERONS DES PR
      -    'OGRAMMEURS OPERATIONNELS' TO WMESSAGE4.

       2000-TRAIT.
      D    DISPLAY 'JE SUIS BIEN DANS LE MODE DEBUGGING'.
           DISPLAY 'LE MESSAGE1 : ' WMESSAGE.
           DISPLAY 'LE MESSAGE2 : ' WMESSAGE2.
           DISPLAY 'LE MESSAGE3 : ' WMESSAGE3.
           DISPLAY 'LE MESSAGE4 : ' WMESSAGE4.

       9000-FIN.
           DISPLAY '=== FIN DU PROGRAMME PG01CH01 ==='.
