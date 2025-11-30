       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. POEI-COBOL.
      ******************************************************************
      * Premier programme COBOL - Hello World                          *
      * Affiche un message simple à l'écran                            *
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGE    PIC X(30) VALUE 'Bienvenue dans COBOL !'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "===============================".
           DISPLAY WS-MESSAGE.
           DISPLAY "===============================".
           STOP RUN.
