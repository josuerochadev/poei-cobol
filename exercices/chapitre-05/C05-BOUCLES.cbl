       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG09CH05.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : PERFORM avec boucles imbriquees
      *
      * - VAR1 de 1 a 5 (boucle externe)
      * - VAR2 de 1 a 3 (boucle milieu)
      * - VAR3 de 1 a 6 (boucle interne)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Variables de controle des boucles
      *----------------------------------------------------------------
       01  VAR1            PIC 99 VALUE 0.
       01  VAR2            PIC 99 VALUE 0.
       01  VAR3            PIC 99 VALUE 0.

      *----------------------------------------------------------------
      * Limites des boucles
      *----------------------------------------------------------------
       01  LIMITE1         PIC 99 VALUE 5.
       01  LIMITE2         PIC 99 VALUE 3.
       01  LIMITE3         PIC 99 VALUE 6.

      *----------------------------------------------------------------
      * Compteur d'iterations
      *----------------------------------------------------------------
       01  WS-COMPTEUR     PIC 9(4) VALUE 0.
       01  WS-COMPTEUR-E   PIC Z(3)9.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  PERFORM - Boucles imbriquees (3 niveaux)'
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY 'VAR1 : 1 a ' LIMITE1
           DISPLAY 'VAR2 : 1 a ' LIMITE2
           DISPLAY 'VAR3 : 1 a ' LIMITE3
           DISPLAY ' '
           DISPLAY 'Format : VAR1.VAR2.VAR3'
           DISPLAY '--------------------------------------------------'

           INITIALIZE VAR1 VAR2 VAR3 WS-COMPTEUR

           PERFORM 1000-BOUCLE-EXTERNE
               VARYING VAR1 FROM 1 BY 1
               UNTIL VAR1 > LIMITE1.

           DISPLAY '--------------------------------------------------'
           MOVE WS-COMPTEUR TO WS-COMPTEUR-E
           DISPLAY ' '
           DISPLAY 'Nombre total d''iterations : ' WS-COMPTEUR-E
           DISPLAY '(Attendu : ' LIMITE1 ' x ' LIMITE2 ' x ' LIMITE3
               ' = 90)'
           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Boucle externe (VAR1)
      *----------------------------------------------------------------
       1000-BOUCLE-EXTERNE.
           DISPLAY ' '
           DISPLAY '>>> Boucle VAR1 = ' VAR1

           PERFORM 2000-BOUCLE-MILIEU
               VARYING VAR2 FROM 1 BY 1
               UNTIL VAR2 > LIMITE2.

      *----------------------------------------------------------------
      * Boucle milieu (VAR2)
      *----------------------------------------------------------------
       2000-BOUCLE-MILIEU.
           DISPLAY '    >> Boucle VAR2 = ' VAR2

           PERFORM 3000-BOUCLE-INTERNE
               VARYING VAR3 FROM 1 BY 1
               UNTIL VAR3 > LIMITE3.

      *----------------------------------------------------------------
      * Boucle interne (VAR3)
      *----------------------------------------------------------------
       3000-BOUCLE-INTERNE.
           ADD 1 TO WS-COMPTEUR
           DISPLAY '       > ' VAR1 '.' VAR2 '.' VAR3.
