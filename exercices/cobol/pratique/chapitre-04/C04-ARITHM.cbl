       IDENTIFICATION DIVISION.
       PROGRAM-ID. C04-ARITHM.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Instructions arithmetiques
      *             ADD, SUBTRACT, MULTIPLY, DIVIDE
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  WS-A            PIC 9(3)    VALUE 100.
       01  WS-B            PIC 9(3)    VALUE 50.
       01  WS-C            PIC 9(3)    VALUE 25.
       01  WS-RESULT       PIC 9(5).
       01  WS-RESULT-S     PIC S9(5).
       01  WS-QUOTIENT     PIC 9(3).
       01  WS-RESTE        PIC 9(2).

      *----------------------------------------------------------------
      * Variables decimales
      *----------------------------------------------------------------
       01  WS-PRIX         PIC 9(3)V99 VALUE 125,50.
       01  WS-QTE          PIC 9(3)    VALUE 10.
       01  WS-TOTAL        PIC 9(6)V99.
       01  WS-TAUX         PIC 9(2)V99 VALUE 20,00.

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-RESULT-E     PIC ZZZ.ZZ9.
       01  WS-TOTAL-E      PIC ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY '========================================'
           DISPLAY '  EXERCICE : Instructions arithmetiques'
           DISPLAY '========================================'
           DISPLAY ' '

           PERFORM 1000-TEST-ADD
           PERFORM 2000-TEST-SUBTRACT
           PERFORM 3000-TEST-MULTIPLY
           PERFORM 4000-TEST-DIVIDE

           STOP RUN.

      *----------------------------------------------------------------
      * ADD : Addition
      *----------------------------------------------------------------
       1000-TEST-ADD.
           DISPLAY '--- ADD (Addition) ---'

      *    ADD simple : b = b + a
           MOVE 50 TO WS-B
           ADD WS-A TO WS-B
           DISPLAY 'ADD 100 TO 50         : ' WS-B

      *    ADD GIVING : c = a + b
           ADD 100 50 GIVING WS-RESULT
           DISPLAY 'ADD 100 50 GIVING     : ' WS-RESULT

      *    ADD multiple
           ADD 10 20 30 40 GIVING WS-RESULT
           DISPLAY 'ADD 10 20 30 40       : ' WS-RESULT

           DISPLAY ' '.

      *----------------------------------------------------------------
      * SUBTRACT : Soustraction
      *----------------------------------------------------------------
       2000-TEST-SUBTRACT.
           DISPLAY '--- SUBTRACT (Soustraction) ---'

      *    SUBTRACT simple : b = b - a
           MOVE 100 TO WS-A
           MOVE 30 TO WS-B
           SUBTRACT WS-B FROM WS-A
           DISPLAY 'SUBTRACT 30 FROM 100  : ' WS-A

      *    SUBTRACT GIVING
           SUBTRACT 25 FROM 100 GIVING WS-RESULT
           DISPLAY 'SUBTRACT 25 FROM 100  : ' WS-RESULT

      *    SUBTRACT avec resultat negatif
           SUBTRACT 150 FROM 100 GIVING WS-RESULT-S
           DISPLAY 'SUBTRACT 150 FROM 100 : ' WS-RESULT-S

           DISPLAY ' '.

      *----------------------------------------------------------------
      * MULTIPLY : Multiplication
      *----------------------------------------------------------------
       3000-TEST-MULTIPLY.
           DISPLAY '--- MULTIPLY (Multiplication) ---'

      *    MULTIPLY simple
           MOVE 10 TO WS-A
           MULTIPLY 5 BY WS-A
           DISPLAY 'MULTIPLY 5 BY 10      : ' WS-A

      *    MULTIPLY GIVING
           MULTIPLY 125,50 BY 10 GIVING WS-TOTAL
           MOVE WS-TOTAL TO WS-TOTAL-E
           DISPLAY 'MULTIPLY 125,50 BY 10 : ' WS-TOTAL-E

      *    MULTIPLY avec ROUNDED
           MULTIPLY WS-PRIX BY WS-QTE GIVING WS-TOTAL ROUNDED
           MOVE WS-TOTAL TO WS-TOTAL-E
           DISPLAY 'Prix x Qte (ROUNDED)  : ' WS-TOTAL-E

           DISPLAY ' '.

      *----------------------------------------------------------------
      * DIVIDE : Division
      *----------------------------------------------------------------
       4000-TEST-DIVIDE.
           DISPLAY '--- DIVIDE (Division) ---'

      *    DIVIDE INTO
           MOVE 100 TO WS-A
           DIVIDE 5 INTO WS-A
           DISPLAY 'DIVIDE 5 INTO 100     : ' WS-A

      *    DIVIDE BY GIVING
           DIVIDE 100 BY 3 GIVING WS-QUOTIENT
           DISPLAY 'DIVIDE 100 BY 3       : ' WS-QUOTIENT

      *    DIVIDE avec REMAINDER
           DIVIDE 100 BY 3 GIVING WS-QUOTIENT REMAINDER WS-RESTE
           DISPLAY 'DIVIDE 100 BY 3       : ' WS-QUOTIENT
               ' reste ' WS-RESTE

      *    DIVIDE avec ROUNDED
           DIVIDE 100 BY 3 GIVING WS-QUOTIENT ROUNDED
           DISPLAY 'DIVIDE 100 BY 3 ROUND : ' WS-QUOTIENT

      *    DIVIDE avec ON SIZE ERROR
           DIVIDE 100 BY 0 GIVING WS-QUOTIENT
               ON SIZE ERROR
                   DISPLAY 'ERREUR : Division par zero !'
               NOT ON SIZE ERROR
                   DISPLAY 'Resultat : ' WS-QUOTIENT
           END-DIVIDE

           DISPLAY ' '.
