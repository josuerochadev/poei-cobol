       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG08CH05.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : PERFORM VARYING
      *             Boucle simple avec compteur
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  WS-A            PIC 99 VALUE 0.
       01  WS-LIMITE       PIC 99 VALUE 10.
       01  WS-SOMME        PIC 9(4) VALUE 0.

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-SOMME-E      PIC Z(3)9.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  PERFORM VARYING - Boucle simple'
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY 'Limite = ' WS-LIMITE
           DISPLAY ' '

           PERFORM 1000-BOUCLE
               VARYING WS-A FROM 1 BY 1
               UNTIL WS-A > WS-LIMITE.

           DISPLAY ' '
           MOVE WS-SOMME TO WS-SOMME-E
           DISPLAY 'Somme de 1 a ' WS-LIMITE ' = ' WS-SOMME-E
           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Paragraphe execute dans la boucle
      *----------------------------------------------------------------
       1000-BOUCLE.
           DISPLAY 'Iteration : A = ' WS-A
           ADD WS-A TO WS-SOMME.
