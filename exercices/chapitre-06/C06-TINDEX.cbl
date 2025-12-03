       IDENTIFICATION DIVISION.
       PROGRAM-ID. TINDEX.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Instruction SET sur les Index
      *
      * Structure de la table :
      *   TABLE-TEST (24 octets)
      *   └─ VAR1 OCCURS 3 INDEXED BY I
      *      ├─ VAR2 PIC A(2)
      *      └─ VAR3 OCCURS 2 INDEXED BY J
      *         └─ VAR4 PIC X(3)
      *
      * Donnees : '12ABCDEF34GHIJKL56MNOPQR'
      *   SET I J TO 1     -> VAR3(1,1) = 'ABC'
      *   SET I J UP BY 1  -> VAR3(2,2) = 'JKL'
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Table a 2 dimensions avec INDEX
      *----------------------------------------------------------------
       01  TABLE-TEST.
           05  VAR1 OCCURS 3 TIMES INDEXED BY I.
               10  VAR2       PIC A(2).
               10  VAR3 OCCURS 2 TIMES INDEXED BY J.
                   15  VAR4   PIC X(3).

      *----------------------------------------------------------------
      * Variables pour afficher la position des index
      *----------------------------------------------------------------
       01  WS-I               PIC 9.
       01  WS-J               PIC 9.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Exercice 2 : Instruction SET sur les Index'
           DISPLAY '=================================================='
           DISPLAY ' '

           MOVE '12ABCDEF34GHIJKL56MNOPQR' TO TABLE-TEST

           DISPLAY 'Donnees chargees : 12ABCDEF34GHIJKL56MNOPQR'
           DISPLAY ' '
           DISPLAY '--------------------------------------------------'
           DISPLAY 'Manipulation des index avec SET :'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '

      *----------------------------------------------------------------
      * Initialiser les index a 1
      *----------------------------------------------------------------
           SET I J TO 1
           SET WS-I TO I
           SET WS-J TO J
           DISPLAY 'SET I J TO 1'
           DISPLAY '  I = ' WS-I ', J = ' WS-J
           DISPLAY '  VAR3(I,J) = VAR3(' WS-I ',' WS-J ') = ' VAR3(I,J)
           DISPLAY ' '

      *----------------------------------------------------------------
      * Incrementer les index de 1
      *----------------------------------------------------------------
           SET I J UP BY 1
           SET WS-I TO I
           SET WS-J TO J
           DISPLAY 'SET I J UP BY 1'
           DISPLAY '  I = ' WS-I ', J = ' WS-J
           DISPLAY '  VAR3(I,J) = VAR3(' WS-I ',' WS-J ') = ' VAR3(I,J)
           DISPLAY ' '

      *----------------------------------------------------------------
      * Demonstration supplementaire
      *----------------------------------------------------------------
           DISPLAY '--------------------------------------------------'
           DISPLAY 'Demonstrations supplementaires :'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '

      * Positionner a une valeur specifique
           SET I TO 3
           SET J TO 1
           SET WS-I TO I
           SET WS-J TO J
           DISPLAY 'SET I TO 3, SET J TO 1'
           DISPLAY '  VAR3(' WS-I ',' WS-J ') = ' VAR3(I,J)
           DISPLAY ' '

      * Decrementer
           SET I DOWN BY 1
           SET WS-I TO I
           DISPLAY 'SET I DOWN BY 1'
           DISPLAY '  VAR3(' WS-I ',' WS-J ') = ' VAR3(I,J)
           DISPLAY ' '

           DISPLAY '=================================================='

           STOP RUN.
