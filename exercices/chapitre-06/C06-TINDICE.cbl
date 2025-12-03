       IDENTIFICATION DIVISION.
       PROGRAM-ID. C06-TINDICE.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Indice d'adressage des Tables
      *
      * Structure de la table :
      *   TABLE-TEST (24 octets)
      *   └─ VAR1 OCCURS 3 (8 octets chacun)
      *      ├─ VAR2 PIC A(2)  (2 octets)
      *      └─ VAR3 OCCURS 2 (3 octets chacun)
      *         └─ VAR4 PIC X(3)
      *
      * Donnees : '12ABCDEF34GHIJKL56MNOPQR'
      *   VAR1(1) = '12ABCDEF'
      *     VAR2(1)   = '12'
      *     VAR3(1,1) = 'ABC'
      *     VAR3(1,2) = 'DEF'
      *   VAR1(2) = '34GHIJKL'
      *     VAR2(2)   = '34'
      *     VAR3(2,1) = 'GHI'
      *     VAR3(2,2) = 'JKL'
      *   VAR1(3) = '56MNOPQR'
      *     VAR2(3)   = '56'
      *     VAR3(3,1) = 'MNO'
      *     VAR3(3,2) = 'PQR'
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Table a 2 dimensions avec structure imbriquee
      *----------------------------------------------------------------
       01  TABLE-TEST.
           05  VAR1 OCCURS 3 TIMES.
               10  VAR2       PIC A(2).
               10  VAR3 OCCURS 2 TIMES.
                   15  VAR4   PIC X(3).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Exercice 6 : Indice d''adressage des Tables'
           DISPLAY '=================================================='
           DISPLAY ' '

           MOVE '12ABCDEF34GHIJKL56MNOPQR' TO TABLE-TEST

           DISPLAY 'Donnees chargees : 12ABCDEF34GHIJKL56MNOPQR'
           DISPLAY ' '
           DISPLAY '--------------------------------------------------'
           DISPLAY 'Affichage des elements de la table :'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '

           DISPLAY 'TABLE-TEST  : ' TABLE-TEST
           DISPLAY ' '
           DISPLAY 'VAR1(1)     : ' VAR1(1)
           DISPLAY '  VAR2(1)   : ' VAR2(1)
           DISPLAY '  VAR3(1,1) : ' VAR3(1,1)
           DISPLAY '  VAR3(1,2) : ' VAR3(1,2)
           DISPLAY ' '
           DISPLAY 'VAR1(2)     : ' VAR1(2)
           DISPLAY '  VAR2(2)   : ' VAR2(2)
           DISPLAY '  VAR3(2,1) : ' VAR3(2,1)
           DISPLAY '  VAR3(2,2) : ' VAR3(2,2)
           DISPLAY ' '
           DISPLAY 'VAR1(3)     : ' VAR1(3)
           DISPLAY '  VAR2(3)   : ' VAR2(3)
           DISPLAY '  VAR3(3,1) : ' VAR3(3,1)
           DISPLAY '  VAR3(3,2) : ' VAR3(3,2)
           DISPLAY ' '
           DISPLAY '=================================================='

           STOP RUN.
