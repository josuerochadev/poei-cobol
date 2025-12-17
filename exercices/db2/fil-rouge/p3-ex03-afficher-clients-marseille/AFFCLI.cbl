       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFFCLI.
      *---------------------------------------------------------
      * P3 EXERCICE 3 : Afficher tous les clients de Marseille
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-PREN-CLIENT    PIC X(10).
       01 WS-SOLDE          PIC S9(8)V99 COMP-3.
       01 WS-POS            PIC X(02).

      * Variable de travail
       01 WS-SOLDE-ED       PIC -ZZZ,ZZ9.99.
       01 WS-COMPTEUR       PIC 9(03) VALUE 0.
       01 WS-FIN-CURSOR     PIC 9(01) VALUE 0.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * Declaration du curseur
           EXEC SQL
               DECLARE C-CLIENTS CURSOR FOR
               SELECT NUM_COMPTE, NOM_CLIENT, PREN_CLIENT,
                      SOLDE, POS
               FROM CLIENT
               WHERE CODE_REGION = '02'
               ORDER BY NUM_COMPTE
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-OUVRIR-CURSOR
           PERFORM 2000-AFFICHER-ENTETE
           PERFORM 3000-LIRE-CLIENTS
               UNTIL WS-FIN-CURSOR = 1
           PERFORM 4000-FERMER-CURSOR
           PERFORM 9000-FIN
           STOP RUN.

       1000-OUVRIR-CURSOR.
           EXEC SQL
               OPEN C-CLIENTS
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR OUVERTURE CURSOR : ' SQLCODE
               MOVE 1 TO WS-FIN-CURSOR
           END-IF.

       2000-AFFICHER-ENTETE.
           DISPLAY '========================================'
           DISPLAY 'CLIENTS DE LA REGION MARSEILLE (02)'
           DISPLAY '========================================'
           DISPLAY 'NUM   NOM        PRENOM     SOLDE      POS'.

       3000-LIRE-CLIENTS.
           EXEC SQL
               FETCH C-CLIENTS
               INTO :WS-NUM-COMPTE, :WS-NOM-CLIENT,
                    :WS-PREN-CLIENT, :WS-SOLDE, :WS-POS
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-COMPTEUR
                   MOVE WS-SOLDE TO WS-SOLDE-ED
                   DISPLAY WS-NUM-COMPTE ' '
                           WS-NOM-CLIENT ' '
                           WS-PREN-CLIENT ' '
                           WS-SOLDE-ED ' '
                           WS-POS
               WHEN 100
                   MOVE 1 TO WS-FIN-CURSOR
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH : ' SQLCODE
                   MOVE 1 TO WS-FIN-CURSOR
           END-EVALUATE.

       4000-FERMER-CURSOR.
           EXEC SQL
               CLOSE C-CLIENTS
           END-EXEC
           DISPLAY '========================================'
           DISPLAY 'TOTAL CLIENTS : ' WS-COMPTEUR.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME AFFCLI'.
