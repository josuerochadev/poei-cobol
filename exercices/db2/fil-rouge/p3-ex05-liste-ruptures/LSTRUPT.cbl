       IDENTIFICATION DIVISION.
       PROGRAM-ID. LSTRUPT.
      *---------------------------------------------------------
      * P3 EXERCICE 5 : Liste clients tries avec ruptures
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-PREN-CLIENT    PIC X(10).
       01 WS-CODE-REGION    PIC X(02).
       01 WS-NOM-REGION     PIC X(15).
       01 WS-CODE-PROF      PIC X(02).
       01 WS-LIB-PROF       PIC X(20).
       01 WS-SOLDE          PIC S9(8)V99 COMP-3.
       01 WS-POS            PIC X(02).

      * Variables de rupture
       01 WS-PREC-REGION    PIC X(02) VALUE SPACES.
       01 WS-PREC-PROF      PIC X(02) VALUE SPACES.

      * Variables de travail
       01 WS-SOLDE-ED       PIC -ZZZ,ZZ9.99.
       01 WS-FIN-CURSOR     PIC 9(01) VALUE 0.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * Declaration du curseur
           EXEC SQL
               DECLARE C-CLIENTS CURSOR FOR
               SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
                      R.CODE_REGION, R.NOM_REGION,
                      P.CODE_PROF, P.LIB_PROF,
                      C.SOLDE, C.POS
               FROM CLIENT C
               INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
               INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
               ORDER BY C.CODE_REGION, C.CODE_PROF, C.NUM_COMPTE
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-OUVRIR-CURSOR
           PERFORM 2000-TRAITER-CLIENTS
               UNTIL WS-FIN-CURSOR = 1
           PERFORM 3000-FERMER-CURSOR
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

       2000-TRAITER-CLIENTS.
           EXEC SQL
               FETCH C-CLIENTS
               INTO :WS-NUM-COMPTE, :WS-NOM-CLIENT, :WS-PREN-CLIENT,
                    :WS-CODE-REGION, :WS-NOM-REGION,
                    :WS-CODE-PROF, :WS-LIB-PROF,
                    :WS-SOLDE, :WS-POS
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 2100-VERIFIER-RUPTURES
                   PERFORM 2200-AFFICHER-CLIENT
               WHEN 100
                   MOVE 1 TO WS-FIN-CURSOR
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH : ' SQLCODE
                   MOVE 1 TO WS-FIN-CURSOR
           END-EVALUATE.

       2100-VERIFIER-RUPTURES.
      * Rupture sur region
           IF WS-CODE-REGION NOT = WS-PREC-REGION
               DISPLAY ' '
               DISPLAY '=== REGION : ' WS-NOM-REGION ' ==='
               MOVE WS-CODE-REGION TO WS-PREC-REGION
               MOVE SPACES TO WS-PREC-PROF
           END-IF

      * Rupture sur profession
           IF WS-CODE-PROF NOT = WS-PREC-PROF
               DISPLAY '---- PROFESSION : ' WS-LIB-PROF ' ----'
               MOVE WS-CODE-PROF TO WS-PREC-PROF
           END-IF.

       2200-AFFICHER-CLIENT.
           MOVE WS-SOLDE TO WS-SOLDE-ED
           DISPLAY '  ' WS-NUM-COMPTE ' '
                   WS-NOM-CLIENT ' '
                   WS-PREN-CLIENT ' '
                   WS-SOLDE-ED ' '
                   WS-POS.

       3000-FERMER-CURSOR.
           EXEC SQL
               CLOSE C-CLIENTS
           END-EXEC.

       9000-FIN.
           DISPLAY ' '
           DISPLAY 'FIN DU PROGRAMME LSTRUPT'.
