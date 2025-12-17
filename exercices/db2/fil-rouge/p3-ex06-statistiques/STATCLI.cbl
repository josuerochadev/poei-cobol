       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATCLI.
      *---------------------------------------------------------
      * P3 EXERCICE 6 : Statistiques DB/CR (total et moyenne)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2 - Debiteurs
       01 WS-TOTAL-DB       PIC S9(10)V99 COMP-3.
       01 WS-MOYENNE-DB     PIC S9(10)V99 COMP-3.
       01 WS-COUNT-DB       PIC S9(05) COMP.

      * Variables host pour DB2 - Crediteurs
       01 WS-TOTAL-CR       PIC S9(10)V99 COMP-3.
       01 WS-MOYENNE-CR     PIC S9(10)V99 COMP-3.
       01 WS-COUNT-CR       PIC S9(05) COMP.

      * Variables d'edition
       01 WS-TOTAL-ED       PIC -ZZZ,ZZZ,ZZ9.99.
       01 WS-MOYENNE-ED     PIC -ZZZ,ZZZ,ZZ9.99.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-STATS-DEBITEURS
           PERFORM 2000-STATS-CREDITEURS
           PERFORM 3000-AFFICHER-RESULTATS
           PERFORM 9000-FIN
           STOP RUN.

       1000-STATS-DEBITEURS.
           EXEC SQL
               SELECT SUM(SOLDE), AVG(SOLDE), COUNT(*)
               INTO :WS-TOTAL-DB, :WS-MOYENNE-DB, :WS-COUNT-DB
               FROM CLIENT
               WHERE POS = 'DB'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR STATS DEBITEURS : ' SQLCODE
           END-IF.

       2000-STATS-CREDITEURS.
           EXEC SQL
               SELECT SUM(SOLDE), AVG(SOLDE), COUNT(*)
               INTO :WS-TOTAL-CR, :WS-MOYENNE-CR, :WS-COUNT-CR
               FROM CLIENT
               WHERE POS = 'CR'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR STATS CREDITEURS : ' SQLCODE
           END-IF.

       3000-AFFICHER-RESULTATS.
           DISPLAY '=== STATISTIQUES DES CLIENTS ==='
           DISPLAY ' '
           DISPLAY '--- DEBITEURS ---'
           MOVE WS-TOTAL-DB TO WS-TOTAL-ED
           DISPLAY 'TOTAL   : ' WS-TOTAL-ED
           MOVE WS-MOYENNE-DB TO WS-MOYENNE-ED
           DISPLAY 'MOYENNE : ' WS-MOYENNE-ED
           DISPLAY ' '
           DISPLAY '--- CREDITEURS ---'
           MOVE WS-TOTAL-CR TO WS-TOTAL-ED
           DISPLAY 'TOTAL   : ' WS-TOTAL-ED
           MOVE WS-MOYENNE-CR TO WS-MOYENNE-ED
           DISPLAY 'MOYENNE : ' WS-MOYENNE-ED.

       9000-FIN.
           DISPLAY ' '
           DISPLAY 'FIN DU PROGRAMME STATCLI'.
