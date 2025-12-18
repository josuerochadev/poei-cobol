       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTMVT.
      *---------------------------------------------------------
      * P3 EXERCICE 9 : Total mouvements d'un client
      * Recupere NUM_COMPTE via ACCEPT (In-Stream JCL)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable d'entree (ACCEPT)
       01 WS-NUM-COMPTE     PIC X(03).

      * Variables host pour DB2
       01 WS-TOTAL-MVT      PIC S9(10)V99 COMP-3.
       01 WS-NB-MVT         PIC S9(05) COMP.
       01 WS-NOM-CLIENT     PIC X(10).

      * Variables d'edition
       01 WS-TOTAL-ED       PIC -ZZZ,ZZZ,ZZ9.99.
       01 WS-NB-ED          PIC ZZ9.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-LIRE-NUM-COMPTE
           PERFORM 2000-VERIFIER-CLIENT
           PERFORM 3000-CALCULER-TOTAUX
           PERFORM 9000-FIN
           STOP RUN.

       1000-LIRE-NUM-COMPTE.
           ACCEPT WS-NUM-COMPTE
           DISPLAY 'NUMERO COMPTE SAISI : [' WS-NUM-COMPTE ']'

           IF WS-NUM-COMPTE = SPACES
               DISPLAY 'ERREUR : NUMERO COMPTE VIDE'
               DISPLAY 'VERIFIER SYSIN DANS LE JCL'
               STOP RUN
           END-IF.

       2000-VERIFIER-CLIENT.
           EXEC SQL
               SELECT NOM_CLIENT
               INTO :WS-NOM-CLIENT
               FROM CLIENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           IF SQLCODE = 100
               DISPLAY 'CLIENT NON TROUVE : ' WS-NUM-COMPTE
               STOP RUN
           END-IF
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR SQL : ' SQLCODE
               STOP RUN
           END-IF.

       3000-CALCULER-TOTAUX.
           EXEC SQL
               SELECT COALESCE(SUM(MONTANT_MVT), 0),
                      COUNT(*)
               INTO :WS-TOTAL-MVT, :WS-NB-MVT
               FROM MOUVEMENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           IF SQLCODE = 0
               DISPLAY '================================'
               DISPLAY 'CLIENT : ' WS-NOM-CLIENT
               DISPLAY 'COMPTE : ' WS-NUM-COMPTE
               DISPLAY '================================'
               MOVE WS-NB-MVT TO WS-NB-ED
               DISPLAY 'NOMBRE MOUVEMENTS : ' WS-NB-ED
               MOVE WS-TOTAL-MVT TO WS-TOTAL-ED
               DISPLAY 'TOTAL MOUVEMENTS  : ' WS-TOTAL-ED
           ELSE
               DISPLAY 'ERREUR CALCUL : ' SQLCODE
           END-IF.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME TOTMVT'.
