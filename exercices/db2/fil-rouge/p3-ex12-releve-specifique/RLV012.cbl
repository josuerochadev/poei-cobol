       IDENTIFICATION DIVISION.
       PROGRAM-ID. RLV012.
      *---------------------------------------------------------
      * P3 EXERCICE 12 : Releve compte client 012 (In-Stream)
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable d'entree (ACCEPT depuis SYSIN In-Stream)
       01 WS-NUM-COMPTE     PIC X(03).

      * Variables host pour DB2
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-PREN-CLIENT    PIC X(10).
       01 WS-DATE-MVT       PIC X(10).
       01 WS-LIB-MOUV       PIC X(15).
       01 WS-MONTANT-MVT    PIC S9(6)V99 COMP-3.
       01 WS-SENS           PIC X(02).

      * Variables d'edition
       01 WS-CREDIT-ED      PIC ZZZ,ZZ9.99.
       01 WS-DEBIT-ED       PIC ZZZ,ZZ9.99.
       01 WS-FIN-CURSOR     PIC 9(01) VALUE 0.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * Curseur pour mouvements
           EXEC SQL
               DECLARE C-MVT-CLI CURSOR FOR
               SELECT DATE_MVT, LIB_MOUV, MONTANT_MVT, SENS
               FROM MOUVEMENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
               ORDER BY DATE_MVT
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-LIRE-NUM-COMPTE
           PERFORM 2000-RECUPERER-CLIENT
           PERFORM 3000-AFFICHER-ENTETE
           PERFORM 4000-OUVRIR-CURSOR
           PERFORM 5000-LIRE-MOUVEMENTS
               UNTIL WS-FIN-CURSOR = 1
           PERFORM 6000-FERMER-CURSOR
           PERFORM 9000-FIN
           STOP RUN.

       1000-LIRE-NUM-COMPTE.
      * Lecture depuis SYSIN (donnee In-Stream du JCL)
           ACCEPT WS-NUM-COMPTE
           DISPLAY 'COMPTE DEMANDE : ' WS-NUM-COMPTE.

       2000-RECUPERER-CLIENT.
           EXEC SQL
               SELECT NOM_CLIENT, PREN_CLIENT
               INTO :WS-NOM-CLIENT, :WS-PREN-CLIENT
               FROM CLIENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'CLIENT NON TROUVE : ' WS-NUM-COMPTE
               STOP RUN
           END-IF.

       3000-AFFICHER-ENTETE.
           DISPLAY '================================================'
           DISPLAY 'Nom Client : ' WS-NOM-CLIENT ' ' WS-PREN-CLIENT
           DISPLAY 'Numero de compte : ' WS-NUM-COMPTE
           DISPLAY '================================================'
           DISPLAY 'Date operation  Libelle         Credit    Debit'
           DISPLAY '================================================'.

       4000-OUVRIR-CURSOR.
           EXEC SQL OPEN C-MVT-CLI END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR OUVERTURE : ' SQLCODE
               MOVE 1 TO WS-FIN-CURSOR
           END-IF.

       5000-LIRE-MOUVEMENTS.
           EXEC SQL
               FETCH C-MVT-CLI
               INTO :WS-DATE-MVT, :WS-LIB-MOUV,
                    :WS-MONTANT-MVT, :WS-SENS
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   PERFORM 5100-AFFICHER-LIGNE
               WHEN 100
                   MOVE 1 TO WS-FIN-CURSOR
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH : ' SQLCODE
                   MOVE 1 TO WS-FIN-CURSOR
           END-EVALUATE.

       5100-AFFICHER-LIGNE.
           MOVE SPACES TO WS-CREDIT-ED
           MOVE SPACES TO WS-DEBIT-ED

           IF WS-SENS = 'CR'
               MOVE WS-MONTANT-MVT TO WS-CREDIT-ED
           ELSE
               MOVE WS-MONTANT-MVT TO WS-DEBIT-ED
           END-IF

           DISPLAY WS-DATE-MVT '  '
                   WS-LIB-MOUV ' '
                   WS-CREDIT-ED ' '
                   WS-DEBIT-ED.

       6000-FERMER-CURSOR.
           EXEC SQL CLOSE C-MVT-CLI END-EXEC
           DISPLAY '================================================'.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME RLV012'.
