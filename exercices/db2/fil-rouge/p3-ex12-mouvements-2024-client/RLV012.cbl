       IDENTIFICATION DIVISION.
       PROGRAM-ID. RLV012.
      *---------------------------------------------------------
      * P3 EXERCICE 12 : Mouvements 2024 d'un client (In-Stream)
      * Reprise de l'exercice 11 avec numero client en SYSIN
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable d'entree (ACCEPT depuis SYSIN In-Stream)
       01 WS-NUM-COMPTE     PIC X(03).

      * Variables host pour DB2
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-DATE-MVT       PIC X(10).
       01 WS-LIB-MOUV       PIC X(15).
       01 WS-MONTANT-MVT    PIC S9(6)V99 COMP-3.
       01 WS-SENS           PIC X(02).
       01 WS-NATURE         PIC X(03).

      * Variables de travail
       01 WS-MONTANT-ED     PIC ZZZ,ZZ9.99.
       01 WS-FIN-CURSOR     PIC 9(01) VALUE 0.
       01 WS-COMPTEUR       PIC 9(03) VALUE 0.

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      * Curseur pour mouvements 2024 d'un client specifique
           EXEC SQL
               DECLARE C-MVT2024-CLI CURSOR FOR
               SELECT DATE_MVT, LIB_MOUV, MONTANT_MVT, SENS, NATURE
               FROM MOUVEMENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
                 AND YEAR(DATE_MVT) = 2024
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
               SELECT NOM_CLIENT
               INTO :WS-NOM-CLIENT
               FROM CLIENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'CLIENT NON TROUVE : ' WS-NUM-COMPTE
               STOP RUN
           END-IF.

       3000-AFFICHER-ENTETE.
           DISPLAY '================================================'
           DISPLAY '  MOUVEMENTS 2024 - CLIENT : ' WS-NOM-CLIENT
           DISPLAY '  Numero de compte : ' WS-NUM-COMPTE
           DISPLAY '================================================'
           DISPLAY 'DATE       LIBELLE          MONTANT   SENS NAT'.

       4000-OUVRIR-CURSOR.
           EXEC SQL OPEN C-MVT2024-CLI END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR OUVERTURE : ' SQLCODE
               MOVE 1 TO WS-FIN-CURSOR
           END-IF.

       5000-LIRE-MOUVEMENTS.
           EXEC SQL
               FETCH C-MVT2024-CLI
               INTO :WS-DATE-MVT, :WS-LIB-MOUV,
                    :WS-MONTANT-MVT, :WS-SENS, :WS-NATURE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-COMPTEUR
                   MOVE WS-MONTANT-MVT TO WS-MONTANT-ED
                   DISPLAY WS-DATE-MVT ' '
                           WS-LIB-MOUV ' '
                           WS-MONTANT-ED ' '
                           WS-SENS ' '
                           WS-NATURE
               WHEN 100
                   MOVE 1 TO WS-FIN-CURSOR
               WHEN OTHER
                   DISPLAY 'ERREUR FETCH : ' SQLCODE
                   MOVE 1 TO WS-FIN-CURSOR
           END-EVALUATE.

       6000-FERMER-CURSOR.
           EXEC SQL CLOSE C-MVT2024-CLI END-EXEC
           DISPLAY '================================================'
           DISPLAY 'TOTAL MOUVEMENTS 2024 : ' WS-COMPTEUR.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME RLV012'.

