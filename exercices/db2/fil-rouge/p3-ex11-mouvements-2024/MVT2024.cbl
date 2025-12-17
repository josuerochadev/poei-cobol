       IDENTIFICATION DIVISION.
       PROGRAM-ID. MVT2024.
      *---------------------------------------------------------
      * P3 EXERCICE 11 : Mouvements de l'annee 2024
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
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

      * Curseur pour mouvements 2024
           EXEC SQL
               DECLARE C-MVT2024 CURSOR FOR
               SELECT M.NUM_COMPTE, C.NOM_CLIENT,
                      M.DATE_MVT, M.LIB_MOUV,
                      M.MONTANT_MVT, M.SENS, M.NATURE
               FROM MOUVEMENT M
               INNER JOIN CLIENT C ON M.NUM_COMPTE = C.NUM_COMPTE
               WHERE YEAR(M.DATE_MVT) = 2024
               ORDER BY M.DATE_MVT, M.NUM_COMPTE
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-AFFICHER-ENTETE
           PERFORM 2000-OUVRIR-CURSOR
           PERFORM 3000-LIRE-MOUVEMENTS
               UNTIL WS-FIN-CURSOR = 1
           PERFORM 4000-FERMER-CURSOR
           PERFORM 9000-FIN
           STOP RUN.

       1000-AFFICHER-ENTETE.
           DISPLAY '================================================'
           DISPLAY '      MOUVEMENTS DE L ANNEE 2024'
           DISPLAY '================================================'
           DISPLAY 'NUM  CLIENT     DATE       LIBELLE        '
                   ' MONTANT   SENS NAT'.

       2000-OUVRIR-CURSOR.
           EXEC SQL OPEN C-MVT2024 END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERREUR OUVERTURE : ' SQLCODE
               MOVE 1 TO WS-FIN-CURSOR
           END-IF.

       3000-LIRE-MOUVEMENTS.
           EXEC SQL
               FETCH C-MVT2024
               INTO :WS-NUM-COMPTE, :WS-NOM-CLIENT,
                    :WS-DATE-MVT, :WS-LIB-MOUV,
                    :WS-MONTANT-MVT, :WS-SENS, :WS-NATURE
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-COMPTEUR
                   MOVE WS-MONTANT-MVT TO WS-MONTANT-ED
                   DISPLAY WS-NUM-COMPTE ' '
                           WS-NOM-CLIENT ' '
                           WS-DATE-MVT ' '
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

       4000-FERMER-CURSOR.
           EXEC SQL CLOSE C-MVT2024 END-EXEC
           DISPLAY '================================================'
           DISPLAY 'TOTAL MOUVEMENTS 2024 : ' WS-COMPTEUR.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME MVT2024'.
