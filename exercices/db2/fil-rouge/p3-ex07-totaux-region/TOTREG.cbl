       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTREG.
      *---------------------------------------------------------
      * P3 EXERCICE 7 : Totaux par region avec niveau 88
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable avec niveau 88 pour les regions
       01 WS-CODE-REGION    PIC X(02).
           88 REGION-PARIS      VALUE '01'.
           88 REGION-MARSEILLE  VALUE '02'.
           88 REGION-LYON       VALUE '03'.
           88 REGION-LILLE      VALUE '04'.

      * Variables host pour DB2
       01 WS-NOM-REGION     PIC X(15).
       01 WS-TOTAL-DB       PIC S9(10)V99 COMP-3.
       01 WS-TOTAL-CR       PIC S9(10)V99 COMP-3.

      * Variables d'edition
       01 WS-TOTAL-ED       PIC -ZZZ,ZZZ,ZZ9.99.

      * Compteur de boucle
       01 WS-IDX            PIC 9(01).

      * SQLCA pour gestion erreurs DB2
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY '=========================================='
           DISPLAY 'TOTAUX PAR REGION (DEBITEURS/CREDITEURS)'
           DISPLAY '=========================================='
           PERFORM 1000-TRAITER-REGIONS
           PERFORM 9000-FIN
           STOP RUN.

       1000-TRAITER-REGIONS.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               EVALUATE WS-IDX
                   WHEN 1 SET REGION-PARIS TO TRUE
                   WHEN 2 SET REGION-MARSEILLE TO TRUE
                   WHEN 3 SET REGION-LYON TO TRUE
                   WHEN 4 SET REGION-LILLE TO TRUE
               END-EVALUATE
               PERFORM 2000-CALCULER-TOTAUX
               PERFORM 3000-AFFICHER-REGION
           END-PERFORM.

       2000-CALCULER-TOTAUX.
      * Recuperer nom region et totaux
           EXEC SQL
               SELECT R.NOM_REGION,
                      COALESCE(SUM(CASE WHEN C.POS = 'DB'
                                   THEN C.SOLDE ELSE 0 END), 0),
                      COALESCE(SUM(CASE WHEN C.POS = 'CR'
                                   THEN C.SOLDE ELSE 0 END), 0)
               INTO :WS-NOM-REGION, :WS-TOTAL-DB, :WS-TOTAL-CR
               FROM REGION R
               LEFT JOIN CLIENT C ON R.CODE_REGION = C.CODE_REGION
               WHERE R.CODE_REGION = :WS-CODE-REGION
               GROUP BY R.NOM_REGION
           END-EXEC

           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               DISPLAY 'ERREUR SQL REGION ' WS-CODE-REGION
                       ' : ' SQLCODE
           END-IF.

       3000-AFFICHER-REGION.
           DISPLAY ' '
           DISPLAY '--- REGION : ' WS-NOM-REGION ' ---'
           MOVE WS-TOTAL-DB TO WS-TOTAL-ED
           DISPLAY 'TOTAL DEBITEURS  : ' WS-TOTAL-ED
           MOVE WS-TOTAL-CR TO WS-TOTAL-ED
           DISPLAY 'TOTAL CREDITEURS : ' WS-TOTAL-ED.

       9000-FIN.
           DISPLAY ' '
           DISPLAY 'FIN DU PROGRAMME TOTREG'.
