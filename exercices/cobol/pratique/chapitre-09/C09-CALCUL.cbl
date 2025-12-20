       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09CALCUL.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * SOUS-PROGRAMME : Calcul TVA et TTC
      *
      * Exercice Chapitre IX - Programmes et Sous-Programmes
      *
      * DESCRIPTION :
      *   Calcule le montant de la TVA et le montant TTC
      *   a partir du montant HT et du taux de TVA.
      *
      * PARAMETRES :
      *   - LK-MONTANT-HT  (IN)  : Montant hors taxes
      *   - LK-TAUX-TVA    (IN)  : Taux de TVA (ex: 20.00 pour 20%)
      *   - LK-MONTANT-TVA (OUT) : Montant de la TVA calculee
      *   - LK-MONTANT-TTC (OUT) : Montant TTC
      *   - LK-CODE-RETOUR (OUT) : 0=OK, 10=Erreur parametre
      *
      * APPELE PAR : C09APPEL (programme principal)
      *----------------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEMP-CALCUL      PIC 9(9)V9(4) VALUE 0.

       LINKAGE SECTION.
      * Parametres d'entree (IN)
       01  LK-MONTANT-HT       PIC 9(7)V99.
       01  LK-TAUX-TVA         PIC 9(2)V99.
      * Parametres de sortie (OUT)
       01  LK-MONTANT-TVA      PIC 9(7)V99.
       01  LK-MONTANT-TTC      PIC 9(7)V99.
      * Code retour
       01  LK-CODE-RETOUR      PIC 99.

       PROCEDURE DIVISION USING LK-MONTANT-HT
                                LK-TAUX-TVA
                                LK-MONTANT-TVA
                                LK-MONTANT-TTC
                                LK-CODE-RETOUR.

       0000-CALCUL-TVA.
      * Initialisation des sorties
           MOVE 0 TO LK-CODE-RETOUR
           MOVE 0 TO LK-MONTANT-TVA
           MOVE 0 TO LK-MONTANT-TTC

      * Validation des entrees
           IF LK-MONTANT-HT < 0
               MOVE 10 TO LK-CODE-RETOUR
               GOBACK
           END-IF

           IF LK-TAUX-TVA < 0 OR LK-TAUX-TVA > 100
               MOVE 10 TO LK-CODE-RETOUR
               GOBACK
           END-IF

      * Calcul de la TVA : HT * (Taux / 100)
           COMPUTE WS-TEMP-CALCUL =
               LK-MONTANT-HT * (LK-TAUX-TVA / 100)
           END-COMPUTE

           MOVE WS-TEMP-CALCUL TO LK-MONTANT-TVA

      * Calcul du TTC : HT + TVA
           COMPUTE LK-MONTANT-TTC = LK-MONTANT-HT + LK-MONTANT-TVA
           END-COMPUTE

           GOBACK.

