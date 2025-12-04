       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09APPEL.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Programme APPELANT - Demonstration CALL
      *
      * Exercice Chapitre IX - Programmes et Sous-Programmes
      *
      * Ce programme principal appelle plusieurs sous-programmes :
      *   - C09CALCUL : Calcul TVA et TTC
      *   - C09VALID  : Validation de donnees
      *   - C09TRIEUR : Tri d'un tableau
      *----------------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Variables pour le calcul TVA
      *----------------------------------------------------------------
       01  WS-MONTANT-HT       PIC 9(7)V99 VALUE 0.
       01  WS-TAUX-TVA         PIC 9(2)V99 VALUE 20.00.
       01  WS-MONTANT-TVA      PIC 9(7)V99 VALUE 0.
       01  WS-MONTANT-TTC      PIC 9(7)V99 VALUE 0.

      *----------------------------------------------------------------
      * Variables pour la validation
      *----------------------------------------------------------------
       01  WS-DONNEE-A-VALIDER PIC X(20) VALUE SPACES.
       01  WS-TYPE-VALIDATION  PIC X VALUE SPACES.
           88  VALIDER-NUMERIQUE VALUE 'N'.
           88  VALIDER-ALPHA     VALUE 'A'.
       01  WS-RESULTAT-VALID   PIC X VALUE SPACES.
           88  DONNEE-VALIDE     VALUE 'V'.
           88  DONNEE-INVALIDE   VALUE 'I'.

      *----------------------------------------------------------------
      * Variables pour le tri
      *----------------------------------------------------------------
       01  WS-TABLEAU.
           05  WS-ELEMENT      PIC 9(5) OCCURS 10 TIMES.
       01  WS-I                PIC 99 VALUE 0.

      *----------------------------------------------------------------
      * Code retour des sous-programmes
      *----------------------------------------------------------------
       01  WS-CODE-RETOUR      PIC 99 VALUE 0.
           88  TRAITEMENT-OK   VALUE 0.
           88  ERREUR-PARAM    VALUE 10.
           88  ERREUR-CALCUL   VALUE 20.

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-ED-MONTANT       PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Chapitre IX - Programmes et Sous-Programmes'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-TEST-CALCUL-TVA
           PERFORM 2000-TEST-VALIDATION
           PERFORM 3000-TEST-TRI-TABLEAU

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Fin des tests'
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * 1000-TEST-CALCUL-TVA : Appel du sous-programme de calcul
      *----------------------------------------------------------------
       1000-TEST-CALCUL-TVA.
           DISPLAY ' '
           DISPLAY '--- Test 1 : Calcul TVA ---'

           MOVE 1500.00 TO WS-MONTANT-HT
           MOVE 20.00 TO WS-TAUX-TVA

           DISPLAY 'Montant HT  : ' WS-MONTANT-HT
           DISPLAY 'Taux TVA    : ' WS-TAUX-TVA '%'

           CALL 'C09CALCUL' USING BY CONTENT   WS-MONTANT-HT
                                  BY CONTENT   WS-TAUX-TVA
                                  BY REFERENCE WS-MONTANT-TVA
                                  BY REFERENCE WS-MONTANT-TTC
                                  BY REFERENCE WS-CODE-RETOUR
           END-CALL

           IF TRAITEMENT-OK
               MOVE WS-MONTANT-TVA TO WS-ED-MONTANT
               DISPLAY 'Montant TVA : ' WS-ED-MONTANT ' EUR'
               MOVE WS-MONTANT-TTC TO WS-ED-MONTANT
               DISPLAY 'Montant TTC : ' WS-ED-MONTANT ' EUR'
           ELSE
               DISPLAY 'ERREUR calcul TVA, code : ' WS-CODE-RETOUR
           END-IF.

      *----------------------------------------------------------------
      * 2000-TEST-VALIDATION : Appel du sous-programme de validation
      *----------------------------------------------------------------
       2000-TEST-VALIDATION.
           DISPLAY ' '
           DISPLAY '--- Test 2 : Validation ---'

      * Test 1 : Valeur numerique valide
           MOVE '12345' TO WS-DONNEE-A-VALIDER
           MOVE 'N' TO WS-TYPE-VALIDATION
           DISPLAY 'Donnee : "' WS-DONNEE-A-VALIDER
               '" Type: Numerique'

           CALL 'C09VALID' USING BY CONTENT   WS-DONNEE-A-VALIDER
                                 BY CONTENT   WS-TYPE-VALIDATION
                                 BY REFERENCE WS-RESULTAT-VALID
           END-CALL

           IF DONNEE-VALIDE
               DISPLAY '  -> Resultat : VALIDE'
           ELSE
               DISPLAY '  -> Resultat : INVALIDE'
           END-IF

      * Test 2 : Valeur alphabetique
           MOVE 'DUPONT' TO WS-DONNEE-A-VALIDER
           MOVE 'A' TO WS-TYPE-VALIDATION
           DISPLAY 'Donnee : "' WS-DONNEE-A-VALIDER
               '" Type: Alphabetique'

           CALL 'C09VALID' USING BY CONTENT   WS-DONNEE-A-VALIDER
                                 BY CONTENT   WS-TYPE-VALIDATION
                                 BY REFERENCE WS-RESULTAT-VALID
           END-CALL

           IF DONNEE-VALIDE
               DISPLAY '  -> Resultat : VALIDE'
           ELSE
               DISPLAY '  -> Resultat : INVALIDE'
           END-IF

      * Test 3 : Valeur numerique invalide (contient lettres)
           MOVE '123AB' TO WS-DONNEE-A-VALIDER
           MOVE 'N' TO WS-TYPE-VALIDATION
           DISPLAY 'Donnee : "' WS-DONNEE-A-VALIDER
               '" Type: Numerique'

           CALL 'C09VALID' USING BY CONTENT   WS-DONNEE-A-VALIDER
                                 BY CONTENT   WS-TYPE-VALIDATION
                                 BY REFERENCE WS-RESULTAT-VALID
           END-CALL

           IF DONNEE-VALIDE
               DISPLAY '  -> Resultat : VALIDE'
           ELSE
               DISPLAY '  -> Resultat : INVALIDE'
           END-IF.

      *----------------------------------------------------------------
      * 3000-TEST-TRI-TABLEAU : Appel du sous-programme de tri
      *----------------------------------------------------------------
       3000-TEST-TRI-TABLEAU.
           DISPLAY ' '
           DISPLAY '--- Test 3 : Tri de tableau ---'

      * Initialiser le tableau avec des valeurs non triees
           MOVE 45 TO WS-ELEMENT(1)
           MOVE 12 TO WS-ELEMENT(2)
           MOVE 78 TO WS-ELEMENT(3)
           MOVE 23 TO WS-ELEMENT(4)
           MOVE 56 TO WS-ELEMENT(5)
           MOVE 89 TO WS-ELEMENT(6)
           MOVE 34 TO WS-ELEMENT(7)
           MOVE 67 TO WS-ELEMENT(8)
           MOVE 1 TO WS-ELEMENT(9)
           MOVE 90 TO WS-ELEMENT(10)

           DISPLAY 'Tableau avant tri :'
           DISPLAY '  ' WS-ELEMENT(1) ' ' WS-ELEMENT(2) ' '
               WS-ELEMENT(3) ' ' WS-ELEMENT(4) ' ' WS-ELEMENT(5)
           DISPLAY '  ' WS-ELEMENT(6) ' ' WS-ELEMENT(7) ' '
               WS-ELEMENT(8) ' ' WS-ELEMENT(9) ' ' WS-ELEMENT(10)

           CALL 'C09TRIEUR' USING BY REFERENCE WS-TABLEAU
                                  BY REFERENCE WS-CODE-RETOUR
           END-CALL

           IF TRAITEMENT-OK
               DISPLAY 'Tableau apres tri :'
               DISPLAY '  ' WS-ELEMENT(1) ' ' WS-ELEMENT(2) ' '
                   WS-ELEMENT(3) ' ' WS-ELEMENT(4) ' ' WS-ELEMENT(5)
               DISPLAY '  ' WS-ELEMENT(6) ' ' WS-ELEMENT(7) ' '
                   WS-ELEMENT(8) ' ' WS-ELEMENT(9) ' ' WS-ELEMENT(10)
           ELSE
               DISPLAY 'ERREUR tri, code : ' WS-CODE-RETOUR
           END-IF.

