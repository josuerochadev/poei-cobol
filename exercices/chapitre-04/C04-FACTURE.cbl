       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG06CH04.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Calcul de facture avec COMPUTE
      *
      * Article1: Prix=3,75 EUR  Qte=100  Remise=5%
      * Article2: Prix=2,15 EUR  Qte=10   Remise=15%
      *
      * Specifications:
      * - Separateur decimal : virgule
      * - Valeurs jusqu'aux millions d'euros
      * - Edition: separateur milliers=espace, pas de zeros inutiles
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Constante TVA
      *----------------------------------------------------------------
       01  WS-TAUX-TVA     PIC 9(2)V99 VALUE 20,00.

      *----------------------------------------------------------------
      * Article 1 : Prix=3,75  Qte=100  Remise=5%
      *----------------------------------------------------------------
       01  WS-ART1.
           05  WS-ART1-PRIX     PIC 9(3)V99  VALUE 3,75.
           05  WS-ART1-QTE      PIC 9(3)     VALUE 100.
           05  WS-ART1-REMISE   PIC 9(2)V99  VALUE 5,00.
           05  WS-ART1-BRUT     PIC 9(7)V99.
           05  WS-ART1-REM-MT   PIC 9(7)V99.
           05  WS-ART1-NET      PIC 9(7)V99.

      *----------------------------------------------------------------
      * Article 2 : Prix=2,15  Qte=10  Remise=15%
      *----------------------------------------------------------------
       01  WS-ART2.
           05  WS-ART2-PRIX     PIC 9(3)V99  VALUE 2,15.
           05  WS-ART2-QTE      PIC 9(3)     VALUE 10.
           05  WS-ART2-REMISE   PIC 9(2)V99  VALUE 15,00.
           05  WS-ART2-BRUT     PIC 9(7)V99.
           05  WS-ART2-REM-MT   PIC 9(7)V99.
           05  WS-ART2-NET      PIC 9(7)V99.

      *----------------------------------------------------------------
      * Totaux facture
      *----------------------------------------------------------------
       01  WS-TOTAUX.
           05  WS-TOTAL-HT      PIC 9(8)V99.
           05  WS-TOTAL-TVA     PIC 9(8)V99.
           05  WS-TOTAL-TTC     PIC 9(8)V99.

      *----------------------------------------------------------------
      * Variables d'edition
      * Z = suppression zeros, . = separateur milliers (espace)
      * , = separateur decimal (virgule via DECIMAL-POINT IS COMMA)
      *----------------------------------------------------------------
       01  WS-EDIT-PRIX     PIC Z(3)9,99.
       01  WS-EDIT-QTE      PIC ZZ9.
       01  WS-EDIT-PCT      PIC Z9,99.
       01  WS-EDIT-MONTANT  PIC Z(4)ZZ9,99.
       01  WS-EDIT-TOTAL    PIC Z(5)ZZ9,99.

      *----------------------------------------------------------------
      * Ligne de separation
      *----------------------------------------------------------------
       01  WS-LIGNE-SEP     PIC X(50) VALUE ALL '-'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INIT
           PERFORM 2000-CALCUL-ART1
           PERFORM 3000-CALCUL-ART2
           PERFORM 4000-CALCUL-TOTAUX
           PERFORM 5000-AFFICHAGE
           STOP RUN.

      *----------------------------------------------------------------
      * Initialisation
      *----------------------------------------------------------------
       1000-INIT.
           INITIALIZE WS-TOTAUX.

      *----------------------------------------------------------------
      * Calcul Article 1
      * Brut = Prix x Qte
      * Remise = Brut x Taux / 100
      * Net = Brut - Remise
      *----------------------------------------------------------------
       2000-CALCUL-ART1.
           COMPUTE WS-ART1-BRUT =
               WS-ART1-PRIX * WS-ART1-QTE.

           COMPUTE WS-ART1-REM-MT =
               WS-ART1-BRUT * WS-ART1-REMISE / 100.

           COMPUTE WS-ART1-NET =
               WS-ART1-BRUT - WS-ART1-REM-MT.

      *----------------------------------------------------------------
      * Calcul Article 2
      *----------------------------------------------------------------
       3000-CALCUL-ART2.
           COMPUTE WS-ART2-BRUT =
               WS-ART2-PRIX * WS-ART2-QTE.

           COMPUTE WS-ART2-REM-MT =
               WS-ART2-BRUT * WS-ART2-REMISE / 100.

           COMPUTE WS-ART2-NET =
               WS-ART2-BRUT - WS-ART2-REM-MT.

      *----------------------------------------------------------------
      * Calcul des totaux facture
      * Total HT = Net Art1 + Net Art2
      * Total TVA = Total HT x Taux TVA / 100
      * Total TTC = Total HT + Total TVA
      *----------------------------------------------------------------
       4000-CALCUL-TOTAUX.
           COMPUTE WS-TOTAL-HT =
               WS-ART1-NET + WS-ART2-NET.

           COMPUTE WS-TOTAL-TVA =
               WS-TOTAL-HT * WS-TAUX-TVA / 100.

           COMPUTE WS-TOTAL-TTC =
               WS-TOTAL-HT + WS-TOTAL-TVA.

      *----------------------------------------------------------------
      * Affichage de la facture
      *----------------------------------------------------------------
       5000-AFFICHAGE.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '                   F A C T U R E                  '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    En-tete colonnes
           DISPLAY 'Article    Prix Unit.   Qte    Remise%     Net HT'
           DISPLAY WS-LIGNE-SEP

      *    Article 1
           MOVE WS-ART1-PRIX TO WS-EDIT-PRIX
           MOVE WS-ART1-QTE TO WS-EDIT-QTE
           MOVE WS-ART1-REMISE TO WS-EDIT-PCT
           MOVE WS-ART1-NET TO WS-EDIT-MONTANT
           DISPLAY 'Article 1  ' WS-EDIT-PRIX ' EUR  '
               WS-EDIT-QTE '    ' WS-EDIT-PCT '%   '
               WS-EDIT-MONTANT ' EUR'

      *    Article 2
           MOVE WS-ART2-PRIX TO WS-EDIT-PRIX
           MOVE WS-ART2-QTE TO WS-EDIT-QTE
           MOVE WS-ART2-REMISE TO WS-EDIT-PCT
           MOVE WS-ART2-NET TO WS-EDIT-MONTANT
           DISPLAY 'Article 2  ' WS-EDIT-PRIX ' EUR  '
               WS-EDIT-QTE '    ' WS-EDIT-PCT '%   '
               WS-EDIT-MONTANT ' EUR'

           DISPLAY WS-LIGNE-SEP
           DISPLAY ' '

      *    Totaux
           DISPLAY 'RECAPITULATIF :'
           DISPLAY WS-LIGNE-SEP

           MOVE WS-TOTAL-HT TO WS-EDIT-TOTAL
           DISPLAY 'Total Hors Taxe (HT)  : ' WS-EDIT-TOTAL ' EUR'

           MOVE WS-TAUX-TVA TO WS-EDIT-PCT
           MOVE WS-TOTAL-TVA TO WS-EDIT-TOTAL
           DISPLAY 'TVA (' WS-EDIT-PCT '%)          : '
               WS-EDIT-TOTAL ' EUR'

           MOVE WS-TOTAL-TTC TO WS-EDIT-TOTAL
           DISPLAY 'Total TTC             : ' WS-EDIT-TOTAL ' EUR'

           DISPLAY WS-LIGNE-SEP
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY ' '.
