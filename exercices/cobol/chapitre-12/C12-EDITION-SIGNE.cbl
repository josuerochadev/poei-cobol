       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12EDSIGN.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-EDITION-SIGNE
      * OBJET     : Demonstration des editions de signes
      *             - Signe fixe (+ ou - en debut/fin)
      *             - Signe flottant (+++, ---)
      *             - Credit/Debit (CR, DB)
      * EXERCICE  : Chapitre XII - Fichier d'impression
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Donnees sources
      *----------------------------------------------------------------*
       01  WS-MONTANT-POS          PIC S9(6)V99.
       01  WS-MONTANT-NEG          PIC S9(6)V99.

      *----------------------------------------------------------------*
      * Zones editees - Signe fixe en debut
      *----------------------------------------------------------------*
       01  WS-EDIT-PLUS-DEB        PIC +ZZZ.ZZ9,99.
       01  WS-EDIT-MOINS-DEB       PIC -ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
      * Zones editees - Signe fixe en fin
      *----------------------------------------------------------------*
       01  WS-EDIT-PLUS-FIN        PIC ZZZ.ZZ9,99+.
       01  WS-EDIT-MOINS-FIN       PIC ZZZ.ZZ9,99-.

      *----------------------------------------------------------------*
      * Zones editees - Signe flottant
      *----------------------------------------------------------------*
       01  WS-EDIT-PLUS-FLOT       PIC +++++++9,99.
       01  WS-EDIT-MOINS-FLOT      PIC -------9,99.

      *----------------------------------------------------------------*
      * Zones editees - CR et DB
      *----------------------------------------------------------------*
       01  WS-EDIT-CR              PIC ZZZ.ZZ9,99CR.
       01  WS-EDIT-DB              PIC ZZZ.ZZ9,99DB.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EDITION DE SIGNES (+, -, CR, DB)                '
           DISPLAY '=================================================='
           DISPLAY ' '

           MOVE +12345.67 TO WS-MONTANT-POS
           MOVE -12345.67 TO WS-MONTANT-NEG

      *    Signe fixe en debut
           DISPLAY '--- Signe fixe en debut de zone ---'
           DISPLAY 'Valeur positive : +12345,67'
           DISPLAY 'Valeur negative : -12345,67'
           DISPLAY ' '

           MOVE WS-MONTANT-POS TO WS-EDIT-PLUS-DEB
           DISPLAY 'PIC +ZZZ.ZZ9,99 (positif) : "' WS-EDIT-PLUS-DEB '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-PLUS-DEB
           DISPLAY 'PIC +ZZZ.ZZ9,99 (negatif) : "' WS-EDIT-PLUS-DEB '"'

           DISPLAY ' '
           MOVE WS-MONTANT-POS TO WS-EDIT-MOINS-DEB
           DISPLAY 'PIC -ZZZ.ZZ9,99 (positif) : "' WS-EDIT-MOINS-DEB '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-MOINS-DEB
           DISPLAY 'PIC -ZZZ.ZZ9,99 (negatif) : "' WS-EDIT-MOINS-DEB '"'
           DISPLAY '(- fixe : affiche - si negatif, espace si positif)'

      *    Signe fixe en fin
           DISPLAY ' '
           DISPLAY '--- Signe fixe en fin de zone ---'
           MOVE WS-MONTANT-POS TO WS-EDIT-PLUS-FIN
           DISPLAY 'PIC ZZZ.ZZ9,99+ (positif) : "' WS-EDIT-PLUS-FIN '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-PLUS-FIN
           DISPLAY 'PIC ZZZ.ZZ9,99+ (negatif) : "' WS-EDIT-PLUS-FIN '"'

           DISPLAY ' '
           MOVE WS-MONTANT-POS TO WS-EDIT-MOINS-FIN
           DISPLAY 'PIC ZZZ.ZZ9,99- (positif) : "' WS-EDIT-MOINS-FIN '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-MOINS-FIN
           DISPLAY 'PIC ZZZ.ZZ9,99- (negatif) : "' WS-EDIT-MOINS-FIN '"'

      *    Signe flottant
           DISPLAY ' '
           DISPLAY '--- Signe flottant ---'
           MOVE WS-MONTANT-POS TO WS-EDIT-PLUS-FLOT
           DISPLAY 'PIC +++++++9,99 (positif) : "' WS-EDIT-PLUS-FLOT '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-PLUS-FLOT
           DISPLAY 'PIC +++++++9,99 (negatif) : "' WS-EDIT-PLUS-FLOT '"'

           DISPLAY ' '
           MOVE WS-MONTANT-POS TO WS-EDIT-MOINS-FLOT
           DISPLAY 'PIC -------9,99 (positif) : "' WS-EDIT-MOINS-FLOT '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-MOINS-FLOT
           DISPLAY 'PIC -------9,99 (negatif) : "' WS-EDIT-MOINS-FLOT '"'
           DISPLAY '(Signe flottant se positionne devant les chiffres)'

      *    CR et DB
           DISPLAY ' '
           DISPLAY '--- Symboles CR (Credit) et DB (Debit) ---'
           MOVE WS-MONTANT-POS TO WS-EDIT-CR
           DISPLAY 'PIC ZZZ.ZZ9,99CR (positif) : "' WS-EDIT-CR '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-CR
           DISPLAY 'PIC ZZZ.ZZ9,99CR (negatif) : "' WS-EDIT-CR '"'

           DISPLAY ' '
           MOVE WS-MONTANT-POS TO WS-EDIT-DB
           DISPLAY 'PIC ZZZ.ZZ9,99DB (positif) : "' WS-EDIT-DB '"'
           MOVE WS-MONTANT-NEG TO WS-EDIT-DB
           DISPLAY 'PIC ZZZ.ZZ9,99DB (negatif) : "' WS-EDIT-DB '"'
           DISPLAY '(CR/DB affiches uniquement si valeur negative)'

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Fin du programme C12-EDITION-SIGNE'
           STOP RUN.

