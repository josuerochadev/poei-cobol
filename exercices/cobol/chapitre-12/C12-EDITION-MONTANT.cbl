       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12EDMNT.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-EDITION-MONTANT
      * OBJET     : Demonstration des editions de montants
      *             - Separateurs decimaux et milliers
      *             - Suppression des zeros (Z)
      *             - Asterisques de protection (*)
      *             - Symboles monetaires fixes et flottants
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
       01  WS-MONTANT              PIC 9(8)V99.
       01  WS-PETIT-MONTANT        PIC 9(8)V99.

      *----------------------------------------------------------------*
      * Zones editees - Suppression zeros (Z)
      *----------------------------------------------------------------*
       01  WS-EDIT-Z1              PIC ZZZZZZZZ9,99.
       01  WS-EDIT-Z2              PIC ZZZ.ZZZ.ZZ9,99.
       01  WS-EDIT-Z3              PIC ZZZZZZZZ,ZZ.

      *----------------------------------------------------------------*
      * Zones editees - Asterisques (*)
      *----------------------------------------------------------------*
       01  WS-EDIT-AST1            PIC *******9,99.
       01  WS-EDIT-AST2            PIC ***.***.***,99.

      *----------------------------------------------------------------*
      * Zones editees - Symbole monetaire fixe
      *----------------------------------------------------------------*
       01  WS-EDIT-EUR-FIXE        PIC E99.999.999,99.

      *----------------------------------------------------------------*
      * Zones editees - Symbole monetaire flottant
      *----------------------------------------------------------------*
       01  WS-EDIT-EUR-FLOT        PIC EEEEEEEEE9,99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EDITION DE MONTANTS                             '
           DISPLAY '=================================================='
           DISPLAY ' '

           MOVE 12345.67 TO WS-MONTANT
           MOVE 12.34 TO WS-PETIT-MONTANT

      *    Suppression des zeros avec Z
           DISPLAY '--- Suppression des zeros (Z) ---'
           DISPLAY 'Valeur source : ' WS-MONTANT ' (12345,67)'
           DISPLAY ' '

           MOVE WS-MONTANT TO WS-EDIT-Z1
           DISPLAY 'PIC ZZZZZZZZ9,99   : "' WS-EDIT-Z1 '"'

           MOVE WS-MONTANT TO WS-EDIT-Z2
           DISPLAY 'PIC ZZZ.ZZZ.ZZ9,99 : "' WS-EDIT-Z2 '"'

           DISPLAY ' '
           DISPLAY 'Valeur source : ' WS-PETIT-MONTANT ' (12,34)'
           MOVE WS-PETIT-MONTANT TO WS-EDIT-Z1
           DISPLAY 'PIC ZZZZZZZZ9,99   : "' WS-EDIT-Z1 '"'

           MOVE WS-PETIT-MONTANT TO WS-EDIT-Z2
           DISPLAY 'PIC ZZZ.ZZZ.ZZ9,99 : "' WS-EDIT-Z2 '"'

      *    Cas special : valeur zero
           DISPLAY ' '
           DISPLAY 'Valeur source : 0,00'
           MOVE 0 TO WS-MONTANT
           MOVE WS-MONTANT TO WS-EDIT-Z3
           DISPLAY 'PIC ZZZZZZZZ,ZZ    : "' WS-EDIT-Z3
                   '" (tout en espaces)'

           DISPLAY ' '
           DISPLAY '--- Protection par asterisques (*) ---'
           MOVE 12345.67 TO WS-MONTANT
           DISPLAY 'Valeur source : 12345,67'
           DISPLAY ' '

           MOVE WS-MONTANT TO WS-EDIT-AST1
           DISPLAY 'PIC *******9,99     : "' WS-EDIT-AST1 '"'

           MOVE WS-MONTANT TO WS-EDIT-AST2
           DISPLAY 'PIC ***.***.***,99  : "' WS-EDIT-AST2 '"'

           DISPLAY ' '
           MOVE WS-PETIT-MONTANT TO WS-EDIT-AST1
           DISPLAY 'Valeur source : 12,34'
           DISPLAY 'PIC *******9,99     : "' WS-EDIT-AST1 '"'

           DISPLAY ' '
           DISPLAY '--- Symbole Euro fixe ---'
           MOVE 12345.67 TO WS-MONTANT
           DISPLAY 'Valeur source : 12345,67'
           MOVE WS-MONTANT TO WS-EDIT-EUR-FIXE
           DISPLAY 'PIC E99.999.999,99 : "' WS-EDIT-EUR-FIXE '"'
           DISPLAY '(E = symbole Euro en position fixe)'

           DISPLAY ' '
           DISPLAY '--- Symbole Euro flottant ---'
           MOVE 12345.67 TO WS-MONTANT
           DISPLAY 'Valeur source : 12345,67'
           MOVE WS-MONTANT TO WS-EDIT-EUR-FLOT
           DISPLAY 'PIC EEEEEEEEE9,99  : "' WS-EDIT-EUR-FLOT '"'

           MOVE WS-PETIT-MONTANT TO WS-EDIT-EUR-FLOT
           DISPLAY 'Valeur source : 12,34'
           DISPLAY 'PIC EEEEEEEEE9,99  : "' WS-EDIT-EUR-FLOT '"'
           DISPLAY '(E flottant se positionne devant les chiffres)'

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Fin du programme C12-EDITION-MONTANT'
           STOP RUN.

