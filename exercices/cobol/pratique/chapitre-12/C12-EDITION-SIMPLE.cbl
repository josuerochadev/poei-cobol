       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12EDSIMP.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-EDITION-SIMPLE
      * OBJET     : Demonstration des editions par insertion simple
      *             - Insertion de blancs (B)
      *             - Insertion de barres (/)
      *             - Insertion de zeros (0)
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
       01  WS-DATE-BRUTE           PIC 9(8).
       01  WS-TEL-BRUT             PIC 9(10).
       01  WS-SIRET-BRUT           PIC 9(14).
       01  WS-NUMERO-BRUT          PIC 9(9).
       01  WS-HEURE-BRUTE          PIC 9(6).

      *----------------------------------------------------------------*
      * Zones editees
      *----------------------------------------------------------------*
       01  WS-DATE-EDIT            PIC 99/99/9999.
       01  WS-TEL-EDIT             PIC 99B99B99B99B99.
       01  WS-SIRET-EDIT           PIC 999B999B999B99999.
       01  WS-NUMERO-EDIT          PIC 999B999B999.
       01  WS-HEURE-EDIT           PIC 99/99/99.
       01  WS-CODE-EDIT            PIC 99009900.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EDITION PAR INSERTION SIMPLE (B, /, 0)          '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Insertion de barres (/) - Format date
           DISPLAY '--- Insertion de barres (/) ---'
           MOVE 15122023 TO WS-DATE-BRUTE
           MOVE WS-DATE-BRUTE TO WS-DATE-EDIT
           DISPLAY 'Date brute  : ' WS-DATE-BRUTE
           DISPLAY 'PIC 99/99/9999'
           DISPLAY 'Date editee : ' WS-DATE-EDIT
           DISPLAY ' '

      *    Insertion de barres (/) - Format heure
           DISPLAY '--- Insertion de barres (/) - Heure ---'
           MOVE 143052 TO WS-HEURE-BRUTE
           MOVE WS-HEURE-BRUTE TO WS-HEURE-EDIT
           DISPLAY 'Heure brute  : ' WS-HEURE-BRUTE
           DISPLAY 'PIC 99/99/99'
           DISPLAY 'Heure editee : ' WS-HEURE-EDIT ' (14h30m52s)'
           DISPLAY ' '

      *    Insertion de blancs (B) - Telephone
           DISPLAY '--- Insertion de blancs (B) - Telephone ---'
           MOVE 0612345678 TO WS-TEL-BRUT
           MOVE WS-TEL-BRUT TO WS-TEL-EDIT
           DISPLAY 'Tel brut  : ' WS-TEL-BRUT
           DISPLAY 'PIC 99B99B99B99B99'
           DISPLAY 'Tel edite : ' WS-TEL-EDIT
           DISPLAY ' '

      *    Insertion de blancs (B) - SIRET
           DISPLAY '--- Insertion de blancs (B) - SIRET ---'
           MOVE 12345678901234 TO WS-SIRET-BRUT
           MOVE WS-SIRET-BRUT TO WS-SIRET-EDIT
           DISPLAY 'SIRET brut  : ' WS-SIRET-BRUT
           DISPLAY 'PIC 999B999B999B99999'
           DISPLAY 'SIRET edite : ' WS-SIRET-EDIT
           DISPLAY ' '

      *    Insertion de blancs (B) - Numero
           DISPLAY '--- Insertion de blancs (B) - Numero ---'
           MOVE 123456789 TO WS-NUMERO-BRUT
           MOVE WS-NUMERO-BRUT TO WS-NUMERO-EDIT
           DISPLAY 'Numero brut  : ' WS-NUMERO-BRUT
           DISPLAY 'PIC 999B999B999'
           DISPLAY 'Numero edite : ' WS-NUMERO-EDIT
           DISPLAY ' '

      *    Insertion de zeros (0)
           DISPLAY '--- Insertion de zeros (0) ---'
           MOVE 1234 TO WS-NUMERO-BRUT
           MOVE WS-NUMERO-BRUT TO WS-CODE-EDIT
           DISPLAY 'Code brut  : ' WS-NUMERO-BRUT
           DISPLAY 'PIC 99009900'
           DISPLAY 'Code edite : ' WS-CODE-EDIT
           DISPLAY ' '

           DISPLAY '=================================================='
           DISPLAY 'Fin du programme C12-EDITION-SIMPLE'
           STOP RUN.

