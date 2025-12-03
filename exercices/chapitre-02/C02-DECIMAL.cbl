       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG02CH01.
       AUTHOR.     ROCHA
       .
      *---------------------------------------------------------
      * PROGRAMME D'AFFICHAGE:
      * - Separateur du nombre decimal (. ou ,)
      * - Nombres signes (+/-)
      * - Clause PICTURE pour numeriques
      *---------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------
      * Variables numeriques avec decimales
      * V = virgule decimale implicite (virtuelle)
      *---------------------------------------------------------
       01  NBDEC1        PIC 9(2)V9(2).
       01  NBDEC2        PIC 9(2)V9(2) VALUE ZEROES.
       01  NBDEC3        PIC 9(2)V9(2).

      *---------------------------------------------------------
      * Variables numeriques signees
      * S = signe (+/-)
      *---------------------------------------------------------
       01  NBR1          PIC S9(3).
       01  NBR2          PIC S9(3).

      *---------------------------------------------------------
      * Variables d'edition (pour affichage formate)
      * Z = zero supprime (remplace par espace)
      * + = affiche toujours le signe
      * - = affiche le signe seulement si negatif
      *---------------------------------------------------------
       01  NBDEC1-E      PIC Z9.9(2).
       01  NBR-E1        PIC +Z9(2).
       01  NBR-E2        PIC -Z9(2).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INIT.
           PERFORM 2000-TRAIT.
           PERFORM 9000-FIN.
           STOP RUN.

       1000-INIT.
           MOVE  9.42  TO NBDEC1.
           MOVE 25.0   TO NBDEC2.
           MOVE 9.000  TO NBDEC3.
           MOVE +71    TO NBR1.
           MOVE -362   TO NBR2.

       2000-TRAIT.
           DISPLAY '=== NOMBRES DECIMAUX ==='
           DISPLAY 'NBDEC1 (9.42)   : ' NBDEC1.
           DISPLAY 'NBDEC2 (25.0)   : ' NBDEC2.
           DISPLAY 'NBDEC3 (9.000)  : ' NBDEC3.

           DISPLAY ' '
           DISPLAY '=== NOMBRES SIGNES (brut) ==='
           DISPLAY 'NBR1 (+71)      : ' NBR1.
           DISPLAY 'NBR2 (-362)     : ' NBR2.

           DISPLAY ' '
           DISPLAY '=== NOMBRES SIGNES (edites) ==='
           MOVE NBR1 TO NBR-E1
           DISPLAY 'NBR1 avec +Z9(2): ' NBR-E1.
           MOVE NBR2 TO NBR-E1
           DISPLAY 'NBR2 avec +Z9(2): ' NBR-E1.

           MOVE NBR1 TO NBR-E2
           DISPLAY 'NBR1 avec -Z9(2): ' NBR-E2.
           MOVE NBR2 TO NBR-E2
           DISPLAY 'NBR2 avec -Z9(2): ' NBR-E2.

       9000-FIN.
           DISPLAY ' '
           DISPLAY '=== FIN DU PROGRAMME PG02CH01 ==='.
