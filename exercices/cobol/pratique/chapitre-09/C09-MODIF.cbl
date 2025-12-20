       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09MODIF.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * SOUS-PROGRAMME : Modification des parametres
      *
      * Exercice Chapitre IX - Demonstration BY REF vs BY CONTENT
      *
      * Ce sous-programme tente de modifier les deux parametres :
      *   - Le premier (BY REFERENCE) sera modifie dans l'appelant
      *   - Le second (BY CONTENT) ne sera PAS modifie dans l'appelant
      *
      * PARAMETRES :
      *   - LK-PARAM-REF  (IN/OUT) : Parametre BY REFERENCE
      *   - LK-PARAM-CONT (IN)     : Parametre BY CONTENT (copie)
      *----------------------------------------------------------------

       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-PARAM-REF        PIC 9(5).
       01  LK-PARAM-CONT       PIC 9(5).

       PROCEDURE DIVISION USING LK-PARAM-REF
                                LK-PARAM-CONT.

       0000-MODIFIER.
           DISPLAY ' '
           DISPLAY '  [DANS SOUS-PROGRAMME]'
           DISPLAY '  Valeurs recues :'
           DISPLAY '    LK-PARAM-REF  = ' LK-PARAM-REF
           DISPLAY '    LK-PARAM-CONT = ' LK-PARAM-CONT

           DISPLAY '  Modification des deux parametres a 999...'

      * Modifier les deux parametres
           MOVE 999 TO LK-PARAM-REF
           MOVE 999 TO LK-PARAM-CONT

           DISPLAY '  Valeurs apres modification :'
           DISPLAY '    LK-PARAM-REF  = ' LK-PARAM-REF
           DISPLAY '    LK-PARAM-CONT = ' LK-PARAM-CONT

           GOBACK.

