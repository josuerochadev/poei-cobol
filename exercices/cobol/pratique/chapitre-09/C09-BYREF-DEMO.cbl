       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09BYREF.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Demonstration BY REFERENCE vs BY CONTENT
      *
      * Exercice Chapitre IX - Programmes et Sous-Programmes
      *
      * Ce programme montre la difference entre :
      *   - BY REFERENCE : l'appele peut modifier la variable
      *   - BY CONTENT   : l'appele recoit une copie (non modifiable)
      *----------------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VALEUR-REF       PIC 9(5) VALUE 100.
       01  WS-VALEUR-CONT      PIC 9(5) VALUE 200.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Demo BY REFERENCE vs BY CONTENT'
           DISPLAY '=================================================='
           DISPLAY ' '

      * Afficher les valeurs initiales
           DISPLAY 'AVANT APPEL :'
           DISPLAY '  WS-VALEUR-REF  (BY REFERENCE) = ' WS-VALEUR-REF
           DISPLAY '  WS-VALEUR-CONT (BY CONTENT)   = ' WS-VALEUR-CONT

      * Appeler le sous-programme qui va tenter de modifier les deux
           CALL 'C09MODIF' USING BY REFERENCE WS-VALEUR-REF
                                 BY CONTENT   WS-VALEUR-CONT
           END-CALL

      * Afficher les valeurs apres l'appel
           DISPLAY ' '
           DISPLAY 'APRES APPEL :'
           DISPLAY '  WS-VALEUR-REF  (BY REFERENCE) = ' WS-VALEUR-REF
           DISPLAY '  WS-VALEUR-CONT (BY CONTENT)   = ' WS-VALEUR-CONT

           DISPLAY ' '
           DISPLAY 'CONCLUSION :'
           DISPLAY '  - BY REFERENCE : modification visible (100->999)'
           DISPLAY '  - BY CONTENT   : modification NON visible (200)'
           DISPLAY '=================================================='

           STOP RUN.

