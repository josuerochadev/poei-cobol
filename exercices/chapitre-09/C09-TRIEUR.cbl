       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09TRIEUR.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * SOUS-PROGRAMME : Tri de tableau (Bubble Sort)
      *
      * Exercice Chapitre IX - Programmes et Sous-Programmes
      *
      * DESCRIPTION :
      *   Trie un tableau de 10 elements en ordre croissant
      *   en utilisant l'algorithme du tri a bulles.
      *
      * PARAMETRES :
      *   - LK-TABLEAU     (IN/OUT) : Tableau de 10 elements
      *   - LK-CODE-RETOUR (OUT)    : 0=OK, 10=Erreur
      *
      * APPELE PAR : C09APPEL (programme principal)
      *----------------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I                PIC 99 VALUE 0.
       01  WS-J                PIC 99 VALUE 0.
       01  WS-TEMP             PIC 9(5) VALUE 0.
       01  WS-ECHANGE          PIC X VALUE 'N'.
           88  ECHANGE-FAIT    VALUE 'O'.
           88  PAS-ECHANGE     VALUE 'N'.
       01  WS-NB-ELEMENTS      PIC 99 VALUE 10.

       LINKAGE SECTION.
      * Parametre d'entree/sortie (IN/OUT)
       01  LK-TABLEAU.
           05  LK-ELEMENT      PIC 9(5) OCCURS 10 TIMES.
      * Code retour (OUT)
       01  LK-CODE-RETOUR      PIC 99.

       PROCEDURE DIVISION USING LK-TABLEAU
                                LK-CODE-RETOUR.

       0000-TRI-TABLEAU.
      * Initialisation
           MOVE 0 TO LK-CODE-RETOUR
           MOVE 'O' TO WS-ECHANGE

      *----------------------------------------------------------------
      * Algorithme Bubble Sort (tri a bulles)
      *
      * Principe : Parcourir le tableau plusieurs fois en comparant
      * les elements adjacents et en les echangeant si necessaire.
      * Continuer jusqu'a ce qu'aucun echange ne soit fait.
      *----------------------------------------------------------------
           PERFORM UNTIL PAS-ECHANGE
               MOVE 'N' TO WS-ECHANGE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I >= WS-NB-ELEMENTS
                   COMPUTE WS-J = WS-I + 1
                   IF LK-ELEMENT(WS-I) > LK-ELEMENT(WS-J)
      * Echange des deux elements
                       MOVE LK-ELEMENT(WS-I) TO WS-TEMP
                       MOVE LK-ELEMENT(WS-J) TO LK-ELEMENT(WS-I)
                       MOVE WS-TEMP TO LK-ELEMENT(WS-J)
                       MOVE 'O' TO WS-ECHANGE
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK.

