       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09VALID.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * SOUS-PROGRAMME : Validation de donnees
      *
      * Exercice Chapitre IX - Programmes et Sous-Programmes
      *
      * DESCRIPTION :
      *   Valide une donnee selon le type specifie :
      *   - 'N' : Numerique (chiffres uniquement)
      *   - 'A' : Alphabetique (lettres et espaces)
      *
      * PARAMETRES :
      *   - LK-DONNEE        (IN)  : Donnee a valider
      *   - LK-TYPE          (IN)  : Type de validation (N/A)
      *   - LK-RESULTAT      (OUT) : V=Valide, I=Invalide
      *
      * APPELE PAR : C09APPEL (programme principal)
      *----------------------------------------------------------------

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I                PIC 99 VALUE 0.
       01  WS-LONGUEUR         PIC 99 VALUE 0.
       01  WS-CARACTERE        PIC X VALUE SPACE.
       01  WS-VALIDE           PIC X VALUE 'V'.
           88  EST-VALIDE      VALUE 'V'.
           88  EST-INVALIDE    VALUE 'I'.

       LINKAGE SECTION.
      * Parametres d'entree (IN)
       01  LK-DONNEE           PIC X(20).
       01  LK-TYPE             PIC X.
      * Parametre de sortie (OUT)
       01  LK-RESULTAT         PIC X.

       PROCEDURE DIVISION USING LK-DONNEE
                                LK-TYPE
                                LK-RESULTAT.

       0000-VALIDATION.
      * Initialisation
           MOVE 'V' TO WS-VALIDE
           MOVE 'V' TO LK-RESULTAT

      * Trouver la longueur effective (sans espaces de fin)
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LK-DONNEE))
               TO WS-LONGUEUR

      * Si donnee vide, invalide
           IF WS-LONGUEUR = 0
               MOVE 'I' TO LK-RESULTAT
               GOBACK
           END-IF

      * Validation selon le type
           EVALUATE LK-TYPE
               WHEN 'N'
                   PERFORM 1000-VALIDER-NUMERIQUE
               WHEN 'A'
                   PERFORM 2000-VALIDER-ALPHABETIQUE
               WHEN OTHER
                   MOVE 'I' TO WS-VALIDE
           END-EVALUATE

           MOVE WS-VALIDE TO LK-RESULTAT
           GOBACK.

      *----------------------------------------------------------------
      * 1000-VALIDER-NUMERIQUE : Verifie que tous les car. sont 0-9
      *----------------------------------------------------------------
       1000-VALIDER-NUMERIQUE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LONGUEUR
               MOVE LK-DONNEE(WS-I:1) TO WS-CARACTERE
               IF WS-CARACTERE NOT NUMERIC
                   MOVE 'I' TO WS-VALIDE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------
      * 2000-VALIDER-ALPHABETIQUE : Verifie lettres et espaces
      *----------------------------------------------------------------
       2000-VALIDER-ALPHABETIQUE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LONGUEUR
               MOVE LK-DONNEE(WS-I:1) TO WS-CARACTERE
               IF NOT (WS-CARACTERE ALPHABETIC)
                   MOVE 'I' TO WS-VALIDE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

