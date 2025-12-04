       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSLS.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-LIST
      * OBJET     : Lecture sequentielle du fichier KSDS PERSONNEL
      *             Edition de chaque enregistrement avec DISPLAY
      * EXERCICE  : TP Chapitre X - Exercice 1
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PERSONNEL
               ASSIGN TO 'PERSONNEL.KSDS'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ENR-MATRICULE
               ALTERNATE RECORD KEY IS ENR-NUM-SS WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD F-PERSONNEL
           RECORDING MODE IS F
           RECORD CONTAINS 66 CHARACTERS.
       01 ENR-PERSONNEL.
          05 ENR-MATRICULE         PIC 9(6).
          05 ENR-NOM               PIC X(15).
          05 ENR-PRENOM            PIC X(15).
          05 ENR-SALAIRE           PIC 9(6).
          05 ENR-PRIMES            PIC 9(6).
          05 ENR-REVENU-ANNUEL     PIC 9(8).
          05 ENR-NUM-SS            PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX.
          88 WS-OK                 VALUE '00'.
          88 WS-EOF                VALUE '10'.

       01 WS-FIN-FICHIER           PIC X VALUE 'N'.
          88 FIN-FICHIER           VALUE 'O'.

       01 WS-COMPTEUR              PIC 9(3) VALUE 0.

      *----------------------------------------------------------------*
      * Variables d'edition
      *----------------------------------------------------------------*
       01 WS-ED-SALAIRE            PIC ZZ.ZZ9.
       01 WS-ED-PRIMES             PIC ZZ.ZZ9.
       01 WS-ED-REVENU             PIC ZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 1 : LECTURE SEQUENTIELLE PERSONNEL     '
           DISPLAY '  Edition de chaque enregistrement avec DISPLAY   '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-LIRE-ET-AFFICHER
           PERFORM 3000-FERMER-FICHIER

           STOP RUN.

      *----------------------------------------------------------------*
       1000-OUVRIR-FICHIER.
           OPEN INPUT F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           DISPLAY 'MATRIC  NOM             PRENOM          '
                   'SALAIRE   PRIMES    REVENU      NUM-SS'
           DISPLAY '------  --------------- --------------- '
                   '--------  --------  ----------  ----------'.

      *----------------------------------------------------------------*
       2000-LIRE-ET-AFFICHER.
           PERFORM UNTIL FIN-FICHIER
               READ F-PERSONNEL
                   AT END
                       SET FIN-FICHIER TO TRUE
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       PERFORM 2100-AFFICHER-ENREGISTREMENT
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
       2100-AFFICHER-ENREGISTREMENT.
           MOVE ENR-SALAIRE TO WS-ED-SALAIRE
           MOVE ENR-PRIMES TO WS-ED-PRIMES
           MOVE ENR-REVENU-ANNUEL TO WS-ED-REVENU

           DISPLAY ENR-MATRICULE '  '
                   ENR-NOM ' '
                   ENR-PRENOM ' '
                   WS-ED-SALAIRE '  '
                   WS-ED-PRIMES '  '
                   WS-ED-REVENU '  '
                   ENR-NUM-SS.

      *----------------------------------------------------------------*
       3000-FERMER-FICHIER.
           CLOSE F-PERSONNEL
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Total enregistrements lus : ' WS-COMPTEUR
           DISPLAY '=================================================='.

