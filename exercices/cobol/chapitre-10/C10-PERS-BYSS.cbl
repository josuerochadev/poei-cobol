       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSSS.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-BYSS
      * OBJET     : Lecture par cle secondaire (Numero S-Social)
      *             Recherche et edition d'un salarie par son N° SS
      * EXERCICE  : TP Chapitre X - Exercices 2 et 3
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
               ACCESS MODE IS DYNAMIC
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
          88 WS-NOT-FOUND          VALUE '23'.

       01 WS-NUM-SS-RECHERCHE      PIC 9(10).
       01 WS-CONTINUER             PIC X VALUE 'O'.
          88 CONTINUER             VALUE 'O' 'o'.

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
           DISPLAY '  EXERCICES 2-3 : LECTURE PAR CLE SECONDAIRE      '
           DISPLAY '  Recherche par Numero de Securite Sociale        '
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN INPUT F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL NOT CONTINUER
               PERFORM 1000-RECHERCHER-PAR-SS
               DISPLAY ' '
               DISPLAY 'Autre recherche ? (O/N) : '
               ACCEPT WS-CONTINUER
           END-PERFORM

           CLOSE F-PERSONNEL
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Recherche par numero de securite sociale
      *----------------------------------------------------------------*
       1000-RECHERCHER-PAR-SS.
           DISPLAY ' '
           DISPLAY 'Entrez le N° SS a rechercher (10 chiffres) : '
           ACCEPT WS-NUM-SS-RECHERCHE

           MOVE WS-NUM-SS-RECHERCHE TO ENR-NUM-SS

      *    Lecture directe par cle secondaire
           READ F-PERSONNEL KEY IS ENR-NUM-SS
               INVALID KEY
                   DISPLAY '*** N° SS non trouve dans le fichier ***'
                   DISPLAY 'N° SS recherche : ' WS-NUM-SS-RECHERCHE
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY '--- Salarie trouve ---'
                   PERFORM 1100-AFFICHER-SALARIE
           END-READ.

      *----------------------------------------------------------------*
      * Affichage detaille d'un salarie
      *----------------------------------------------------------------*
       1100-AFFICHER-SALARIE.
           MOVE ENR-SALAIRE TO WS-ED-SALAIRE
           MOVE ENR-PRIMES TO WS-ED-PRIMES
           MOVE ENR-REVENU-ANNUEL TO WS-ED-REVENU

           DISPLAY 'Matricule      : ' ENR-MATRICULE
           DISPLAY 'Nom            : ' ENR-NOM
           DISPLAY 'Prenom         : ' ENR-PRENOM
           DISPLAY 'N° SS          : ' ENR-NUM-SS
           DISPLAY 'Salaire        : ' WS-ED-SALAIRE ' EUR'
           DISPLAY 'Primes         : ' WS-ED-PRIMES ' EUR'
           DISPLAY 'Revenu annuel  : ' WS-ED-REVENU ' EUR'.

