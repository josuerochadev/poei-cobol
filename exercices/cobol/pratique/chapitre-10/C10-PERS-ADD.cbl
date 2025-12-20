       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSAD.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-ADD
      * OBJET     : Ajout d'un nouveau salarie dans le fichier KSDS
      *             - Saisie des informations
      *             - Verification cle primaire unique
      *             - WRITE du nouvel enregistrement
      *             - Verification mise a jour index secondaire
      * EXERCICE  : TP Chapitre X - Exercices 5 et 6
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
          88 WS-DUP-KEY            VALUE '22'.
          88 WS-NOT-FOUND          VALUE '23'.
          88 WS-EOF                VALUE '10'.

       01 WS-NOUVEAU-SALARIE.
          05 WS-MATRICULE          PIC 9(6).
          05 WS-NOM                PIC X(15).
          05 WS-PRENOM             PIC X(15).
          05 WS-SALAIRE            PIC 9(6).
          05 WS-PRIMES             PIC 9(6).
          05 WS-REVENU             PIC 9(8).
          05 WS-NUM-SS             PIC 9(10).

       01 WS-CONFIRMER             PIC X VALUE SPACE.
       01 WS-FIN-FICHIER           PIC X VALUE 'N'.
          88 FIN-FICHIER           VALUE 'O'.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICES 5-6 : AJOUT NOUVEAU SALARIE           '
           DISPLAY '  + Verification index secondaire                 '
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN I-O F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture I-O : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM 1000-SAISIR-SALARIE
           PERFORM 2000-AJOUTER-SALARIE
           PERFORM 3000-VERIFIER-INDEX
           PERFORM 9000-FERMER

           STOP RUN.

      *----------------------------------------------------------------*
      * Saisie des informations du nouveau salarie
      *----------------------------------------------------------------*
       1000-SAISIR-SALARIE.
           DISPLAY '--- Saisie du nouveau salarie ---'
           DISPLAY ' '

           DISPLAY 'Matricule (6 chiffres) : '
           ACCEPT WS-MATRICULE

           DISPLAY 'Nom (15 car. max) : '
           ACCEPT WS-NOM

           DISPLAY 'Prenom (15 car. max) : '
           ACCEPT WS-PRENOM

           DISPLAY 'Salaire mensuel (6 chiffres) : '
           ACCEPT WS-SALAIRE

           DISPLAY 'Primes annuelles (6 chiffres) : '
           ACCEPT WS-PRIMES

           DISPLAY 'N° Securite Sociale (10 chiffres) : '
           ACCEPT WS-NUM-SS

      *    Calcul du revenu annuel
           COMPUTE WS-REVENU = (WS-SALAIRE * 12) + WS-PRIMES

           DISPLAY ' '
           DISPLAY '--- Resume du nouveau salarie ---'
           DISPLAY 'Matricule      : ' WS-MATRICULE
           DISPLAY 'Nom            : ' WS-NOM
           DISPLAY 'Prenom         : ' WS-PRENOM
           DISPLAY 'Salaire        : ' WS-SALAIRE
           DISPLAY 'Primes         : ' WS-PRIMES
           DISPLAY 'Revenu annuel  : ' WS-REVENU
           DISPLAY 'N° SS          : ' WS-NUM-SS.

      *----------------------------------------------------------------*
      * Ajout du salarie dans le fichier
      *----------------------------------------------------------------*
       2000-AJOUTER-SALARIE.
           DISPLAY ' '
           DISPLAY 'Confirmer ajout ? (O/N) : '
           ACCEPT WS-CONFIRMER

           IF WS-CONFIRMER = 'O' OR WS-CONFIRMER = 'o'
               MOVE WS-MATRICULE TO ENR-MATRICULE
               MOVE WS-NOM TO ENR-NOM
               MOVE WS-PRENOM TO ENR-PRENOM
               MOVE WS-SALAIRE TO ENR-SALAIRE
               MOVE WS-PRIMES TO ENR-PRIMES
               MOVE WS-REVENU TO ENR-REVENU-ANNUEL
               MOVE WS-NUM-SS TO ENR-NUM-SS

               WRITE ENR-PERSONNEL
                   INVALID KEY
                       EVALUATE TRUE
                           WHEN WS-DUP-KEY
                               DISPLAY '*** ERREUR : Matricule deja '
                                       'existant ***'
                           WHEN OTHER
                               DISPLAY '*** ERREUR WRITE : '
                                       WS-FILE-STATUS ' ***'
                       END-EVALUATE
                   NOT INVALID KEY
                       DISPLAY ' '
                       DISPLAY '*** Salarie ajoute avec succes ***'
               END-WRITE
           ELSE
               DISPLAY 'Ajout annule'
           END-IF.

      *----------------------------------------------------------------*
      * Verification de la mise a jour de l'index secondaire
      * On recherche le nouveau salarie par son N° SS
      *----------------------------------------------------------------*
       3000-VERIFIER-INDEX.
           IF WS-CONFIRMER = 'O' OR WS-CONFIRMER = 'o'
               DISPLAY ' '
               DISPLAY '--- Exercice 6 : Verification index secondaire'
               DISPLAY '    Recherche par N° SS : ' WS-NUM-SS

               MOVE WS-NUM-SS TO ENR-NUM-SS

               READ F-PERSONNEL KEY IS ENR-NUM-SS
                   INVALID KEY
                       DISPLAY '*** ERREUR : N° SS non trouve ***'
                       DISPLAY '    Index secondaire NON mis a jour !'
                   NOT INVALID KEY
                       DISPLAY '*** Index secondaire MIS A JOUR ***'
                       DISPLAY '    Matricule retrouve : ' ENR-MATRICULE
                       DISPLAY '    Nom retrouve       : ' ENR-NOM
               END-READ
           END-IF.

      *----------------------------------------------------------------*
       9000-FERMER.
           CLOSE F-PERSONNEL
           DISPLAY ' '
           DISPLAY 'Fin du programme'.

