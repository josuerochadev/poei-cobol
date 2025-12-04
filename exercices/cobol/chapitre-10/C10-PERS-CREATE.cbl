       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSCR.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-CREATE
      * OBJET     : Creation du fichier KSDS PERSONNEL avec :
      *             - Cle primaire : MATRICULE
      *             - Cle secondaire : NUM-SS (avec doublons autorises)
      * EXERCICE  : TP Chapitre X - Exercice (preparation)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * Fichier KSDS avec cle primaire et secondaire
      *----------------------------------------------------------------*
           SELECT F-PERSONNEL
               ASSIGN TO 'PERSONNEL.KSDS'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS ENR-MATRICULE
               ALTERNATE RECORD KEY IS ENR-NUM-SS WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Structure enregistrement PERSONNEL (66 caracteres)
      * Identique au chapitre IX
      *----------------------------------------------------------------*
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
      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01 WS-FILE-STATUS           PIC XX.
          88 WS-OK                 VALUE '00'.
          88 WS-DUP-KEY            VALUE '22'.

       01 WS-IDX                   PIC 9 VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  CREATION FICHIER KSDS PERSONNEL                 '
           DISPLAY '  Cle primaire   : MATRICULE                      '
           DISPLAY '  Cle secondaire : NUM-SS (avec doublons)         '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-CREER-FICHIER
           PERFORM 2000-AFFICHER-RESUME

           DISPLAY ' '
           DISPLAY 'Fichier PERSONNEL.KSDS cree avec succes'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation et chargement du fichier
      *----------------------------------------------------------------*
       1000-CREER-FICHIER.
           OPEN OUTPUT F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture OUTPUT : ' WS-FILE-STATUS
               STOP RUN
           END-IF

      *    Salarie 1 : DUPONT Jean
           MOVE 000001 TO ENR-MATRICULE
           MOVE 'DUPONT' TO ENR-NOM
           MOVE 'JEAN' TO ENR-PRENOM
           MOVE 002500 TO ENR-SALAIRE
           MOVE 001000 TO ENR-PRIMES
           MOVE 00000000 TO ENR-REVENU-ANNUEL
           MOVE 1850512345 TO ENR-NUM-SS
           PERFORM 1100-ECRIRE-SALARIE

      *    Salarie 2 : MARTIN Marie
           MOVE 000002 TO ENR-MATRICULE
           MOVE 'MARTIN' TO ENR-NOM
           MOVE 'MARIE' TO ENR-PRENOM
           MOVE 003000 TO ENR-SALAIRE
           MOVE 001500 TO ENR-PRIMES
           MOVE 00000000 TO ENR-REVENU-ANNUEL
           MOVE 2751067890 TO ENR-NUM-SS
           PERFORM 1100-ECRIRE-SALARIE

      *    Salarie 3 : BERNARD Pierre
           MOVE 000003 TO ENR-MATRICULE
           MOVE 'BERNARD' TO ENR-NOM
           MOVE 'PIERRE' TO ENR-PRENOM
           MOVE 002800 TO ENR-SALAIRE
           MOVE 001200 TO ENR-PRIMES
           MOVE 00000000 TO ENR-REVENU-ANNUEL
           MOVE 1920198765 TO ENR-NUM-SS
           PERFORM 1100-ECRIRE-SALARIE

      *    Salarie 4 : PETIT Sophie
           MOVE 000004 TO ENR-MATRICULE
           MOVE 'PETIT' TO ENR-NOM
           MOVE 'SOPHIE' TO ENR-PRENOM
           MOVE 003200 TO ENR-SALAIRE
           MOVE 002000 TO ENR-PRIMES
           MOVE 00000000 TO ENR-REVENU-ANNUEL
           MOVE 2880234567 TO ENR-NUM-SS
           PERFORM 1100-ECRIRE-SALARIE

      *    Salarie 5 : DURAND Claude
           MOVE 000005 TO ENR-MATRICULE
           MOVE 'DURAND' TO ENR-NOM
           MOVE 'CLAUDE' TO ENR-PRENOM
           MOVE 002600 TO ENR-SALAIRE
           MOVE 000800 TO ENR-PRIMES
           MOVE 00000000 TO ENR-REVENU-ANNUEL
           MOVE 1780345678 TO ENR-NUM-SS
           PERFORM 1100-ECRIRE-SALARIE

           CLOSE F-PERSONNEL.

      *----------------------------------------------------------------*
      * Ecriture d'un salarie
      *----------------------------------------------------------------*
       1100-ECRIRE-SALARIE.
           ADD 1 TO WS-IDX
           WRITE ENR-PERSONNEL
               INVALID KEY
                   DISPLAY 'Erreur ecriture : ' WS-FILE-STATUS
                   DISPLAY 'Matricule : ' ENR-MATRICULE
           END-WRITE
           IF WS-OK
               DISPLAY 'Insere : ' ENR-MATRICULE ' - '
                       ENR-NOM ' - SS: ' ENR-NUM-SS
           END-IF.

      *----------------------------------------------------------------*
      * Resume de la creation
      *----------------------------------------------------------------*
       2000-AFFICHER-RESUME.
           DISPLAY ' '
           DISPLAY '--- Resume ---'
           DISPLAY WS-IDX ' salaries inseres dans PERSONNEL.KSDS'
           DISPLAY 'Index primaire   : MATRICULE (000001 a 000005)'
           DISPLAY 'Index secondaire : NUM-SS'.

