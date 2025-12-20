       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11CLICRE.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-CLIENT-CREATE
      * OBJET     : Creation du fichier PS CLIENT (institution financiere)
      *             Structure : ID-CLIENT, NOM, PRENOM, SOLDE, TYPE-COMPTE
      *             10 enregistrements en desordre
      * EXERCICE  : TP Chapitre XI - Preparation
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENT
               ASSIGN TO 'CLIENT.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Structure CLIENT (70 caracteres)
      * - ID-CLIENT  : Cle primaire (8 car.)
      * - NOM        : Nom du client (20 car.)
      * - PRENOM     : Prenom du client (15 car.)
      * - SOLDE      : Solde du compte (12 car. dont 2 dec.)
      * - TYPE-COMPTE: Type de compte (15 car.)
      *----------------------------------------------------------------*
       FD  F-CLIENT
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-CLIENT.
           05  CLI-ID              PIC X(8).
           05  CLI-NOM             PIC X(20).
           05  CLI-PRENOM          PIC X(15).
           05  CLI-SOLDE           PIC S9(10)V99.
           05  CLI-TYPE-COMPTE     PIC X(15).

       WORKING-STORAGE SECTION.
       01  WS-STATUS               PIC XX.
       01  WS-CPT                  PIC 99 VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  CREATION FICHIER PS CLIENT                      '
           DISPLAY '  Institution financiere - 10 clients             '
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN OUTPUT F-CLIENT

      *    Client 1 - ID en desordre volontairement
           MOVE 'CLI00007' TO CLI-ID
           MOVE 'DUPONT' TO CLI-NOM
           MOVE 'JEAN' TO CLI-PRENOM
           MOVE 15000.50 TO CLI-SOLDE
           MOVE 'COURANT' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 2
           MOVE 'CLI00003' TO CLI-ID
           MOVE 'MARTIN' TO CLI-NOM
           MOVE 'MARIE' TO CLI-PRENOM
           MOVE 45000.00 TO CLI-SOLDE
           MOVE 'EPARGNE' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 3
           MOVE 'CLI00010' TO CLI-ID
           MOVE 'BERNARD' TO CLI-NOM
           MOVE 'PIERRE' TO CLI-PRENOM
           MOVE -500.25 TO CLI-SOLDE
           MOVE 'COURANT' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 4
           MOVE 'CLI00001' TO CLI-ID
           MOVE 'PETIT' TO CLI-NOM
           MOVE 'SOPHIE' TO CLI-PRENOM
           MOVE 125000.00 TO CLI-SOLDE
           MOVE 'PEL' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 5
           MOVE 'CLI00008' TO CLI-ID
           MOVE 'DURAND' TO CLI-NOM
           MOVE 'CLAUDE' TO CLI-PRENOM
           MOVE 8500.75 TO CLI-SOLDE
           MOVE 'LIVRET-A' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 6
           MOVE 'CLI00004' TO CLI-ID
           MOVE 'LEROY' TO CLI-NOM
           MOVE 'MARC' TO CLI-PRENOM
           MOVE 32000.00 TO CLI-SOLDE
           MOVE 'COURANT' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 7
           MOVE 'CLI00009' TO CLI-ID
           MOVE 'MOREAU' TO CLI-NOM
           MOVE 'ANNE' TO CLI-PRENOM
           MOVE 67500.50 TO CLI-SOLDE
           MOVE 'EPARGNE' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 8
           MOVE 'CLI00002' TO CLI-ID
           MOVE 'THOMAS' TO CLI-NOM
           MOVE 'LUC' TO CLI-PRENOM
           MOVE 3200.00 TO CLI-SOLDE
           MOVE 'LIVRET-A' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 9
           MOVE 'CLI00006' TO CLI-ID
           MOVE 'RICHARD' TO CLI-NOM
           MOVE 'PAUL' TO CLI-PRENOM
           MOVE 18750.25 TO CLI-SOLDE
           MOVE 'PEL' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

      *    Client 10
           MOVE 'CLI00005' TO CLI-ID
           MOVE 'ROBERT' TO CLI-NOM
           MOVE 'JULIE' TO CLI-PRENOM
           MOVE 95000.00 TO CLI-SOLDE
           MOVE 'EPARGNE' TO CLI-TYPE-COMPTE
           PERFORM 9000-ECRIRE-CLIENT

           CLOSE F-CLIENT

           DISPLAY ' '
           DISPLAY WS-CPT ' clients crees dans CLIENT.PS'
           DISPLAY '(Enregistrements en desordre pour test du tri)'
           DISPLAY ' '
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
       9000-ECRIRE-CLIENT.
           WRITE ENR-CLIENT
           ADD 1 TO WS-CPT
           DISPLAY 'Cree : ' CLI-ID ' - ' CLI-NOM ' - ' CLI-TYPE-COMPTE.

