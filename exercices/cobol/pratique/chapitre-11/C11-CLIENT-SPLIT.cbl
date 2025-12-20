       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11CLISPL.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-CLIENT-SPLIT
      * OBJET     : Exercice 4 - Creation de 2 Data Sets avec cles
      *             specifiques a partir du fichier CLIENT original
      *             - CLIENT-COURANT.PS : Comptes COURANT (cle: C-)
      *             - CLIENT-EPARGNE.PS : Comptes EPARGNE/PEL/LIVRET
      *             (cle: E-)
      *             Le fichier original garde sa cle CLI-
      * EXERCICE  : TP Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENT-ENTREE
               ASSIGN TO 'CLIENT.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

           SELECT F-TRI
               ASSIGN TO 'SORT-CLIENT.TMP'.

           SELECT F-CLIENT-COURANT
               ASSIGN TO 'CLIENT-COURANT.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-C.

           SELECT F-CLIENT-EPARGNE
               ASSIGN TO 'CLIENT-EPARGNE.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-EP.

       DATA DIVISION.
       FILE SECTION.
       FD  F-CLIENT-ENTREE
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-ENTREE              PIC X(70).

       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-ID              PIC X(8).
           05  TRI-NOM             PIC X(20).
           05  TRI-PRENOM          PIC X(15).
           05  TRI-SOLDE           PIC S9(10)V99.
           05  TRI-TYPE-COMPTE     PIC X(15).

       FD  F-CLIENT-COURANT
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-COURANT             PIC X(70).

       FD  F-CLIENT-EPARGNE
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-EPARGNE             PIC X(70).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-C             PIC XX.
       01  WS-STATUS-EP            PIC XX.

       01  WS-FIN-LECTURE          PIC X VALUE 'N'.
           88 FIN-LECTURE          VALUE 'O'.

       01  WS-FIN-TRI              PIC X VALUE 'N'.
           88 FIN-TRI              VALUE 'O'.

       01  WS-CLIENT.
           05  WS-ID               PIC X(8).
           05  WS-NOM              PIC X(20).
           05  WS-PRENOM           PIC X(15).
           05  WS-SOLDE            PIC S9(10)V99.
           05  WS-TYPE-COMPTE      PIC X(15).

       01  WS-CLIENT-OUT.
           05  WS-OUT-ID           PIC X(8).
           05  WS-OUT-NOM          PIC X(20).
           05  WS-OUT-PRENOM       PIC X(15).
           05  WS-OUT-SOLDE        PIC S9(10)V99.
           05  WS-OUT-TYPE         PIC X(15).

       01  WS-NUMERO-SEQ-C         PIC 9(5) VALUE 0.
       01  WS-NUMERO-SEQ-E         PIC 9(5) VALUE 0.
       01  WS-CPT-COURANT          PIC 99 VALUE 0.
       01  WS-CPT-EPARGNE          PIC 99 VALUE 0.
       01  WS-CPT-TOTAL            PIC 99 VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 4 : CREATION DE 2 DATA SETS            '
           DISPLAY '  - CLIENT-COURANT.PS (cle C-xxxxx)               '
           DISPLAY '  - CLIENT-EPARGNE.PS (cle E-xxxxx)               '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Tri du fichier original puis dispatch dans les 2 fichiers
           SORT F-TRI
               ON ASCENDING KEY TRI-TYPE-COMPTE
               ON ASCENDING KEY TRI-NOM
               INPUT PROCEDURE IS 1000-LIRE-ENTREE
               OUTPUT PROCEDURE IS 2000-DISPATCHER

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Resultats :'
           DISPLAY '  - CLIENT-COURANT.PS : ' WS-CPT-COURANT ' clients'
           DISPLAY '  - CLIENT-EPARGNE.PS : ' WS-CPT-EPARGNE ' clients'
           DISPLAY '  - Total traites     : ' WS-CPT-TOTAL
           DISPLAY '=================================================='
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Lecture du fichier d'entree
      *----------------------------------------------------------------*
       1000-LIRE-ENTREE.
           DISPLAY '--- Lecture du fichier CLIENT.PS ---'
           OPEN INPUT F-CLIENT-ENTREE
           MOVE 'N' TO WS-FIN-LECTURE

           PERFORM UNTIL FIN-LECTURE
               READ F-CLIENT-ENTREE INTO WS-CLIENT
                   AT END
                       SET FIN-LECTURE TO TRUE
                   NOT AT END
                       MOVE WS-CLIENT TO ENR-TRI
                       RELEASE ENR-TRI
               END-READ
           END-PERFORM

           CLOSE F-CLIENT-ENTREE
           DISPLAY 'Lecture terminee'.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Dispatch vers les 2 fichiers
      * - COURANT         -> CLIENT-COURANT.PS (cle C-xxxxx)
      * - EPARGNE/PEL/LIVRET -> CLIENT-EPARGNE.PS (cle E-xxxxx)
      *----------------------------------------------------------------*
       2000-DISPATCHER.
           DISPLAY ' '
           DISPLAY '--- Dispatch vers les 2 fichiers ---'
           OPEN OUTPUT F-CLIENT-COURANT
           OPEN OUTPUT F-CLIENT-EPARGNE
           MOVE 'N' TO WS-FIN-TRI

           PERFORM UNTIL FIN-TRI
               RETURN F-TRI INTO WS-CLIENT
                   AT END
                       SET FIN-TRI TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CPT-TOTAL
                       PERFORM 2100-TRAITER-CLIENT
               END-RETURN
           END-PERFORM

           CLOSE F-CLIENT-COURANT
           CLOSE F-CLIENT-EPARGNE
           DISPLAY 'Dispatch termine'.

      *----------------------------------------------------------------*
      * Traitement d'un client : affectation au bon fichier
      *----------------------------------------------------------------*
       2100-TRAITER-CLIENT.
           EVALUATE TRUE
               WHEN WS-TYPE-COMPTE = 'COURANT'
                   PERFORM 2200-ECRIRE-COURANT
               WHEN OTHER
      *            EPARGNE, PEL, LIVRET-A -> Fichier EPARGNE
                   PERFORM 2300-ECRIRE-EPARGNE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * Ecriture dans CLIENT-COURANT.PS avec nouvelle cle C-xxxxx
      *----------------------------------------------------------------*
       2200-ECRIRE-COURANT.
           ADD 1 TO WS-NUMERO-SEQ-C
           ADD 1 TO WS-CPT-COURANT

      *    Generer la nouvelle cle : C-00001, C-00002, ...
           STRING 'C-' DELIMITED SIZE
                  WS-NUMERO-SEQ-C DELIMITED SIZE
                  INTO WS-OUT-ID

           MOVE WS-NOM TO WS-OUT-NOM
           MOVE WS-PRENOM TO WS-OUT-PRENOM
           MOVE WS-SOLDE TO WS-OUT-SOLDE
           MOVE WS-TYPE-COMPTE TO WS-OUT-TYPE

           WRITE ENR-COURANT FROM WS-CLIENT-OUT
           DISPLAY 'COURANT  : ' WS-OUT-ID ' - ' WS-NOM ' - '
                   WS-TYPE-COMPTE.

      *----------------------------------------------------------------*
      * Ecriture dans CLIENT-EPARGNE.PS avec nouvelle cle E-xxxxx
      *----------------------------------------------------------------*
       2300-ECRIRE-EPARGNE.
           ADD 1 TO WS-NUMERO-SEQ-E
           ADD 1 TO WS-CPT-EPARGNE

      *    Generer la nouvelle cle : E-00001, E-00002, ...
           STRING 'E-' DELIMITED SIZE
                  WS-NUMERO-SEQ-E DELIMITED SIZE
                  INTO WS-OUT-ID

           MOVE WS-NOM TO WS-OUT-NOM
           MOVE WS-PRENOM TO WS-OUT-PRENOM
           MOVE WS-SOLDE TO WS-OUT-SOLDE
           MOVE WS-TYPE-COMPTE TO WS-OUT-TYPE

           WRITE ENR-EPARGNE FROM WS-CLIENT-OUT
           DISPLAY 'EPARGNE  : ' WS-OUT-ID ' - ' WS-NOM ' - '
                   WS-TYPE-COMPTE.

