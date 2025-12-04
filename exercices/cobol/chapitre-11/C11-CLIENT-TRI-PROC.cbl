       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11CLIPRC.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-CLIENT-TRI-PROC
      * OBJET     : Exercice 3 - TRI PROCEDURAL avec INPUT/OUTPUT PROC
      *             - INPUT PROCEDURE  : Lecture avec RELEASE
      *             - OUTPUT PROCEDURE : Edition avec RETURN
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

           SELECT F-CLIENT-SORTIE
               ASSIGN TO 'CLIENT-TRI-PROC.PS'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-S.

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

       FD  F-CLIENT-SORTIE
           RECORDING MODE IS F
           RECORD CONTAINS 70 CHARACTERS.
       01  ENR-SORTIE              PIC X(70).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-S             PIC XX.

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

       01  WS-CPT-LUS              PIC 99 VALUE 0.
       01  WS-CPT-ECRITS           PIC 99 VALUE 0.
       01  WS-ED-SOLDE             PIC -----.---.--9,99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 3 : TRI PROCEDURAL                     '
           DISPLAY '  INPUT PROCEDURE (RELEASE) + OUTPUT PROCEDURE    '
           DISPLAY '  (RETURN)                                        '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Tri avec procedures INPUT et OUTPUT
           SORT F-TRI
               ON ASCENDING KEY TRI-ID
               INPUT PROCEDURE IS 1000-LIRE-ENTREE
               OUTPUT PROCEDURE IS 2000-ECRIRE-SORTIE

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Enregistrements lus    : ' WS-CPT-LUS
           DISPLAY 'Enregistrements ecrits : ' WS-CPT-ECRITS
           DISPLAY '=================================================='
           DISPLAY 'Fichier trie : CLIENT-TRI-PROC.PS'
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Lecture du fichier d'entree
      * Utilise RELEASE pour envoyer chaque enregistrement au tri
      *----------------------------------------------------------------*
       1000-LIRE-ENTREE.
           DISPLAY '--- INPUT PROCEDURE : Lecture avec RELEASE ---'
           OPEN INPUT F-CLIENT-ENTREE
           MOVE 'N' TO WS-FIN-LECTURE

           PERFORM UNTIL FIN-LECTURE
               READ F-CLIENT-ENTREE INTO WS-CLIENT
                   AT END
                       SET FIN-LECTURE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CPT-LUS
      *                Envoyer l'enregistrement au tri via RELEASE
                       MOVE WS-CLIENT TO ENR-TRI
                       RELEASE ENR-TRI
                       DISPLAY 'RELEASE #' WS-CPT-LUS ' : ' WS-ID
                               ' - ' WS-NOM
               END-READ
           END-PERFORM

           CLOSE F-CLIENT-ENTREE
           DISPLAY ' '
           DISPLAY 'INPUT PROCEDURE terminee - ' WS-CPT-LUS
                   ' enregistrements envoyes au tri'.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Ecriture du fichier de sortie
      * Utilise RETURN pour recuperer chaque enregistrement trie
      *----------------------------------------------------------------*
       2000-ECRIRE-SORTIE.
           DISPLAY ' '
           DISPLAY '--- OUTPUT PROCEDURE : Ecriture avec RETURN ---'
           OPEN OUTPUT F-CLIENT-SORTIE
           MOVE 'N' TO WS-FIN-TRI

           DISPLAY ' '
           DISPLAY 'Enregistrements tries (ASCENDING ID) :'
           DISPLAY '-------- -------------------- ---------------'

           PERFORM UNTIL FIN-TRI
      *        Recuperer l'enregistrement suivant du tri via RETURN
               RETURN F-TRI INTO WS-CLIENT
                   AT END
                       SET FIN-TRI TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CPT-ECRITS
      *                Ecrire dans le fichier de sortie
                       WRITE ENR-SORTIE FROM WS-CLIENT
                       MOVE WS-SOLDE TO WS-ED-SOLDE
                       DISPLAY 'RETURN  #' WS-CPT-ECRITS ' : '
                               WS-ID ' - ' WS-NOM ' - ' WS-ED-SOLDE
               END-RETURN
           END-PERFORM

           CLOSE F-CLIENT-SORTIE
           DISPLAY ' '
           DISPLAY 'OUTPUT PROCEDURE terminee - ' WS-CPT-ECRITS
                   ' enregistrements ecrits'.

