       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11CLIDSC.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-CLIENT-TRI-DESC
      * OBJET     : Exercice 2 - TRI DESCENDING sur ID-CLIENT (cle)
      *             Utilisation de SORT avec USING/GIVING
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
               ASSIGN TO 'CLIENT-TRI-DESC.PS'
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
       01  WS-CPT                  PIC 99 VALUE 0.
       01  WS-ED-SOLDE             PIC -----.---.--9,99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 2 : TRI DESCENDING SUR ID-CLIENT       '
           DISPLAY '  SORT ... USING ... GIVING                       '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Afficher le fichier avant tri
           DISPLAY '--- Fichier AVANT tri ---'
           PERFORM 1000-AFFICHER-FICHIER-ENTREE

      *    Tri DESCENDING sur la cle ID-CLIENT
           DISPLAY ' '
           DISPLAY '--- Execution du SORT DESCENDING KEY TRI-ID ---'
           SORT F-TRI
               ON DESCENDING KEY TRI-ID
               USING F-CLIENT-ENTREE
               GIVING F-CLIENT-SORTIE

           DISPLAY 'Tri termine avec succes'

      *    Afficher le fichier apres tri
           DISPLAY ' '
           DISPLAY '--- Fichier APRES tri (DESCENDING) ---'
           PERFORM 2000-AFFICHER-FICHIER-SORTIE

           DISPLAY ' '
           DISPLAY 'Fichier trie : CLIENT-TRI-DESC.PS'
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Afficher le fichier d'entree
      *----------------------------------------------------------------*
       1000-AFFICHER-FICHIER-ENTREE.
           OPEN INPUT F-CLIENT-ENTREE
           DISPLAY 'ID       NOM                  PRENOM          '
                   'SOLDE            TYPE'
           DISPLAY '-------- -------------------- --------------- '
                   '---------------- ---------------'
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-E = '10'
               READ F-CLIENT-ENTREE INTO ENR-TRI
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       MOVE TRI-SOLDE TO WS-ED-SOLDE
                       DISPLAY TRI-ID ' ' TRI-NOM ' ' TRI-PRENOM ' '
                               WS-ED-SOLDE ' ' TRI-TYPE-COMPTE
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-ENTREE
           DISPLAY 'Total : ' WS-CPT ' enregistrements'.

      *----------------------------------------------------------------*
      * Afficher le fichier de sortie
      *----------------------------------------------------------------*
       2000-AFFICHER-FICHIER-SORTIE.
           OPEN INPUT F-CLIENT-SORTIE
           DISPLAY 'ID       NOM                  PRENOM          '
                   'SOLDE            TYPE'
           DISPLAY '-------- -------------------- --------------- '
                   '---------------- ---------------'
           MOVE 0 TO WS-CPT
           PERFORM UNTIL WS-STATUS-S = '10'
               READ F-CLIENT-SORTIE INTO ENR-TRI
                   AT END CONTINUE
                   NOT AT END
                       ADD 1 TO WS-CPT
                       MOVE TRI-SOLDE TO WS-ED-SOLDE
                       DISPLAY TRI-ID ' ' TRI-NOM ' ' TRI-PRENOM ' '
                               WS-ED-SOLDE ' ' TRI-TYPE-COMPTE
               END-READ
           END-PERFORM
           CLOSE F-CLIENT-SORTIE
           DISPLAY 'Total : ' WS-CPT ' enregistrements'.

