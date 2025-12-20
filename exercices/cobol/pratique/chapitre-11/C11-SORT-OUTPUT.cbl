       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11SORTOU.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-SORT-OUTPUT
      * OBJET     : Demonstration OUTPUT PROCEDURE avec RETURN
      *             Edition d'un rapport avec ruptures de controle
      * EXERCICE  : Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ENTREE
               ASSIGN TO 'EMPLOYES-ENTREE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

           SELECT F-TRI
               ASSIGN TO 'SORT-WORK.TMP'.

       DATA DIVISION.
       FILE SECTION.
       FD  F-ENTREE
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-ENTREE              PIC X(50).

       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-MATRICULE       PIC 9(6).
           05  TRI-NOM             PIC X(20).
           05  TRI-DEPT            PIC X(10).
           05  TRI-SALAIRE         PIC 9(6).
           05  TRI-STATUT          PIC X.
           05  FILLER              PIC X(7).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.

       01  WS-FIN-TRI              PIC X VALUE 'N'.
           88 FIN-TRI              VALUE 'O'.

       01  WS-EMPLOYE.
           05  WS-MATRICULE        PIC 9(6).
           05  WS-NOM              PIC X(20).
           05  WS-DEPT             PIC X(10).
           05  WS-SALAIRE          PIC 9(6).
           05  WS-STATUT           PIC X.
           05  FILLER              PIC X(7).

      *----------------------------------------------------------------*
      * Variables pour les ruptures de controle
      *----------------------------------------------------------------*
       01  WS-DEPT-PRECEDENT       PIC X(10) VALUE SPACES.
       01  WS-SOUS-TOTAL           PIC 9(8) VALUE 0.
       01  WS-TOTAL-GENERAL        PIC 9(10) VALUE 0.
       01  WS-CPT-DEPT             PIC 9(3) VALUE 0.
       01  WS-CPT-TOTAL            PIC 9(3) VALUE 0.

      *----------------------------------------------------------------*
      * Variables d'edition
      *----------------------------------------------------------------*
       01  WS-ED-SALAIRE           PIC ZZZ.ZZ9.
       01  WS-ED-SOUS-TOTAL        PIC ZZ.ZZZ.ZZ9.
       01  WS-ED-TOTAL             PIC ZZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  DEMONSTRATION OUTPUT PROCEDURE (RETURN)         '
           DISPLAY '  Edition avec ruptures de controle par DEPT      '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Creer le fichier d'entree
           PERFORM 1000-CREER-FICHIER-ENTREE

      *    Tri avec OUTPUT PROCEDURE pour edition
           SORT F-TRI
               ON ASCENDING KEY TRI-DEPT
               ON ASCENDING KEY TRI-NOM
               USING F-ENTREE
               OUTPUT PROCEDURE IS 2000-EDITER-RAPPORT

           DISPLAY ' '
           DISPLAY 'Fin du programme C11-SORT-OUTPUT'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation du fichier d'entree
      *----------------------------------------------------------------*
       1000-CREER-FICHIER-ENTREE.
           OPEN OUTPUT F-ENTREE

           MOVE '000001DUPONT              INFO      002500A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000002MARTIN              RH        003000A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000003BERNARD             COMPTA    002800A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000004PETIT               INFO      003200A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000005DURAND              RH        002600A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000006LEROY               INFO      002900A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000007MOREAU              COMPTA    003100A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           CLOSE F-ENTREE
           DISPLAY '7 employes crees'.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Edition du rapport avec ruptures
      * Utilise RETURN pour recuperer les enregistrements tries
      *----------------------------------------------------------------*
       2000-EDITER-RAPPORT.
           DISPLAY ' '
           DISPLAY '=============================================='
           DISPLAY '     RAPPORT DES EMPLOYES PAR DEPARTEMENT     '
           DISPLAY '=============================================='

           MOVE SPACES TO WS-DEPT-PRECEDENT
           MOVE 0 TO WS-SOUS-TOTAL
           MOVE 0 TO WS-TOTAL-GENERAL
           MOVE 0 TO WS-CPT-TOTAL
           MOVE 'N' TO WS-FIN-TRI

           PERFORM UNTIL FIN-TRI
      *        RETURN recupere l'enregistrement suivant du tri
               RETURN F-TRI INTO WS-EMPLOYE
                   AT END
                       SET FIN-TRI TO TRUE
      *                Afficher le dernier sous-total
                       IF WS-DEPT-PRECEDENT NOT = SPACES
                           PERFORM 2200-AFFICHER-SOUS-TOTAL
                       END-IF
                   NOT AT END
      *                Detecter changement de departement (rupture)
                       IF WS-DEPT NOT = WS-DEPT-PRECEDENT
      *                    Sous-total du departement precedent
                           IF WS-DEPT-PRECEDENT NOT = SPACES
                               PERFORM 2200-AFFICHER-SOUS-TOTAL
                           END-IF
      *                    Nouveau departement
                           PERFORM 2100-EN-TETE-DEPARTEMENT
                       END-IF
      *                Afficher l'employe
                       PERFORM 2300-AFFICHER-EMPLOYE
               END-RETURN
           END-PERFORM

      *    Total general
           PERFORM 2400-AFFICHER-TOTAL-GENERAL.

      *----------------------------------------------------------------*
      * En-tete de departement
      *----------------------------------------------------------------*
       2100-EN-TETE-DEPARTEMENT.
           MOVE WS-DEPT TO WS-DEPT-PRECEDENT
           MOVE 0 TO WS-SOUS-TOTAL
           MOVE 0 TO WS-CPT-DEPT
           DISPLAY ' '
           DISPLAY '----------------------------------------------'
           DISPLAY 'DEPARTEMENT : ' WS-DEPT
           DISPLAY '----------------------------------------------'.

      *----------------------------------------------------------------*
      * Sous-total de departement
      *----------------------------------------------------------------*
       2200-AFFICHER-SOUS-TOTAL.
           MOVE WS-SOUS-TOTAL TO WS-ED-SOUS-TOTAL
           DISPLAY '   ----------------------------------------'
           DISPLAY '   Sous-total ' WS-DEPT-PRECEDENT ' : '
                   WS-ED-SOUS-TOTAL ' EUR (' WS-CPT-DEPT ' emp.)'
           ADD WS-SOUS-TOTAL TO WS-TOTAL-GENERAL.

      *----------------------------------------------------------------*
      * Affichage d'un employe
      *----------------------------------------------------------------*
       2300-AFFICHER-EMPLOYE.
           MOVE WS-SALAIRE TO WS-ED-SALAIRE
           DISPLAY '   ' WS-MATRICULE ' - ' WS-NOM ' - '
                   WS-ED-SALAIRE ' EUR'
           ADD WS-SALAIRE TO WS-SOUS-TOTAL
           ADD 1 TO WS-CPT-DEPT
           ADD 1 TO WS-CPT-TOTAL.

      *----------------------------------------------------------------*
      * Total general
      *----------------------------------------------------------------*
       2400-AFFICHER-TOTAL-GENERAL.
           MOVE WS-TOTAL-GENERAL TO WS-ED-TOTAL
           DISPLAY ' '
           DISPLAY '=============================================='
           DISPLAY 'TOTAL GENERAL : ' WS-ED-TOTAL ' EUR'
           DISPLAY 'Nombre total employes : ' WS-CPT-TOTAL
           DISPLAY '=============================================='.

