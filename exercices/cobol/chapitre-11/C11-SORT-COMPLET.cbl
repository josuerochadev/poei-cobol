       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11SORTCP.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-SORT-COMPLET
      * OBJET     : Exemple complet avec INPUT et OUTPUT PROCEDURE
      *             - INPUT  : Filtrage + transformation
      *             - OUTPUT : Edition avec ruptures et totaux
      * EXERCICE  : TP Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLOYES
               ASSIGN TO 'EMPLOYES-BRUT.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

           SELECT F-TRI
               ASSIGN TO 'SORT-WORK.TMP'.

           SELECT F-RAPPORT
               ASSIGN TO 'RAPPORT-EMPLOYES.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-R.

       DATA DIVISION.
       FILE SECTION.
       FD  F-EMPLOYES
           RECORDING MODE IS F
           RECORD CONTAINS 60 CHARACTERS.
       01  ENR-EMPLOYE             PIC X(60).

       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-MATRICULE       PIC 9(6).
           05  TRI-NOM             PIC X(20).
           05  TRI-DEPT            PIC X(10).
           05  TRI-SALAIRE         PIC 9(6).
           05  TRI-STATUT          PIC X.
           05  TRI-SAL-ANNUEL      PIC 9(8).
           05  FILLER              PIC X(9).

       FD  F-RAPPORT
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  LIGNE-RAPPORT           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-R             PIC XX.

       01  WS-FIN-LECTURE          PIC X VALUE 'N'.
           88 FIN-LECTURE          VALUE 'O'.

       01  WS-FIN-TRI              PIC X VALUE 'N'.
           88 FIN-TRI              VALUE 'O'.

      *----------------------------------------------------------------*
      * Zone de travail pour INPUT PROCEDURE
      *----------------------------------------------------------------*
       01  WS-EMPLOYE.
           05  WS-MATRICULE        PIC 9(6).
           05  WS-NOM              PIC X(20).
           05  WS-DEPT             PIC X(10).
           05  WS-SALAIRE          PIC 9(6).
           05  WS-STATUT           PIC X.
           05  FILLER              PIC X(17).

       01  WS-CPT-LUS              PIC 9(3) VALUE 0.
       01  WS-CPT-FILTRES          PIC 9(3) VALUE 0.

      *----------------------------------------------------------------*
      * Zone de travail pour OUTPUT PROCEDURE
      *----------------------------------------------------------------*
       01  WS-DEPT-PRECEDENT       PIC X(10) VALUE SPACES.
       01  WS-SOUS-TOTAL           PIC 9(10) VALUE 0.
       01  WS-TOTAL-GENERAL        PIC 9(12) VALUE 0.
       01  WS-CPT-DEPT             PIC 9(3) VALUE 0.
       01  WS-CPT-TOTAL            PIC 9(4) VALUE 0.

      *----------------------------------------------------------------*
      * Lignes du rapport
      *----------------------------------------------------------------*
       01  WS-LIGNE-TITRE.
           05  FILLER PIC X(25) VALUE SPACES.
           05  FILLER PIC X(30) VALUE 'RAPPORT DES EMPLOYES ACTIFS'.
           05  FILLER PIC X(25) VALUE SPACES.

       01  WS-LIGNE-SEPARATEUR.
           05  FILLER PIC X(80) VALUE ALL '='.

       01  WS-LIGNE-DEPT.
           05  FILLER              PIC X(15) VALUE 'DEPARTEMENT : '.
           05  WS-LD-DEPT          PIC X(10).
           05  FILLER              PIC X(55) VALUE SPACES.

       01  WS-LIGNE-DETAIL.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-DET-MAT          PIC 9(6).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DET-NOM          PIC X(20).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DET-SAL-MENS     PIC ZZ.ZZ9.
           05  FILLER              PIC X(5) VALUE ' EUR '.
           05  WS-DET-SAL-ANN      PIC ZZZ.ZZ.ZZ9.
           05  FILLER              PIC X(10) VALUE ' EUR/an'.
           05  FILLER              PIC X(11) VALUE SPACES.

       01  WS-LIGNE-SOUS-TOTAL.
           05  FILLER              PIC X(5) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE 'Sous-total :'.
           05  WS-ST-MONTANT       PIC ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(5) VALUE ' EUR '.
           05  FILLER              PIC X(1) VALUE '('.
           05  WS-ST-CPT           PIC ZZ9.
           05  FILLER              PIC X(10) VALUE ' employes)'.
           05  FILLER              PIC X(24) VALUE SPACES.

       01  WS-LIGNE-TOTAL.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE 'TOTAL GENERAL :'.
           05  WS-TG-MONTANT       PIC Z.ZZZ.ZZZ.ZZ9.
           05  FILLER              PIC X(5) VALUE ' EUR '.
           05  FILLER              PIC X(1) VALUE '('.
           05  WS-TG-CPT           PIC Z.ZZ9.
           05  FILLER              PIC X(10) VALUE ' employes)'.
           05  FILLER              PIC X(19) VALUE SPACES.

       01  WS-LIGNE-VIDE           PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  TP COMPLET : INPUT + OUTPUT PROCEDURE           '
           DISPLAY '  Filtrage, calcul, tri et edition                '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Creer le fichier d'entree
           PERFORM 0100-CREER-FICHIER-ENTREE

      *    Tri complet avec les deux procedures
           SORT F-TRI
               ON ASCENDING KEY TRI-DEPT
               ON DESCENDING KEY TRI-SAL-ANNUEL
               INPUT PROCEDURE IS 1000-PREPARER-DONNEES
               OUTPUT PROCEDURE IS 2000-GENERER-RAPPORT

           DISPLAY ' '
           DISPLAY 'Rapport genere : RAPPORT-EMPLOYES.TXT'
           DISPLAY 'Fin du programme C11-SORT-COMPLET'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation du fichier d'entree
      *----------------------------------------------------------------*
       0100-CREER-FICHIER-ENTREE.
           OPEN OUTPUT F-EMPLOYES

           MOVE '000001DUPONT              INFO      002500A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000002MARTIN              RH        003000I' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000003BERNARD             COMPTA    002800A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000004PETIT               INFO      003200A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000005DURAND              RH        002600A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000006LEROY               INFO      002900I' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000007MOREAU              COMPTA    003100A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000008THOMAS              RH        002700A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000009ROBERT              INFO      003500I' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE
           MOVE '000010RICHARD             COMPTA    002400A' TO ENR-EMPLOYE
           WRITE ENR-EMPLOYE

           CLOSE F-EMPLOYES
           DISPLAY '10 employes crees (7 actifs, 3 inactifs)'.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Filtrage et transformation
      *----------------------------------------------------------------*
       1000-PREPARER-DONNEES.
           DISPLAY ' '
           DISPLAY '--- INPUT PROCEDURE : Filtrage et calcul ---'
           OPEN INPUT F-EMPLOYES
           MOVE 'N' TO WS-FIN-LECTURE

           PERFORM UNTIL FIN-LECTURE
               READ F-EMPLOYES INTO WS-EMPLOYE
                   AT END
                       SET FIN-LECTURE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CPT-LUS
      *                Filtrer : ne garder que les actifs
                       IF WS-STATUT = 'A'
                           ADD 1 TO WS-CPT-FILTRES
      *                    Preparer l'enregistrement pour le tri
                           MOVE WS-MATRICULE TO TRI-MATRICULE
                           MOVE WS-NOM TO TRI-NOM
                           MOVE WS-DEPT TO TRI-DEPT
                           MOVE WS-SALAIRE TO TRI-SALAIRE
                           MOVE WS-STATUT TO TRI-STATUT
      *                    Calculer le salaire annuel (x12)
                           COMPUTE TRI-SAL-ANNUEL =
                               WS-SALAIRE * 12
      *                    Envoyer au tri
                           RELEASE ENR-TRI
                           DISPLAY 'RELEASE : ' WS-NOM
                                   ' - Sal.ann=' TRI-SAL-ANNUEL
                       ELSE
                           DISPLAY 'IGNORE  : ' WS-NOM ' (inactif)'
                       END-IF
               END-READ
           END-PERFORM

           CLOSE F-EMPLOYES
           DISPLAY ' '
           DISPLAY 'Employes lus     : ' WS-CPT-LUS
           DISPLAY 'Employes filtres : ' WS-CPT-FILTRES.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Generation du rapport
      *----------------------------------------------------------------*
       2000-GENERER-RAPPORT.
           DISPLAY ' '
           DISPLAY '--- OUTPUT PROCEDURE : Generation rapport ---'

           OPEN OUTPUT F-RAPPORT

      *    En-tete du rapport
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-SEPARATEUR
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-TITRE
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-SEPARATEUR

           MOVE SPACES TO WS-DEPT-PRECEDENT
           MOVE 0 TO WS-SOUS-TOTAL
           MOVE 0 TO WS-TOTAL-GENERAL
           MOVE 0 TO WS-CPT-TOTAL
           MOVE 'N' TO WS-FIN-TRI

           PERFORM UNTIL FIN-TRI
               RETURN F-TRI
                   AT END
                       SET FIN-TRI TO TRUE
                       IF WS-DEPT-PRECEDENT NOT = SPACES
                           PERFORM 2200-ECRIRE-SOUS-TOTAL
                       END-IF
                   NOT AT END
      *                Rupture de departement
                       IF TRI-DEPT NOT = WS-DEPT-PRECEDENT
                           IF WS-DEPT-PRECEDENT NOT = SPACES
                               PERFORM 2200-ECRIRE-SOUS-TOTAL
                           END-IF
                           PERFORM 2100-ECRIRE-EN-TETE-DEPT
                       END-IF
                       PERFORM 2300-ECRIRE-DETAIL
               END-RETURN
           END-PERFORM

      *    Total general
           PERFORM 2400-ECRIRE-TOTAL-GENERAL

           CLOSE F-RAPPORT.

      *----------------------------------------------------------------*
       2100-ECRIRE-EN-TETE-DEPT.
           MOVE TRI-DEPT TO WS-DEPT-PRECEDENT
           MOVE TRI-DEPT TO WS-LD-DEPT
           MOVE 0 TO WS-SOUS-TOTAL
           MOVE 0 TO WS-CPT-DEPT
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-VIDE
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-DEPT.

      *----------------------------------------------------------------*
       2200-ECRIRE-SOUS-TOTAL.
           MOVE WS-SOUS-TOTAL TO WS-ST-MONTANT
           MOVE WS-CPT-DEPT TO WS-ST-CPT
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-SOUS-TOTAL
           ADD WS-SOUS-TOTAL TO WS-TOTAL-GENERAL.

      *----------------------------------------------------------------*
       2300-ECRIRE-DETAIL.
           MOVE TRI-MATRICULE TO WS-DET-MAT
           MOVE TRI-NOM TO WS-DET-NOM
           MOVE TRI-SALAIRE TO WS-DET-SAL-MENS
           MOVE TRI-SAL-ANNUEL TO WS-DET-SAL-ANN
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-DETAIL
           ADD TRI-SAL-ANNUEL TO WS-SOUS-TOTAL
           ADD 1 TO WS-CPT-DEPT
           ADD 1 TO WS-CPT-TOTAL.

      *----------------------------------------------------------------*
       2400-ECRIRE-TOTAL-GENERAL.
           MOVE WS-TOTAL-GENERAL TO WS-TG-MONTANT
           MOVE WS-CPT-TOTAL TO WS-TG-CPT
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-VIDE
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-SEPARATEUR
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-TOTAL
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-SEPARATEUR.

