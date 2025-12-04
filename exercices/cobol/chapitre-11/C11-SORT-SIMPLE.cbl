       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11SORTSM.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-SORT-SIMPLE
      * OBJET     : Demonstration du tri simple avec SORT
      *             USING / GIVING (sans procedure)
      * EXERCICE  : Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Fichier d'entree (non trie)
           SELECT F-ENTREE
               ASSIGN TO 'EMPLOYES-ENTREE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

      *    Fichier de travail pour le tri
           SELECT F-TRI
               ASSIGN TO 'SORT-WORK.TMP'.

      *    Fichier de sortie (trie)
           SELECT F-SORTIE
               ASSIGN TO 'EMPLOYES-SORTIE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-S.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Fichier d'entree
      *----------------------------------------------------------------*
       FD  F-ENTREE
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-ENTREE              PIC X(50).

      *----------------------------------------------------------------*
      * Fichier de travail SORT (SD au lieu de FD)
      *----------------------------------------------------------------*
       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-MATRICULE       PIC 9(6).
           05  TRI-NOM             PIC X(20).
           05  TRI-DEPT            PIC X(10).
           05  TRI-SALAIRE         PIC 9(6).
           05  TRI-STATUT          PIC X.
           05  FILLER              PIC X(7).

      *----------------------------------------------------------------*
      * Fichier de sortie
      *----------------------------------------------------------------*
       FD  F-SORTIE
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-SORTIE              PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-S             PIC XX.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  DEMONSTRATION SORT SIMPLE (USING / GIVING)      '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    D'abord creer le fichier d'entree
           PERFORM 1000-CREER-FICHIER-ENTREE
           PERFORM 1500-AFFICHER-AVANT-TRI

      *    Tri par nom croissant
           DISPLAY ' '
           DISPLAY '--- Tri par NOM (ASCENDING) ---'
           SORT F-TRI
               ON ASCENDING KEY TRI-NOM
               USING F-ENTREE
               GIVING F-SORTIE

           PERFORM 2000-AFFICHER-RESULTAT

      *    Tri par salaire decroissant
           DISPLAY ' '
           DISPLAY '--- Tri par SALAIRE (DESCENDING) ---'
           SORT F-TRI
               ON DESCENDING KEY TRI-SALAIRE
               USING F-ENTREE
               GIVING F-SORTIE

           PERFORM 2000-AFFICHER-RESULTAT

      *    Tri multi-cles : DEPT croissant, SALAIRE decroissant
           DISPLAY ' '
           DISPLAY '--- Tri DEPT asc, SALAIRE desc ---'
           SORT F-TRI
               ON ASCENDING KEY TRI-DEPT
               ON DESCENDING KEY TRI-SALAIRE
               USING F-ENTREE
               GIVING F-SORTIE

           PERFORM 2000-AFFICHER-RESULTAT

           DISPLAY ' '
           DISPLAY 'Fin du programme C11-SORT-SIMPLE'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation du fichier d'entree avec donnees de test
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
           DISPLAY '7 employes crees dans EMPLOYES-ENTREE.DAT'.

      *----------------------------------------------------------------*
      * Afficher le fichier avant tri
      *----------------------------------------------------------------*
       1500-AFFICHER-AVANT-TRI.
           DISPLAY ' '
           DISPLAY '--- Fichier AVANT tri ---'
           OPEN INPUT F-ENTREE
           PERFORM UNTIL WS-STATUS-E = '10'
               READ F-ENTREE INTO ENR-TRI
                   AT END CONTINUE
                   NOT AT END
                       DISPLAY TRI-MATRICULE ' '
                               TRI-NOM ' '
                               TRI-DEPT ' '
                               TRI-SALAIRE
               END-READ
           END-PERFORM
           CLOSE F-ENTREE.

      *----------------------------------------------------------------*
      * Afficher le fichier apres tri
      *----------------------------------------------------------------*
       2000-AFFICHER-RESULTAT.
           OPEN INPUT F-SORTIE
           PERFORM UNTIL WS-STATUS-S = '10'
               READ F-SORTIE INTO ENR-TRI
                   AT END CONTINUE
                   NOT AT END
                       DISPLAY TRI-MATRICULE ' '
                               TRI-NOM ' '
                               TRI-DEPT ' '
                               TRI-SALAIRE
               END-READ
           END-PERFORM
           CLOSE F-SORTIE.

