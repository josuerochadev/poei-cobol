       IDENTIFICATION DIVISION.
       PROGRAM-ID. C11SORTIN.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C11-SORT-INPUT
      * OBJET     : Demonstration INPUT PROCEDURE avec RELEASE
      *             Filtrage des employes actifs avant tri
      * EXERCICE  : Chapitre XI - Tri interne
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ENTREE
               ASSIGN TO 'EMPLOYES-MIXTE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

           SELECT F-TRI
               ASSIGN TO 'SORT-WORK.TMP'.

           SELECT F-SORTIE
               ASSIGN TO 'EMPLOYES-ACTIFS.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-S.

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

       FD  F-SORTIE
           RECORDING MODE IS F
           RECORD CONTAINS 50 CHARACTERS.
       01  ENR-SORTIE              PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-E             PIC XX.
       01  WS-STATUS-S             PIC XX.

       01  WS-FIN-LECTURE          PIC X VALUE 'N'.
           88 FIN-LECTURE          VALUE 'O'.

       01  WS-EMPLOYE.
           05  WS-MATRICULE        PIC 9(6).
           05  WS-NOM              PIC X(20).
           05  WS-DEPT             PIC X(10).
           05  WS-SALAIRE          PIC 9(6).
           05  WS-STATUT           PIC X.
           05  FILLER              PIC X(7).

       01  WS-CPT-LUS              PIC 9(3) VALUE 0.
       01  WS-CPT-FILTRES          PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  DEMONSTRATION INPUT PROCEDURE (RELEASE)         '
           DISPLAY '  Filtrage : ne garder que les employes actifs    '
           DISPLAY '=================================================='
           DISPLAY ' '

      *    Creer le fichier avec employes actifs et inactifs
           PERFORM 1000-CREER-FICHIER-MIXTE

      *    Tri avec INPUT PROCEDURE pour filtrer
           DISPLAY ' '
           DISPLAY '--- Tri avec filtrage (STATUT = A) ---'

           SORT F-TRI
               ON ASCENDING KEY TRI-NOM
               INPUT PROCEDURE IS 2000-FILTRER-ACTIFS
               GIVING F-SORTIE

           DISPLAY ' '
           DISPLAY 'Employes lus       : ' WS-CPT-LUS
           DISPLAY 'Employes filtres   : ' WS-CPT-FILTRES

      *    Afficher le resultat
           PERFORM 3000-AFFICHER-RESULTAT

           DISPLAY ' '
           DISPLAY 'Fin du programme C11-SORT-INPUT'
           STOP RUN.

      *----------------------------------------------------------------*
      * Creation fichier avec employes actifs (A) et inactifs (I)
      *----------------------------------------------------------------*
       1000-CREER-FICHIER-MIXTE.
           OPEN OUTPUT F-ENTREE

           MOVE '000001DUPONT              INFO      002500A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000002MARTIN              RH        003000I       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000003BERNARD             COMPTA    002800A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000004PETIT               INFO      003200I       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000005DURAND              RH        002600A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000006LEROY               INFO      002900I       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           MOVE '000007MOREAU              COMPTA    003100A       '
               TO ENR-ENTREE
           WRITE ENR-ENTREE

           CLOSE F-ENTREE
           DISPLAY '7 employes crees (4 actifs, 3 inactifs)'.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Filtrer les employes actifs
      * Utilise RELEASE pour envoyer au tri
      *----------------------------------------------------------------*
       2000-FILTRER-ACTIFS.
           OPEN INPUT F-ENTREE
           MOVE 'N' TO WS-FIN-LECTURE

           PERFORM UNTIL FIN-LECTURE
               READ F-ENTREE INTO WS-EMPLOYE
                   AT END
                       SET FIN-LECTURE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CPT-LUS
      *                Filtrer : ne garder que statut 'A' (actif)
                       IF WS-STATUT = 'A'
                           ADD 1 TO WS-CPT-FILTRES
      *                    RELEASE envoie l'enregistrement au tri
                           MOVE WS-EMPLOYE TO ENR-TRI
                           RELEASE ENR-TRI
                           DISPLAY 'RELEASE : ' WS-NOM ' (actif)'
                       ELSE
                           DISPLAY 'IGNORE  : ' WS-NOM ' (inactif)'
                       END-IF
               END-READ
           END-PERFORM

           CLOSE F-ENTREE.

      *----------------------------------------------------------------*
      * Afficher le resultat trie
      *----------------------------------------------------------------*
       3000-AFFICHER-RESULTAT.
           DISPLAY ' '
           DISPLAY '--- Resultat : employes actifs tries par nom ---'
           OPEN INPUT F-SORTIE
           PERFORM UNTIL WS-STATUS-S = '10'
               READ F-SORTIE INTO ENR-TRI
                   AT END CONTINUE
                   NOT AT END
                       DISPLAY TRI-NOM ' - ' TRI-DEPT ' - ' TRI-SALAIRE
               END-READ
           END-PERFORM
           CLOSE F-SORTIE.

