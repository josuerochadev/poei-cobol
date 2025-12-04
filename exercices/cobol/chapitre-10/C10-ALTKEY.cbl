       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10ALTKEY.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-ALTKEY
      * OBJET     : Démonstration ALTERNATE RECORD KEY
      *             Accès par clé primaire et clé secondaire
      * EXERCICE  : Chapitre X - Traitement des fichiers
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * Fichier INDEXED avec clé primaire et secondaire
      *----------------------------------------------------------------*
           SELECT F-EMPLOYES
               ASSIGN TO 'EMPLOYES.IDX'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-MATRICULE
               ALTERNATE RECORD KEY IS EMP-NOM WITH DUPLICATES
               ALTERNATE RECORD KEY IS EMP-DEPT WITH DUPLICATES
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Structure fichier EMPLOYES
      *----------------------------------------------------------------*
       FD  F-EMPLOYES
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  ENR-EMPLOYE.
           05  EMP-MATRICULE      PIC 9(6).
           05  EMP-NOM            PIC X(25).
           05  EMP-PRENOM         PIC X(20).
           05  EMP-DEPT           PIC X(10).
           05  EMP-SALAIRE        PIC 9(6)V99.
           05  FILLER             PIC X(11).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01  WS-STATUS              PIC XX.
           88 WS-OK               VALUE '00'.
           88 WS-EOF              VALUE '10'.
           88 WS-NOT-FOUND        VALUE '23'.

      *----------------------------------------------------------------*
      * Variables de contrôle
      *----------------------------------------------------------------*
       01  WS-FIN-FICHIER         PIC X VALUE 'N'.
           88 FIN-FICHIER         VALUE 'O'.
           88 PAS-FIN             VALUE 'N'.

       01  WS-CHOIX               PIC 9 VALUE 0.
       01  WS-RECHERCHE           PIC X(25).
       01  WS-COMPTEUR            PIC 9(3) VALUE 0.

      *----------------------------------------------------------------*
      * Données de test
      *----------------------------------------------------------------*
       01  WS-TEST-DATA.
           05  FILLER PIC X(80) VALUE
               '000001DUPONT                   JEAN                INFO
      -        '010000'.
           05  FILLER PIC X(80) VALUE
               '000002MARTIN                   MARIE               RH
      -        '008500'.
           05  FILLER PIC X(80) VALUE
               '000003DUPONT                   PIERRE              COMPT
      -        'A012000'.
           05  FILLER PIC X(80) VALUE
               '000004BERNARD                  SOPHIE              INFO
      -        '011500'.
           05  FILLER PIC X(80) VALUE
               '000005PETIT                    CLAUDE              RH
      -        '009000'.
           05  FILLER PIC X(80) VALUE
               '000006DURAND                   MARC                INFO
      -        '013000'.

       01  WS-TEST-TABLE REDEFINES WS-TEST-DATA.
           05  WS-TEST-ENR        PIC X(80) OCCURS 6.

       01  WS-IDX                 PIC 9 VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '     DEMONSTRATION ALTERNATE RECORD KEY           '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-CREER-FICHIER
           PERFORM 2000-MENU-RECHERCHE
           PERFORM 3000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY 'Fin du programme C10-ALTKEY'
           STOP RUN.

      *----------------------------------------------------------------*
      * Création du fichier avec données de test
      *----------------------------------------------------------------*
       1000-CREER-FICHIER.
           DISPLAY '--- Creation du fichier EMPLOYES ---'

           OPEN OUTPUT F-EMPLOYES
           IF NOT WS-OK
               DISPLAY 'Erreur creation fichier : ' WS-STATUS
               STOP RUN
           END-IF

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 6
               MOVE WS-TEST-ENR(WS-IDX) TO ENR-EMPLOYE
               WRITE ENR-EMPLOYE
                   INVALID KEY
                       DISPLAY 'Erreur ecriture : ' WS-STATUS
               END-WRITE
           END-PERFORM

           CLOSE F-EMPLOYES
           DISPLAY '6 employes inseres'
           DISPLAY ' '.

      *----------------------------------------------------------------*
      * Menu de recherche
      *----------------------------------------------------------------*
       2000-MENU-RECHERCHE.
           OPEN I-O F-EMPLOYES
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture : ' WS-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL WS-CHOIX = 9
               DISPLAY '--- MENU RECHERCHE ---'
               DISPLAY '1. Recherche par MATRICULE (cle primaire)'
               DISPLAY '2. Recherche par NOM (cle secondaire)'
               DISPLAY '3. Liste par DEPARTEMENT (cle secondaire)'
               DISPLAY '4. Liste complete par MATRICULE'
               DISPLAY '5. Liste complete par NOM'
               DISPLAY '9. Quitter'
               DISPLAY 'Choix : '
               ACCEPT WS-CHOIX

               EVALUATE WS-CHOIX
                   WHEN 1
                       PERFORM 2100-RECHERCHE-MATRICULE
                   WHEN 2
                       PERFORM 2200-RECHERCHE-NOM
                   WHEN 3
                       PERFORM 2300-LISTE-DEPARTEMENT
                   WHEN 4
                       PERFORM 2400-LISTE-MATRICULE
                   WHEN 5
                       PERFORM 2500-LISTE-NOM
                   WHEN 9
                       DISPLAY 'Au revoir'
                   WHEN OTHER
                       DISPLAY 'Choix invalide'
               END-EVALUATE
               DISPLAY ' '
           END-PERFORM.

      *----------------------------------------------------------------*
      * Recherche directe par matricule (clé primaire)
      *----------------------------------------------------------------*
       2100-RECHERCHE-MATRICULE.
           DISPLAY 'Matricule recherche (6 chiffres) : '
           ACCEPT EMP-MATRICULE

           READ F-EMPLOYES KEY IS EMP-MATRICULE
               INVALID KEY
                   DISPLAY 'Matricule non trouve'
               NOT INVALID KEY
                   PERFORM 2900-AFFICHER-EMPLOYE
           END-READ.

      *----------------------------------------------------------------*
      * Recherche directe par nom (clé secondaire)
      *----------------------------------------------------------------*
       2200-RECHERCHE-NOM.
           DISPLAY 'Nom recherche : '
           ACCEPT WS-RECHERCHE
           MOVE WS-RECHERCHE TO EMP-NOM

           READ F-EMPLOYES KEY IS EMP-NOM
               INVALID KEY
                   DISPLAY 'Nom non trouve'
               NOT INVALID KEY
                   DISPLAY 'Premier employe trouve :'
                   PERFORM 2900-AFFICHER-EMPLOYE
           END-READ.

      *----------------------------------------------------------------*
      * Liste par département (parcours clé secondaire)
      *----------------------------------------------------------------*
       2300-LISTE-DEPARTEMENT.
           DISPLAY 'Departement (INFO, RH, COMPTA) : '
           ACCEPT WS-RECHERCHE
           MOVE WS-RECHERCHE TO EMP-DEPT

           MOVE 0 TO WS-COMPTEUR
           SET PAS-FIN TO TRUE

      *    Positionnement sur la clé secondaire DEPT
           START F-EMPLOYES KEY = EMP-DEPT
               INVALID KEY
                   DISPLAY 'Departement non trouve'
                   SET FIN-FICHIER TO TRUE
           END-START

           PERFORM UNTIL FIN-FICHIER
               READ F-EMPLOYES NEXT
                   AT END
                       SET FIN-FICHIER TO TRUE
                   NOT AT END
      *                Arrêter si on change de département
                       IF EMP-DEPT NOT = WS-RECHERCHE
                           SET FIN-FICHIER TO TRUE
                       ELSE
                           ADD 1 TO WS-COMPTEUR
                           PERFORM 2900-AFFICHER-EMPLOYE
                       END-IF
               END-READ
           END-PERFORM

           DISPLAY 'Total : ' WS-COMPTEUR ' employe(s)'.

      *----------------------------------------------------------------*
      * Liste complète par matricule (ordre clé primaire)
      *----------------------------------------------------------------*
       2400-LISTE-MATRICULE.
           DISPLAY '--- Liste par MATRICULE ---'
           SET PAS-FIN TO TRUE
           MOVE 0 TO WS-COMPTEUR

      *    Positionnement au début (clé primaire)
           MOVE ZEROS TO EMP-MATRICULE
           START F-EMPLOYES KEY >= EMP-MATRICULE
               INVALID KEY SET FIN-FICHIER TO TRUE
           END-START

           PERFORM UNTIL FIN-FICHIER
               READ F-EMPLOYES NEXT
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       PERFORM 2900-AFFICHER-EMPLOYE
               END-READ
           END-PERFORM

           DISPLAY 'Total : ' WS-COMPTEUR ' employe(s)'.

      *----------------------------------------------------------------*
      * Liste complète par nom (ordre clé secondaire)
      *----------------------------------------------------------------*
       2500-LISTE-NOM.
           DISPLAY '--- Liste par NOM ---'
           SET PAS-FIN TO TRUE
           MOVE 0 TO WS-COMPTEUR

      *    Positionnement au début sur la clé secondaire NOM
           MOVE SPACES TO EMP-NOM
           START F-EMPLOYES KEY >= EMP-NOM
               INVALID KEY SET FIN-FICHIER TO TRUE
           END-START

           PERFORM UNTIL FIN-FICHIER
               READ F-EMPLOYES NEXT
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END
                       ADD 1 TO WS-COMPTEUR
                       PERFORM 2900-AFFICHER-EMPLOYE
               END-READ
           END-PERFORM

           DISPLAY 'Total : ' WS-COMPTEUR ' employe(s)'.

      *----------------------------------------------------------------*
      * Affichage d'un employé
      *----------------------------------------------------------------*
       2900-AFFICHER-EMPLOYE.
           DISPLAY EMP-MATRICULE ' | '
                   EMP-NOM ' | '
                   EMP-PRENOM ' | '
                   EMP-DEPT ' | '
                   EMP-SALAIRE.

      *----------------------------------------------------------------*
      * Fermeture
      *----------------------------------------------------------------*
       3000-FERMER-FICHIER.
           CLOSE F-EMPLOYES.

