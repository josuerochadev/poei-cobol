       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12RAPPORT.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-RAPPORT
      * OBJET     : Edition d'un rapport avec sauts de page
      *             - WRITE AFTER ADVANCING
      *             - Gestion des lignes par page
      *             - En-tete et pied de page
      * EXERCICE  : Chapitre XII - Fichier d'impression
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-RAPPORT
               ASSIGN TO 'RAPPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-RAPPORT.
       01  ENR-RAPPORT             PIC X(80).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Constantes
      *----------------------------------------------------------------*
       01  WS-LIGNES-PAR-PAGE      PIC 99 VALUE 20.

      *----------------------------------------------------------------*
      * Variables de travail
      *----------------------------------------------------------------*
       01  WS-STATUS               PIC XX.
       01  WS-NUM-PAGE             PIC 999 VALUE 0.
       01  WS-NUM-LIGNE            PIC 99 VALUE 99.
       01  WS-TOTAL-LIGNES         PIC 999 VALUE 0.
       01  WS-I                    PIC 99.
       01  WS-MONTANT              PIC 9(6)V99.

      *----------------------------------------------------------------*
      * Table des employes
      *----------------------------------------------------------------*
       01  WS-NB-EMPLOYES          PIC 99 VALUE 25.
       01  WS-TABLE-EMPLOYES.
           05  WS-EMPLOYE OCCURS 25 TIMES.
               10  WS-EMP-MATRICULE    PIC 9(5).
               10  WS-EMP-NOM          PIC X(20).
               10  WS-EMP-SERVICE      PIC X(15).
               10  WS-EMP-SALAIRE      PIC 9(5)V99.

      *----------------------------------------------------------------*
      * Zones d'edition
      *----------------------------------------------------------------*
       01  WS-LIGNE-VIDE           PIC X(80) VALUE SPACES.

       01  WS-EN-TETE-1.
           05  FILLER              PIC X(25) VALUE SPACES.
           05  FILLER              PIC X(30) VALUE
               'LISTE DES EMPLOYES'.
           05  FILLER              PIC X(15) VALUE 'Page : '.
           05  WS-ED-PAGE          PIC ZZ9.
           05  FILLER              PIC X(7) VALUE SPACES.

       01  WS-EN-TETE-2.
           05  FILLER              PIC X(80) VALUE ALL '='.

       01  WS-EN-TETE-COL.
           05  FILLER              PIC X(8) VALUE 'MATR.'.
           05  FILLER              PIC X(22) VALUE 'NOM'.
           05  FILLER              PIC X(17) VALUE 'SERVICE'.
           05  FILLER              PIC X(15) VALUE 'SALAIRE'.
           05  FILLER              PIC X(18) VALUE SPACES.

       01  WS-LIGNE-TIRET.
           05  FILLER              PIC X(80) VALUE ALL '-'.

       01  WS-LIGNE-DETAIL.
           05  WS-ED-MATR          PIC ZZZZZ.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-ED-NOM           PIC X(20).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-ED-SERVICE       PIC X(15).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-ED-SALAIRE       PIC ZZ.ZZ9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  FILLER              PIC X(15) VALUE SPACES.

       01  WS-LIGNE-TOTAL.
           05  FILLER              PIC X(45) VALUE SPACES.
           05  FILLER              PIC X(17) VALUE 'TOTAL EMPLOYES :'.
           05  WS-ED-TOTAL         PIC ZZ9.
           05  FILLER              PIC X(13) VALUE SPACES.

       01  WS-PIED-PAGE.
           05  FILLER              PIC X(30) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE '--- Fin de page ---'.
           05  FILLER              PIC X(30) VALUE SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EDITION RAPPORT AVEC SAUTS DE PAGE              '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INITIALISER
           PERFORM 2000-OUVRIR-FICHIER
           PERFORM 3000-EDITER-EMPLOYES
           PERFORM 4000-ECRIRE-TOTAL
           PERFORM 5000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY 'Rapport genere : RAPPORT.TXT'
           DISPLAY 'Nombre de pages : ' WS-NUM-PAGE
           DISPLAY 'Total employes  : ' WS-TOTAL-LIGNES
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Initialisation - Chargement des employes
      *----------------------------------------------------------------*
       1000-INITIALISER.
           MOVE 10001 TO WS-EMP-MATRICULE(1)
           MOVE 'DUPONT Jean' TO WS-EMP-NOM(1)
           MOVE 'Comptabilite' TO WS-EMP-SERVICE(1)
           MOVE 2850.00 TO WS-EMP-SALAIRE(1)

           MOVE 10002 TO WS-EMP-MATRICULE(2)
           MOVE 'MARTIN Marie' TO WS-EMP-NOM(2)
           MOVE 'Informatique' TO WS-EMP-SERVICE(2)
           MOVE 3200.00 TO WS-EMP-SALAIRE(2)

           MOVE 10003 TO WS-EMP-MATRICULE(3)
           MOVE 'BERNARD Paul' TO WS-EMP-NOM(3)
           MOVE 'Commercial' TO WS-EMP-SERVICE(3)
           MOVE 2650.00 TO WS-EMP-SALAIRE(3)

           MOVE 10004 TO WS-EMP-MATRICULE(4)
           MOVE 'PETIT Sophie' TO WS-EMP-NOM(4)
           MOVE 'RH' TO WS-EMP-SERVICE(4)
           MOVE 2900.00 TO WS-EMP-SALAIRE(4)

           MOVE 10005 TO WS-EMP-MATRICULE(5)
           MOVE 'ROBERT Pierre' TO WS-EMP-NOM(5)
           MOVE 'Informatique' TO WS-EMP-SERVICE(5)
           MOVE 3100.00 TO WS-EMP-SALAIRE(5)

           MOVE 10006 TO WS-EMP-MATRICULE(6)
           MOVE 'RICHARD Anne' TO WS-EMP-NOM(6)
           MOVE 'Comptabilite' TO WS-EMP-SERVICE(6)
           MOVE 2750.00 TO WS-EMP-SALAIRE(6)

           MOVE 10007 TO WS-EMP-MATRICULE(7)
           MOVE 'DURAND Michel' TO WS-EMP-NOM(7)
           MOVE 'Direction' TO WS-EMP-SERVICE(7)
           MOVE 4500.00 TO WS-EMP-SALAIRE(7)

           MOVE 10008 TO WS-EMP-MATRICULE(8)
           MOVE 'LEROY Claire' TO WS-EMP-NOM(8)
           MOVE 'Commercial' TO WS-EMP-SERVICE(8)
           MOVE 2700.00 TO WS-EMP-SALAIRE(8)

           MOVE 10009 TO WS-EMP-MATRICULE(9)
           MOVE 'MOREAU Jacques' TO WS-EMP-NOM(9)
           MOVE 'Logistique' TO WS-EMP-SERVICE(9)
           MOVE 2400.00 TO WS-EMP-SALAIRE(9)

           MOVE 10010 TO WS-EMP-MATRICULE(10)
           MOVE 'SIMON Nathalie' TO WS-EMP-NOM(10)
           MOVE 'RH' TO WS-EMP-SERVICE(10)
           MOVE 2950.00 TO WS-EMP-SALAIRE(10)

           MOVE 10011 TO WS-EMP-MATRICULE(11)
           MOVE 'LAURENT Luc' TO WS-EMP-NOM(11)
           MOVE 'Informatique' TO WS-EMP-SERVICE(11)
           MOVE 3050.00 TO WS-EMP-SALAIRE(11)

           MOVE 10012 TO WS-EMP-MATRICULE(12)
           MOVE 'LEFEBVRE Emma' TO WS-EMP-NOM(12)
           MOVE 'Commercial' TO WS-EMP-SERVICE(12)
           MOVE 2600.00 TO WS-EMP-SALAIRE(12)

           MOVE 10013 TO WS-EMP-MATRICULE(13)
           MOVE 'MICHEL Thomas' TO WS-EMP-NOM(13)
           MOVE 'Comptabilite' TO WS-EMP-SERVICE(13)
           MOVE 2800.00 TO WS-EMP-SALAIRE(13)

           MOVE 10014 TO WS-EMP-MATRICULE(14)
           MOVE 'GARCIA Julie' TO WS-EMP-NOM(14)
           MOVE 'Logistique' TO WS-EMP-SERVICE(14)
           MOVE 2450.00 TO WS-EMP-SALAIRE(14)

           MOVE 10015 TO WS-EMP-MATRICULE(15)
           MOVE 'DAVID Antoine' TO WS-EMP-NOM(15)
           MOVE 'Direction' TO WS-EMP-SERVICE(15)
           MOVE 4200.00 TO WS-EMP-SALAIRE(15)

           MOVE 10016 TO WS-EMP-MATRICULE(16)
           MOVE 'ROUX Caroline' TO WS-EMP-NOM(16)
           MOVE 'RH' TO WS-EMP-SERVICE(16)
           MOVE 2850.00 TO WS-EMP-SALAIRE(16)

           MOVE 10017 TO WS-EMP-MATRICULE(17)
           MOVE 'VINCENT Hugo' TO WS-EMP-NOM(17)
           MOVE 'Informatique' TO WS-EMP-SERVICE(17)
           MOVE 3150.00 TO WS-EMP-SALAIRE(17)

           MOVE 10018 TO WS-EMP-MATRICULE(18)
           MOVE 'FOURNIER Lea' TO WS-EMP-NOM(18)
           MOVE 'Commercial' TO WS-EMP-SERVICE(18)
           MOVE 2550.00 TO WS-EMP-SALAIRE(18)

           MOVE 10019 TO WS-EMP-MATRICULE(19)
           MOVE 'MOREL Nicolas' TO WS-EMP-NOM(19)
           MOVE 'Logistique' TO WS-EMP-SERVICE(19)
           MOVE 2500.00 TO WS-EMP-SALAIRE(19)

           MOVE 10020 TO WS-EMP-MATRICULE(20)
           MOVE 'GIRARD Camille' TO WS-EMP-NOM(20)
           MOVE 'Comptabilite' TO WS-EMP-SERVICE(20)
           MOVE 2700.00 TO WS-EMP-SALAIRE(20)

           MOVE 10021 TO WS-EMP-MATRICULE(21)
           MOVE 'ANDRE Marc' TO WS-EMP-NOM(21)
           MOVE 'Direction' TO WS-EMP-SERVICE(21)
           MOVE 4000.00 TO WS-EMP-SALAIRE(21)

           MOVE 10022 TO WS-EMP-MATRICULE(22)
           MOVE 'MERCIER Laura' TO WS-EMP-NOM(22)
           MOVE 'RH' TO WS-EMP-SERVICE(22)
           MOVE 2900.00 TO WS-EMP-SALAIRE(22)

           MOVE 10023 TO WS-EMP-MATRICULE(23)
           MOVE 'BLANC Olivier' TO WS-EMP-NOM(23)
           MOVE 'Informatique' TO WS-EMP-SERVICE(23)
           MOVE 3250.00 TO WS-EMP-SALAIRE(23)

           MOVE 10024 TO WS-EMP-MATRICULE(24)
           MOVE 'GUERIN Sarah' TO WS-EMP-NOM(24)
           MOVE 'Commercial' TO WS-EMP-SERVICE(24)
           MOVE 2650.00 TO WS-EMP-SALAIRE(24)

           MOVE 10025 TO WS-EMP-MATRICULE(25)
           MOVE 'BONNET Maxime' TO WS-EMP-NOM(25)
           MOVE 'Logistique' TO WS-EMP-SERVICE(25)
           MOVE 2350.00 TO WS-EMP-SALAIRE(25).

      *----------------------------------------------------------------*
      * Ouverture fichier
      *----------------------------------------------------------------*
       2000-OUVRIR-FICHIER.
           OPEN OUTPUT F-RAPPORT
           IF WS-STATUS NOT = '00'
               DISPLAY 'ERREUR OUVERTURE : ' WS-STATUS
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
      * Edition des employes avec saut de page
      *----------------------------------------------------------------*
       3000-EDITER-EMPLOYES.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-NB-EMPLOYES
      *        Verifier si nouvelle page necessaire
               IF WS-NUM-LIGNE >= WS-LIGNES-PAR-PAGE
                   PERFORM 3100-SAUT-PAGE
               END-IF
      *        Ecrire la ligne de detail
               MOVE WS-EMP-MATRICULE(WS-I) TO WS-ED-MATR
               MOVE WS-EMP-NOM(WS-I) TO WS-ED-NOM
               MOVE WS-EMP-SERVICE(WS-I) TO WS-ED-SERVICE
               MOVE WS-EMP-SALAIRE(WS-I) TO WS-ED-SALAIRE
               WRITE ENR-RAPPORT FROM WS-LIGNE-DETAIL
               ADD 1 TO WS-NUM-LIGNE
               ADD 1 TO WS-TOTAL-LIGNES
               DISPLAY WS-LIGNE-DETAIL
           END-PERFORM.

      *----------------------------------------------------------------*
      * Saut de page avec en-tete
      *----------------------------------------------------------------*
       3100-SAUT-PAGE.
      *    Ecrire pied de page si ce n'est pas la premiere page
           IF WS-NUM-PAGE > 0
               WRITE ENR-RAPPORT FROM WS-LIGNE-VIDE
               WRITE ENR-RAPPORT FROM WS-PIED-PAGE
               WRITE ENR-RAPPORT FROM WS-LIGNE-VIDE
               WRITE ENR-RAPPORT FROM WS-LIGNE-VIDE
           END-IF

           ADD 1 TO WS-NUM-PAGE
           MOVE WS-NUM-PAGE TO WS-ED-PAGE

      *    Ecrire l'en-tete de la nouvelle page
           WRITE ENR-RAPPORT FROM WS-EN-TETE-1
           WRITE ENR-RAPPORT FROM WS-EN-TETE-2
           WRITE ENR-RAPPORT FROM WS-LIGNE-VIDE
           WRITE ENR-RAPPORT FROM WS-EN-TETE-COL
           WRITE ENR-RAPPORT FROM WS-LIGNE-TIRET

           MOVE 0 TO WS-NUM-LIGNE.

      *----------------------------------------------------------------*
      * Ecrire le total
      *----------------------------------------------------------------*
       4000-ECRIRE-TOTAL.
           WRITE ENR-RAPPORT FROM WS-LIGNE-TIRET
           MOVE WS-TOTAL-LIGNES TO WS-ED-TOTAL
           WRITE ENR-RAPPORT FROM WS-LIGNE-TOTAL
           DISPLAY WS-LIGNE-TOTAL.

      *----------------------------------------------------------------*
      * Fermeture fichier
      *----------------------------------------------------------------*
       5000-FERMER-FICHIER.
           WRITE ENR-RAPPORT FROM WS-LIGNE-VIDE
           WRITE ENR-RAPPORT FROM WS-PIED-PAGE
           CLOSE F-RAPPORT.

