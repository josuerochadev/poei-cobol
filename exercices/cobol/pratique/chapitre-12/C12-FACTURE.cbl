       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12FACT.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-FACTURE
      * OBJET     : TP Complet - Edition d'une facture
      *             Demonstration de toutes les techniques d'edition
      *             - En-tete avec date formatee
      *             - Lignes de detail avec montants
      *             - Totaux avec symbole monetaire
      * EXERCICE  : Chapitre XII - Fichier d'impression
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-FACTURE
               ASSIGN TO 'FACTURE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-FACTURE.
       01  ENR-FACTURE             PIC X(80).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Variables de travail
      *----------------------------------------------------------------*
       01  WS-STATUS               PIC XX.
       01  WS-DATE-SYSTEM.
           05  WS-ANNEE            PIC 9(4).
           05  WS-MOIS             PIC 99.
           05  WS-JOUR             PIC 99.

      *----------------------------------------------------------------*
      * Donnees de la facture
      *----------------------------------------------------------------*
       01  WS-NUM-FACTURE          PIC 9(6) VALUE 123456.
       01  WS-NUM-CLIENT           PIC 9(8) VALUE 00012345.

       01  WS-LIGNES-FACTURE.
           05  WS-LIGNE OCCURS 5 TIMES.
               10  WS-DESIGNATION  PIC X(25).
               10  WS-QTE          PIC 9(3).
               10  WS-PU           PIC 9(5)V99.
               10  WS-MONTANT-LIG  PIC 9(7)V99.

       01  WS-TOTAL-HT             PIC 9(8)V99 VALUE 0.
       01  WS-TVA                  PIC 9(7)V99.
       01  WS-TOTAL-TTC            PIC 9(8)V99.
       01  WS-TAUX-TVA             PIC V99 VALUE 0.20.

       01  WS-I                    PIC 9.

      *----------------------------------------------------------------*
      * Zones d'edition
      *----------------------------------------------------------------*
       01  WS-LIGNE-VIDE           PIC X(80) VALUE SPACES.

       01  WS-EN-TETE-1.
           05  FILLER              PIC X(30) VALUE SPACES.
           05  FILLER              PIC X(20) VALUE 'F A C T U R E'.
           05  FILLER              PIC X(30) VALUE SPACES.

       01  WS-EN-TETE-2.
           05  FILLER              PIC X(60) VALUE ALL '='.
           05  FILLER              PIC X(20) VALUE SPACES.

       01  WS-LIGNE-DATE.
           05  FILLER              PIC X(10) VALUE 'Date : '.
           05  WS-ED-DATE          PIC 99/99/9999.
           05  FILLER              PIC X(20) VALUE SPACES.
           05  FILLER              PIC X(12) VALUE 'Facture N° '.
           05  WS-ED-NUM-FACT      PIC ZZZ.ZZ9.
           05  FILLER              PIC X(21) VALUE SPACES.

       01  WS-LIGNE-CLIENT.
           05  FILLER              PIC X(12) VALUE 'Client N° : '.
           05  WS-ED-NUM-CLI       PIC 99.999.999.
           05  FILLER              PIC X(57) VALUE SPACES.

       01  WS-LIGNE-TIRET.
           05  FILLER              PIC X(80) VALUE ALL '-'.

       01  WS-LIGNE-ENTETE-DET.
           05  FILLER              PIC X(25) VALUE 'DESIGNATION'.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  FILLER              PIC X(5) VALUE 'QTE'.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  FILLER              PIC X(12) VALUE 'P.U.'.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  FILLER              PIC X(15) VALUE 'MONTANT'.
           05  FILLER              PIC X(14) VALUE SPACES.

       01  WS-LIGNE-DETAIL.
           05  WS-ED-DESIGN        PIC X(25).
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-ED-QTE           PIC ZZ9.
           05  FILLER              PIC X(5) VALUE SPACES.
           05  WS-ED-PU            PIC ZZ.ZZ9,99.
           05  FILLER              PIC X(3) VALUE SPACES.
           05  WS-ED-MONTANT       PIC ZZZ.ZZ9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  FILLER              PIC X(14) VALUE SPACES.

       01  WS-LIGNE-TOTAL-HT.
           05  FILLER              PIC X(40) VALUE SPACES.
           05  FILLER              PIC X(15) VALUE 'TOTAL HT :'.
           05  WS-ED-TOTAL-HT      PIC ZZZ.ZZ9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  FILLER              PIC X(9) VALUE SPACES.

       01  WS-LIGNE-TVA.
           05  FILLER              PIC X(40) VALUE SPACES.
           05  FILLER              PIC X(15) VALUE 'TVA (20%) :'.
           05  WS-ED-TVA           PIC ZZZ.ZZ9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  FILLER              PIC X(9) VALUE SPACES.

       01  WS-LIGNE-TOTAL-TTC.
           05  FILLER              PIC X(40) VALUE SPACES.
           05  FILLER              PIC X(15) VALUE 'TOTAL TTC :'.
           05  WS-ED-TOTAL-TTC     PIC ***.**9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  FILLER              PIC X(9) VALUE SPACES.

       01  WS-LIGNE-PIED.
           05  FILLER              PIC X(20) VALUE SPACES.
           05  FILLER              PIC X(40)
               VALUE 'Merci de votre confiance !'.
           05  FILLER              PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  TP COMPLET : EDITION FACTURE                    '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INITIALISER
           PERFORM 2000-OUVRIR-FICHIER
           PERFORM 3000-ECRIRE-EN-TETE
           PERFORM 4000-ECRIRE-DETAILS
           PERFORM 5000-ECRIRE-TOTAUX
           PERFORM 6000-ECRIRE-PIED
           PERFORM 7000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY 'Facture generee : FACTURE.TXT'
           DISPLAY 'Fin du programme'
           STOP RUN.

      *----------------------------------------------------------------*
      * Initialisation des donnees
      *----------------------------------------------------------------*
       1000-INITIALISER.
      *    Recuperer la date systeme
           MOVE FUNCTION CURRENT-DATE TO WS-DATE-SYSTEM

      *    Initialiser les lignes de facture
           MOVE 'Ordinateur portable' TO WS-DESIGNATION(1)
           MOVE 2 TO WS-QTE(1)
           MOVE 899.99 TO WS-PU(1)

           MOVE 'Souris sans fil' TO WS-DESIGNATION(2)
           MOVE 5 TO WS-QTE(2)
           MOVE 29.90 TO WS-PU(2)

           MOVE 'Clavier mecanique' TO WS-DESIGNATION(3)
           MOVE 3 TO WS-QTE(3)
           MOVE 79.99 TO WS-PU(3)

           MOVE 'Ecran 27 pouces' TO WS-DESIGNATION(4)
           MOVE 2 TO WS-QTE(4)
           MOVE 349.00 TO WS-PU(4)

           MOVE 'Cable HDMI 2m' TO WS-DESIGNATION(5)
           MOVE 10 TO WS-QTE(5)
           MOVE 12.50 TO WS-PU(5)

      *    Calculer les montants
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               COMPUTE WS-MONTANT-LIG(WS-I) =
                   WS-QTE(WS-I) * WS-PU(WS-I)
               ADD WS-MONTANT-LIG(WS-I) TO WS-TOTAL-HT
           END-PERFORM

           COMPUTE WS-TVA = WS-TOTAL-HT * WS-TAUX-TVA
           COMPUTE WS-TOTAL-TTC = WS-TOTAL-HT + WS-TVA.

      *----------------------------------------------------------------*
      * Ouverture fichier
      *----------------------------------------------------------------*
       2000-OUVRIR-FICHIER.
           OPEN OUTPUT F-FACTURE
           IF WS-STATUS NOT = '00'
               DISPLAY 'ERREUR OUVERTURE FICHIER : ' WS-STATUS
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
      * Ecrire l'en-tete
      *----------------------------------------------------------------*
       3000-ECRIRE-EN-TETE.
           WRITE ENR-FACTURE FROM WS-LIGNE-VIDE
           WRITE ENR-FACTURE FROM WS-EN-TETE-1
           WRITE ENR-FACTURE FROM WS-EN-TETE-2
           WRITE ENR-FACTURE FROM WS-LIGNE-VIDE

      *    Formater la date JJ/MM/AAAA
           STRING WS-JOUR WS-MOIS WS-ANNEE DELIMITED SIZE
               INTO WS-ED-DATE
           END-STRING
           MOVE WS-NUM-FACTURE TO WS-ED-NUM-FACT
           WRITE ENR-FACTURE FROM WS-LIGNE-DATE

           MOVE WS-NUM-CLIENT TO WS-ED-NUM-CLI
           WRITE ENR-FACTURE FROM WS-LIGNE-CLIENT

           WRITE ENR-FACTURE FROM WS-LIGNE-VIDE
           WRITE ENR-FACTURE FROM WS-LIGNE-TIRET
           WRITE ENR-FACTURE FROM WS-LIGNE-ENTETE-DET
           WRITE ENR-FACTURE FROM WS-LIGNE-TIRET.

      *----------------------------------------------------------------*
      * Ecrire les lignes de detail
      *----------------------------------------------------------------*
       4000-ECRIRE-DETAILS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE WS-DESIGNATION(WS-I) TO WS-ED-DESIGN
               MOVE WS-QTE(WS-I) TO WS-ED-QTE
               MOVE WS-PU(WS-I) TO WS-ED-PU
               MOVE WS-MONTANT-LIG(WS-I) TO WS-ED-MONTANT
               WRITE ENR-FACTURE FROM WS-LIGNE-DETAIL
               DISPLAY WS-LIGNE-DETAIL
           END-PERFORM.

      *----------------------------------------------------------------*
      * Ecrire les totaux
      *----------------------------------------------------------------*
       5000-ECRIRE-TOTAUX.
           WRITE ENR-FACTURE FROM WS-LIGNE-TIRET

           MOVE WS-TOTAL-HT TO WS-ED-TOTAL-HT
           WRITE ENR-FACTURE FROM WS-LIGNE-TOTAL-HT
           DISPLAY WS-LIGNE-TOTAL-HT

           MOVE WS-TVA TO WS-ED-TVA
           WRITE ENR-FACTURE FROM WS-LIGNE-TVA
           DISPLAY WS-LIGNE-TVA

           WRITE ENR-FACTURE FROM WS-LIGNE-TIRET

           MOVE WS-TOTAL-TTC TO WS-ED-TOTAL-TTC
           WRITE ENR-FACTURE FROM WS-LIGNE-TOTAL-TTC
           DISPLAY WS-LIGNE-TOTAL-TTC.

      *----------------------------------------------------------------*
      * Ecrire le pied de page
      *----------------------------------------------------------------*
       6000-ECRIRE-PIED.
           WRITE ENR-FACTURE FROM WS-LIGNE-VIDE
           WRITE ENR-FACTURE FROM WS-EN-TETE-2
           WRITE ENR-FACTURE FROM WS-LIGNE-PIED.

      *----------------------------------------------------------------*
      * Fermeture fichier
      *----------------------------------------------------------------*
       7000-FERMER-FICHIER.
           CLOSE F-FACTURE.

