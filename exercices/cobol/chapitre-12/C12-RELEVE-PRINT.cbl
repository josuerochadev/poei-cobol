       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12RELPRT.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-RELEVE-PRINT
      * OBJET     : Exercice 06 - Impression du releve bancaire
      *             a partir du fichier AIMPRIM
      *
      * ENTREE    : AIMPRIM.DAT - Fichier preparation impression
      *
      * SORTIE    : RELEVE.TXT - Releve bancaire imprimable
      *
      * FORMAT PAGE (57 lignes max) :
      *   - En-tete    : 20 lignes
      *   - Corps      : 17 lignes (5 operations max, 3 lignes vides entre)
      *   - Pied page  : 20 lignes
      *
      * EXERCICE  : Chapitre XII - Fichier d'impression
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAIMPRIM ASSIGN TO 'AIMPRIM.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-AIMPRIM.

           SELECT FRELEVE ASSIGN TO 'RELEVE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-RELEVE.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Fichier AIMPRIM (entree)
      *----------------------------------------------------------------*
       FD  FAIMPRIM
           RECORD CONTAINS 150 CHARACTERS.
       01  ENR-AIMPRIM             PIC X(150).

      *----------------------------------------------------------------*
      * Fichier RELEVE (sortie impression)
      *----------------------------------------------------------------*
       FD  FRELEVE.
       01  ENR-RELEVE              PIC X(132).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Constantes
      *----------------------------------------------------------------*
       01  WS-LARGEUR-PAGE         PIC 999 VALUE 132.
       01  WS-LIGNES-ENTETE        PIC 99 VALUE 20.
       01  WS-LIGNES-CORPS         PIC 99 VALUE 17.
       01  WS-LIGNES-PIED          PIC 99 VALUE 20.
       01  WS-LIGNES-ENTRE-MVT     PIC 9 VALUE 3.

      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01  FS-AIMPRIM              PIC XX.
       01  FS-RELEVE               PIC XX.

      *----------------------------------------------------------------*
      * Indicateurs
      *----------------------------------------------------------------*
       01  WS-FIN-AIMPRIM          PIC X VALUE 'N'.
           88  FIN-AIMPRIM         VALUE 'O'.
       01  WS-ENTETE-IMPRIME       PIC X VALUE 'N'.
           88  ENTETE-IMPRIME      VALUE 'O'.
       01  WS-PREMIER-MVT-PAGE     PIC X VALUE 'O'.
           88  PREMIER-MVT-PAGE    VALUE 'O'.

      *----------------------------------------------------------------*
      * Compteurs
      *----------------------------------------------------------------*
       01  WS-NUM-PAGE             PIC 999 VALUE 0.
       01  WS-NUM-LIGNE            PIC 99 VALUE 0.
       01  WS-CPT-MVT-PAGE         PIC 99 VALUE 0.
       01  WS-I                    PIC 99.

      *----------------------------------------------------------------*
      * Donnees en-tete memorisees
      *----------------------------------------------------------------*
       01  WS-ENTETE-DATA.
           05  WS-ENT-CODE-CLIENT  PIC 9(5).
           05  WS-ENT-NOM          PIC X(20).
           05  WS-ENT-PRENOM       PIC X(20).
           05  WS-ENT-CODE-AGENCE  PIC 9(7).
           05  WS-ENT-LIB-AGENCE   PIC X(30).
           05  WS-ENT-RIB          PIC X(23).
           05  WS-ENT-DATE-DEB     PIC 9(8).
           05  WS-ENT-DATE-FIN     PIC 9(8).
           05  WS-ENT-SOLDE-PREC   PIC S9(11)V99.
           05  WS-ENT-SENS-SOLDE   PIC X.

      *----------------------------------------------------------------*
      * Enregistrement ENTETE lu (E) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-ENTETE.
           05  ENT-TYPE            PIC X.
           05  ENT-NUM-PAGE        PIC 999.
           05  ENT-NUM-LIGNE       PIC 99.
           05  ENT-CODE-CLIENT     PIC 9(5).
           05  ENT-NOM-CLIENT      PIC X(20).
           05  ENT-PRENOM-CLIENT   PIC X(20).
           05  ENT-CODE-AGENCE     PIC 9(7).
           05  ENT-LIBELLE-AGENCE  PIC X(30).
           05  ENT-RIB             PIC X(23).
           05  ENT-DATE-DEBUT      PIC 9(8).
           05  ENT-DATE-FIN        PIC 9(8).
           05  ENT-SOLDE-PREC      PIC S9(11)V99.
           05  ENT-SENS-SOLDE      PIC X.
           05  FILLER              PIC X(16).

      *----------------------------------------------------------------*
      * Enregistrement DETAIL lu (D) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-DETAIL.
           05  DET-TYPE            PIC X.
           05  DET-NUM-PAGE        PIC 999.
           05  DET-NUM-LIGNE       PIC 99.
           05  DET-CODE-CLIENT     PIC 9(5).
           05  DET-DATE-MVT        PIC 9(8).
           05  DET-DATE-MVT-R REDEFINES DET-DATE-MVT.
               10  DET-ANNEE       PIC 9(4).
               10  DET-MOIS        PIC 99.
               10  DET-JOUR        PIC 99.
           05  DET-LIBELLE         PIC X(30).
           05  DET-MONTANT-DEBIT   PIC 9(11)V99.
           05  DET-MONTANT-CREDIT  PIC 9(11)V99.
           05  DET-SOLDE-CUMUL     PIC S9(11)V99.
           05  DET-SENS-SOLDE      PIC X.
           05  FILLER              PIC X(58).

      *----------------------------------------------------------------*
      * Enregistrement PIED lu (P) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-PIED.
           05  PIE-TYPE            PIC X.
           05  PIE-NUM-PAGE        PIC 999.
           05  PIE-NUM-LIGNE       PIC 99.
           05  PIE-TYPE-PIED       PIC X.
           05  PIE-CUMUL-DEBIT     PIC 9(11)V99.
           05  PIE-CUMUL-CREDIT    PIC 9(11)V99.
           05  PIE-SOLDE-NOUVEAU   PIC S9(11)V99.
           05  PIE-SENS-SOLDE      PIC X.
           05  PIE-NB-MVT          PIC 999.
           05  FILLER              PIC X(99).

      *----------------------------------------------------------------*
      * Lignes d'impression
      *----------------------------------------------------------------*
       01  WS-LIGNE-VIDE           PIC X(132) VALUE SPACES.

       01  WS-LIGNE-SEPARATEUR.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(112) VALUE ALL '='.
           05  FILLER              PIC X(10) VALUE SPACES.

       01  WS-LIGNE-TIRET.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(112) VALUE ALL '-'.
           05  FILLER              PIC X(10) VALUE SPACES.

      *--- Ligne titre banque ---
       01  WS-LIGNE-BANQUE.
           05  FILLER              PIC X(45) VALUE SPACES.
           05  FILLER              PIC X(42)
               VALUE '*** BANQUE CENTRALE DE FORMATION ***'.
           05  FILLER              PIC X(45) VALUE SPACES.

      *--- Ligne titre releve ---
       01  WS-LIGNE-TITRE.
           05  FILLER              PIC X(50) VALUE SPACES.
           05  FILLER              PIC X(32)
               VALUE 'R E L E V E   D E   C O M P T E'.
           05  FILLER              PIC X(50) VALUE SPACES.

      *--- Ligne agence ---
       01  WS-LIGNE-AGENCE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'Agence : '.
           05  WS-ED-CODE-AGENCE   PIC 9.999.999.
           05  FILLER              PIC X(3) VALUE ' - '.
           05  WS-ED-LIB-AGENCE    PIC X(30).
           05  FILLER              PIC X(68) VALUE SPACES.

      *--- Ligne client ---
       01  WS-LIGNE-CLIENT.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'Client  : '.
           05  WS-ED-CODE-CLIENT   PIC 99.999.
           05  FILLER              PIC X(3) VALUE ' - '.
           05  WS-ED-NOM-CLIENT    PIC X(20).
           05  FILLER              PIC X(1) VALUE SPACE.
           05  WS-ED-PRENOM-CLIENT PIC X(20).
           05  FILLER              PIC X(60) VALUE SPACES.

      *--- Ligne RIB ---
       01  WS-LIGNE-RIB.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'RIB     : '.
           05  WS-ED-RIB           PIC X(23).
           05  FILLER              PIC X(89) VALUE SPACES.

      *--- Ligne periode ---
       01  WS-LIGNE-PERIODE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(11) VALUE 'Periode du '.
           05  WS-ED-DATE-DEB      PIC 99/99/9999.
           05  FILLER              PIC X(5) VALUE ' au '.
           05  WS-ED-DATE-FIN      PIC 99/99/9999.
           05  FILLER              PIC X(30) VALUE SPACES.
           05  FILLER              PIC X(8) VALUE 'Page : '.
           05  WS-ED-NUM-PAGE      PIC ZZ9.
           05  FILLER              PIC X(45) VALUE SPACES.

      *--- Ligne solde precedent ---
       01  WS-LIGNE-SOLDE-PREC.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'Solde precedent :'.
           05  FILLER              PIC X(50) VALUE SPACES.
           05  WS-ED-SOLDE-PREC    PIC ---.---.---.--9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  WS-ED-SENS-PREC     PIC X(3).
           05  FILLER              PIC X(23) VALUE SPACES.

      *--- Ligne en-tete colonnes ---
       01  WS-LIGNE-ENTETE-COL.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(12) VALUE 'DATE'.
           05  FILLER              PIC X(32) VALUE 'LIBELLE'.
           05  FILLER              PIC X(20) VALUE 'DEBIT'.
           05  FILLER              PIC X(20) VALUE 'CREDIT'.
           05  FILLER              PIC X(20) VALUE 'SOLDE'.
           05  FILLER              PIC X(18) VALUE SPACES.

      *--- Ligne detail mouvement ---
       01  WS-LIGNE-MVT.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  WS-ED-DATE-MVT      PIC 99/99/9999.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-ED-LIBELLE-MVT   PIC X(30).
           05  WS-ED-DEBIT-MVT     PIC ---.---.--9,99.
           05  FILLER              PIC X(4) VALUE SPACES.
           05  WS-ED-CREDIT-MVT    PIC ---.---.--9,99.
           05  FILLER              PIC X(4) VALUE SPACES.
           05  WS-ED-SOLDE-MVT     PIC ---.---.--9,99.
           05  FILLER              PIC X(1) VALUE SPACE.
           05  WS-ED-SENS-MVT      PIC X(2).
           05  FILLER              PIC X(24) VALUE SPACES.

      *--- Ligne cumuls page ---
       01  WS-LIGNE-CUMUL-PAGE.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(42)
               VALUE 'CUMULS PAGE :'.
           05  WS-ED-CUM-DEB-PAGE  PIC ---.---.--9,99.
           05  FILLER              PIC X(4) VALUE SPACES.
           05  WS-ED-CUM-CRE-PAGE  PIC ---.---.--9,99.
           05  FILLER              PIC X(41) VALUE SPACES.

      *--- Ligne total releve ---
       01  WS-LIGNE-TOTAL.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(42)
               VALUE '*** TOTAUX DU RELEVE ***'.
           05  WS-ED-TOT-DEBIT     PIC ---.---.--9,99.
           05  FILLER              PIC X(4) VALUE SPACES.
           05  WS-ED-TOT-CREDIT    PIC ---.---.--9,99.
           05  FILLER              PIC X(41) VALUE SPACES.

      *--- Ligne nouveau solde ---
       01  WS-LIGNE-SOLDE-NOUV.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'NOUVEAU SOLDE :'.
           05  FILLER              PIC X(50) VALUE SPACES.
           05  WS-ED-SOLDE-NOUV    PIC ***.***.***.**9,99.
           05  FILLER              PIC X(3) VALUE ' E '.
           05  WS-ED-SENS-NOUV     PIC X(3).
           05  FILLER              PIC X(23) VALUE SPACES.

      *--- Ligne nombre operations ---
       01  WS-LIGNE-NB-MVT.
           05  FILLER              PIC X(10) VALUE SPACES.
           05  FILLER              PIC X(25)
               VALUE 'Nombre d''operations :'.
           05  WS-ED-NB-MVT        PIC ZZ9.
           05  FILLER              PIC X(94) VALUE SPACES.

      *--- Ligne fin de releve ---
       01  WS-LIGNE-FIN.
           05  FILLER              PIC X(40) VALUE SPACES.
           05  FILLER              PIC X(52)
               VALUE '*** FIN DU RELEVE - MERCI DE VOTRE CONFIANCE ***'.
           05  FILLER              PIC X(40) VALUE SPACES.

      *----------------------------------------------------------------*
      * Zone de travail pour dates
      *----------------------------------------------------------------*
       01  WS-DATE-TRAVAIL.
           05  WS-DT-ANNEE         PIC 9(4).
           05  WS-DT-MOIS          PIC 99.
           05  WS-DT-JOUR          PIC 99.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 06 : IMPRESSION RELEVE BANCAIRE        '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INITIALISER
           PERFORM 2000-TRAITER-FICHIER
           PERFORM 9000-TERMINER

           STOP RUN.

      *----------------------------------------------------------------*
      * Initialisation
      *----------------------------------------------------------------*
       1000-INITIALISER.
           OPEN INPUT FAIMPRIM
           OPEN OUTPUT FRELEVE

           IF FS-AIMPRIM NOT = '00' OR FS-RELEVE NOT = '00'
               DISPLAY 'ERREUR OUVERTURE FICHIERS'
               DISPLAY '  AIMPRIM : ' FS-AIMPRIM
               DISPLAY '  RELEVE  : ' FS-RELEVE
               STOP RUN
           END-IF

           DISPLAY 'Fichiers ouverts avec succes.'
           DISPLAY ' '.

      *----------------------------------------------------------------*
      * Traitement du fichier AIMPRIM
      *----------------------------------------------------------------*
       2000-TRAITER-FICHIER.
           PERFORM UNTIL FIN-AIMPRIM
               READ FAIMPRIM INTO WS-ENR-ENTETE
                   AT END
                       MOVE 'O' TO WS-FIN-AIMPRIM
                   NOT AT END
                       PERFORM 2100-DISPATCHER-ENR
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
      * Dispatcher selon type enregistrement
      *----------------------------------------------------------------*
       2100-DISPATCHER-ENR.
           EVALUATE ENT-TYPE
               WHEN 'E'
                   MOVE ENR-AIMPRIM TO WS-ENR-ENTETE
                   PERFORM 3000-TRAITER-ENTETE
               WHEN 'D'
                   MOVE ENR-AIMPRIM TO WS-ENR-DETAIL
                   PERFORM 4000-TRAITER-DETAIL
               WHEN 'P'
                   MOVE ENR-AIMPRIM TO WS-ENR-PIED
                   PERFORM 5000-TRAITER-PIED
               WHEN OTHER
                   DISPLAY 'Type enregistrement inconnu : ' ENT-TYPE
           END-EVALUATE.

      *----------------------------------------------------------------*
      * Traitement enregistrement ENTETE
      *----------------------------------------------------------------*
       3000-TRAITER-ENTETE.
      *    Memoriser les donnees de l'en-tete
           MOVE ENT-CODE-CLIENT TO WS-ENT-CODE-CLIENT
           MOVE ENT-NOM-CLIENT TO WS-ENT-NOM
           MOVE ENT-PRENOM-CLIENT TO WS-ENT-PRENOM
           MOVE ENT-CODE-AGENCE TO WS-ENT-CODE-AGENCE
           MOVE ENT-LIBELLE-AGENCE TO WS-ENT-LIB-AGENCE
           MOVE ENT-RIB TO WS-ENT-RIB
           MOVE ENT-DATE-DEBUT TO WS-ENT-DATE-DEB
           MOVE ENT-DATE-FIN TO WS-ENT-DATE-FIN
           MOVE ENT-SOLDE-PREC TO WS-ENT-SOLDE-PREC
           MOVE ENT-SENS-SOLDE TO WS-ENT-SENS-SOLDE

           MOVE 'O' TO WS-ENTETE-IMPRIME
           MOVE 1 TO WS-NUM-PAGE
           PERFORM 3100-IMPRIMER-ENTETE.

      *----------------------------------------------------------------*
      * Imprimer l'en-tete de page
      *----------------------------------------------------------------*
       3100-IMPRIMER-ENTETE.
           MOVE 0 TO WS-NUM-LIGNE
           MOVE 0 TO WS-CPT-MVT-PAGE
           MOVE 'O' TO WS-PREMIER-MVT-PAGE

      *    Lignes vides en haut
           PERFORM 3 TIMES
               WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
               ADD 1 TO WS-NUM-LIGNE
           END-PERFORM

      *    Titre banque
           WRITE ENR-RELEVE FROM WS-LIGNE-SEPARATEUR
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           WRITE ENR-RELEVE FROM WS-LIGNE-BANQUE
           WRITE ENR-RELEVE FROM WS-LIGNE-TITRE
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           WRITE ENR-RELEVE FROM WS-LIGNE-SEPARATEUR
           ADD 6 TO WS-NUM-LIGNE

      *    Informations client
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           MOVE WS-ENT-CODE-AGENCE TO WS-ED-CODE-AGENCE
           MOVE WS-ENT-LIB-AGENCE TO WS-ED-LIB-AGENCE
           WRITE ENR-RELEVE FROM WS-LIGNE-AGENCE

           MOVE WS-ENT-CODE-CLIENT TO WS-ED-CODE-CLIENT
           MOVE WS-ENT-NOM TO WS-ED-NOM-CLIENT
           MOVE WS-ENT-PRENOM TO WS-ED-PRENOM-CLIENT
           WRITE ENR-RELEVE FROM WS-LIGNE-CLIENT

           MOVE WS-ENT-RIB TO WS-ED-RIB
           WRITE ENR-RELEVE FROM WS-LIGNE-RIB
           ADD 4 TO WS-NUM-LIGNE

      *    Periode et page
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           MOVE WS-ENT-DATE-DEB TO WS-DATE-TRAVAIL
           STRING WS-DT-JOUR WS-DT-MOIS WS-DT-ANNEE DELIMITED SIZE
               INTO WS-ED-DATE-DEB
           MOVE WS-ENT-DATE-FIN TO WS-DATE-TRAVAIL
           STRING WS-DT-JOUR WS-DT-MOIS WS-DT-ANNEE DELIMITED SIZE
               INTO WS-ED-DATE-FIN
           MOVE WS-NUM-PAGE TO WS-ED-NUM-PAGE
           WRITE ENR-RELEVE FROM WS-LIGNE-PERIODE
           ADD 2 TO WS-NUM-LIGNE

      *    Solde precedent
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           MOVE WS-ENT-SOLDE-PREC TO WS-ED-SOLDE-PREC
           IF WS-ENT-SENS-SOLDE = 'C'
               MOVE 'CR' TO WS-ED-SENS-PREC
           ELSE
               MOVE 'DB' TO WS-ED-SENS-PREC
           END-IF
           WRITE ENR-RELEVE FROM WS-LIGNE-SOLDE-PREC
           ADD 2 TO WS-NUM-LIGNE

      *    En-tete colonnes
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           WRITE ENR-RELEVE FROM WS-LIGNE-TIRET
           WRITE ENR-RELEVE FROM WS-LIGNE-ENTETE-COL
           WRITE ENR-RELEVE FROM WS-LIGNE-TIRET
           ADD 4 TO WS-NUM-LIGNE.

      *----------------------------------------------------------------*
      * Traitement enregistrement DETAIL
      *----------------------------------------------------------------*
       4000-TRAITER-DETAIL.
      *    Lignes vides entre mouvements (sauf premier)
           IF NOT PREMIER-MVT-PAGE
               PERFORM WS-LIGNES-ENTRE-MVT TIMES
                   WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
                   ADD 1 TO WS-NUM-LIGNE
               END-PERFORM
           ELSE
               MOVE 'N' TO WS-PREMIER-MVT-PAGE
           END-IF

      *    Formater et ecrire la ligne de detail
           MOVE DET-DATE-MVT TO WS-DATE-TRAVAIL
           STRING WS-DT-JOUR WS-DT-MOIS WS-DT-ANNEE DELIMITED SIZE
               INTO WS-ED-DATE-MVT
           MOVE DET-LIBELLE TO WS-ED-LIBELLE-MVT

           IF DET-MONTANT-DEBIT > 0
               MOVE DET-MONTANT-DEBIT TO WS-ED-DEBIT-MVT
           ELSE
               MOVE SPACES TO WS-ED-DEBIT-MVT
           END-IF

           IF DET-MONTANT-CREDIT > 0
               MOVE DET-MONTANT-CREDIT TO WS-ED-CREDIT-MVT
           ELSE
               MOVE SPACES TO WS-ED-CREDIT-MVT
           END-IF

           MOVE DET-SOLDE-CUMUL TO WS-ED-SOLDE-MVT
           IF DET-SENS-SOLDE = 'C'
               MOVE 'CR' TO WS-ED-SENS-MVT
           ELSE
               MOVE 'DB' TO WS-ED-SENS-MVT
           END-IF

           WRITE ENR-RELEVE FROM WS-LIGNE-MVT
           ADD 1 TO WS-NUM-LIGNE
           ADD 1 TO WS-CPT-MVT-PAGE

           DISPLAY 'Mouvement imprime : ' DET-DATE-MVT.

      *----------------------------------------------------------------*
      * Traitement enregistrement PIED
      *----------------------------------------------------------------*
       5000-TRAITER-PIED.
           EVALUATE PIE-TYPE-PIED
               WHEN 'P'
                   PERFORM 5100-IMPRIMER-PIED-PAGE
               WHEN 'T'
                   PERFORM 5200-IMPRIMER-TOTAL
           END-EVALUATE.

      *----------------------------------------------------------------*
      * Imprimer pied de page
      *----------------------------------------------------------------*
       5100-IMPRIMER-PIED-PAGE.
      *    Completer les lignes jusqu'au pied de page
           COMPUTE WS-I = WS-LIGNES-ENTETE + WS-LIGNES-CORPS
               - WS-NUM-LIGNE
           PERFORM WS-I TIMES
               WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           END-PERFORM

      *    Cumuls de la page
           WRITE ENR-RELEVE FROM WS-LIGNE-TIRET
           MOVE PIE-CUMUL-DEBIT TO WS-ED-CUM-DEB-PAGE
           MOVE PIE-CUMUL-CREDIT TO WS-ED-CUM-CRE-PAGE
           WRITE ENR-RELEVE FROM WS-LIGNE-CUMUL-PAGE
           WRITE ENR-RELEVE FROM WS-LIGNE-TIRET

      *    Completer le pied de page
           PERFORM 15 TIMES
               WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           END-PERFORM

      *    Nouvelle page
           ADD 1 TO WS-NUM-PAGE
           PERFORM 3100-IMPRIMER-ENTETE.

      *----------------------------------------------------------------*
      * Imprimer total du releve
      *----------------------------------------------------------------*
       5200-IMPRIMER-TOTAL.
      *    Completer lignes restantes du corps
           COMPUTE WS-I = WS-LIGNES-ENTETE + WS-LIGNES-CORPS
               - WS-NUM-LIGNE
           IF WS-I > 0
               PERFORM WS-I TIMES
                   WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
               END-PERFORM
           END-IF

      *    Lignes de separation
           WRITE ENR-RELEVE FROM WS-LIGNE-SEPARATEUR
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE

      *    Total debit/credit
           MOVE PIE-CUMUL-DEBIT TO WS-ED-TOT-DEBIT
           MOVE PIE-CUMUL-CREDIT TO WS-ED-TOT-CREDIT
           WRITE ENR-RELEVE FROM WS-LIGNE-TOTAL
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE

      *    Nouveau solde (avec asterisques de protection)
           MOVE PIE-SOLDE-NOUVEAU TO WS-ED-SOLDE-NOUV
           IF PIE-SENS-SOLDE = 'C'
               MOVE 'CR' TO WS-ED-SENS-NOUV
           ELSE
               MOVE 'DB' TO WS-ED-SENS-NOUV
           END-IF
           WRITE ENR-RELEVE FROM WS-LIGNE-SOLDE-NOUV
           WRITE ENR-RELEVE FROM WS-LIGNE-VIDE

      *    Nombre d'operations
           MOVE PIE-NB-MVT TO WS-ED-NB-MVT
           WRITE ENR-RELEVE FROM WS-LIGNE-NB-MVT

      *    Lignes vides
           PERFORM 5 TIMES
               WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           END-PERFORM

      *    Fin de releve
           WRITE ENR-RELEVE FROM WS-LIGNE-SEPARATEUR
           WRITE ENR-RELEVE FROM WS-LIGNE-FIN
           WRITE ENR-RELEVE FROM WS-LIGNE-SEPARATEUR

      *    Completer le pied de page
           PERFORM 5 TIMES
               WRITE ENR-RELEVE FROM WS-LIGNE-VIDE
           END-PERFORM.

      *----------------------------------------------------------------*
      * Terminaison
      *----------------------------------------------------------------*
       9000-TERMINER.
           CLOSE FAIMPRIM FRELEVE

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  RELEVE BANCAIRE GENERE                          '
           DISPLAY '=================================================='
           DISPLAY '  Nombre de pages : ' WS-NUM-PAGE
           DISPLAY '  Fichier sortie  : RELEVE.TXT'
           DISPLAY '=================================================='
           DISPLAY ' '.

