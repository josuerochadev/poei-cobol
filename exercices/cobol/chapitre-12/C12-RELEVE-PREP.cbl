       IDENTIFICATION DIVISION.
       PROGRAM-ID. C12RELPRE.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C12-RELEVE-PREP
      * OBJET     : Exercice 05 - Preparation fichier impression AIMPRIM
      *             pour edition du releve bancaire
      *
      * ENTREES   : - AGENCE.DAT (INDEXED) - Informations agence
      *             - CLIENT.DAT (INDEXED) - Informations client
      *             - RIB.DAT    (INDEXED) - RIB et solde precedent
      *             - MVTC.DAT   (SEQUENTIAL) - Mouvements du compte
      *
      * SORTIE    : AIMPRIM.DAT - Fichier preparation impression
      *             Enregistrements de type :
      *             'E' = Entete (1 seul)
      *             'D' = Detail (mouvements avec num page/ligne)
      *             'P' = Pied de page (cumuls)
      *
      * CRITERES  : Client 01210, periode mai 2025
      * EXERCICE  : Chapitre XII - Fichier d'impression
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Fichier AGENCE (INDEXED)
           SELECT FAGENCE ASSIGN TO 'AGENCE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AG-CODE
               FILE STATUS IS FS-AGENCE.

      *    Fichier CLIENT (INDEXED)
           SELECT FCLIENT ASSIGN TO 'CLIENT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CL-CODE
               FILE STATUS IS FS-CLIENT.

      *    Fichier RIB (INDEXED)
           SELECT FRIB ASSIGN TO 'RIB.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS RIB-CODE-CLT
               FILE STATUS IS FS-RIB.

      *    Fichier MOUVEMENTS (SEQUENTIAL)
           SELECT FMVTC ASSIGN TO 'MVTC.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-MVTC.

      *    Fichier AIMPRIM (sortie)
           SELECT FAIMPRIM ASSIGN TO 'AIMPRIM.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-AIMPRIM.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Fichier AGENCE
      *----------------------------------------------------------------*
       FD  FAGENCE.
       01  ENR-AGENCE.
           05  AG-CODE             PIC 9(7).
           05  AG-LIBELLE          PIC X(30).

      *----------------------------------------------------------------*
      * Fichier CLIENT
      *----------------------------------------------------------------*
       FD  FCLIENT.
       01  ENR-CLIENT.
           05  CL-CODE             PIC 9(5).
           05  CL-NOM              PIC X(20).
           05  CL-PRENOM           PIC X(20).
           05  CL-CODE-AGENCE      PIC 9(7).

      *----------------------------------------------------------------*
      * Fichier RIB
      *----------------------------------------------------------------*
       FD  FRIB.
       01  ENR-RIB.
           05  RIB-CODE-CLT        PIC 9(5).
           05  RIB-COMPTE          PIC X(23).
           05  RIB-DATE            PIC 9(8).
           05  RIB-SOLDE           PIC 9(9)V99.
           05  RIB-SENS            PIC X.

      *----------------------------------------------------------------*
      * Fichier MOUVEMENTS
      *----------------------------------------------------------------*
       FD  FMVTC.
       01  ENR-MVTC.
           05  MV-CODE-CLT         PIC 9(5).
           05  MV-DATE             PIC 9(8).
           05  MV-MONTANT          PIC 9(9)V99.
           05  MV-SENS             PIC X.

      *----------------------------------------------------------------*
      * Fichier AIMPRIM - Sortie
      *----------------------------------------------------------------*
       FD  FAIMPRIM
           RECORD CONTAINS 150 CHARACTERS.
       01  ENR-AIMPRIM             PIC X(150).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Constantes
      *----------------------------------------------------------------*
       01  WS-CLIENT-CIBLE         PIC 9(5) VALUE 01210.
       01  WS-PERIODE-DEBUT        PIC 9(8) VALUE 20250501.
       01  WS-PERIODE-FIN          PIC 9(8) VALUE 20250531.
       01  WS-MAX-DETAIL-PAGE      PIC 99 VALUE 5.

      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01  FS-AGENCE               PIC XX.
       01  FS-CLIENT               PIC XX.
       01  FS-RIB                  PIC XX.
       01  FS-MVTC                 PIC XX.
       01  FS-AIMPRIM              PIC XX.

      *----------------------------------------------------------------*
      * Indicateurs
      *----------------------------------------------------------------*
       01  WS-FIN-MVTC             PIC X VALUE 'N'.
           88  FIN-MVTC            VALUE 'O'.
       01  WS-CLIENT-TROUVE        PIC X VALUE 'N'.
           88  CLIENT-TROUVE       VALUE 'O'.

      *----------------------------------------------------------------*
      * Compteurs et cumuls
      *----------------------------------------------------------------*
       01  WS-NUM-PAGE             PIC 999 VALUE 0.
       01  WS-NUM-LIGNE            PIC 99 VALUE 0.
       01  WS-CPT-MVT              PIC 999 VALUE 0.
       01  WS-CPT-MVT-PAGE         PIC 99 VALUE 0.

       01  WS-CUMUL-DEBIT-PAGE     PIC 9(11)V99 VALUE 0.
       01  WS-CUMUL-CREDIT-PAGE    PIC 9(11)V99 VALUE 0.
       01  WS-CUMUL-DEBIT-TOTAL    PIC 9(11)V99 VALUE 0.
       01  WS-CUMUL-CREDIT-TOTAL   PIC 9(11)V99 VALUE 0.
       01  WS-SOLDE-NOUVEAU        PIC S9(11)V99 VALUE 0.
       01  WS-SOLDE-PRECEDENT      PIC S9(11)V99 VALUE 0.

      *----------------------------------------------------------------*
      * Donnees client recuperees
      *----------------------------------------------------------------*
       01  WS-INFO-CLIENT.
           05  WS-CL-CODE          PIC 9(5).
           05  WS-CL-NOM           PIC X(20).
           05  WS-CL-PRENOM        PIC X(20).
           05  WS-CL-AGENCE        PIC 9(7).
           05  WS-AG-LIBELLE       PIC X(30).
           05  WS-RIB-COMPTE       PIC X(23).
           05  WS-RIB-SOLDE        PIC 9(9)V99.
           05  WS-RIB-SENS         PIC X.

      *----------------------------------------------------------------*
      * Enregistrement ENTETE (E) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-ENTETE.
           05  ENT-TYPE            PIC X VALUE 'E'.
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
           05  FILLER              PIC X(16) VALUE SPACES.

      *----------------------------------------------------------------*
      * Enregistrement DETAIL (D) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-DETAIL.
           05  DET-TYPE            PIC X VALUE 'D'.
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
           05  FILLER              PIC X(58) VALUE SPACES.

      *----------------------------------------------------------------*
      * Enregistrement PIED DE PAGE (P) - 150 caracteres
      *----------------------------------------------------------------*
       01  WS-ENR-PIED.
           05  PIE-TYPE            PIC X VALUE 'P'.
           05  PIE-NUM-PAGE        PIC 999.
           05  PIE-NUM-LIGNE       PIC 99.
           05  PIE-TYPE-PIED       PIC X.
      *        'P' = Pied de page, 'T' = Total releve
           05  PIE-CUMUL-DEBIT     PIC 9(11)V99.
           05  PIE-CUMUL-CREDIT    PIC 9(11)V99.
           05  PIE-SOLDE-NOUVEAU   PIC S9(11)V99.
           05  PIE-SENS-SOLDE      PIC X.
           05  PIE-NB-MVT          PIC 999.
           05  FILLER              PIC X(99) VALUE SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 05 : PREPARATION FICHIER AIMPRIM       '
           DISPLAY '  Releve bancaire - Client 01210 - Mai 2025       '
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INITIALISER
           IF CLIENT-TROUVE
               PERFORM 2000-TRAITER-MOUVEMENTS
               PERFORM 3000-FINALISER
           END-IF
           PERFORM 9000-TERMINER

           STOP RUN.

      *----------------------------------------------------------------*
      * Initialisation - Ouverture fichiers et lecture infos client
      *----------------------------------------------------------------*
       1000-INITIALISER.
           OPEN INPUT FAGENCE FCLIENT FRIB FMVTC
           OPEN OUTPUT FAIMPRIM

           IF FS-AGENCE NOT = '00' OR FS-CLIENT NOT = '00'
              OR FS-RIB NOT = '00' OR FS-MVTC NOT = '00'
              OR FS-AIMPRIM NOT = '00'
               DISPLAY 'ERREUR OUVERTURE FICHIERS'
               DISPLAY '  AGENCE  : ' FS-AGENCE
               DISPLAY '  CLIENT  : ' FS-CLIENT
               DISPLAY '  RIB     : ' FS-RIB
               DISPLAY '  MVTC    : ' FS-MVTC
               DISPLAY '  AIMPRIM : ' FS-AIMPRIM
               STOP RUN
           END-IF

           DISPLAY 'Fichiers ouverts avec succes.'
           DISPLAY ' '

      *    Lecture des informations du client cible
           PERFORM 1100-LIRE-CLIENT
           IF CLIENT-TROUVE
               PERFORM 1200-LIRE-AGENCE
               PERFORM 1300-LIRE-RIB
               PERFORM 1400-ECRIRE-ENTETE
           END-IF.

      *----------------------------------------------------------------*
      * Lecture informations CLIENT
      *----------------------------------------------------------------*
       1100-LIRE-CLIENT.
           MOVE WS-CLIENT-CIBLE TO CL-CODE
           READ FCLIENT
               INVALID KEY
                   DISPLAY 'CLIENT ' WS-CLIENT-CIBLE ' NON TROUVE'
                   MOVE 'N' TO WS-CLIENT-TROUVE
               NOT INVALID KEY
                   MOVE 'O' TO WS-CLIENT-TROUVE
                   MOVE CL-CODE TO WS-CL-CODE
                   MOVE CL-NOM TO WS-CL-NOM
                   MOVE CL-PRENOM TO WS-CL-PRENOM
                   MOVE CL-CODE-AGENCE TO WS-CL-AGENCE
                   DISPLAY 'Client trouve : ' WS-CL-CODE ' '
                       WS-CL-NOM ' ' WS-CL-PRENOM
           END-READ.

      *----------------------------------------------------------------*
      * Lecture informations AGENCE
      *----------------------------------------------------------------*
       1200-LIRE-AGENCE.
           MOVE WS-CL-AGENCE TO AG-CODE
           READ FAGENCE
               INVALID KEY
                   MOVE 'AGENCE INCONNUE' TO WS-AG-LIBELLE
               NOT INVALID KEY
                   MOVE AG-LIBELLE TO WS-AG-LIBELLE
                   DISPLAY 'Agence : ' AG-CODE ' ' WS-AG-LIBELLE
           END-READ.

      *----------------------------------------------------------------*
      * Lecture informations RIB
      *----------------------------------------------------------------*
       1300-LIRE-RIB.
           MOVE WS-CLIENT-CIBLE TO RIB-CODE-CLT
           READ FRIB
               INVALID KEY
                   DISPLAY 'RIB non trouve pour client ' WS-CLIENT-CIBLE
                   MOVE ZEROS TO WS-RIB-SOLDE
                   MOVE 'C' TO WS-RIB-SENS
                   MOVE SPACES TO WS-RIB-COMPTE
               NOT INVALID KEY
                   MOVE RIB-COMPTE TO WS-RIB-COMPTE
                   MOVE RIB-SOLDE TO WS-RIB-SOLDE
                   MOVE RIB-SENS TO WS-RIB-SENS
                   DISPLAY 'RIB : ' WS-RIB-COMPTE
                   DISPLAY 'Solde precedent : ' WS-RIB-SOLDE
                       ' (' WS-RIB-SENS ')'
           END-READ

      *    Calcul solde precedent signe
           IF WS-RIB-SENS = 'D'
               COMPUTE WS-SOLDE-PRECEDENT = 0 - WS-RIB-SOLDE
           ELSE
               MOVE WS-RIB-SOLDE TO WS-SOLDE-PRECEDENT
           END-IF
           MOVE WS-SOLDE-PRECEDENT TO WS-SOLDE-NOUVEAU.

      *----------------------------------------------------------------*
      * Ecriture enregistrement ENTETE
      *----------------------------------------------------------------*
       1400-ECRIRE-ENTETE.
           INITIALIZE WS-ENR-ENTETE
           MOVE 'E' TO ENT-TYPE
           MOVE 1 TO ENT-NUM-PAGE
           MOVE 1 TO ENT-NUM-LIGNE
           MOVE WS-CL-CODE TO ENT-CODE-CLIENT
           MOVE WS-CL-NOM TO ENT-NOM-CLIENT
           MOVE WS-CL-PRENOM TO ENT-PRENOM-CLIENT
           MOVE WS-CL-AGENCE TO ENT-CODE-AGENCE
           MOVE WS-AG-LIBELLE TO ENT-LIBELLE-AGENCE
           MOVE WS-RIB-COMPTE TO ENT-RIB
           MOVE WS-PERIODE-DEBUT TO ENT-DATE-DEBUT
           MOVE WS-PERIODE-FIN TO ENT-DATE-FIN
           MOVE WS-SOLDE-PRECEDENT TO ENT-SOLDE-PREC
           IF WS-SOLDE-PRECEDENT >= 0
               MOVE 'C' TO ENT-SENS-SOLDE
           ELSE
               MOVE 'D' TO ENT-SENS-SOLDE
           END-IF

           WRITE ENR-AIMPRIM FROM WS-ENR-ENTETE
           DISPLAY 'Enregistrement ENTETE ecrit.'
           DISPLAY ' '

           MOVE 1 TO WS-NUM-PAGE
           MOVE 0 TO WS-NUM-LIGNE.

      *----------------------------------------------------------------*
      * Traitement des mouvements
      *----------------------------------------------------------------*
       2000-TRAITER-MOUVEMENTS.
           DISPLAY '--- Lecture des mouvements ---'
           PERFORM UNTIL FIN-MVTC
               READ FMVTC
                   AT END
                       MOVE 'O' TO WS-FIN-MVTC
                   NOT AT END
                       PERFORM 2100-ANALYSER-MOUVEMENT
               END-READ
           END-PERFORM.

      *----------------------------------------------------------------*
      * Analyse et traitement d'un mouvement
      *----------------------------------------------------------------*
       2100-ANALYSER-MOUVEMENT.
      *    Verifier si mouvement concerne le client cible
           IF MV-CODE-CLT = WS-CLIENT-CIBLE
               AND MV-DATE >= WS-PERIODE-DEBUT
               AND MV-DATE <= WS-PERIODE-FIN

      *        Verifier si nouvelle page necessaire
               IF WS-CPT-MVT-PAGE >= WS-MAX-DETAIL-PAGE
                   PERFORM 2200-SAUT-PAGE
               END-IF

               PERFORM 2300-ECRIRE-DETAIL
           END-IF.

      *----------------------------------------------------------------*
      * Saut de page avec pied de page
      *----------------------------------------------------------------*
       2200-SAUT-PAGE.
      *    Ecrire pied de page
           INITIALIZE WS-ENR-PIED
           MOVE 'P' TO PIE-TYPE
           MOVE WS-NUM-PAGE TO PIE-NUM-PAGE
           MOVE WS-NUM-LIGNE TO PIE-NUM-LIGNE
           MOVE 'P' TO PIE-TYPE-PIED
           MOVE WS-CUMUL-DEBIT-PAGE TO PIE-CUMUL-DEBIT
           MOVE WS-CUMUL-CREDIT-PAGE TO PIE-CUMUL-CREDIT
           MOVE WS-SOLDE-NOUVEAU TO PIE-SOLDE-NOUVEAU
           IF WS-SOLDE-NOUVEAU >= 0
               MOVE 'C' TO PIE-SENS-SOLDE
           ELSE
               MOVE 'D' TO PIE-SENS-SOLDE
           END-IF
           MOVE WS-CPT-MVT-PAGE TO PIE-NB-MVT

           WRITE ENR-AIMPRIM FROM WS-ENR-PIED
           DISPLAY 'Pied de page ' WS-NUM-PAGE ' ecrit - '
               WS-CPT-MVT-PAGE ' mvts'

      *    Nouvelle page
           ADD 1 TO WS-NUM-PAGE
           MOVE 0 TO WS-NUM-LIGNE
           MOVE 0 TO WS-CPT-MVT-PAGE
           MOVE 0 TO WS-CUMUL-DEBIT-PAGE
           MOVE 0 TO WS-CUMUL-CREDIT-PAGE.

      *----------------------------------------------------------------*
      * Ecriture enregistrement DETAIL
      *----------------------------------------------------------------*
       2300-ECRIRE-DETAIL.
           ADD 1 TO WS-NUM-LIGNE
           ADD 1 TO WS-CPT-MVT
           ADD 1 TO WS-CPT-MVT-PAGE

           INITIALIZE WS-ENR-DETAIL
           MOVE 'D' TO DET-TYPE
           MOVE WS-NUM-PAGE TO DET-NUM-PAGE
           MOVE WS-NUM-LIGNE TO DET-NUM-LIGNE
           MOVE MV-CODE-CLT TO DET-CODE-CLIENT
           MOVE MV-DATE TO DET-DATE-MVT

      *    Libelle operation selon sens
           IF MV-SENS = 'D'
               MOVE 'DEBIT - OPERATION' TO DET-LIBELLE
               MOVE MV-MONTANT TO DET-MONTANT-DEBIT
               MOVE 0 TO DET-MONTANT-CREDIT
               ADD MV-MONTANT TO WS-CUMUL-DEBIT-PAGE
               ADD MV-MONTANT TO WS-CUMUL-DEBIT-TOTAL
               SUBTRACT MV-MONTANT FROM WS-SOLDE-NOUVEAU
           ELSE
               MOVE 'CREDIT - OPERATION' TO DET-LIBELLE
               MOVE 0 TO DET-MONTANT-DEBIT
               MOVE MV-MONTANT TO DET-MONTANT-CREDIT
               ADD MV-MONTANT TO WS-CUMUL-CREDIT-PAGE
               ADD MV-MONTANT TO WS-CUMUL-CREDIT-TOTAL
               ADD MV-MONTANT TO WS-SOLDE-NOUVEAU
           END-IF

           MOVE WS-SOLDE-NOUVEAU TO DET-SOLDE-CUMUL
           IF WS-SOLDE-NOUVEAU >= 0
               MOVE 'C' TO DET-SENS-SOLDE
           ELSE
               MOVE 'D' TO DET-SENS-SOLDE
           END-IF

           WRITE ENR-AIMPRIM FROM WS-ENR-DETAIL
           DISPLAY 'Detail: ' DET-DATE-MVT ' '
               MV-MONTANT ' ' MV-SENS.

      *----------------------------------------------------------------*
      * Finalisation - Ecriture pied de page final et total
      *----------------------------------------------------------------*
       3000-FINALISER.
      *    Ecrire dernier pied de page si mouvements sur la page
           IF WS-CPT-MVT-PAGE > 0
               INITIALIZE WS-ENR-PIED
               MOVE 'P' TO PIE-TYPE
               MOVE WS-NUM-PAGE TO PIE-NUM-PAGE
               ADD 1 TO WS-NUM-LIGNE
               MOVE WS-NUM-LIGNE TO PIE-NUM-LIGNE
               MOVE 'P' TO PIE-TYPE-PIED
               MOVE WS-CUMUL-DEBIT-PAGE TO PIE-CUMUL-DEBIT
               MOVE WS-CUMUL-CREDIT-PAGE TO PIE-CUMUL-CREDIT
               MOVE WS-SOLDE-NOUVEAU TO PIE-SOLDE-NOUVEAU
               IF WS-SOLDE-NOUVEAU >= 0
                   MOVE 'C' TO PIE-SENS-SOLDE
               ELSE
                   MOVE 'D' TO PIE-SENS-SOLDE
               END-IF
               MOVE WS-CPT-MVT-PAGE TO PIE-NB-MVT
               WRITE ENR-AIMPRIM FROM WS-ENR-PIED
           END-IF

      *    Ecrire total general du releve
           INITIALIZE WS-ENR-PIED
           MOVE 'P' TO PIE-TYPE
           MOVE WS-NUM-PAGE TO PIE-NUM-PAGE
           ADD 1 TO WS-NUM-LIGNE
           MOVE WS-NUM-LIGNE TO PIE-NUM-LIGNE
           MOVE 'T' TO PIE-TYPE-PIED
           MOVE WS-CUMUL-DEBIT-TOTAL TO PIE-CUMUL-DEBIT
           MOVE WS-CUMUL-CREDIT-TOTAL TO PIE-CUMUL-CREDIT
           MOVE WS-SOLDE-NOUVEAU TO PIE-SOLDE-NOUVEAU
           IF WS-SOLDE-NOUVEAU >= 0
               MOVE 'C' TO PIE-SENS-SOLDE
           ELSE
               MOVE 'D' TO PIE-SENS-SOLDE
           END-IF
           MOVE WS-CPT-MVT TO PIE-NB-MVT
           WRITE ENR-AIMPRIM FROM WS-ENR-PIED

           DISPLAY ' '
           DISPLAY 'Total general ecrit.'.

      *----------------------------------------------------------------*
      * Terminaison
      *----------------------------------------------------------------*
       9000-TERMINER.
           CLOSE FAGENCE FCLIENT FRIB FMVTC FAIMPRIM

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  STATISTIQUES FICHIER AIMPRIM                    '
           DISPLAY '=================================================='
           DISPLAY '  Nombre de pages        : ' WS-NUM-PAGE
           DISPLAY '  Nombre de mouvements   : ' WS-CPT-MVT
           DISPLAY '  Cumul debits           : ' WS-CUMUL-DEBIT-TOTAL
           DISPLAY '  Cumul credits          : ' WS-CUMUL-CREDIT-TOTAL
           DISPLAY '  Solde nouveau          : ' WS-SOLDE-NOUVEAU
           DISPLAY '=================================================='
           DISPLAY 'Fichier AIMPRIM.DAT genere avec succes.'
           DISPLAY ' '.

