       IDENTIFICATION DIVISION.
       PROGRAM-ID. C06-BANQUE02.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * TD BANQUE VIRTUELLE - Exercice 3
      *
      * Chargement du buffer et dispatch vers les 4 tables :
      * 1. Charger TABLE-BUFFER avec 20 lignes (affectation)
      * 2. Parcourir le buffer
      * 3. Selon ID-TABLE (A/C/R/M), dispatcher vers la bonne table
      * 4. Valider les codes agence avant insertion
      *
      * Validation code agence : cle = 7 - (code6 MOD 7)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Constantes
      *----------------------------------------------------------------
       01  WS-MAX-BUFFER       PIC 99 VALUE 20.
       01  WS-MAX-AGENCES      PIC 99 VALUE 10.
       01  WS-MAX-CLIENTS      PIC 99 VALUE 10.
       01  WS-MAX-RIB          PIC 99 VALUE 10.
       01  WS-MAX-MVTC         PIC 99 VALUE 20.

      *----------------------------------------------------------------
      * Compteurs des tables
      *----------------------------------------------------------------
       01  WS-NB-AGENCES       PIC 99 VALUE 0.
       01  WS-NB-CLIENTS       PIC 99 VALUE 0.
       01  WS-NB-RIB           PIC 99 VALUE 0.
       01  WS-NB-MVTC          PIC 99 VALUE 0.
       01  WS-NB-AG-REJET      PIC 99 VALUE 0.

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  WS-I                PIC 99 VALUE 0.
       01  WS-CODE6            PIC 9(6) VALUE 0.
       01  WS-CODE1            PIC 9 VALUE 0.
       01  WS-RESTE            PIC 9 VALUE 0.
       01  WS-CLE-CALC         PIC 9 VALUE 0.
       01  WS-CODE-VALIDE      PIC 9 VALUE 0.
           88  CODE-OK         VALUE 1.
           88  CODE-KO         VALUE 0.

      *================================================================
      * TABLE 1 : AGENCES (TAG) - 37 octets par ligne
      *================================================================
       01  TABLE-AGENCES.
           05  AGENCE OCCURS 10 TIMES INDEXED BY IDX-AG.
               10  CODE-TAG        PIC 9(7).
               10  LIBELLE-TAG     PIC X(30).

      *================================================================
      * TABLE 2 : CLIENTS (TCLT) - 54 octets par ligne
      *================================================================
       01  TABLE-CLIENTS.
           05  CLIENT OCCURS 10 TIMES INDEXED BY IDX-CLT.
               10  CODE-TCLT       PIC 9(7).
               10  NOM-TCLT        PIC X(20).
               10  PRENOM-TCLT     PIC X(20).
               10  CODEAG-TCLT     PIC 9(7).

      *================================================================
      * TABLE 3 : RIB (TRIB) - 50 octets par ligne
      *================================================================
       01  TABLE-RIB.
           05  RIB OCCURS 10 TIMES INDEXED BY IDX-RIB.
               10  CLT-TRIB        PIC 9(7).
               10  CPTE-TRIB       PIC X(23).
               10  DATE-TRIB       PIC 9(8).
               10  SOLDE-TRIB      PIC 9(9)V99.
               10  SENS-TRIB       PIC X.

      *================================================================
      * TABLE 4 : MOUVEMENTS COMPTES (TMVTC) - 27 octets par ligne
      *================================================================
       01  TABLE-MVTC.
           05  MVTC OCCURS 20 TIMES INDEXED BY IDX-MVT.
               10  CLT-TMVTC       PIC 9(7).
               10  DATE-TMVTC      PIC 9(8).
               10  MONT-TMVTC      PIC 9(9)V99.
               10  SENS-TMVTC      PIC X.

      *================================================================
      * TABLE 5 : BUFFER GLOBAL (WBUFFER) - 55 octets par ligne
      *================================================================
       01  TABLE-BUFFER.
           05  BUFFER-LIGNE OCCURS 20 TIMES INDEXED BY IDX-BUF.
               10  ID-TABLE        PIC X.
                   88  EST-AGENCE      VALUE 'A'.
                   88  EST-CLIENT      VALUE 'C'.
                   88  EST-RIB         VALUE 'R'.
                   88  EST-MOUVEMENT   VALUE 'M'.
               10  ENR-BUFFER      PIC X(54).

      *----------------------------------------------------------------
      * Zones de travail pour extraction du buffer
      *----------------------------------------------------------------
       01  WS-ENR-AGENCE.
           05  WS-AG-CODE          PIC 9(7).
           05  WS-AG-LIBELLE       PIC X(30).

       01  WS-ENR-CLIENT.
           05  WS-CLT-CODE         PIC 9(7).
           05  WS-CLT-NOM          PIC X(20).
           05  WS-CLT-PRENOM       PIC X(20).
           05  WS-CLT-CODEAG       PIC 9(7).

       01  WS-ENR-RIB.
           05  WS-RIB-CLT          PIC 9(7).
           05  WS-RIB-CPTE         PIC X(23).
           05  WS-RIB-DATE         PIC 9(8).
           05  WS-RIB-SOLDE        PIC 9(9)V99.
           05  WS-RIB-SENS         PIC X.

       01  WS-ENR-MVTC.
           05  WS-MVT-CLT          PIC 9(7).
           05  WS-MVT-DATE         PIC 9(8).
           05  WS-MVT-MONT         PIC 9(9)V99.
           05  WS-MVT-SENS         PIC X.

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-SOLDE-E             PIC Z(9)9,99.
       01  WS-MONT-E              PIC Z(9)9,99.

      *----------------------------------------------------------------
      * Zone de travail pour chargement buffer (55 car)
      *----------------------------------------------------------------
       01  WS-LIGNE-BUF           PIC X(55).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  TD BANQUE VIRTUELLE - Chargement et Dispatch'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INIT-TABLES
           PERFORM 2000-CHARGER-BUFFER
           PERFORM 3000-DISPATCHER-BUFFER
           PERFORM 4000-AFFICHER-RESULTATS

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Fin du programme BANQUE02'
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Initialisation des tables
      *----------------------------------------------------------------
       1000-INIT-TABLES.
           INITIALIZE TABLE-AGENCES
           INITIALIZE TABLE-CLIENTS
           INITIALIZE TABLE-RIB
           INITIALIZE TABLE-MVTC
           INITIALIZE TABLE-BUFFER
           MOVE 0 TO WS-NB-AGENCES WS-NB-CLIENTS
                     WS-NB-RIB WS-NB-MVTC WS-NB-AG-REJET.

      *----------------------------------------------------------------
      * Chargement du buffer avec des donnees de test
      * Format : ID (1) + Donnees (54) = 55 caracteres
      *----------------------------------------------------------------
       2000-CHARGER-BUFFER.
           DISPLAY 'Chargement du buffer...'
           DISPLAY ' '

      * --- AGENCES (A) : ID(1) + Code(7) + Libelle(30) = 38 ---
      * Codes valides : cle = 7 - (code6 MOD 7)
           STRING 'A' '1000002' 'AGENCE PARIS CENTRE           '
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(1)

           STRING 'A' '2000004' 'AGENCE LYON PART-DIEU         '
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(2)

           STRING 'A' '3000006' 'AGENCE MARSEILLE VIEUX-PORT   '
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(3)

           STRING 'A' '4000001' 'AGENCE BORDEAUX CENTRE        '
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(4)

      * Code invalide (cle devrait etre 3, pas 9)
           STRING 'A' '5000009' 'AGENCE INVALIDE TEST          '
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(5)

      * --- CLIENTS (C) : ID(1) + Code(7) + Nom(20) + Prenom(20) + Ag(7)
           STRING 'C' '0000001' 'DUPONT              '
               'JEAN                ' '1000002'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(6)

           STRING 'C' '0000002' 'MARTIN              '
               'MARIE               ' '1000002'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(7)

           STRING 'C' '0000003' 'DURAND              '
               'PIERRE              ' '2000004'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(8)

           STRING 'C' '0000004' 'BERNARD             '
               'SOPHIE              ' '3000006'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(9)

      * --- RIB (R) : ID(1) + Clt(7) + IBAN(23) + Date(8) + Solde(11) + S
           STRING 'R' '0000001' 'FR76123456789012345678901'
               '20250430' '00001500000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(10)

           STRING 'R' '0000002' 'FR76987654321098765432109'
               '20250430' '00000850000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(11)

           STRING 'R' '0000003' 'FR76111111111111111111111'
               '20250430' '00000230000' 'D'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(12)

           STRING 'R' '0000004' 'FR76222222222222222222222'
               '20250430' '00004500000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(13)

      * --- MOUVEMENTS (M) : ID(1) + Clt(7) + Date(8) + Mont(11) + Sens(1)
           STRING 'M' '0000001' '20250501' '00000150000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(14)

           STRING 'M' '0000001' '20250505' '00000045000' 'D'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(15)

           STRING 'M' '0000002' '20250502' '00000200000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(16)

           STRING 'M' '0000002' '20250510' '00000075000' 'D'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(17)

           STRING 'M' '0000003' '20250503' '00000100000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(18)

           STRING 'M' '0000003' '20250508' '00000050000' 'D'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(19)

           STRING 'M' '0000004' '20250504' '00000500000' 'C'
               DELIMITED SIZE INTO WS-LIGNE-BUF
           MOVE WS-LIGNE-BUF TO BUFFER-LIGNE(20)

           DISPLAY 'Buffer charge avec 20 lignes.'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Parcours du buffer et dispatch vers les tables
      *----------------------------------------------------------------
       3000-DISPATCHER-BUFFER.
           DISPLAY 'Dispatch du buffer vers les tables...'
           DISPLAY ' '

           SET IDX-BUF TO 1
           PERFORM UNTIL IDX-BUF > WS-MAX-BUFFER
               EVALUATE TRUE
                   WHEN EST-AGENCE(IDX-BUF)
                       PERFORM 3100-TRAITER-AGENCE
                   WHEN EST-CLIENT(IDX-BUF)
                       PERFORM 3200-TRAITER-CLIENT
                   WHEN EST-RIB(IDX-BUF)
                       PERFORM 3300-TRAITER-RIB
                   WHEN EST-MOUVEMENT(IDX-BUF)
                       PERFORM 3400-TRAITER-MVTC
               END-EVALUATE
               SET IDX-BUF UP BY 1
           END-PERFORM

           DISPLAY 'Dispatch termine.'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Traitement d'une ligne Agence
      *----------------------------------------------------------------
       3100-TRAITER-AGENCE.
           MOVE ENR-BUFFER(IDX-BUF) TO WS-ENR-AGENCE

      * Extraire code6 et cle pour validation
           DIVIDE WS-AG-CODE BY 10 GIVING WS-CODE6
               REMAINDER WS-CODE1
           PERFORM 3110-VALIDER-CODE-AGENCE

           IF CODE-OK
               ADD 1 TO WS-NB-AGENCES
               SET IDX-AG TO WS-NB-AGENCES
               MOVE WS-AG-CODE TO CODE-TAG(IDX-AG)
               MOVE WS-AG-LIBELLE TO LIBELLE-TAG(IDX-AG)
               DISPLAY '  [A] Agence ajoutee : ' WS-AG-CODE
                   ' - ' WS-AG-LIBELLE
           ELSE
               ADD 1 TO WS-NB-AG-REJET
               DISPLAY '  [A] Agence REJETEE : ' WS-AG-CODE
                   ' (code invalide)'
           END-IF.

      *----------------------------------------------------------------
      * Validation du code agence
      *----------------------------------------------------------------
       3110-VALIDER-CODE-AGENCE.
           COMPUTE WS-RESTE = FUNCTION MOD(WS-CODE6, 7)
           COMPUTE WS-CLE-CALC = 7 - WS-RESTE

           IF WS-CLE-CALC = WS-CODE1
               SET CODE-OK TO TRUE
           ELSE
               SET CODE-KO TO TRUE
           END-IF.

      *----------------------------------------------------------------
      * Traitement d'une ligne Client
      *----------------------------------------------------------------
       3200-TRAITER-CLIENT.
           MOVE ENR-BUFFER(IDX-BUF) TO WS-ENR-CLIENT

           ADD 1 TO WS-NB-CLIENTS
           SET IDX-CLT TO WS-NB-CLIENTS
           MOVE WS-CLT-CODE TO CODE-TCLT(IDX-CLT)
           MOVE WS-CLT-NOM TO NOM-TCLT(IDX-CLT)
           MOVE WS-CLT-PRENOM TO PRENOM-TCLT(IDX-CLT)
           MOVE WS-CLT-CODEAG TO CODEAG-TCLT(IDX-CLT)

           DISPLAY '  [C] Client ajoute : ' WS-CLT-CODE
               ' - ' WS-CLT-NOM WS-CLT-PRENOM.

      *----------------------------------------------------------------
      * Traitement d'une ligne RIB
      *----------------------------------------------------------------
       3300-TRAITER-RIB.
           MOVE ENR-BUFFER(IDX-BUF) TO WS-ENR-RIB

           ADD 1 TO WS-NB-RIB
           SET IDX-RIB TO WS-NB-RIB
           MOVE WS-RIB-CLT TO CLT-TRIB(IDX-RIB)
           MOVE WS-RIB-CPTE TO CPTE-TRIB(IDX-RIB)
           MOVE WS-RIB-DATE TO DATE-TRIB(IDX-RIB)
           MOVE WS-RIB-SOLDE TO SOLDE-TRIB(IDX-RIB)
           MOVE WS-RIB-SENS TO SENS-TRIB(IDX-RIB)

           MOVE WS-RIB-SOLDE TO WS-SOLDE-E
           DISPLAY '  [R] RIB ajoute : Client ' WS-RIB-CLT
               ' - Solde ' WS-SOLDE-E ' ' WS-RIB-SENS.

      *----------------------------------------------------------------
      * Traitement d'une ligne Mouvement
      *----------------------------------------------------------------
       3400-TRAITER-MVTC.
           MOVE ENR-BUFFER(IDX-BUF) TO WS-ENR-MVTC

           ADD 1 TO WS-NB-MVTC
           SET IDX-MVT TO WS-NB-MVTC
           MOVE WS-MVT-CLT TO CLT-TMVTC(IDX-MVT)
           MOVE WS-MVT-DATE TO DATE-TMVTC(IDX-MVT)
           MOVE WS-MVT-MONT TO MONT-TMVTC(IDX-MVT)
           MOVE WS-MVT-SENS TO SENS-TMVTC(IDX-MVT)

           MOVE WS-MVT-MONT TO WS-MONT-E
           DISPLAY '  [M] Mouvement ajoute : Client ' WS-MVT-CLT
               ' - ' WS-MONT-E ' ' WS-MVT-SENS.

      *----------------------------------------------------------------
      * Affichage des resultats
      *----------------------------------------------------------------
       4000-AFFICHER-RESULTATS.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'RESUME DU CHARGEMENT :'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '
           DISPLAY 'Agences chargees  : ' WS-NB-AGENCES
           DISPLAY 'Agences rejetees  : ' WS-NB-AG-REJET
           DISPLAY 'Clients charges   : ' WS-NB-CLIENTS
           DISPLAY 'RIB charges       : ' WS-NB-RIB
           DISPLAY 'Mouvements charges: ' WS-NB-MVTC
           DISPLAY ' '

           PERFORM 4100-AFFICHER-AGENCES
           PERFORM 4200-AFFICHER-CLIENTS
           PERFORM 4300-AFFICHER-RIB
           PERFORM 4400-AFFICHER-MVTC.

      *----------------------------------------------------------------
      * Affichage table Agences
      *----------------------------------------------------------------
       4100-AFFICHER-AGENCES.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'TABLE AGENCES :'
           DISPLAY '--------------------------------------------------'

           IF WS-NB-AGENCES = 0
               DISPLAY '  (aucune agence)'
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NB-AGENCES
                   SET IDX-AG TO WS-I
                   DISPLAY '  ' CODE-TAG(IDX-AG) ' | '
                       LIBELLE-TAG(IDX-AG)
               END-PERFORM
           END-IF
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Affichage table Clients
      *----------------------------------------------------------------
       4200-AFFICHER-CLIENTS.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'TABLE CLIENTS :'
           DISPLAY '--------------------------------------------------'

           IF WS-NB-CLIENTS = 0
               DISPLAY '  (aucun client)'
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NB-CLIENTS
                   SET IDX-CLT TO WS-I
                   DISPLAY '  ' CODE-TCLT(IDX-CLT) ' | '
                       NOM-TCLT(IDX-CLT) ' ' PRENOM-TCLT(IDX-CLT)
                       ' | Ag: ' CODEAG-TCLT(IDX-CLT)
               END-PERFORM
           END-IF
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Affichage table RIB
      *----------------------------------------------------------------
       4300-AFFICHER-RIB.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'TABLE RIB :'
           DISPLAY '--------------------------------------------------'

           IF WS-NB-RIB = 0
               DISPLAY '  (aucun RIB)'
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NB-RIB
                   SET IDX-RIB TO WS-I
                   MOVE SOLDE-TRIB(IDX-RIB) TO WS-SOLDE-E
                   DISPLAY '  Clt ' CLT-TRIB(IDX-RIB) ' | '
                       CPTE-TRIB(IDX-RIB) ' | '
                       WS-SOLDE-E ' ' SENS-TRIB(IDX-RIB)
               END-PERFORM
           END-IF
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Affichage table Mouvements
      *----------------------------------------------------------------
       4400-AFFICHER-MVTC.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'TABLE MOUVEMENTS :'
           DISPLAY '--------------------------------------------------'

           IF WS-NB-MVTC = 0
               DISPLAY '  (aucun mouvement)'
           ELSE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-NB-MVTC
                   SET IDX-MVT TO WS-I
                   MOVE MONT-TMVTC(IDX-MVT) TO WS-MONT-E
                   DISPLAY '  Clt ' CLT-TMVTC(IDX-MVT) ' | '
                       DATE-TMVTC(IDX-MVT) ' | '
                       WS-MONT-E ' ' SENS-TMVTC(IDX-MVT)
               END-PERFORM
           END-IF
           DISPLAY ' '.
