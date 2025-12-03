       IDENTIFICATION DIVISION.
       PROGRAM-ID. C06-BANQUE01.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * TD BANQUE VIRTUELLE - Exercices 1 et 2
      *
      * Declaration des 5 tables pour le systeme bancaire :
      * - TABLE-AGENCES   : Agences bancaires
      * - TABLE-CLIENTS   : Clients de la banque
      * - TABLE-RIB       : RIB des clients (comptes)
      * - TABLE-MVTC      : Mouvements bancaires
      * - TABLE-BUFFER    : Buffer de chargement (table globale)
      *
      * Structure BUFFER : ID-TABLE (1) + DONNEES (54) = 55 octets
      *   'A' = Agence, 'C' = Client, 'R' = RIB, 'M' = Mouvement
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

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  WS-I                PIC 99 VALUE 0.
       01  WS-CODE6            PIC 9(6) VALUE 0.
       01  WS-CODE1            PIC 9 VALUE 0.
       01  WS-RESTE            PIC 9 VALUE 0.
       01  WS-CLE-CALC         PIC 9 VALUE 0.

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
      * Redefinition du buffer pour chaque type d'enregistrement
      *----------------------------------------------------------------
       01  WS-ENR-AGENCE.
           05  WS-AG-CODE          PIC 9(7).
           05  WS-AG-LIBELLE       PIC X(30).
           05  FILLER              PIC X(17).

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
           05  FILLER              PIC X(4).

       01  WS-ENR-MVTC.
           05  WS-MVT-CLT          PIC 9(7).
           05  WS-MVT-DATE         PIC 9(8).
           05  WS-MVT-MONT         PIC 9(9)V99.
           05  WS-MVT-SENS         PIC X.
           05  FILLER              PIC X(27).

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-SOLDE-E             PIC Z(9)9,99.
       01  WS-MONT-E              PIC Z(9)9,99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  TD BANQUE VIRTUELLE - Declaration des Tables'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-INIT-TABLES
           PERFORM 2000-AFFICHER-STRUCTURES
           PERFORM 3000-DEMO-VALIDATION-CODE

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Fin du programme BANQUE01'
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
           DISPLAY 'Tables initialisees avec succes.'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Affichage des structures des tables
      *----------------------------------------------------------------
       2000-AFFICHER-STRUCTURES.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'STRUCTURES DES TABLES DECLAREES :'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '

           DISPLAY '1. TABLE-AGENCES (TAG)'
           DISPLAY '   - CODE-TAG      PIC 9(7)   : Code agence'
           DISPLAY '   - LIBELLE-TAG   PIC X(30)  : Libelle'
           DISPLAY '   Taille par ligne : 37 octets'
           DISPLAY '   Capacite : ' WS-MAX-AGENCES ' agences'
           DISPLAY ' '

           DISPLAY '2. TABLE-CLIENTS (TCLT)'
           DISPLAY '   - CODE-TCLT     PIC 9(7)   : Code client'
           DISPLAY '   - NOM-TCLT      PIC X(20)  : Nom'
           DISPLAY '   - PRENOM-TCLT   PIC X(20)  : Prenom'
           DISPLAY '   - CODEAG-TCLT   PIC 9(7)   : Code agence'
           DISPLAY '   Taille par ligne : 54 octets'
           DISPLAY '   Capacite : ' WS-MAX-CLIENTS ' clients'
           DISPLAY ' '

           DISPLAY '3. TABLE-RIB (TRIB)'
           DISPLAY '   - CLT-TRIB      PIC 9(7)   : Code client'
           DISPLAY '   - CPTE-TRIB     PIC X(23)  : IBAN/RIB'
           DISPLAY '   - DATE-TRIB     PIC 9(8)   : Date solde'
           DISPLAY '   - SOLDE-TRIB    PIC 9(9)V99: Solde'
           DISPLAY '   - SENS-TRIB     PIC X      : C/D'
           DISPLAY '   Taille par ligne : 50 octets'
           DISPLAY '   Capacite : ' WS-MAX-RIB ' RIB'
           DISPLAY ' '

           DISPLAY '4. TABLE-MVTC (Mouvements)'
           DISPLAY '   - CLT-TMVTC     PIC 9(7)   : Code client'
           DISPLAY '   - DATE-TMVTC    PIC 9(8)   : Date mvt'
           DISPLAY '   - MONT-TMVTC    PIC 9(9)V99: Montant'
           DISPLAY '   - SENS-TMVTC    PIC X      : C/D'
           DISPLAY '   Taille par ligne : 27 octets'
           DISPLAY '   Capacite : ' WS-MAX-MVTC ' mouvements'
           DISPLAY ' '

           DISPLAY '5. TABLE-BUFFER (Global)'
           DISPLAY '   - ID-TABLE      PIC X      : A/C/R/M'
           DISPLAY '   - ENR-BUFFER    PIC X(54)  : Donnees'
           DISPLAY '   Taille par ligne : 55 octets'
           DISPLAY '   Capacite : ' WS-MAX-BUFFER ' lignes'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Demonstration de la validation du code agence
      * Regle : code1 = 7 - (code6 MOD 7)
      *----------------------------------------------------------------
       3000-DEMO-VALIDATION-CODE.
           DISPLAY '--------------------------------------------------'
           DISPLAY 'VALIDATION CODE AGENCE :'
           DISPLAY 'Regle : cle = 7 - (code6 MOD 7)'
           DISPLAY '--------------------------------------------------'
           DISPLAY ' '

      * Test avec code valide : 123456 -> cle = 7 - (123456 mod 7)
      *                                     = 7 - (123456 - 17636*7)
      *                                     = 7 - 4 = 3
      * Donc code complet = 1234563
           MOVE 123456 TO WS-CODE6
           MOVE 3 TO WS-CODE1
           PERFORM 3100-VERIFIER-CODE
           DISPLAY '  Code 1234563 : ' WS-CODE6 WS-CODE1
               ' -> Attendu VALIDE'
           DISPLAY ' '

      * Test avec code invalide : 1234564 (mauvaise cle)
           MOVE 123456 TO WS-CODE6
           MOVE 4 TO WS-CODE1
           PERFORM 3100-VERIFIER-CODE
           DISPLAY '  Code 1234564 : ' WS-CODE6 WS-CODE1
               ' -> Attendu INVALIDE'
           DISPLAY ' '

      * Test avec autre code valide : 100000 -> cle = 7 - 0 = 7
      * Mais 7 mod 7 = 0, donc cle = 7 - 0 = 7... non, 7 n'est pas
      * un chiffre valide pour PIC 9. Recalculons :
      * 100000 mod 7 = 100000 - 14285*7 = 100000 - 99995 = 5
      * cle = 7 - 5 = 2, donc code = 1000002
           MOVE 100000 TO WS-CODE6
           MOVE 2 TO WS-CODE1
           PERFORM 3100-VERIFIER-CODE
           DISPLAY '  Code 1000002 : ' WS-CODE6 WS-CODE1
               ' -> Attendu VALIDE'
           DISPLAY ' '.

      *----------------------------------------------------------------
      * Verification d'un code agence
      *----------------------------------------------------------------
       3100-VERIFIER-CODE.
           COMPUTE WS-RESTE = FUNCTION MOD(WS-CODE6, 7)
           COMPUTE WS-CLE-CALC = 7 - WS-RESTE

           IF WS-CLE-CALC = WS-CODE1
               DISPLAY '  -> CODE VALIDE (cle calculee = ' WS-CLE-CALC
                   ')'
           ELSE
               DISPLAY '  -> CODE INVALIDE (cle attendue = '
                   WS-CLE-CALC ', fournie = ' WS-CODE1 ')'
           END-IF.
