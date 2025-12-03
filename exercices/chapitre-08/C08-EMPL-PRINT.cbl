       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08-EMPPRT.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Lecture et impression fichier EMPLOYE etendu
      *
      * Exercice VIII - Operations E/S sur les Fichiers
      * Affichage format tableau avec colonnes :
      * ID-EMPL | NOM | PRENOM | ADRESSE | DEBIT | CREDIT | SALAIRE
      *
      * Les champs DEBIT, CREDIT, SALAIRE :
      * - 10 caracteres numeriques avec 2 decimales
      * - Affichage avec signe Euro
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZOS.
       OBJECT-COMPUTER. IBM-ZOS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FEMPL ASSIGN TO 'EMPLOYE-EXT.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  FEMPL
           RECORDING MODE IS F
           RECORD CONTAINS 116 CHARACTERS.
       01  REC-EMPL.
           05  ID-EMP             PIC X(06).
           05  NOM-EMP            PIC X(20).
           05  PRENOM-EMP         PIC X(20).
           05  ADR-EMP            PIC X(40).
      *----------------------------------------------------------------
      * Champs monetaires definis avec FILLER pour exercice
      * Chaque champ : 10 car. numeriques, 2 decimales
      *----------------------------------------------------------------
           05  FILLER             PIC 9(8)V99.
           05  FILLER             PIC 9(8)V99.
           05  FILLER             PIC 9(8)V99.

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * FILE STATUS
      *----------------------------------------------------------------
       01  WS-FS                  PIC XX.

      *----------------------------------------------------------------
      * Indicateur fin de fichier
      *----------------------------------------------------------------
       01  WS-FIN-FICHIER         PIC 9 VALUE 0.
           88  FIN-FICHIER        VALUE 1.

      *----------------------------------------------------------------
      * Zone de travail pour lecture avec FILLER nommes
      *----------------------------------------------------------------
       01  WS-EMPL.
           05  WS-ID              PIC X(06).
           05  WS-NOM             PIC X(20).
           05  WS-PRENOM          PIC X(20).
           05  WS-ADR             PIC X(40).
           05  WS-DEBIT           PIC 9(8)V99.
           05  WS-CREDIT          PIC 9(8)V99.
           05  WS-SALAIRE         PIC 9(8)V99.

      *----------------------------------------------------------------
      * Zones d'edition pour affichage avec symbole Euro
      *----------------------------------------------------------------
       01  WS-DEBIT-EDT           PIC Z(5)9.99.
       01  WS-CREDIT-EDT          PIC Z(5)9.99.
       01  WS-SALAIRE-EDT         PIC Z(5)9.99.

      *----------------------------------------------------------------
      * Ligne d'affichage complete
      *----------------------------------------------------------------
       01  WS-LIGNE-AFFICH.
           05  WS-AFF-ID          PIC X(06).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-NOM         PIC X(12).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-PRENOM      PIC X(10).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-ADR         PIC X(20).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-DEBIT       PIC X(12).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-CREDIT      PIC X(12).
           05  FILLER             PIC X VALUE '|'.
           05  WS-AFF-SALAIRE     PIC X(12).

      *----------------------------------------------------------------
      * Compteur
      *----------------------------------------------------------------
       01  WS-CPT-LUS             PIC 9(3) VALUE 0.

      *----------------------------------------------------------------
      * Ligne de separation
      *----------------------------------------------------------------
       01  WS-SEPARATEUR          PIC X(90) VALUE ALL '-'.

      *----------------------------------------------------------------
      * En-tete du tableau
      *----------------------------------------------------------------
       01  WS-ENTETE.
           05  FILLER             PIC X(06) VALUE 'ID-EMP'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(12) VALUE 'NOM'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(10) VALUE 'PRENOM'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(20) VALUE 'ADRESSE'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(12) VALUE 'DEBIT EUR'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(12) VALUE 'CREDIT EUR'.
           05  FILLER             PIC X VALUE '|'.
           05  FILLER             PIC X(12) VALUE 'SALAIRE EUR'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Lecture fichier EMPLOYE - Format Tableau'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-OUVRIR-FICHIER
           PERFORM 2000-AFFICHER-ENTETE
           PERFORM 3000-LIRE-EMPLOYES
           PERFORM 4000-FERMER-FICHIER

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Ouverture en INPUT (lecture)
      *----------------------------------------------------------------
       1000-OUVRIR-FICHIER.
           OPEN INPUT FEMPL

           EVALUATE WS-FS
               WHEN '00'
                   DISPLAY 'Fichier EMPLOYE-EXT.DAT ouvert.'
               WHEN '35'
                   DISPLAY 'Erreur : Fichier non trouve !'
                   DISPLAY 'Executez d''abord C08-EMPL-WRITE'
                   STOP RUN
               WHEN OTHER
                   DISPLAY 'Erreur ouverture fichier : ' WS-FS
                   STOP RUN
           END-EVALUATE.

      *----------------------------------------------------------------
      * Affichage de l'en-tete du tableau
      *----------------------------------------------------------------
       2000-AFFICHER-ENTETE.
           DISPLAY ' '
           DISPLAY WS-SEPARATEUR
           DISPLAY WS-ENTETE
           DISPLAY WS-SEPARATEUR.

      *----------------------------------------------------------------
      * Lecture de tous les employes
      *----------------------------------------------------------------
       3000-LIRE-EMPLOYES.
      * Premiere lecture
           READ FEMPL INTO WS-EMPL
               AT END SET FIN-FICHIER TO TRUE
           END-READ

      * Boucle de lecture
           PERFORM UNTIL FIN-FICHIER
               ADD 1 TO WS-CPT-LUS
               PERFORM 3100-FORMATER-LIGNE
               DISPLAY WS-LIGNE-AFFICH

               READ FEMPL INTO WS-EMPL
                   AT END SET FIN-FICHIER TO TRUE
               END-READ
           END-PERFORM

           DISPLAY WS-SEPARATEUR
           DISPLAY ' '
           DISPLAY 'Total employes lus : ' WS-CPT-LUS.

      *----------------------------------------------------------------
      * Formatage d'une ligne pour affichage
      *----------------------------------------------------------------
       3100-FORMATER-LIGNE.
           INITIALIZE WS-LIGNE-AFFICH

           MOVE WS-ID TO WS-AFF-ID

      * Tronquer le nom a 12 caracteres pour l'affichage
           MOVE WS-NOM(1:12) TO WS-AFF-NOM

      * Tronquer le prenom a 10 caracteres
           MOVE WS-PRENOM(1:10) TO WS-AFF-PRENOM

      * Tronquer l'adresse a 20 caracteres
           MOVE WS-ADR(1:20) TO WS-AFF-ADR

      * Formater les montants avec symbole Euro
           MOVE WS-DEBIT TO WS-DEBIT-EDT
           STRING WS-DEBIT-EDT DELIMITED BY SIZE
                  ' E' DELIMITED BY SIZE
               INTO WS-AFF-DEBIT
           END-STRING

           MOVE WS-CREDIT TO WS-CREDIT-EDT
           STRING WS-CREDIT-EDT DELIMITED BY SIZE
                  ' E' DELIMITED BY SIZE
               INTO WS-AFF-CREDIT
           END-STRING

           MOVE WS-SALAIRE TO WS-SALAIRE-EDT
           STRING WS-SALAIRE-EDT DELIMITED BY SIZE
                  ' E' DELIMITED BY SIZE
               INTO WS-AFF-SALAIRE
           END-STRING.

      *----------------------------------------------------------------
      * Fermeture
      *----------------------------------------------------------------
       4000-FERMER-FICHIER.
           CLOSE FEMPL

           IF WS-FS = '00'
               DISPLAY ' '
               DISPLAY 'Fichier ferme avec succes.'
           ELSE
               DISPLAY 'Erreur fermeture : ' WS-FS
           END-IF.
