       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERSREV.
       AUTHOR. ROCHA.
      ******************************************************************
      * PROGRAMME : C09-PERSREV
      * OBJET     : Programme principal - Calcul du revenu annuel
      *             Lit un fichier ESDS PERSONNEL et appelle le
      *             sous-programme CALREV pour calculer le revenu.
      * EXERCICE  : TP Chapitre IX - Programmes et Sous-programmes
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * Fichier ESDS (Entry-Sequenced Data Set)
      * Organisation séquentielle, accès séquentiel
      *----------------------------------------------------------------*
           SELECT F-PERSONNEL
               ASSIGN TO 'PERSONNEL.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      *----------------------------------------------------------------*
      * Structure de l'enregistrement PERSONNEL (66 caractères)
      *----------------------------------------------------------------*
       FD F-PERSONNEL.
       01 ENR-PERSONNEL.
          05 ENR-MATRICULE         PIC 9(6).
          05 ENR-NOM               PIC X(15).
          05 ENR-PRENOM            PIC X(15).
          05 ENR-SALAIRE           PIC 9(6).
          05 ENR-PRIMES            PIC 9(6).
          05 ENR-REVENU-ANNUEL     PIC 9(8).
          05 ENR-NUM-SS            PIC 9(10).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Variables de contrôle
      *----------------------------------------------------------------*
       01 WS-FILE-STATUS           PIC XX.
          88 WS-STATUS-OK          VALUE '00'.
          88 WS-STATUS-EOF         VALUE '10'.

       01 WS-FIN-FICHIER           PIC X VALUE 'N'.
          88 FIN-FICHIER           VALUE 'O'.
          88 PAS-FIN-FICHIER       VALUE 'N'.

       01 WS-COMPTEUR              PIC 9(3) VALUE 0.

      *----------------------------------------------------------------*
      * Zone de passage de paramètres pour C09CALREV
      *----------------------------------------------------------------*
       01 WS-PARAM-CALREV.
          05 WS-PARAM-SALAIRE      PIC 9(6).
          05 WS-PARAM-PRIMES       PIC 9(6).
          05 WS-PARAM-REVENU       PIC 9(8).

      *----------------------------------------------------------------*
      * Variables d'édition pour affichage
      *----------------------------------------------------------------*
       01 WS-EDITION.
          05 WS-ED-SALAIRE         PIC ZZ.ZZ9.
          05 WS-ED-PRIMES          PIC ZZ.ZZ9.
          05 WS-ED-REVENU          PIC ZZZ.ZZ.ZZ9.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *----------------------------------------------------------------*
      * Programme principal
      *----------------------------------------------------------------*
       0000-PROGRAMME-PRINCIPAL.
           PERFORM 1000-INITIALISATION
           PERFORM 2000-TRAITEMENT
              UNTIL FIN-FICHIER
           PERFORM 3000-TERMINAISON
           STOP RUN.

      *----------------------------------------------------------------*
      * Initialisation : ouverture fichier et affichage en-tête
      *----------------------------------------------------------------*
       1000-INITIALISATION.
           OPEN INPUT F-PERSONNEL

           IF NOT WS-STATUS-OK
               DISPLAY '*** ERREUR OUVERTURE FICHIER ***'
               DISPLAY 'File Status : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           DISPLAY '=================================================='
           DISPLAY '       CALCUL DU REVENU ANNUEL DES EMPLOYES       '
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY 'MATRIC  NOM             PRENOM          '
                   'SALAIRE   PRIMES    REVENU ANNUEL'
           DISPLAY '------  --------------- --------------- '
                   '--------  --------  -------------'

      *    Première lecture
           PERFORM 2100-LIRE-ENREGISTREMENT.

      *----------------------------------------------------------------*
      * Traitement : pour chaque enregistrement
      *----------------------------------------------------------------*
       2000-TRAITEMENT.
      *    Incrémenter le compteur
           ADD 1 TO WS-COMPTEUR

      *    Préparer les paramètres pour C09CALREV
           MOVE ENR-SALAIRE TO WS-PARAM-SALAIRE
           MOVE ENR-PRIMES  TO WS-PARAM-PRIMES
           INITIALIZE WS-PARAM-REVENU

      *    Appel du sous-programme C09CALREV
           CALL 'C09CALREV' USING WS-PARAM-SALAIRE
                                  WS-PARAM-PRIMES
                                  WS-PARAM-REVENU
           END-CALL

      *    Récupérer le résultat dans l'enregistrement
           MOVE WS-PARAM-REVENU TO ENR-REVENU-ANNUEL

      *    Édition de l'enregistrement
           PERFORM 2200-AFFICHER-ENREGISTREMENT

      *    Lecture suivante
           PERFORM 2100-LIRE-ENREGISTREMENT.

      *----------------------------------------------------------------*
      * Lecture d'un enregistrement
      *----------------------------------------------------------------*
       2100-LIRE-ENREGISTREMENT.
           READ F-PERSONNEL
               AT END
                   SET FIN-FICHIER TO TRUE
               NOT AT END
                   CONTINUE
           END-READ.

      *----------------------------------------------------------------*
      * Affichage formaté d'un enregistrement
      *----------------------------------------------------------------*
       2200-AFFICHER-ENREGISTREMENT.
           MOVE ENR-SALAIRE       TO WS-ED-SALAIRE
           MOVE ENR-PRIMES        TO WS-ED-PRIMES
           MOVE ENR-REVENU-ANNUEL TO WS-ED-REVENU

           DISPLAY ENR-MATRICULE ' '
                   ENR-NOM ' '
                   ENR-PRENOM ' '
                   WS-ED-SALAIRE '  '
                   WS-ED-PRIMES '  '
                   WS-ED-REVENU.

      *----------------------------------------------------------------*
      * Terminaison : fermeture fichier et statistiques
      *----------------------------------------------------------------*
       3000-TERMINAISON.
           CLOSE F-PERSONNEL

           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'Nombre d''enregistrements traites : ' WS-COMPTEUR
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY 'Fin du programme PERSREV'.

