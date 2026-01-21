       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGAJT.
      ******************************************************************
      * PROGRAMME : PRGAJT
      * FONCTION  : Ajout d'un nouveau client
      * TRANSACTION : AJOU
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPAJT (MAPSET CLIAJT)
      *
      * MODE PSEUDO-CONVERSATIONNEL :
      *   - Premier passage : Affiche ecran vide pour saisie
      *   - Passages suivants : Valide et enregistre le client
      *   - PF3 : Quitter la transaction
      *
      * CONTROLES EFFECTUES :
      *   - Numero de compte numerique (6 chiffres)
      *   - Pas de doublure (client n'existe pas deja)
      *   - Code region valide (01-04)
      *   - Sexe valide (M ou F)
      *   - Situation sociale valide (C/M/D/V)
      *   - Position valide (DB ou CR)
      *
      * OPTIMISATIONS IMPLEMENTEES :
      * ---------------------------
      * 1. FSET dans BMS : Tous les champs sont renvoyes
      *    -> Simplification des validations (pas de test longueur)
      * 2. DATAONLY : Reaffichage sans renvoyer la structure
      *    -> Optimisation du trafic reseau
      * 3. CURSOR dynamique : Positionnement sur le champ en erreur
      *    -> Meilleure experience utilisateur
      *
      * FIL ROUGE CICS - EXERCICE 7
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-FLAG-INIT         PIC X(01) VALUE 'N'.
              88 PREMIER-PASSAGE   VALUE 'N'.
              88 PASSAGE-SUIVANT   VALUE 'O'.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLIAJT)
      *-----------------------------------------------------------------
       COPY CLIAJT.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT           PIC X(06).
           05 CLI-CODREG           PIC X(02).
           05 CLI-NATCPT           PIC X(02).
           05 CLI-NOM              PIC X(10).
           05 CLI-PRENOM           PIC X(10).
           05 CLI-DATNAISS         PIC X(08).
           05 CLI-SEXE             PIC X(01).
           05 CLI-ACTPRO           PIC X(02).
           05 CLI-SITSO            PIC X(01).
           05 CLI-ADRESSE          PIC X(10).
           05 CLI-SOLDE            PIC X(10).
           05 CLI-POSITION         PIC X(02).
           05 FILLER               PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP                 PIC S9(08) COMP VALUE 0.
       01  WS-ERREUR               PIC X(01) VALUE 'N'.
           88 ERREUR-DETECTEE      VALUE 'O'.
           88 PAS-ERREUR           VALUE 'N'.
       01  WS-MSG-FIN              PIC X(40)
           VALUE 'TRANSACTION AJOU TERMINEE - AU REVOIR'.
      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES (EVITE ECRASEMENT PAR LOW-VALUES)
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-NUMCPT            PIC X(06).
           05 WS-NUMCPTL           PIC S9(04) COMP.
           05 WS-CODREG            PIC X(02).
           05 WS-CODREGL           PIC S9(04) COMP.
           05 WS-NATCPT            PIC X(02).
           05 WS-NOM               PIC X(10).
           05 WS-NOML              PIC S9(04) COMP.
           05 WS-PRENOM            PIC X(10).
           05 WS-DATNAISS          PIC X(08).
           05 WS-SEXE              PIC X(01).
           05 WS-SEXEL             PIC S9(04) COMP.
           05 WS-ACTPRO            PIC X(02).
           05 WS-SITSO             PIC X(01).
           05 WS-SITSOL            PIC S9(04) COMP.
           05 WS-ADRESSE           PIC X(10).
           05 WS-SOLDE             PIC X(10).
           05 WS-POSITION          PIC X(02).
           05 WS-POSITL            PIC S9(04) COMP.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
      * Point d'entree du programme
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN EIBAID = DFHPF3
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN OTHER
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('AJOU')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-PREMIER-PASSAGE.
      *-----------------------------------------------------------------
      * Affichage de l'ecran vide pour saisie
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAJTO
           MOVE 'SAISIR LES DONNEES DU NOUVEAU CLIENT ET VALIDER'
               TO MSGO
           MOVE 'O' TO WS-FLAG-INIT

           EXEC CICS SEND MAP('MAPAJT')
               MAPSET('CLIAJT')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Reception et validation des donnees saisies
      *-----------------------------------------------------------------
           MOVE 'N' TO WS-ERREUR

           EXEC CICS RECEIVE MAP('MAPAJT')
               MAPSET('CLIAJT')
               RESP(WS-RESP)
           END-EXEC

      * Gestion MAPFAIL (aucune donnee transmise)
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPAJTO
               MOVE 'AUCUNE DONNEE SAISIE - VEUILLEZ REMPLIR' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE NUMCPTL   TO WS-NUMCPTL
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NATCPTI   TO WS-NATCPT
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE PRENOMI   TO WS-PRENOM
           MOVE DATNAI    TO WS-DATNAISS
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE ACTPROI   TO WS-ACTPRO
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE ADRESSEI  TO WS-ADRESSE
           MOVE SOLDEI    TO WS-SOLDE
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL

      * Validation des donnees
           PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN

           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verification doublure (client existe deja ?)
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
           IF ERREUR-DETECTEE
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Preparation et ecriture de l'enregistrement
           PERFORM 2300-PREPARER-ENREGISTREMENT
           PERFORM 2400-ECRIRE-CLIENT

           EXEC CICS SEND MAP('MAPAJT')
               MAPSET('CLIAJT')
           END-EXEC.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       2100-VALIDER-DONNEES.
      *-----------------------------------------------------------------
      * Controles de conformite des donnees saisies
      * Utilise les variables WS- sauvegardees (pas MAPAJTI)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAJTO

      * Controle numero de compte (obligatoire et numerique)
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               MOVE -1 TO NUMCPTL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE -1 TO NUMCPTL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle code region (01, 02, 03 ou 04)
           IF WS-CODREGL = 0 OR WS-CODREG = SPACES
               MOVE 'CODE REGION OBLIGATOIRE' TO MSGO
               MOVE -1 TO CODREGL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE -1 TO CODREGL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle nom (obligatoire)
           IF WS-NOML = 0 OR WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE -1 TO NOML
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle sexe (M ou F)
           IF WS-SEXEL = 0 OR WS-SEXE = SPACES
               MOVE 'SEXE OBLIGATOIRE' TO MSGO
               MOVE -1 TO SEXEL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE -1 TO SEXEL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle situation sociale (C, M, D ou V)
           IF WS-SITSOL = 0 OR WS-SITSO = SPACES
               MOVE 'SITUATION SOCIALE OBLIGATOIRE' TO MSGO
               MOVE -1 TO SITSOL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE -1 TO SITSOL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle position (DB ou CR)
           IF WS-POSITL = 0 OR WS-POSITION = SPACES
               MOVE 'POSITION OBLIGATOIRE' TO MSGO
               MOVE -1 TO POSITL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE -1 TO POSITL
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF.

       2100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       2200-VERIFIER-DOUBLURE.
      *-----------------------------------------------------------------
      * Verification que le client n'existe pas deja
      * Note: NOTFND est attendu (client nouveau), NORMAL = doublure
      *-----------------------------------------------------------------
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT EN DOUBLE - CE CLIENT EXISTE DEJA'
                   TO MSGO
               MOVE 'O' TO WS-ERREUR
           END-IF.

       2200-FIN.
           EXIT.

      *-----------------------------------------------------------------
       2300-PREPARER-ENREGISTREMENT.
      *-----------------------------------------------------------------
      * Transfert des donnees sauvegardees vers l'enregistrement
      *-----------------------------------------------------------------
           INITIALIZE ENR-CLIENT

           MOVE WS-NUMCPT    TO CLI-NUMCPT
           MOVE WS-CODREG    TO CLI-CODREG
           MOVE WS-NATCPT    TO CLI-NATCPT
           MOVE WS-NOM       TO CLI-NOM
           MOVE WS-PRENOM    TO CLI-PRENOM
           MOVE WS-DATNAISS  TO CLI-DATNAISS
           MOVE WS-SEXE      TO CLI-SEXE
           MOVE WS-ACTPRO    TO CLI-ACTPRO
           MOVE WS-SITSO     TO CLI-SITSO
           MOVE WS-ADRESSE   TO CLI-ADRESSE
           MOVE WS-SOLDE     TO CLI-SOLDE
           MOVE WS-POSITION  TO CLI-POSITION.

      *-----------------------------------------------------------------
       2400-ECRIRE-CLIENT.
      *-----------------------------------------------------------------
      * Ecriture du nouvel enregistrement dans le fichier VSAM
      *-----------------------------------------------------------------
           EXEC CICS WRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPAJTO
                   MOVE 'CLIENT AJOUTE AVEC SUCCES - NOUVEAU OU PF3'
                       TO MSGO
               WHEN DFHRESP(DUPREC)
                   MOVE 'ENREGISTREMENT EN DOUBLE' TO MSGO
                   MOVE 'O' TO WS-ERREUR
               WHEN OTHER
                   MOVE 'ERREUR ECRITURE FICHIER - CONTACTEZ SUPPORT'
                       TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
      * Fin de la transaction
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
