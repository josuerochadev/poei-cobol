       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMAJ.
      ******************************************************************
      * PROGRAMME : PRGMAJ
      * FONCTION  : Mise a jour d'un client existant
      * TRANSACTION : MAJO
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPMAJ (MAPSET CLIMAJ)
      *
      * MODE PSEUDO-CONVERSATIONNEL A 3 PHASES :
      * ----------------------------------------
      * Phase 1 (RECHERCHE) :
      *   - Affiche ecran vide pour saisie numero compte
      *   - NUMCPT en UNPROT (saisissable)
      *   - Autres champs vides
      *
      * Phase 2 (AFFICHAGE) :
      *   - Lit le client avec READ UPDATE (verrouillage)
      *   - Affiche les donnees actuelles
      *   - NUMCPT passe en ASKIP (protege, cle non modifiable)
      *   - Autres champs en UNPROT pour modification
      *
      * Phase 3 (VALIDATION) :
      *   - Recoit les modifications
      *   - Valide les donnees
      *   - REWRITE pour sauvegarder
      *
      * DIFFERENCE AVEC AJOUT (WRITE) :
      * - READ UPDATE obligatoire avant REWRITE
      * - La cle (NUMCPT) ne peut pas etre modifiee
      * - Le client doit exister (pas de creation)
      *
      * OPTIMISATIONS IMPLEMENTEES :
      * ---------------------------
      * 1. FSET dans BMS : Tous les champs sont renvoyes par le terminal
      *    -> Plus besoin de relire le fichier pour fusionner
      *    -> Suppression de la logique de fusion (80 lignes en moins)
      * 2. DATAONLY : Reaffichage sans renvoyer la structure de l'ecran
      *    -> Optimisation du trafic reseau
      * 3. CURSOR dynamique : Positionnement sur le champ en erreur
      *    -> Meilleure experience utilisateur
      *
      * FIL ROUGE CICS - EXERCICE 10
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase et le numero de compte entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-RECHERCHE  VALUE '1'.
              88 PHASE-AFFICHAGE  VALUE '2'.
              88 PHASE-VALIDATION VALUE '3'.
           05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLIMAJ)
      *-----------------------------------------------------------------
       COPY CLIMAJ.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT          PIC X(06).
           05 CLI-CODREG          PIC X(02).
           05 CLI-NATCPT          PIC X(02).
           05 CLI-NOM             PIC X(10).
           05 CLI-PRENOM          PIC X(10).
           05 CLI-DATNAISS        PIC X(08).
           05 CLI-SEXE            PIC X(01).
           05 CLI-ACTPRO          PIC X(02).
           05 CLI-SITSO           PIC X(01).
           05 CLI-ADRESSE         PIC X(10).
           05 CLI-SOLDE           PIC X(10).
           05 CLI-POSITION        PIC X(02).
           05 FILLER              PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP                PIC S9(08) COMP VALUE 0.
       01  WS-ERREUR              PIC X(01) VALUE 'N'.
           88 ERREUR-DETECTEE     VALUE 'O'.
           88 PAS-ERREUR          VALUE 'N'.
       01  WS-MSG-FIN             PIC X(40)
           VALUE 'TRANSACTION MAJO TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES (EVITE ECRASEMENT PAR LOW-VALUES)
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-NUMCPT           PIC X(06).
           05 WS-NUMCPTL          PIC S9(04) COMP.
           05 WS-CODREG           PIC X(02).
           05 WS-CODREGL          PIC S9(04) COMP.
           05 WS-NATCPT           PIC X(02).
           05 WS-NOM              PIC X(10).
           05 WS-NOML             PIC S9(04) COMP.
           05 WS-PRENOM           PIC X(10).
           05 WS-DATNAISS         PIC X(08).
           05 WS-DATNAISSL        PIC S9(04) COMP.
           05 WS-SEXE             PIC X(01).
           05 WS-SEXEL            PIC S9(04) COMP.
           05 WS-ACTPRO           PIC X(02).
           05 WS-SITSO            PIC X(01).
           05 WS-SITSOL           PIC S9(04) COMP.
           05 WS-ADRESSE          PIC X(10).
           05 WS-SOLDE            PIC X(10).
           05 WS-POSITION         PIC X(02).
           05 WS-POSITL           PIC S9(04) COMP.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
      * OBLIGATOIRE pour acceder aux donnees du RETURN precedent
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
           05 LS-PHASE            PIC X(01).
           05 LS-NUMCPT-SAVED     PIC X(06).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
      * Point d'entree du programme
      * Gestion du mode pseudo-conversationnel
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Phase recherche
                   PERFORM 1000-INIT-RECHERCHE
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT-RECHERCHE
               WHEN OTHER
      *            Traitement selon la phase en cours
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('MAJO')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT-RECHERCHE.
      *-----------------------------------------------------------------
      * Affichage ecran initial pour saisie numero compte
      * NUMCPT en UNPROT (saisissable)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE 'SAISIR LE NUMERO DE COMPTE A MODIFIER' TO MSGO
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-NUMCPT-SAVED

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la phase en cours
      *-----------------------------------------------------------------
           MOVE 'N' TO WS-ERREUR

           EVALUATE TRUE
               WHEN PHASE-RECHERCHE
                   PERFORM 3000-RECHERCHER-CLIENT THRU 3000-FIN
               WHEN PHASE-AFFICHAGE
               WHEN PHASE-VALIDATION
                   PERFORM 4000-VALIDER-MODIFICATION THRU 4000-FIN
           END-EVALUATE.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-RECHERCHER-CLIENT.
      *-----------------------------------------------------------------
      * Phase 1 -> 2 : Recherche du client par son numero
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du numero saisi
           MOVE NUMCPTI TO WS-NUMCPT
           MOVE NUMCPTL TO WS-NUMCPTL

      *    Controle numero de compte
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Lecture du client (sans UPDATE car on affiche seulement)
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Client trouve - Affichage des donnees
           PERFORM 3100-AFFICHER-CLIENT

      *    Passage en phase AFFICHAGE/VALIDATION
           MOVE '2' TO WS-PHASE
           MOVE WS-NUMCPT TO WS-NUMCPT-SAVED.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Affiche les donnees du client dans la MAP
      * NUMCPT passe en ASKIP (protege)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO

      *    Transfert des donnees vers la MAP
           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NATCPT   TO NATCPTO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-ACTPRO   TO ACTPROO
           MOVE CLI-SITSO    TO SITSOO
           MOVE CLI-ADRESSE  TO ADRESSEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITO

      *    IMPORTANT : Proteger le numero de compte (cle non modifiable)
      *    DFHBMASK = X'20' = ASKIP (protege, intensite normale)
           MOVE DFHBMASK TO NUMCPTA

           MOVE 'CLIENT TROUVE - MODIFIER ET VALIDER AVEC ENTER' TO MSGO

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       4000-VALIDER-MODIFICATION.
      *-----------------------------------------------------------------
      * Phase 2/3 : Reception et validation des modifications
      *
      * IMPORTANT - MISE A JOUR vs AJOUT :
      * En mise a jour, l'utilisateur ne modifie que certains champs.
      * Les champs non modifies ont une longueur = 0 (terminal n'envoie
      * que les champs modifies). On doit donc :
      *   1. Relire le client pour avoir ses donnees actuelles
      *   2. Ne remplacer que les champs modifies (longueur > 0)
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE WS-NUMCPT-SAVED TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'AUCUNE MODIFICATION - ENTREZ DES DONNEES' TO MSGO
               MOVE -1 TO CODREGL
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    SAUVEGARDE DES DONNEES MAP AVANT ECRASEMENT PAR LOW-VALUES
           MOVE WS-NUMCPT-SAVED TO WS-NUMCPT
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NATCPTI   TO WS-NATCPT
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE PRENOMI   TO WS-PRENOM
           MOVE DATNAI    TO WS-DATNAISS
           MOVE DATNAL    TO WS-DATNAISSL
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE ACTPROI   TO WS-ACTPRO
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE ADRESSEI  TO WS-ADRESSE
           MOVE SOLDEI    TO WS-SOLDE
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL

      *    AVEC FSET : Tous les champs sont renvoyes par le terminal,
      *    plus besoin de relire le fichier pour fusionner les donnees.

      *    Validation des donnees saisies
           PERFORM 4100-VALIDER-DONNEES THRU 4100-FIN

           IF ERREUR-DETECTEE
               MOVE DFHBMASK TO NUMCPTA
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Ecriture de l'enregistrement
           PERFORM 4300-ECRIRE-MODIFICATION THRU 4300-FIN

           MOVE DFHBMASK TO NUMCPTA
           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4100-VALIDER-DONNEES.
      *-----------------------------------------------------------------
      * Controles de conformite des donnees saisies
      * AVEC FSET : Les variables WS-* contiennent toutes les valeurs
      * de l'ecran (modifiees ou non). Plus besoin de fusion.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE WS-NUMCPT TO NUMCPTO

      *    Controle code region (01, 02, 03 ou 04)
           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE -1 TO CODREGL
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle nom (obligatoire)
           IF WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE -1 TO NOML
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle sexe (M ou F)
           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE -1 TO SEXEL
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle situation sociale (C, M, D ou V)
           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE -1 TO SITSOL
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle position (DB ou CR)
           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE -1 TO POSITL
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF.

       4100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4300-ECRIRE-MODIFICATION.
      *-----------------------------------------------------------------
      * Mise a jour de l'enregistrement avec READ UPDATE + REWRITE
      *
      * IMPORTANT : Le REWRITE necessite un READ UPDATE prealable
      * dans la meme unite de travail (UOW).
      *
      * Les variables WS-* contiennent les donnees finales (apres fusion
      * des modifications utilisateur avec les donnees actuelles).
      *-----------------------------------------------------------------
      *    READ UPDATE pour verrouiller l'enregistrement
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR VERROUILLAGE - REESSAYEZ' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4300-FIN
           END-IF

      *    Reappliquer les modifications sur l'enregistrement lu
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
           MOVE WS-POSITION  TO CLI-POSITION

      *    REWRITE - Mise a jour effective
           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPMAJO
                   MOVE WS-NUMCPT TO NUMCPTO
                   MOVE 'MISE A JOUR EFFECTUEE - NOUVEAU OU PF3'
                       TO MSGO
      *            Retour en phase recherche pour nouveau client
                   MOVE '1' TO WS-PHASE
                   MOVE SPACES TO WS-NUMCPT-SAVED
               WHEN OTHER
                   MOVE 'ERREUR MISE A JOUR - CONTACTEZ SUPPORT' TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.

       4300-FIN.
           EXIT.

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
