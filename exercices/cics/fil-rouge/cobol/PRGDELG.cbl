       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGDELG.
      ******************************************************************
      * PROGRAMME : PRGDELG
      * FONCTION  : Suppression generique de clients par prefixe
      * TRANSACTION : DELG
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPDEL (MAPSET CLIDEL)
      *
      * MODE PSEUDO-CONVERSATIONNEL A 2 PHASES :
      * ----------------------------------------
      * Phase 1 (COMPTAGE) :
      *   - Saisie prefixe (1 a 5 car) ou cle complete (6 car)
      *   - Comptage des clients correspondants via STARTBR/READNEXT
      *   - Affichage du nombre de clients trouves
      *
      * Phase 2 (CONFIRMATION) :
      *   - L'utilisateur confirme avec O ou annule avec N
      *   - Si O : Suppression de tous les clients correspondants
      *   - Si N : Retour en phase comptage
      *
      * PARTICULARITE :
      * - PREFIXE en PIC X (pas NUM) pour eviter justification droite
      * - Permet saisie partielle : '1', '11', '111', etc.
      * - Utilise STARTBR/READNEXT pour parcourir les clients
      *
      * OPTIMISATIONS IMPLEMENTEES :
      * ---------------------------
      * 1. FSET dans BMS : PREFIXE et CONFIRM renvoyes automatiquement
      * 2. DATAONLY : Reaffichage sans renvoyer la structure map
      * 3. CURSOR dynamique : Positionnement sur le champ en erreur
      *
      * FIL ROUGE CICS - EXERCICE 17
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase et le prefixe entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-COMPTAGE   VALUE '1'.
              88 PHASE-CONFIRM    VALUE '2'.
           05 WS-PREFIXE-SAVED    PIC X(06) VALUE SPACES.
           05 WS-LONGUEUR-SAVED   PIC 9(01) VALUE 0.
           05 WS-NBCLI-SAVED      PIC 9(05) VALUE 0.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLIDEL)
      *-----------------------------------------------------------------
       COPY CLIDEL.

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
       01  WS-MSG-FIN             PIC X(40)
           VALUE 'TRANSACTION DELG TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * VARIABLES POUR LA NAVIGATION VSAM
      *-----------------------------------------------------------------
       01  WS-BROWSE.
           05 WS-CLE-DEBUT        PIC X(06) VALUE SPACES.
           05 WS-CLE-COURANTE     PIC X(06) VALUE SPACES.
           05 WS-FIN-BROWSE       PIC X(01) VALUE 'N'.
              88 FIN-BROWSE       VALUE 'O'.
              88 PAS-FIN-BROWSE   VALUE 'N'.

      *-----------------------------------------------------------------
      * COMPTEURS
      *-----------------------------------------------------------------
       01  WS-COMPTEURS.
           05 WS-COMPTEUR         PIC 9(05) VALUE 0.
           05 WS-COMPTEUR-SUP     PIC 9(05) VALUE 0.

      *-----------------------------------------------------------------
      * TABLE DES CLES A SUPPRIMER (max 100 clients)
      *-----------------------------------------------------------------
       01  WS-TABLE-CLES.
           05 WS-NB-CLES          PIC 9(03) VALUE 0.
           05 WS-CLES OCCURS 100 TIMES.
              10 WS-CLE-SUP       PIC X(06).
       01  WS-IDX-SUP             PIC 9(03) VALUE 0.

      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-PREFIXE          PIC X(06).
           05 WS-PREFIXEL         PIC S9(04) COMP.
           05 WS-CONFIRM          PIC X(01).
           05 WS-CONFIRML         PIC S9(04) COMP.

      *-----------------------------------------------------------------
      * LONGUEUR DU PREFIXE SAISI
      *-----------------------------------------------------------------
       01  WS-LONGUEUR            PIC 9(01) VALUE 0.
       01  WS-INDEX               PIC 9(01) VALUE 0.

      *-----------------------------------------------------------------
      * MESSAGE FORMATE
      *-----------------------------------------------------------------
       01  WS-MSG-RESULT          PIC X(60) VALUE SPACES.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
           05 LS-PHASE            PIC X(01).
           05 LS-PREFIXE-SAVED    PIC X(06).
           05 LS-LONGUEUR-SAVED   PIC 9(01).
           05 LS-NBCLI-SAVED      PIC 9(05).

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
      *            Premier appel - Phase comptage
                   PERFORM 1000-INIT-COMPTAGE
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT-COMPTAGE
               WHEN OTHER
      *            Traitement selon la phase en cours
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('DELG')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT-COMPTAGE.
      *-----------------------------------------------------------------
      * Affichage ecran initial pour saisie prefixe
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPDELO
           MOVE 'SAISIR PREFIXE (1-5 CAR) OU CLE COMPLETE (6 CAR)'
               TO MSGO
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-PREFIXE-SAVED
           MOVE 0 TO WS-LONGUEUR-SAVED
           MOVE 0 TO WS-NBCLI-SAVED

           EXEC CICS SEND MAP('MAPDEL')
               MAPSET('CLIDEL')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la phase en cours
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN PHASE-COMPTAGE
                   PERFORM 3000-COMPTER-CLIENTS THRU 3000-FIN
               WHEN PHASE-CONFIRM
                   PERFORM 4000-CONFIRMER-SUPPRESSION THRU 4000-FIN
           END-EVALUATE.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-COMPTER-CLIENTS.
      *-----------------------------------------------------------------
      * Phase 1 : Comptage des clients correspondant au prefixe
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPDEL')
               MAPSET('CLIDEL')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'VEUILLEZ SAISIR UN PREFIXE' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du prefixe saisi
           MOVE PREFIXEI TO WS-PREFIXE
           MOVE PREFIXEL TO WS-PREFIXEL

      *    Controle prefixe non vide
           IF WS-PREFIXEL = 0 OR WS-PREFIXE = SPACES
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'PREFIXE OBLIGATOIRE (1 A 6 CARACTERES)' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calcul de la longueur effective du prefixe
           PERFORM 3050-CALCULER-LONGUEUR THRU 3050-FIN

      *    Controle : au moins 1 caractere
           IF WS-LONGUEUR = 0
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'PREFIXE INVALIDE - MIN 1 CARACTERE' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Comptage des clients via STARTBR/READNEXT
           PERFORM 3100-PARCOURIR-FICHIER THRU 3100-FIN

      *    Affichage du resultat
           MOVE LOW-VALUES TO MAPDELO
           MOVE WS-PREFIXE TO PREFIXEO
           MOVE WS-COMPTEUR TO NBCLIO

           IF WS-COMPTEUR = 0
               MOVE 'AUCUN CLIENT TROUVE - SAISIR AUTRE PREFIXE' TO MSGO
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Clients trouves - Passage en phase CONFIRMATION
           STRING WS-COMPTEUR DELIMITED BY SIZE
               ' CLIENT(S) TROUVE(S) - CONFIRMER SUPPRESSION (O/N) ?'
               DELIMITED BY SIZE
               INTO WS-MSG-RESULT
           MOVE WS-MSG-RESULT TO MSGO

      *    Sauvegarde pour la phase suivante
           MOVE '2' TO WS-PHASE
           MOVE WS-PREFIXE TO WS-PREFIXE-SAVED
           MOVE WS-LONGUEUR TO WS-LONGUEUR-SAVED
           MOVE WS-COMPTEUR TO WS-NBCLI-SAVED

           EXEC CICS SEND MAP('MAPDEL')
               MAPSET('CLIDEL')
               ERASE
           END-EXEC.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3050-CALCULER-LONGUEUR.
      *-----------------------------------------------------------------
      * Calcule la longueur effective du prefixe (sans espaces finaux)
      *-----------------------------------------------------------------
           MOVE 0 TO WS-LONGUEUR

      *    Parcours de droite a gauche pour trouver le dernier non-espace
           PERFORM VARYING WS-INDEX FROM 6 BY -1
               UNTIL WS-INDEX < 1 OR WS-LONGUEUR > 0
               IF WS-PREFIXE(WS-INDEX:1) NOT = SPACE
                   MOVE WS-INDEX TO WS-LONGUEUR
               END-IF
           END-PERFORM.

       3050-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-PARCOURIR-FICHIER.
      *-----------------------------------------------------------------
      * Parcours du fichier pour compter les clients correspondants
      *-----------------------------------------------------------------
           MOVE 0 TO WS-COMPTEUR
           MOVE 'N' TO WS-FIN-BROWSE

      *    Construction de la cle de debut (prefixe complete par des 0)
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT

      *    Completer avec des zeros pour le GTEQ
           MOVE WS-LONGUEUR TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

      *    Positionnement sur le premier client >= prefixe
           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
      *        Aucun enregistrement trouve
               MOVE 0 TO WS-COMPTEUR
               GO TO 3100-FIN
           END-IF

      *    Initialiser la cle courante pour READNEXT
           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

      *    Boucle de lecture
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
      *                Fin de fichier atteinte
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
      *                Autre erreur
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR) NOT =
                       WS-PREFIXE(1:WS-LONGUEUR)
      *                Cle ne correspond plus au prefixe
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Client correspondant trouve
                       ADD 1 TO WS-COMPTEUR
               END-EVALUATE
           END-PERFORM

      *    Fermeture du browse
           EXEC CICS ENDBR
               FILE('FCLIENT')
           END-EXEC.

       3100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4000-CONFIRMER-SUPPRESSION.
      *-----------------------------------------------------------------
      * Phase 2 : Reception de la confirmation et suppression
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPDEL')
               MAPSET('CLIDEL')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPDELO
               MOVE WS-PREFIXE-SAVED TO PREFIXEO
               MOVE WS-NBCLI-SAVED TO NBCLIO
               MOVE 'VEUILLEZ REPONDRE O OU N' TO MSGO
               MOVE -1 TO CONFIRML
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Sauvegarde de la confirmation
           MOVE CONFIRMI TO WS-CONFIRM
           MOVE CONFIRML TO WS-CONFIRML

      *    Verification de la reponse
           IF WS-CONFIRM NOT = 'O' AND WS-CONFIRM NOT = 'N'
              AND WS-CONFIRM NOT = 'o' AND WS-CONFIRM NOT = 'n'
               MOVE LOW-VALUES TO MAPDELO
               MOVE WS-PREFIXE-SAVED TO PREFIXEO
               MOVE WS-NBCLI-SAVED TO NBCLIO
               MOVE 'REPONSE INVALIDE - SAISIR O OU N' TO MSGO
               MOVE -1 TO CONFIRML
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Si N ou n : Annulation
           IF WS-CONFIRM = 'N' OR WS-CONFIRM = 'n'
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'SUPPRESSION ANNULEE - NOUVEAU PREFIXE OU PF3'
                   TO MSGO
               MOVE '1' TO WS-PHASE
               MOVE SPACES TO WS-PREFIXE-SAVED
               MOVE 0 TO WS-LONGUEUR-SAVED
               MOVE 0 TO WS-NBCLI-SAVED
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Si O ou o : Suppression
           PERFORM 4100-SUPPRIMER-CLIENTS THRU 4100-FIN.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4100-SUPPRIMER-CLIENTS.
      *-----------------------------------------------------------------
      * Suppression effective de tous les clients correspondants
      * Methode : Collecter les cles d'abord, puis supprimer apres ENDBR
      * (evite le deadlock READNEXT/DELETE)
      *-----------------------------------------------------------------
           MOVE 0 TO WS-COMPTEUR-SUP
           MOVE 0 TO WS-NB-CLES
           MOVE 'N' TO WS-FIN-BROWSE

      *    Restaurer le prefixe et la longueur
           MOVE WS-PREFIXE-SAVED TO WS-PREFIXE
           MOVE WS-LONGUEUR-SAVED TO WS-LONGUEUR

      *    Construction de la cle de debut
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT

      *    Completer avec des zeros
           MOVE WS-LONGUEUR TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

      *    Positionnement sur le premier client >= prefixe
           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'ERREUR POSITIONNEMENT - AUCUNE SUPPRESSION'
                   TO MSGO
               MOVE '1' TO WS-PHASE
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
               END-EXEC
               GO TO 4100-FIN
           END-IF

      *    Initialiser la cle courante pour READNEXT
           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

      *    PHASE 1 : Collecter les cles dans la table (sans UPDATE)
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR) NOT =
                       WS-PREFIXE(1:WS-LONGUEUR)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-NB-CLES >= 100
      *                Table pleine - on s'arrete
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Stocker la cle dans la table
                       ADD 1 TO WS-NB-CLES
                       MOVE WS-CLE-COURANTE TO WS-CLE-SUP(WS-NB-CLES)
               END-EVALUATE
           END-PERFORM

      *    Fermeture du browse AVANT les suppressions
           EXEC CICS ENDBR
               FILE('FCLIENT')
           END-EXEC

      *    PHASE 2 : Supprimer chaque cle collectee
           PERFORM VARYING WS-IDX-SUP FROM 1 BY 1
               UNTIL WS-IDX-SUP > WS-NB-CLES
               MOVE WS-CLE-SUP(WS-IDX-SUP) TO WS-CLE-COURANTE
               EXEC CICS DELETE
                   FILE('FCLIENT')
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   ADD 1 TO WS-COMPTEUR-SUP
               END-IF
           END-PERFORM

      *    Affichage du resultat
           MOVE LOW-VALUES TO MAPDELO
           MOVE SPACES TO WS-MSG-RESULT

           IF WS-NB-CLES >= 100
      *        Limite atteinte - prevenir l'utilisateur
               STRING WS-COMPTEUR-SUP DELIMITED BY SIZE
                   ' SUPPR. (LIMITE 100) - RELANCER POUR CONTINUER'
                   DELIMITED BY SIZE
                   INTO WS-MSG-RESULT
           ELSE
               STRING WS-COMPTEUR-SUP DELIMITED BY SIZE
                   ' CLIENT(S) SUPPRIME(S) - NOUVEAU PREFIXE OU PF3'
                   DELIMITED BY SIZE
                   INTO WS-MSG-RESULT
           END-IF
           MOVE WS-MSG-RESULT TO MSGO

      *    Retour en phase comptage
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-PREFIXE-SAVED
           MOVE 0 TO WS-LONGUEUR-SAVED
           MOVE 0 TO WS-NBCLI-SAVED

           EXEC CICS SEND MAP('MAPDEL')
               MAPSET('CLIDEL')
               ERASE
           END-EXEC.

       4100-FIN.
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
