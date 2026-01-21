# Annexe 2 - Partie 2 : Programmes COBOL (Suppression, Liste, Statistiques)

---

## Navigation

- [Retour au rapport principal](rapport-fil-rouge-cics.md)
- [Annexe 1 - Maps BMS](annexes-1-maps-bms.md)
- [Annexe 2 - Partie 1 : Programmes COBOL (Consultation, Creation, Modification)](annexes-2-partie1.md)
- **Annexe 2 - Partie 2 : Programmes COBOL (Suppression, Liste, Statistiques)** (vous etes ici)
- [Annexe 3 - JCL](annexes-3-jcl.md)

---

## Table des matieres

1. [PRGSUP - Suppression d'un client](#1-prgsup---suppression-dun-client)
2. [PRGDELG - Suppression generique par prefixe](#2-prgdelg---suppression-generique-par-prefixe)
3. [PRGLGEN - Liste generique avec pagination](#3-prglgen---liste-generique-avec-pagination)
4. [PRGSTAT - Statistiques par region](#4-prgstat---statistiques-par-region)

---

## 1. PRGSUP - Suppression d'un client

**Transaction CICS** : `SUPP`

**Fonction** : Suppression d'un client existant avec confirmation

**Exercice** : Fil rouge CICS - Exercice 13

**Description** : Ce programme permet de supprimer un client du fichier FCLIENT. Il fonctionne en mode pseudo-conversationnel a 2 phases :
- Phase 1 (Recherche) : Saisie du numero de compte a supprimer
- Phase 2 (Confirmation) : Affichage des donnees du client et demande de confirmation (O/N)

La commande CICS DELETE est utilisee sans READ UPDATE prealable.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGSUP.
      ******************************************************************
      * PROGRAMME : PRGSUP
      * FONCTION  : Suppression d'un client existant
      * TRANSACTION : SUPP
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPSUP (MAPSET CLISUP)
      *
      * MODE PSEUDO-CONVERSATIONNEL A 2 PHASES :
      * ----------------------------------------
      * Phase 1 (RECHERCHE) :
      *   - Affiche ecran vide pour saisie numero compte
      *   - NUMCPT en UNPROT (saisissable)
      *   - Autres champs vides
      *
      * Phase 2 (CONFIRMATION) :
      *   - Lit le client et affiche ses donnees
      *   - L'utilisateur confirme avec O ou annule avec N
      *   - Si O : DELETE pour supprimer l'enregistrement
      *   - Si N : Retour en phase recherche
      *
      * COMMANDE CICS DELETE :
      * - Ne necessite PAS de READ UPDATE prealable
      * - Supprime directement par la cle (RIDFLD)
      * - Erreur NOTFND si le client n'existe pas
      *
      * FIL ROUGE CICS - EXERCICE 13
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
              88 PHASE-CONFIRM    VALUE '2'.
           05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLISUP)
      *-----------------------------------------------------------------
       COPY CLISUP.

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
           VALUE 'TRANSACTION SUPP TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-NUMCPT           PIC X(06).
           05 WS-NUMCPTL          PIC S9(04) COMP.
           05 WS-CONFIRM          PIC X(01).
           05 WS-CONFIRML         PIC S9(04) COMP.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
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
               TRANSID('SUPP')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT-RECHERCHE.
      *-----------------------------------------------------------------
      * Affichage ecran initial pour saisie numero compte
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSUPO
           MOVE 'SAISIR LE NUMERO DE COMPTE A SUPPRIMER' TO MSGO
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-NUMCPT-SAVED

           EXEC CICS SEND MAP('MAPSUP')
               MAPSET('CLISUP')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la phase en cours
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN PHASE-RECHERCHE
                   PERFORM 3000-RECHERCHER-CLIENT THRU 3000-FIN
               WHEN PHASE-CONFIRM
                   PERFORM 4000-CONFIRMER-SUPPRESSION THRU 4000-FIN
           END-EVALUATE.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-RECHERCHER-CLIENT.
      *-----------------------------------------------------------------
      * Phase 1 -> 2 : Recherche du client par son numero
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPSUP')
               MAPSET('CLISUP')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du numero saisi
           MOVE NUMCPTI TO WS-NUMCPT
           MOVE NUMCPTL TO WS-NUMCPTL

      *    Controle numero de compte
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Lecture du client pour affichage
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Client trouve - Affichage des donnees
           PERFORM 3100-AFFICHER-CLIENT

      *    Passage en phase CONFIRMATION
           MOVE '2' TO WS-PHASE
           MOVE WS-NUMCPT TO WS-NUMCPT-SAVED.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Affiche les donnees du client dans la MAP pour confirmation
      * Tous les champs en ASKIP (lecture seule)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSUPO

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

      *    Libelle region
           EVALUATE CLI-CODREG
               WHEN '01'
                   MOVE 'PARIS' TO LIBREGO
               WHEN '02'
                   MOVE 'MARSEILLE' TO LIBREGO
               WHEN '03'
                   MOVE 'LYON' TO LIBREGO
               WHEN '04'
                   MOVE 'LILLE' TO LIBREGO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBREGO
           END-EVALUATE

      *    Libelle nature compte
           EVALUATE CLI-NATCPT
               WHEN '01'
                   MOVE 'COURANT' TO LIBNATO
               WHEN '02'
                   MOVE 'EPARGNE' TO LIBNATO
               WHEN '03'
                   MOVE 'PROFESSIONNEL' TO LIBNATO
               WHEN OTHER
                   MOVE 'AUTRE' TO LIBNATO
           END-EVALUATE

      *    Libelle sexe
           EVALUATE CLI-SEXE
               WHEN 'M'
                   MOVE 'MASCULIN' TO LIBSEXO
               WHEN 'F'
                   MOVE 'FEMININ' TO LIBSEXO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBSEXO
           END-EVALUATE

      *    Libelle situation sociale
           EVALUATE CLI-SITSO
               WHEN 'C'
                   MOVE 'CELIBATAIRE' TO LIBSITO
               WHEN 'M'
                   MOVE 'MARIE(E)' TO LIBSITO
               WHEN 'D'
                   MOVE 'DIVORCE(E)' TO LIBSITO
               WHEN 'V'
                   MOVE 'VEUF(VE)' TO LIBSITO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBSITO
           END-EVALUATE

      *    Libelle position
           EVALUATE CLI-POSITION
               WHEN 'CR'
                   MOVE 'CREDITEUR' TO LIBPOSO
               WHEN 'DB'
                   MOVE 'DEBITEUR' TO LIBPOSO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBPOSO
           END-EVALUATE

      *    Proteger le numero de compte
           MOVE DFHBMASK TO NUMCPTA

           MOVE 'CLIENT TROUVE - CONFIRMER SUPPRESSION (O/N) ?' TO MSGO

           EXEC CICS SEND MAP('MAPSUP')
               MAPSET('CLISUP')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       4000-CONFIRMER-SUPPRESSION.
      *-----------------------------------------------------------------
      * Phase 2 : Reception de la confirmation et suppression
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPSUP')
               MAPSET('CLISUP')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPSUPO
               MOVE WS-NUMCPT-SAVED TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'VEUILLEZ REPONDRE O OU N' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Sauvegarde de la confirmation
           MOVE CONFIRMI TO WS-CONFIRM
           MOVE CONFIRML TO WS-CONFIRML

      *    Verification de la reponse
           IF WS-CONFIRM NOT = 'O' AND WS-CONFIRM NOT = 'N'
              AND WS-CONFIRM NOT = 'o' AND WS-CONFIRM NOT = 'n'
               MOVE LOW-VALUES TO MAPSUPO
               MOVE WS-NUMCPT-SAVED TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'REPONSE INVALIDE - SAISIR O OU N' TO MSGO
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Si N ou n : Annulation
           IF WS-CONFIRM = 'N' OR WS-CONFIRM = 'n'
               MOVE LOW-VALUES TO MAPSUPO
               MOVE 'SUPPRESSION ANNULEE - NOUVEAU NUMERO OU PF3' TO MSGO
               MOVE '1' TO WS-PHASE
               MOVE SPACES TO WS-NUMCPT-SAVED
               EXEC CICS SEND MAP('MAPSUP')
                   MAPSET('CLISUP')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Si O ou o : Suppression
           PERFORM 4100-SUPPRIMER-CLIENT THRU 4100-FIN.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4100-SUPPRIMER-CLIENT.
      *-----------------------------------------------------------------
      * Suppression effective de l'enregistrement
      * La commande DELETE ne necessite PAS de READ UPDATE prealable
      *-----------------------------------------------------------------
           MOVE WS-NUMCPT-SAVED TO CLI-NUMCPT

           EXEC CICS DELETE
               FILE('FCLIENT')
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPSUPO
                   MOVE 'CLIENT SUPPRIME - NOUVEAU NUMERO OU PF3' TO MSGO
      *            Retour en phase recherche
                   MOVE '1' TO WS-PHASE
                   MOVE SPACES TO WS-NUMCPT-SAVED
               WHEN DFHRESP(NOTFND)
                   MOVE LOW-VALUES TO MAPSUPO
                   MOVE 'ERREUR : CLIENT DEJA SUPPRIME' TO MSGO
                   MOVE '1' TO WS-PHASE
                   MOVE SPACES TO WS-NUMCPT-SAVED
               WHEN OTHER
                   MOVE LOW-VALUES TO MAPSUPO
                   MOVE WS-NUMCPT-SAVED TO NUMCPTO
                   MOVE DFHBMASK TO NUMCPTA
                   MOVE 'ERREUR SUPPRESSION - CONTACTEZ SUPPORT' TO MSGO
           END-EVALUATE

           EXEC CICS SEND MAP('MAPSUP')
               MAPSET('CLISUP')
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
```

---

## 2. PRGDELG - Suppression generique par prefixe

**Transaction CICS** : `DELG`

**Fonction** : Suppression generique de clients par prefixe

**Exercice** : Fil rouge CICS - Exercice 17

**Description** : Ce programme permet de supprimer plusieurs clients dont le numero de compte commence par un prefixe donne (1 a 5 caracteres) ou une cle complete (6 caracteres). Il fonctionne en mode pseudo-conversationnel a 2 phases :
- Phase 1 (Comptage) : Saisie du prefixe et comptage des clients correspondants via STARTBR/READNEXT
- Phase 2 (Confirmation) : L'utilisateur confirme la suppression de tous les clients trouves

La methode utilisee collecte d'abord les cles dans une table (max 100), puis supprime apres ENDBR pour eviter le deadlock READNEXT/DELETE.

```cobol
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
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
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
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calcul de la longueur effective du prefixe
           PERFORM 3050-CALCULER-LONGUEUR THRU 3050-FIN

      *    Controle : au moins 1 caractere
           IF WS-LONGUEUR = 0
               MOVE LOW-VALUES TO MAPDELO
               MOVE 'PREFIXE INVALIDE - MIN 1 CARACTERE' TO MSGO
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
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
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
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
               EXEC CICS SEND MAP('MAPDEL')
                   MAPSET('CLIDEL')
                   ERASE
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
```

---

## 3. PRGLGEN - Liste generique avec pagination

**Transaction CICS** : `LGEN`

**Fonction** : Liste generique des clients par prefixe avec pagination

**Exercice** : Fil rouge CICS - Exercice 18

**Description** : Ce programme permet d'afficher une liste paginee des clients dont le numero de compte commence par un prefixe donne. Il fonctionne en mode pseudo-conversationnel avec :
- Saisie d'un prefixe (1 a 6 caracteres)
- Affichage de 10 clients par page
- Navigation : PF7 (page precedente) / PF8 (page suivante)

La COMMAREA sauvegarde le prefixe et la position de navigation pour permettre de parcourir tout le fichier par pages.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGLGEN.
      ******************************************************************
      * PROGRAMME : PRGLGEN
      * FONCTION  : Liste generique des clients par prefixe
      * TRANSACTION : LGEN
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPLGEN (MAPSET CLILIST)
      *
      * MODE PSEUDO-CONVERSATIONNEL AVEC PAGINATION :
      * ---------------------------------------------
      * - Saisie d'un prefixe (1 a 6 caracteres)
      * - Affichage de 10 clients par page
      * - Navigation : PF7 (page prec.) / PF8 (page suiv.)
      *
      * COMMAREA :
      * - Sauvegarde du prefixe et de la position de navigation
      * - Permet de parcourir tout le fichier par pages
      *
      * FIL ROUGE CICS - EXERCICE 18
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * CONSTANTES
      *-----------------------------------------------------------------
       01  WS-LIGNES-PAR-PAGE    PIC 9(02) VALUE 10.

      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la position de navigation entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PREFIXE-SAVED   PIC X(06) VALUE SPACES.
           05 WS-LONGUEUR-SAVED  PIC 9(01) VALUE 0.
           05 WS-DERNIERE-CLE    PIC X(06) VALUE SPACES.
           05 WS-PAGE-COURANTE   PIC 9(03) VALUE 0.
           05 WS-TOTAL-CLIENTS   PIC 9(05) VALUE 0.
           05 WS-TOTAL-PAGES     PIC 9(03) VALUE 0.
           05 WS-FIN-FICHIER     PIC X(01) VALUE 'N'.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      *-----------------------------------------------------------------
       COPY CLILIST.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT         PIC X(06).
           05 CLI-CODREG         PIC X(02).
           05 CLI-NATCPT         PIC X(02).
           05 CLI-NOM            PIC X(10).
           05 CLI-PRENOM         PIC X(10).
           05 CLI-DATNAISS       PIC X(08).
           05 CLI-SEXE           PIC X(01).
           05 CLI-ACTPRO         PIC X(02).
           05 CLI-SITSO          PIC X(01).
           05 CLI-ADRESSE        PIC X(10).
           05 CLI-SOLDE          PIC X(10).
           05 CLI-POSITION       PIC X(02).
           05 FILLER             PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP               PIC S9(08) COMP VALUE 0.
       01  WS-MSG-FIN            PIC X(40)
           VALUE 'TRANSACTION LGEN TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * VARIABLES POUR LA NAVIGATION VSAM
      *-----------------------------------------------------------------
       01  WS-BROWSE.
           05 WS-CLE-DEBUT       PIC X(06) VALUE SPACES.
           05 WS-CLE-COURANTE    PIC X(06) VALUE SPACES.
           05 WS-FIN-BROWSE      PIC X(01) VALUE 'N'.
              88 FIN-BROWSE      VALUE 'O'.
              88 PAS-FIN-BROWSE  VALUE 'N'.

      *-----------------------------------------------------------------
      * VARIABLES DE SAISIE
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-PREFIXE         PIC X(06).
           05 WS-PREFIXEL        PIC S9(04) COMP.

      *-----------------------------------------------------------------
      * LONGUEUR DU PREFIXE
      *-----------------------------------------------------------------
       01  WS-LONGUEUR           PIC 9(01) VALUE 0.
       01  WS-INDEX              PIC 9(02) VALUE 0.

      *-----------------------------------------------------------------
      * COMPTEURS
      *-----------------------------------------------------------------
       01  WS-COMPTEURS.
           05 WS-COMPTEUR        PIC 9(05) VALUE 0.
           05 WS-LIGNE-COURANTE  PIC 9(02) VALUE 0.
           05 WS-CLIENTS-SAUVES  PIC 9(02) VALUE 0.

      *-----------------------------------------------------------------
      * TABLE DES CLIENTS A AFFICHER (10 MAX)
      *-----------------------------------------------------------------
       01  WS-TABLE-CLIENTS.
           05 WS-CLI OCCURS 10 TIMES.
              10 WS-CLI-NUM      PIC X(06).
              10 WS-CLI-REG      PIC X(02).
              10 WS-CLI-NOM      PIC X(10).
              10 WS-CLI-PRE      PIC X(10).
              10 WS-CLI-SOL      PIC X(10).
              10 WS-CLI-POS      PIC X(02).

      *-----------------------------------------------------------------
      * MESSAGE FORMATE
      *-----------------------------------------------------------------
       01  WS-MSG-RESULT         PIC X(60) VALUE SPACES.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  DFHCOMMAREA.
           05 LS-PREFIXE-SAVED   PIC X(06).
           05 LS-LONGUEUR-SAVED  PIC 9(01).
           05 LS-DERNIERE-CLE    PIC X(06).
           05 LS-PAGE-COURANTE   PIC 9(03).
           05 LS-TOTAL-CLIENTS   PIC 9(05).
           05 LS-TOTAL-PAGES     PIC 9(03).
           05 LS-FIN-FICHIER     PIC X(01).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Affichage initial
                   PERFORM 1000-INIT
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT
               WHEN OTHER
      *            Restaurer la COMMAREA et traiter
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('LGEN')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT.
      *-----------------------------------------------------------------
      * Affichage ecran initial
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPLGENO
           MOVE 'SAISIR UN PREFIXE (1-6 CAR) ET APPUYER SUR ENTER'
               TO MSGO
           MOVE SPACES TO WS-PREFIXE-SAVED
           MOVE 0 TO WS-LONGUEUR-SAVED
           MOVE SPACES TO WS-DERNIERE-CLE
           MOVE 0 TO WS-PAGE-COURANTE
           MOVE 0 TO WS-TOTAL-CLIENTS
           MOVE 0 TO WS-TOTAL-PAGES
           MOVE 'N' TO WS-FIN-FICHIER

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la touche pressee
      *-----------------------------------------------------------------
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 3000-CHERCHER THRU 3000-FIN
               WHEN DFHPF8
                   PERFORM 4000-PAGE-SUIVANTE THRU 4000-FIN
               WHEN DFHPF7
                   PERFORM 5000-PAGE-PRECEDENTE THRU 5000-FIN
               WHEN OTHER
                   MOVE LOW-VALUES TO MAPLGENO
                   MOVE 'TOUCHE NON RECONNUE - UTILISER ENTER/PF7/PF8/PF3'
                       TO MSGO
                   PERFORM 6100-RESTAURER-AFFICHAGE
           END-EVALUATE.

      *-----------------------------------------------------------------
       3000-CHERCHER.
      *-----------------------------------------------------------------
      * Nouvelle recherche avec prefixe saisi
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPLGEN')
               MAPSET('CLILIST')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'VEUILLEZ SAISIR UN PREFIXE' TO MSGO
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du prefixe saisi
           MOVE PREFIXEI TO WS-PREFIXE
           MOVE PREFIXEL TO WS-PREFIXEL

      *    Controle prefixe non vide
           IF WS-PREFIXEL = 0 OR WS-PREFIXE = SPACES
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREFIXE OBLIGATOIRE (1 A 6 CARACTERES)' TO MSGO
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calcul de la longueur effective du prefixe
           PERFORM 3050-CALCULER-LONGUEUR

      *    Controle : au moins 1 caractere
           IF WS-LONGUEUR = 0
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREFIXE INVALIDE - MIN 1 CARACTERE' TO MSGO
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarder le prefixe pour la navigation
           MOVE WS-PREFIXE TO WS-PREFIXE-SAVED
           MOVE WS-LONGUEUR TO WS-LONGUEUR-SAVED

      *    Compter le total de clients
           PERFORM 3100-COMPTER-TOTAL

           IF WS-TOTAL-CLIENTS = 0
      *        Reinitialiser la COMMAREA AVANT le SEND MAP
               MOVE SPACES TO WS-PREFIXE-SAVED
               MOVE 0 TO WS-LONGUEUR-SAVED
               MOVE SPACES TO WS-DERNIERE-CLE
               MOVE 0 TO WS-PAGE-COURANTE
               MOVE 0 TO WS-TOTAL-PAGES
               MOVE 'N' TO WS-FIN-FICHIER
      *        Preparer l'ecran
               MOVE LOW-VALUES TO MAPLGENO
               MOVE WS-PREFIXE TO PREFIXEO
               MOVE 'AUCUN CLIENT TROUVE - SAISIR AUTRE PREFIXE' TO MSGO
      *        Envoyer l'ecran (simplifie comme PRGDELG)
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calculer le nombre de pages
           DIVIDE WS-TOTAL-CLIENTS BY WS-LIGNES-PAR-PAGE
               GIVING WS-TOTAL-PAGES REMAINDER WS-INDEX
           IF WS-INDEX > 0
               ADD 1 TO WS-TOTAL-PAGES
           END-IF

      *    Afficher la premiere page
           MOVE 1 TO WS-PAGE-COURANTE
           MOVE 'N' TO WS-FIN-FICHIER
           PERFORM 6000-AFFICHER-PAGE.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3050-CALCULER-LONGUEUR.
      *-----------------------------------------------------------------
           MOVE 0 TO WS-LONGUEUR
           PERFORM VARYING WS-INDEX FROM 6 BY -1
               UNTIL WS-INDEX < 1 OR WS-LONGUEUR > 0
               IF WS-PREFIXE(WS-INDEX:1) NOT = SPACE
                   MOVE WS-INDEX TO WS-LONGUEUR
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       3100-COMPTER-TOTAL.
      *-----------------------------------------------------------------
      * Compte le nombre total de clients correspondant au prefixe
      *-----------------------------------------------------------------
           MOVE 0 TO WS-TOTAL-CLIENTS
           MOVE 'N' TO WS-FIN-BROWSE

      *    Construction de la cle de debut
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT
           MOVE WS-LONGUEUR TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3100-FIN
           END-IF

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

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
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR-SAVED) NOT =
                       WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       ADD 1 TO WS-TOTAL-CLIENTS
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.

       3100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4000-PAGE-SUIVANTE.
      *-----------------------------------------------------------------
      * PF8 - Afficher la page suivante
      *-----------------------------------------------------------------
           IF WS-PAGE-COURANTE >= WS-TOTAL-PAGES
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'DERNIERE PAGE ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 4000-FIN
           END-IF

           IF WS-FIN-FICHIER = 'O'
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'FIN DU FICHIER ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 4000-FIN
           END-IF

           ADD 1 TO WS-PAGE-COURANTE
           PERFORM 6000-AFFICHER-PAGE.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       5000-PAGE-PRECEDENTE.
      *-----------------------------------------------------------------
      * PF7 - Afficher la page precedente
      *-----------------------------------------------------------------
           IF WS-PAGE-COURANTE <= 1
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREMIERE PAGE ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 5000-FIN
           END-IF

           SUBTRACT 1 FROM WS-PAGE-COURANTE
           MOVE 'N' TO WS-FIN-FICHIER
           PERFORM 6000-AFFICHER-PAGE.

       5000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       6000-AFFICHER-PAGE.
      *-----------------------------------------------------------------
      * Affiche la page courante (10 clients)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPLGENO
           MOVE 0 TO WS-LIGNE-COURANTE
           MOVE 'N' TO WS-FIN-BROWSE

      *    Initialiser la table des clients
           INITIALIZE WS-TABLE-CLIENTS

      *    Construction de la cle de debut
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED) TO WS-CLE-DEBUT
           MOVE WS-LONGUEUR-SAVED TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR POSITIONNEMENT FICHIER' TO MSGO
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 6000-FIN
           END-IF

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

      *    Sauter les enregistrements des pages precedentes
           COMPUTE WS-COMPTEUR = (WS-PAGE-COURANTE - 1) * 10
           PERFORM WS-COMPTEUR TIMES
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'O' TO WS-FIN-BROWSE
               END-IF
           END-PERFORM

      *    Lire les 10 clients de cette page
           PERFORM UNTIL FIN-BROWSE OR WS-LIGNE-COURANTE >= 10
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                       MOVE 'O' TO WS-FIN-FICHIER
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR-SAVED) NOT =
                       WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED)
                       MOVE 'O' TO WS-FIN-BROWSE
                       MOVE 'O' TO WS-FIN-FICHIER
                   WHEN OTHER
                       ADD 1 TO WS-LIGNE-COURANTE
                       MOVE CLI-NUMCPT TO WS-CLI-NUM(WS-LIGNE-COURANTE)
                       MOVE CLI-CODREG TO WS-CLI-REG(WS-LIGNE-COURANTE)
                       MOVE CLI-NOM TO WS-CLI-NOM(WS-LIGNE-COURANTE)
                       MOVE CLI-PRENOM TO WS-CLI-PRE(WS-LIGNE-COURANTE)
                       MOVE CLI-SOLDE TO WS-CLI-SOL(WS-LIGNE-COURANTE)
                       MOVE CLI-POSITION TO WS-CLI-POS(WS-LIGNE-COURANTE)
                       MOVE WS-CLE-COURANTE TO WS-DERNIERE-CLE
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

      *    Transferer les donnees vers la MAP
           MOVE WS-CLI-NUM(1)  TO L1NUMO
           MOVE WS-CLI-REG(1)  TO L1REGO
           MOVE WS-CLI-NOM(1)  TO L1NOMO
           MOVE WS-CLI-PRE(1)  TO L1PREO
           MOVE WS-CLI-SOL(1)  TO L1SOLO
           MOVE WS-CLI-POS(1)  TO L1POSO

           MOVE WS-CLI-NUM(2)  TO L2NUMO
           MOVE WS-CLI-REG(2)  TO L2REGO
           MOVE WS-CLI-NOM(2)  TO L2NOMO
           MOVE WS-CLI-PRE(2)  TO L2PREO
           MOVE WS-CLI-SOL(2)  TO L2SOLO
           MOVE WS-CLI-POS(2)  TO L2POSO

           MOVE WS-CLI-NUM(3)  TO L3NUMO
           MOVE WS-CLI-REG(3)  TO L3REGO
           MOVE WS-CLI-NOM(3)  TO L3NOMO
           MOVE WS-CLI-PRE(3)  TO L3PREO
           MOVE WS-CLI-SOL(3)  TO L3SOLO
           MOVE WS-CLI-POS(3)  TO L3POSO

           MOVE WS-CLI-NUM(4)  TO L4NUMO
           MOVE WS-CLI-REG(4)  TO L4REGO
           MOVE WS-CLI-NOM(4)  TO L4NOMO
           MOVE WS-CLI-PRE(4)  TO L4PREO
           MOVE WS-CLI-SOL(4)  TO L4SOLO
           MOVE WS-CLI-POS(4)  TO L4POSO

           MOVE WS-CLI-NUM(5)  TO L5NUMO
           MOVE WS-CLI-REG(5)  TO L5REGO
           MOVE WS-CLI-NOM(5)  TO L5NOMO
           MOVE WS-CLI-PRE(5)  TO L5PREO
           MOVE WS-CLI-SOL(5)  TO L5SOLO
           MOVE WS-CLI-POS(5)  TO L5POSO

           MOVE WS-CLI-NUM(6)  TO L6NUMO
           MOVE WS-CLI-REG(6)  TO L6REGO
           MOVE WS-CLI-NOM(6)  TO L6NOMO
           MOVE WS-CLI-PRE(6)  TO L6PREO
           MOVE WS-CLI-SOL(6)  TO L6SOLO
           MOVE WS-CLI-POS(6)  TO L6POSO

           MOVE WS-CLI-NUM(7)  TO L7NUMO
           MOVE WS-CLI-REG(7)  TO L7REGO
           MOVE WS-CLI-NOM(7)  TO L7NOMO
           MOVE WS-CLI-PRE(7)  TO L7PREO
           MOVE WS-CLI-SOL(7)  TO L7SOLO
           MOVE WS-CLI-POS(7)  TO L7POSO

           MOVE WS-CLI-NUM(8)  TO L8NUMO
           MOVE WS-CLI-REG(8)  TO L8REGO
           MOVE WS-CLI-NOM(8)  TO L8NOMO
           MOVE WS-CLI-PRE(8)  TO L8PREO
           MOVE WS-CLI-SOL(8)  TO L8SOLO
           MOVE WS-CLI-POS(8)  TO L8POSO

           MOVE WS-CLI-NUM(9)  TO L9NUMO
           MOVE WS-CLI-REG(9)  TO L9REGO
           MOVE WS-CLI-NOM(9)  TO L9NOMO
           MOVE WS-CLI-PRE(9)  TO L9PREO
           MOVE WS-CLI-SOL(9)  TO L9SOLO
           MOVE WS-CLI-POS(9)  TO L9POSO

           MOVE WS-CLI-NUM(10) TO L10NUMO
           MOVE WS-CLI-REG(10) TO L10REGO
           MOVE WS-CLI-NOM(10) TO L10NOMO
           MOVE WS-CLI-PRE(10) TO L10PREO
           MOVE WS-CLI-SOL(10) TO L10SOLO
           MOVE WS-CLI-POS(10) TO L10POSO

      *    Informations de pagination
           MOVE WS-PREFIXE-SAVED TO PREFIXEO
           MOVE WS-PAGE-COURANTE TO PAGNUMO
           MOVE WS-TOTAL-PAGES TO PAGTOTO
           MOVE WS-TOTAL-CLIENTS TO CLITOTO

      *    Message
           IF WS-FIN-FICHIER = 'O'
               MOVE 'FIN DE LISTE - PF7 POUR REVENIR' TO MSGO
           ELSE
               MOVE 'PF7=PREC  PF8=SUIV  PF3=QUITTER' TO MSGO
           END-IF

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

       6000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       6100-RESTAURER-AFFICHAGE.
      *-----------------------------------------------------------------
      * Restaure l'affichage de la page courante avec un message
      *-----------------------------------------------------------------
           MOVE WS-PREFIXE-SAVED TO PREFIXEO
           MOVE WS-PAGE-COURANTE TO PAGNUMO
           MOVE WS-TOTAL-PAGES TO PAGTOTO
           MOVE WS-TOTAL-CLIENTS TO CLITOTO

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
```

---

## 4. PRGSTAT - Statistiques par region

**Transaction CICS** : `STAT`

**Fonction** : Statistiques clients par region

**Exercice** : Fil rouge CICS - Exercice 19

**Description** : Ce programme calcule et affiche des statistiques pour les clients d'une region donnee. Il utilise un acces via AIX (Alternate Index) sur le code region pour une lecture directe des clients de la region demandee.

Statistiques calculees :
- Nombre total de clients de la region
- Nombre et somme des soldes des clients debiteurs (DB)
- Nombre et somme des soldes des clients crediteurs (CR)

Regions disponibles :
- 01 - Paris
- 02 - Marseille
- 03 - Lyon
- 04 - Lille

Pre-requis :
- AIX defini sur CODREG (offset 6, longueur 2)
- PATH defini (ROCHA.CICS.CLIENT.PATH)
- Definition CICS : FILE(PCLIENT) DSN(PATH)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGSTAT.
      ******************************************************************
      * PROGRAMME : PRGSTAT
      * FONCTION  : Statistiques clients par region
      * TRANSACTION : STAT
      * FICHIER   : PCLIENT (PATH vers AIX sur CODREG)
      * MAP       : MAPSTAT (MAPSET CLISTAT)
      *
      * MODE PSEUDO-CONVERSATIONNEL :
      * -----------------------------
      * - Saisie d'un code region (01, 02, 03 ou 04)
      * - Acces direct via AIX/PATH sur le code region
      * - STARTBR positionne directement sur la region demandee
      * - READNEXT ne lit que les clients de cette region
      * - Calcul et affichage des statistiques :
      *   - Nombre total de clients de la region
      *   - Nombre et somme des clients debiteurs (DB)
      *   - Nombre et somme des clients crediteurs (CR)
      *
      * REGIONS :
      * 01 - Paris      02 - Marseille
      * 03 - Lyon       04 - Lille
      *
      * PRE-REQUIS :
      * - AIX defini sur CODREG (offset 6, longueur 2)
      * - PATH defini (ROCHA.CICS.CLIENT.PATH)
      * - Definition CICS : FILE(PCLIENT) DSN(PATH)
      *
      * FIL ROUGE CICS - EXERCICE 19
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde le code region entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-CODE-REGION-SAVED PIC X(02) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      *-----------------------------------------------------------------
       COPY CLISTAT.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT         PIC X(06).
           05 CLI-CODREG         PIC X(02).
           05 CLI-NATCPT         PIC X(02).
           05 CLI-NOM            PIC X(10).
           05 CLI-PRENOM         PIC X(10).
           05 CLI-DATNAISS       PIC X(08).
           05 CLI-SEXE           PIC X(01).
           05 CLI-ACTPRO         PIC X(02).
           05 CLI-SITSO          PIC X(01).
           05 CLI-ADRESSE        PIC X(10).
           05 CLI-SOLDE          PIC X(10).
           05 CLI-POSITION       PIC X(02).
           05 FILLER             PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP               PIC S9(08) COMP VALUE 0.
       01  WS-MSG-FIN            PIC X(40)
           VALUE 'TRANSACTION STAT TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * VARIABLES POUR LA NAVIGATION VSAM VIA AIX/PATH
      *-----------------------------------------------------------------
       01  WS-BROWSE.
           05 WS-CLE-AIX         PIC X(02) VALUE SPACES.
           05 WS-FIN-BROWSE      PIC X(01) VALUE 'N'.
              88 FIN-BROWSE      VALUE 'O'.
              88 PAS-FIN-BROWSE  VALUE 'N'.

      *-----------------------------------------------------------------
      * VARIABLES DE SAISIE
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-CODE-REGION     PIC X(02).
           05 WS-CODE-REGIONL    PIC S9(04) COMP.

      *-----------------------------------------------------------------
      * TABLE DES NOMS DE REGIONS
      *-----------------------------------------------------------------
       01  WS-TABLE-REGIONS.
           05 FILLER             PIC X(17) VALUE '01PARIS          '.
           05 FILLER             PIC X(17) VALUE '02MARSEILLE      '.
           05 FILLER             PIC X(17) VALUE '03LYON           '.
           05 FILLER             PIC X(17) VALUE '04LILLE          '.
       01  WS-TAB-REGIONS REDEFINES WS-TABLE-REGIONS.
           05 WS-REGION OCCURS 4 TIMES.
              10 WS-REG-CODE     PIC X(02).
              10 WS-REG-NOM      PIC X(15).

      *-----------------------------------------------------------------
      * STATISTIQUES CALCULEES
      *-----------------------------------------------------------------
       01  WS-STATS.
           05 WS-NB-TOTAL        PIC 9(05) VALUE 0.
           05 WS-NB-DEBITEURS    PIC 9(05) VALUE 0.
           05 WS-MT-DEBITEURS    PIC 9(12) VALUE 0.
           05 WS-NB-CREDITEURS   PIC 9(05) VALUE 0.
           05 WS-MT-CREDITEURS   PIC 9(12) VALUE 0.

      *-----------------------------------------------------------------
      * VARIABLES POUR CONVERSION SOLDE
      *-----------------------------------------------------------------
       01  WS-SOLDE-ALPHA        PIC X(10) VALUE SPACES.
       01  WS-SOLDE-NUM REDEFINES WS-SOLDE-ALPHA
                                 PIC 9(10).
       01  WS-NOM-REGION         PIC X(15) VALUE SPACES.
       01  WS-INDEX              PIC 9(01) VALUE 0.

      *-----------------------------------------------------------------
      * FORMATS D'AFFICHAGE
      *-----------------------------------------------------------------
       01  WS-MT-EDIT            PIC ZZZ,ZZZ,ZZZ,ZZ9.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  DFHCOMMAREA.
           05 LS-CODE-REGION-SAVED PIC X(02).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Affichage initial
                   PERFORM 1000-INIT
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT
               WHEN OTHER
      *            Restaurer la COMMAREA et traiter
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('STAT')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT.
      *-----------------------------------------------------------------
      * Affichage ecran initial
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSTATO
           MOVE 'SAISIR UN CODE REGION (01, 02, 03 OU 04)'
               TO MSGO
           MOVE SPACES TO WS-CODE-REGION-SAVED

           EXEC CICS SEND MAP('MAPSTAT')
               MAPSET('CLISTAT')
               FROM(MAPSTATO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Recevoir la saisie et calculer les statistiques
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPSTAT')
               MAPSET('CLISTAT')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPSTATO
               MOVE 'VEUILLEZ SAISIR UN CODE REGION' TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Sauvegarde du code region saisi
           MOVE CODREGI TO WS-CODE-REGION
           MOVE CODREGL TO WS-CODE-REGIONL

      *    Controle code region non vide
           IF WS-CODE-REGIONL = 0 OR WS-CODE-REGION = SPACES
               MOVE LOW-VALUES TO MAPSTATO
               MOVE 'CODE REGION OBLIGATOIRE (01-04)'
                   TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Verification que le code region est valide
           PERFORM 2100-VERIFIER-REGION

           IF WS-NOM-REGION = SPACES
               MOVE LOW-VALUES TO MAPSTATO
               MOVE WS-CODE-REGION TO CODREGO
               MOVE 'CODE REGION INVALIDE (01-04)'
                   TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Sauvegarder pour la COMMAREA
           MOVE WS-CODE-REGION TO WS-CODE-REGION-SAVED

      *    Calculer les statistiques via AIX/PATH
           PERFORM 3000-CALCULER-STATS THRU 3000-FIN

      *    Afficher les resultats
           PERFORM 4000-AFFICHER-RESULTATS.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       2100-VERIFIER-REGION.
      *-----------------------------------------------------------------
      * Verifie que le code region est valide et recupere le nom
      *-----------------------------------------------------------------
           MOVE SPACES TO WS-NOM-REGION

           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 4 OR WS-NOM-REGION NOT = SPACES
               IF WS-CODE-REGION = WS-REG-CODE(WS-INDEX)
                   MOVE WS-REG-NOM(WS-INDEX) TO WS-NOM-REGION
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       3000-CALCULER-STATS.
      *-----------------------------------------------------------------
      * Parcours du fichier via AIX/PATH pour la region demandee
      * L'AIX permet d'acceder directement aux clients de la region
      *-----------------------------------------------------------------
           INITIALIZE WS-STATS
           MOVE 'N' TO WS-FIN-BROWSE

      *    Positionner sur la cle AIX (code region)
           MOVE WS-CODE-REGION TO WS-CLE-AIX

           EXEC CICS STARTBR
               FILE('PCLIENT')
               RIDFLD(WS-CLE-AIX)
               RESP(WS-RESP)
           END-EXEC

      *    Gestion explicite des erreurs STARTBR
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
      *            Aucun client dans cette region
                   GO TO 3000-FIN
               WHEN DFHRESP(ENDFILE)
      *            Fichier vide
                   GO TO 3000-FIN
               WHEN OTHER
      *            Autre erreur
                   GO TO 3000-FIN
           END-EVALUATE

      *    Boucle de lecture des enregistrements de la region
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('PCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-AIX)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                      AND WS-RESP NOT = DFHRESP(DUPKEY)
      *                Erreur autre que DUPKEY (normal pour AIX)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN CLI-CODREG NOT = WS-CODE-REGION
      *                Changement de region = fin du browse
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Client de la region - comptabiliser
                       ADD 1 TO WS-NB-TOTAL
      *                Convertir le solde en numerique
                       PERFORM 3100-CONVERTIR-SOLDE
      *                Verifier si debiteur ou crediteur
                       IF CLI-POSITION = 'DB'
                           ADD 1 TO WS-NB-DEBITEURS
                           ADD WS-SOLDE-NUM TO WS-MT-DEBITEURS
                       ELSE
                           ADD 1 TO WS-NB-CREDITEURS
                           ADD WS-SOLDE-NUM TO WS-MT-CREDITEURS
                       END-IF
               END-EVALUATE
           END-PERFORM

      *    Fermeture du browse
           EXEC CICS ENDBR FILE('PCLIENT') END-EXEC.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-CONVERTIR-SOLDE.
      *-----------------------------------------------------------------
      * Convertit le solde texte en numerique
      * Le solde est stocke en PIC X(10), format numerique
      * Utilise REDEFINES pour la conversion (compatible mainframe)
      *-----------------------------------------------------------------
           MOVE CLI-SOLDE TO WS-SOLDE-ALPHA.

      *-----------------------------------------------------------------
       4000-AFFICHER-RESULTATS.
      *-----------------------------------------------------------------
      * Affiche les resultats des statistiques
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSTATO

      *    Code et nom de la region
           MOVE WS-CODE-REGION TO CODREGO
           MOVE WS-NOM-REGION TO NOMREGO

      *    Statistiques totales
           MOVE WS-NB-TOTAL TO NBTOTO

      *    Statistiques debiteurs
           MOVE WS-NB-DEBITEURS TO NBDBO
           MOVE WS-MT-DEBITEURS TO WS-MT-EDIT
           MOVE WS-MT-EDIT TO MTDBO

      *    Statistiques crediteurs
           MOVE WS-NB-CREDITEURS TO NBCRO
           MOVE WS-MT-CREDITEURS TO WS-MT-EDIT
           MOVE WS-MT-EDIT TO MTCRO

      *    Message de resultat
           IF WS-NB-TOTAL = 0
               MOVE 'AUCUN CLIENT DANS CETTE REGION' TO MSGO
           ELSE
               MOVE 'STATISTIQUES CALCULEES AVEC SUCCES' TO MSGO
           END-IF

           EXEC CICS SEND MAP('MAPSTAT')
               MAPSET('CLISTAT')
               FROM(MAPSTATO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
```

---

## Navigation

- [Retour au rapport principal](rapport-fil-rouge-cics.md)
- [Annexe 1 - Maps BMS](annexes-1-maps-bms.md)
- [Annexe 2 - Partie 1 : Programmes COBOL (Consultation, Creation, Modification)](annexes-2-partie1.md)
- **Annexe 2 - Partie 2 : Programmes COBOL (Suppression, Liste, Statistiques)** (vous etes ici)
- [Annexe 3 - JCL](annexes-3-jcl.md)
