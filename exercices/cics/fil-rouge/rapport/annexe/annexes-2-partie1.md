# Annexe 2 - Partie 1 : Programmes COBOL (Affichage, Ajout, Mise a Jour)

---

## Navigation

- [Retour au rapport principal](./rapport-fil-rouge.md)
- [Annexe 1 - Definitions BMS](./annexes-1.md)
- **Annexe 2 - Partie 1 : Programmes COBOL (Affichage, Ajout, Mise a Jour)** (vous etes ici)
- [Annexe 2 - Partie 2 : Programmes COBOL (Suppression, Navigation)](./annexes-2-partie2.md)
- [Annexe 2 - Partie 3 : Programmes COBOL (Menu, Recherche)](./annexes-2-partie3.md)
- [Annexe 3 - Copybooks COBOL](./annexes-3.md)

---

## 1. Programme PRGCLIA - Affichage d'un client

| Propriete | Valeur |
|-----------|--------|
| **Transaction** | AFFI |
| **Fonction** | Affichage d'un client par numero de compte |
| **Fichier** | FCLIENT (VSAM KSDS) |
| **MAP/MAPSET** | MAPAFF / CLIAFF |
| **Exercice** | Fil Rouge CICS - Exercice 3 |

### Description

Ce programme permet d'afficher les informations d'un client a partir de son numero de compte. Il fonctionne en mode pseudo-conversationnel :
- **Premier passage** : Affiche un ecran vide avec invite de saisie
- **Passages suivants** : Lit le fichier VSAM et affiche les donnees du client
- **PF3** : Quitte la transaction

Le programme effectue une lecture directe (READ) sur le fichier VSAM KSDS en utilisant le numero de compte comme cle. Il gere les cas d'erreur (client inexistant, erreur fichier) et convertit les codes en libelles lisibles (region, sexe, situation sociale, position).

### Code source

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCLIA.
      ******************************************************************
      * PROGRAMME : PRGCLIA
      * FONCTION  : Affichage d'un client par numero de compte
      * TRANSACTION : AFFI
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPAFF (MAPSET CLIAFF)
      *
      * MODE PSEUDO-CONVERSATIONNEL :
      *   - Premier passage : Affiche ecran vide
      *   - Passages suivants : Lit et affiche le client
      *   - PF3 : Quitter la transaction
      *
      * FIL ROUGE CICS - EXERCICE 3
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
      * Stocke dans ROCHA.CICS.LINK(CLIAFF)
      *-----------------------------------------------------------------
       COPY CLIAFF.

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
       01  WS-NUMCPT               PIC X(06) VALUE SPACES.
       01  WS-MSG-FIN              PIC X(40)
           VALUE 'TRANSACTION AFFI TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * LIBELLES POUR AFFICHAGE
      *-----------------------------------------------------------------
       01  WS-LIB-REGION.
           05 FILLER               PIC X(17) VALUE '01 - PARIS      '.
           05 FILLER               PIC X(17) VALUE '02 - MARSEILLE  '.
           05 FILLER               PIC X(17) VALUE '03 - LYON       '.
           05 FILLER               PIC X(17) VALUE '04 - LILLE      '.
       01  WS-TAB-REGION REDEFINES WS-LIB-REGION.
           05 WS-REGION            PIC X(17) OCCURS 4.

       01  WS-LIB-SEXE.
           05 FILLER               PIC X(08) VALUE 'MASCULIN'.
           05 FILLER               PIC X(08) VALUE 'FEMININ '.
       01  WS-TAB-SEXE REDEFINES WS-LIB-SEXE.
           05 WS-SEXE-LIB          PIC X(08) OCCURS 2.

       01  WS-LIB-SITSO.
           05 FILLER               PIC X(12) VALUE 'CELIBATAIRE '.
           05 FILLER               PIC X(12) VALUE 'MARIE(E)    '.
           05 FILLER               PIC X(12) VALUE 'DIVORCE(E)  '.
           05 FILLER               PIC X(12) VALUE 'VEUF(VE)    '.

       01  WS-IDX                  PIC 9(02) VALUE 0.

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
                   PERFORM 2000-TRAITEMENT
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('AFFI')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-PREMIER-PASSAGE.
      *-----------------------------------------------------------------
      * Affichage de l'ecran vide avec message de saisie
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAFFO
           MOVE 'SAISIR LE NUMERO DE COMPTE ET APPUYER SUR ENTREE'
               TO MSGO
           MOVE 'O' TO WS-FLAG-INIT

           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Reception des donnees et recherche du client
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPAFF')
               MAPSET('CLIAFF')
               RESP(WS-RESP)
           END-EXEC

      * Gestion MAPFAIL (aucune donnee transmise)
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPAFFO
               MOVE 'ERREUR RECEPTION - RESSAISIR' TO MSGO
               EXEC CICS SEND MAP('MAPAFF')
                   MAPSET('CLIAFF')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verifier que le numero de compte est saisi
           IF NUMCPTL = 0 OR NUMCPTI = SPACES
               MOVE LOW-VALUES TO MAPAFFO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               EXEC CICS SEND MAP('MAPAFF')
                   MAPSET('CLIAFF')
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Preparer la cle de recherche
           MOVE NUMCPTI TO WS-NUMCPT

      * Lecture du fichier VSAM
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(WS-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

      * Traitement du resultat
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 3000-AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE LOW-VALUES TO MAPAFFO
                   MOVE WS-NUMCPT TO NUMCPTO
                   MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO'
                       TO MSGO
               WHEN OTHER
                   MOVE LOW-VALUES TO MAPAFFO
                   MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT'
                       TO MSGO
           END-EVALUATE

           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
           END-EXEC.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Transfert des donnees du fichier vers la MAP
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAFFO

      * Donnees directes
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

      * Libelle region
           EVALUATE CLI-CODREG
               WHEN '01'
                   MOVE '01 - PARIS' TO LIBREGO
               WHEN '02'
                   MOVE '02 - MARSEILLE' TO LIBREGO
               WHEN '03'
                   MOVE '03 - LYON' TO LIBREGO
               WHEN '04'
                   MOVE '04 - LILLE' TO LIBREGO
               WHEN OTHER
                   MOVE 'REGION INCONNUE' TO LIBREGO
           END-EVALUATE

      * Libelle sexe
           EVALUATE CLI-SEXE
               WHEN 'M'
                   MOVE 'MASCULIN' TO LIBSEXO
               WHEN 'F'
                   MOVE 'FEMININ' TO LIBSEXO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBSEXO
           END-EVALUATE

      * Libelle situation sociale
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

      * Libelle position
           EVALUATE CLI-POSITION
               WHEN 'CR'
                   MOVE 'CREDITEUR' TO LIBPOSO
               WHEN 'DB'
                   MOVE 'DEBITEUR' TO LIBPOSO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBPOSO
           END-EVALUATE

           MOVE 'CLIENT TROUVE - PF3=QUITTER OU NOUVELLE RECHERCHE'
               TO MSGO.

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

## 2. Programme PRGAJT - Ajout d'un nouveau client

| Propriete | Valeur |
|-----------|--------|
| **Transaction** | AJOU |
| **Fonction** | Ajout d'un nouveau client |
| **Fichier** | FCLIENT (VSAM KSDS) |
| **MAP/MAPSET** | MAPAJT / CLIAJT |
| **Exercice** | Fil Rouge CICS - Exercice 7 |

### Description

Ce programme permet d'ajouter un nouveau client dans le fichier VSAM. Il fonctionne en mode pseudo-conversationnel :
- **Premier passage** : Affiche un ecran vide pour la saisie
- **Passages suivants** : Valide les donnees et enregistre le client
- **PF3** : Quitte la transaction

Le programme effectue des controles de conformite :
- Numero de compte numerique (6 chiffres)
- Verification de non-doublure (client n'existe pas deja)
- Code region valide (01-04)
- Sexe valide (M ou F)
- Situation sociale valide (C/M/D/V)
- Position valide (DB ou CR)

### Code source

```cobol
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
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
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
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verification doublure (client existe deja ?)
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
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
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle code region (01, 02, 03 ou 04)
           IF WS-CODREGL = 0 OR WS-CODREG = SPACES
               MOVE 'CODE REGION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle nom (obligatoire)
           IF WS-NOML = 0 OR WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle sexe (M ou F)
           IF WS-SEXEL = 0 OR WS-SEXE = SPACES
               MOVE 'SEXE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle situation sociale (C, M, D ou V)
           IF WS-SITSOL = 0 OR WS-SITSO = SPACES
               MOVE 'SITUATION SOCIALE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle position (DB ou CR)
           IF WS-POSITL = 0 OR WS-POSITION = SPACES
               MOVE 'POSITION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
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
```

---

## 3. Programme PRGMAJ - Mise a jour d'un client existant

| Propriete | Valeur |
|-----------|--------|
| **Transaction** | MAJO |
| **Fonction** | Mise a jour d'un client existant |
| **Fichier** | FCLIENT (VSAM KSDS) |
| **MAP/MAPSET** | MAPMAJ / CLIMAJ |
| **Exercice** | Fil Rouge CICS - Exercice 10 |

### Description

Ce programme permet de modifier les informations d'un client existant. Il fonctionne en mode pseudo-conversationnel a 3 phases :

- **Phase 1 (RECHERCHE)** : Affiche un ecran vide pour saisie du numero de compte. Le champ NUMCPT est en UNPROT (saisissable).
- **Phase 2 (AFFICHAGE)** : Lit le client et affiche les donnees actuelles. Le champ NUMCPT passe en ASKIP (protege, cle non modifiable). Les autres champs sont en UNPROT pour modification.
- **Phase 3 (VALIDATION)** : Recoit les modifications, valide les donnees et effectue le REWRITE pour sauvegarder.

**Difference avec l'ajout (WRITE)** :
- READ UPDATE obligatoire avant REWRITE
- La cle (NUMCPT) ne peut pas etre modifiee
- Le client doit exister (pas de creation)

Le programme utilise une technique de fusion : seuls les champs modifies par l'utilisateur (longueur > 0) remplacent les valeurs existantes.

### Code source

```cobol
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
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
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
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
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
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
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
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
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

      *    RELECTURE DU CLIENT POUR AVOIR LES DONNEES ACTUELLES
           MOVE WS-NUMCPT TO CLI-NUMCPT
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE WS-NUMCPT TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'ERREUR RELECTURE CLIENT - REESSAYEZ' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    FUSION : Ne remplacer que les champs modifies (longueur > 0)
      *    Les champs non modifies gardent leur valeur actuelle (CLI-*)
           PERFORM 4050-FUSIONNER-MODIFICATIONS

      *    Validation des donnees finales
           PERFORM 4100-VALIDER-DONNEES THRU 4100-FIN

           IF ERREUR-DETECTEE
               MOVE DFHBMASK TO NUMCPTA
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
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
       4050-FUSIONNER-MODIFICATIONS.
      *-----------------------------------------------------------------
      * Fusionne les modifications de l'utilisateur avec les donnees
      * actuelles du client. Seuls les champs modifies (longueur > 0)
      * remplacent les valeurs existantes.
      *-----------------------------------------------------------------
      *    Code region : si modifie, prendre la nouvelle valeur
           IF WS-CODREGL > 0
               MOVE WS-CODREG TO CLI-CODREG
           ELSE
               MOVE CLI-CODREG TO WS-CODREG
           END-IF

      *    Nature compte : pas de longueur, on prend si non vide
           IF WS-NATCPT NOT = SPACES AND WS-NATCPT NOT = LOW-VALUES
               MOVE WS-NATCPT TO CLI-NATCPT
           ELSE
               MOVE CLI-NATCPT TO WS-NATCPT
           END-IF

      *    Nom
           IF WS-NOML > 0
               MOVE WS-NOM TO CLI-NOM
           ELSE
               MOVE CLI-NOM TO WS-NOM
           END-IF

      *    Prenom : pas de longueur obligatoire
           IF WS-PRENOM NOT = SPACES AND WS-PRENOM NOT = LOW-VALUES
               MOVE WS-PRENOM TO CLI-PRENOM
           ELSE
               MOVE CLI-PRENOM TO WS-PRENOM
           END-IF

      *    Date naissance
           IF WS-DATNAISSL > 0
               MOVE WS-DATNAISS TO CLI-DATNAISS
           ELSE
               MOVE CLI-DATNAISS TO WS-DATNAISS
           END-IF

      *    Sexe
           IF WS-SEXEL > 0
               MOVE WS-SEXE TO CLI-SEXE
           ELSE
               MOVE CLI-SEXE TO WS-SEXE
           END-IF

      *    Activite pro : pas de longueur obligatoire
           IF WS-ACTPRO NOT = SPACES AND WS-ACTPRO NOT = LOW-VALUES
               MOVE WS-ACTPRO TO CLI-ACTPRO
           ELSE
               MOVE CLI-ACTPRO TO WS-ACTPRO
           END-IF

      *    Situation sociale
           IF WS-SITSOL > 0
               MOVE WS-SITSO TO CLI-SITSO
           ELSE
               MOVE CLI-SITSO TO WS-SITSO
           END-IF

      *    Adresse : pas de longueur obligatoire
           IF WS-ADRESSE NOT = SPACES AND WS-ADRESSE NOT = LOW-VALUES
               MOVE WS-ADRESSE TO CLI-ADRESSE
           ELSE
               MOVE CLI-ADRESSE TO WS-ADRESSE
           END-IF

      *    Solde : pas de longueur obligatoire
           IF WS-SOLDE NOT = SPACES AND WS-SOLDE NOT = LOW-VALUES
               MOVE WS-SOLDE TO CLI-SOLDE
           ELSE
               MOVE CLI-SOLDE TO WS-SOLDE
           END-IF

      *    Position
           IF WS-POSITL > 0
               MOVE WS-POSITION TO CLI-POSITION
           ELSE
               MOVE CLI-POSITION TO WS-POSITION
           END-IF.

      *-----------------------------------------------------------------
       4100-VALIDER-DONNEES.
      *-----------------------------------------------------------------
      * Controles de conformite des donnees finales (apres fusion)
      * Note: Les variables WS-* contiennent soit la modification de
      * l'utilisateur, soit la valeur actuelle du client (via fusion)
      * Donc on ne verifie plus les longueurs, seulement les valeurs.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE WS-NUMCPT TO NUMCPTO

      *    Controle code region (01, 02, 03 ou 04)
           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle nom (obligatoire)
           IF WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle sexe (M ou F)
           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle situation sociale (C, M, D ou V)
           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle position (DB ou CR)
           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
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
```

---

## Navigation

- [Retour au rapport principal](./rapport-fil-rouge.md)
- [Annexe 1 - Definitions BMS](./annexes-1.md)
- **Annexe 2 - Partie 1 : Programmes COBOL (Affichage, Ajout, Mise a Jour)** (vous etes ici)
- [Annexe 2 - Partie 2 : Programmes COBOL (Suppression, Navigation)](./annexes-2-partie2.md)
- [Annexe 2 - Partie 3 : Programmes COBOL (Menu, Recherche)](./annexes-2-partie3.md)
- [Annexe 3 - Copybooks COBOL](./annexes-3.md)
