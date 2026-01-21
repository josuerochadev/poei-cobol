# Annexe 2 - Code Source Complet du Fil Rouge CICS

> Document fusionné pour export PDF - Navigation supprimée


## Introduction

Cette annexe contient l'integralite du code source developpe pour le projet fil-rouge CICS de gestion de clients bancaires. Le code est organise en trois parties pour faciliter la consultation.


### Programmes COBOL (7 fichiers)

| Programme | Transaction | Fonction | Exercice |
|-----------|-------------|----------|----------|
| PRGCLIA | AFFI | Affichage client par numero | 3 |
| PRGAJT | AJOU | Ajout d'un nouveau client | 7 |
| PRGMAJ | MAJO | Mise a jour client existant | 10 |
| PRGSUP | SUPP | Suppression avec confirmation | 13 |
| PRGDELG | DELG | Suppression generique par prefixe | 17 |
| PRGLGEN | LGEN | Liste paginee par prefixe | 18 |
| PRGSTAT | STAT | Statistiques par region (AIX/PATH) | 19 |

### Ecrans BMS (7 fichiers)

| Mapset | Map | Fonction |
|--------|-----|----------|
| CLIAFF | MAPAFF | Ecran affichage client |
| CLIAJT | MAPAJT | Ecran ajout client |
| CLIMAJ | MAPMAJ | Ecran mise a jour |
| CLISUP | MAPSUP | Ecran suppression |
| CLIDEL | MAPDEL | Ecran suppression generique |
| CLILIST | MAPLGEN | Ecran liste paginee |
| CLISTAT | MAPSTAT | Ecran statistiques |

## Structure d'un enregistrement client

```
Position  Longueur  Champ         Description
01-06     6         NUMCPT        Numero de compte (cle primaire)
07-08     2         CODREG        Code region (01-04)
09-10     2         NATCPT        Nature du compte
11-20     10        NOM           Nom du client
21-30     10        PRENOM        Prenom du client
31-38     8         DATNAISS      Date de naissance (AAAAMMJJ)
39        1         SEXE          Sexe (M/F)
40-41     2         ACTPRO        Activite professionnelle
42        1         SITSO         Situation sociale (C/M/D/V)
43-52     10        ADRESSE       Adresse
53-62     10        SOLDE         Solde du compte
63-64     2         POSITION      Position (CR/DB)
65-80     16        FILLER        Reserve
```

## Codes de reference

### Codes Region
- 01 : Paris
- 02 : Marseille
- 03 : Lyon
- 04 : Lille

### Codes Sexe
- M : Masculin
- F : Feminin

### Codes Situation Sociale
- C : Celibataire
- M : Marie(e)
- D : Divorce(e)
- V : Veuf/Veuve

### Codes Position
- CR : Crediteur
- DB : Debiteur

---

# Partie 1 : Programmes COBOL (Affichage, Ajout, Mise a Jour)

## 1. Programme PRGCLIA - Affichage d'un client
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

# Partie 2 : Programmes COBOL (Suppression, Liste, Statistiques)

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

# Partie 3 : Ecrans BMS

## 1. CLIAFF - Affichage Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIAFF |
| **Map** | MAPAFF |
| **Transaction** | AFFI |
| **Fonction** | Ecran d'affichage des informations d'un client. Permet de saisir un numero de compte et d'afficher toutes les donnees associees au client (region, nature de compte, nom, prenom, date de naissance, sexe, activite professionnelle, situation sociale, adresse, solde et position). |

```asm
***********************************************************************
*  MAPSET : CLIAFF - Affichage Client
*  Transaction : AFFI
*  Fil Rouge CICS - Exercice 2
***********************************************************************
CLIAFF   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAFF   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AFFICHAGE CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(6,25),LENGTH=20,ATTRB=ASKIP
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(7,25),LENGTH=20,ATTRB=ASKIP
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(11,24),LENGTH=10,ATTRB=ASKIP
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,24),LENGTH=10,ATTRB=ASKIP
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,25),LENGTH=10,ATTRB=ASKIP
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(19,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 2. CLIAJT - Ajout Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIAJT |
| **Map** | MAPAJT |
| **Transaction** | AJOU |
| **Fonction** | Ecran de saisie pour l'ajout d'un nouveau client. Tous les champs sont en mode saisie (UNPROT) pour permettre la creation complete d'un enregistrement client dans le fichier VSAM. |

```asm
***********************************************************************
*  MAPSET : CLIAJT - Ajout Client
*  Transaction : AJOU
*  Fil Rouge CICS - Exercice 6
***********************************************************************
CLIAJT   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAJT   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,28),LENGTH=24,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AJOUT CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONES DE SAISIE - TOUS LES CHAMPS EN UNPROT
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 3. CLIMAJ - Mise a jour Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIMAJ |
| **Map** | MAPMAJ |
| **Transaction** | MAJO |
| **Fonction** | Ecran de mise a jour des donnees client. Le numero de compte est d'abord saisissable pour la recherche, puis passe en lecture seule apres affichage des donnees. Cette gestion dynamique des attributs se fait dans le programme COBOL via le suffixe 'A'. |

```asm
***********************************************************************
*  MAPSET : CLIMAJ - Mise a jour Client
*  Transaction : MAJO
*  Fil Rouge CICS - Exercice 9
*
*  PARTICULARITE MISE A JOUR :
*  ---------------------------
*  Le numero de compte est d'abord saisissable (recherche),
*  puis passe en lecture seule apres affichage des donnees.
*  Cette gestion dynamique des attributs se fait dans le programme
*  COBOL via le suffixe 'A' (ex: NUMCPTA pour modifier l'attribut).
*
*  Attribut DFHBMASK = X'20' = ASKIP (protege, normal)
*  Attribut DFHBMUNN = X'4C' = UNPROT + NUM (saisie numerique)
***********************************************************************
CLIMAJ   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPMAJ   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** MISE A JOUR CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* NUMERO DE COMPTE - CHAMP CLE
* Commence en UNPROT pour la saisie initiale (recherche)
* Le programme passera l'attribut a ASKIP apres affichage
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(Cle - non modifiable)'
*----------------------------------------------------------------------
* ZONES DE SAISIE/MODIFICATION
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 4. CLISUP - Suppression Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLISUP |
| **Map** | MAPSUP |
| **Transaction** | SUPP / SULE |
| **Fonction** | Ecran de suppression d'un client. Le numero de compte est saisi pour rechercher le client, les donnees sont affichees en lecture seule pour confirmation. Un champ CONFIRM (O/N) permet de valider la suppression. Deux modes d'utilisation : SUPP (suppression directe) et SULE (suppression avec lecture prealable). |

```asm
***********************************************************************
*  MAPSET : CLISUP - Suppression Client
*  Transaction : SUPP / SULE
*  Fil Rouge CICS - Exercice 12
*
*  PARTICULARITE SUPPRESSION :
*  ---------------------------
*  Le numero de compte est saisi pour rechercher le client.
*  Les donnees sont affichees en lecture seule pour confirmation.
*  Un champ CONFIRM (O/N) permet de valider la suppression.
*
*  Deux modes d'utilisation :
*  - SUPP : Suppression directe (Ex 13-14)
*  - SULE : Suppression avec lecture prealable (Ex 15)
***********************************************************************
CLISUP   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPSUP   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** SUPPRESSION CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT (LECTURE SEULE)
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(6,25),LENGTH=20,ATTRB=ASKIP
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(7,25),LENGTH=20,ATTRB=ASKIP
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(11,24),LENGTH=10,ATTRB=ASKIP
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,24),LENGTH=10,ATTRB=ASKIP
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,25),LENGTH=10,ATTRB=ASKIP
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='CONFIRMER SUPPRESSION (O/N) :'
CONFIRM  DFHMDF POS=(18,33),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(18,35),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(21,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(21,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher/Confirmer  PF3=Quitter  CLEAR=X
               Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 5. CLIDEL - Suppression Generique Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIDEL |
| **Map** | MAPDEL |
| **Transaction** | DELG |
| **Fonction** | Ecran de suppression generique permettant la suppression par prefixe (1 a 5 caracteres) ou par cle complete (6 caracteres). Mode pseudo-conversationnel a 2 phases : Phase 1 pour la saisie et le comptage, Phase 2 pour la confirmation et la suppression effective. |

```asm
***********************************************************************
*  MAPSET : CLIDEL - Suppression Generique Client
*  Transaction : DELG
*  Fil Rouge CICS - Exercice 17
*
*  PARTICULARITE :
*  ---------------
*  Permet la suppression par prefixe (1 a 5 car) ou cle complete (6 car)
*  - Prefixe : Supprime tous les clients correspondants
*  - Cle complete : Supprime un seul client
*
*  Le champ PREFIXE est en PIC X (pas NUM) pour eviter la
*  justification a droite des valeurs numeriques.
*
*  MODE PSEUDO-CONVERSATIONNEL A 2 PHASES :
*  Phase 1 : Saisie prefixe/cle -> Comptage et affichage
*  Phase 2 : Confirmation O/N -> Suppression effective
***********************************************************************
CLIDEL   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPDEL   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,20),LENGTH=40,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** SUPPRESSION GENERIQUE CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE OU CLE COMPLETE
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='PREFIXE OU CLE COMPLETE :'
PREFIXE  DFHMDF POS=(5,28),LENGTH=6,ATTRB=(UNPROT,IC)
         DFHMDF POS=(5,35),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(5,37),LENGTH=30,ATTRB=ASKIP,                       X
               INITIAL='(1 a 6 caracteres)'
*----------------------------------------------------------------------
* ZONE D'INFORMATION - NOMBRE DE CLIENTS
*----------------------------------------------------------------------
         DFHMDF POS=(8,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='CLIENTS CORRESPONDANTS  :'
NBCLI    DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(8,35),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(10,2),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='CONFIRMER (O/N)         :'
CONFIRM  DFHMDF POS=(10,28),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,30),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(14,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(15,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(15,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 6. CLILIST - Liste Generique des Clients

| Element | Valeur |
|---------|--------|
| **Mapset** | CLILIST |
| **Map** | MAPLGEN |
| **Transaction** | LGEN |
| **Fonction** | Ecran de liste generique des clients avec pagination. Permet de rechercher des clients par prefixe et d'afficher jusqu'a 10 clients par page avec navigation PF7/PF8. Affiche pour chaque client : numero de compte, region, nom, prenom, solde et position. |

```asm
***********************************************************************
*  MAPSET : CLILIST - Liste Generique des Clients
*  Transaction : LGEN
*  Fil Rouge CICS - Exercice 18
***********************************************************************
CLILIST  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPLGEN  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,18),LENGTH=44,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** LISTE GENERIQUE DES CLIENTS ***'
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE
*----------------------------------------------------------------------
         DFHMDF POS=(3,2),LENGTH=10,ATTRB=ASKIP,INITIAL='PREFIXE :'
PREFIXE  DFHMDF POS=(3,13),LENGTH=6,ATTRB=(UNPROT,IC)
         DFHMDF POS=(3,20),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(3,22),LENGTH=18,ATTRB=ASKIP,                       X
               INITIAL='(1 a 6 caracteres)'
*----------------------------------------------------------------------
* EN-TETE DES COLONNES
*----------------------------------------------------------------------
         DFHMDF POS=(5,1),LENGTH=50,ATTRB=(ASKIP,BRT),                  X
               INITIAL='NUMCPT RG NOM        PRENOM     SOLDE      POS'
*----------------------------------------------------------------------
* LIGNE 1
*----------------------------------------------------------------------
L1NUM    DFHMDF POS=(7,1),LENGTH=6,ATTRB=ASKIP
L1REG    DFHMDF POS=(7,8),LENGTH=2,ATTRB=ASKIP
L1NOM    DFHMDF POS=(7,11),LENGTH=10,ATTRB=ASKIP
L1PRE    DFHMDF POS=(7,22),LENGTH=10,ATTRB=ASKIP
L1SOL    DFHMDF POS=(7,33),LENGTH=10,ATTRB=ASKIP
L1POS    DFHMDF POS=(7,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 2
*----------------------------------------------------------------------
L2NUM    DFHMDF POS=(8,1),LENGTH=6,ATTRB=ASKIP
L2REG    DFHMDF POS=(8,8),LENGTH=2,ATTRB=ASKIP
L2NOM    DFHMDF POS=(8,11),LENGTH=10,ATTRB=ASKIP
L2PRE    DFHMDF POS=(8,22),LENGTH=10,ATTRB=ASKIP
L2SOL    DFHMDF POS=(8,33),LENGTH=10,ATTRB=ASKIP
L2POS    DFHMDF POS=(8,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 3
*----------------------------------------------------------------------
L3NUM    DFHMDF POS=(9,1),LENGTH=6,ATTRB=ASKIP
L3REG    DFHMDF POS=(9,8),LENGTH=2,ATTRB=ASKIP
L3NOM    DFHMDF POS=(9,11),LENGTH=10,ATTRB=ASKIP
L3PRE    DFHMDF POS=(9,22),LENGTH=10,ATTRB=ASKIP
L3SOL    DFHMDF POS=(9,33),LENGTH=10,ATTRB=ASKIP
L3POS    DFHMDF POS=(9,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 4
*----------------------------------------------------------------------
L4NUM    DFHMDF POS=(10,1),LENGTH=6,ATTRB=ASKIP
L4REG    DFHMDF POS=(10,8),LENGTH=2,ATTRB=ASKIP
L4NOM    DFHMDF POS=(10,11),LENGTH=10,ATTRB=ASKIP
L4PRE    DFHMDF POS=(10,22),LENGTH=10,ATTRB=ASKIP
L4SOL    DFHMDF POS=(10,33),LENGTH=10,ATTRB=ASKIP
L4POS    DFHMDF POS=(10,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 5
*----------------------------------------------------------------------
L5NUM    DFHMDF POS=(11,1),LENGTH=6,ATTRB=ASKIP
L5REG    DFHMDF POS=(11,8),LENGTH=2,ATTRB=ASKIP
L5NOM    DFHMDF POS=(11,11),LENGTH=10,ATTRB=ASKIP
L5PRE    DFHMDF POS=(11,22),LENGTH=10,ATTRB=ASKIP
L5SOL    DFHMDF POS=(11,33),LENGTH=10,ATTRB=ASKIP
L5POS    DFHMDF POS=(11,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 6
*----------------------------------------------------------------------
L6NUM    DFHMDF POS=(12,1),LENGTH=6,ATTRB=ASKIP
L6REG    DFHMDF POS=(12,8),LENGTH=2,ATTRB=ASKIP
L6NOM    DFHMDF POS=(12,11),LENGTH=10,ATTRB=ASKIP
L6PRE    DFHMDF POS=(12,22),LENGTH=10,ATTRB=ASKIP
L6SOL    DFHMDF POS=(12,33),LENGTH=10,ATTRB=ASKIP
L6POS    DFHMDF POS=(12,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 7
*----------------------------------------------------------------------
L7NUM    DFHMDF POS=(13,1),LENGTH=6,ATTRB=ASKIP
L7REG    DFHMDF POS=(13,8),LENGTH=2,ATTRB=ASKIP
L7NOM    DFHMDF POS=(13,11),LENGTH=10,ATTRB=ASKIP
L7PRE    DFHMDF POS=(13,22),LENGTH=10,ATTRB=ASKIP
L7SOL    DFHMDF POS=(13,33),LENGTH=10,ATTRB=ASKIP
L7POS    DFHMDF POS=(13,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 8
*----------------------------------------------------------------------
L8NUM    DFHMDF POS=(14,1),LENGTH=6,ATTRB=ASKIP
L8REG    DFHMDF POS=(14,8),LENGTH=2,ATTRB=ASKIP
L8NOM    DFHMDF POS=(14,11),LENGTH=10,ATTRB=ASKIP
L8PRE    DFHMDF POS=(14,22),LENGTH=10,ATTRB=ASKIP
L8SOL    DFHMDF POS=(14,33),LENGTH=10,ATTRB=ASKIP
L8POS    DFHMDF POS=(14,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 9
*----------------------------------------------------------------------
L9NUM    DFHMDF POS=(15,1),LENGTH=6,ATTRB=ASKIP
L9REG    DFHMDF POS=(15,8),LENGTH=2,ATTRB=ASKIP
L9NOM    DFHMDF POS=(15,11),LENGTH=10,ATTRB=ASKIP
L9PRE    DFHMDF POS=(15,22),LENGTH=10,ATTRB=ASKIP
L9SOL    DFHMDF POS=(15,33),LENGTH=10,ATTRB=ASKIP
L9POS    DFHMDF POS=(15,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 10
*----------------------------------------------------------------------
L10NUM   DFHMDF POS=(16,1),LENGTH=6,ATTRB=ASKIP
L10REG   DFHMDF POS=(16,8),LENGTH=2,ATTRB=ASKIP
L10NOM   DFHMDF POS=(16,11),LENGTH=10,ATTRB=ASKIP
L10PRE   DFHMDF POS=(16,22),LENGTH=10,ATTRB=ASKIP
L10SOL   DFHMDF POS=(16,33),LENGTH=10,ATTRB=ASKIP
L10POS   DFHMDF POS=(16,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE INFORMATIONS PAGINATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=6,ATTRB=ASKIP,INITIAL='PAGE :'
PAGNUM   DFHMDF POS=(18,9),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,13),LENGTH=1,ATTRB=ASKIP,INITIAL='/'
PAGTOT   DFHMDF POS=(18,15),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,22),LENGTH=7,ATTRB=ASKIP,INITIAL='TOTAL :'
CLITOT   DFHMDF POS=(18,30),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,36),LENGTH=10,ATTRB=ASKIP,INITIAL='CLIENT(S)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=60,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Chercher  PF7=Prec  PF8=Suiv  PF3=Quitter'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 7. CLISTAT - Statistiques par Region

| Element | Valeur |
|---------|--------|
| **Mapset** | CLISTAT |
| **Map** | MAPSTAT |
| **Transaction** | STAT |
| **Fonction** | Ecran de statistiques par region. Affiche pour une region donnee : le nombre total de clients, le nombre et la somme des clients debiteurs (DB), le nombre et la somme des clients crediteurs (CR). Regions disponibles : 01-Paris, 02-Marseille, 03-Lyon, 04-Lille. |

```asm
***********************************************************************
*  MAPSET : CLISTAT - Statistiques par Region
*  Transaction : STAT
*  Fil Rouge CICS - Exercice 19
*
*  FONCTIONNALITE :
*  ----------------
*  Affiche les statistiques d'une region :
*  - Nombre total de clients
*  - Nombre et somme des clients debiteurs (DB)
*  - Nombre et somme des clients crediteurs (CR)
*
*  REGIONS DISPONIBLES :
*  01 - Paris     02 - Marseille
*  03 - Lyon      04 - Lille
*
*  MODE PSEUDO-CONVERSATIONNEL :
*  - Premier passage : Affichage ecran de saisie
*  - Passages suivants : Calcul et affichage des statistiques
***********************************************************************
CLISTAT  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPSTAT  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,20),LENGTH=40,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** STATISTIQUES PAR REGION ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - CODE REGION
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION           :'
CODREG   DFHMDF POS=(4,28),LENGTH=2,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,31),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(4,33),LENGTH=40,ATTRB=ASKIP,                       X
               INITIAL='(01=Paris, 02=Marseille, 03=Lyon, 04=Lille)'
*----------------------------------------------------------------------
* NOM DE LA REGION
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='REGION                :'
NOMREG   DFHMDF POS=(6,28),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* SEPARATEUR
*----------------------------------------------------------------------
         DFHMDF POS=(8,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* STATISTIQUES GLOBALES
*----------------------------------------------------------------------
         DFHMDF POS=(10,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='NOMBRE TOTAL DE CLIENTS         :'
NBTOT    DFHMDF POS=(10,38),LENGTH=5,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* STATISTIQUES DEBITEURS
*----------------------------------------------------------------------
         DFHMDF POS=(12,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='CLIENTS DEBITEURS (DB)          :'
NBDB     DFHMDF POS=(12,38),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='SOMME DES SOLDES DEBITEURS      :'
MTDB     DFHMDF POS=(13,38),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* STATISTIQUES CREDITEURS
*----------------------------------------------------------------------
         DFHMDF POS=(15,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='CLIENTS CREDITEURS (CR)         :'
NBCR     DFHMDF POS=(15,38),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='SOMME DES SOLDES CREDITEURS     :'
MTCR     DFHMDF POS=(16,38),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* SEPARATEUR
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Calculer  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---
