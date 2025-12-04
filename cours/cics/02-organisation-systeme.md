# Chapitre II - Organisation du système CICS

## 1. Composants de CICS

### 1.1 Vue d'ensemble des composants

CICS est composé de plusieurs gestionnaires (managers) qui travaillent ensemble pour traiter les transactions :

```
┌─────────────────────────────────────────────────────────────────────┐
│                         RÉGION CICS                                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐     │
│  │    Terminal     │  │    Program      │  │   Transaction   │     │
│  │    Control      │  │    Control      │  │    Manager      │     │
│  │     (TCP)       │  │     (PCP)       │  │     (TM)        │     │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘     │
│           │                    │                    │               │
│  ┌────────┴────────┐  ┌────────┴────────┐  ┌────────┴────────┐     │
│  │     Storage     │  │      Task       │  │      File       │     │
│  │    Management   │  │     Control     │  │     Control     │     │
│  │      (SM)       │  │     (KCP)       │  │      (FC)       │     │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘     │
│           │                    │                    │               │
│  ┌────────┴────────┐  ┌────────┴────────┐  ┌────────┴────────┐     │
│  │   Temporary     │  │    Interval    │  │    Transient    │     │
│  │    Storage      │  │    Control     │  │      Data       │     │
│  │     (TS)        │  │     (IC)       │  │      (TD)       │     │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘     │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 Description des composants principaux

#### Terminal Control Program (TCP)
Gère les communications avec les terminaux :
- Réception des entrées utilisateur
- Envoi des écrans de sortie
- Gestion des sessions

#### Program Control Program (PCP)
Gère l'exécution des programmes :
- Chargement des programmes en mémoire
- Appels entre programmes (LINK, XCTL)
- Gestion des retours (RETURN)

#### Task Control Program (KCP)
Gère les tâches (instances de transactions) :
- Création et destruction des tâches
- Ordonnancement (scheduling)
- Gestion des priorités

#### Storage Management (SM)
Gère la mémoire :
- Allocation dynamique (GETMAIN)
- Libération (FREEMAIN)
- Optimisation de l'utilisation

#### File Control (FC)
Gère l'accès aux fichiers :
- Opérations VSAM (READ, WRITE, DELETE)
- Verrouillage des enregistrements
- Gestion des erreurs

#### Temporary Storage (TS)
Gère le stockage temporaire :
- Queues TS pour données intermédiaires
- Accessible par plusieurs transactions
- Persistant ou volatile

#### Transient Data (TD)
Gère les files d'attente :
- Queues intra-partition (internes)
- Queues extra-partition (fichiers)
- Déclenchement automatique de transactions

#### Interval Control (IC)
Gère le temps :
- Démarrage différé de transactions (START)
- Attentes programmées (DELAY)
- Expiration de délais

## 2. Déroulement d'une Transaction

### 2.1 Cycle de vie d'une transaction

```
┌─────────────────────────────────────────────────────────────────────┐
│              CYCLE DE VIE D'UNE TRANSACTION CICS                    │
└─────────────────────────────────────────────────────────────────────┘

  UTILISATEUR                    CICS                      PROGRAMME
      │                           │                            │
      │  1. Saisie code trans     │                            │
      │  (ex: MENU + Entrée)      │                            │
      │ ─────────────────────────►│                            │
      │                           │                            │
      │                    2. Recherche dans PCT               │
      │                    (Program Control Table)             │
      │                           │                            │
      │                    3. Recherche dans PPT               │
      │                    (Processing Program Table)          │
      │                           │                            │
      │                    4. Chargement programme             │
      │                           │ ──────────────────────────►│
      │                           │                            │
      │                           │  5. Exécution              │
      │                           │◄────────────────────────── │
      │                           │                            │
      │  6. Affichage résultat    │                            │
      │◄───────────────────────── │                            │
      │                           │                            │
```

### 2.2 Les tables système

CICS utilise plusieurs tables pour gérer les ressources :

| Table | Nom complet | Rôle |
|-------|-------------|------|
| **PCT** | Program Control Table | Association Transaction → Programme |
| **PPT** | Processing Program Table | Liste des programmes disponibles |
| **FCT** | File Control Table | Définition des fichiers VSAM |
| **TCT** | Terminal Control Table | Définition des terminaux |
| **DCT** | Destination Control Table | Définition des queues TD |
| **TST** | Temporary Storage Table | Configuration du stockage temporaire |

#### Exemple de définition PCT

```
Transaction ID : MENU
Programme      : MENUPGM
Priorité       : 1
Sécurité       : PUBLIC
```

### 2.3 États d'une tâche

```
        ┌──────────┐
        │  CRÉÉE   │
        └────┬─────┘
             │
             ▼
        ┌──────────┐
    ┌───│  PRÊTE   │◄──┐
    │   └────┬─────┘   │
    │        │         │
    │        ▼         │
    │   ┌──────────┐   │
    │   │ EN COURS │───┘
    │   └────┬─────┘
    │        │
    │        ▼
    │   ┌──────────┐
    └──►│ EN ATTENTE│ (I/O, ressource, délai)
        └────┬─────┘
             │
             ▼
        ┌──────────┐
        │ TERMINÉE │
        └──────────┘
```

## 3. Bloc EIB (Execute Interface Block)

### 3.1 Présentation

L'**EIB** (Execute Interface Block) est une zone de communication automatiquement mise à disposition de chaque programme CICS. Elle contient des informations sur :
- La transaction en cours
- Le terminal utilisateur
- Les erreurs éventuelles
- L'heure et la date

### 3.2 Déclaration

L'EIB est automatiquement disponible dans la LINKAGE SECTION lors de la compilation CICS. Cependant, il faut la déclarer pour y accéder :

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01 DFHCOMMAREA        PIC X(100).

       EXEC CICS
           ADDRESS EIB(DFHEIBLK)
       END-EXEC
```

### 3.3 Champs principaux de l'EIB

| Champ | Type | Description |
|-------|------|-------------|
| **EIBAID** | PIC X(1) | Touche fonction appuyée (PF1, ENTER, etc.) |
| **EIBCALEN** | PIC S9(4) COMP | Longueur de la COMMAREA |
| **EIBCPOSN** | PIC S9(4) COMP | Position du curseur |
| **EIBDATE** | PIC S9(7) COMP-3 | Date au format 0CYYDDD |
| **EIBDS** | PIC X(8) | Nom du fichier en cours |
| **EIBRESP** | PIC S9(8) COMP | Code réponse de la dernière commande |
| **EIBRESP2** | PIC S9(8) COMP | Code réponse secondaire |
| **EIBTASKN** | PIC S9(7) COMP-3 | Numéro de la tâche |
| **EIBTIME** | PIC S9(7) COMP-3 | Heure au format 0HHMMSS |
| **EIBTRMID** | PIC X(4) | Identifiant du terminal |
| **EIBTRNID** | PIC X(4) | Code de la transaction |

### 3.4 Exemples d'utilisation

#### Récupérer le code transaction
```cobol
       WORKING-STORAGE SECTION.
       01 WS-TRANS-ID         PIC X(4).

       PROCEDURE DIVISION.
           MOVE EIBTRNID TO WS-TRANS-ID
           DISPLAY 'Transaction : ' WS-TRANS-ID
```

#### Tester la touche appuyée
```cobol
       WORKING-STORAGE SECTION.
       01 DFHAID.
          COPY DFHAID.

       PROCEDURE DIVISION.
           IF EIBAID = DFHPF3
               EXEC CICS RETURN END-EXEC
           END-IF

           IF EIBAID = DFHENTER
               PERFORM TRAITEMENT-SAISIE
           END-IF
```

#### Vérifier le code retour
```cobol
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-ID)
           END-EXEC

           IF EIBRESP = DFHRESP(NORMAL)
               PERFORM AFFICHER-CLIENT
           ELSE IF EIBRESP = DFHRESP(NOTFND)
               MOVE 'CLIENT NON TROUVE' TO WS-MESSAGE
           ELSE
               MOVE 'ERREUR LECTURE' TO WS-MESSAGE
           END-IF
```

### 3.5 Codes EIBAID courants

```cobol
       01 DFHAID.
          05 DFHENTER    PIC X VALUE X'7D'.  *> Touche Entrée
          05 DFHCLEAR    PIC X VALUE X'6D'.  *> Touche Clear
          05 DFHPA1      PIC X VALUE X'6C'.  *> PA1
          05 DFHPA2      PIC X VALUE X'6E'.  *> PA2
          05 DFHPF1      PIC X VALUE X'F1'.  *> PF1
          05 DFHPF2      PIC X VALUE X'F2'.  *> PF2
          05 DFHPF3      PIC X VALUE X'F3'.  *> PF3
          05 DFHPF12     PIC X VALUE X'7C'.  *> PF12
```

## 4. Notion de commandes CICS

### 4.1 Structure générale

Toutes les commandes CICS suivent le même format :

```cobol
       EXEC CICS
           COMMANDE option1(valeur1)
                    option2(valeur2)
                    [RESP(variable-resp)]
                    [RESP2(variable-resp2)]
       END-EXEC
```

### 4.2 Catégories de commandes

#### Commandes de contrôle de programme

| Commande | Description |
|----------|-------------|
| `RETURN` | Retour au CICS ou programme appelant |
| `XCTL` | Transfert de contrôle (sans retour) |
| `LINK` | Appel de sous-programme (avec retour) |
| `ABEND` | Arrêt anormal du programme |

```cobol
      * Retour à CICS
           EXEC CICS
               RETURN
           END-EXEC

      * Retour avec nouvelle transaction
           EXEC CICS
               RETURN TRANSID('MENU')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(100)
           END-EXEC

      * Appel d'un sous-programme
           EXEC CICS
               LINK PROGRAM('SOUSPGM')
                    COMMAREA(WS-DATA)
                    LENGTH(50)
           END-EXEC

      * Transfert de contrôle
           EXEC CICS
               XCTL PROGRAM('AUTREPGM')
                    COMMAREA(WS-DATA)
           END-EXEC
```

#### Commandes de gestion des fichiers

| Commande | Description |
|----------|-------------|
| `READ` | Lecture d'un enregistrement |
| `WRITE` | Écriture d'un nouvel enregistrement |
| `REWRITE` | Mise à jour d'un enregistrement |
| `DELETE` | Suppression d'un enregistrement |
| `STARTBR` | Début de parcours séquentiel |
| `READNEXT` | Lecture suivante |
| `ENDBR` | Fin de parcours |

```cobol
      * Lecture directe
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT)
                    RIDFLD(WS-NUMCLI)
                    RESP(WS-RESP)
           END-EXEC

      * Écriture
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-CLIENT)
                     RIDFLD(WS-NUMCLI)
           END-EXEC

      * Mise à jour (après READ UPDATE)
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT)
                    RIDFLD(WS-NUMCLI)
                    UPDATE
           END-EXEC

           MOVE 'NOUVEAU NOM' TO CLI-NOM OF WS-CLIENT

           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT)
           END-EXEC
```

#### Commandes de gestion d'écran (BMS)

| Commande | Description |
|----------|-------------|
| `SEND MAP` | Envoi d'un écran formaté |
| `RECEIVE MAP` | Réception des données saisies |
| `SEND TEXT` | Envoi de texte simple |

```cobol
      * Envoi d'un écran
           EXEC CICS
               SEND MAP('MAP001')
                    MAPSET('MAPSET1')
                    FROM(MAP001O)
                    ERASE
           END-EXEC

      * Réception des données
           EXEC CICS
               RECEIVE MAP('MAP001')
                       MAPSET('MAPSET1')
                       INTO(MAP001I)
           END-EXEC
```

#### Commandes de stockage temporaire

```cobol
      * Écriture dans une queue TS
           EXEC CICS
               WRITEQ TS QUEUE('MAQUEUETS')
                         FROM(WS-DATA)
                         LENGTH(100)
           END-EXEC

      * Lecture depuis une queue TS
           EXEC CICS
               READQ TS QUEUE('MAQUEUETS')
                        INTO(WS-DATA)
                        LENGTH(WS-LEN)
                        ITEM(1)
           END-EXEC

      * Suppression d'une queue TS
           EXEC CICS
               DELETEQ TS QUEUE('MAQUEUETS')
           END-EXEC
```

### 4.3 Gestion des erreurs

#### Option RESP
```cobol
       WORKING-STORAGE SECTION.
       01 WS-RESP             PIC S9(8) COMP.
       01 WS-RESP2            PIC S9(8) COMP.

       PROCEDURE DIVISION.
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT)
                    RIDFLD(WS-NUMCLI)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM TRAITEMENT-OK
               WHEN DFHRESP(NOTFND)
                   MOVE 'ENREGISTREMENT NON TROUVE' TO WS-MSG
               WHEN DFHRESP(DISABLED)
                   MOVE 'FICHIER DESACTIVE' TO WS-MSG
               WHEN OTHER
                   MOVE 'ERREUR INATTENDUE' TO WS-MSG
           END-EVALUATE
```

#### Codes RESP courants

| Code | Constante | Description |
|------|-----------|-------------|
| 0 | NORMAL | Exécution réussie |
| 13 | NOTFND | Enregistrement non trouvé |
| 14 | DUPREC | Enregistrement en double |
| 16 | INVREQ | Requête invalide |
| 22 | DISABLED | Ressource désactivée |
| 26 | FILENOTFOUND | Fichier non défini |
| 32 | ILLOGIC | Erreur logique VSAM |

### 4.4 COMMAREA - Zone de communication

La **COMMAREA** (Communication Area) permet de passer des données :
- Entre transactions (pseudo-conversationnel)
- Entre programmes (LINK, XCTL)

```cobol
       WORKING-STORAGE SECTION.
       01 WS-COMMAREA.
          05 CA-ACTION        PIC X(1).
             88 CA-AFFICHER   VALUE 'A'.
             88 CA-MODIFIER   VALUE 'M'.
          05 CA-NUMCLI        PIC 9(8).
          05 CA-NOM           PIC X(30).

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 LK-ACTION        PIC X(1).
          05 LK-NUMCLI        PIC 9(8).
          05 LK-NOM           PIC X(30).

       PROCEDURE DIVISION.
      * Premier passage (pas de COMMAREA)
           IF EIBCALEN = 0
               PERFORM INITIALISATION
           ELSE
      * Passages suivants (COMMAREA existe)
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM TRAITEMENT
           END-IF

      * Retour avec COMMAREA
           EXEC CICS
               RETURN TRANSID('TRAN')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
```

## 5. Résumé

```
┌─────────────────────────────────────────────────────────────────────┐
│                 ORGANISATION CICS - RÉSUMÉ                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  COMPOSANTS                                                          │
│  • TCP : Terminaux          • FC : Fichiers                         │
│  • PCP : Programmes         • TS : Stockage temporaire              │
│  • KCP : Tâches             • TD : Files d'attente                  │
│  • SM  : Mémoire            • IC : Contrôle temps                   │
│                                                                      │
│  TABLES SYSTÈME                                                      │
│  • PCT : Transaction → Programme                                     │
│  • PPT : Programmes disponibles                                      │
│  • FCT : Fichiers VSAM                                              │
│                                                                      │
│  EIB - CHAMPS ESSENTIELS                                            │
│  • EIBTRNID : Code transaction                                      │
│  • EIBTRMID : Terminal                                              │
│  • EIBAID   : Touche appuyée                                        │
│  • EIBRESP  : Code retour                                           │
│  • EIBCALEN : Longueur COMMAREA                                     │
│                                                                      │
│  COMMANDES PRINCIPALES                                               │
│  • Contrôle : RETURN, LINK, XCTL                                    │
│  • Fichiers : READ, WRITE, REWRITE, DELETE                          │
│  • Écrans   : SEND MAP, RECEIVE MAP                                 │
│  • Stockage : WRITEQ TS, READQ TS                                   │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [← Précédent : Présentation générale](01-presentation-generale.md)
- [Suivant : SGBD IMS →](03-sgbd-ims.md)
- [Retour au sommaire](README.md)
