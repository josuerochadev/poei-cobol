# Chapitre II - Organisation du système CICS

## II-1 Composants de CICS

### Vue d'ensemble

CICS est composé de plusieurs **gestionnaires** (managers) qui travaillent ensemble pour traiter les transactions. Chaque composant a un rôle spécifique.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           RÉGION CICS                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                    NOYAU CICS (Nucleus)                            │  │
│  │  Gestion globale de la région, dispatcher, recovery               │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                                                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐        │
│  │   Terminal      │  │    Program      │  │     Task        │        │
│  │   Control       │  │    Control      │  │    Control      │        │
│  │    (TCP)        │  │     (PCP)       │  │     (KCP)       │        │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘        │
│                                                                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐        │
│  │     File        │  │    Storage      │  │   Interval      │        │
│  │   Control       │  │   Control       │  │   Control       │        │
│  │    (FC)         │  │    (SC)         │  │    (IC)         │        │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘        │
│                                                                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐        │
│  │   Temporary     │  │   Transient     │  │     BMS         │        │
│  │    Storage      │  │     Data        │  │  (Basic Mapping │        │
│  │    (TS)         │  │     (TD)        │  │   Support)      │        │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘        │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Détail des composants

#### Terminal Control Program (TCP)

Gère toutes les communications avec les terminaux 3270.

| Fonction | Description |
|----------|-------------|
| Réception | Capture les entrées utilisateur (saisie, touches fonction) |
| Envoi | Affiche les écrans et messages sur le terminal |
| Session | Gère l'état de connexion du terminal |
| Buffer | Gère les tampons d'entrée/sortie |

```
┌─────────────────────────────────────────────────────────────────┐
│                    TERMINAL CONTROL                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Terminal 3270 ◄────────────► TCP ◄────────────► Programme      │
│                                                                  │
│  • Gestion du protocole 3270                                    │
│  • Conversion des données (EBCDIC)                              │
│  • Gestion des touches AID (PF, PA, ENTER, CLEAR)               │
│  • Transmission synchrone/asynchrone                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Program Control Program (PCP)

Gère le cycle de vie des programmes dans CICS.

| Fonction | Description |
|----------|-------------|
| Chargement | Charge les programmes depuis la bibliothèque |
| Appel | Gère les appels LINK et XCTL entre programmes |
| Retour | Gère les retours RETURN vers CICS ou l'appelant |
| Cache | Maintient les programmes en mémoire (réutilisation) |

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROGRAM CONTROL                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Bibliothèque ──► Chargement ──► Exécution ──► Retour           │
│   (DFHRPL)           │              │            │              │
│                      ▼              ▼            ▼              │
│                   ┌──────┐     ┌──────┐     ┌──────┐           │
│                   │ LINK │     │ XCTL │     │RETURN│           │
│                   └──────┘     └──────┘     └──────┘           │
│                                                                  │
│  LINK  : Appel avec retour (comme CALL en COBOL)                │
│  XCTL  : Transfert définitif (pas de retour)                    │
│  RETURN: Fin du programme                                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Task Control Program (KCP)

Gère le multitâche dans CICS (plusieurs transactions simultanées).

| Fonction | Description |
|----------|-------------|
| Création | Crée une nouvelle tâche pour chaque transaction |
| Dispatch | Ordonnance l'exécution des tâches (priorités) |
| Suspension | Met en attente les tâches (I/O, ressource) |
| Destruction | Libère les ressources en fin de tâche |

```
┌─────────────────────────────────────────────────────────────────┐
│                      TASK CONTROL                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐   │
│  │ Task 1 │  │ Task 2 │  │ Task 3 │  │ Task 4 │  │ Task 5 │   │
│  │ MENU   │  │ INQU   │  │ MENU   │  │ XFER   │  │ UPDT   │   │
│  │Priority│  │Priority│  │Priority│  │Priority│  │Priority│   │
│  │   1    │  │   2    │  │   1    │  │   3    │  │   1    │   │
│  └───┬────┘  └───┬────┘  └───┬────┘  └───┬────┘  └───┬────┘   │
│      │           │           │           │           │         │
│      └───────────┴───────────┴───────────┴───────────┘         │
│                              │                                  │
│                     ┌────────▼────────┐                        │
│                     │   DISPATCHER    │                        │
│                     │  (Ordonnanceur) │                        │
│                     └─────────────────┘                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### File Control (FC)

Gère l'accès aux fichiers VSAM et autres ressources de données.

| Fonction | Description |
|----------|-------------|
| Lecture | READ, READNEXT, READPREV |
| Écriture | WRITE (création) |
| Mise à jour | REWRITE (modification) |
| Suppression | DELETE |
| Parcours | STARTBR, ENDBR (browse) |

#### Storage Control (SC)

Gère l'allocation dynamique de mémoire.

| Commande | Description |
|----------|-------------|
| GETMAIN | Allocation d'une zone mémoire |
| FREEMAIN | Libération d'une zone mémoire |

#### Interval Control (IC)

Gère les opérations liées au temps.

| Commande | Description |
|----------|-------------|
| START | Démarrage différé d'une transaction |
| DELAY | Pause dans l'exécution |
| ASKTIME | Récupération de l'heure courante |
| FORMATTIME | Formatage de date/heure |

#### Temporary Storage (TS)

Gère le stockage temporaire de données.

```
┌─────────────────────────────────────────────────────────────────┐
│                   TEMPORARY STORAGE                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Queue TS = Zone de stockage identifiée par un nom (1-8 car.)  │
│                                                                  │
│  ┌─────────────────────────────────────────┐                    │
│  │  Queue: TSQUEUE1                        │                    │
│  │  ┌────────┬────────┬────────┬────────┐ │                    │
│  │  │ Item 1 │ Item 2 │ Item 3 │ Item 4 │ │                    │
│  │  └────────┴────────┴────────┴────────┘ │                    │
│  └─────────────────────────────────────────┘                    │
│                                                                  │
│  Types:                                                         │
│  • MAIN     : En mémoire (volatile, rapide)                     │
│  • AUXILIARY: Sur disque (persistant, plus lent)                │
│                                                                  │
│  Commandes: WRITEQ TS, READQ TS, DELETEQ TS                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Transient Data (TD)

Gère les files d'attente de messages.

```
┌─────────────────────────────────────────────────────────────────┐
│                    TRANSIENT DATA                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Deux types de queues:                                          │
│                                                                  │
│  INTRAPARTITION (internes à CICS)                               │
│  ┌─────────────────────────────────────────┐                    │
│  │  Programme A ──► Queue TD ──► Programme B                    │
│  │  (Déclenchement automatique possible)                        │
│  └─────────────────────────────────────────┘                    │
│                                                                  │
│  EXTRAPARTITION (fichiers externes)                             │
│  ┌─────────────────────────────────────────┐                    │
│  │  Programme ◄──► Queue TD ◄──► Fichier séquentiel            │
│  │  (Impression, logs, interfaces batch)                        │
│  └─────────────────────────────────────────┘                    │
│                                                                  │
│  Commandes: WRITEQ TD, READQ TD, DELETEQ TD                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Basic Mapping Support (BMS)

Gère la création et l'affichage des écrans formatés.

| Fonction | Description |
|----------|-------------|
| MAP | Définition de l'écran (champs, positions, attributs) |
| MAPSET | Groupe de MAPs |
| SEND MAP | Envoi d'un écran formaté |
| RECEIVE MAP | Réception des données saisies |

### Tables système CICS

CICS utilise des **tables de définition** pour connaître les ressources disponibles :

| Table | Nom complet | Contenu |
|-------|-------------|---------|
| **PCT** | Program Control Table | Transactions et programmes associés |
| **PPT** | Processing Program Table | Programmes disponibles |
| **FCT** | File Control Table | Fichiers VSAM accessibles |
| **TCT** | Terminal Control Table | Terminaux autorisés |
| **DCT** | Destination Control Table | Queues TD définies |
| **TST** | Temporary Storage Table | Configuration TS |
| **SIT** | System Initialization Table | Paramètres de démarrage |

```
┌─────────────────────────────────────────────────────────────────┐
│                    TABLES SYSTÈME                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Transaction "MENU" ──► PCT ──► Programme "MENUPGM"             │
│                                      │                          │
│                                      ▼                          │
│                         PPT ──► Localisation du programme       │
│                                      │                          │
│                                      ▼                          │
│                              Chargement et exécution            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## II-2 Déroulement d'une Transaction

### Cycle de vie complet

```
┌─────────────────────────────────────────────────────────────────────────┐
│              DÉROULEMENT D'UNE TRANSACTION CICS                          │
└─────────────────────────────────────────────────────────────────────────┘

  UTILISATEUR              CICS                           SYSTÈME
      │                     │                                │
      │ 1. Saisie "MENU"    │                                │
      │    + ENTRÉE         │                                │
      │ ───────────────────►│                                │
      │                     │                                │
      │              2. TCP reçoit la demande                │
      │                     │                                │
      │              3. Recherche "MENU" dans PCT            │
      │                     │ ──────────────────────────────►│
      │                     │◄────── Programme: MENUPGM ─────│
      │                     │                                │
      │              4. Recherche MENUPGM dans PPT           │
      │                     │ ──────────────────────────────►│
      │                     │◄────── Adresse du programme ───│
      │                     │                                │
      │              5. KCP crée une TÂCHE                   │
      │                     │                                │
      │              6. PCP charge le programme              │
      │                     │ (si pas déjà en mémoire)       │
      │                     │                                │
      │              7. Exécution du programme               │
      │                     │                                │
      │                     │  ┌─────────────────────────┐   │
      │                     │  │ PROCEDURE DIVISION.     │   │
      │                     │  │   EXEC CICS             │   │
      │                     │  │     SEND TEXT...        │   │
      │                     │  │   END-EXEC              │   │
      │                     │  │   EXEC CICS             │   │
      │                     │  │     RETURN              │   │
      │                     │  │   END-EXEC              │   │
      │                     │  └─────────────────────────┘   │
      │                     │                                │
      │ 8. Affichage écran  │                                │
      │◄────────────────────│                                │
      │                     │                                │
      │              9. Tâche terminée, ressources libérées  │
      │                     │                                │
```

### Étapes détaillées

| Étape | Composant | Action |
|-------|-----------|--------|
| 1 | Terminal | Utilisateur saisit le code transaction |
| 2 | TCP | Réception de la demande |
| 3 | PCT | Recherche du programme associé |
| 4 | PPT | Localisation du programme |
| 5 | KCP | Création de la tâche |
| 6 | PCP | Chargement du programme |
| 7 | Programme | Exécution des instructions |
| 8 | TCP | Envoi du résultat au terminal |
| 9 | KCP | Destruction de la tâche |

### Mode pseudo-conversationnel

CICS utilise le mode **pseudo-conversationnel** pour optimiser les ressources :

```
┌─────────────────────────────────────────────────────────────────┐
│              MODE PSEUDO-CONVERSATIONNEL                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  CONVERSATIONNEL (inefficace)                                   │
│  ─────────────────────────────                                  │
│  ┌────────────────────────────────────────────────────────┐    │
│  │ Tâche active pendant toute la "réflexion" utilisateur │    │
│  │ (ressources bloquées 30 sec, 1 min, 5 min...)         │    │
│  └────────────────────────────────────────────────────────┘    │
│                                                                  │
│  PSEUDO-CONVERSATIONNEL (recommandé)                            │
│  ────────────────────────────────────                           │
│  ┌────────┐        ┌────────┐        ┌────────┐               │
│  │ Tâche 1│        │ Tâche 2│        │ Tâche 3│               │
│  │(affiche)│        │(traite)│        │(affiche)│               │
│  └───┬────┘        └───┬────┘        └───┬────┘               │
│      │                 │                 │                      │
│      │◄── Attente ────►│◄── Attente ────►│                      │
│      │   utilisateur   │   utilisateur   │                      │
│      │ (pas de tâche)  │ (pas de tâche)  │                      │
│                                                                  │
│  RETURN TRANSID('XXXX') COMMAREA(...)                           │
│  → Fin de tâche mais préparation du prochain appel             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### États d'une tâche

```
       ┌──────────────┐
       │    CRÉÉE     │ ◄── Transaction lancée
       └──────┬───────┘
              │
              ▼
       ┌──────────────┐
       │    PRÊTE     │ ◄── En attente du dispatcher
       └──────┬───────┘
              │
              ▼
       ┌──────────────┐         ┌──────────────┐
       │  EN COURS    │ ◄─────► │  EN ATTENTE  │
       │  (RUNNING)   │         │  (WAITING)   │
       └──────┬───────┘         └──────────────┘
              │                   (I/O, ressource,
              │                    DELAY, WAIT)
              ▼
       ┌──────────────┐
       │   TERMINÉE   │ ◄── RETURN exécuté
       └──────────────┘
```

## II-3 Bloc EIB (Execute Interface Block)

### Présentation

L'**EIB** (Execute Interface Block) est une zone de communication **automatiquement fournie** à chaque programme CICS. Elle contient des informations essentielles sur :

- La transaction en cours
- Le terminal utilisateur
- L'état des commandes CICS
- La date et l'heure

```
┌─────────────────────────────────────────────────────────────────┐
│                         BLOC EIB                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  CICS fournit automatiquement l'EIB au programme        │   │
│  │                                                          │   │
│  │  Programme COBOL                                         │   │
│  │  ─────────────────                                       │   │
│  │  LINKAGE SECTION.                                        │   │
│  │  01 DFHEIBLK.  ◄── EIB (généré automatiquement)         │   │
│  │     05 EIBTIME   ...                                     │   │
│  │     05 EIBDATE   ...                                     │   │
│  │     05 EIBTRNID  ...                                     │   │
│  │     ...                                                  │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Structure de l'EIB

L'EIB est défini dans le copybook **DFHEIBLK** :

```cobol
       01 DFHEIBLK.
          02 EIBTIME      PIC S9(7) COMP-3.    *> Heure 0HHMMSS
          02 EIBDATE      PIC S9(7) COMP-3.    *> Date 0CYYDDD
          02 EIBTRNID     PIC X(4).            *> Code transaction
          02 EIBTASKN     PIC S9(7) COMP-3.    *> Numéro de tâche
          02 EIBTRMID     PIC X(4).            *> ID terminal
          02 DFHEIGDI     PIC S9(4) COMP.      *> Réservé
          02 EIBCPOSN     PIC S9(4) COMP.      *> Position curseur
          02 EIBCALEN     PIC S9(4) COMP.      *> Longueur COMMAREA
          02 EIBAID       PIC X(1).            *> Touche AID appuyée
          02 EIBFN        PIC X(2).            *> Code fonction CICS
          02 EIBRCODE     PIC X(6).            *> Code retour
          02 EIBDS        PIC X(8).            *> Nom dataset
          02 EIBREQID     PIC X(8).            *> ID requête
          02 EIBRSRCE     PIC X(8).            *> Nom ressource
          02 EIBSYNC      PIC X(1).            *> Syncpoint
          02 EIBFREE      PIC X(1).            *> Free session
          02 EIBRECV      PIC X(1).            *> Receive issued
          02 EIBSEND      PIC X(1).            *> Send issued
          02 EIBATT       PIC X(1).            *> Attach
          02 EIBEOC       PIC X(1).            *> End of chain
          02 EIBFMH       PIC X(1).            *> FMH received
          02 EIBCOMPL     PIC X(1).            *> Complete data
          02 EIBSIG       PIC X(1).            *> Signal
          02 EIBCONF      PIC X(1).            *> Confirm
          02 EIBERR       PIC X(1).            *> Error
          02 EIBERRCD     PIC X(4).            *> Error code
          02 EIBSYNRB     PIC X(1).            *> Syncpoint rollback
          02 EIBNODAT     PIC X(1).            *> No data
          02 EIBRESP      PIC S9(8) COMP.      *> Response code
          02 EIBRESP2     PIC S9(8) COMP.      *> Response code 2
```

### Champs principaux de l'EIB

| Champ | Type | Description | Exemple |
|-------|------|-------------|---------|
| **EIBTIME** | COMP-3 | Heure au format 0HHMMSS | 0143052 = 14:30:52 |
| **EIBDATE** | COMP-3 | Date au format 0CYYDDD | 0124339 = 339ème jour de 2024 |
| **EIBTRNID** | X(4) | Code de la transaction | "MENU" |
| **EIBTASKN** | COMP-3 | Numéro unique de la tâche | 0012345 |
| **EIBTRMID** | X(4) | Identifiant du terminal | "T001" |
| **EIBCPOSN** | COMP | Position du curseur (0-1919) | 480 |
| **EIBCALEN** | COMP | Longueur de la COMMAREA | 100 |
| **EIBAID** | X(1) | Touche AID appuyée | X'7D' (ENTER) |
| **EIBRESP** | COMP | Code retour de la dernière commande | 0 (NORMAL) |
| **EIBRESP2** | COMP | Code retour secondaire | 0 |
| **EIBDS** | X(8) | Nom du dernier fichier accédé | "CLIENTS " |

### Codes EIBAID (touches AID)

Les touches AID (Attention IDentifier) sont stockées dans EIBAID :

```cobol
      * Copybook DFHAID - Valeurs des touches
       01 DFHAID.
          05 DFHENTER    PIC X VALUE X'7D'.   *> Touche Entrée
          05 DFHCLEAR    PIC X VALUE X'6D'.   *> Touche Clear
          05 DFHCLRP     PIC X VALUE X'6E'.   *> Clear Partition
          05 DFHPEN      PIC X VALUE X'7E'.   *> Light Pen
          05 DFHOPID     PIC X VALUE X'E6'.   *> Operator ID
          05 DFHPA1      PIC X VALUE X'6C'.   *> PA1
          05 DFHPA2      PIC X VALUE X'6E'.   *> PA2
          05 DFHPA3      PIC X VALUE X'6B'.   *> PA3
          05 DFHPF1      PIC X VALUE X'F1'.   *> PF1
          05 DFHPF2      PIC X VALUE X'F2'.   *> PF2
          05 DFHPF3      PIC X VALUE X'F3'.   *> PF3
          05 DFHPF4      PIC X VALUE X'F4'.   *> PF4
          05 DFHPF5      PIC X VALUE X'F5'.   *> PF5
          05 DFHPF6      PIC X VALUE X'F6'.   *> PF6
          05 DFHPF7      PIC X VALUE X'F7'.   *> PF7
          05 DFHPF8      PIC X VALUE X'F8'.   *> PF8
          05 DFHPF9      PIC X VALUE X'F9'.   *> PF9
          05 DFHPF10     PIC X VALUE X'7A'.   *> PF10
          05 DFHPF11     PIC X VALUE X'7B'.   *> PF11
          05 DFHPF12     PIC X VALUE X'7C'.   *> PF12
```

### Exemples d'utilisation de l'EIB

#### Tester la touche appuyée

```cobol
       WORKING-STORAGE SECTION.
           COPY DFHAID.

       PROCEDURE DIVISION.
      * Tester quelle touche a été appuyée
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM TRAITEMENT-SAISIE
               WHEN DFHPF3
                   PERFORM FIN-PROGRAMME
               WHEN DFHPF7
                   PERFORM PAGE-PRECEDENTE
               WHEN DFHPF8
                   PERFORM PAGE-SUIVANTE
               WHEN DFHCLEAR
                   PERFORM REINITIALISER-ECRAN
               WHEN OTHER
                   MOVE 'TOUCHE NON GEREE' TO WS-MESSAGE
           END-EVALUATE.
```

#### Vérifier le code retour

```cobol
       PROCEDURE DIVISION.
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    RESP(WS-RESP)
           END-EXEC

      * Analyser le résultat via EIBRESP
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                   PERFORM AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE 'CLIENT INTROUVABLE' TO WS-MESSAGE
               WHEN DFHRESP(DISABLED)
                   MOVE 'FICHIER INDISPONIBLE' TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'ERREUR TECHNIQUE' TO WS-MESSAGE
           END-EVALUATE.
```

#### Récupérer les informations de contexte

```cobol
       WORKING-STORAGE SECTION.
       01 WS-INFO-CONTEXTE.
          05 WS-TRANS         PIC X(4).
          05 WS-TERMINAL      PIC X(4).
          05 WS-HEURE         PIC 9(6).
          05 WS-DATE          PIC 9(7).

       PROCEDURE DIVISION.
      * Récupérer le contexte d'exécution
           MOVE EIBTRNID TO WS-TRANS
           MOVE EIBTRMID TO WS-TERMINAL
           MOVE EIBTIME  TO WS-HEURE
           MOVE EIBDATE  TO WS-DATE

           DISPLAY 'Transaction: ' WS-TRANS
           DISPLAY 'Terminal   : ' WS-TERMINAL
           DISPLAY 'Heure      : ' WS-HEURE
           DISPLAY 'Date       : ' WS-DATE.
```

#### Détecter le premier passage

```cobol
       PROCEDURE DIVISION.
      * EIBCALEN = 0 signifie pas de COMMAREA
      * donc c'est le premier appel de la transaction
           IF EIBCALEN = 0
               PERFORM INITIALISATION
               PERFORM AFFICHER-ECRAN-VIDE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM TRAITER-SAISIE
           END-IF.
```

## II-4 Notion de commandes CICS

### Structure générale

Toutes les commandes CICS suivent la même syntaxe :

```cobol
       EXEC CICS
           commande [option1(valeur1)]
                    [option2(valeur2)]
                    [option3(valeur3)]
                    [RESP(variable-resp)]
                    [RESP2(variable-resp2)]
       END-EXEC
```

**Règles syntaxiques :**

| Règle | Description |
|-------|-------------|
| Délimiteurs | `EXEC CICS` ... `END-EXEC` |
| Commande | En majuscules, première instruction |
| Options | Entre parenthèses, séparées par espaces |
| Valeurs | Variables COBOL ou littéraux |
| RESP | Optionnel, récupère le code retour |

### Catégories de commandes

```
┌─────────────────────────────────────────────────────────────────┐
│                   CATÉGORIES DE COMMANDES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │
│  │   PROGRAMME     │  │    TERMINAL     │  │    FICHIERS     │ │
│  │                 │  │                 │  │                 │ │
│  │  RETURN         │  │  SEND           │  │  READ           │ │
│  │  LINK           │  │  RECEIVE        │  │  WRITE          │ │
│  │  XCTL           │  │  SEND MAP       │  │  REWRITE        │ │
│  │  ABEND          │  │  RECEIVE MAP    │  │  DELETE         │ │
│  │                 │  │  SEND TEXT      │  │  STARTBR        │ │
│  └─────────────────┘  └─────────────────┘  │  READNEXT       │ │
│                                            │  ENDBR          │ │
│  ┌─────────────────┐  ┌─────────────────┐  └─────────────────┘ │
│  │    STOCKAGE     │  │     TEMPS       │                      │
│  │                 │  │                 │                      │
│  │  WRITEQ TS      │  │  START          │                      │
│  │  READQ TS       │  │  DELAY          │                      │
│  │  DELETEQ TS     │  │  ASKTIME        │                      │
│  │  WRITEQ TD      │  │  FORMATTIME     │                      │
│  │  READQ TD       │  │                 │                      │
│  └─────────────────┘  └─────────────────┘                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Commandes de contrôle de programme

#### RETURN - Fin du programme

```cobol
      * Retour simple à CICS
           EXEC CICS
               RETURN
           END-EXEC

      * Retour avec réinvocation (pseudo-conversationnel)
           EXEC CICS
               RETURN TRANSID('MENU')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC
```

#### LINK - Appel avec retour

```cobol
      * Appeler un sous-programme et revenir
           EXEC CICS
               LINK PROGRAM('CALCPGM')
                    COMMAREA(WS-DATA)
                    LENGTH(100)
           END-EXEC
      * Le contrôle revient ici après CALCPGM
```

#### XCTL - Transfert sans retour

```cobol
      * Transférer le contrôle définitivement
           EXEC CICS
               XCTL PROGRAM('MENUPGM')
                    COMMAREA(WS-DATA)
                    LENGTH(100)
           END-EXEC
      * On ne revient JAMAIS ici
```

#### ABEND - Arrêt anormal

```cobol
      * Terminer anormalement avec un code
           EXEC CICS
               ABEND ABCODE('ERR1')
           END-EXEC
```

### Commandes de gestion d'écran

#### SEND TEXT - Envoi de texte simple

```cobol
       01 WS-MESSAGE PIC X(50) VALUE 'BIENVENUE DANS CICS'.

           EXEC CICS
               SEND TEXT FROM(WS-MESSAGE)
                    LENGTH(50)
                    ERASE
           END-EXEC
```

#### SEND MAP - Envoi d'écran formaté

```cobol
           EXEC CICS
               SEND MAP('MENUMAP')
                    MAPSET('MENUSET')
                    FROM(MENUMAPO)
                    ERASE
           END-EXEC
```

#### RECEIVE MAP - Réception des données saisies

```cobol
           EXEC CICS
               RECEIVE MAP('MENUMAP')
                       MAPSET('MENUSET')
                       INTO(MENUMAPI)
           END-EXEC
```

### Commandes de gestion de fichiers

#### READ - Lecture directe

```cobol
       01 WS-CLIENT-KEY  PIC X(8).
       01 WS-CLIENT-REC  PIC X(200).

           MOVE '12345678' TO WS-CLIENT-KEY

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    RESP(WS-RESP)
           END-EXEC
```

#### WRITE - Création

```cobol
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-CLIENT-REC)
                     RIDFLD(WS-CLIENT-KEY)
                     RESP(WS-RESP)
           END-EXEC
```

#### READ UPDATE + REWRITE - Mise à jour

```cobol
      * Lecture pour mise à jour (verrouillage)
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

      * Modification des données
           MOVE 'NOUVEAU NOM' TO CLI-NOM OF WS-CLIENT-REC

      * Réécriture
           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT-REC)
                       RESP(WS-RESP)
           END-EXEC
```

#### DELETE - Suppression

```cobol
           EXEC CICS
               DELETE FILE('CLIENTS')
                      RIDFLD(WS-CLIENT-KEY)
                      RESP(WS-RESP)
           END-EXEC
```

#### Parcours séquentiel (Browse)

```cobol
      * Début du parcours
           EXEC CICS
               STARTBR FILE('CLIENTS')
                       RIDFLD(WS-CLIENT-KEY)
                       GTEQ
           END-EXEC

      * Lecture des enregistrements
           PERFORM UNTIL WS-FIN-FICHIER = 'O'
               EXEC CICS
                   READNEXT FILE('CLIENTS')
                            INTO(WS-CLIENT-REC)
                            RIDFLD(WS-CLIENT-KEY)
                            RESP(WS-RESP)
               END-EXEC
               IF EIBRESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-FICHIER
               ELSE
                   PERFORM TRAITER-CLIENT
               END-IF
           END-PERFORM

      * Fin du parcours
           EXEC CICS
               ENDBR FILE('CLIENTS')
           END-EXEC
```

### Commandes de stockage temporaire

#### WRITEQ TS - Écriture

```cobol
           EXEC CICS
               WRITEQ TS QUEUE('TSPANIER')
                         FROM(WS-ARTICLE)
                         LENGTH(100)
                         RESP(WS-RESP)
           END-EXEC
```

#### READQ TS - Lecture

```cobol
           EXEC CICS
               READQ TS QUEUE('TSPANIER')
                        INTO(WS-ARTICLE)
                        LENGTH(WS-LEN)
                        ITEM(1)
                        RESP(WS-RESP)
           END-EXEC
```

#### DELETEQ TS - Suppression

```cobol
           EXEC CICS
               DELETEQ TS QUEUE('TSPANIER')
                          RESP(WS-RESP)
           END-EXEC
```

### Gestion des erreurs

#### Option RESP

```cobol
       WORKING-STORAGE SECTION.
       01 WS-RESP      PIC S9(8) COMP.
       01 WS-RESP2     PIC S9(8) COMP.

       PROCEDURE DIVISION.
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM TRAITEMENT-OK
               WHEN DFHRESP(NOTFND)
                   MOVE 'CLIENT NON TROUVE' TO WS-MSG
               WHEN DFHRESP(DISABLED)
                   MOVE 'FICHIER FERME' TO WS-MSG
               WHEN DFHRESP(INVREQ)
                   MOVE 'REQUETE INVALIDE' TO WS-MSG
               WHEN OTHER
                   STRING 'ERREUR RESP=' WS-RESP
                          ' RESP2=' WS-RESP2
                          DELIMITED SIZE INTO WS-MSG
           END-EVALUATE.
```

#### Codes RESP courants

| Code | Constante | Description |
|------|-----------|-------------|
| 0 | NORMAL | Succès |
| 12 | FILENOTFOUND | Fichier non défini dans FCT |
| 13 | NOTFND | Enregistrement non trouvé |
| 14 | DUPREC | Clé en double |
| 15 | DUPKEY | Index alternatif en double |
| 16 | INVREQ | Requête invalide |
| 18 | NOSPACE | Plus d'espace disponible |
| 19 | NOTOPEN | Fichier non ouvert |
| 20 | ENDFILE | Fin de fichier atteinte |
| 22 | DISABLED | Ressource désactivée |
| 26 | PGMIDERR | Programme non trouvé |
| 27 | TRANSIDERR | Transaction non trouvée |
| 28 | ENDDATA | Plus de données |
| 32 | ILLOGIC | Erreur logique VSAM |
| 44 | QIDERR | Queue non trouvée |
| 70 | ITEMERR | Numéro d'item invalide |

### COMMAREA - Zone de communication

La **COMMAREA** permet de passer des données entre transactions ou programmes :

```cobol
       WORKING-STORAGE SECTION.
       01 WS-COMMAREA.
          05 CA-ACTION        PIC X(1).
             88 CA-INIT       VALUE 'I'.
             88 CA-AFFICHER   VALUE 'A'.
             88 CA-MODIFIER   VALUE 'M'.
          05 CA-NUM-CLIENT    PIC 9(8).
          05 CA-NOM           PIC X(30).
          05 CA-MESSAGE       PIC X(50).

       LINKAGE SECTION.
       01 DFHCOMMAREA         PIC X(89).

       PROCEDURE DIVISION.
      *─────────────────────────────────────────────────────────────
      * Premier passage : EIBCALEN = 0 (pas de COMMAREA)
      *─────────────────────────────────────────────────────────────
           IF EIBCALEN = 0
               INITIALIZE WS-COMMAREA
               SET CA-INIT TO TRUE
               PERFORM AFFICHER-ECRAN-INITIAL
           ELSE
      *─────────────────────────────────────────────────────────────
      * Passages suivants : récupérer la COMMAREA
      *─────────────────────────────────────────────────────────────
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM TRAITER-SAISIE
           END-IF

      *─────────────────────────────────────────────────────────────
      * Retour à CICS avec COMMAREA pour prochain passage
      *─────────────────────────────────────────────────────────────
           EXEC CICS
               RETURN TRANSID('CLNT')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE II - RÉSUMÉ                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  II-1 COMPOSANTS                                                         │
│       • TCP : Terminaux             • FC  : Fichiers                    │
│       • PCP : Programmes            • SC  : Mémoire                     │
│       • KCP : Tâches                • IC  : Temps                       │
│       • TS  : Stockage temporaire   • TD  : Files d'attente            │
│       • BMS : Écrans formatés                                           │
│       • Tables : PCT, PPT, FCT, TCT, DCT                               │
│                                                                          │
│  II-2 DÉROULEMENT TRANSACTION                                           │
│       • Transaction → PCT → PPT → Chargement → Exécution → Retour      │
│       • Mode pseudo-conversationnel (optimisation ressources)           │
│       • États : CRÉÉE → PRÊTE → RUNNING ↔ WAITING → TERMINÉE           │
│                                                                          │
│  II-3 BLOC EIB                                                          │
│       • Zone de communication automatique                               │
│       • Champs clés :                                                   │
│         - EIBTRNID : Code transaction    - EIBAID  : Touche AID        │
│         - EIBTRMID : ID terminal         - EIBRESP : Code retour       │
│         - EIBCALEN : Longueur COMMAREA   - EIBTIME : Heure             │
│       • DFHAID : Copybook des touches (DFHENTER, DFHPF1-12...)        │
│                                                                          │
│  II-4 COMMANDES CICS                                                    │
│       • Syntaxe : EXEC CICS ... END-EXEC                               │
│       • Programme : RETURN, LINK, XCTL, ABEND                          │
│       • Terminal  : SEND, RECEIVE, SEND MAP, RECEIVE MAP               │
│       • Fichiers  : READ, WRITE, REWRITE, DELETE, STARTBR/ENDBR       │
│       • Stockage  : WRITEQ TS/TD, READQ TS/TD, DELETEQ TS/TD          │
│       • Gestion erreurs : RESP, RESP2, DFHRESP(xxx)                    │
│       • COMMAREA  : Passage de données inter-transactions              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [← Précédent : Présentation générale](01-presentation-generale.md)
- [Suivant : SGBD IMS →](03-sgbd-ims.md)
- [Retour au sommaire](README.md)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Présentation Générale](01-presentation-generale.md) | [Chapitre III - SGBD IMS](03-sgbd-ims.md) |

---
*Formation COBOL - Module CICS*
