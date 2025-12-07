# Chapitre III - TSO (Time Sharing Option)

## Introduction

Ce chapitre présente TSO, l'interface interactive de z/OS :
1. Présentation générale et session TSO
2. LOGON & LOGOFF
3. Gestion des travaux
4. Gestion des données

---

## III-1 Présentation générale : mode Interactif et Transactionnel

### Modes de traitement z/OS

```
┌─────────────────────────────────────────────────────────────────┐
│                    MODES DE TRAITEMENT Z/OS                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   MODE BATCH (Différé)                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │   • Soumission de travaux (JCL)                        │   │
│   │   • Exécution en file d'attente                        │   │
│   │   • Pas d'interaction utilisateur                      │   │
│   │   • Traitements de masse                               │   │
│   │   • Résultats consultables après exécution             │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   MODE INTERACTIF (TSO)                                         │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │   • Dialogue direct avec le système                    │   │
│   │   • Commandes exécutées immédiatement                  │   │
│   │   • Réponse en temps réel                              │   │
│   │   • Développement, tests, administration               │   │
│   │   • Interface ligne de commande ou ISPF                │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   MODE TRANSACTIONNEL (CICS/IMS)                                │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │   • Transactions courtes et rapides                    │   │
│   │   • Milliers d'utilisateurs simultanés                 │   │
│   │   • Applications métier (banque, retail...)            │   │
│   │   • Temps de réponse sub-seconde                       │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Définition de TSO

**TSO (Time Sharing Option)** est le sous-système interactif de z/OS qui permet aux utilisateurs de :

- Exécuter des commandes en temps réel
- Gérer des fichiers (datasets)
- Soumettre et contrôler des travaux batch
- Développer et tester des programmes
- Administrer le système

```
┌─────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE TSO                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   UTILISATEURS                                                  │
│   ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐                      │
│   │User1│ │User2│ │User3│ │User4│ │User5│  ...                 │
│   └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘                      │
│      │       │       │       │       │                          │
│      └───────┴───────┼───────┴───────┘                          │
│                      │                                          │
│                      ▼                                          │
│              ┌───────────────┐                                  │
│              │     VTAM      │  Gestionnaire réseau             │
│              │   (Réseau)    │                                  │
│              └───────┬───────┘                                  │
│                      │                                          │
│                      ▼                                          │
│              ┌───────────────┐                                  │
│              │     TCAS      │  TSO Control Address Space       │
│              │  (Contrôleur) │                                  │
│              └───────┬───────┘                                  │
│                      │                                          │
│         ┌────────────┼────────────┐                             │
│         ▼            ▼            ▼                             │
│   ┌──────────┐ ┌──────────┐ ┌──────────┐                       │
│   │  TSO/E   │ │  TSO/E   │ │  TSO/E   │  Address Spaces       │
│   │  User1   │ │  User2   │ │  User3   │  (1 par utilisateur)  │
│   └──────────┘ └──────────┘ └──────────┘                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-1-1 Fonctions principales de TSO

| Catégorie | Fonctions |
|-----------|-----------|
| **Gestion de session** | LOGON, LOGOFF, PROFILE |
| **Gestion des données** | ALLOCATE, DELETE, RENAME, COPY |
| **Gestion des travaux** | SUBMIT, STATUS, CANCEL, OUTPUT |
| **Édition** | EDIT (éditeur ligne), ISPF (plein écran) |
| **Utilitaires** | LISTDS, LISTCAT, LISTALC |
| **Communication** | SEND, LISTBC |
| **Exécution** | CALL, EXEC (REXX/CLIST) |

### III-1-2 Relation entre TSO et ISPF

```
┌─────────────────────────────────────────────────────────────────┐
│                    TSO vs ISPF                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   TSO (Time Sharing Option)                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Sous-système de base z/OS                           │   │
│   │  • Interface ligne de commande                         │   │
│   │  • Commandes natives (ALLOCATE, SUBMIT...)             │   │
│   │  • Mode "READY" (prompt TSO)                           │   │
│   │                                                         │   │
│   │  READY                                                  │   │
│   │  listds 'sys1.proclib'                                 │   │
│   │  --SYS1.PROCLIB                                        │   │
│   │  --RECFM-FB--LRECL-80--BLKSIZE-27920                   │   │
│   │  READY                                                  │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                          │                                       │
│                          │ ISPF s'exécute SOUS TSO              │
│                          ▼                                       │
│   ISPF (Interactive System Productivity Facility)               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Application exécutée sous TSO                       │   │
│   │  • Interface plein écran (panels)                      │   │
│   │  • Menus et fonctions intégrées                        │   │
│   │  • Éditeur puissant (EDIT)                             │   │
│   │  • Utilitaires (3.4, 3.2, etc.)                        │   │
│   │                                                         │   │
│   │  ┌─────────────────────────────────────────────────┐   │   │
│   │  │  ISPF Primary Option Menu                       │   │   │
│   │  │  Option ===> _                                  │   │   │
│   │  │                                                 │   │   │
│   │  │  0  Settings      1  View       2  Edit        │   │   │
│   │  │  3  Utilities     4  Foreground 5  Batch       │   │   │
│   │  │  6  Command       7  Dialog     ...            │   │   │
│   │  └─────────────────────────────────────────────────┘   │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Comparaison TSO natif vs ISPF

| Aspect | TSO natif | ISPF |
|--------|-----------|------|
| **Interface** | Ligne de commande | Plein écran (panels) |
| **Navigation** | Commandes textuelles | Menus, touches fonction |
| **Édition** | EDIT (ligne par ligne) | EDIT (plein écran) |
| **Apprentissage** | Plus difficile | Plus intuitif |
| **Productivité** | Limitée | Élevée |
| **Utilisation** | Scripts, automatisation | Développement quotidien |

---

### III-1-3 Structure et environnement TSO

#### Composants de l'environnement TSO

```
┌─────────────────────────────────────────────────────────────────┐
│                    ENVIRONNEMENT TSO                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    TSO/E                                │   │
│   │              (TSO Extensions)                           │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │                                                         │   │
│   │  ┌───────────┐  ┌───────────┐  ┌───────────┐          │   │
│   │  │   CLIST   │  │   REXX    │  │   EXEC    │          │   │
│   │  │ (Scripts) │  │ (Scripts) │  │ Processor │          │   │
│   │  └───────────┘  └───────────┘  └───────────┘          │   │
│   │                                                         │   │
│   │  ┌───────────┐  ┌───────────┐  ┌───────────┐          │   │
│   │  │ ISPF/PDF  │  │   SDSF    │  │   RACF    │          │   │
│   │  │ (Éditeur) │  │  (Spool)  │  │(Sécurité) │          │   │
│   │  └───────────┘  └───────────┘  └───────────┘          │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Datasets utilisateur typiques :                               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  userid.ISPF.ISPPROF   - Profil ISPF                   │   │
│   │  userid.CLIST          - Scripts CLIST                 │   │
│   │  userid.REXX.EXEC      - Scripts REXX                  │   │
│   │  userid.COBOL.SOURCE   - Sources COBOL                 │   │
│   │  userid.COBOL.LOAD     - Modules exécutables           │   │
│   │  userid.JCL            - Procédures JCL                │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Variables d'environnement TSO

| Variable | Description |
|----------|-------------|
| **SYSPROC** | Bibliothèques de procédures (CLIST/REXX) |
| **SYSEXEC** | Bibliothèques REXX |
| **ISPPLIB** | Panels ISPF |
| **ISPMLIB** | Messages ISPF |
| **ISPSLIB** | Skeletons ISPF |
| **ISPTLIB** | Tables ISPF |

---

## III-2 Session TSO : LOGON et LOGOFF

### III-2-1 LOGON - Connexion à TSO

#### Écran de connexion

```
┌─────────────────────────────────────────────────────────────────┐
│                    ÉCRAN LOGON TSO                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │                    TSO/E LOGON                          │   │
│   │                                                         │   │
│   │   Enter LOGON parameters below:              RACF LOGON │   │
│   │                                                         │   │
│   │   Userid    ===> USER01_                                │   │
│   │   Password  ===> ________     (Password will not appear)│   │
│   │   Procedure ===> IKJACCNT     (Runnable procedure)     │   │
│   │   Acct Nmbr ===> ACCT001      (Account number)         │   │
│   │   Size      ===> 4096         (Region size in K)       │   │
│   │   Perform   ===>              (Performance group)      │   │
│   │   Command   ===> ISPF         (Initial command)        │   │
│   │   Group     ===>              (RACF group)             │   │
│   │                                                         │   │
│   │   Enter an 'S' before each option desired:             │   │
│   │   _Nomail   _Nonotice   _Reconnect   _OIDcard          │   │
│   │                                                         │   │
│   │   PF1/PF13 ==> Help   PF3/PF15 ==> Logoff              │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Paramètres LOGON

| Paramètre | Description | Exemple |
|-----------|-------------|---------|
| **Userid** | Identifiant utilisateur | USER01 |
| **Password** | Mot de passe RACF | (masqué) |
| **Procedure** | Procédure de connexion | IKJACCNT |
| **Acct Nmbr** | Numéro de compte facturation | ACCT001 |
| **Size** | Taille région mémoire (Ko) | 4096 |
| **Command** | Commande initiale | ISPF |
| **Group** | Groupe RACF | DEVGROUP |

#### Syntaxe commande LOGON

```
LOGON userid/password ACCT(account) PROC(procedure) SIZE(size)
      COMMAND(cmd) GROUP(group) NOMAIL NONOTICE RECONNECT

Exemples :
┌─────────────────────────────────────────────────────────────────┐
│                                                                  │
│  LOGON USER01                                                   │
│  (Connexion simple, mot de passe demandé)                       │
│                                                                  │
│  LOGON USER01 PROC(DBSPROC) SIZE(8192)                         │
│  (Connexion avec procédure DB2 et 8 Mo de région)              │
│                                                                  │
│  LOGON USER01 COMMAND(ISPF) NOMAIL                             │
│  (Connexion directe vers ISPF, sans messages)                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Processus de LOGON

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCESSUS LOGON                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   1. Utilisateur entre userid/password                          │
│      │                                                          │
│      ▼                                                          │
│   2. RACF vérifie les credentials                               │
│      │                                                          │
│      ├──► ÉCHEC : Message erreur, nouvelle tentative           │
│      │                                                          │
│      ▼ SUCCÈS                                                   │
│   3. TCAS crée un Address Space pour l'utilisateur             │
│      │                                                          │
│      ▼                                                          │
│   4. Exécution de la procédure LOGON (IKJACCNT)                │
│      │                                                          │
│      ▼                                                          │
│   5. Allocation des datasets système et utilisateur             │
│      │                                                          │
│      ▼                                                          │
│   6. Exécution de la commande initiale (ex: ISPF)              │
│      │                                                          │
│      ▼                                                          │
│   7. Session TSO active                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-2-2 LOGOFF - Déconnexion de TSO

#### Méthodes de déconnexion

```
┌─────────────────────────────────────────────────────────────────┐
│                    MÉTHODES LOGOFF                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   1. Commande LOGOFF (depuis READY)                             │
│      ┌─────────────────────────────────────────────────────┐    │
│      │ READY                                               │    │
│      │ logoff                                              │    │
│      │ IKJ56470I USER01 LOGGED OFF TSO AT 14:35:22        │    │
│      └─────────────────────────────────────────────────────┘    │
│                                                                  │
│   2. Depuis ISPF : Option X ou =X                               │
│      ┌─────────────────────────────────────────────────────┐    │
│      │ Option ===> X                                       │    │
│      └─────────────────────────────────────────────────────┘    │
│                                                                  │
│   3. Touche PF3 répétée jusqu'à sortie ISPF puis LOGOFF        │
│                                                                  │
│   4. LOGOFF HOLD (garde la session en attente)                 │
│      ┌─────────────────────────────────────────────────────┐    │
│      │ READY                                               │    │
│      │ logoff hold                                         │    │
│      │ (Session peut être reprise avec RECONNECT)          │    │
│      └─────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Options LOGOFF

| Option | Description |
|--------|-------------|
| **LOGOFF** | Déconnexion normale |
| **LOGOFF HOLD** | Garde la session pour reconnexion |
| **LOGOFF NOFLUSH** | Ne vide pas les buffers de sortie |

---

## III-3 Commandes TSO

### III-3-1 Commande HELP

La commande **HELP** fournit de l'aide sur les commandes TSO.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE HELP                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ HELP command [operand] [FUNCTION|SYNTAX|OPERANDS|ALL]  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ help                                                    │   │
│   │ (Liste toutes les commandes disponibles)               │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ help allocate                                           │   │
│   │ (Aide sur la commande ALLOCATE)                        │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ help allocate syntax                                    │   │
│   │ (Syntaxe de la commande ALLOCATE)                      │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ help listds all                                         │   │
│   │ (Documentation complète de LISTDS)                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-3-2 Liste des commandes TSO principales

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDES TSO PAR CATÉGORIE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   CONTRÔLE DE SESSION                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LOGON    - Connexion                                    │   │
│   │ LOGOFF   - Déconnexion                                  │   │
│   │ PROFILE  - Paramètres de session                        │   │
│   │ TERMINAL - Caractéristiques terminal                    │   │
│   │ TIME     - Affiche date et heure                        │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   GESTION DES DONNÉES (DATA MANAGEMENT)                         │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ ALLOCATE - Allouer un dataset                           │   │
│   │ FREE     - Libérer un dataset alloué                    │   │
│   │ DELETE   - Supprimer un dataset                         │   │
│   │ RENAME   - Renommer un dataset                          │   │
│   │ COPY     - Copier un dataset                            │   │
│   │ LISTDS   - Lister attributs d'un dataset               │   │
│   │ LISTCAT  - Lister entrées catalogue                     │   │
│   │ LISTALC  - Lister allocations courantes                │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   GESTION DES TRAVAUX (JOB MANAGEMENT)                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ SUBMIT   - Soumettre un job                             │   │
│   │ STATUS   - État d'un job                                │   │
│   │ CANCEL   - Annuler un job                               │   │
│   │ OUTPUT   - Consulter sortie d'un job                    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   COMMUNICATION                                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ SEND     - Envoyer un message                           │   │
│   │ LISTBC   - Lister messages broadcast                    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   EXÉCUTION                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ CALL     - Exécuter un programme                        │   │
│   │ EXEC     - Exécuter un CLIST/REXX                       │   │
│   │ RUN      - Exécuter en foreground                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-3-3 Commandes de contrôle de session

#### Commande PROFILE

Configure les paramètres de la session TSO.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE PROFILE                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ PROFILE [PREFIX(prefix)] [NOPREFIX]                    │   │
│   │         [PROMPT|NOPROMPT] [INTERCOM|NOINTERCOM]        │   │
│   │         [MSGID|NOMSGID] [MODE|NOMODE]                  │   │
│   │         [WTPMSG|NOWTPMSG] [PAUSE|NOPAUSE]              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ profile                                                 │   │
│   │ IKJ56688I PREFIX IS SET TO USER01                      │   │
│   │ IKJ56689I PROMPT MODE IS ON                            │   │
│   │ IKJ56691I INTERCOM MODE IS ON                          │   │
│   │ IKJ56694I MESSAGE ID MODE IS ON                        │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ profile prefix(prod)                                    │   │
│   │ (Datasets préfixés par PROD au lieu de USER01)         │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ profile noprefix                                        │   │
│   │ (Désactive le préfixe automatique)                     │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ profile nomsgid                                         │   │
│   │ (Masque les identifiants de messages)                  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Option | Description |
|--------|-------------|
| **PREFIX(hlq)** | Définit le préfixe par défaut des datasets |
| **NOPREFIX** | Pas de préfixe automatique |
| **PROMPT** | Demande les paramètres manquants |
| **MSGID** | Affiche les codes message (IKJxxxxx) |
| **INTERCOM** | Autorise réception de messages |
| **PAUSE** | Pause après chaque écran |

#### Commande SEND

Envoie un message à un autre utilisateur TSO.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE SEND                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ SEND 'message' USER(userid) [LOGON] [NOW]              │   │
│   │ SEND 'message' OPERATOR [CN(console)]                  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ send 'Salut, tu es dispo?' user(user02)                │   │
│   │ IKJ56702I MESSAGE SENT TO USER02                       │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ send 'Job terminé' user(user02) logon                  │   │
│   │ (Le message sera délivré au prochain LOGON)            │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ send 'Problème critique' operator                      │   │
│   │ (Message à la console opérateur)                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-3-4 Commandes de gestion des données (Data Management)

#### Commande ALLOCATE

Crée un nouveau dataset ou alloue un dataset existant.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE ALLOCATE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe simplifiée :                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ ALLOCATE DATASET(dsname) [NEW|OLD|SHR|MOD]             │   │
│   │          [DSORG(org)] [RECFM(fmt)] [LRECL(len)]        │   │
│   │          [BLKSIZE(size)] [SPACE(units,(primary,sec))]  │   │
│   │          [VOLUME(volser)] [UNIT(unit)]                 │   │
│   │          [FILE(ddname)] [CATALOG]                      │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ alloc da('user01.cobol.source') new catalog            │   │
│   │       dsorg(po) recfm(f,b) lrecl(80) blksize(27920)   │   │
│   │       space(5,2) tracks dir(10)                        │   │
│   │ (Crée une bibliothèque PDS pour sources COBOL)         │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ alloc da('user01.data.file') new catalog               │   │
│   │       dsorg(ps) recfm(f,b) lrecl(100) blksize(27900)  │   │
│   │       space(10,5) tracks                               │   │
│   │ (Crée un fichier séquentiel)                           │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ alloc da('user01.cobol.source') shr                    │   │
│   │ (Alloue un dataset existant en lecture partagée)       │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ alloc da('user01.temp') new delete                     │   │
│   │       space(1,1) tracks                                │   │
│   │ (Crée un dataset temporaire, supprimé à la fin)        │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Paramètre | Description | Valeurs |
|-----------|-------------|---------|
| **DATASET** | Nom du dataset | 'HLQ.NAME' ou DA() |
| **NEW** | Création d'un nouveau dataset | |
| **OLD** | Accès exclusif | |
| **SHR** | Accès partagé (lecture) | |
| **MOD** | Ajout en fin de fichier | |
| **DSORG** | Organisation | PS, PO, VS |
| **RECFM** | Format enregistrement | F, FB, V, VB, U |
| **LRECL** | Longueur enregistrement | 1-32760 |
| **BLKSIZE** | Taille bloc | Multiple LRECL |
| **SPACE** | Allocation | (TRK/CYL,(prim,sec)) |
| **DIR** | Blocs directory (PDS) | Nombre |
| **CATALOG** | Cataloguer le dataset | |

#### Commande DELETE

Supprime un dataset du système.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE DELETE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ DELETE 'dsname' [PURGE] [NOSCRATCH]                    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ delete 'user01.temp.file'                              │   │
│   │ ENTRY USER01.TEMP.FILE DELETED                         │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ delete 'user01.old.data' purge                         │   │
│   │ (Supprime même si date d'expiration non atteinte)      │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ delete 'user01.old.data' noscratch                     │   │
│   │ (Décatalogue sans supprimer physiquement)              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande LISTALC

Liste les datasets actuellement alloués dans la session.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE LISTALC                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LISTALC [STATUS] [HISTORY] [MEMBERS]                   │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemple :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ listalc status                                          │   │
│   │ --DDNAME---DISP--                                       │   │
│   │ --SYSPROC--                                             │   │
│   │   SYS1.CMDPROC                             KEEP,KEEP   │   │
│   │   USER01.CLIST                             KEEP,KEEP   │   │
│   │ --SYSEXEC--                                             │   │
│   │   USER01.REXX.EXEC                         KEEP,KEEP   │   │
│   │ --ISPPROF--                                             │   │
│   │   USER01.ISPF.ISPPROF                      KEEP,KEEP   │   │
│   │ --ISPPLIB--                                             │   │
│   │   ISP.SISPPENU                             KEEP,KEEP   │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande LISTDS

Affiche les attributs d'un dataset.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE LISTDS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LISTDS 'dsname' [STATUS] [MEMBERS] [HISTORY] [ALL]     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ listds 'sys1.proclib'                                  │   │
│   │ SYS1.PROCLIB                                           │   │
│   │ --RECFM-FB--LRECL-80--BLKSIZE-27920                    │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ listds 'sys1.proclib' members                          │   │
│   │ SYS1.PROCLIB                                           │   │
│   │ --RECFM-FB--LRECL-80--BLKSIZE-27920                    │   │
│   │ --MEMBERS--                                             │   │
│   │   ASMACL                                               │   │
│   │   ASMACLEG                                             │   │
│   │   ASMACLG                                              │   │
│   │   ...                                                   │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ listds 'user01.data' status                            │   │
│   │ USER01.DATA                                            │   │
│   │ --RECFM-FB--LRECL-100--BLKSIZE-27900                   │   │
│   │ --VOLUMES--                                             │   │
│   │   USER01                                               │   │
│   │ --IN USE-- (si le fichier est ouvert)                  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande LISTCAT

Liste les entrées du catalogue (VSAM et non-VSAM).

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE LISTCAT                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LISTCAT ENTRIES('mask') [ALL] [VOLUME] [ALLOCATION]    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ listcat entries('user01.*')                            │   │
│   │ NONVSAM ------- USER01.COBOL.SOURCE                    │   │
│   │ NONVSAM ------- USER01.COBOL.LOAD                      │   │
│   │ NONVSAM ------- USER01.JCL                             │   │
│   │ CLUSTER ------- USER01.VSAM.KSDS                       │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ listcat entries('user01.vsam.ksds') all                │   │
│   │ CLUSTER ------- USER01.VSAM.KSDS                       │   │
│   │      DATA ----- USER01.VSAM.KSDS.DATA                  │   │
│   │         KEYLEN-8  AVGLRECL-100  RKP-0                  │   │
│   │         RECORDS-TOTAL-1000  SPLITS-CI-5                │   │
│   │      INDEX ---- USER01.VSAM.KSDS.INDEX                 │   │
│   │         LEVELS-2                                        │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande LISTBC

Affiche les messages broadcast système.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE LISTBC                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LISTBC                                                  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemple :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ listbc                                                  │   │
│   │ IKJ56191I NO BROADCAST MESSAGES                        │   │
│   │                                                         │   │
│   │ ou                                                      │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ listbc                                                  │   │
│   │ *** SYSTEM MAINTENANCE PLANNED FOR SATURDAY 22:00 ***  │   │
│   │ *** PLEASE SAVE YOUR WORK AND LOGOFF BY 21:30 ***      │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande RENAME

Renomme un dataset.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE RENAME                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ RENAME 'old-dsname' 'new-dsname'                       │   │
│   │ RENAME 'dsname(old-member)' (new-member)               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ rename 'user01.old.data' 'user01.new.data'             │   │
│   │ IKJ56701I DSNAME CHANGED TO USER01.NEW.DATA            │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ rename 'user01.cobol.source(oldpgm)' (newpgm)          │   │
│   │ (Renomme un membre de PDS)                             │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### III-3-5 Commandes de gestion des travaux

#### Commande SUBMIT

Soumet un job JCL pour exécution batch.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE SUBMIT                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ SUBMIT 'dsname' | 'dsname(member)'                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ submit 'user01.jcl(myjob)'                             │   │
│   │ JOB MYJOB(JOB12345) SUBMITTED                          │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ submit 'user01.jcl.cntl(compile)'                      │   │
│   │ JOB COMPILE(JOB12346) SUBMITTED                        │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Le job est placé dans la file JES2/JES3 pour exécution.      │
│   Utilisez STATUS pour suivre son avancement.                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande STATUS

Affiche l'état des jobs soumis.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE STATUS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ STATUS [jobname] [jobid]                               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ status                                                  │   │
│   │ IKJ56192I JOB MYJOB(JOB12345) ON OUTPUT QUEUE          │   │
│   │ IKJ56192I JOB COMPILE(JOB12346) EXECUTING              │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ status myjob                                            │   │
│   │ IKJ56192I JOB MYJOB(JOB12345) ON OUTPUT QUEUE          │   │
│   │                                                         │   │
│   │ États possibles :                                       │   │
│   │ - ON INPUT QUEUE   : En attente d'exécution            │   │
│   │ - EXECUTING        : En cours d'exécution              │   │
│   │ - ON OUTPUT QUEUE  : Terminé, sortie disponible        │   │
│   │ - ON HELD QUEUE    : Suspendu                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande CANCEL

Annule un job en attente ou en cours d'exécution.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE CANCEL                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ CANCEL jobname [PURGE] [DUMP]                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ cancel myjob                                            │   │
│   │ IKJ56250I JOB MYJOB(JOB12345) CANCELLED                │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ cancel myjob purge                                      │   │
│   │ (Annule et supprime toutes les sorties)                │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ cancel myjob dump                                       │   │
│   │ (Annule avec génération d'un dump pour diagnostic)     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande OUTPUT

Consulte et gère les sorties (SYSOUT) d'un job.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE OUTPUT                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Syntaxe :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ OUTPUT jobname [PRINT|DELETE|HOLD|RELEASE]             │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples :                                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ READY                                                   │   │
│   │ output myjob print(*)                                  │   │
│   │ (Affiche toutes les sorties du job)                    │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ output myjob delete                                     │   │
│   │ (Supprime les sorties du job)                          │   │
│   │                                                         │   │
│   │ READY                                                   │   │
│   │ output myjob hold                                       │   │
│   │ (Met les sorties en attente)                           │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Note : SDSF (option S depuis ISPF) est plus pratique         │
│          pour consulter les sorties.                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Synthèse

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLÉS DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ✓ TSO = Interface interactive de z/OS                         │
│    • Exécution de commandes en temps réel                      │
│    • Alternative au mode batch (JCL)                           │
│    • Base pour ISPF (interface plein écran)                    │
│                                                                  │
│  ✓ SESSION TSO                                                  │
│    • LOGON : Connexion avec userid/password                    │
│    • LOGOFF : Déconnexion (HOLD pour maintenir)                │
│    • Chaque utilisateur = 1 Address Space                      │
│                                                                  │
│  ✓ COMMANDES DE SESSION                                         │
│    • PROFILE : Configuration session (PREFIX, MSGID...)        │
│    • SEND : Envoi de messages                                  │
│    • HELP : Documentation en ligne                             │
│                                                                  │
│  ✓ COMMANDES DATA MANAGEMENT                                    │
│    • ALLOCATE : Créer/allouer datasets                         │
│    • DELETE : Supprimer datasets                               │
│    • RENAME : Renommer datasets                                │
│    • LISTDS : Attributs d'un dataset                          │
│    • LISTCAT : Entrées catalogue                               │
│    • LISTALC : Allocations courantes                           │
│                                                                  │
│  ✓ COMMANDES JOB MANAGEMENT                                     │
│    • SUBMIT : Soumettre un job                                 │
│    • STATUS : État des jobs                                    │
│    • CANCEL : Annuler un job                                   │
│    • OUTPUT : Consulter les sorties                            │
│                                                                  │
│  ✓ CONSEIL : Utiliser ISPF pour le travail quotidien           │
│    TSO natif reste utile pour scripts et automatisation        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-mémoire des commandes TSO

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MÉMOIRE TSO                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SESSION                                                        │
│  ────────────────────────────────────────────────────────────── │
│  LOGON userid                    Connexion                      │
│  LOGOFF                          Déconnexion                    │
│  PROFILE                         Afficher paramètres            │
│  PROFILE PREFIX(hlq)             Définir préfixe                │
│  TIME                            Afficher date/heure            │
│                                                                  │
│  DATASETS                                                       │
│  ────────────────────────────────────────────────────────────── │
│  ALLOC DA('dsn') NEW CATALOG ... Créer dataset                 │
│  ALLOC DA('dsn') SHR              Allouer existant             │
│  FREE DA('dsn')                   Libérer allocation            │
│  DELETE 'dsn'                     Supprimer dataset             │
│  RENAME 'old' 'new'               Renommer                      │
│  LISTDS 'dsn' MEMBERS             Lister attributs             │
│  LISTCAT ENTRIES('mask')          Lister catalogue             │
│  LISTALC STATUS                   Allocations actives           │
│                                                                  │
│  JOBS                                                           │
│  ────────────────────────────────────────────────────────────── │
│  SUBMIT 'dsn(member)'             Soumettre job                 │
│  STATUS                           État des jobs                 │
│  CANCEL jobname                   Annuler job                   │
│  OUTPUT jobname PRINT(*)          Voir sorties                  │
│                                                                  │
│  COMMUNICATION                                                  │
│  ────────────────────────────────────────────────────────────── │
│  SEND 'msg' USER(uid)             Envoyer message               │
│  LISTBC                           Messages broadcast            │
│                                                                  │
│  AIDE                                                           │
│  ────────────────────────────────────────────────────────────── │
│  HELP                             Liste commandes               │
│  HELP command ALL                 Documentation complète        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre II - Fonctionnement Z/OS](02-fonctionnement-zos.md) | [Chapitre IV - ISPF](04-ispf.md) |
