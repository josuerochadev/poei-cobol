# Chapitre IX - Architecture Multicouches et Transactions TSI

## Table des matières

1. [IX-1 Architecture Multicouches](#ix-1-architecture-multicouches)
2. [IX-2 Modèle d'application Transactionnelle](#ix-2-modèle-dapplication-transactionnelle)
3. [IX-3 Conditions exceptionnelles](#ix-3-conditions-exceptionnelles)
4. [IX-4 Gestion des Terminaux](#ix-4-gestion-des-terminaux)
5. [IX-5 Définition des ressources](#ix-5-définition-des-ressources)
6. [IX-6 Étude de cas](#ix-6-étude-de-cas)
7. [IX-7 Échanges de données](#ix-7-échanges-de-données)
8. [IX-8 Les E/S sous CICS](#ix-8-les-es-sous-cics)

---

## IX-1 Architecture Multicouches

### Accesseurs Logiques et Physiques

Les concepts d'accesseurs logiques/physiques et transactions TSI sont essentiels en gestion des données et en systèmes transactionnels.

#### Accesseurs Logiques

Les accesseurs logiques permettent d'accéder aux données **indépendamment de leur implémentation physique** :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ACCESSEURS LOGIQUES                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  CARACTÉRISTIQUES :                                                      │
│  • Accès aux données indépendant de l'implémentation physique           │
│  • Modèles de données abstraits                                          │
│  • Indépendance vis-à-vis des fichiers ou tables                        │
│                                                                          │
│  UTILISATION SGF (VSAM) :          UTILISATION BDD (SQL) :              │
│  ┌──────────────────────┐          ┌──────────────────────┐             │
│  │  READ                │          │  SELECT              │             │
│  │  WRITE               │          │  INSERT              │             │
│  │  REWRITE             │          │  UPDATE              │             │
│  │  DELETE              │          │  DELETE              │             │
│  └──────────────────────┘          └──────────────────────┘             │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Accesseurs Physiques

Les accesseurs physiques interagissent **directement avec le stockage des données** :

| Caractéristique | Description |
|-----------------|-------------|
| **Interaction directe** | Disque, mémoire, fichiers binaires |
| **Optimisation** | Via index, partitions |
| **Fichiers VSAM** | Lecture/écriture via commandes directes |

#### Exemple : Accès VSAM via CICS

En CICS-COBOL, l'accès au Data Set VSAM se fait via des commandes CICS File Control :

```cobol
      *─── READ est un accès LOGIQUE ─────────────────────────────────
      *─── L'accès PHYSIQUE est géré en arrière-plan par CICS ────────
           EXEC CICS READ
               DATASET('FILE1')
               RIDFLD(KEY-VALUE)
               INTO(WS-RECORD)
           END-EXEC.
```

#### Exemple : Lecture et réécriture VSAM

```cobol
      *─── Lecture avec intention de mise à jour ─────────────────────
           EXEC CICS READ
               DATASET('FILE1')
               RIDFLD(KEY-VALUE)
               INTO(WS-RECORD)
               UPDATE
           END-EXEC.

      *─── Réécriture après modification ─────────────────────────────
           EXEC CICS REWRITE
               DATASET('FILE1')
               FROM(WS-RECORD)
           END-EXEC.
```

### Transactions TSI (Transaction Status Information)

**TSI** désigne les informations relevées sur le statut d'une transaction lors de son exécution. C'est une zone de contrôle utilisée dans les applications transactionnelles (IMS TM, CICS).

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    TRANSACTIONS TSI                                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  FONCTIONS DE LA TSI :                                                   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  1. Tracer le statut d'une transaction en cours                  │   │
│  │  2. Stocker les codes internes d'erreur                          │   │
│  │  3. Transporter des indicateurs de contexte entre programmes     │   │
│  │  4. Identifier l'environnement d'exécution (Batch / Online)      │   │
│  │  5. Gérer les comportements de reprise (restart, commit, rollback)│   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  Note : La TSI n'est pas un composant officiel IBM, mais une notion    │
│  très courante dans les architectures bancaires et Télécom.             │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Structure typique de la TSI

| Champ TSI | Signification |
|-----------|---------------|
| `TSI-REGION-ID` | Région IMS ou CICS courante |
| `TSI-USER-ID` | Identifiant utilisateur |
| `TSI-TERMINAL-ID` | Terminal LU / LTERM IMS |
| `TSI-TRAN-ID` | Code de Transaction |
| `TSI-RETURN-CODE` | Codes applicatifs (00, 04, 08, 12, 16) |
| `TSI-ERROR-CODE` | Code d'erreur interne |
| `TSI-FUNCTION` | Fonction métier demandée |
| `TSI-TRACE-ID` | Identifiant trace / diagnostic |
| `TSI-TIME` | Heure transactionnelle |
| `TSI-DATE` | Date transactionnelle |
| `TSI-ENV` | Environnement (ONLINE / BATCH / TEST / PROD) |

#### Initialisation de la TSI

```cobol
      *─── Gestion des retours ────────────────────────────────────────
       INIT-TSI.
           MOVE 0 TO TSI-RETURN-CODE.
           MOVE SPACES TO TSI-BLOCK.
           MOVE 0 TO TSI-ERROR-CODE.
      *─── Identification technique ───────────────────────────────────
           MOVE SPACES TO TSI-ERROR-MESSAGE.
           MOVE WS-REGION TO TSI-REGION-ID.
           MOVE 0 TO TSI-SEVERITY.
           MOVE WS-TRAN TO TSI-TRAN-CODE.
      *─── Horodatage ─────────────────────────────────────────────────
           MOVE WS-PGM TO TSI-PROGRAM-ID.
           MOVE WS-DATE TO TSI-DATE-AAAAMMJJ.
           MOVE WS-USER TO TSI-USER-ID.
           MOVE WS-TIME TO TSI-TIME-HHMMSS.
           MOVE WS-TERM TO TSI-TERMINAL-ID.
      *─── Contexte métier ────────────────────────────────────────────
           MOVE 'INIT' TO TSI-FUNCTION-CODE.
           MOVE '00' TO TSI-SUB-FUNCTION.
```

#### Chargement TSI dans un programme CICS

```cobol
      *─── Récupération des données EIB ───────────────────────────────
           MOVE EIBTRNID TO WS-TRAN.
           MOVE EIBTRMID TO WS-TERM.
           MOVE EIBTASKN TO WS-TRACE-ID.
           MOVE EIBDATE  TO WS-DATE.
           MOVE EIBTIME  TO WS-TIME.
           MOVE 'CICS '  TO WS-ENV.
```

**Après exécution de la transaction :**
- Identité complète de l'exécution
- Traçabilité
- Valeurs par défaut cohérentes
- Aucun champ résiduel mémoire
- Compatibilité IMS / CICS / Batch

### Couche User Interface (Présentation)

#### Choix de la technologie

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    TECHNOLOGIES DE PRÉSENTATION                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. INTERFACES TERMINAUX 3270                                            │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • CICS + BMS (Basic Mapping Support)                            │   │
│  │  • Cartes d'écran (MAP) pour interactions utilisateurs           │   │
│  │  • Interface traditionnelle mainframe                            │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. INTERFACES WEB MODERNES                                              │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Frameworks : React, Angular, Vue.js                           │   │
│  │  • Communication via REST ou Web Services                        │   │
│  │  • IBM z/OS Connect pour exposer les transactions                │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  3. APPLICATIONS DESKTOP                                                 │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • JavaFX, .NET pour interfaces graphiques                       │   │
│  │  • Applications locales                                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  4. APPLICATIONS HYBRIDES                                                │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Combinaison terminaux + composants modernes                   │   │
│  │  • Migration progressive                                         │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Flux de travail entre les couches

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    FLUX MULTICOUCHES                                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────┐                                                    │
│  │   UTILISATEUR   │  Interaction avec l'UI (Web ou Terminal)          │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │  PRÉSENTATION   │  Collecte des données                              │
│  │                 │  Envoi requête (API ou MQ)                         │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │  LOGIQUE MÉTIER │  Programme COBOL traite la requête                │
│  │     (CICS)      │  Opérations sur la base de données                │
│  └────────┬────────┘                                                    │
│           │                                                              │
│           ▼                                                              │
│  ┌─────────────────┐                                                    │
│  │  PRÉSENTATION   │  Réponse traduite en format UI                    │
│  │                 │  Affichée à l'utilisateur                          │
│  └─────────────────┘                                                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## IX-2 Modèle d'application Transactionnelle

### Application Transactionnelle WEB

#### Contexte

Pour créer une interface Web avec un Framework JavaScript qui interagit avec CICS et VSAM :

| Composant | Rôle |
|-----------|------|
| **CICS** | Gestionnaire de transactions mainframe |
| **VSAM** | Gestionnaire de fichiers pour stocker les données |
| **API REST** | Interface entre le Front-end et le mainframe |

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE WEB - CICS                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                      FRONTEND WEB                                │   │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐                          │   │
│  │  │  React  │  │ Angular │  │  Vue.js │                          │   │
│  │  └────┬────┘  └────┬────┘  └────┬────┘                          │   │
│  │       └────────────┼────────────┘                                │   │
│  │                    │                                              │   │
│  └────────────────────┼─────────────────────────────────────────────┘   │
│                       │ HTTP/REST                                        │
│                       ▼                                                  │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    BACKEND (Node.js / Java)                      │   │
│  │                                                                  │   │
│  │  • API REST exposant les opérations CICS                        │   │
│  │  • Intermédiaire entre Frontend et Mainframe                    │   │
│  │                                                                  │   │
│  └────────────────────┬─────────────────────────────────────────────┘   │
│                       │ CICS Transaction Gateway                         │
│                       ▼                                                  │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    MAINFRAME (CICS + VSAM)                       │   │
│  │                                                                  │   │
│  │  ┌─────────────┐        ┌─────────────┐                         │   │
│  │  │    CICS     │◄──────►│    VSAM     │                         │   │
│  │  │ Transactions│        │   Fichiers  │                         │   │
│  │  └─────────────┘        └─────────────┘                         │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Outils Front-end Web

| Outil | Description |
|-------|-------------|
| **React** | Bibliothèque JavaScript de Facebook pour créer des UI dynamiques et interactives |
| **Angular** | Framework Google en TypeScript pour applications Web dynamiques |
| **Vue.js** | Framework JavaScript open-source simple et flexible pour SPA |
| **RESTful** | Style d'architecture pour services Web suivant les principes REST |

### Application Transactionnelle MQ

IBM MQ permet la communication asynchrone entre la couche Présentation et la couche Métier COBOL.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE IBM MQ                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  COUCHE PRÉSENTATION                    COUCHE MÉTIER COBOL             │
│  ───────────────────                    ─────────────────────           │
│                                                                          │
│  ┌─────────────────┐                    ┌─────────────────┐             │
│  │  Application    │                    │  Programme      │             │
│  │  Frontend       │                    │  COBOL/CICS     │             │
│  └────────┬────────┘                    └────────┬────────┘             │
│           │                                      │                       │
│           │ MQPUT                                │ MQGET                 │
│           ▼                                      ▼                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                         IBM MQ                                   │   │
│  │  ┌─────────────────┐              ┌─────────────────┐           │   │
│  │  │ Queue Requêtes  │              │ Queue Réponses  │           │   │
│  │  │ (OUTBOUND)      │              │ (INBOUND)       │           │   │
│  │  └─────────────────┘              └─────────────────┘           │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  AVANTAGES :                                                             │
│  • Communication asynchrone fiable                                       │
│  • Découplage des systèmes                                              │
│  • Gestion des transactions                                              │
│  • Persistance des messages                                              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Intégration COBOL avec IBM MQ

| Commande MQ | Fonction |
|-------------|----------|
| `MQPUT` | Envoyer un message dans une queue |
| `MQGET` | Récupérer un message d'une queue |
| `MQOPEN` | Ouvrir une connexion à une queue |
| `MQCLOSE` | Fermer une connexion |

#### Alternatives à IBM MQ

| Solution | Caractéristiques |
|----------|------------------|
| **RabbitMQ** | Open-source, environnements distribués |
| **Apache Kafka** | Traitement de flux de données à grande échelle |
| **ActiveMQ** | Open-source, gestion de files de messages |

### Architecture Multi-Frontend

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE MULTI-FRONTEND                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  UTILISATEURS                                                            │
│  ────────────                                                            │
│                                                                          │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐                            │
│  │ Terminal │   │   Web    │   │  Mobile  │                            │
│  │   3270   │   │ Browser  │   │   App    │                            │
│  └────┬─────┘   └────┬─────┘   └────┬─────┘                            │
│       │              │              │                                    │
│       │ TN3270       │ HTTP         │ HTTP                              │
│       │              │              │                                    │
│       ▼              ▼              ▼                                    │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    MIDDLEWARE / API GATEWAY                      │   │
│  │  • z/OS Connect (REST API)                                       │   │
│  │  • CICS Transaction Gateway                                      │   │
│  │  • IBM MQ (messaging)                                            │   │
│  └────────────────────────────┬─────────────────────────────────────┘   │
│                               │                                          │
│                               ▼                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    SERVEUR CICS                                  │   │
│  │                                                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │              PROGRAMMES COBOL                            │   │   │
│  │  │                                                          │   │   │
│  │  │  • Logique métier partagée                               │   │   │
│  │  │  • Transactions réutilisables                            │   │   │
│  │  │  • Accès aux données                                     │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                               │                                  │   │
│  │                               ▼                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │                    FICHIERS VSAM                         │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## IX-3 Conditions exceptionnelles

### Traitement des exceptions

Une situation anormale lors de l'exécution d'une commande CICS est identifiée par une **condition d'exception**. Chaque commande CICS possède son propre ensemble de conditions possibles.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    GESTION DES CONDITIONS                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Lorsqu'une condition est déclenchée :                                   │
│                                                                          │
│  1. LAISSER CONTINUER                                                    │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │  • Option RESP sur la commande                               │    │
│     │  • Option NOHANDLE sur la commande                           │    │
│     │  • IGNORE CONDITION                                          │    │
│     └─────────────────────────────────────────────────────────────┘    │
│                                                                          │
│  2. TRANSFÉRER LE CONTRÔLE                                               │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │  • HANDLE ABEND (label)                                      │    │
│     │  • HANDLE CONDITION condition-name (label)                   │    │
│     └─────────────────────────────────────────────────────────────┘    │
│                                                                          │
│  3. ACTION PAR DÉFAUT CICS                                               │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │  • Ne rien faire = ABEND de la tâche                         │    │
│     └─────────────────────────────────────────────────────────────┘    │
│                                                                          │
│  4. MÉLANGER LES MÉTHODES selon les besoins                              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Commandes de gestion des erreurs

| Commande | Description |
|----------|-------------|
| **RESP** | Code de réponse principal défini par CICS |
| **HANDLE CONDITION** | Transfert du contrôle vers un paragraphe |
| **IGNORE CONDITION** | Aucune action si condition se produit |
| **HANDLE ABEND** | Gestion des fins anormales |
| **ABEND** | Terminer intentionnellement la tâche |
| **NOHANDLE** | Désactiver la gestion pour une commande |

### Option RESP

L'option RESP peut être spécifiée dans n'importe quelle commande CICS. CICS fournit la fonction **DFHRESP** pour tester symboliquement les valeurs.

#### Tableau des conditions RESP

| Condition | Valeur | Description |
|-----------|--------|-------------|
| `NORMAL` | 00 | Exécution normale |
| `NOTFND` | 13 | Enregistrement non trouvé |
| `DUPREC` | 14 | Enregistrement en double |
| `INVREQ` | 16 | Requête invalide |
| `NOSPACE` | 18 | Plus d'espace disponible |
| `NOTOPEN` | 19 | Fichier fermé ou désactivé |
| `ENDFILE` | 20 | Fin de fichier détectée |
| `LENGERR` | 22 | Longueur incorrecte |
| `QZERO` | 23 | Queue vide ou fin de queue |
| `QBUSY` | 25 | Queue intra-partition occupée |
| `ITEMERR` | 26 | ITEM hors plage |
| `PGMIDERR` | 27 | Programme désactivé ou inactif |
| `EXPIRED` | 31 | Temps spécifié expiré |
| `MAPFAIL` | 36 | Erreur RECEIVE MAP |

#### Utilisation de l'option RESP

```cobol
       WORKING-STORAGE SECTION.
       77  WS-RESPCODE           PIC S9(8) COMP.
       . . . .
       PROCEDURE DIVISION.
       . . .
           EXEC CICS SEND
               FROM(WS-DATA)
               LENGTH(WS-LENGTH)
               ERASE
               RESP(WS-RESPCODE)
           END-EXEC.

           EVALUATE TRUE
               WHEN WS-RESPCODE = DFHRESP(NORMAL)
                   GO TO NORMAL-PARA
               WHEN WS-RESPCODE = DFHRESP(LENGERR)
                   GO TO LENGERR-PARA
           END-EVALUATE.
```

### HANDLE CONDITION

Transfert du contrôle vers une procédure sur les exceptions attendues.

> **Note** : Maximum 12 conditions peuvent être codées dans une instruction HANDLE CONDITION.

```cobol
           EXEC CICS HANDLE CONDITION
               MAPFAIL(PARA-1)
               PGMIDERR(PARA-2)
               LENGERR(PARA-3)
               ERROR(PARA-X)
           END-EXEC.
```

- Si MAPFAIL → exécuter PARA-1
- Si PGMIDERR → exécuter PARA-2
- Si LENGERR → exécuter PARA-3
- Toute autre erreur → PARA-X

**HANDLE CONDITION reste active jusqu'à :**
- Une condition IGNORE
- Une autre HANDLE CONDITION pour la même erreur
- Désactivation par NOHANDLE

### IGNORE CONDITION

N'entraîne aucune action si la condition spécifiée se produit.

```cobol
           EXEC CICS IGNORE CONDITION
               LENGERR
           END-EXEC.

           EXEC CICS RECEIVE
               INTO(WS-INPUT)
               LENGTH(WS-LENGTH)
           END-EXEC.
```

Si LENGERR se produit lors du RECEIVE, la condition est ignorée et le contrôle passe à l'instruction suivante.

### HANDLE ABEND

Gère les fins anormales dans le programme (S0C4, S0C7, etc.).

```cobol
           EXEC CICS HANDLE ABEND
               PROGRAM(name) | LABEL(ABEND-ROUTINE) | RESET | CANCEL
           END-EXEC.
```

| Option | Description |
|--------|-------------|
| **PROGRAM** | Passer le contrôle à un programme |
| **LABEL** | Passer le contrôle à un paragraphe |
| **CANCEL** | Annuler la demande HANDLE ABEND précédente |
| **RESET** | Réactiver la demande précédemment annulée |

#### Exemple HANDLE ABEND

```cobol
       WORKING-STORAGE SECTION.
       77  MSG-LEN              PIC S9(4) COMP.
       01  MSG-DATA.
           05  MSG-DATA1        PIC X(15).
           05  MSG-DATA2        PIC X(50).
       :
       PROCEDURE DIVISION.
       :
           EXEC CICS HANDLE ABEND
               LABEL(ABEND-ROUTINE)
           END-EXEC.
           :
       ABEND-ROUTINE.
           MOVE 'ABEND OCCURED.' TO MSG-DATA1.
           MOVE 'TASK CANCELLED WITH ABCODE 9999.' TO MSG-DATA2.
           MOVE 65 TO MSG-LEN.

           EXEC CICS SEND
               FROM(MSG-DATA)
               LENGTH(MSG-LEN)
               NOHANDLE
           END-EXEC.

           EXEC CICS HANDLE ABEND
               CANCEL
           END-EXEC.

           EXEC CICS ABEND
               ABCODE('9999')
           END-EXEC.
```

### Commande ABEND

Termine une tâche de manière anormale.

```cobol
           EXEC CICS ABEND
               ABCODE(name) | CANCEL | NODUMP
           END-EXEC.
```

| Option | Description |
|--------|-------------|
| **ABCODE(name)** | Code de 4 caractères (ne pas commencer par 'A') |
| **CANCEL** | Ignorer tous les exits HANDLE ABEND |
| **NODUMP** | Fin anormale sans dump |

### Option NOHANDLE

Désactive temporairement toutes les conditions HANDLE pour une commande spécifique.

```cobol
           EXEC CICS RECEIVE
               INTO(WS-INPUT)
               LENGTH(WS-LENGTH)
               NOHANDLE
           END-EXEC.
```

> **Note** : L'utilisation de l'option RESP implique NOHANDLE.

---

## IX-4 Gestion des Terminaux

### Communication programme/terminal

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    MODES DE CONVERSATION                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. CONVERSATIONNEL                                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  1. Système envoie message à l'écran                             │   │
│  │  2. Attend la réponse de l'utilisateur (THINK TIME)              │   │
│  │  3. Reçoit la réponse                                            │   │
│  │  4. Traite les données reçues                                    │   │
│  │                                                                  │   │
│  │  ⚠ Programme reste en mémoire pendant l'attente                 │   │
│  │  ⚠ Utilisation limitée en pratique                              │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. PSEUDO-CONVERSATIONNEL (Terminer et Relancer)                        │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  1. Système envoie message au terminal                           │   │
│  │  2. Termine la transaction en spécifiant la suivante             │   │
│  │  3. Ressources libérées pour autres transactions                 │   │
│  │  4. Interrogation à intervalle régulier                          │   │
│  │  5. Réception des données → démarrage transaction suivante       │   │
│  │                                                                  │   │
│  │  ✓ Programme libéré de la mémoire pendant l'attente             │   │
│  │  ✓ Mode recommandé pour les applications                        │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### BMS - Basic Mapping Support

Interface de programmation entre les terminaux et les programmes CICS. BMS fournit trois macro-instructions en langage assembleur.

#### Convention de codage BMS

| Colonne | Contenu |
|---------|---------|
| 1-8 | Label |
| 10-15 | Macro-name |
| 16-71 | Operands |
| 72 | Continuation |

#### MAP Physique vs Symbolique

| Type | Description |
|------|-------------|
| **MAP Physique** | Contrôle l'alignement écran, envoi/réception des données. Assemblé dans CICS LOAD LIBRARY. |
| **MAP Symbolique** | Définit les champs utilisés pour stocker les données VARIABLE. Copié dans les programmes via COPY. |

### DFHMSD - Définition du MAPSET

Le MAPSET est une collection de MAPs liées entre elles formant un module de chargement.

#### Opérandes DFHMSD

| Opérande | Description |
|----------|-------------|
| **TYPE** | `MAP` = physique, `DSECT` = symbolique, `&&SYSPARM` = les deux, `FINAL` = fin |
| **MODE** | `IN` = entrée, `OUT` = sortie, `INOUT` = entrée-sortie |
| **LANG** | Langage : COBOL, PLI, ASM, RPG |
| **STORAGE** | `AUTO` = zone distincte, `MAP-IOAREA` = partage |
| **TIOAPFX** | `YES` pour réserver 12 octets (requis niveau commande) |
| **CTRL** | `FREEKB`, `FRSET`, `ALARM`, `PRINT` |
| **TERM** | Si autre que terminal 3270 |

### DFHMDI - Définition de la MAP

Définit une MAP avec ses caractéristiques dans un MAPSET.

| Opérande | Description |
|----------|-------------|
| **SIZE** | (ligne, colonne) - taille de la MAP |
| **JUSTIFY** | BOTTOM, RIGHT, LEFT |
| **CTRL** | Identique à DFHMSD |
| **TIOAPFX** | Doit être 'YES' |
| **DATA** | FIELD / BLOCK |
| **HILIGHT** | OFF/BLINK/REVERSE/UNDERLINE |

### DFHMDF - Définition des champs

#### Opérandes principaux

| Opérande | Description |
|----------|-------------|
| **LENGTH** | Longueur du champ (max 256) |
| **JUSTIFY** | LEFT/RIGHT, BLANK/ZERO |
| **PICIN** | Format d'entrée |
| **PICOUT** | Format de sortie |
| **POS** | Position (ligne, colonne) |
| **HILIGHT** | OFF/BLINK/REVERSE/UNDERLINE |
| **ATTRB** | Attributs du champ |

#### Attribut ATTRB

```
ATTRB=(ASKIP|PROT|UNPROT,{NUM},BRT|NORM|DRK,DET|IC|FSET)
```

| Attribut | Description |
|----------|-------------|
| **UNPROT** | Champ saisissable |
| **NUM** | Numérique uniquement |
| **PROT** | Champ protégé |
| **ASKIP** | Saut automatique (défaut) |
| **DRK** | Champ invisible |
| **NORM** | Intensité normale |
| **BRT** | Intensité brillante |
| **IC** | Position initiale du curseur |
| **DET** | Champ détectable |
| **FSET** | MDT activé |

#### MDT (Modified Data Tag)

- Bit dans l'octet d'attribut (8ème position)
- **ON (1)** : champ modifié, sera transmis sur RECEIVE
- **OFF (0)** : champ non modifié
- Si l'utilisateur modifie le champ, MDT passe automatiquement à ON

### RECEIVE MAP

Extrait les données d'entrée d'un terminal.

```cobol
           EXEC CICS RECEIVE
               MAP('map-name')
               MAPSET('mapset-name')
               INTO(data-area)
               LENGTH(data-value)
           END-EXEC.
```

| Option | Description |
|--------|-------------|
| **MAP** | Nom de la MAP (7 caractères) |
| **MAPSET** | Nom du MAPSET (7 caractères) |
| **INTO** | Zone de données destination (défaut: nom MAP + 'I') |
| **FROM** | Zone de données à mapper |
| **LENGTH** | Longueur des données (demi-mot binaire) |

### SEND MAP

Envoie les données formatées à un terminal.

```cobol
           EXEC CICS SEND
               MAP('map-name')
               MAPSET('mapset-name')
               FROM(data-area)
               DATAONLY | MAPONLY
               FREEKB
               FRSET
               ERASE | ERASEAUP
               CURSOR(data-value)
           END-EXEC.
```

| Option | Description |
|--------|-------------|
| **DATAONLY** | Données application uniquement |
| **MAPONLY** | Données MAP par défaut uniquement |
| **FREEKB** | Déverrouiller le clavier |
| **FRSET** | Désactiver MDT pour tous les champs |
| **ERASE** | Effacer l'écran |
| **ERASEAUP** | Effacer les champs non protégés |
| **CURSOR** | Positionner le curseur |

### EIBAID - Touche de fonction

L'EIB (Exec Interface Block) contient des informations sur chaque opération d'entrée.

#### Copybook DFHAID

```cobol
       01  DFHAID.
           02  DFHNULL           PIC X VALUE IS ' '.
           02  DFHENTER          PIC X VALUE IS ''''.
           02  DFHCLEAR          PIC X VALUE IS '_'.
           02  DFHPA1            PIC X VALUE IS '%'.
           02  DFHPA2            PIC X VALUE IS '>'.
           02  DFHPA3            PIC X VALUE IS ','.
           02  DFHPF1            PIC X VALUE IS '1'.
           02  DFHPF2            PIC X VALUE IS '2'.
           02  DFHPF3            PIC X VALUE IS '3'.
           02  DFHPF4            PIC X VALUE IS '4'.
           02  DFHPF5            PIC X VALUE IS '5'.
           02  DFHPF6            PIC X VALUE IS '6'.
           02  DFHPF7            PIC X VALUE IS '7'.
           02  DFHPF8            PIC X VALUE IS '8'.
           02  DFHPF9            PIC X VALUE IS '9'.
           02  DFHPF10           PIC X VALUE IS ':'.
           02  DFHPF11           PIC X VALUE IS '#'.
           02  DFHPF12           PIC X VALUE IS '@'.
           ...
```

#### Vérification en COBOL

```cobol
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM PARA-1
               WHEN DFHPF1
                   PERFORM PARA-2
               WHEN OTHER
                   PERFORM PARA-3
           END-EVALUATE.
```

#### Vérification par CICS (HANDLE AID)

```cobol
           EXEC CICS HANDLE AID
               DFHENTER(PARA-1)
               DFHPF1(PARA-2)
               ANYKEY(PARA-3)
           END-EXEC.
```

---

## IX-5 Définition des ressources

### Transaction CEMT

CEMT (Master Terminal) permet d'effectuer des opérations de gestion sur les ressources CICS.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COMMANDE CEMT                                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  CEMT I[NQ] S[ET] P[ERFORM]                                              │
│                                                                          │
│  FONCTIONS :                                                             │
│  • Affichage de l'état des tâches                                       │
│  • Modification des classes de transaction                              │
│  • Chargement de programmes, MAPSETs, bibliothèques                     │
│  • Purge de tâches                                                       │
│  • Mise en/hors service des terminaux                                   │
│  • Changement d'état de transactions                                     │
│  • Arrêt de CICS (SHUTDOWN)                                             │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### CEMT INQ (Inquiry)

| Commande | Description |
|----------|-------------|
| `CEMT I TASK ALL` | Tous les terminaux et processus connectés |
| `CEMT I TRANSID Transid` | Tâches exécutant une transaction spécifique |
| `CEMT I TASK ACTIVE` | Toutes les tâches actives |
| `CEMT I TASK SUSPENDED` | Toutes les tâches suspendues |
| `CEMT I TASK TERM` | Tâches initiées par terminal |
| `CEMT I TDQUEUE ALL` | Toutes les files de données transitoires |

#### CEMT SET

| Commande | Description |
|----------|-------------|
| `CEMT SET PROGRAM nom NEWCOPY` | Charger nouvelle copie du programme |
| `CEMT SET PROGRAM nom DISABLED/ENABLED` | Activer/désactiver un programme |
| `CEMT SET TRANSACTION id DISABLED/ENABLED` | Activer/désactiver une transaction |
| `CEMT SET TASK num PURGE` | Terminer une tâche |
| `CEMT SET TERMINAL trmid INSERVICE` | Enregistrer le terminal |
| `CEMT SET TERMINAL trmid OUTSERVICE` | Déconnecter le terminal |

#### CEMT PERFORM

| Commande | Description |
|----------|-------------|
| `CEMT P SHUTDOWN` | Ordre d'arrêt de CICS |
| `CEMT P SHUTDOWN IMMEDIATE` | Arrêt immédiat |
| `CEMT P SHUTDOWN REBUILD` | Effacer le cache mémoire |

### Transaction CEDA

CEDA permet de définir et gérer les ressources sur le CSD (CICS System Definition).

| Commande | Description |
|----------|-------------|
| `CEDA ADD` | Ajouter un groupe à une liste |
| `CEDA ALTER` | Modifier les attributs d'une définition |
| `CEDA APPEND` | Ajouter des groupes à une liste |
| `CEDA CHECK` | Vérifier la cohérence des définitions |
| `CEDA COPY` | Copier une définition |
| `CEDA DEFINE` | Créer de nouvelles définitions |
| `CEDA VIEW` | Afficher les attributs |
| `CEDA DELETE` | Supprimer une définition |
| `CEDA DISPLAY` | Afficher les noms de groupe/liste |
| `CEDA EXPAND` | Afficher les définitions d'un groupe |
| `CEDA INSTALL` | Rendre disponible pour CICS actif |
| `CEDA LOCK` | Limiter l'accès de mise à jour |
| `CEDA MOVE` | Déplacer des définitions |
| `CEDA REMOVE` | Supprimer un groupe d'une liste |
| `CEDA RENAME` | Modifier le nom d'une ressource |
| `CEDA UNLOCK` | Supprimer le verrou |

### Transaction CECI

CECI (Command Level Interpreter) permet de vérifier la syntaxe et exécuter des commandes CICS interactivement.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    UTILISATION DE CECI                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  FONCTIONS :                                                             │
│  • Exécution de commandes et affichage des résultats                    │
│  • Création/suppression de données de test                              │
│  • Introduction de données erronées pour tester la logique              │
│  • Réparation d'enregistrements corrompus                               │
│                                                                          │
│  EXEMPLE :                                                               │
│  SEND MAP('MAP1') MAPSET('MAPTEST') ERASE                               │
│                                                                          │
│  ÉTAPES D'EXÉCUTION :                                                   │
│  1. Saisir une commande et appuyer sur ENTER                            │
│  2. Taper options et arguments                                           │
│  3. PF5 : Afficher l'écran Variables                                     │
│  4. PF2 : Basculer hexadécimal/caractères                                │
│  5. ENTER : Transmettre au traducteur                                    │
│  6. PF9 : Afficher les messages générés                                  │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Types de variables CECI

| Type | Longueur | Abréviation | Exemple |
|------|----------|-------------|---------|
| Halfword | 2 | H | 01234 |
| Fullword | 4 | F | 0123456789 |
| Packed Decimal | 4 | P | 4000010C |
| Doubleword | 8 | D | 00ABCDE123456789 |
| Buffer | Max 32767 | Numeric length | - |

### Transaction CEDF

CEDF (CICS Execution Diagnostic Facility) est l'outil de débogage interactif.

| Transaction | Description |
|-------------|-------------|
| **CEDF** | Tester les programmes associés aux transactions utilisateur |
| **CEDX** | Surveiller et déboguer les transactions non-terminales |

**Syntaxe :**
```
CEDF {termId|sysID} [,ON|,OFF]
```

#### Statuts de l'écran CEDF

| Status | Description |
|--------|-------------|
| PROGRAM INITIATION | Démarrage du programme |
| ABOUT TO EXECUTE COMMAND | Avant exécution d'une commande |
| COMMAND EXECUTE COMPLETE | Commande exécutée |
| PROGRAM TERMINATION | Fin du programme |
| TASK TERMINATION | Fin de la tâche |
| AN ABEND HAS OCCURRED | Une erreur s'est produite |
| ABNORMAL TASK TERMINATION | Fin anormale de la tâche |
| EXECUTION INTERFACE BLOCK | Affichage du bloc EIB |
| DISPLAY ON CONDITION | Affichage sur condition |

#### Touches PF dans CEDF

| Touche | Fonction |
|--------|----------|
| **PF1** | Affiche l'aide |
| **PF2** | Bascule alphanumérique/hexadécimal |
| **PF3** | Fin de session EDF |
| **PF4** | Affiche les champs EIB |
| **PF5** | Affiche le storage du programme |
| **PF6** | Passage affichage CEDF/application |
| **PF7/PF8** | Défilement du stockage |
| **PF9** | Détermine les conditions d'interruption |
| **PF10** | Défilement plein écran arrière |
| **PF11** | Défilement commande/EIB avant |
| **PF12** | Termine la tâche utilisateur |

---

## IX-6 Étude de cas

### Installation d'une Transaction TRNM

La transaction TRNM affiche une MAP contenant une variable à saisir, puis réaffiche cette variable dans une nouvelle position.

#### Étapes d'installation

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    INSTALLATION D'UNE APPLICATION                        │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. DÉFINIR et INSTALLER la MAP                                         │
│     CEDA DEF MAP(MAPTEST) GROUP(GRPTEST)                                │
│     CEDA INS MAP(MAPTEST) GROUP(GRPTEST)                                │
│                                                                          │
│  2. DÉFINIR et INSTALLER la TRANSACTION                                  │
│     CEDA DEF TRANS(TRNM) GROUP(GRPTEST) PROGRAM(PROGTEST)               │
│     CEDA INS TRANS(TRNM) GROUP(GRPTEST)                                 │
│                                                                          │
│  3. DÉFINIR et INSTALLER le PROGRAMME                                    │
│     CEDA DEF PROG(PROGTEST) GROUP(GRPTEST) LANGUAGE(COBOL)              │
│     CEDA INS PROG(PROGTEST) GROUP(GRPTEST)                              │
│                                                                          │
│  4. VISUALISER le groupe                                                 │
│     CEDA DISPLAY GROUP(GRPTEST)                                         │
│     (Taper 'V' devant chaque objet)                                     │
│                                                                          │
│  5. AJOUTER le groupe à une liste (optionnel)                           │
│     CEDA ADD GROUP(GRPTEST) LIST(XYZLIST)                               │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

> **Note** : La commande `CEDA ADD GROUP(...) LIST(...)` est réservée aux administrateurs CICS en production.

#### Débogage et validation

1. Utiliser CEDF pour déboguer la transaction TRNM
2. Vérifier les valeurs EIB et Storage à chaque étape
3. Utiliser CECI pour valider les commandes SEND ou RECEIVE

---

## IX-7 Échanges de données

### Traitement I/O à travers la MAP

Les opérations Input/Output se caractérisent par un échange de données traité par le programme COBOL.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ÉCHANGES DE DONNÉES VIA MAP                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ÉLÉMENTS NÉCESSAIRES :                                                  │
│  1. Le programme COBOL                                                   │
│  2. La MAP symbolique (variables à manipuler)                           │
│  3. La définition de la transaction                                      │
│  4. CEDF pour le débogage                                               │
│                                                                          │
│  INSTRUCTIONS CLÉS :                                                     │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  SEND   : Envoi de la MAP et des valeurs vers l'écran          │   │
│  │  RECEIVE: Stockage des données saisies depuis l'écran          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  CONVENTIONS DE SUFFIXES :                                               │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  'I' : Variables INPUT (données entrantes)                      │   │
│  │  'O' : Variables OUTPUT (données sortantes)                     │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Décomposition d'une MAPSET

CICS permet de définir jusqu'à **1023 MAP par MAPSET** pour mieux organiser l'emplacement du curseur.

**Exemple de décomposition :**
- MAP1 : réservée pour la variable « SIMPLE »
- MAP2 : réservée pour la variable « CHPOUT »
- MAP3 : réservée pour la variable « CHPIO »

### Exercice pratique

1. Créer une nouvelle MAPSET composée de deux MAP (MAPIO1)
2. Adapter le programme COBOL à la nouvelle MAP (PROGIO1)
3. Définir une nouvelle transaction TIO1
4. Modifier les caractéristiques des MAP (OUTLINE, COLOR)
5. Modifier les valeurs des variables VAR1 et VAR2
6. Mettre à jour avec `CEDA INSTALL`
7. Exécuter la transaction TIO1
8. Exécuter les commandes :
   ```
   CEDA REM GROUP(GRPTEST) LIST(XYZLIST)
   CEDA ADD GROUP(GRPTEST) LIST(XYZLIST)
   ```
9. Réexécuter la transaction TIO1

---

## IX-8 Les E/S sous CICS

### Présentation

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    E/S SOUS CICS - CARACTÉRISTIQUES                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  a) CICS exécute la majorité des instructions E/S définies en COBOL    │
│                                                                          │
│  b) Instructions précédées de : EXEC CICS instruction E/S               │
│                                                                          │
│  c) Pas de paragraphe ENVIRONMENT DIVISION (contrairement au batch)    │
│                                                                          │
│  d) Pas d'instructions OPEN et CLOSE pour les Data Set                  │
│     → Ouverture/Fermeture gérées par CICS                               │
│                                                                          │
│  e) Déclaration des Data Set au niveau du JCL CICS                      │
│                                                                          │
│  f) Library des programmes déclarée dans le JCL CICS                    │
│                                                                          │
│  g) Mise à jour des JCL systèmes par l'administrateur                   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Commande READ

Lit un enregistrement à partir d'un fichier en utilisant la clé primaire.

```cobol
           EXEC CICS READ
               FILE('ddname-file')
               INTO(zone-donnée)
               RIDFLD(zone-donnée)
               LENGTH(valeur-donnée)
               KEYLENGTH(valeur-donnée)
               UPDATE
               EQUAL | GTEQ
               RBA | RRN
           END-EXEC.
```

| Option | Description |
|--------|-------------|
| **FILE** | Nom du fichier |
| **INTO** | Zone destination de l'enregistrement |
| **LENGTH** | Nombre max de caractères (S9(4) COMP) |
| **RIDFLD** | Zone contenant la clé |
| **KEYLENGTH** | Longueur de la clé |
| **GENERIC** | Clés partielles |
| **UPDATE** | Obtenir l'enregistrement pour mise à jour |
| **EQUAL** | Clé exacte uniquement |
| **GTEQ** | Clé supérieure ou égale |
| **RBA** | Fichier ESDS |
| **RRN** | Fichier RRDS |

#### Conditions d'erreur READ

| Code | Condition | Code | Condition |
|------|-----------|------|-----------|
| 12 | FILENOTFOUND | 21 | ILLOGIC |
| 13 | NOTFND | 22 | LENGERR |
| 15 | DUPKEY | 70 | NOTAUTH |
| 16 | INVREQ | 84 | DISABLED |
| 17 | IOERR | 100 | LOCKED |
| 19 | NOTOPEN | 101 | RECORDBUSY |

### Commande WRITE

Écrit un nouvel enregistrement dans le Data Set.

```cobol
           EXEC CICS WRITE
               FILE('ddname-file')
               FROM(zone-donnée)
               RIDFLD(zone-donnée)
               LENGTH(valeur-donnée)
               KEYLENGTH(valeur-donnée)
               RBA | RRN
           END-EXEC.
```

| Condition | Code |
|-----------|------|
| FILENOTFOUND | 12 |
| DUPREC | 14 |
| INVREQ | 16 |
| IOERR | 17 |
| NOSPACE | 18 |
| NOTOPEN | 19 |
| LENGERR | 22 |
| NOTAUTH | 70 |
| DISABLED | 84 |
| LOCKED | 100 |

### Commande REWRITE

Met à jour un enregistrement existant. **Doit être précédée d'un READ UPDATE.**

```cobol
           EXEC CICS REWRITE
               FILE('ddname-file')
               FROM(zone-donnée)
               LENGTH(valeur-donnée)
           END-EXEC.
```

> **Important** : Le champ de la clé ne doit pas changer pour les Data Set VSAM.

### Commande DELETE

Supprime un ou plusieurs enregistrements d'un Data Set VSAM (KSDS et RRDS).

#### Après READ UPDATE

```cobol
           EXEC CICS DELETE
               FILE('ddname-file')
           END-EXEC.
```

#### Avec RIDFLD (sans READ préalable)

```cobol
           EXEC CICS DELETE
               FILE('ddname-file')
               RIDFLD(zone-donnée)
               KEYLENGTH(valeur-donnée)
               GENERIC
               NUMREC(numéro-record)
           END-EXEC.
```

### Commandes de navigation (Browse)

#### STARTBR - Démarrage

Démarre la navigation sans effectuer de lecture.

```cobol
           EXEC CICS STARTBR
               FILE('ddname-file')
               RIDFLD(zone-donnée)
               KEYLENGTH(valeur-donnée)
               REQID(valeur-donnée)
               GTEQ | EQUAL
               GENERIC
           END-EXEC.
```

**Types de navigation :**
- KSDS séquencé par clé
- ESDS par adresse RBA
- RRDS par numéro RRN
- Parcours par chemin d'index alternatif

#### RESETBR - Réinitialisation

Réinitialise la position de navigation (équivalent à ENDBR + STARTBR).

```cobol
           EXEC CICS RESETBR
               FILE('ddname-file')
               RIDFLD(zone-donnée)
               KEYLENGTH(valeur-donnée)
               EQUAL | GTEQ
               REQID(valeur-donnée)
               GENERIC
               RBA | RRN
           END-EXEC.
```

#### READNEXT - Lecture suivante

Lit les enregistrements en ordre séquentiel.

```cobol
           EXEC CICS READNEXT
               FILE('ddname-file')
               INTO(zone-donnée) | SET(pointeur-réf)
               RIDFLD(zone-donnée)
               KEYLENGTH(valeur-donnée)
               REQID(valeur-donnée)
               UPDATE
               RBA | RRN
           END-EXEC.
```

**Gestion des verrous avec UPDATE :**
- Si DELETE ou REWRITE exécutée → verrou actif jusqu'au SYNCPOINT
- Si READNEXT ou READPREV suivant → verrou terminé

#### READPREV - Lecture précédente

Récupère les enregistrements dans l'ordre inverse.

```cobol
           EXEC CICS READPREV
               FILE('ddname-file')
               INTO(zone-donnée) | SET(pointeur-réf)
               RIDFLD(zone-donnée)
               KEYLENGTH(valeur-donnée)
               REQID(valeur-donnée)
               UPDATE
               RBA | RRN
           END-EXEC.
```

> **Note** : READPREV suivi de READNEXT récupère le même enregistrement.

> **Astuce** : `KEYLENGTH(0)` place le curseur au début du fichier.

#### ENDBR - Fin de navigation

Termine la navigation dans le Data Set.

```cobol
           EXEC CICS ENDBR
               FILE('ddname-file')
               REQID(valeur-donnée)
           END-EXEC.
```

> **Important** : Si STARTBR n'a pas réussi, il n'est pas possible d'émettre ENDBR.

### Séquence de navigation complète

```
STARTBR → (READNEXT/READPREV)* → ENDBR
```

> **Important** : Update et Browse sont des fonctions mutuellement exclusives. Pour mettre à jour pendant le Browse :
> 1. Lancer ENDBR
> 2. Faire READ avec option UPDATE
> 3. REWRITE l'enregistrement
> 4. Lancer STARTBR et continuer la navigation

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE IX - RÉSUMÉ                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  IX-1 ARCHITECTURE MULTICOUCHES                                          │
│       • Accesseurs logiques vs physiques                                │
│       • Transactions TSI : traçabilité et contexte                      │
│       • Couche présentation : 3270, Web, Mobile                         │
│                                                                          │
│  IX-2 MODÈLE D'APPLICATION TRANSACTIONNELLE                              │
│       • Architecture Web (React/Angular/Vue.js + CICS)                  │
│       • IBM MQ pour communication asynchrone                            │
│       • Multi-Frontend : 3270 + Web + Mobile                            │
│                                                                          │
│  IX-3 CONDITIONS EXCEPTIONNELLES                                         │
│       • RESP : codes de réponse (DFHRESP)                               │
│       • HANDLE CONDITION : transfert sur exception                      │
│       • HANDLE ABEND : gestion des fins anormales                       │
│       • IGNORE CONDITION / NOHANDLE                                     │
│                                                                          │
│  IX-4 GESTION DES TERMINAUX                                              │
│       • Mode conversationnel vs pseudo-conversationnel                  │
│       • BMS : DFHMSD, DFHMDI, DFHMDF                                    │
│       • SEND MAP / RECEIVE MAP                                          │
│       • EIBAID et HANDLE AID                                            │
│                                                                          │
│  IX-5 DÉFINITION DES RESSOURCES                                          │
│       • CEMT : gestion du système                                       │
│       • CEDA : définition des ressources                                │
│       • CECI : interpréteur de commandes                                │
│       • CEDF : débogage interactif                                      │
│                                                                          │
│  IX-6 ÉTUDE DE CAS                                                       │
│       • Installation d'une transaction                                   │
│       • Séquence : MAP → TRANS → PROG → INSTALL                         │
│       • Débogage avec CEDF, validation avec CECI                        │
│                                                                          │
│  IX-7 ÉCHANGES DE DONNÉES                                                │
│       • Suffixes I/O pour les variables                                 │
│       • Décomposition des MAPSET (jusqu'à 1023 MAP)                     │
│       • Exercices pratiques                                              │
│                                                                          │
│  IX-8 LES E/S SOUS CICS                                                  │
│       • READ / WRITE / REWRITE / DELETE                                 │
│       • Navigation : STARTBR → READNEXT/READPREV → ENDBR                │
│       • RESETBR pour repositionnement                                   │
│       • Gestion des verrous (UPDATE)                                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Annexes

### Récapitulatif des commandes CICS

| Commande | Fonction |
|----------|----------|
| **READ** | Lire un enregistrement |
| **WRITE** | Écrire un nouvel enregistrement |
| **REWRITE** | Mettre à jour un enregistrement existant |
| **DELETE** | Supprimer un enregistrement |
| **STARTBR** | Démarrer une navigation |
| **READNEXT** | Lire l'enregistrement suivant |
| **READPREV** | Lire l'enregistrement précédent |
| **RESETBR** | Repositionner la navigation |
| **ENDBR** | Terminer une navigation |
| **SEND MAP** | Envoyer une MAP au terminal |
| **RECEIVE MAP** | Recevoir les données d'une MAP |
| **RETURN** | Retourner le contrôle à CICS |
| **HANDLE CONDITION** | Gérer les conditions d'exception |
| **HANDLE ABEND** | Gérer les fins anormales |
| **IGNORE CONDITION** | Ignorer une condition |
| **ABEND** | Terminer anormalement une tâche |

### Transactions utilitaires CICS

| Transaction | Description |
|-------------|-------------|
| **CEMT** | Master Terminal - Gestion du système |
| **CEDA** | Resource Definition Online |
| **CECI** | Command Level Interpreter |
| **CEDF** | Execution Diagnostic Facility |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VIII - Travaux Pratiques](08-travaux-pratiques.md) | - |

---
*Formation COBOL - Module CICS*
