# Chapitre I - Présentation générale de CICS

## I-1 Historique de CICS

### Contexte de création

Dans les années 1960, les entreprises avaient besoin d'accéder à leurs données en temps réel, et non plus uniquement via des traitements batch différés. IBM a développé CICS pour répondre à ce besoin.

### Chronologie

| Année | Événement |
|-------|-----------|
| **1968** | Première version de CICS (Public Utility Customer Information Control System) |
| **1970s** | Adoption massive dans le secteur bancaire et financier |
| **1974** | CICS/VS - Support de la mémoire virtuelle |
| **1980s** | Support de DB2, nouvelles fonctionnalités réseau |
| **1990** | CICS/ESA - Architecture Enterprise Systems |
| **1996** | CICS Transaction Server (CICS TS) V1.1 |
| **2000s** | Support Java, Web Services, connectivité Internet |
| **2010s** | CICS TS V5.x - Cloud, API REST, JSON |
| **2020s** | CICS TS V6.x - Conteneurs, DevOps, intégration moderne |

### Importance actuelle

Aujourd'hui encore, CICS traite la majorité des transactions financières mondiales :

- **30 milliards** de transactions par jour (estimation)
- Présent dans **90%** des entreprises du Fortune 500
- Plus de **50 ans** d'évolution continue

## I-2 Qu'est-ce que CICS ?

### Définition

**CICS** (Customer Information Control System) est un **moniteur transactionnel** (TP Monitor - Transaction Processing Monitor) qui permet :

- L'exécution de **transactions en temps réel**
- La gestion de **milliers d'utilisateurs simultanés**
- L'accès sécurisé aux **données et ressources**

```
┌─────────────────────────────────────────────────────────────────┐
│                          CICS                                    │
│                                                                  │
│   "Middleware" entre les utilisateurs et les ressources         │
│                                                                  │
│   ┌──────────────┐    ┌──────────────┐    ┌──────────────┐     │
│   │ Utilisateurs │───►│     CICS     │───►│  Ressources  │     │
│   │  (Terminaux) │    │  (Moniteur)  │    │   (Données)  │     │
│   └──────────────┘    └──────────────┘    └──────────────┘     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Notion de transaction

Une **transaction CICS** est :

- Une unité de travail **atomique** (tout ou rien)
- Identifiée par un **code de 4 caractères** (ex: `MENU`, `INQU`, `ACCT`)
- Associée à un ou plusieurs **programmes**

```
   Utilisateur tape "MENU" + Entrée
              │
              ▼
   ┌─────────────────────┐
   │  Transaction MENU   │
   │  ─────────────────  │
   │  Programme: MENUPGM │
   │  Action: Afficher   │
   │  le menu principal  │
   └─────────────────────┘
```

### Propriétés ACID

Chaque transaction CICS respecte les propriétés **ACID** :

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| **A**tomicité | Tout ou rien | Virement : débit ET crédit, ou aucun |
| **C**ohérence | État valide avant/après | Soldes toujours corrects |
| **I**solation | Invisible aux autres | Transaction en cours non visible |
| **D**urabilité | Modifications permanentes | Données sauvegardées même si panne |

## I-3 CICS sous z/OS

### Position dans l'architecture z/OS

```
┌─────────────────────────────────────────────────────────────────┐
│                      APPLICATIONS                                │
│         (Programmes COBOL, PL/I, Assembleur, Java)              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                          CICS TS                                 │
│    (CICS Transaction Server - Moniteur transactionnel)          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                           z/OS                                   │
│              (Système d'exploitation mainframe)                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    RESSOURCES DONNÉES                            │
│   ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐  │
│   │  VSAM  │  │  DB2   │  │  IMS   │  │   MQ   │  │  Files │  │
│   └────────┘  └────────┘  └────────┘  └────────┘  └────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Région CICS

Une **région CICS** est une instance du moniteur transactionnel en cours d'exécution :

- S'exécute comme un **job z/OS** (address space)
- Peut héberger **plusieurs transactions**
- Dispose de ses propres **ressources** (fichiers, programmes, queues)

```
┌─────────────────────────────────────────────────────────────────┐
│                        z/OS                                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │
│  │  Région     │  │  Région     │  │  Région     │             │
│  │  CICS       │  │  CICS       │  │  CICS       │             │
│  │  PROD       │  │  TEST       │  │  DEV        │             │
│  │             │  │             │  │             │             │
│  │ Trans: MENU │  │ Trans: MENU │  │ Trans: MENU │             │
│  │        INQU │  │        TEST │  │        DBG  │             │
│  └─────────────┘  └─────────────┘  └─────────────┘             │
└─────────────────────────────────────────────────────────────────┘
```

### Intégration avec les sous-systèmes z/OS

| Sous-système | Rôle avec CICS |
|--------------|----------------|
| **RACF** | Sécurité et contrôle d'accès |
| **DB2** | Accès aux bases de données relationnelles |
| **IMS** | Accès aux bases hiérarchiques |
| **MQ** | Messagerie asynchrone |
| **VTAM** | Gestion des communications réseau |

## I-4 Caractéristiques de CICS

### Haute disponibilité

```
┌─────────────────────────────────────────────────────────────────┐
│                    HAUTE DISPONIBILITÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  • Fonctionne 24h/24, 7j/7, 365 jours/an                        │
│  • Reprise automatique après incident (recovery)                 │
│  • Pas d'interruption de service pour maintenance               │
│  • Journalisation des transactions (logging)                     │
│  • Sauvegarde et restauration automatiques                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Performance

| Métrique | Valeur typique |
|----------|----------------|
| Transactions/seconde | Milliers à dizaines de milliers |
| Temps de réponse | Millisecondes |
| Utilisateurs simultanés | Des milliers |
| Disponibilité | 99.999% (5 nines) |

### Scalabilité

- **Horizontal** : Ajout de régions CICS (Sysplex)
- **Vertical** : Augmentation des ressources d'une région
- **Dynamique** : Ajustement sans redémarrage

### Sécurité

```
┌─────────────────────────────────────────────────────────────────┐
│                      SÉCURITÉ CICS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Authentification                                                │
│  ├── Connexion utilisateur (Sign-on)                            │
│  └── Intégration RACF/ACF2/Top Secret                           │
│                                                                  │
│  Autorisation                                                    │
│  ├── Contrôle d'accès aux transactions                          │
│  ├── Contrôle d'accès aux ressources                            │
│  └── Profils de sécurité                                        │
│                                                                  │
│  Audit                                                           │
│  ├── Journalisation des accès                                   │
│  └── Traçabilité des opérations                                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## I-5 Principales fonctions de CICS

### Vue d'ensemble des fonctions

```
┌─────────────────────────────────────────────────────────────────┐
│                    FONCTIONS CICS                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │
│  │   GESTION DES   │  │   GESTION DES   │  │   GESTION DES   │ │
│  │   TERMINAUX     │  │    DONNÉES      │  │     TÂCHES      │ │
│  │                 │  │                 │  │                 │ │
│  │ • Écrans 3270   │  │ • Fichiers VSAM │  │ • Scheduling    │ │
│  │ • Saisie/Affich │  │ • Bases DB2/IMS │  │ • Priorités     │ │
│  │ • BMS (maps)    │  │ • Queues TS/TD  │  │ • Multitâche    │ │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘ │
│                                                                  │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │
│  │   GESTION DES   │  │    GESTION      │  │   RÉCUPÉRATION  │ │
│  │   PROGRAMMES    │  │   MÉMOIRE       │  │   (RECOVERY)    │ │
│  │                 │  │                 │  │                 │ │
│  │ • Chargement    │  │ • Allocation    │  │ • Journaux      │ │
│  │ • LINK/XCTL     │  │ • GETMAIN       │  │ • Syncpoint     │ │
│  │ • RETURN        │  │ • FREEMAIN      │  │ • Rollback      │ │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Détail des fonctions principales

| Fonction | Description | Commandes associées |
|----------|-------------|---------------------|
| **Terminal Control** | Gestion des écrans et saisies | SEND, RECEIVE |
| **File Control** | Accès aux fichiers VSAM | READ, WRITE, DELETE |
| **Program Control** | Appels entre programmes | LINK, XCTL, RETURN |
| **Task Control** | Gestion des tâches | SUSPEND, WAIT |
| **Storage Control** | Allocation mémoire | GETMAIN, FREEMAIN |
| **Interval Control** | Gestion du temps | START, DELAY |
| **Temporary Storage** | Stockage temporaire | WRITEQ TS, READQ TS |
| **Transient Data** | Files d'attente | WRITEQ TD, READQ TD |

## I-6 Traitement BATCH vs ONLINE

### Comparaison

```
┌────────────────────────────────────────────────────────────────────┐
│              BATCH                    │          ONLINE (CICS)      │
├───────────────────────────────────────┼────────────────────────────┤
│                                       │                            │
│  ┌─────────┐      ┌─────────┐        │   ┌─────┐    ┌─────────┐  │
│  │ Fichier │ ───► │ Fichier │        │   │User │◄──►│  CICS   │  │
│  │ Entrée  │      │ Sortie  │        │   └─────┘    └────┬────┘  │
│  └─────────┘      └─────────┘        │                   │       │
│       │                ▲              │              ┌────▼────┐  │
│       └───► PROGRAMME ─┘              │              │ Données │  │
│                                       │              └─────────┘  │
│  • Exécution différée                 │  • Exécution immédiate    │
│  • Pas d'interaction                  │  • Conversationnel        │
│  • Gros volumes                       │  • Petites transactions   │
│                                       │                            │
└───────────────────────────────────────┴────────────────────────────┘
```

### Tableau comparatif détaillé

| Critère | Traitement BATCH | Traitement ONLINE (CICS) |
|---------|------------------|--------------------------|
| **Mode d'exécution** | Différé (planifié) | Temps réel (immédiat) |
| **Interaction** | Aucune | Conversationnelle |
| **Déclenchement** | Scheduler (JES) | Utilisateur / Événement |
| **Temps de réponse** | Minutes à heures | Millisecondes |
| **Volume par traitement** | Très élevé (millions) | Faible (1 transaction) |
| **Nombre de traitements** | Peu par jour | Des milliers par seconde |
| **Ressources** | Exclusives pendant exécution | Partagées |
| **Exemple** | Édition des relevés mensuels | Consultation de solde |
| **Horaire** | Nuit / Week-end | 24h/24 |

### Exemples concrets

**BATCH :**
```
• Calcul des intérêts mensuels pour tous les comptes
• Génération des relevés bancaires
• Mise à jour massive des tarifs
• Archivage des anciennes transactions
```

**ONLINE (CICS) :**
```
• Consultation du solde d'un compte
• Virement entre deux comptes
• Retrait au distributeur
• Réservation d'un billet d'avion
```

## I-7 Gestion des terminaux / données / tâches

### Gestion des terminaux

```
┌─────────────────────────────────────────────────────────────────┐
│                  GESTION DES TERMINAUX                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Terminal 3270                        BMS (Basic Mapping Support)│
│  ┌─────────────────────────┐         ┌────────────────────────┐ │
│  │ ▌MENU PRINCIPAL      ▐ │         │  • Définition d'écrans │ │
│  │                         │         │  • Mappage des champs  │ │
│  │  1. Consultation        │  ◄────► │  • Validation saisie   │ │
│  │  2. Virement            │         │  • Formatage affichage │ │
│  │  3. Historique          │         └────────────────────────┘ │
│  │                         │                                     │
│  │  Choix: _               │         Commandes:                  │
│  │                         │         • SEND MAP                  │
│  │  PF3=Quitter            │         • RECEIVE MAP               │
│  └─────────────────────────┘         • SEND TEXT                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Composants de gestion des terminaux :**

| Composant | Rôle |
|-----------|------|
| **TCP** (Terminal Control Program) | Gestion des communications |
| **BMS** (Basic Mapping Support) | Définition et gestion des écrans |
| **TCT** (Terminal Control Table) | Liste des terminaux autorisés |

### Gestion des données

```
┌─────────────────────────────────────────────────────────────────┐
│                   GESTION DES DONNÉES                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐       │
│  │    VSAM     │     │    DB2      │     │    IMS      │       │
│  │   (Fichiers)│     │    (SQL)    │     │(Hiérarchique)│       │
│  └──────┬──────┘     └──────┬──────┘     └──────┬──────┘       │
│         │                   │                   │               │
│         └───────────────────┼───────────────────┘               │
│                             │                                    │
│                    ┌────────▼────────┐                          │
│                    │   File Control  │                          │
│                    │       (FC)      │                          │
│                    └─────────────────┘                          │
│                                                                  │
│  Commandes:  READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT   │
│                                                                  │
│  Stockage temporaire:                                           │
│  • TS (Temporary Storage) - Données intermédiaires              │
│  • TD (Transient Data) - Files d'attente                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Types d'accès aux données :**

| Type | Description | Commandes |
|------|-------------|-----------|
| **Fichiers VSAM** | KSDS, ESDS, RRDS | READ, WRITE, DELETE |
| **DB2** | SQL embarqué | EXEC SQL ... END-EXEC |
| **IMS DB** | Base hiérarchique | EXEC DLI ... END-EXEC |
| **Temporary Storage** | Données temporaires | WRITEQ TS, READQ TS |
| **Transient Data** | Files d'attente | WRITEQ TD, READQ TD |

### Gestion des tâches

```
┌─────────────────────────────────────────────────────────────────┐
│                   GESTION DES TÂCHES                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Une TÂCHE (Task) = Instance d'exécution d'une transaction      │
│                                                                  │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐     │
│  │ Task 1  │    │ Task 2  │    │ Task 3  │    │ Task 4  │     │
│  │ MENU    │    │ INQU    │    │ MENU    │    │ XFER    │     │
│  │ User A  │    │ User B  │    │ User C  │    │ User A  │     │
│  └─────────┘    └─────────┘    └─────────┘    └─────────┘     │
│       │              │              │              │            │
│       └──────────────┴──────────────┴──────────────┘            │
│                             │                                    │
│                    ┌────────▼────────┐                          │
│                    │  Task Control   │                          │
│                    │      (KCP)      │                          │
│                    └─────────────────┘                          │
│                                                                  │
│  • Multitâche : plusieurs tâches simultanées                    │
│  • Priorités : ordonnancement des tâches                        │
│  • États : RUNNING, WAITING, SUSPENDED                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Cycle de vie d'une tâche :**

```
  ┌──────────┐
  │  CRÉÉE   │  ◄── Utilisateur lance une transaction
  └────┬─────┘
       │
       ▼
  ┌──────────┐
  │  PRÊTE   │  ◄── En attente d'exécution
  └────┬─────┘
       │
       ▼
  ┌──────────┐
  │ RUNNING  │  ◄── En cours d'exécution
  └────┬─────┘
       │
       ├────────────┐
       ▼            ▼
  ┌──────────┐  ┌──────────┐
  │ WAITING  │  │SUSPENDED │  ◄── En attente I/O ou ressource
  └────┬─────┘  └────┬─────┘
       │             │
       └──────┬──────┘
              │
              ▼
        ┌──────────┐
        │ TERMINÉE │  ◄── Fin normale ou ABEND
        └──────────┘
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│              CHAPITRE I - RÉSUMÉ                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  I-1 HISTORIQUE                                                  │
│      • Créé en 1968 par IBM                                     │
│      • Évolution continue jusqu'à CICS TS V6.x                  │
│                                                                  │
│  I-2 DÉFINITION                                                  │
│      • Moniteur transactionnel (TP Monitor)                     │
│      • Transactions ACID identifiées par 4 caractères           │
│                                                                  │
│  I-3 CICS SOUS z/OS                                             │
│      • Région CICS = instance en exécution                      │
│      • Intégration RACF, DB2, IMS, MQ                          │
│                                                                  │
│  I-4 CARACTÉRISTIQUES                                           │
│      • Haute disponibilité (24/7)                               │
│      • Performance (milliers de trans/sec)                      │
│      • Sécurité intégrée                                        │
│                                                                  │
│  I-5 FONCTIONS PRINCIPALES                                      │
│      • Terminal, File, Program, Task Control                    │
│      • Storage, Interval Control                                │
│      • Temporary Storage, Transient Data                        │
│                                                                  │
│  I-6 BATCH vs ONLINE                                            │
│      • Batch : différé, gros volumes, sans interaction         │
│      • Online : temps réel, petites transactions, interactif   │
│                                                                  │
│  I-7 GESTION TERMINAUX/DONNÉES/TÂCHES                          │
│      • Terminaux : BMS, écrans 3270                            │
│      • Données : VSAM, DB2, IMS, TS, TD                        │
│      • Tâches : multitâche, priorités, états                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [Suivant : Organisation du système →](02-organisation-systeme.md)
- [Retour au sommaire](README.md)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Organisation Système](02-organisation-systeme.md) |

---
*Formation COBOL - Module CICS*
