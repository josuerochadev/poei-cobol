# Chapitre V - Notions d'architecture Z/OS

## Introduction

Ce chapitre présente trois composants fondamentaux de l'architecture z/OS :
1. **SYSPLEX** - Clustering et haute disponibilité
2. **SMS** - Gestion automatisée du stockage
3. **RACF** - Sécurité et contrôle d'accès

---

## V-1 Notion de SYSPLEX

### Définition

Un **SYSPLEX** (System Complex) est un cluster de systèmes z/OS qui travaillent ensemble comme une seule entité logique.

```
┌─────────────────────────────────────────────────────────────────┐
│                    QU'EST-CE QU'UN SYSPLEX ?                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Un SYSPLEX permet de :                                        │
│                                                                  │
│   • Combiner plusieurs mainframes en un seul système logique   │
│   • Partager les données et les ressources                     │
│   • Assurer la haute disponibilité (failover automatique)      │
│   • Équilibrer la charge de travail                            │
│   • Augmenter la capacité sans interruption                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de SYSPLEX

#### Basic SYSPLEX

Configuration simple sans partage de données :

```
┌─────────────────────────────────────────────────────────────────┐
│                    BASIC SYSPLEX                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌───────────┐         ┌───────────┐         ┌───────────┐    │
│   │  System   │         │  System   │         │  System   │    │
│   │    A      │         │    B      │         │    C      │    │
│   │  (z/OS)   │         │  (z/OS)   │         │  (z/OS)   │    │
│   └─────┬─────┘         └─────┬─────┘         └─────┬─────┘    │
│         │                     │                     │          │
│         └──────────┬──────────┴──────────┬──────────┘          │
│                    │                     │                      │
│              ┌─────┴─────┐         ┌─────┴─────┐               │
│              │   DASD    │         │   DASD    │               │
│              │  (Disques)│         │  (Disques)│               │
│              └───────────┘         └───────────┘               │
│                                                                  │
│   Caractéristiques :                                            │
│   • Pas de Coupling Facility                                   │
│   • Synchronisation par canaux CTC                             │
│   • Partage de données limité                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Parallel SYSPLEX

Configuration avancée avec Coupling Facility :

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARALLEL SYSPLEX                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌───────────┐         ┌───────────┐         ┌───────────┐    │
│   │  System   │         │  System   │         │  System   │    │
│   │    A      │         │    B      │         │    C      │    │
│   │  (z/OS)   │         │  (z/OS)   │         │  (z/OS)   │    │
│   └─────┬─────┘         └─────┬─────┘         └─────┬─────┘    │
│         │                     │                     │          │
│         └──────────┬──────────┼──────────┬──────────┘          │
│                    │          │          │                      │
│                    ▼          ▼          ▼                      │
│              ┌─────────────────────────────────┐               │
│              │      COUPLING FACILITY          │               │
│              │  ┌─────────────────────────┐   │               │
│              │  │ • Lock structures       │   │               │
│              │  │ • Cache structures      │   │               │
│              │  │ • List structures       │   │               │
│              │  └─────────────────────────┘   │               │
│              └─────────────────────────────────┘               │
│                              │                                  │
│                    ┌─────────┼─────────┐                       │
│                    ▼         ▼         ▼                       │
│              ┌─────────┐ ┌─────────┐ ┌─────────┐              │
│              │  DASD   │ │  DASD   │ │  DASD   │              │
│              │ Shared  │ │ Shared  │ │ Shared  │              │
│              └─────────┘ └─────────┘ └─────────┘              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Coupling Facility (CF)

La **Coupling Facility** est le cœur du Parallel SYSPLEX :

```
┌─────────────────────────────────────────────────────────────────┐
│                    COUPLING FACILITY                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   STRUCTURES DE LA CF                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  LOCK STRUCTURES                                       │   │
│   │  ├── Gestion des verrous inter-systèmes               │   │
│   │  ├── Sérialisation des accès aux données              │   │
│   │  └── Évite les conflits d'accès concurrent            │   │
│   │                                                         │   │
│   │  CACHE STRUCTURES                                      │   │
│   │  ├── Cache partagé entre systèmes                     │   │
│   │  ├── Cohérence des données (invalidation croisée)     │   │
│   │  └── Performance des accès DB2, VSAM                  │   │
│   │                                                         │   │
│   │  LIST STRUCTURES                                       │   │
│   │  ├── Files d'attente partagées                        │   │
│   │  ├── Communication inter-systèmes                     │   │
│   │  └── Workload balancing                               │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   AVANTAGES                                                     │
│   • Accès concurrent aux mêmes données                         │
│   • Haute disponibilité (failover transparent)                 │
│   • Scalabilité linéaire                                       │
│   • Single System Image (SSI)                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Composants d'un SYSPLEX

| Composant | Fonction |
|-----------|----------|
| **XCF** | Cross-system Coupling Facility - Communication inter-systèmes |
| **XES** | Cross-system Extended Services - Services étendus |
| **WLM** | Workload Manager - Répartition de charge |
| **ARM** | Automatic Restart Manager - Redémarrage automatique |
| **SFM** | Sysplex Failure Management - Gestion des pannes |
| **GRS** | Global Resource Serialization - Sérialisation globale |

### Avantages du SYSPLEX

```
┌─────────────────────────────────────────────────────────────────┐
│                    AVANTAGES SYSPLEX                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   HAUTE DISPONIBILITÉ                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Failover automatique en cas de panne                │   │
│   │  • Pas de point unique de défaillance                  │   │
│   │  • Maintenance sans interruption (rolling upgrade)      │   │
│   │  • Disponibilité 99,999% (5 min d'arrêt/an)           │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   SCALABILITÉ                                                   │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Ajout de systèmes sans interruption                 │   │
│   │  • Jusqu'à 32 systèmes par SYSPLEX                     │   │
│   │  • Capacité quasi-linéaire                             │   │
│   │  • Équilibrage dynamique de charge                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   PARTAGE DE DONNÉES                                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • DB2 Data Sharing                                    │   │
│   │  • VSAM Record Level Sharing (RLS)                     │   │
│   │  • IMS Data Sharing                                    │   │
│   │  • Accès concurrent sans conflit                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple : DB2 Data Sharing

```
┌─────────────────────────────────────────────────────────────────┐
│                    DB2 DATA SHARING                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────┐     ┌─────────┐     ┌─────────┐                  │
│   │  DB2    │     │  DB2    │     │  DB2    │                  │
│   │ Member  │     │ Member  │     │ Member  │                  │
│   │   A     │     │   B     │     │   C     │                  │
│   └────┬────┘     └────┬────┘     └────┬────┘                  │
│        │               │               │                        │
│        └───────────────┼───────────────┘                        │
│                        │                                        │
│                        ▼                                        │
│              ┌─────────────────┐                                │
│              │     GROUP       │                                │
│              │   BUFFER POOL   │  (dans la Coupling Facility)  │
│              └────────┬────────┘                                │
│                       │                                        │
│                       ▼                                        │
│              ┌─────────────────┐                                │
│              │   BASE DE       │                                │
│              │   DONNÉES       │  (DASD partagé)               │
│              │   PARTAGÉE      │                                │
│              └─────────────────┘                                │
│                                                                  │
│   Tous les membres DB2 accèdent aux mêmes données              │
│   avec cohérence garantie par la CF                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## V-2 Notion de SMS

### Définition

**SMS (Storage Management Subsystem)** est le système de gestion automatisée du stockage sous z/OS.

```
┌─────────────────────────────────────────────────────────────────┐
│                    QU'EST-CE QUE SMS ?                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   SMS permet de :                                               │
│                                                                  │
│   • Automatiser l'allocation des datasets                      │
│   • Gérer le placement sur les volumes                         │
│   • Appliquer des politiques de stockage                       │
│   • Migrer automatiquement les données                         │
│   • Sauvegarder selon des règles définies                      │
│                                                                  │
│   AVANT SMS                          AVEC SMS                   │
│   ┌─────────────────────┐           ┌─────────────────────┐    │
│   │ L'utilisateur       │           │ SMS choisit         │    │
│   │ choisit le volume   │    →      │ automatiquement     │    │
│   │ (VOLUME=VOL001)     │           │ le meilleur volume  │    │
│   └─────────────────────┘           └─────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Architecture SMS

```
┌─────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE SMS                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   UTILISATEUR                                                   │
│       │                                                          │
│       │  Demande allocation dataset                             │
│       ▼                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                         SMS                              │   │
│   │  ┌─────────────────────────────────────────────────┐   │   │
│   │  │              ACS ROUTINES                        │   │   │
│   │  │         (Automatic Class Selection)              │   │   │
│   │  └─────────────────────────────────────────────────┘   │   │
│   │                        │                                │   │
│   │       ┌────────────────┼────────────────┐              │   │
│   │       ▼                ▼                ▼              │   │
│   │  ┌─────────┐     ┌─────────┐     ┌─────────┐         │   │
│   │  │  DATA   │     │ STORAGE │     │MANAGEMENT│         │   │
│   │  │  CLASS  │     │  CLASS  │     │  CLASS   │         │   │
│   │  └─────────┘     └─────────┘     └─────────┘         │   │
│   │                                                        │   │
│   └─────────────────────────────────────────────────────────┘   │
│                        │                                        │
│                        ▼                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │              STORAGE GROUPS                              │   │
│   │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐      │   │
│   │  │ VOL001  │ │ VOL002  │ │ VOL003  │ │ VOL004  │      │   │
│   │  └─────────┘ └─────────┘ └─────────┘ └─────────┘      │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Classes SMS

#### Data Class

Définit les **attributs physiques** du dataset :

```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA CLASS                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Attributs définis :                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • RECFM     - Format d'enregistrement (F, FB, V, VB)  │   │
│   │  • LRECL     - Longueur d'enregistrement               │   │
│   │  • BLKSIZE   - Taille de bloc                          │   │
│   │  • SPACE     - Allocation primaire/secondaire          │   │
│   │  • DSORG     - Organisation (PS, PO, VS)               │   │
│   │  • EXPDT     - Date d'expiration                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemple de Data Class :                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  DATA CLASS : DCCOBOL                                   │   │
│   │  ─────────────────────────                              │   │
│   │  RECFM     : FB                                         │   │
│   │  LRECL     : 80                                         │   │
│   │  BLKSIZE   : 27920                                      │   │
│   │  SPACE     : (5,2) TRACKS                              │   │
│   │  DSORG     : PO                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Storage Class

Définit les **exigences de performance** :

```
┌─────────────────────────────────────────────────────────────────┐
│                    STORAGE CLASS                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Attributs définis :                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Disponibilité requise                               │   │
│   │  • Performance d'accès (temps de réponse)              │   │
│   │  • Accessibilité (lecture seule, R/W)                  │   │
│   │  • Type de support (DASD rapide, standard)             │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemples de Storage Classes :                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  SCFAST   - Haute performance (SSD, cache)             │   │
│   │  SCSTD    - Performance standard                        │   │
│   │  SCARCH   - Archivage (bandes, faible coût)            │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Management Class

Définit les **politiques de gestion du cycle de vie** :

```
┌─────────────────────────────────────────────────────────────────┐
│                    MANAGEMENT CLASS                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Politiques définies :                                         │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Migration automatique vers stockage moins coûteux   │   │
│   │  • Sauvegarde automatique (fréquence, rétention)       │   │
│   │  • Expiration et suppression automatique               │   │
│   │  • Compression                                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Cycle de vie géré par SMS :                                   │
│                                                                  │
│   ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐    │
│   │ Création│ →  │Migration│ →  │ Archive │ →  │Suppression│   │
│   │  DASD   │    │  ML1    │    │  Bande  │    │          │    │
│   │ rapide  │    │         │    │         │    │          │    │
│   └─────────┘    └─────────┘    └─────────┘    └─────────┘    │
│       │              │              │              │            │
│    Jour 0      Après 30j      Après 90j      Après 365j       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Storage Group

Groupe de volumes gérés ensemble :

```
┌─────────────────────────────────────────────────────────────────┐
│                    STORAGE GROUPS                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Types de Storage Groups :                                     │
│                                                                  │
│   POOL                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  Groupe de volumes DASD pour allocation automatique    │   │
│   │  ┌───────┐ ┌───────┐ ┌───────┐ ┌───────┐              │   │
│   │  │VOL001 │ │VOL002 │ │VOL003 │ │VOL004 │              │   │
│   │  └───────┘ └───────┘ └───────┘ └───────┘              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   VIO (Virtual I/O)                                             │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  Datasets temporaires en mémoire virtuelle             │   │
│   │  Performance maximale, pas d'I/O physique              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   TAPE                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  Groupe de volumes bande pour archivage                │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### ACS Routines

Les **ACS (Automatic Class Selection) Routines** déterminent automatiquement les classes :

```
┌─────────────────────────────────────────────────────────────────┐
│                    ACS ROUTINES                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Critères de sélection :                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Nom du dataset (HLQ, qualificateurs)                │   │
│   │  • Userid du créateur                                   │   │
│   │  • Job name                                             │   │
│   │  • Type de dataset demandé                              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Exemple de règle ACS :                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  FILTLIST PROD_HLQ INCLUDE('PROD.**')                  │   │
│   │                                                         │   │
│   │  SELECT                                                 │   │
│   │    WHEN (&HLQ = &PROD_HLQ)                             │   │
│   │      SET &STORCLAS = 'SCPROD'                          │   │
│   │      SET &MGMTCLAS = 'MCPROD'                          │   │
│   │    OTHERWISE                                            │   │
│   │      SET &STORCLAS = 'SCSTD'                           │   │
│   │  END                                                    │   │
│   │                                                         │   │
│   │  → Datasets PROD.** → Storage Class SCPROD             │   │
│   │  → Autres datasets → Storage Class SCSTD               │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Avantages de SMS

| Avant SMS | Avec SMS |
|-----------|----------|
| Choix manuel du volume | Sélection automatique |
| Gestion manuelle de l'espace | Équilibrage automatique |
| Sauvegarde manuelle | Backup automatisé |
| Migration manuelle | HSM automatique |
| Incohérence des attributs | Standards enforced |

---

## V-3 Notion de RACF

### Définition

**RACF (Resource Access Control Facility)** est le système de sécurité principal de z/OS.

```
┌─────────────────────────────────────────────────────────────────┐
│                    QU'EST-CE QUE RACF ?                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   RACF assure :                                                 │
│                                                                  │
│   • L'authentification des utilisateurs                        │
│   • L'autorisation d'accès aux ressources                      │
│   • L'audit des accès (journalisation)                         │
│   • La gestion des mots de passe                               │
│   • La protection des données                                   │
│                                                                  │
│   Certifications :                                              │
│   • Common Criteria EAL5+                                      │
│   • FIPS 140-2                                                 │
│   • SOX, PCI-DSS compliance                                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Architecture RACF

```
┌─────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE RACF                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   UTILISATEUR                                                   │
│       │                                                          │
│       │  Demande d'accès à une ressource                        │
│       ▼                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    RACF                                  │   │
│   │                                                          │   │
│   │  1. IDENTIFICATION                                       │   │
│   │     Qui êtes-vous ? (userid)                            │   │
│   │                                                          │   │
│   │  2. AUTHENTIFICATION                                     │   │
│   │     Prouvez-le ! (password, certificat, MFA)            │   │
│   │                                                          │   │
│   │  3. AUTORISATION                                         │   │
│   │     Avez-vous le droit ? (profils, permissions)         │   │
│   │                                                          │   │
│   │  4. AUDIT                                                │   │
│   │     Journalisation de l'accès (SMF records)             │   │
│   │                                                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                        │                                        │
│          ┌─────────────┼─────────────┐                         │
│          ▼             ▼             ▼                         │
│     ┌─────────┐   ┌─────────┐   ┌─────────┐                   │
│     │ Accès   │   │ Accès   │   │ Accès   │                   │
│     │ Autorisé│   │ Refusé  │   │ Journalisé                  │
│     └─────────┘   └─────────┘   └─────────┘                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Base de données RACF

```
┌─────────────────────────────────────────────────────────────────┐
│                    BASE DE DONNÉES RACF                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   La base RACF contient :                                       │
│                                                                  │
│   PROFILS UTILISATEURS                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  USER=USER01                                            │   │
│   │  ├── Nom          : DUPONT JEAN                        │   │
│   │  ├── Groupe       : DEVGROUP                           │   │
│   │  ├── Attributs    : SPECIAL, OPERATIONS               │   │
│   │  ├── Password     : ******** (crypté)                  │   │
│   │  └── Dernière connexion : 2025/001 14:30              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   PROFILS GROUPES                                               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  GROUP=DEVGROUP                                         │   │
│   │  ├── Supérieur    : SYS1                               │   │
│   │  ├── Propriétaire : ADMIN01                            │   │
│   │  └── Membres      : USER01, USER02, USER03             │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   PROFILS RESSOURCES                                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  DATASET='PROD.PAYROLL.**'                             │   │
│   │  ├── UACC        : NONE (accès par défaut)            │   │
│   │  ├── Propriétaire: PAYROLL                            │   │
│   │  └── Access List :                                     │   │
│   │      ├── USER01  : READ                                │   │
│   │      ├── USER02  : UPDATE                              │   │
│   │      └── PAYROLL : ALTER                               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Niveaux d'accès RACF

```
┌─────────────────────────────────────────────────────────────────┐
│                    NIVEAUX D'ACCÈS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Du moins au plus permissif :                                  │
│                                                                  │
│   ┌─────────┬───────────────────────────────────────────────┐   │
│   │  NONE   │ Aucun accès                                   │   │
│   ├─────────┼───────────────────────────────────────────────┤   │
│   │ EXECUTE │ Exécution seulement (programmes)              │   │
│   ├─────────┼───────────────────────────────────────────────┤   │
│   │  READ   │ Lecture seule                                 │   │
│   ├─────────┼───────────────────────────────────────────────┤   │
│   │ UPDATE  │ Lecture et écriture                           │   │
│   ├─────────┼───────────────────────────────────────────────┤   │
│   │ CONTROL │ Contrôle (VSAM, modification attributs)       │   │
│   ├─────────┼───────────────────────────────────────────────┤   │
│   │  ALTER  │ Tous droits (y compris suppression)           │   │
│   └─────────┴───────────────────────────────────────────────┘   │
│                                                                  │
│   Exemple :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  Dataset : PROD.PAYROLL.DATA                           │   │
│   │                                                         │   │
│   │  UACC=NONE     → Par défaut, personne n'a accès       │   │
│   │  USER01=READ   → USER01 peut lire                      │   │
│   │  USER02=UPDATE → USER02 peut lire et modifier          │   │
│   │  ADMIN=ALTER   → ADMIN a tous les droits              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Classes de ressources RACF

| Classe | Ressources protégées |
|--------|---------------------|
| **DATASET** | Fichiers (datasets) |
| **FACILITY** | Fonctions système |
| **PROGRAM** | Programmes exécutables |
| **TERMINAL** | Terminaux |
| **TAPEVOL** | Volumes bande |
| **DASDVOL** | Volumes disque |
| **SURROGAT** | Soumission de jobs pour un autre utilisateur |
| **STARTED** | Started tasks |

### Commandes RACF principales

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDES RACF                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   GESTION DES UTILISATEURS                                      │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  ADDUSER userid ...        Créer un utilisateur        │   │
│   │  ALTUSER userid ...        Modifier un utilisateur     │   │
│   │  DELUSER userid            Supprimer un utilisateur    │   │
│   │  LISTUSER userid           Afficher infos utilisateur  │   │
│   │  PASSWORD userid           Réinitialiser mot de passe  │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   GESTION DES GROUPES                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  ADDGROUP groupe ...       Créer un groupe             │   │
│   │  ALTGROUP groupe ...       Modifier un groupe          │   │
│   │  DELGROUP groupe           Supprimer un groupe         │   │
│   │  LISTGRP groupe            Afficher infos groupe       │   │
│   │  CONNECT userid GROUP(grp) Ajouter user au groupe      │   │
│   │  REMOVE userid GROUP(grp)  Retirer user du groupe      │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   GESTION DES RESSOURCES                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  ADDSD 'dsname' ...        Protéger un dataset         │   │
│   │  ALTDSD 'dsname' ...       Modifier protection         │   │
│   │  DELDSD 'dsname'           Supprimer protection        │   │
│   │  LISTDSD 'dsname'          Afficher protection         │   │
│   │  PERMIT 'dsname' ID(user) ACCESS(level)  Autoriser     │   │
│   │  PERMIT 'dsname' ID(user) DELETE         Révoquer      │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   RECHERCHE                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  SEARCH MASK('PROD.**')    Rechercher profils          │   │
│   │  SEARCH USER(userid)       Profils d'un utilisateur    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemples RACF

```
┌─────────────────────────────────────────────────────────────────┐
│                    EXEMPLES RACF                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   1. Créer un utilisateur :                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  ADDUSER USER01 NAME('DUPONT JEAN') DFLTGRP(DEVGROUP)  │   │
│   │          PASSWORD(INITIAL) OWNER(ADMIN)                 │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   2. Protéger un dataset :                                      │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  ADDSD 'PROD.PAYROLL.**' UACC(NONE) OWNER(PAYROLL)    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   3. Autoriser l'accès :                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  PERMIT 'PROD.PAYROLL.**' ID(USER01) ACCESS(READ)      │   │
│   │  PERMIT 'PROD.PAYROLL.**' ID(PAYGRP) ACCESS(UPDATE)    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   4. Vérifier les accès :                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  LISTDSD 'PROD.PAYROLL.**' AUTH                        │   │
│   │                                                         │   │
│   │  Résultat:                                              │   │
│   │  PROD.PAYROLL.** (G)                                   │   │
│   │     UACC=NONE                                          │   │
│   │     USER01   READ                                       │   │
│   │     PAYGRP   UPDATE                                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Messages d'erreur RACF courants

| Code | Message | Cause |
|------|---------|-------|
| **ICH408I** | USER NOT DEFINED | Utilisateur inconnu |
| **ICH409I** | PASSWORD NOT AUTHORIZED | Mauvais mot de passe |
| **ICH70001I** | NOT AUTHORIZED TO DATASET | Accès refusé |
| **ICH70002I** | INSUFFICIENT AUTHORITY | Droits insuffisants |
| **IRR012I** | VERIFICATION FAILED | Authentification échouée |

---

## Synthèse

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLÉS DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ✓ SYSPLEX - Haute disponibilité et scalabilité                │
│    • Basic SYSPLEX : Clustering simple                         │
│    • Parallel SYSPLEX : Avec Coupling Facility                 │
│    • Data Sharing : DB2, VSAM, IMS                             │
│    • Failover automatique, disponibilité 99,999%               │
│                                                                  │
│  ✓ SMS - Gestion automatisée du stockage                       │
│    • Data Class : Attributs physiques (RECFM, LRECL...)        │
│    • Storage Class : Performance et disponibilité              │
│    • Management Class : Cycle de vie (backup, migration)       │
│    • Storage Group : Groupes de volumes                        │
│    • ACS Routines : Sélection automatique des classes          │
│                                                                  │
│  ✓ RACF - Sécurité et contrôle d'accès                         │
│    • Authentification : userid/password                        │
│    • Autorisation : Profils et permissions                     │
│    • Niveaux : NONE < READ < UPDATE < CONTROL < ALTER          │
│    • Classes : DATASET, FACILITY, PROGRAM...                   │
│    • Commandes : ADDUSER, ADDSD, PERMIT, LISTDSD               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-mémoire

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MÉMOIRE ARCHITECTURE                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SYSPLEX                                                        │
│  ────────────────────────────────────────────────────────────── │
│  Coupling Facility    Centre du Parallel SYSPLEX               │
│  XCF                  Communication inter-systèmes              │
│  WLM                  Workload Manager (répartition)            │
│  ARM                  Automatic Restart Manager                 │
│  GRS                  Global Resource Serialization             │
│                                                                  │
│  SMS - CLASSES                                                  │
│  ────────────────────────────────────────────────────────────── │
│  Data Class           Attributs physiques (DCB)                │
│  Storage Class        Performance requise                       │
│  Management Class     Cycle de vie (backup, migration)          │
│  Storage Group        Groupe de volumes                         │
│  ACS Routine          Sélection automatique                     │
│                                                                  │
│  RACF - NIVEAUX D'ACCÈS                                         │
│  ────────────────────────────────────────────────────────────── │
│  NONE                 Aucun accès                               │
│  EXECUTE              Exécution seulement                       │
│  READ                 Lecture seule                             │
│  UPDATE               Lecture + écriture                        │
│  CONTROL              Contrôle VSAM                             │
│  ALTER                Tous droits                               │
│                                                                  │
│  RACF - COMMANDES                                               │
│  ────────────────────────────────────────────────────────────── │
│  ADDUSER/ALTUSER/DELUSER     Gestion utilisateurs              │
│  ADDGROUP/CONNECT/REMOVE     Gestion groupes                   │
│  ADDSD/ALTDSD/DELDSD         Protection datasets               │
│  PERMIT ... ACCESS(level)    Autoriser accès                   │
│  LISTUSER/LISTGRP/LISTDSD    Consulter profils                 │
│  SEARCH MASK('pattern')      Rechercher profils                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IV - ISPF/PDF](04-ispf.md) | [Chapitre VI - JCL](06-jcl.md) |
