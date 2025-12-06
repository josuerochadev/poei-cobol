# Chapitre I - Présentation générale de Z/OS

## Introduction

Ce chapitre présente l'environnement dans lequel sera utilisé le système de gestion de fichiers VSAM. Le système d'exploitation IBM Z/OS utilise l'outil VSAM pour tous les fichiers stockés sur disque.

---

## I-1 Historique des Mainframes

### Les origines (années 1950-1960)

| Année | Événement | Système |
|-------|-----------|---------|
| 1952 | Premier ordinateur scientifique IBM | IBM 701 |
| 1953 | Premier ordinateur commercial | IBM 702 |
| 1959 | Transistors remplacent les tubes | IBM 7090 |
| 1964 | **Révolution System/360** | IBM S/360 |

### L'ère System/360 (1964)

Le **System/360** marque un tournant majeur :
- Premier système **compatible ascendant** (programmes portables entre modèles)
- Architecture **unifiée** pour applications scientifiques et commerciales
- Introduction du concept de **famille de machines**
- Base de l'architecture mainframe moderne

### Évolution des systèmes

```
System/360 (1964)
    ↓
System/370 (1970) - Mémoire virtuelle
    ↓
System/370-XA (1983) - Adressage 31 bits
    ↓
System/390 (1990) - ESCON, Sysplex
    ↓
zSeries (2000) - 64 bits, Linux
    ↓
System z (2006) - Virtualisation avancée
    ↓
IBM Z (2017-aujourd'hui) - Cloud hybride, IA
```

### Évolution des systèmes d'exploitation

| Période | Système | Caractéristiques |
|---------|---------|------------------|
| 1966 | OS/360 | Premier OS multiprogrammation |
| 1972 | OS/VS1, OS/VS2 | Mémoire virtuelle |
| 1974 | MVS | Multiple Virtual Storage |
| 1988 | MVS/ESA | Extended Storage Architecture |
| 1995 | OS/390 | Unix System Services |
| 2001 | **z/OS** | 64 bits, Unicode, TCP/IP natif |

---

## I-2 IBM Z et l'environnement Multicloud hybride

### Positionnement actuel d'IBM Z

IBM Z n'est plus un système isolé mais le **cœur d'une architecture cloud hybride** :

```
                    ┌─────────────────────────────────────┐
                    │         CLOUD HYBRIDE IBM           │
                    ├─────────────────────────────────────┤
                    │                                     │
    ┌───────────┐   │   ┌───────────┐   ┌───────────┐   │   ┌───────────┐
    │  Cloud    │◄──┼──►│   IBM Z   │◄──┼──►│  Cloud   │◄──┼──►│  Cloud    │
    │  Privé    │   │   │  (z/OS)   │   │   │  IBM     │   │   │  Public   │
    └───────────┘   │   └───────────┘   │   └───────────┘   │   └───────────┘
                    │         ▲         │                   │
                    │         │         │                   │
                    │   ┌─────┴─────┐   │                   │
                    │   │  Linux    │   │                   │
                    │   │  on Z     │   │                   │
                    │   └───────────┘   │                   │
                    └─────────────────────────────────────┘
```

### Capacités Multicloud

| Composant | Fonction |
|-----------|----------|
| **IBM Cloud Pak** | Conteneurs sur z/OS et Linux on Z |
| **Red Hat OpenShift** | Orchestration Kubernetes sur Z |
| **Ansible** | Automatisation infrastructure Z |
| **z/OS Connect** | APIs REST pour applications z/OS |
| **IBM Wazi** | Développement cloud pour z/OS |

### Intégration avec les technologies modernes

```
┌────────────────────────────────────────────────────────────┐
│                    APPLICATIONS MODERNES                    │
├────────────────────────────────────────────────────────────┤
│  React │ Angular │ Node.js │ Python │ Java │ Go │ REST    │
└────────────────────────────────────────────────────────────┘
                              ▲
                              │ APIs / z/OS Connect
                              ▼
┌────────────────────────────────────────────────────────────┐
│                         IBM Z                               │
├──────────────────────┬─────────────────────────────────────┤
│       z/OS           │           Linux on Z                 │
├──────────────────────┼─────────────────────────────────────┤
│ • COBOL              │ • Containers (Docker/Podman)        │
│ • CICS               │ • Kubernetes/OpenShift              │
│ • DB2                │ • MongoDB, PostgreSQL               │
│ • IMS                │ • Apache Kafka                      │
│ • VSAM               │ • Machine Learning                  │
└──────────────────────┴─────────────────────────────────────┘
```

---

## I-3 Longévité record des Mainframes IBM

### Statistiques de durabilité

| Aspect | Durée/Statistique |
|--------|-------------------|
| **Compatibilité ascendante** | 60+ ans (depuis 1964) |
| **Applications COBOL actives** | 220+ milliards de lignes de code |
| **Transactions quotidiennes** | 30+ milliards (cartes bancaires, etc.) |
| **Disponibilité** | 99,999% (5 min d'arrêt/an) |

### Pourquoi cette longévité ?

1. **Investissements protégés**
   - Code COBOL de 1970 fonctionne encore aujourd'hui
   - Pas de réécriture nécessaire lors des mises à niveau matérielles
   - ROI sur plusieurs décennies

2. **Fiabilité légendaire**
   - Architecture redondante native
   - Récupération automatique des erreurs
   - Isolation des workloads

3. **Évolution continue**
   - Nouvelles fonctionnalités à chaque génération
   - Support des technologies modernes (containers, IA)
   - Performance en constante amélioration

### Secteurs d'utilisation majeurs

```
┌─────────────────────────────────────────────────────────────┐
│                  SECTEURS MAINFRAME                          │
├──────────────┬──────────────┬──────────────┬───────────────┤
│   BANQUE     │  ASSURANCE   │   RETAIL     │  TRANSPORT    │
│   92% des    │   80% des    │   23 des 25  │   Réservation │
│   transactions│  assureurs   │   plus grands│   aérienne    │
│   mondiales  │   mondiaux   │   retailers  │   mondiale    │
├──────────────┼──────────────┼──────────────┼───────────────┤
│   SANTÉ      │   GOUVERNEMENT│   TELECOM   │   ÉNERGIE     │
│   Dossiers   │   Impôts,    │   Facturation│   Réseaux     │
│   patients   │   Sécurité   │   millions   │   électriques │
│              │   sociale    │   d'abonnés  │               │
└──────────────┴──────────────┴──────────────┴───────────────┘
```

---

## I-4 Points forts des Mainframes

### Tableau comparatif

| Critère | Mainframe | Serveurs distribués |
|---------|-----------|---------------------|
| **Disponibilité** | 99,999% | 99,9% (typique) |
| **Transactions/sec** | Millions | Milliers |
| **Sécurité** | EAL5+ (certifié) | Variable |
| **Virtualisation** | Native depuis 1960 | Ajoutée (VMware, etc.) |
| **Coût/transaction** | Très faible | Plus élevé à grande échelle |
| **Empreinte carbone** | Optimisée | Plus importante |

### Les 6 piliers de la puissance mainframe

#### 1. Performance et scalabilité

```
                    CAPACITÉ DE TRAITEMENT
    ┌────────────────────────────────────────────────┐
    │                                                │
    │  ████████████████████████████  19 MIPS (z16)  │
    │  ████████████████████████      17 MIPS (z15)  │
    │  ████████████████              13 MIPS (z14)  │
    │  ██████████████                12 MIPS (z13)  │
    │                                                │
    │  MIPS = Millions d'Instructions Par Seconde   │
    └────────────────────────────────────────────────┘
```

- **Jusqu'à 200 processeurs** par système
- **40 To de mémoire** maximum
- **Millions de transactions** par seconde

#### 2. Haute disponibilité

```
┌─────────────────────────────────────────────────────────┐
│              ARCHITECTURE HAUTE DISPONIBILITÉ            │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   ┌─────────┐     ┌─────────┐     ┌─────────┐          │
│   │ LPAR 1  │     │ LPAR 2  │     │ LPAR 3  │          │
│   │(Prod)   │     │(Backup) │     │(Test)   │          │
│   └────┬────┘     └────┬────┘     └────┬────┘          │
│        │               │               │                │
│        └───────────────┼───────────────┘                │
│                        ▼                                │
│              ┌─────────────────┐                        │
│              │    SYSPLEX      │ ◄── Cluster de         │
│              │   (Parallel)    │     mainframes         │
│              └─────────────────┘                        │
│                        │                                │
│        ┌───────────────┼───────────────┐                │
│        ▼               ▼               ▼                │
│   ┌─────────┐     ┌─────────┐     ┌─────────┐          │
│   │ DASD 1  │     │ DASD 2  │     │ DASD 3  │          │
│   │(Miroir) │◄───►│(Miroir) │◄───►│(Miroir) │          │
│   └─────────┘     └─────────┘     └─────────┘          │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

- **Parallel Sysplex** : Cluster de mainframes
- **GDPS** : Disaster Recovery automatique
- **Basculement** en quelques secondes

#### 3. Sécurité intégrée

| Niveau | Protection |
|--------|------------|
| **Matériel** | Chiffrement transparent (CP Assist) |
| **Système** | RACF, contrôle d'accès granulaire |
| **Réseau** | Isolation VLAN, chiffrement TLS |
| **Données** | Pervasive Encryption (tout chiffré) |
| **Certification** | Common Criteria EAL5+ |

#### 4. Virtualisation native

```
┌─────────────────────────────────────────────────────────┐
│                    HYPERVISEUR PR/SM                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   ┌───────────┐  ┌───────────┐  ┌───────────┐          │
│   │  LPAR 1   │  │  LPAR 2   │  │  LPAR 3   │          │
│   │   z/OS    │  │   z/OS    │  │  Linux    │          │
│   │  (Prod)   │  │  (Test)   │  │  on Z     │          │
│   ├───────────┤  ├───────────┤  ├───────────┤          │
│   │ 4 CPU     │  │ 2 CPU     │  │ 8 CPU     │          │
│   │ 64 Go RAM │  │ 32 Go RAM │  │ 128 Go RAM│          │
│   └───────────┘  └───────────┘  └───────────┘          │
│                                                         │
│   ┌───────────┐  ┌───────────┐                         │
│   │  LPAR 4   │  │  LPAR 5   │   ... jusqu'à 85 LPAR  │
│   │  z/VM     │  │  zCX      │                         │
│   │           │  │ (Docker)  │                         │
│   └───────────┘  └───────────┘                         │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

- **LPAR** : Logical Partition (partition logique)
- **PR/SM** : Hyperviseur matériel
- **z/VM** : Hyperviseur logiciel (milliers de VMs)

#### 5. Efficacité énergétique

| Métrique | Mainframe | Équivalent distribué |
|----------|-----------|----------------------|
| **Espace** | 1 rack | 50-100 serveurs |
| **Énergie** | 1x | 10-20x |
| **Refroidissement** | Optimisé | Intensif |
| **Empreinte CO2** | Réduite | Plus importante |

#### 6. Gestion des workloads (WLM)

```
┌─────────────────────────────────────────────────────────┐
│              WORKLOAD MANAGER (WLM)                      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   Priorité HAUTE          Priorité MOYENNE              │
│   ┌───────────────┐       ┌───────────────┐            │
│   │ Transactions  │       │    Batch       │            │
│   │   en ligne    │       │    Reports     │            │
│   │   (CICS)      │       │               │            │
│   └───────────────┘       └───────────────┘            │
│                                                         │
│   Priorité BASSE          Background                    │
│   ┌───────────────┐       ┌───────────────┐            │
│   │    Tests      │       │  Maintenance  │            │
│   │               │       │               │            │
│   └───────────────┘       └───────────────┘            │
│                                                         │
│   WLM ajuste dynamiquement les ressources CPU/Mémoire  │
│   selon les objectifs de service définis               │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

## I-5 Architecture générale du système Z/OS

### Vue d'ensemble

```
┌─────────────────────────────────────────────────────────────────┐
│                        UTILISATEURS                              │
├──────────────┬──────────────┬──────────────┬───────────────────┤
│   TSO/ISPF   │    CICS      │     IMS      │    Batch (JCL)    │
│  (Interactif)│ (Transact.)  │  (Base/Trans)│    (Différé)      │
└──────────────┴──────────────┴──────────────┴───────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         z/OS                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    SOUS-SYSTÈMES                         │   │
│   ├─────────────┬─────────────┬─────────────┬───────────────┤   │
│   │    JES2     │    VTAM     │    RACF     │     SMS       │   │
│   │  (Travaux)  │  (Réseau)   │ (Sécurité)  │  (Stockage)   │   │
│   └─────────────┴─────────────┴─────────────┴───────────────┘   │
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    NOYAU z/OS                            │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │  • Gestion mémoire (Real/Virtual)                       │   │
│   │  • Gestion processus (TCB, SRB)                         │   │
│   │  • Gestion E/S (IOS)                                    │   │
│   │  • Recovery/Termination Manager (RTM)                   │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    RESSOURCES PHYSIQUES                          │
├─────────────┬─────────────┬─────────────┬───────────────────────┤
│  Processeurs│   Mémoire   │    DASD     │    Bandes / Réseau    │
│    (CP)     │   (RAM)     │  (Disques)  │                       │
└─────────────┴─────────────┴─────────────┴───────────────────────┘
```

### Composants principaux

#### Sous-systèmes essentiels

| Sous-système | Nom complet | Fonction |
|--------------|-------------|----------|
| **JES2/JES3** | Job Entry Subsystem | Gestion des travaux batch |
| **VTAM** | Virtual Telecom Access Method | Communications réseau |
| **RACF** | Resource Access Control Facility | Sécurité et autorisations |
| **SMS** | Storage Management Subsystem | Gestion du stockage |
| **WLM** | Workload Manager | Gestion des ressources |
| **RMF** | Resource Measurement Facility | Monitoring performance |

#### Méthodes d'accès aux données

| Méthode | Type | Utilisation |
|---------|------|-------------|
| **VSAM** | Indexé, séquentiel, relatif | Fichiers structurés |
| **QSAM** | Séquentiel | Fichiers séquentiels simples |
| **BSAM** | Bloc | Accès bas niveau |
| **BDAM** | Direct | Accès direct (legacy) |

### Modes de fonctionnement

```
┌─────────────────────────────────────────────────────────────────┐
│                    MODES DE TRAITEMENT                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────┐       ┌─────────────────────┐         │
│   │      INTERACTIF     │       │        BATCH        │         │
│   ├─────────────────────┤       ├─────────────────────┤         │
│   │                     │       │                     │         │
│   │  • TSO/ISPF         │       │  • JCL (Job Control)│         │
│   │  • CICS             │       │  • Exécution différée│        │
│   │  • IMS/TM           │       │  • Traitements de   │         │
│   │                     │       │    masse            │         │
│   │  Temps réel         │       │  File d'attente     │         │
│   │  Réponse immédiate  │       │  Planifié           │         │
│   │                     │       │                     │         │
│   └─────────────────────┘       └─────────────────────┘         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## I-6 Terminologie IBM

### Vocabulaire essentiel

| Terme IBM | Signification | Équivalent courant |
|-----------|---------------|-------------------|
| **DASD** | Direct Access Storage Device | Disque dur |
| **LPAR** | Logical Partition | Machine virtuelle |
| **SYSRES** | System Residence | Volume système |
| **IPL** | Initial Program Load | Boot / Démarrage |
| **ABEND** | Abnormal End | Crash / Erreur fatale |
| **JCL** | Job Control Language | Script de lancement |
| **PDS** | Partitioned Data Set | Bibliothèque de fichiers |
| **VOLSER** | Volume Serial | Identifiant de disque |
| **UCB** | Unit Control Block | Descripteur de périphérique |
| **SVC** | Supervisor Call | Appel système |

### Unités de mesure

| Terme | Définition |
|-------|------------|
| **MIPS** | Millions d'Instructions Par Seconde |
| **MSU** | Million Service Units (capacité) |
| **EXCP** | Execute Channel Program (E/S) |
| **Track** | Piste (unité de stockage DASD) |
| **Cylinder** | Cylindre = ensemble de pistes |

### Types de datasets (fichiers)

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES DE DATASETS                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   SEQUENTIAL (PS)           PARTITIONED (PDS/PDSE)              │
│   ┌───────────────┐         ┌───────────────────────┐           │
│   │ Enreg 1       │         │  Directory            │           │
│   │ Enreg 2       │         │  ├── MEMBER1          │           │
│   │ Enreg 3       │         │  ├── MEMBER2          │           │
│   │ ...           │         │  └── MEMBER3          │           │
│   └───────────────┘         └───────────────────────┘           │
│                                                                  │
│   VSAM                                                          │
│   ┌───────────────────────────────────────────────┐             │
│   │  ESDS (Entry Sequenced) - Séquentiel          │             │
│   │  KSDS (Key Sequenced)   - Indexé              │             │
│   │  RRDS (Relative Record) - Relatif             │             │
│   │  LDS  (Linear)          - Linéaire            │             │
│   └───────────────────────────────────────────────┘             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Convention de nommage des datasets

```
Format : HLQ.QUALIFIER2.QUALIFIER3...QUALIFIERn

Exemples :
┌─────────────────────────────────────────────────────────────────┐
│  SYS1.PROCLIB          - Bibliothèque système                   │
│  USER01.COBOL.SOURCE   - Sources COBOL utilisateur             │
│  PROD.PAYROLL.MASTER   - Fichier maître paie production        │
│  TEST.DATA.BACKUP      - Sauvegarde données test               │
└─────────────────────────────────────────────────────────────────┘

Règles :
• HLQ (High Level Qualifier) = 1-8 caractères
• Chaque qualificateur = 1-8 caractères
• Maximum 44 caractères au total
• Caractères autorisés : A-Z, 0-9, @, #, $
```

### Codes retour standards

| Code | Signification |
|------|---------------|
| **0** | Succès, aucune erreur |
| **4** | Warning (avertissement) |
| **8** | Erreur, mais traitement continué |
| **12** | Erreur grave |
| **16** | Erreur critique |
| **>16** | Erreur système (ABEND) |

### Codes ABEND courants

| Code | Type | Cause probable |
|------|------|----------------|
| **S0C1** | System | Opération invalide |
| **S0C4** | System | Violation de protection mémoire |
| **S0C7** | System | Données non numériques |
| **S0CB** | System | Division par zéro |
| **S013** | System | Erreur d'ouverture fichier |
| **S322** | System | Time limit dépassé |
| **S806** | System | Module non trouvé |
| **S913** | System | Problème de sécurité RACF |
| **U0001-U4095** | User | ABEND applicatif (programmeur) |

---

## Synthèse

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLÉS DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ✓ Mainframe = 60+ ans d'évolution, compatibilité préservée    │
│                                                                  │
│  ✓ IBM Z = Cœur du cloud hybride moderne                       │
│                                                                  │
│  ✓ Points forts :                                               │
│    • Disponibilité 99,999%                                      │
│    • Sécurité certifiée EAL5+                                   │
│    • Performance millions transactions/sec                      │
│    • Virtualisation native (LPAR)                               │
│    • Efficacité énergétique                                     │
│                                                                  │
│  ✓ z/OS = Système d'exploitation moderne                       │
│    • Sous-systèmes spécialisés (JES, VTAM, RACF, SMS)          │
│    • VSAM pour gestion des fichiers                             │
│    • Modes interactif (TSO/CICS) et batch (JCL)                │
│                                                                  │
│  ✓ Terminologie spécifique IBM                                  │
│    • DASD, LPAR, IPL, ABEND, PDS, VOLSER...                    │
│    • Conventions de nommage strictes                            │
│    • Codes retour standardisés                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Fonctionnement Z/OS](02-fonctionnement-zos.md) |
