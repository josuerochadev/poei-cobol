# Chapitre II - Fonctionnement de Z/OS

## Introduction

Ce chapitre présente les mécanismes fondamentaux du système d'exploitation z/OS :
1. Organisation et gestion de la mémoire
2. L'espace adresse MVS
3. Gestion des tâches
4. Gestion des travaux
5. Gestion des données

---

## II-1 Organisation et gestion de la mémoire

### II-1-1 Différence entre les deux types de mémoires : CPC et auxiliaire

Le système z/OS utilise deux types de stockage distincts :

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES DE MÉMOIRE Z/OS                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   MÉMOIRE CPC (Central Processor Complex)                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Mémoire vive (RAM) du processeur                    │   │
│   │  • Accès très rapide (nanosecondes)                    │   │
│   │  • Volatile (perdue à l'arrêt)                         │   │
│   │  • Coûteuse                                            │   │
│   │  • Capacité : jusqu'à 48 To (z16)                      │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   MÉMOIRE AUXILIAIRE                                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Disques DASD (Direct Access Storage Device)         │   │
│   │  • Accès plus lent (millisecondes)                     │   │
│   │  • Persistante (conservée à l'arrêt)                   │   │
│   │  • Moins coûteuse par Go                               │   │
│   │  • Capacité : quasi illimitée                          │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Tableau comparatif

| Caractéristique | Mémoire CPC | Mémoire auxiliaire |
|-----------------|-------------|-------------------|
| **Type physique** | RAM | Disques DASD |
| **Vitesse d'accès** | Nanosecondes | Millisecondes |
| **Persistance** | Volatile | Permanente |
| **Coût/Go** | Élevé | Modéré |
| **Usage** | Exécution programmes | Pagination, swap |

---

### II-1-2 Notion de mémoire virtuelle

#### Principe fondamental

La **mémoire virtuelle** permet à chaque programme de disposer d'un espace d'adressage **indépendant** et **plus grand** que la mémoire physique disponible.

```
┌─────────────────────────────────────────────────────────────────┐
│                    MÉMOIRE VIRTUELLE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Programme A voit :         Programme B voit :                 │
│   ┌───────────────┐          ┌───────────────┐                  │
│   │ Adresse 0     │          │ Adresse 0     │                  │
│   │ Adresse 1000  │          │ Adresse 1000  │                  │
│   │ Adresse 2000  │          │ Adresse 2000  │                  │
│   │ ...           │          │ ...           │                  │
│   │ Adresse MAX   │          │ Adresse MAX   │                  │
│   └───────────────┘          └───────────────┘                  │
│          │                          │                           │
│          │    TRANSLATION           │                           │
│          └──────────┬───────────────┘                           │
│                     ▼                                           │
│          ┌─────────────────────┐                                │
│          │  MÉMOIRE PHYSIQUE   │                                │
│          │      (CPC)          │                                │
│          │   + AUXILIAIRE      │                                │
│          └─────────────────────┘                                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Avantages de la mémoire virtuelle

| Avantage | Description |
|----------|-------------|
| **Isolation** | Chaque programme a son propre espace, protégé des autres |
| **Simplicité** | Le programmeur voit un espace linéaire continu |
| **Flexibilité** | Espace virtuel > mémoire physique |
| **Protection** | Accès mémoire contrôlé par le système |
| **Partage** | Code commun partageable entre programmes |

---

### II-1-3 Mécanisme de gestion de la mémoire virtuelle

#### Organisation en pages et frames

```
┌─────────────────────────────────────────────────────────────────┐
│              PAGINATION : PAGES ET FRAMES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   MÉMOIRE VIRTUELLE              MÉMOIRE PHYSIQUE (CPC)         │
│   (Pages de 4 Ko)                (Frames de 4 Ko)               │
│                                                                  │
│   ┌─────────────┐                ┌─────────────┐                │
│   │   Page 0    │ ──────────────►│  Frame 5    │                │
│   ├─────────────┤                ├─────────────┤                │
│   │   Page 1    │ ──────────────►│  Frame 12   │                │
│   ├─────────────┤                ├─────────────┤                │
│   │   Page 2    │ ───┐           │  Frame 3    │                │
│   ├─────────────┤    │           ├─────────────┤                │
│   │   Page 3    │ ─┐ │           │  Frame 8    │                │
│   ├─────────────┤  │ │           ├─────────────┤                │
│   │   Page 4    │  │ │  ┌───────►│  Frame 21   │                │
│   └─────────────┘  │ │  │        └─────────────┘                │
│                    │ │  │                                       │
│                    │ │  │        MÉMOIRE AUXILIAIRE             │
│                    │ │  │        (Pages sur disque)             │
│                    │ │  │        ┌─────────────┐                │
│                    │ └──┼───────►│  Slot 45    │                │
│                    │    │        ├─────────────┤                │
│                    └────┼───────►│  Slot 67    │                │
│                         │        └─────────────┘                │
│                         │                                       │
│   Page 4 = en mémoire CPC                                       │
│   Pages 2,3 = sur disque (swappées)                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Tailles de pages z/OS

| Type de page | Taille | Utilisation |
|--------------|--------|-------------|
| **Page standard** | 4 Ko (4096 octets) | Usage général |
| **Large page** | 1 Mo | Applications haute performance |
| **2G page** | 2 Go | Bases de données, cache |

---

### II-1-4 Translation de l'adresse dynamique (DAT)

#### Dynamic Address Translation

Le processeur z/Architecture dispose d'un mécanisme matériel appelé **DAT** (Dynamic Address Translation) qui convertit les adresses virtuelles en adresses réelles.

```
┌─────────────────────────────────────────────────────────────────┐
│                    DAT - TRANSLATION D'ADRESSE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ADRESSE VIRTUELLE (64 bits)                                   │
│   ┌────────┬────────┬────────┬────────┬──────────────┐          │
│   │ Region │ Region │ Segment│  Page  │   Offset     │          │
│   │ 1st Idx│ 2nd Idx│  Index │  Index │  (12 bits)   │          │
│   └────┬───┴────┬───┴────┬───┴────┬───┴──────┬───────┘          │
│        │        │        │        │          │                   │
│        ▼        ▼        ▼        ▼          │                   │
│   ┌────────┐┌────────┐┌────────┐┌────────┐   │                   │
│   │Region  ││Region  ││Segment ││ Page   │   │                   │
│   │1st Tab ││2nd Tab ││ Table  ││ Table  │   │                   │
│   └────┬───┘└────┬───┘└────┬───┘└────┬───┘   │                   │
│        │        │        │        │          │                   │
│        └────────┴────────┴────────┘          │                   │
│                          │                   │                   │
│                          ▼                   │                   │
│                    ┌──────────┐              │                   │
│                    │  Frame   │◄─────────────┘                   │
│                    │  Address │    (Offset conservé)             │
│                    └──────────┘                                  │
│                          │                                       │
│                          ▼                                       │
│                  ADRESSE RÉELLE (CPC)                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Tables de translation

| Table | Fonction | Entrées |
|-------|----------|---------|
| **Region First Table** | Index région niveau 1 | 2048 |
| **Region Second Table** | Index région niveau 2 | 2048 |
| **Region Third Table** | Index région niveau 3 | 2048 |
| **Segment Table** | Index de segment | 2048 |
| **Page Table** | Mapping page → frame | 256 |

#### TLB - Translation Lookaside Buffer

Pour accélérer la translation, le processeur maintient un **cache** des translations récentes :

```
┌─────────────────────────────────────────────────────────────────┐
│                         TLB CACHE                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────┬─────────────────┐                         │
│   │ Adresse virtuelle│ Adresse réelle  │                         │
│   ├─────────────────┼─────────────────┤                         │
│   │ 0x00001000      │ 0x5A000000      │ ◄── Hit = rapide        │
│   ├─────────────────┼─────────────────┤                         │
│   │ 0x00002000      │ 0x3B000000      │                         │
│   ├─────────────────┼─────────────────┤                         │
│   │ 0x00005000      │ 0x12000000      │                         │
│   └─────────────────┴─────────────────┘                         │
│                                                                  │
│   Miss TLB = parcours des tables (plus lent)                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### II-1-5 Espace d'adressage

#### Définition

Un **espace d'adressage** (Address Space) est l'environnement virtuel complet d'une tâche z/OS. Chaque travail (job), utilisateur TSO, ou région CICS possède son propre espace d'adressage.

```
┌─────────────────────────────────────────────────────────────────┐
│                    ESPACE D'ADRESSAGE 64 BITS                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   16 Exaoctets (2^64)                                           │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │                    BAR (64 bits)                        │   │
│   │              High Virtual Storage                       │   │
│   │         (Programmes 64 bits, données)                   │   │
│   │                                                         │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │ ─ ─ ─ ─ ─ ─ ─ ─ 2 Go (Bar) ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │                                                         │   │
│   │              Extended Private (31 bits)                 │   │
│   │                  (16 Mo - 2 Go)                         │   │
│   │                                                         │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │ ─ ─ ─ ─ ─ ─ ─ ─ 16 Mo (Line) ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │              Private Area (24 bits)                     │   │
│   │                  (0 - 16 Mo)                            │   │
│   │           Programmes 24 bits legacy                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Zones de l'espace d'adressage (31 bits)

```
┌─────────────────────────────────────────────────────────────────┐
│              STRUCTURE DÉTAILLÉE (vue 31 bits)                   │
├─────────────────────────────────────────────────────────────────┤
│   2 Go                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │          Extended Common Storage Area (ECSA)            │   │
│   │               (Partagé entre espaces)                   │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Extended System Queue Area (ESQA)              │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Extended Private Area                          │   │
│   │               (Programmes utilisateur)                  │   │
│   │          Extended User Region                           │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Extended Link Pack Area (ELPA)                 │   │
│   │               (Modules réentrants partagés)             │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   16 Mo ─────────────────── THE LINE ─────────────────────────  │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Common Storage Area (CSA)                      │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          System Queue Area (SQA)                        │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Private Area (User Region)                     │   │
│   │               (Code + données programme)                │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Link Pack Area (LPA)                           │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          Nucleus (Noyau z/OS)                           │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │          PSA (Prefixed Save Area) - 4 Ko                │   │
│   0 └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Description des zones

| Zone | Contenu | Partage |
|------|---------|---------|
| **PSA** | Zone préfixée par processeur | Unique/CPU |
| **Nucleus** | Noyau z/OS, routines système | Commun |
| **LPA/ELPA** | Modules réentrants (SVC, etc.) | Commun |
| **SQA/ESQA** | Blocs de contrôle système | Commun |
| **CSA/ECSA** | Données partagées applications | Commun |
| **Private** | Code et données du programme | Privé |

---

### II-1-6 Mémoire CPC (Central Storage)

#### Caractéristiques

```
┌─────────────────────────────────────────────────────────────────┐
│                    MÉMOIRE CPC (REAL STORAGE)                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    FRAMES LIBRES                        │   │
│   │   ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐           │   │
│   │   │Free│ │Free│ │Free│ │Free│ │Free│ │Free│           │   │
│   │   └────┘ └────┘ └────┘ └────┘ └────┘ └────┘           │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    FRAMES UTILISÉES                     │   │
│   │   ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐           │   │
│   │   │Pg 1│ │Pg 5│ │Pg 2│ │Pg 8│ │Pg 3│ │Pg 9│           │   │
│   │   │Job1│ │Job2│ │Job1│ │TSO │ │CICS│ │Job2│           │   │
│   │   └────┘ └────┘ └────┘ └────┘ └────┘ └────┘           │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                    FRAMES FIXÉES                        │   │
│   │   ┌────┐ ┌────┐ ┌────┐                                 │   │
│   │   │Nuc │ │I/O │ │Lock│  ◄── Ne peuvent pas être       │   │
│   │   │    │ │Buff│ │    │      paginées (page-fixed)     │   │
│   │   └────┘ └────┘ └────┘                                 │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Gestion des frames

| État | Description |
|------|-------------|
| **Free** | Frame disponible pour allocation |
| **In use** | Frame contenant une page active |
| **Fixed** | Frame verrouillée (non paginable) |
| **LFAREA** | Large Frame Area (pages 1Mo/2Go) |

---

### II-1-7 Stockage auxiliaire (Auxiliary Storage)

#### Datasets de pagination

```
┌─────────────────────────────────────────────────────────────────┐
│                    STOCKAGE AUXILIAIRE                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   PAGE DATASETS                    SWAP DATASETS                │
│   ┌─────────────────────┐         ┌─────────────────────┐       │
│   │ SYS1.PAGE00         │         │ SYS1.SWAP00         │       │
│   │ ┌─────┬─────┬─────┐ │         │ ┌─────────────────┐ │       │
│   │ │Slot │Slot │Slot │ │         │ │   Espace        │ │       │
│   │ │ 1   │ 2   │ 3   │ │         │ │   adressage     │ │       │
│   │ └─────┴─────┴─────┘ │         │ │   complet       │ │       │
│   │ Pages individuelles │         │ └─────────────────┘ │       │
│   └─────────────────────┘         └─────────────────────┘       │
│                                                                  │
│   • PAGE DS : Pages individuelles                               │
│   • SWAP DS : Espaces d'adressage complets (swap-out)          │
│                                                                  │
│   Configuration typique :                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ LOCAL PAGE DS    : Pages privées                        │   │
│   │ COMMON PAGE DS   : Pages communes (CSA, LPA)            │   │
│   │ PLPA PAGE DS     : Pageable LPA                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### II-1-8 Pagination entre mémoire CPC et auxiliaire

#### Mécanisme de Page-In / Page-Out

```
┌─────────────────────────────────────────────────────────────────┐
│                    PAGINATION (PAGING)                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   PAGE-IN (Chargement)                                          │
│   ─────────────────────                                         │
│                                                                  │
│   ┌─────────────┐         ┌─────────────┐                       │
│   │   DASD      │         │    CPC      │                       │
│   │ (Page DS)   │ ──────► │  (Frame)    │                       │
│   │             │         │             │                       │
│   └─────────────┘         └─────────────┘                       │
│                                                                  │
│   Déclenché par : Page Fault (accès à page non résidente)      │
│                                                                  │
│   ─────────────────────────────────────────────────────────────  │
│                                                                  │
│   PAGE-OUT (Déchargement)                                       │
│   ───────────────────────                                       │
│                                                                  │
│   ┌─────────────┐         ┌─────────────┐                       │
│   │    CPC      │         │   DASD      │                       │
│   │  (Frame)    │ ──────► │ (Page DS)   │                       │
│   │             │         │             │                       │
│   └─────────────┘         └─────────────┘                       │
│                                                                  │
│   Déclenché par : Besoin de frames libres (stealing)           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Algorithme de remplacement (Page Stealing)

```
┌─────────────────────────────────────────────────────────────────┐
│                    ALGORITHME LRU                                │
│            (Least Recently Used / Moins Récemment Utilisé)       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   1. Système a besoin d'une frame libre                         │
│                                                                  │
│   2. ASM (Auxiliary Storage Manager) cherche une page           │
│      "victime" parmi les moins utilisées                        │
│                                                                  │
│   3. Page Reference Bit :                                       │
│      ┌────────────────────────────────────────┐                 │
│      │ Bit = 1 : Page accédée récemment       │                 │
│      │           → Reset bit, passer à autre  │                 │
│      │ Bit = 0 : Page non accédée             │                 │
│      │           → Candidate au stealing      │                 │
│      └────────────────────────────────────────┘                 │
│                                                                  │
│   4. Page modifiée ?                                            │
│      ┌────────────────────────────────────────┐                 │
│      │ Non : Frame libérée directement        │                 │
│      │ Oui : Page-out vers auxiliaire d'abord │                 │
│      └────────────────────────────────────────┘                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### II-1-9 Terminologie du stockage virtuel

| Terme | Définition |
|-------|------------|
| **Virtual Storage** | Espace d'adressage vu par le programme |
| **Real Storage** | Mémoire physique CPC |
| **Auxiliary Storage** | Stockage sur disque (paging) |
| **Page** | Unité de mémoire virtuelle (4 Ko) |
| **Frame** | Unité de mémoire réelle (4 Ko) |
| **Slot** | Emplacement page sur auxiliaire |
| **Page Fault** | Accès à page non résidente en CPC |
| **Page-In** | Chargement page du disque vers CPC |
| **Page-Out** | Écriture page du CPC vers disque |
| **Page Stealing** | Récupération de frames occupées |
| **Page-Fixed** | Page verrouillée en CPC (non paginable) |
| **Swapping** | Déplacement espace d'adressage entier |
| **Working Set** | Ensemble des pages actives d'un programme |
| **DAT** | Dynamic Address Translation |
| **TLB** | Translation Lookaside Buffer (cache DAT) |

---

## II-2 Gestion des tâches

### Concepts fondamentaux

#### Unités de travail z/OS

```
┌─────────────────────────────────────────────────────────────────┐
│                    HIÉRARCHIE DES TÂCHES                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ADDRESS SPACE (Espace d'adressage)                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │   ┌─────────────┐   ┌─────────────┐   ┌─────────────┐  │   │
│   │   │    TCB 1    │   │    TCB 2    │   │    TCB 3    │  │   │
│   │   │   (Tâche)   │   │   (Tâche)   │   │  (Subtask)  │  │   │
│   │   └─────────────┘   └─────────────┘   └─────────────┘  │   │
│   │                                                         │   │
│   │   ┌─────────────┐   ┌─────────────┐                    │   │
│   │   │    SRB 1    │   │    SRB 2    │                    │   │
│   │   │  (Service)  │   │  (Service)  │                    │   │
│   │   └─────────────┘   └─────────────┘                    │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### TCB vs SRB

| Aspect | TCB (Task Control Block) | SRB (Service Request Block) |
|--------|--------------------------|----------------------------|
| **Nature** | Tâche utilisateur | Routine système |
| **Interruptible** | Oui | Non |
| **Dispatchable** | Par priorité | Priorité élevée |
| **Wait possible** | Oui (I/O, events) | Non |
| **Exemple** | Programme COBOL | Routine I/O système |

### États d'une tâche

```
┌─────────────────────────────────────────────────────────────────┐
│                    CYCLE DE VIE D'UNE TÂCHE                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│                    ┌─────────────┐                               │
│                    │   CREATED   │                               │
│                    │  (Créée)    │                               │
│                    └──────┬──────┘                               │
│                           │ ATTACH                               │
│                           ▼                                      │
│                    ┌─────────────┐                               │
│       ┌───────────►│    READY    │◄───────────┐                 │
│       │            │  (Prête)    │            │                 │
│       │            └──────┬──────┘            │                 │
│       │                   │ DISPATCH          │ I/O Complete    │
│       │                   ▼                   │ Event Post      │
│       │            ┌─────────────┐            │                 │
│       │            │   RUNNING   │            │                 │
│       │            │ (Exécution) │            │                 │
│       │            └──────┬──────┘            │                 │
│       │                   │                   │                 │
│       │     ┌─────────────┼─────────────┐     │                 │
│       │     │             │             │     │                 │
│       │     ▼             ▼             ▼     │                 │
│       │ Preemption    I/O Wait     Event Wait │                 │
│       │     │             │             │     │                 │
│       │     │      ┌──────┴──────┐      │     │                 │
│       └─────┘      │   WAITING   │      └─────┘                 │
│                    │  (Attente)  │                               │
│                    └─────────────┘                               │
│                           │                                      │
│                           │ DETACH / ABEND                       │
│                           ▼                                      │
│                    ┌─────────────┐                               │
│                    │ TERMINATED  │                               │
│                    │  (Finie)    │                               │
│                    └─────────────┘                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Dispatcher et priorités

```
┌─────────────────────────────────────────────────────────────────┐
│                         DISPATCHER                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Files de priorité (0 = plus haute, 255 = plus basse)          │
│                                                                  │
│   Priorité 0-15  : SYSTEM (z/OS, JES, VTAM...)                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ █████████████████████████████████████████████████████   │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Priorité 16-31 : SUBSYSTEM (CICS, DB2, IMS...)               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ ████████████████████████████████████████████            │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Priorité 32-127 : TSO, Online                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ ███████████████████████████                             │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Priorité 128-255 : BATCH                                      │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ █████████████████                                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   WLM ajuste dynamiquement selon objectifs de service          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-3 Gestion des travaux (Jobs)

### Architecture JES2/JES3

```
┌─────────────────────────────────────────────────────────────────┐
│                    JOB ENTRY SUBSYSTEM                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────┐                                               │
│   │   SUBMIT    │  Soumission JCL                               │
│   │  (Entrée)   │                                               │
│   └──────┬──────┘                                               │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │  CONVERSION │  Analyse syntaxe JCL                          │
│   │             │  Création JCT (Job Control Table)             │
│   └──────┬──────┘                                               │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │   INPUT     │  File d'attente d'entrée                      │
│   │   QUEUE     │  (Classé par priorité/classe)                 │
│   └──────┬──────┘                                               │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │  EXECUTION  │  Initiator sélectionne et exécute             │
│   │             │  (Allocation ressources)                      │
│   └──────┬──────┘                                               │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │   OUTPUT    │  File d'attente de sortie                     │
│   │   QUEUE     │  (SYSOUT, impressions)                        │
│   └──────┬──────┘                                               │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │   PURGE     │  Nettoyage après consultation/impression      │
│   │             │                                               │
│   └─────────────┘                                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Phases d'un job

| Phase | Description | Responsable |
|-------|-------------|-------------|
| **INPUT** | Lecture et conversion JCL | JES Reader |
| **CONVERSION** | Validation syntaxe, création JCT | Converter |
| **SETUP** | Allocation volumes, datasets | Allocation |
| **EXECUTION** | Exécution des steps | Initiator |
| **OUTPUT** | Écriture SYSOUT | Output writer |
| **PURGE** | Libération ressources | Purge |

### Classes de jobs

```
┌─────────────────────────────────────────────────────────────────┐
│                    CLASSES D'EXÉCUTION                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   CLASS=A    Travaux courts, haute priorité                     │
│   CLASS=B    Travaux moyens                                     │
│   CLASS=C    Travaux longs, basse priorité                      │
│   CLASS=S    Travaux système                                    │
│   CLASS=T    Travaux de test                                    │
│                                                                  │
│   Exemple JCL :                                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ //MYJOB  JOB (ACCT),'NOM',CLASS=A,MSGCLASS=X            │   │
│   │ //STEP1  EXEC PGM=MYPROG                                │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   MSGCLASS = Classe de sortie des messages (SYSOUT)            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Initiators

```
┌─────────────────────────────────────────────────────────────────┐
│                       INITIATORS                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Initiator = Espace d'adressage dédié à l'exécution batch     │
│                                                                  │
│   ┌─────────────┐  ┌─────────────┐  ┌─────────────┐            │
│   │ INITIATOR 1 │  │ INITIATOR 2 │  │ INITIATOR 3 │            │
│   │ CLASS=A,B   │  │ CLASS=A,B,C │  │ CLASS=C,D   │            │
│   │             │  │             │  │             │            │
│   │ ┌─────────┐ │  │ ┌─────────┐ │  │ ┌─────────┐ │            │
│   │ │ JOB123  │ │  │ │ JOB456  │ │  │ │ JOB789  │ │            │
│   │ │(Running)│ │  │ │(Running)│ │  │ │(Running)│ │            │
│   │ └─────────┘ │  │ └─────────┘ │  │ └─────────┘ │            │
│   └─────────────┘  └─────────────┘  └─────────────┘            │
│                                                                  │
│   Chaque initiator :                                            │
│   • Sélectionne jobs selon ses classes                         │
│   • Exécute un job à la fois                                   │
│   • Gère l'allocation des ressources                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-4 Gestion des données

### Organisation des données z/OS

```
┌─────────────────────────────────────────────────────────────────┐
│                    HIÉRARCHIE DU STOCKAGE                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   VOLUME (DASD)                                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  VOLSER: PROD01                                         │   │
│   │  ┌─────────────────────────────────────────────────┐    │   │
│   │  │  VTOC (Volume Table of Contents)                │    │   │
│   │  │  ┌───────────────────────────────────────────┐  │    │   │
│   │  │  │ DSN=SYS1.PROCLIB, Extents, RECFM, etc.   │  │    │   │
│   │  │  │ DSN=USER.DATA.FILE, Extents, RECFM, etc. │  │    │   │
│   │  │  │ ...                                       │  │    │   │
│   │  │  └───────────────────────────────────────────┘  │    │   │
│   │  └─────────────────────────────────────────────────┘    │   │
│   │                                                         │   │
│   │  ┌─────────────────────────────────────────────────┐    │   │
│   │  │  DATA SETS (Fichiers)                           │    │   │
│   │  │  ┌─────────┐ ┌─────────┐ ┌─────────┐           │    │   │
│   │  │  │ DS 1    │ │ DS 2    │ │ DS 3    │           │    │   │
│   │  │  │(Extent1)│ │(Extent1)│ │(Extent1)│           │    │   │
│   │  │  │(Extent2)│ │         │ │(Extent2)│           │    │   │
│   │  │  └─────────┘ └─────────┘ └─────────┘           │    │   │
│   │  └─────────────────────────────────────────────────┘    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de datasets

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES DE DATASETS                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   SEQUENTIAL (PS - Physical Sequential)                         │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ Record 1 │ Record 2 │ Record 3 │ ... │ Record N │ EOF   │   │
│   └─────────────────────────────────────────────────────────┘   │
│   Accès : Séquentiel uniquement                                 │
│                                                                  │
│   ─────────────────────────────────────────────────────────────  │
│                                                                  │
│   PARTITIONED (PDS/PDSE)                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ DIRECTORY │ MEMBER1 │ MEMBER2 │ MEMBER3 │ ...           │   │
│   └─────────────────────────────────────────────────────────┘   │
│   Accès : Direct par nom de membre                              │
│   Usage : Bibliothèques (sources, load modules, JCL)           │
│                                                                  │
│   ─────────────────────────────────────────────────────────────  │
│                                                                  │
│   VSAM (Virtual Storage Access Method)                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ KSDS : Key Sequenced    (Indexé sur clé)               │   │
│   │ ESDS : Entry Sequenced  (Séquentiel)                   │   │
│   │ RRDS : Relative Record  (Numéro d'enregistrement)      │   │
│   │ LDS  : Linear           (Flux d'octets)                │   │
│   └─────────────────────────────────────────────────────────┘   │
│   Accès : Séquentiel, Direct, ou les deux                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Attributs des datasets

| Attribut | Description | Valeurs typiques |
|----------|-------------|------------------|
| **DSORG** | Organisation | PS, PO, VS |
| **RECFM** | Format enregistrement | F, FB, V, VB, U |
| **LRECL** | Longueur enregistrement | 80, 133, 32760 |
| **BLKSIZE** | Taille bloc | Multiple de LRECL |
| **SPACE** | Allocation espace | (TRK,10,5), (CYL,1,1) |
| **DSNAME** | Nom du dataset | HLQ.QUALIFIER.NAME |

### Format des enregistrements (RECFM)

```
┌─────────────────────────────────────────────────────────────────┐
│                    FORMATS D'ENREGISTREMENT                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   F (Fixed) - Longueur fixe                                     │
│   ┌──────────┐┌──────────┐┌──────────┐                          │
│   │ 80 bytes ││ 80 bytes ││ 80 bytes │                          │
│   └──────────┘└──────────┘└──────────┘                          │
│                                                                  │
│   FB (Fixed Blocked) - Fixe bloqué                              │
│   ┌────────────────────────────────────┐                        │
│   │ Bloc (ex: 800 bytes)               │                        │
│   │ ┌────────┐┌────────┐...┌────────┐  │                        │
│   │ │ Rec 1  ││ Rec 2  │   │ Rec 10 │  │  (10 x 80 = 800)      │
│   │ └────────┘└────────┘   └────────┘  │                        │
│   └────────────────────────────────────┘                        │
│                                                                  │
│   V (Variable) - Longueur variable                              │
│   ┌────┬────────┐┌────┬──────────────┐┌────┬────┐               │
│   │ RDW│ Data   ││ RDW│ Data         ││ RDW│Data│               │
│   │ 4B │ 50B    ││ 4B │ 120B         ││ 4B │20B │               │
│   └────┴────────┘└────┴──────────────┘└────┴────┘               │
│   RDW = Record Descriptor Word (longueur)                       │
│                                                                  │
│   VB (Variable Blocked) - Variable bloqué                       │
│   ┌────────────────────────────────────────────────────┐        │
│   │ BDW │ RDW │ Data │ RDW │ Data │ RDW │ Data │       │        │
│   │ 4B  │ 4B  │ ...  │ 4B  │ ...  │ 4B  │ ...  │       │        │
│   └────────────────────────────────────────────────────┘        │
│   BDW = Block Descriptor Word                                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Catalog et SMS

```
┌─────────────────────────────────────────────────────────────────┐
│                    SYSTÈME DE CATALOGAGE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   MASTER CATALOG                                                │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │ SYS1.*        → Volume SYSRES                           │   │
│   │ USER.*        → USER CATALOG (alias)                    │   │
│   │ PROD.*        → PROD CATALOG (alias)                    │   │
│   └─────────────────────────────────────────────────────────┘   │
│          │                    │                                  │
│          ▼                    ▼                                  │
│   ┌─────────────┐      ┌─────────────┐                          │
│   │ USER CATALOG│      │ PROD CATALOG│                          │
│   │             │      │             │                          │
│   │ USER.DATA   │      │ PROD.MASTER │                          │
│   │ USER.SOURCE │      │ PROD.TRANS  │                          │
│   └─────────────┘      └─────────────┘                          │
│                                                                  │
│   SMS (Storage Management Subsystem)                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │   DATA CLASS    : Attributs logiques (RECFM, LRECL)    │   │
│   │   STORAGE CLASS : Niveau de service (performance)      │   │
│   │   MANAGEMENT CLASS : Règles de gestion (backup, migr.) │   │
│   │   STORAGE GROUP : Pool de volumes physiques            │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
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
│  ✓ MÉMOIRE                                                      │
│    • CPC (RAM) = rapide, volatile                              │
│    • Auxiliaire (DASD) = persistant, pagination                │
│    • Mémoire virtuelle = isolation + flexibilité               │
│    • DAT = translation adresse virtuelle → réelle              │
│    • Pages 4 Ko, pagination automatique                         │
│                                                                  │
│  ✓ ESPACE D'ADRESSAGE                                           │
│    • 64 bits = 16 Exaoctets théoriques                         │
│    • Zones : PSA, Nucleus, LPA, CSA, Private                   │
│    • "The Line" = 16 Mo (compatibilité 24 bits)                │
│    • "The Bar" = 2 Go (compatibilité 31 bits)                  │
│                                                                  │
│  ✓ GESTION DES TÂCHES                                           │
│    • TCB = tâche utilisateur interruptible                     │
│    • SRB = routine système prioritaire                         │
│    • Dispatcher = ordonnancement par priorité                  │
│    • WLM = ajustement dynamique                                │
│                                                                  │
│  ✓ GESTION DES TRAVAUX                                          │
│    • JES2/JES3 = gestionnaire de jobs                          │
│    • Phases : INPUT → CONVERSION → EXECUTION → OUTPUT          │
│    • Classes = priorité et ressources                          │
│    • Initiators = exécuteurs batch                             │
│                                                                  │
│  ✓ GESTION DES DONNÉES                                          │
│    • Volume DASD = VTOC + datasets                             │
│    • Types : PS (séquentiel), PO (partitionné), VSAM          │
│    • RECFM : F, FB, V, VB                                      │
│    • Catalog = index global des datasets                       │
│    • SMS = gestion automatisée du stockage                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Présentation générale](01-presentation-zos.md) | [Chapitre III - TSO](03-tso.md) |
