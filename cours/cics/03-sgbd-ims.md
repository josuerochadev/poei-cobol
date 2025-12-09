# Chapitre III - SGBD IMS

## Préambule : Les types de SGBD

Avant d'aborder IMS, il est essentiel de comprendre les trois grandes familles de Systèmes de Gestion de Bases de Données (SGBD).

### a) SGBD Hiérarchique

Le modèle **hiérarchique** organise les données en structure d'arbre :

```
┌─────────────────────────────────────────────────────────────────┐
│                    MODÈLE HIÉRARCHIQUE                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│                      ┌──────────┐                               │
│                      │  RACINE  │                               │
│                      └────┬─────┘                               │
│              ┌────────────┼────────────┐                        │
│              ▼            ▼            ▼                        │
│         ┌────────┐  ┌────────┐  ┌────────┐                     │
│         │Enfant 1│  │Enfant 2│  │Enfant 3│                     │
│         └───┬────┘  └────────┘  └───┬────┘                     │
│             ▼                       ▼                           │
│        ┌────────┐              ┌────────┐                       │
│        │P-Enfant│              │P-Enfant│                       │
│        └────────┘              └────────┘                       │
│                                                                  │
│  Caractéristiques :                                             │
│  • Relation parent-enfant stricte (1:N)                         │
│  • Un enfant n'a qu'un seul parent                              │
│  • Navigation de haut en bas                                    │
│  • Accès rapide si le chemin est connu                          │
│                                                                  │
│  Exemple : IMS DB (IBM)                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Avantages :**
- Performance excellente pour les accès prédéfinis
- Structure simple à comprendre
- Intégrité des données garantie par la hiérarchie

**Inconvénients :**
- Rigidité de la structure
- Redondance des données si relations multiples
- Difficile à modifier après conception

### b) SGBD Réseau

Le modèle **réseau** (ou maillé) permet des relations plus complexes :

```
┌─────────────────────────────────────────────────────────────────┐
│                      MODÈLE RÉSEAU                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│         ┌────────┐           ┌────────┐                         │
│         │ Noeud A│◄─────────►│ Noeud B│                         │
│         └───┬────┘           └───┬────┘                         │
│             │                    │                              │
│             ▼                    ▼                              │
│         ┌────────┐           ┌────────┐                         │
│         │ Noeud C│◄─────────►│ Noeud D│                         │
│         └───┬────┘           └────────┘                         │
│             │                    ▲                              │
│             └────────────────────┘                              │
│                                                                  │
│  Caractéristiques :                                             │
│  • Relations M:N possibles                                      │
│  • Un enfant peut avoir plusieurs parents                       │
│  • Structure en graphe                                          │
│  • Plus flexible que le hiérarchique                            │
│                                                                  │
│  Exemple : IDMS (Computer Associates)                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Avantages :**
- Relations multiples possibles
- Moins de redondance que le hiérarchique
- Bonne performance

**Inconvénients :**
- Complexité de navigation
- Conception difficile
- Modifications structurelles complexes

### c) SGBD Relationnel

Le modèle **relationnel** organise les données en tables liées par des clés :

```
┌─────────────────────────────────────────────────────────────────┐
│                    MODÈLE RELATIONNEL                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE CLIENTS                    TABLE COMMANDES               │
│  ┌────────┬──────────┐           ┌────────┬────────┬────────┐  │
│  │ CLI_ID │ NOM      │           │ CMD_ID │ CLI_ID │ DATE   │  │
│  ├────────┼──────────┤           ├────────┼────────┼────────┤  │
│  │ 001    │ DUPONT   │◄─────────►│ C001   │ 001    │ 2024   │  │
│  │ 002    │ MARTIN   │           │ C002   │ 001    │ 2024   │  │
│  │ 003    │ DURAND   │           │ C003   │ 002    │ 2024   │  │
│  └────────┴──────────┘           └────────┴────────┴────────┘  │
│         ▲                                                       │
│         │ Clé primaire ──────────► Clé étrangère               │
│                                                                  │
│  Caractéristiques :                                             │
│  • Données en tables (lignes et colonnes)                       │
│  • Relations via clés étrangères                                │
│  • Langage SQL standardisé                                      │
│  • Grande flexibilité                                           │
│                                                                  │
│  Exemple : DB2 (IBM), Oracle, MySQL, PostgreSQL                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Notion de cardinalité

La **cardinalité** définit le nombre d'occurrences d'une entité pouvant être associées à une autre :

| Notation | Signification | Exemple |
|----------|---------------|---------|
| **(0, 1)** | Optionnel, une seule occurrence | Un salarié peut avoir 0 ou 1 carte d'accès |
| **(1, 1)** | Obligatoire, une seule occurrence | Un salarié a exactement 1 numéro d'employé |
| **(0, n)** | Optionnel, plusieurs possibles | Un client peut avoir 0 ou plusieurs commandes |
| **(1, n)** | Obligatoire, plusieurs possibles | Une commande a au moins 1 ligne de commande |

**Exemple de relation (1,1) :**
```
TABLE SALARIE                    TABLE CARTE_ACCES
┌──────────┬──────────┐          ┌──────────┬──────────┐
│ SAL_ID   │ NOM      │          │ CARTE_ID │ SAL_ID   │
├──────────┼──────────┤          ├──────────┼──────────┤
│ 001      │ DUPONT   │◄────────►│ C001     │ 001      │
│ 002      │ MARTIN   │◄────────►│ C002     │ 002      │
└──────────┴──────────┘          └──────────┴──────────┘

Un salarié a une seule carte d'accès et une carte est rattachée à un seul salarié.
```

**Avantages :**
- Flexibilité maximale
- Langage SQL standard
- Requêtes ad-hoc possibles
- Facilité de modification

**Inconvénients :**
- Performance moindre pour certains accès massifs
- Optimisation nécessaire pour gros volumes
- Overhead du moteur SQL (normalisation peut impacter les performances)

### Comparaison des trois modèles

| Critère | Hiérarchique | Réseau | Relationnel |
|---------|--------------|--------|-------------|
| **Structure** | Arbre | Graphe | Tables |
| **Relations** | 1:N | M:N | M:N via jointures |
| **Langage** | DL/I | DML propriétaire | SQL |
| **Flexibilité** | Faible | Moyenne | Élevée |
| **Performance** | Excellente (accès fixe) | Bonne | Variable |
| **Complexité** | Moyenne | Élevée | Faible |
| **Exemple** | IMS | IDMS | DB2, Oracle |

## III-1 Caractéristiques du SGBD IMS

### a) Origine et Historique

```
┌─────────────────────────────────────────────────────────────────┐
│                    HISTORIQUE IMS                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1966  Programme Apollo (NASA)                                  │
│        └── Besoin de gérer les pièces de la fusée Saturn V     │
│                                                                  │
│  1968  IMS V1 - Information Management System                   │
│        └── Premier SGBD hiérarchique commercial                 │
│                                                                  │
│  1969  IMS/360 - Support System/360                             │
│        └── Intégration avec les mainframes IBM                  │
│                                                                  │
│  1970s Adoption massive                                         │
│        └── Banques, assurances, industrie                       │
│                                                                  │
│  1980s IMS/ESA                                                  │
│        └── Architecture étendue                                 │
│                                                                  │
│  1990s IMS/DB et IMS/TM séparés                                │
│        └── Modularisation des composants                        │
│                                                                  │
│  2000s IMS Version 9+                                           │
│        └── XML, Java, Web Services                              │
│                                                                  │
│  2020s IMS Version 15                                           │
│        └── Cloud, API REST, intégration moderne                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Contexte de création :**
- Projet Apollo de la NASA (1966)
- Gestion de la nomenclature des pièces du lanceur Saturn V
- Besoin de tracer les relations parent-enfant des composants
- IBM développe IMS pour répondre à ce besoin

### b) Structure de IMS

IMS se compose de deux sous-systèmes majeurs :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         ARCHITECTURE IMS                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│                         ┌─────────────────┐                             │
│                         │   APPLICATION   │                             │
│                         │     COBOL       │                             │
│                         └────────┬────────┘                             │
│                                  │                                       │
│                    ┌─────────────┴─────────────┐                        │
│                    ▼                           ▼                        │
│          ┌─────────────────┐         ┌─────────────────┐               │
│          │     IMS TM      │         │     IMS DB      │               │
│          │   Transaction   │         │    Database     │               │
│          │    Manager      │         │    Manager      │               │
│          │                 │         │                 │               │
│          │ • Gestion       │         │ • Gestion des   │               │
│          │   transactions  │         │   bases de      │               │
│          │ • Écrans        │         │   données       │               │
│          │ • Messages      │         │ • Accès DL/I    │               │
│          │ • Similaire     │         │ • Hiérarchies   │               │
│          │   à CICS        │         │                 │               │
│          └─────────────────┘         └────────┬────────┘               │
│                                               │                         │
│                                      ┌────────▼────────┐               │
│                                      │  BASES IMS DB   │               │
│                                      │  (Hiérarchiques)│               │
│                                      └─────────────────┘               │
│                                                                          │
│  Note: CICS peut utiliser IMS DB via DBCTL (sans IMS TM)               │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### IMS DB (Database Manager)

Gère les bases de données hiérarchiques :

| Composant | Description |
|-----------|-------------|
| **DBD** (Database Description) | Définition physique de la base |
| **PSB** (Program Specification Block) | Vue logique pour un programme |
| **PCB** (Program Communication Block) | Zone de statut et communication |
| **Segment** | Unité de données (enregistrement) |

#### IMS TM (Transaction Manager)

Moniteur transactionnel alternatif à CICS :

| Composant | Description |
|-----------|-------------|
| **MFS** (Message Format Service) | Gestion des écrans (équivalent BMS) |
| **MPP** (Message Processing Program) | Programme de traitement |
| **BMP** (Batch Message Processing) | Traitement batch avec accès DB |

### c) Fonctionnalités

#### Le modèle de données hiérarchique

```
┌─────────────────────────────────────────────────────────────────┐
│                EXEMPLE : BASE CLIENTS BANCAIRES                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│                       ┌─────────────┐                           │
│                       │   CLIENT    │ ◄── Segment RACINE        │
│                       │ (Root)      │                           │
│                       │             │                           │
│                       │ • NUM_CLI   │                           │
│                       │ • NOM       │                           │
│                       │ • PRENOM    │                           │
│                       └──────┬──────┘                           │
│                              │                                   │
│          ┌───────────────────┼───────────────────┐              │
│          │                   │                   │              │
│          ▼                   ▼                   ▼              │
│   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐        │
│   │   COMPTE    │    │   ADRESSE   │    │  TELEPHONE  │        │
│   │             │    │             │    │             │        │
│   │ • NUM_CPT   │    │ • RUE       │    │ • TYPE      │        │
│   │ • TYPE      │    │ • VILLE     │    │ • NUMERO    │        │
│   │ • SOLDE     │    │ • CP        │    │             │        │
│   └──────┬──────┘    └─────────────┘    └─────────────┘        │
│          │                                                       │
│          ▼                                                       │
│   ┌─────────────┐                                               │
│   │ MOUVEMENT   │                                               │
│   │             │                                               │
│   │ • DATE      │                                               │
│   │ • MONTANT   │                                               │
│   │ • LIBELLE   │                                               │
│   └─────────────┘                                               │
│                                                                  │
│  Niveau 1 : CLIENT (racine)                                     │
│  Niveau 2 : COMPTE, ADRESSE, TELEPHONE (enfants)               │
│  Niveau 3 : MOUVEMENT (petit-enfant)                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Le langage DL/I (Data Language/I)

DL/I est le langage d'accès aux données IMS :

```cobol
      * Appel DL/I générique
       CALL 'CBLTDLI' USING fonction
                            pcb
                            segment-io
                            ssa1
                            ssa2...
```

**Fonctions de lecture :**

| Fonction | Nom | Description |
|----------|-----|-------------|
| **GU** | Get Unique | Accès direct à un segment |
| **GN** | Get Next | Lecture séquentielle |
| **GNP** | Get Next within Parent | Séquentiel sous le parent courant |
| **GHU** | Get Hold Unique | GU avec verrouillage pour mise à jour |
| **GHN** | Get Hold Next | GN avec verrouillage |
| **GHNP** | Get Hold Next within Parent | GNP avec verrouillage |

**Fonctions de mise à jour :**

| Fonction | Nom | Description |
|----------|-----|-------------|
| **ISRT** | Insert | Insertion d'un nouveau segment |
| **REPL** | Replace | Modification (après GHU/GHN) |
| **DLET** | Delete | Suppression (après GHU/GHN) |

#### Segment Search Argument (SSA)

Le SSA spécifie le segment recherché :

```cobol
      * SSA non qualifié (sans condition)
       01 SSA-CLIENT-UNQUAL   PIC X(8) VALUE 'CLIENT  '.

      * SSA qualifié (avec condition)
       01 SSA-CLIENT-QUAL.
          05 FILLER           PIC X(8) VALUE 'CLIENT  '.
          05 FILLER           PIC X(1) VALUE '('.
          05 FILLER           PIC X(8) VALUE 'NUM_CLI '.
          05 FILLER           PIC X(2) VALUE '= '.
          05 SSA-NUM-CLI      PIC X(8).
          05 FILLER           PIC X(1) VALUE ')'.
```

#### PCB - Program Communication Block

Le PCB retourne le statut de chaque opération :

```cobol
       01 PCB-MASK.
          05 PCB-DBD-NAME     PIC X(8).    *> Nom de la DBD
          05 PCB-SEG-LEVEL    PIC X(2).    *> Niveau hiérarchique
          05 PCB-STATUS       PIC X(2).    *> Code statut
          05 PCB-PROC-OPT     PIC X(4).    *> Options de traitement
          05 PCB-RESERVED     PIC S9(5) COMP.
          05 PCB-SEG-NAME     PIC X(8).    *> Nom du segment
          05 PCB-KEY-LENGTH   PIC S9(5) COMP.
          05 PCB-SENS-SEGS    PIC S9(5) COMP.
          05 PCB-KEY-FB       PIC X(50).   *> Clé concaténée
```

**Codes statut courants :**

| Code | Signification |
|------|---------------|
| `bb` (blancs) | Opération réussie |
| `GE` | Segment non trouvé |
| `GB` | Fin de base de données |
| `GK` | Type de segment différent |
| `II` | Insertion impossible (doublon) |
| `AI` | Erreur d'ouverture |
| `AO` | Erreur de fermeture |

### d) Avantages du SGBD IMS

```
┌─────────────────────────────────────────────────────────────────┐
│                      AVANTAGES IMS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  PERFORMANCE                                                     │
│  ├── Accès très rapide pour chemins prédéfinis                  │
│  ├── Optimisé pour les transactions à fort volume               │
│  ├── Pas d'overhead d'optimiseur de requêtes                    │
│  └── Traitement batch et online performant                      │
│                                                                  │
│  FIABILITÉ                                                       │
│  ├── Plus de 50 ans d'existence                                 │
│  ├── Éprouvé dans les environnements critiques                  │
│  ├── Mécanismes de recovery robustes                            │
│  └── Haute disponibilité                                        │
│                                                                  │
│  INTÉGRITÉ DES DONNÉES                                          │
│  ├── Hiérarchie garantit la cohérence parent-enfant             │
│  ├── Suppression en cascade automatique                         │
│  └── Transactions ACID                                          │
│                                                                  │
│  SCALABILITÉ                                                     │
│  ├── Gère des volumes massifs de données                        │
│  ├── Milliers de transactions par seconde                       │
│  └── Évolution sans refonte majeure                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### e) Limites et Inconvénients

```
┌─────────────────────────────────────────────────────────────────┐
│                   LIMITES ET INCONVÉNIENTS                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  RIGIDITÉ                                                        │
│  ├── Structure hiérarchique fixe                                │
│  ├── Modifications de schéma complexes                          │
│  ├── Réorganisation nécessaire pour changements                 │
│  └── Planification initiale cruciale                            │
│                                                                  │
│  REDONDANCE                                                      │
│  ├── Données dupliquées si relations M:N                        │
│  ├── Pas de jointures comme en relationnel                      │
│  └── Cohérence à gérer manuellement                             │
│                                                                  │
│  COMPÉTENCES                                                     │
│  ├── Langage DL/I spécifique                                    │
│  ├── Moins de développeurs formés que SQL                       │
│  └── Courbe d'apprentissage                                     │
│                                                                  │
│  FLEXIBILITÉ                                                     │
│  ├── Pas de requêtes ad-hoc faciles                             │
│  ├── Accès prédéfinis uniquement                                │
│  └── Reporting moins flexible                                   │
│                                                                  │
│  COÛT                                                            │
│  ├── Licence IBM coûteuse                                       │
│  ├── Expertise rare et chère                                    │
│  └── Environnement mainframe requis                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Insuffisances spécifiques des SGBD hiérarchiques (vs Relationnel)

Les **jointures** dans un SGBD hiérarchique (comme IMS) ont des limitations majeures par rapport aux SGBD relationnels :

| Limitation | Description | Impact |
|------------|-------------|--------|
| **Navigation explicite** | Il faut naviguer manuellement à travers les segments pour obtenir les données enfants | Code plus complexe, plus de lignes |
| **Modèle hiérarchique** | Relations fixes (parent-enfant uniquement), pas de relations flexibles entre segments | Pas de jointures ad-hoc possibles |
| **Performance variable** | Naviguer à travers plusieurs niveaux peut être lent si la hiérarchie est profonde | Temps de réponse variable |
| **Requêtes limitées** | Pas de langage déclaratif comme SQL pour exprimer des requêtes complexes | Moins de flexibilité d'interrogation |

```
┌─────────────────────────────────────────────────────────────────────────┐
│            COMPARAISON : ACCÈS HIÉRARCHIQUE vs RELATIONNEL              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  HIÉRARCHIQUE (IMS)              │  RELATIONNEL (DB2/SQL)               │
│  ─────────────────────           │  ──────────────────────              │
│                                   │                                      │
│  • Navigation segment par        │  • Jointures déclaratives            │
│    segment                        │                                      │
│                                   │                                      │
│  • Chemin d'accès prédéfini      │  • Optimiseur choisit le             │
│    par le développeur             │    meilleur chemin                   │
│                                   │                                      │
│  • Plusieurs appels DL/I         │  • Une seule requête SQL             │
│    nécessaires                    │                                      │
│                                   │                                      │
│  • Performant si structure       │  • Flexible mais overhead            │
│    bien adaptée                   │    de l'optimiseur                   │
│                                   │                                      │
│  • Modification de structure     │  • Ajout de colonnes/tables          │
│    = réorganisation majeure       │    relativement simple               │
│                                   │                                      │
└─────────────────────────────────────────────────────────────────────────┘
```

### f) Applications de IMS

IMS reste utilisé dans des secteurs critiques :

| Secteur | Utilisation |
|---------|-------------|
| **Banque** | Comptes clients, transactions, historiques |
| **Assurance** | Polices, sinistres, bénéficiaires |
| **Santé** | Dossiers patients, prescriptions |
| **Industrie** | Nomenclatures produits, pièces détachées |
| **Transport** | Réservations, vols, passagers |
| **Distribution** | Stocks, commandes, livraisons |
| **Télécoms** | Abonnés, facturation, équipements |

**Exemples concrets :**

```
┌─────────────────────────────────────────────────────────────────┐
│                    EXEMPLES D'UTILISATION                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  BANQUE : Hiérarchie Client                                     │
│  CLIENT ─► COMPTE ─► MOUVEMENT                                  │
│       └─► ADRESSE                                               │
│       └─► PROCURATION                                           │
│                                                                  │
│  INDUSTRIE : Nomenclature produit                               │
│  PRODUIT ─► COMPOSANT ─► SOUS-COMPOSANT                         │
│         └─► FOURNISSEUR                                         │
│                                                                  │
│  SANTÉ : Dossier patient                                        │
│  PATIENT ─► CONSULTATION ─► PRESCRIPTION                        │
│         └─► ALLERGIE                                            │
│         └─► ANTECEDENT                                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### g) Caractéristiques techniques

#### Exemple pratique : Base hiérarchique REGION/COMPTE/MVT

Le PDF du cours illustre le modèle hiérarchique avec cet exemple bancaire :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    EXEMPLE : HIÉRARCHIE BANCAIRE                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│                            ┌──────────┐                                 │
│                            │  REGION  │  ◄── Segment RACINE             │
│                            └────┬─────┘                                 │
│              ┌──────────────────┼──────────────────┐                    │
│              │                  │                  │                    │
│              ▼                  ▼                  ▼                    │
│        ┌──────────┐       ┌──────────┐       ┌──────────┐              │
│        │ COMPTE 1 │       │ COMPTE 2 │       │ COMPTE 3 │              │
│        └────┬─────┘       └────┬─────┘       └────┬─────┘              │
│        ┌────┴────┐        ┌────┴────┐        ┌────┴────┐               │
│        ▼         ▼        ▼         ▼        ▼         ▼               │
│     ┌─────┐  ┌─────┐   ┌─────┐  ┌─────┐   ┌─────┐  ┌─────┐            │
│     │MVT 1│  │MVT 2│   │MVT 3│  │MVT 4│   │MVT 5│  │MVT 6│            │
│     └─────┘  └─────┘   └─────┘  └─────┘   └─────┘  └─────┘            │
│                                                                          │
│  Chaque MVT est rattaché à un unique COMPTE                             │
│  Chaque COMPTE est rattaché à une unique REGION                         │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

**Structure des segments :**

```
REGION  : ID-REGION    ZONE-REGION    DEPOT-REGION    ENGAGEMENT-REGION
          (clé)

COMPTE  : ID-COMPTE    NOM-COMPTE     SOLDE-COMPTE    ID-REGION    SOLDE-MOYEN
          (clé)                                        (FK parent)

MVT     : ID-MVT       DATE-MVT       ID-COMPTE       SENS-MVT     MONTANT-MVT
          (clé)                        (FK parent)
```

**Navigation hiérarchique pour lire tous les MVT d'une REGION :**

```
┌─────────────────────────────────────────────────────────────────────────┐
│  ALGORITHME DE PARCOURS HIÉRARCHIQUE                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. Indiquer ID-REGION à traiter : 'REGION1'                           │
│                                                                          │
│  2. Récupérer le 1er COMPTE de la REGION1                              │
│                                                                          │
│  3. Pour chaque COMPTE de la REGION1 :                                 │
│     │                                                                    │
│     └─► Récupérer le nème MVT du nème COMPTE                           │
│                                                                          │
│  4. Parcourir les MVT des COMPTE de la REGION1                         │
│     jusqu'à la fin de tous les MVT                                      │
│                                                                          │
│  ⚠️ Navigation explicite obligatoire : on descend niveau par niveau    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

**Équivalent SQL (SGBD Relationnel) :**

```sql
-- En SQL, une seule requête avec jointures suffit :

SELECT R.ID-REGION, R.ZONE-REGION,
       C.ID-COMPTE, C.NOM-COMPTE,
       M.ID-MVT, M.DATE-MVT, M.MONTANT-MVT
FROM REGION R
JOIN COMPTE C ON R.ID-REGION = C.ID-REGION
JOIN MVT M ON C.ID-COMPTE = M.ID-COMPTE
WHERE R.ID-REGION = 'REGION1'
ORDER BY C.ID-COMPTE, M.DATE-MVT;

-- Cette requête fait une jointure entre REGION, COMPTE et MVT
-- pour obtenir tous les mouvements d'une région en une seule opération
```

#### Méthodes d'accès

| Type | Description | Cas d'usage |
|------|-------------|-------------|
| **HSAM** | Hierarchical Sequential | Lecture séquentielle uniquement, batch |
| **HISAM** | Hierarchical Indexed Sequential | Accès séquentiel et direct, petites bases |
| **HDAM** | Hierarchical Direct Access | Accès direct par hashing, gros volumes |
| **HIDAM** | Hierarchical Indexed Direct | Accès direct indexé, le plus polyvalent |

#### Types de bases

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES DE BASES IMS                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  FULL FUNCTION DATABASE                                          │
│  ├── Base hiérarchique complète                                 │
│  ├── Support HDAM, HIDAM, HISAM, HSAM                          │
│  └── Toutes les fonctionnalités DL/I                           │
│                                                                  │
│  FAST PATH DATABASE                                              │
│  ├── DEDBs (Data Entry Databases)                               │
│  ├── MSDBs (Main Storage Databases)                             │
│  ├── Optimisées pour performances extrêmes                      │
│  └── Fonctionnalités réduites                                   │
│                                                                  │
│  HALDB (High Availability Large Database)                       │
│  ├── Bases de très grande taille                                │
│  ├── Partitionnement automatique                                │
│  ├── Réorganisation online                                      │
│  └── Haute disponibilité                                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Intégration avec CICS

CICS accède à IMS DB via **DBCTL** (Database Control) :

```
┌─────────────────────────────────────────────────────────────────┐
│                     CICS + IMS DB (DBCTL)                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│    ┌─────────────┐                                              │
│    │ Programme   │                                              │
│    │ COBOL-CICS  │                                              │
│    └──────┬──────┘                                              │
│           │ EXEC CICS DLI ...                                   │
│           ▼                                                      │
│    ┌─────────────┐                                              │
│    │    CICS     │                                              │
│    └──────┬──────┘                                              │
│           │                                                      │
│           ▼                                                      │
│    ┌─────────────┐                                              │
│    │   DBCTL     │ ◄── Interface CICS-IMS                       │
│    │  (Region)   │                                              │
│    └──────┬──────┘                                              │
│           │ DL/I                                                 │
│           ▼                                                      │
│    ┌─────────────┐                                              │
│    │   IMS DB    │                                              │
│    │  (Bases)    │                                              │
│    └─────────────┘                                              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Exemple de programme COBOL-CICS-IMS :**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMCICSIMS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CLIENT-SEG.
          05 CLI-NUM           PIC 9(8).
          05 CLI-NOM           PIC X(30).
          05 CLI-PRENOM        PIC X(20).

       01 WS-SSA-CLIENT.
          05 FILLER            PIC X(9) VALUE 'CLIENT  ('.
          05 FILLER            PIC X(8) VALUE 'CLI_NUM '.
          05 FILLER            PIC X(2) VALUE '= '.
          05 WS-SEARCH-NUM     PIC X(8).
          05 FILLER            PIC X(1) VALUE ')'.

       01 WS-RESP              PIC S9(8) COMP.

       PROCEDURE DIVISION.
      *─────────────────────────────────────────────────────────────
      * Lecture d'un client via DL/I depuis CICS
      *─────────────────────────────────────────────────────────────
           MOVE '12345678' TO WS-SEARCH-NUM

           EXEC CICS
               DLI GU
                   USING PCB(1)
                   SEGMENT(WS-CLIENT-SEG)
                   SEGLENGTH(LENGTH OF WS-CLIENT-SEG)
                   SSGMNTNAME('CLIENT')
                   SSA(WS-SSA-CLIENT)
                   RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   PERFORM CLIENT-INTROUVABLE
               WHEN OTHER
                   PERFORM ERREUR-TECHNIQUE
           END-EVALUATE

           EXEC CICS RETURN END-EXEC.
```

## Comparaison IMS / VSAM / DB2

```
┌─────────────────────────────────────────────────────────────────────────┐
│              COMPARAISON DES SYSTÈMES DE DONNÉES                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│                    VSAM              IMS DB             DB2              │
│  ──────────────────────────────────────────────────────────────────────│
│                                                                          │
│  Type           Fichiers           Hiérarchique      Relationnel        │
│                                                                          │
│  Structure      KSDS, ESDS, RRDS   Segments/Arbre    Tables/Lignes     │
│                                                                          │
│  Langage        COBOL natif        DL/I              SQL                │
│                                                                          │
│  Relations      Aucune             Parent-Enfant     Clés étrangères    │
│                                    (1:N)             (M:N)              │
│                                                                          │
│  Accès          READ, WRITE        GU, GN, ISRT      SELECT, INSERT     │
│                 REWRITE, DELETE    REPL, DLET        UPDATE, DELETE     │
│                                                                          │
│  Flexibilité    Moyenne            Faible            Élevée             │
│                                                                          │
│  Performance    Bonne              Excellente        Variable           │
│                                    (accès fixes)     (optimiseur)       │
│                                                                          │
│  Requêtes       Non                Non               Oui (ad-hoc)       │
│  Ad-hoc                                                                  │
│                                                                          │
│  Cas d'usage    Fichiers simples   Nomenclatures     Applications       │
│                 Référentiels       Hiérarchies       modernes           │
│                                    Transactions      Reporting          │
│                                    massives                             │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE III - RÉSUMÉ                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  PRÉAMBULE : TYPES DE SGBD                                              │
│  ─────────────────────────                                              │
│  • Hiérarchique : Arbre, parent-enfant, IMS                            │
│  • Réseau      : Graphe, relations M:N, IDMS                           │
│  • Relationnel : Tables, SQL, DB2/Oracle, cardinalité (0,1)/(1,n)     │
│                                                                          │
│  III-1 CARACTÉRISTIQUES IMS                                             │
│  ─────────────────────────                                              │
│                                                                          │
│  a) ORIGINE                                                              │
│     • 1966 : Projet Apollo NASA                                         │
│     • 1968 : Première version IMS                                       │
│     • 50+ ans d'évolution continue                                      │
│                                                                          │
│  b) STRUCTURE                                                            │
│     • IMS TM : Moniteur transactionnel                                  │
│     • IMS DB : Gestionnaire de bases hiérarchiques                      │
│     • DBD/PSB/PCB : Composants de définition                           │
│                                                                          │
│  c) FONCTIONNALITÉS                                                      │
│     • Modèle hiérarchique (segments en arbre)                          │
│     • Langage DL/I (GU, GN, ISRT, REPL, DLET)                          │
│     • SSA pour la recherche                                             │
│                                                                          │
│  d) AVANTAGES                                                            │
│     • Performance excellente                                            │
│     • Fiabilité éprouvée                                                │
│     • Intégrité des données                                             │
│                                                                          │
│  e) LIMITES                                                              │
│     • Rigidité structurelle                                             │
│     • Pas de requêtes ad-hoc                                            │
│     • Compétences spécifiques                                           │
│     • Insuffisances vs relationnel : navigation explicite,             │
│       pas de jointures flexibles                                        │
│                                                                          │
│  f) APPLICATIONS                                                         │
│     • Banque, Assurance, Santé, Industrie                              │
│     • Nomenclatures, hiérarchies complexes                             │
│                                                                          │
│  g) CARACTÉRISTIQUES TECHNIQUES                                         │
│     • Exemple REGION/COMPTE/MVT (hiérarchie bancaire)                  │
│     • Comparaison SQL vs navigation DL/I                               │
│     • HSAM, HISAM, HDAM, HIDAM                                         │
│     • Full Function, Fast Path, HALDB                                  │
│     • Intégration CICS via DBCTL                                       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre II - Organisation Système](02-organisation-systeme.md) | [Chapitre IV - Architecture Multicouches](04-architecture-multicouches.md) |

---
*Formation COBOL - Module CICS*
