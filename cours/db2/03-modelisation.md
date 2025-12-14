# Chapitre III - Modélisation des Données

## III-1 : Cycle de Conception d'une Base de Données

### Vue d'ensemble

La conception d'une base de données suit un processus en plusieurs étapes, partant des besoins métier jusqu'à l'implémentation physique.

```
┌─────────────────────────────────────────────────────────────────┐
│              CYCLE DE CONCEPTION D'UNE BASE DE DONNÉES           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. ANALYSE DES BESOINS MÉTIER                                  │
│     │   • Interviews utilisateurs                               │
│     │   • Documents existants                                   │
│     │   • Règles de gestion                                     │
│     ▼                                                           │
│  2. MODÈLE CONCEPTUEL DE DONNÉES (MCD)                         │
│     │   • Entités et attributs                                  │
│     │   • Relations et cardinalités                             │
│     │   • Indépendant de tout SGBD                             │
│     ▼                                                           │
│  3. NORMALISATION                                               │
│     │   • 1NF, 2NF, 3NF                                        │
│     │   • Élimination des anomalies                            │
│     ▼                                                           │
│  4. MODÈLE LOGIQUE DE DONNÉES (MLD)                            │
│     │   • Tables et colonnes                                    │
│     │   • Clés primaires et étrangères                         │
│     │   • Contraintes d'intégrité                              │
│     ▼                                                           │
│  5. MODÈLE PHYSIQUE DE DONNÉES (MPD)                           │
│         • Scripts DDL (CREATE TABLE...)                         │
│         • Indexation                                            │
│         • Optimisation pour le SGBD cible                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Niveaux d'abstraction

| Niveau | Modèle | Question | Indépendant du SGBD ? |
|--------|--------|----------|----------------------|
| **Conceptuel** | MCD | QUOI stocker ? | Oui |
| **Logique** | MLD | COMMENT organiser ? | Partiellement |
| **Physique** | MPD | OÙ et COMMENT stocker ? | Non |

---

## III-2 : Modèle Conceptuel de Données (MCD)

### Définition

Le **MCD** est une représentation abstraite des données et de leurs relations, indépendante de tout système informatique. Il utilise la notation **entité-relation** (E-R).

### Éléments du MCD

```
┌─────────────────────────────────────────────────────────────────┐
│                    ÉLÉMENTS DU MCD                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. ENTITÉ                                                       │
│     Représente un objet du monde réel identifiable              │
│     ┌─────────────────┐                                         │
│     │    EMPLOYÉ      │                                         │
│     └─────────────────┘                                         │
│                                                                  │
│  2. ATTRIBUT                                                     │
│     Propriété caractéristique d'une entité                      │
│     ┌─────────────────┐                                         │
│     │    EMPLOYÉ      │                                         │
│     ├─────────────────┤                                         │
│     │ #EMP_NUM (PK)   │  ← Identifiant unique                   │
│     │ EMP_NOM         │                                         │
│     │ EMP_PRENOM      │                                         │
│     │ DATE_NAISSANCE  │                                         │
│     └─────────────────┘                                         │
│                                                                  │
│  3. RELATION (ASSOCIATION)                                      │
│     Lien sémantique entre deux entités                          │
│                                                                  │
│     ┌──────────┐              ┌──────────┐                      │
│     │ EMPLOYÉ  │──TRAVAILLE──│DÉPARTEMENT│                      │
│     └──────────┘      │       └──────────┘                      │
│                       │                                         │
│                    RELATION                                     │
│                                                                  │
│  4. CARDINALITÉ                                                  │
│     Quantifie la relation (min, max)                            │
│     • (0,1) : optionnel, au plus 1                              │
│     • (1,1) : obligatoire, exactement 1                         │
│     • (0,n) : optionnel, plusieurs possibles                    │
│     • (1,n) : obligatoire, au moins 1                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de relations

```
┌─────────────────────────────────────────────────────────────────┐
│                  TYPES DE RELATIONS (CARDINALITÉS)               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1-1 : UN À UN                                                   │
│  ┌──────────┐    1,1     1,1    ┌──────────┐                   │
│  │ EMPLOYÉ  │────────────────────│  BADGE   │                   │
│  └──────────┘                    └──────────┘                   │
│  Un employé possède UN badge, un badge appartient à UN employé  │
│                                                                  │
│  ───────────────────────────────────────────────────────────    │
│                                                                  │
│  1-N : UN À PLUSIEURS                                           │
│  ┌──────────┐    1,1     0,n    ┌──────────┐                   │
│  │ EMPLOYÉ  │────────────────────│DÉPARTEMENT│                  │
│  └──────────┘                    └──────────┘                   │
│  Un employé appartient à UN département,                        │
│  un département a PLUSIEURS employés                            │
│                                                                  │
│  ───────────────────────────────────────────────────────────    │
│                                                                  │
│  N-N : PLUSIEURS À PLUSIEURS                                    │
│  ┌──────────┐    0,n     0,n    ┌──────────┐                   │
│  │ EMPLOYÉ  │────────────────────│  PROJET  │                   │
│  └──────────┘                    └──────────┘                   │
│  Un employé participe à PLUSIEURS projets,                      │
│  un projet a PLUSIEURS employés                                 │
│                                                                  │
│  → Nécessite une table d'association (PARTICIPATION)            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple de MCD

```
┌─────────────────────────────────────────────────────────────────┐
│               MCD - GESTION DES EMPLOYÉS                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────┐                 ┌─────────────────┐        │
│  │    EMPLOYÉ      │                 │   DÉPARTEMENT   │        │
│  ├─────────────────┤                 ├─────────────────┤        │
│  │ #EMP_NUM        │   APPARTIENT    │ #DEPT_NUM       │        │
│  │ EMP_NOM         │─────(1,1)───────│ DEPT_NOM        │        │
│  │ EMP_PRENOM      │      │          │ LOCALISATION    │        │
│  │ DATE_EMBAUCHE   │      │          └─────────────────┘        │
│  │ SALAIRE         │      │                 │                   │
│  │ COMMISSION      │    (0,n)               │                   │
│  └─────────────────┘      │               (1,1)                 │
│          │                │                 │                   │
│        (0,1)              │          ┌──────┴──────┐            │
│          │                │          │   DIRIGE    │            │
│     ┌────┴────┐           │          └─────────────┘            │
│     │ DIRIGE  │           │                 │                   │
│     └─────────┘           │               (0,1)                 │
│                           │                 │                   │
│  (Un employé peut         │                 ▼                   │
│   avoir un directeur      └────────────► EMPLOYÉ                │
│   qui est aussi                      (le directeur)             │
│   un employé)                                                   │
│                                                                  │
│  ┌─────────────────┐                                            │
│  │  GRILLE_SAL     │                                            │
│  ├─────────────────┤                                            │
│  │ #GRADE          │                                            │
│  │ SALAIRE_MIN     │                                            │
│  │ SALAIRE_MAX     │                                            │
│  └─────────────────┘                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-3 : Normalisation

### Pourquoi normaliser ?

La **normalisation** vise à éliminer les **anomalies** de données :

```
┌─────────────────────────────────────────────────────────────────┐
│                 ANOMALIES DANS UNE BD NON NORMALISÉE            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Table non normalisée :                                          │
│  ┌────────┬─────────┬──────────┬───────────┬─────────────┐     │
│  │EMP_NUM │ EMP_NOM │ DEPT_NUM │ DEPT_NOM  │ DEPT_LOC    │     │
│  ├────────┼─────────┼──────────┼───────────┼─────────────┤     │
│  │ 7369   │ ARTHUR  │    20    │ RECHERCHE │ STRASBOURG  │     │
│  │ 7499   │ PAUL    │    30    │ VENTES    │ LYON        │     │
│  │ 7521   │ MARTIN  │    30    │ VENTES    │ LYON        │     │
│  └────────┴─────────┴──────────┴───────────┴─────────────┘     │
│                                                                  │
│  ANOMALIE D'INSERTION :                                          │
│  → Impossible de créer un département sans employé              │
│                                                                  │
│  ANOMALIE DE MISE À JOUR :                                      │
│  → Changer "LYON" en "PARIS" pour VENTES = modifier 2 lignes    │
│  → Risque d'incohérence si oubli                                │
│                                                                  │
│  ANOMALIE DE SUPPRESSION :                                      │
│  → Supprimer ARTHUR (seul du dept 20) = perd info RECHERCHE    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Les trois formes normales

```
┌─────────────────────────────────────────────────────────────────┐
│                  LES FORMES NORMALES (1NF, 2NF, 3NF)            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  1NF - PREMIÈRE FORME NORMALE                            │    │
│  │  ─────────────────────────────                           │    │
│  │                                                          │    │
│  │  Règles :                                               │    │
│  │  • Attributs ATOMIQUES (pas de valeurs répétitives)     │    │
│  │  • Clé primaire définie                                 │    │
│  │  • Pas de groupes répétitifs                            │    │
│  │                                                          │    │
│  │  VIOLATION :                                            │    │
│  │  ┌────────┬──────────────────────┐                      │    │
│  │  │EMP_NUM │ TELEPHONES           │ ← Attribut multi-valué│   │
│  │  ├────────┼──────────────────────┤                      │    │
│  │  │ 7369   │ 0601, 0602, 0603     │                      │    │
│  │  └────────┴──────────────────────┘                      │    │
│  │                                                          │    │
│  │  CORRECTION :                                           │    │
│  │  ┌────────┬──────────┐                                  │    │
│  │  │EMP_NUM │ TELEPHONE│                                  │    │
│  │  ├────────┼──────────┤                                  │    │
│  │  │ 7369   │ 0601     │                                  │    │
│  │  │ 7369   │ 0602     │                                  │    │
│  │  │ 7369   │ 0603     │                                  │    │
│  │  └────────┴──────────┘                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  2NF - DEUXIÈME FORME NORMALE                           │    │
│  │  ─────────────────────────────                          │    │
│  │                                                          │    │
│  │  Prérequis : être en 1NF                                │    │
│  │                                                          │    │
│  │  Règle :                                                │    │
│  │  • Tout attribut non-clé dépend de TOUTE la clé        │    │
│  │    (pas de dépendance partielle)                        │    │
│  │                                                          │    │
│  │  VIOLATION (clé composite : EMP_NUM + PROJET_NUM) :     │    │
│  │  ┌────────┬───────────┬──────────┬──────────────┐       │    │
│  │  │EMP_NUM │PROJET_NUM │ EMP_NOM  │ PROJET_NOM   │       │    │
│  │  ├────────┼───────────┼──────────┼──────────────┤       │    │
│  │  │ 7369   │   P001    │ ARTHUR   │ ALPHA        │       │    │
│  │  └────────┴───────────┴──────────┴──────────────┘       │    │
│  │            ▲              │            │                │    │
│  │            └──────────────┘            │                │    │
│  │            EMP_NOM ne dépend que       │                │    │
│  │            de EMP_NUM (pas de PROJET)  │                │    │
│  │                                        │                │    │
│  │  CORRECTION : Séparer en 3 tables     ▼                │    │
│  │  EMPLOYEE(EMP_NUM, EMP_NOM)                            │    │
│  │  PROJET(PROJET_NUM, PROJET_NOM)                        │    │
│  │  AFFECTATION(EMP_NUM, PROJET_NUM)                      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  3NF - TROISIÈME FORME NORMALE                          │    │
│  │  ─────────────────────────────                          │    │
│  │                                                          │    │
│  │  Prérequis : être en 2NF                                │    │
│  │                                                          │    │
│  │  Règle :                                                │    │
│  │  • Pas de DÉPENDANCE TRANSITIVE                         │    │
│  │  • Un attribut non-clé ne dépend pas d'un autre        │    │
│  │    attribut non-clé                                     │    │
│  │                                                          │    │
│  │  VIOLATION :                                            │    │
│  │  ┌────────┬──────────┬───────────┬─────────────┐       │    │
│  │  │EMP_NUM │ DEPT_NUM │ DEPT_NOM  │ DEPT_LOC    │       │    │
│  │  └────────┴──────────┴───────────┴─────────────┘       │    │
│  │      │         │          │            │               │    │
│  │      │         │          └────────────┘               │    │
│  │      │         │          DEPT_NOM et DEPT_LOC         │    │
│  │      │         │          dépendent de DEPT_NUM        │    │
│  │      │         │          (pas directement de EMP_NUM) │    │
│  │      │         │                                       │    │
│  │  EMP_NUM → DEPT_NUM → DEPT_NOM (transitivité)         │    │
│  │                                                          │    │
│  │  CORRECTION : Séparer                                   │    │
│  │  EMPLOYEE(EMP_NUM, DEPT_NUM)                           │    │
│  │  DEPT(DEPT_NUM, DEPT_NOM, DEPT_LOC)                    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Tableau récapitulatif

| Forme | Règle | Vérifie |
|-------|-------|---------|
| **1NF** | Attributs atomiques | Pas de répétition, clé primaire |
| **2NF** | Dépendance totale | Pas de dépendance partielle à la clé |
| **3NF** | Pas de transitivité | Attribut non-clé → clé uniquement |

### Exemple complet de normalisation

```
┌─────────────────────────────────────────────────────────────────┐
│              EXEMPLE DE NORMALISATION COMPLÈTE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE INITIALE (non normalisée) :                              │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │ COMMANDE                                                 │    │
│  ├────────┬────────┬────────────┬─────────┬────────┬───────┤   │
│  │CMD_NUM │CLI_NUM │ CLI_NOM    │PRODUITS │CLI_VILLE│TOTAL │   │
│  ├────────┼────────┼────────────┼─────────┼────────┼───────┤   │
│  │ C001   │ CL01   │ MARTIN     │P1,P2,P3 │PARIS   │ 150   │   │
│  │ C002   │ CL01   │ MARTIN     │P2,P4    │PARIS   │ 80    │   │
│  └────────┴────────┴────────────┴─────────┴────────┴───────┘   │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  1NF : Éliminer les attributs multi-valués (PRODUITS)           │
│  ┌────────┬────────┬────────────┬────────┬────────┬───────┐    │
│  │CMD_NUM │CLI_NUM │ CLI_NOM    │PROD_NUM│CLI_VILLE│TOTAL │    │
│  ├────────┼────────┼────────────┼────────┼────────┼───────┤    │
│  │ C001   │ CL01   │ MARTIN     │  P1    │PARIS   │ 150   │    │
│  │ C001   │ CL01   │ MARTIN     │  P2    │PARIS   │ 150   │    │
│  │ C001   │ CL01   │ MARTIN     │  P3    │PARIS   │ 150   │    │
│  │ C002   │ CL01   │ MARTIN     │  P2    │PARIS   │ 80    │    │
│  │ C002   │ CL01   │ MARTIN     │  P4    │PARIS   │ 80    │    │
│  └────────┴────────┴────────────┴────────┴────────┴───────┘    │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  2NF : Éliminer les dépendances partielles                      │
│        (clé = CMD_NUM + PROD_NUM)                               │
│                                                                  │
│  COMMANDE (CMD_NUM, CLI_NUM, TOTAL)                             │
│  LIGNE_CMD (CMD_NUM, PROD_NUM)                                  │
│  CLIENT (CLI_NUM, CLI_NOM, CLI_VILLE)                           │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  3NF : Déjà en 3NF (pas de dépendance transitive)              │
│                                                                  │
│  RÉSULTAT FINAL :                                               │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  CLIENT (CLI_NUM, CLI_NOM, CLI_VILLE)                    │  │
│  │  COMMANDE (CMD_NUM, CLI_NUM, TOTAL)                      │  │
│  │  LIGNE_CMD (CMD_NUM, PROD_NUM)                           │  │
│  │  PRODUIT (PROD_NUM, PROD_NOM, PRIX)                      │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-4 : Du MCD au MLD

### Règles de transformation

```
┌─────────────────────────────────────────────────────────────────┐
│              RÈGLES DE TRANSFORMATION MCD → MLD                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  RÈGLE 1 : ENTITÉ → TABLE                                       │
│  ─────────────────────────                                      │
│  Chaque entité devient une table                                │
│  L'identifiant devient la clé primaire                          │
│                                                                  │
│  MCD:                        MLD:                               │
│  ┌─────────────┐            ┌─────────────────────────┐        │
│  │  EMPLOYÉ    │     →      │ EMPLOYEE                │        │
│  ├─────────────┤            ├─────────────────────────┤        │
│  │ #EMP_NUM    │            │ EMP_NUM (PK)            │        │
│  │ EMP_NOM     │            │ EMP_NOM                 │        │
│  └─────────────┘            └─────────────────────────┘        │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  RÈGLE 2 : RELATION 1-N → CLÉ ÉTRANGÈRE                        │
│  ──────────────────────────────────────                        │
│  La clé primaire du côté "1" migre vers le côté "N"            │
│                                                                  │
│  MCD:                                                           │
│  ┌──────────┐    1,1     0,n    ┌──────────┐                   │
│  │ EMPLOYÉ  │────────────────────│DÉPARTEMENT│                  │
│  └──────────┘                    └──────────┘                   │
│                                                                  │
│  MLD:                                                           │
│  ┌───────────────────────┐     ┌───────────────────────┐       │
│  │ EMPLOYEE              │     │ DEPT                  │       │
│  ├───────────────────────┤     ├───────────────────────┤       │
│  │ EMP_NUM (PK)          │     │ DEPT_NUM (PK)         │       │
│  │ EMP_NOM               │     │ DEPT_NOM              │       │
│  │ DEPT_NUM (FK) ────────┼─────│                       │       │
│  └───────────────────────┘     └───────────────────────┘       │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  RÈGLE 3 : RELATION N-N → TABLE D'ASSOCIATION                  │
│  ────────────────────────────────────────────                  │
│  Créer une nouvelle table avec les 2 clés étrangères           │
│                                                                  │
│  MCD:                                                           │
│  ┌──────────┐    0,n     0,n    ┌──────────┐                   │
│  │ EMPLOYÉ  │────────────────────│  PROJET  │                   │
│  └──────────┘                    └──────────┘                   │
│                                                                  │
│  MLD:                                                           │
│  ┌─────────────┐  ┌──────────────────┐  ┌─────────────┐        │
│  │ EMPLOYEE    │  │  AFFECTATION     │  │ PROJET      │        │
│  ├─────────────┤  ├──────────────────┤  ├─────────────┤        │
│  │EMP_NUM (PK) │◄─│EMP_NUM (FK,PK)   │  │PROJ_NUM(PK) │        │
│  │EMP_NOM      │  │PROJ_NUM (FK,PK)──┼──│PROJ_NOM     │        │
│  └─────────────┘  │DATE_AFFECTATION  │  └─────────────┘        │
│                   └──────────────────┘                          │
│                                                                  │
│  ─────────────────────────────────────────────────────────────  │
│                                                                  │
│  RÈGLE 4 : RELATION 1-1 → FUSION OU FK                         │
│  ─────────────────────────────────────                         │
│  Soit fusionner les tables, soit ajouter une FK avec UNIQUE    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple complet MCD → MLD

```
┌─────────────────────────────────────────────────────────────────┐
│                    MCD GESTION EMPLOYÉS                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────┐                                            │
│  │    EMPLOYÉ      │                                            │
│  ├─────────────────┤         APPARTIENT                         │
│  │ #EMP_NUM        │─────────(1,1)───────┐                      │
│  │ EMP_NOM         │                     │                      │
│  │ POSTE           │                     │                      │
│  │ DATE_EMB        │                   (0,n)                    │
│  │ SALAIRE         │                     │                      │
│  │ COMMISSION      │            ┌────────┴────────┐             │
│  └─────────────────┘            │   DÉPARTEMENT   │             │
│          │                      ├─────────────────┤             │
│        (0,1)                    │ #DEPT_NUM       │             │
│          │                      │ DEPT_NOM        │             │
│     ┌────┴────┐                 │ LOCALISATION    │             │
│     │ DIRIGE  │                 └─────────────────┘             │
│     └────┬────┘                                                 │
│          │                                                       │
│        (0,n)                                                     │
│          │                                                       │
│          ▼                                                       │
│      EMPLOYÉ (le manager)                                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    MLD RÉSULTANT                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE DEPT                                                      │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ DEPT_NUM   SMALLINT     NOT NULL  PRIMARY KEY              │ │
│  │ DEPT_NOM   VARCHAR(30)  NOT NULL                           │ │
│  │ LOC        VARCHAR(30)                                     │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  TABLE EMPLOYEE                                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ EMP_NUM    INTEGER      NOT NULL  PRIMARY KEY              │ │
│  │ EMP_NOM    VARCHAR(30)  NOT NULL                           │ │
│  │ POSTE      VARCHAR(20)                                     │ │
│  │ DIR        INTEGER                 REFERENCES EMPLOYEE     │ │
│  │ DATE_EMB   DATE                                            │ │
│  │ SAL        DECIMAL(7,2)                                    │ │
│  │ COMM       DECIMAL(7,2)                                    │ │
│  │ DEPT_NUM   SMALLINT     NOT NULL  REFERENCES DEPT          │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  Relations :                                                     │
│  • EMPLOYEE.DEPT_NUM → DEPT.DEPT_NUM (N-1)                      │
│  • EMPLOYEE.DIR → EMPLOYEE.EMP_NUM (auto-référence)            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## III-5 : Vocabulaire Relationnel

### Correspondance terminologique

| Terme académique | Terme commercial | Exemple |
|------------------|------------------|---------|
| **Domaine** | Type de données | INTEGER, VARCHAR |
| **Relation** | Table | EMPLOYEE |
| **Attribut** | Colonne | EMP_NOM |
| **N-uplet** (Tuple) | Ligne (Enregistrement) | (7369, 'ARTHUR', ...) |
| **Degré** | Nombre de colonnes | 8 colonnes |
| **Cardinalité** | Nombre de lignes | 14 enregistrements |

### Schéma d'une relation

```
┌─────────────────────────────────────────────────────────────────┐
│              ANATOMIE D'UNE TABLE (RELATION)                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE : EMPLOYEE (nom de la relation)                           │
│  Schéma : EMPLOYEE(EMP_NUM, EMP_NOM, POSTE, SAL, DEPT_NUM)      │
│                                                                  │
│                     ATTRIBUTS (colonnes)                        │
│                 ┌───────┬─────────┬───────┬───────┬──────────┐  │
│                 │EMP_NUM│ EMP_NOM │ POSTE │  SAL  │ DEPT_NUM │  │
│  ┌──────────────┼───────┼─────────┼───────┼───────┼──────────┤  │
│  │   TUPLE 1    │ 7369  │ ARTHUR  │ AGENT │  800  │    20    │  │
│  ├──────────────┼───────┼─────────┼───────┼───────┼──────────┤  │
│  │   TUPLE 2    │ 7499  │ PAUL    │VENDEUR│ 1600  │    30    │  │
│  ├──────────────┼───────┼─────────┼───────┼───────┼──────────┤  │
│  │   TUPLE 3    │ 7521  │ MARTIN  │VENDEUR│ 1250  │    30    │  │
│  └──────────────┴───────┴─────────┴───────┴───────┴──────────┘  │
│                                                                  │
│  DEGRÉ = 5 (nombre de colonnes)                                 │
│  CARDINALITÉ = 3 (nombre de lignes)                             │
│                                                                  │
│  DOMAINES :                                                      │
│  • EMP_NUM  : INTEGER                                           │
│  • EMP_NOM  : VARCHAR(30)                                       │
│  • POSTE    : VARCHAR(20)                                       │
│  • SAL      : DECIMAL(7,2)                                      │
│  • DEPT_NUM : SMALLINT                                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE III - RÉSUMÉ                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  III-1 CYCLE DE CONCEPTION                                      │
│        Besoins → MCD → Normalisation → MLD → MPD (DDL)         │
│                                                                  │
│  III-2 MCD (MODÈLE CONCEPTUEL)                                  │
│        • Entités : objets du métier                             │
│        • Attributs : propriétés                                 │
│        • Relations : liens entre entités                        │
│        • Cardinalités : 1-1, 1-N, N-N                          │
│                                                                  │
│  III-3 NORMALISATION                                            │
│        • 1NF : attributs atomiques, clé primaire               │
│        • 2NF : pas de dépendance partielle                     │
│        • 3NF : pas de dépendance transitive                    │
│        • Objectif : éliminer les anomalies                     │
│                                                                  │
│  III-4 TRANSFORMATION MCD → MLD                                 │
│        • Entité → Table                                         │
│        • Relation 1-N → Clé étrangère                          │
│        • Relation N-N → Table d'association                    │
│                                                                  │
│  III-5 VOCABULAIRE                                              │
│        • Relation = Table                                       │
│        • Attribut = Colonne                                     │
│        • Tuple = Ligne                                          │
│        • Degré = Nb colonnes, Cardinalité = Nb lignes          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Questions de compréhension

1. **Cycle de conception**
   - Quelles sont les 5 étapes du cycle de conception d'une BD ?
   - Quelle est la différence entre MCD et MLD ?

2. **MCD**
   - Qu'est-ce qu'une entité ? Un attribut ? Une relation ?
   - Expliquez les cardinalités (0,1), (1,1), (0,n), (1,n).

3. **Normalisation**
   - Pourquoi normalise-t-on une base de données ?
   - Quelle est la règle de la 3NF ?

4. **Transformation**
   - Comment transforme-t-on une relation 1-N en MLD ?
   - Comment gère-t-on une relation N-N ?

### Exercice pratique : Bibliothèque

Concevez le MCD et le MLD pour une bibliothèque :

**Besoins :**
- Un livre a un ISBN, un titre, un auteur, une année de publication
- Un adhérent a un numéro, un nom, une adresse, un téléphone
- Un adhérent peut emprunter plusieurs livres
- Un livre peut être emprunté par plusieurs adhérents (successivement)
- On veut garder l'historique des emprunts (date emprunt, date retour)

**Questions :**
1. Identifiez les entités et leurs attributs
2. Identifiez les relations et leurs cardinalités
3. Dessinez le MCD
4. Appliquez les règles de transformation pour obtenir le MLD
5. Vérifiez que le MLD est en 3NF

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre II - Architecture DB2](02-architecture-db2.md) | [Chapitre IV - Modèle relationnel](04-modele-relationnel.md) |

---
*Formation DB2/SQL - M2i Formation*
