# Chapitre IV - Le Modèle Relationnel

## IV-1 : Propriétés d'une Table

### Structure d'une table relationnelle

Une **table** (ou relation) est l'élément fondamental du modèle relationnel. Elle possède des propriétés bien définies.

```
┌─────────────────────────────────────────────────────────────────┐
│                    STRUCTURE D'UNE TABLE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE : EMPLOYEE                                                │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │          ATTRIBUTS (Colonnes)                               ││
│  │  ┌────────┬─────────┬────────┬────────┬──────┬──────────┐  ││
│  │  │EMP_NUM │ EMP_NOM │ POSTE  │  SAL   │ COMM │ DEPT_NUM │  ││
│  │  ├────────┼─────────┼────────┼────────┼──────┼──────────┤  ││
│  │  │  7369  │ ARTHUR  │ AGENT  │   800  │ NULL │    20    │  ││
│  │  │  7499  │ PAUL    │VENDEUR │  1600  │  300 │    30    │  ││
│  │  │  7521  │ MARTIN  │VENDEUR │  1250  │  500 │    30    │  ││
│  │  │  7566  │ ROGER   │MANAGER │  2975  │ NULL │    20    │  ││
│  │  │  7654  │ ALLEN   │VENDEUR │  1250  │ 1400 │    30    │  ││
│  │  └────────┴─────────┴────────┴────────┴──────┴──────────┘  ││
│  │                                                              ││
│  │  ◄──────────────── TUPLES (Lignes) ─────────────────────►   ││
│  └─────────────────────────────────────────────────────────────┘│
│                                                                  │
│  Propriétés :                                                    │
│  • NOM unique dans le schéma                                    │
│  • DEGRÉ = 6 (nombre de colonnes)                               │
│  • CARDINALITÉ = 5 (nombre de lignes)                           │
│  • Chaque colonne a un TYPE (domaine)                           │
│  • L'ordre des colonnes n'a pas d'importance                    │
│  • L'ordre des lignes n'a pas d'importance                      │
│  • Pas de lignes en double (unicité garantie par la PK)        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Propriétés fondamentales

| Propriété | Description |
|-----------|-------------|
| **Nom** | Identifiant unique de la table dans le schéma |
| **Colonnes** | Attributs avec nom et type de données |
| **Lignes** | Enregistrements (tuples) de données |
| **Degré** | Nombre de colonnes |
| **Cardinalité** | Nombre de lignes |
| **Unicité** | Pas de lignes identiques (clé primaire) |

---

## IV-2 : Clé Primaire (Primary Key)

### Définition

La **clé primaire** (PK - Primary Key) est un attribut ou un ensemble d'attributs qui identifie de manière **unique** chaque ligne d'une table.

```
┌─────────────────────────────────────────────────────────────────┐
│                      CLÉ PRIMAIRE (PK)                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Caractéristiques :                                              │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  1. UNIQUE      : Valeur différente pour chaque ligne   │    │
│  │  2. NOT NULL    : Jamais de valeur nulle                │    │
│  │  3. STABLE      : Ne doit pas changer après création    │    │
│  │  4. MINIMALE    : Pas d'attribut superflu (si composée) │    │
│  │  5. UNE SEULE   : Une seule PK par table                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Exemple :                                                       │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │  TABLE EMPLOYEE                                             │ │
│  │  ┌────────┬─────────┬────────┬──────────┐                 │ │
│  │  │EMP_NUM │ EMP_NOM │ POSTE  │ DEPT_NUM │                 │ │
│  │  │  (PK)  │         │        │          │                 │ │
│  │  ├────────┼─────────┼────────┼──────────┤                 │ │
│  │  │  7369  │ ARTHUR  │ AGENT  │    20    │  ← 7369 unique  │ │
│  │  │  7499  │ PAUL    │VENDEUR │    30    │  ← 7499 unique  │ │
│  │  │  7499  │ DUPONT  │MANAGER │    10    │  ← INTERDIT!    │ │
│  │  └────────┴─────────┴────────┴──────────┘                 │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de clés primaires

```
┌─────────────────────────────────────────────────────────────────┐
│                  TYPES DE CLÉS PRIMAIRES                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. CLÉ SIMPLE (1 attribut)                                     │
│     ┌────────────────────────────────────────┐                  │
│     │ EMPLOYEE                               │                  │
│     │ EMP_NUM INTEGER PRIMARY KEY            │                  │
│     │ EMP_NOM VARCHAR(30)                    │                  │
│     └────────────────────────────────────────┘                  │
│                                                                  │
│  2. CLÉ COMPOSITE (plusieurs attributs)                         │
│     ┌────────────────────────────────────────┐                  │
│     │ LIGNE_COMMANDE                         │                  │
│     │ CMD_NUM  INTEGER  ─┬─ PRIMARY KEY      │                  │
│     │ LIGNE_NUM INTEGER ─┘                   │                  │
│     │ QUANTITE INTEGER                       │                  │
│     └────────────────────────────────────────┘                  │
│                                                                  │
│  3. CLÉ NATURELLE (attribut métier existant)                   │
│     ┌────────────────────────────────────────┐                  │
│     │ PAYS                                   │                  │
│     │ CODE_ISO CHAR(2) PRIMARY KEY  ← 'FR'   │                  │
│     │ NOM_PAYS VARCHAR(50)                   │                  │
│     └────────────────────────────────────────┘                  │
│                                                                  │
│  4. CLÉ ARTIFICIELLE (générée par le système)                  │
│     ┌────────────────────────────────────────┐                  │
│     │ CLIENT                                 │                  │
│     │ CLIENT_ID INTEGER GENERATED ALWAYS     │                  │
│     │           AS IDENTITY PRIMARY KEY      │                  │
│     │ CLIENT_NOM VARCHAR(50)                 │                  │
│     └────────────────────────────────────────┘                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Syntaxe SQL

```sql
-- Clé primaire simple (inline)
CREATE TABLE DEPT (
    DEPT_NUM   SMALLINT NOT NULL PRIMARY KEY,
    DEPT_NOM   VARCHAR(30) NOT NULL,
    LOC        VARCHAR(30)
);

-- Clé primaire simple (constraint nommée)
CREATE TABLE DEPT (
    DEPT_NUM   SMALLINT NOT NULL,
    DEPT_NOM   VARCHAR(30) NOT NULL,
    LOC        VARCHAR(30),
    CONSTRAINT PK_DEPT PRIMARY KEY (DEPT_NUM)
);

-- Clé primaire composite
CREATE TABLE LIGNE_CMD (
    CMD_NUM    INTEGER NOT NULL,
    LIGNE_NUM  INTEGER NOT NULL,
    PROD_NUM   INTEGER NOT NULL,
    QUANTITE   INTEGER NOT NULL,
    CONSTRAINT PK_LIGNE PRIMARY KEY (CMD_NUM, LIGNE_NUM)
);
```

---

## IV-3 : Clé Étrangère (Foreign Key)

### Définition

La **clé étrangère** (FK - Foreign Key) est un attribut qui fait référence à la clé primaire d'une autre table. Elle assure l'**intégrité référentielle**.

```
┌─────────────────────────────────────────────────────────────────┐
│                    CLÉ ÉTRANGÈRE (FK)                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Caractéristiques :                                              │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  1. RÉFÉRENCE la PK d'une autre table (table parent)    │    │
│  │  2. Peut être NULL (sauf si NOT NULL explicite)         │    │
│  │  3. PLUSIEURS FK possibles par table                    │    │
│  │  4. Garantit l'INTÉGRITÉ RÉFÉRENTIELLE                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Schéma :                                                        │
│                                                                  │
│  TABLE PARENT (DEPT)          TABLE ENFANT (EMPLOYEE)           │
│  ┌─────────────────────┐      ┌─────────────────────────────┐   │
│  │ DEPT_NUM (PK)       │◄─────│ DEPT_NUM (FK)               │   │
│  │ DEPT_NOM            │      │ EMP_NUM (PK)                │   │
│  │ LOC                 │      │ EMP_NOM                     │   │
│  └─────────────────────┘      └─────────────────────────────┘   │
│                                                                  │
│  Exemple de données :                                            │
│                                                                  │
│  DEPT                         EMPLOYEE                          │
│  ┌────────┬───────────┐      ┌────────┬─────────┬────────┐     │
│  │DEPT_NUM│ DEPT_NOM  │      │EMP_NUM │ EMP_NOM │DEPT_NUM│     │
│  ├────────┼───────────┤      ├────────┼─────────┼────────┤     │
│  │   10   │COMPTABILITE│      │ 7369   │ ARTHUR  │   20   │     │
│  │   20   │ RECHERCHE │◄─────│ 7499   │ PAUL    │   30   │     │
│  │   30   │  VENTES   │◄─────│ 7521   │ MARTIN  │   30   │     │
│  │   40   │EXPLOITATION│      │ 7566   │ ROGER   │   20   │     │
│  └────────┴───────────┘      └────────┴─────────┴────────┘     │
│                                       │                         │
│  INTERDIT : INSERT EMPLOYEE avec DEPT_NUM = 50 (n'existe pas)  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Intégrité référentielle

```
┌─────────────────────────────────────────────────────────────────┐
│                  INTÉGRITÉ RÉFÉRENTIELLE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Règle : Une FK doit référencer une valeur EXISTANTE            │
│          dans la table parent (ou être NULL si autorisé)        │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  OPÉRATION           │  VÉRIFICATION                    │    │
│  ├─────────────────────────────────────────────────────────┤    │
│  │  INSERT enfant       │  FK doit exister dans parent     │    │
│  │  UPDATE FK enfant    │  Nouvelle valeur doit exister    │    │
│  │  DELETE parent       │  Pas d'enfant référençant ?      │    │
│  │  UPDATE PK parent    │  Pas d'enfant référençant ?      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Exemples :                                                      │
│                                                                  │
│  -- AUTORISÉ : DEPT_NUM 30 existe                               │
│  INSERT INTO EMPLOYEE VALUES (9999, 'TEST', 30);                │
│                                                                  │
│  -- REFUSÉ : DEPT_NUM 99 n'existe pas                           │
│  INSERT INTO EMPLOYEE VALUES (9998, 'TEST', 99);                │
│  -- Erreur : SQLCODE -530 (foreign key violation)               │
│                                                                  │
│  -- REFUSÉ : Des employés sont dans DEPT 30                     │
│  DELETE FROM DEPT WHERE DEPT_NUM = 30;                          │
│  -- Erreur : SQLCODE -532 (delete restricted)                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Syntaxe SQL

```sql
-- Clé étrangère simple
CREATE TABLE EMPLOYEE (
    EMP_NUM    INTEGER NOT NULL PRIMARY KEY,
    EMP_NOM    VARCHAR(30) NOT NULL,
    DEPT_NUM   SMALLINT NOT NULL,
    CONSTRAINT FK_EMP_DEPT
        FOREIGN KEY (DEPT_NUM) REFERENCES DEPT(DEPT_NUM)
);

-- Auto-référence (employé → manager)
CREATE TABLE EMPLOYEE (
    EMP_NUM    INTEGER NOT NULL PRIMARY KEY,
    EMP_NOM    VARCHAR(30) NOT NULL,
    DIR        INTEGER,
    DEPT_NUM   SMALLINT NOT NULL,
    CONSTRAINT FK_EMP_MGR
        FOREIGN KEY (DIR) REFERENCES EMPLOYEE(EMP_NUM),
    CONSTRAINT FK_EMP_DEPT
        FOREIGN KEY (DEPT_NUM) REFERENCES DEPT(DEPT_NUM)
);
```

---

## IV-4 : Comportements des Clés Étrangères

### Actions ON DELETE

```
┌─────────────────────────────────────────────────────────────────┐
│              COMPORTEMENTS ON DELETE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Que faire quand on supprime une ligne PARENT référencée ?      │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  ON DELETE RESTRICT (défaut)                            │    │
│  │  ───────────────────────────                            │    │
│  │  Interdit la suppression si des enfants existent        │    │
│  │                                                          │    │
│  │  DELETE FROM DEPT WHERE DEPT_NUM = 30;                  │    │
│  │  → ERREUR : Employés existent dans ce département       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  ON DELETE CASCADE                                       │    │
│  │  ──────────────────                                      │    │
│  │  Supprime automatiquement tous les enfants              │    │
│  │                                                          │    │
│  │  DELETE FROM DEPT WHERE DEPT_NUM = 30;                  │    │
│  │  → Supprime DEPT 30 ET tous les employés du DEPT 30    │    │
│  │                                                          │    │
│  │  ⚠ DANGEREUX : peut supprimer beaucoup de données       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  ON DELETE SET NULL                                      │    │
│  │  ────────────────────                                    │    │
│  │  Met la FK des enfants à NULL                           │    │
│  │                                                          │    │
│  │  DELETE FROM DEPT WHERE DEPT_NUM = 30;                  │    │
│  │  → Supprime DEPT 30                                     │    │
│  │  → EMPLOYEE.DEPT_NUM devient NULL pour ces employés     │    │
│  │                                                          │    │
│  │  Prérequis : DEPT_NUM doit accepter NULL                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Actions ON UPDATE

| Action | Comportement |
|--------|--------------|
| **RESTRICT** | Interdit la modification de la PK parent si référencée |
| **CASCADE** | Propage la modification aux FK des enfants |
| **SET NULL** | Met les FK des enfants à NULL |

### Syntaxe complète

```sql
CREATE TABLE EMPLOYEE (
    EMP_NUM    INTEGER NOT NULL PRIMARY KEY,
    EMP_NOM    VARCHAR(30) NOT NULL,
    DEPT_NUM   SMALLINT,
    CONSTRAINT FK_EMP_DEPT
        FOREIGN KEY (DEPT_NUM)
        REFERENCES DEPT(DEPT_NUM)
        ON DELETE SET NULL
        ON UPDATE RESTRICT
);
```

---

## IV-5 : Contraintes d'Intégrité

### Vue d'ensemble

```
┌─────────────────────────────────────────────────────────────────┐
│               CONTRAINTES D'INTÉGRITÉ DB2                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  NOT NULL                                                │    │
│  │  ────────                                                │    │
│  │  La colonne ne peut pas contenir de valeur nulle        │    │
│  │                                                          │    │
│  │  EMP_NOM VARCHAR(30) NOT NULL                           │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  UNIQUE                                                  │    │
│  │  ──────                                                  │    │
│  │  Toutes les valeurs de la colonne doivent être uniques  │    │
│  │  (NULL autorisé, et plusieurs NULL possibles)           │    │
│  │                                                          │    │
│  │  EMAIL VARCHAR(100) UNIQUE                              │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  PRIMARY KEY                                             │    │
│  │  ───────────                                             │    │
│  │  = UNIQUE + NOT NULL                                     │    │
│  │  Identifie chaque ligne de manière unique               │    │
│  │                                                          │    │
│  │  EMP_NUM INTEGER PRIMARY KEY                            │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  FOREIGN KEY                                             │    │
│  │  ───────────                                             │    │
│  │  Référence une PK d'une autre table                     │    │
│  │  Garantit l'intégrité référentielle                     │    │
│  │                                                          │    │
│  │  FOREIGN KEY (DEPT_NUM) REFERENCES DEPT(DEPT_NUM)       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  CHECK                                                   │    │
│  │  ─────                                                   │    │
│  │  Vérifie une condition sur les valeurs                  │    │
│  │                                                          │    │
│  │  SAL DECIMAL(7,2) CHECK (SAL > 0)                       │    │
│  │  SEXE CHAR(1) CHECK (SEXE IN ('M', 'F'))                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  DEFAULT                                                 │    │
│  │  ───────                                                 │    │
│  │  Valeur par défaut si non spécifiée                     │    │
│  │                                                          │    │
│  │  DATE_CREATION DATE DEFAULT CURRENT_DATE                │    │
│  │  STATUT CHAR(1) DEFAULT 'A'                             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Tableau récapitulatif

| Contrainte | NULL autorisé ? | Duplicata autorisés ? | Usage |
|------------|-----------------|----------------------|-------|
| **NOT NULL** | Non | Oui | Champ obligatoire |
| **UNIQUE** | Oui | Non | Valeurs distinctes |
| **PRIMARY KEY** | Non | Non | Identifiant unique |
| **FOREIGN KEY** | Configurable | Oui | Référence externe |
| **CHECK** | Configurable | Configurable | Règle de validation |
| **DEFAULT** | N/A | N/A | Valeur par défaut |

### Exemple complet

```sql
CREATE TABLE EMPLOYEE (
    -- Clé primaire
    EMP_NUM     INTEGER       NOT NULL,

    -- Champs obligatoires
    EMP_NOM     VARCHAR(30)   NOT NULL,

    -- Contrainte CHECK
    POSTE       VARCHAR(20)   NOT NULL
                CHECK (POSTE IN ('AGENT', 'VENDEUR', 'MANAGER',
                                 'ANALYSTE', 'PDG')),

    -- Auto-référence (nullable pour le PDG)
    DIR         INTEGER,

    -- Date avec défaut
    DATE_EMB    DATE          NOT NULL DEFAULT CURRENT_DATE,

    -- Contrainte CHECK numérique
    SAL         DECIMAL(7,2)  NOT NULL CHECK (SAL > 0),

    -- Nullable par défaut
    COMM        DECIMAL(7,2)  CHECK (COMM >= 0),

    -- Clé étrangère NOT NULL
    DEPT_NUM    SMALLINT      NOT NULL,

    -- Contraintes nommées
    CONSTRAINT PK_EMPLOYEE PRIMARY KEY (EMP_NUM),
    CONSTRAINT FK_EMP_MGR FOREIGN KEY (DIR)
               REFERENCES EMPLOYEE(EMP_NUM),
    CONSTRAINT FK_EMP_DEPT FOREIGN KEY (DEPT_NUM)
               REFERENCES DEPT(DEPT_NUM),
    CONSTRAINT CK_COMM_SAL CHECK (COMM IS NULL OR COMM <= SAL)
);
```

---

## IV-6 : Index

### Rôle des index

Un **index** est une structure de données qui améliore la vitesse des opérations de recherche sur une table.

```
┌─────────────────────────────────────────────────────────────────┐
│                         INDEX DB2                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Sans index : FULL TABLE SCAN (lecture séquentielle)            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  SELECT * FROM EMPLOYEE WHERE EMP_NOM = 'MARTIN'        │    │
│  │                                                          │    │
│  │  Page 1 → Page 2 → Page 3 → ... → Page N                │    │
│  │  Lit TOUTES les pages pour trouver 'MARTIN'             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Avec index : INDEX SCAN (accès direct)                         │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                                                          │    │
│  │  INDEX sur EMP_NOM (B-tree)                             │    │
│  │                                                          │    │
│  │              [M]                                         │    │
│  │             /   \                                        │    │
│  │          [A-L]  [N-Z]                                   │    │
│  │           /       \                                      │    │
│  │     [ARTHUR]   [MARTIN] ──► Page 3, Row 5              │    │
│  │                                                          │    │
│  │  Trouve directement la position de 'MARTIN'             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Types d'index :                                                 │
│  • UNIQUE INDEX    : valeurs uniques (PK automatique)          │
│  • NON-UNIQUE INDEX: doublons autorisés                        │
│  • CLUSTERING INDEX: ordonne physiquement les données          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Syntaxe

```sql
-- Index simple
CREATE INDEX IDX_EMP_NOM ON EMPLOYEE(EMP_NOM);

-- Index unique
CREATE UNIQUE INDEX IDX_EMP_EMAIL ON EMPLOYEE(EMAIL);

-- Index composite
CREATE INDEX IDX_EMP_DEPT_SAL ON EMPLOYEE(DEPT_NUM, SAL);

-- Suppression
DROP INDEX IDX_EMP_NOM;
```

### Quand créer un index ?

| Créer un index SI... | Ne PAS créer d'index SI... |
|----------------------|----------------------------|
| Colonne dans WHERE fréquent | Table très petite |
| Colonne dans JOIN | Colonne rarement recherchée |
| Colonne dans ORDER BY | Table avec beaucoup d'INSERT/UPDATE |
| Haute sélectivité | Faible sélectivité (peu de valeurs distinctes) |

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE IV - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  IV-1 PROPRIÉTÉS D'UNE TABLE                                    │
│       • Nom, colonnes (attributs), lignes (tuples)              │
│       • Degré (nb colonnes), Cardinalité (nb lignes)           │
│       • Pas de doublons (unicité par PK)                        │
│                                                                  │
│  IV-2 CLÉ PRIMAIRE (PK)                                         │
│       • Identifie de manière unique chaque ligne                │
│       • UNIQUE + NOT NULL + STABLE                              │
│       • Simple, composite, naturelle ou artificielle            │
│                                                                  │
│  IV-3 CLÉ ÉTRANGÈRE (FK)                                        │
│       • Référence la PK d'une autre table                       │
│       • Garantit l'intégrité référentielle                      │
│       • Peut être NULL (si autorisé)                            │
│                                                                  │
│  IV-4 COMPORTEMENTS FK                                          │
│       • ON DELETE : RESTRICT, CASCADE, SET NULL                 │
│       • ON UPDATE : RESTRICT, CASCADE, SET NULL                 │
│                                                                  │
│  IV-5 CONTRAINTES D'INTÉGRITÉ                                   │
│       • NOT NULL, UNIQUE, PRIMARY KEY, FOREIGN KEY              │
│       • CHECK (condition), DEFAULT (valeur)                     │
│                                                                  │
│  IV-6 INDEX                                                      │
│       • Améliore les performances de recherche                  │
│       • B-tree : accès rapide vs scan complet                   │
│       • UNIQUE, NON-UNIQUE, CLUSTERING                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Questions de compréhension

1. **Clé primaire**
   - Quelles sont les 5 caractéristiques d'une clé primaire ?
   - Quelle est la différence entre une clé naturelle et artificielle ?

2. **Clé étrangère**
   - Qu'est-ce que l'intégrité référentielle ?
   - Que se passe-t-il si on essaie d'insérer une FK qui n'existe pas dans la table parent ?

3. **Contraintes**
   - Quelle est la différence entre UNIQUE et PRIMARY KEY ?
   - Donnez un exemple de contrainte CHECK.

4. **Index**
   - Pourquoi les index améliorent-ils les performances ?
   - Dans quels cas ne faut-il PAS créer d'index ?

### Exercice pratique

Créez le schéma SQL pour une base de données de gestion de commandes :

**Tables à créer :**
- CLIENT (client_id, nom, email, ville)
- PRODUIT (produit_id, designation, prix, stock)
- COMMANDE (cmd_id, client_id, date_cmd, statut)
- LIGNE_CMD (cmd_id, ligne_num, produit_id, quantite, prix_unitaire)

**Contraintes à implémenter :**
- Clés primaires appropriées
- Clés étrangères avec comportements appropriés
- Email client unique
- Prix et stock positifs
- Statut commande dans ('EN_COURS', 'VALIDEE', 'EXPEDIEE', 'LIVREE')
- Quantité > 0

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - Modélisation](03-modelisation.md) | [Chapitre V - Types et DB2I](05-types-db2i.md) |

---
*Formation DB2/SQL - M2i Formation*
