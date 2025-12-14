# Chapitre VI - SQL DDL (Data Definition Language)

## VI-1 : Introduction au DDL

### Rôle du DDL

Le **DDL** (Data Definition Language) permet de définir et modifier la structure des objets de la base de données.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDES DDL                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  CREATE  ──► Créer un nouvel objet                              │
│  ALTER   ──► Modifier un objet existant                         │
│  DROP    ──► Supprimer un objet                                 │
│                                                                  │
│  Objets concernés :                                              │
│  • DATABASE, TABLESPACE, STOGROUP                               │
│  • TABLE, INDEX, VIEW                                           │
│  • SYNONYM, ALIAS                                               │
│  • SEQUENCE, TRIGGER, PROCEDURE                                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## VI-2 : CREATE TABLE

### Syntaxe de base

```sql
CREATE TABLE nom_table (
    colonne1  type  [contraintes],
    colonne2  type  [contraintes],
    ...
    [CONSTRAINT nom_contrainte contrainte_table]
);
```

### Exemple complet

```sql
-- Table DEPT (parent)
CREATE TABLE DEPT (
    DEPT_NUM    SMALLINT      NOT NULL,
    DEPT_NOM    VARCHAR(30)   NOT NULL,
    LOC         VARCHAR(30),
    CONSTRAINT PK_DEPT PRIMARY KEY (DEPT_NUM)
);

-- Table EMPLOYEE (enfant)
CREATE TABLE EMPLOYEE (
    EMP_NUM     INTEGER       NOT NULL,
    EMP_NOM     VARCHAR(30)   NOT NULL,
    POSTE       VARCHAR(20),
    DIR         INTEGER,
    DATE_EMB    DATE          DEFAULT CURRENT_DATE,
    SAL         DECIMAL(7,2)  NOT NULL CHECK (SAL > 0),
    COMM        DECIMAL(7,2),
    DEPT_NUM    SMALLINT      NOT NULL,

    CONSTRAINT PK_EMPLOYEE PRIMARY KEY (EMP_NUM),
    CONSTRAINT FK_EMP_MGR FOREIGN KEY (DIR)
               REFERENCES EMPLOYEE(EMP_NUM),
    CONSTRAINT FK_EMP_DEPT FOREIGN KEY (DEPT_NUM)
               REFERENCES DEPT(DEPT_NUM)
               ON DELETE RESTRICT
);
```

### Types de contraintes

| Contrainte | Niveau | Syntaxe |
|------------|--------|---------|
| **NOT NULL** | Colonne | `colonne TYPE NOT NULL` |
| **DEFAULT** | Colonne | `colonne TYPE DEFAULT valeur` |
| **UNIQUE** | Colonne/Table | `UNIQUE (col)` |
| **PRIMARY KEY** | Colonne/Table | `PRIMARY KEY (col)` |
| **FOREIGN KEY** | Table | `FOREIGN KEY (col) REFERENCES table(col)` |
| **CHECK** | Colonne/Table | `CHECK (condition)` |

### CREATE TABLE AS (copie)

```sql
-- Créer une table à partir d'une requête
CREATE TABLE EMP_DEPT30 AS (
    SELECT EMP_NUM, EMP_NOM, SAL
    FROM EMPLOYEE
    WHERE DEPT_NUM = 30
) WITH DATA;

-- Créer une structure vide (sans données)
CREATE TABLE EMP_BACKUP LIKE EMPLOYEE;
```

---

## VI-3 : ALTER TABLE

### Ajouter une colonne

```sql
ALTER TABLE EMPLOYEE
ADD EMAIL VARCHAR(100);

-- Avec contrainte
ALTER TABLE EMPLOYEE
ADD TELEPHONE VARCHAR(20) DEFAULT 'N/A';
```

### Modifier une colonne

```sql
-- Augmenter la taille d'un VARCHAR
ALTER TABLE EMPLOYEE
ALTER COLUMN EMP_NOM SET DATA TYPE VARCHAR(50);

-- Ajouter une valeur par défaut
ALTER TABLE EMPLOYEE
ALTER COLUMN COMM SET DEFAULT 0;

-- Supprimer la valeur par défaut
ALTER TABLE EMPLOYEE
ALTER COLUMN COMM DROP DEFAULT;
```

### Ajouter/Supprimer des contraintes

```sql
-- Ajouter une contrainte CHECK
ALTER TABLE EMPLOYEE
ADD CONSTRAINT CK_EMAIL CHECK (EMAIL LIKE '%@%');

-- Ajouter une contrainte UNIQUE
ALTER TABLE EMPLOYEE
ADD CONSTRAINT UK_EMAIL UNIQUE (EMAIL);

-- Supprimer une contrainte
ALTER TABLE EMPLOYEE
DROP CONSTRAINT CK_EMAIL;

-- Ajouter une clé étrangère
ALTER TABLE EMPLOYEE
ADD CONSTRAINT FK_NEW FOREIGN KEY (DEPT_NUM)
    REFERENCES DEPT(DEPT_NUM);
```

### Renommer (DB2 12+)

```sql
-- Renommer une colonne
ALTER TABLE EMPLOYEE
RENAME COLUMN EMP_NOM TO EMPLOYEE_NAME;

-- Renommer une table
RENAME TABLE EMPLOYEE TO EMP;
```

---

## VI-4 : DROP TABLE

### Syntaxe

```sql
-- Suppression simple
DROP TABLE nom_table;

-- Ordre de suppression important pour les FK !
-- Supprimer les tables enfants AVANT les tables parents
DROP TABLE EMPLOYEE;   -- Enfant d'abord
DROP TABLE DEPT;       -- Parent ensuite
```

### Attention aux dépendances

```
┌─────────────────────────────────────────────────────────────────┐
│              ORDRE DE SUPPRESSION DES TABLES                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  DEPT (parent)                                                   │
│    ▲                                                             │
│    │ FK                                                          │
│    │                                                             │
│  EMPLOYEE (enfant)                                               │
│                                                                  │
│  DROP TABLE DEPT;     ──► ERREUR si EMPLOYEE existe             │
│  DROP TABLE EMPLOYEE; ──► OK                                     │
│  DROP TABLE DEPT;     ──► OK (plus de dépendance)               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## VI-5 : CREATE INDEX

### Syntaxe

```sql
-- Index simple
CREATE INDEX IDX_EMP_NOM ON EMPLOYEE(EMP_NOM);

-- Index unique
CREATE UNIQUE INDEX IDX_EMP_EMAIL ON EMPLOYEE(EMAIL);

-- Index composite (plusieurs colonnes)
CREATE INDEX IDX_EMP_DEPT_SAL ON EMPLOYEE(DEPT_NUM, SAL DESC);

-- Index avec ordre
CREATE INDEX IDX_EMP_SAL ON EMPLOYEE(SAL DESC);
```

### Types d'index DB2

| Type | Usage | Création |
|------|-------|----------|
| **UNIQUE** | Garantit l'unicité | `CREATE UNIQUE INDEX` |
| **Non-unique** | Performance recherche | `CREATE INDEX` |
| **Clustering** | Ordonne physiquement | `CLUSTER` dans tablespace |
| **Include** | Colonnes additionnelles | `INCLUDE (col)` |

### DROP INDEX

```sql
DROP INDEX IDX_EMP_NOM;
```

---

## VI-6 : CREATE VIEW

### Définition

Une **vue** est une table virtuelle basée sur une requête SELECT. Elle ne stocke pas de données mais offre une vue filtrée ou transformée des données.

### Syntaxe

```sql
CREATE VIEW nom_vue AS
SELECT ...
FROM ...
WHERE ...;
```

### Exemples

```sql
-- Vue simple : employés du département 30
CREATE VIEW V_EMP_VENTES AS
SELECT EMP_NUM, EMP_NOM, SAL, COMM
FROM EMPLOYEE
WHERE DEPT_NUM = 30;

-- Vue avec jointure
CREATE VIEW V_EMP_DEPT AS
SELECT E.EMP_NUM, E.EMP_NOM, E.SAL,
       D.DEPT_NOM, D.LOC
FROM EMPLOYEE E
INNER JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM;

-- Vue avec calcul
CREATE VIEW V_EMP_ANNUAL_SAL AS
SELECT EMP_NUM, EMP_NOM, SAL * 12 AS SAL_ANNUEL
FROM EMPLOYEE;

-- Vue avec restriction (WITH CHECK OPTION)
CREATE VIEW V_EMP_RECHERCHE AS
SELECT *
FROM EMPLOYEE
WHERE DEPT_NUM = 20
WITH CHECK OPTION;
-- Empêche INSERT/UPDATE qui sortiraient de la vue
```

### Avantages des vues

| Avantage | Description |
|----------|-------------|
| **Sécurité** | Masquer des colonnes sensibles |
| **Simplicité** | Simplifier les requêtes complexes |
| **Abstraction** | Isoler les applications de la structure physique |
| **Cohérence** | Garantir une vue uniforme des données |

### DROP VIEW

```sql
DROP VIEW V_EMP_VENTES;
```

---

## VI-7 : Autres objets DDL

### SYNONYM et ALIAS

```sql
-- Créer un alias (pointeur local)
CREATE ALIAS EMP FOR EMPLOYEE;

-- Créer un synonyme public
CREATE SYNONYM PUBLIC.EMPLOYE FOR HRSCHEMA.EMPLOYEE;

-- Utilisation
SELECT * FROM EMP;  -- Équivalent à SELECT * FROM EMPLOYEE
```

### SEQUENCE (DB2 8+)

```sql
-- Créer une séquence
CREATE SEQUENCE SEQ_EMP_NUM
    START WITH 8000
    INCREMENT BY 1
    NO MAXVALUE
    NO CYCLE
    CACHE 20;

-- Utiliser la séquence
INSERT INTO EMPLOYEE (EMP_NUM, EMP_NOM, DEPT_NUM)
VALUES (NEXT VALUE FOR SEQ_EMP_NUM, 'NOUVEAU', 20);

-- Obtenir la valeur courante
SELECT PREVIOUS VALUE FOR SEQ_EMP_NUM FROM SYSIBM.SYSDUMMY1;
```

### TRIGGER

```sql
-- Trigger pour audit
CREATE TRIGGER TRG_EMP_AUDIT
AFTER UPDATE ON EMPLOYEE
REFERENCING OLD AS O NEW AS N
FOR EACH ROW
BEGIN ATOMIC
    INSERT INTO EMPLOYEE_AUDIT (EMP_NUM, OLD_SAL, NEW_SAL, CHG_DATE)
    VALUES (N.EMP_NUM, O.SAL, N.SAL, CURRENT_TIMESTAMP);
END;
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE VI - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  VI-1 DDL                                                        │
│       • CREATE, ALTER, DROP                                      │
│       • Définit la structure de la base                         │
│                                                                  │
│  VI-2 CREATE TABLE                                               │
│       • Colonnes avec types et contraintes                      │
│       • NOT NULL, DEFAULT, UNIQUE, PK, FK, CHECK                │
│       • CREATE TABLE AS ... WITH DATA                           │
│                                                                  │
│  VI-3 ALTER TABLE                                                │
│       • ADD colonne, ALTER COLUMN, DROP CONSTRAINT              │
│       • Modifier la structure existante                         │
│                                                                  │
│  VI-4 DROP TABLE                                                 │
│       • Supprimer une table                                     │
│       • Attention à l'ordre (FK)                                │
│                                                                  │
│  VI-5 INDEX                                                      │
│       • CREATE [UNIQUE] INDEX                                   │
│       • Améliore les performances de recherche                  │
│                                                                  │
│  VI-6 VIEW                                                       │
│       • Table virtuelle basée sur SELECT                        │
│       • Sécurité, simplicité, abstraction                       │
│       • WITH CHECK OPTION                                        │
│                                                                  │
│  VI-7 AUTRES OBJETS                                              │
│       • ALIAS, SYNONYM, SEQUENCE, TRIGGER                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Exercice 1 : Création de tables

Créez les tables suivantes avec les contraintes appropriées :

1. **CLIENT** (cli_id PK, cli_nom NOT NULL, email UNIQUE, ville)
2. **PRODUIT** (prod_id PK, designation NOT NULL, prix > 0, stock >= 0)
3. **COMMANDE** (cmd_id PK, cli_id FK, date_cmd DEFAULT, statut CHECK)

### Exercice 2 : Modification

1. Ajoutez une colonne `telephone` à CLIENT
2. Créez un index sur `cli_nom`
3. Créez une vue des commandes avec le nom du client

### Exercice 3 : Ordre de création/suppression

Dans quel ordre créer puis supprimer ces tables ? DEPT, EMPLOYEE, AFFECTATION, PROJET (sachant que AFFECTATION référence EMPLOYEE et PROJET)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Types et DB2I](05-types-db2i.md) | [Chapitre VII - SQL DML](07-sql-dml.md) |

---
*Formation DB2/SQL - M2i Formation*
