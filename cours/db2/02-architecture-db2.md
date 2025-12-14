# Chapitre II - Architecture DB2

## II-1 : Présentation de DB2

### Qu'est-ce que DB2 ?

**DB2** (Database 2) est le système de gestion de bases de données relationnelles (SGBDR) développé par IBM. C'est le SGBD de référence pour les environnements mainframe z/OS.

```
┌─────────────────────────────────────────────────────────────────┐
│                            DB2                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  • SGBD relationnel IBM                                          │
│  • Créé en 1983 (IBM Research - System R)                       │
│  • Standard SQL ANSI/ISO                                         │
│  • Haute performance pour transactions et analytics              │
│                                                                  │
│  Plateformes :                                                   │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  DB2 for z/OS      │  Mainframe IBM Z                   │    │
│  │  DB2 for i         │  IBM Power Systems (AS/400)        │    │
│  │  DB2 for LUW       │  Linux, Unix, Windows              │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### DB2 dans l'écosystème z/OS

```
┌─────────────────────────────────────────────────────────────────┐
│                    ÉCOSYSTÈME z/OS                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                    APPLICATIONS                          │    │
│  │       COBOL / PL/I / Assembleur / Java                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                           │                                      │
│          ┌────────────────┼────────────────┐                    │
│          ▼                ▼                ▼                    │
│   ┌────────────┐   ┌────────────┐   ┌────────────┐             │
│   │    CICS    │   │    IMS     │   │   Batch    │             │
│   │  (Online)  │   │   (DL/I)   │   │   (JCL)    │             │
│   └─────┬──────┘   └─────┬──────┘   └─────┬──────┘             │
│         │                │                │                     │
│         └────────────────┼────────────────┘                     │
│                          ▼                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                       DB2                                │   │
│   │              (SQL, Embedded SQL)                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                          │                                       │
│                          ▼                                       │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                      VSAM                                │   │
│   │             (Stockage physique)                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Caractéristiques principales de DB2

| Caractéristique | Description |
|-----------------|-------------|
| **Performance** | Optimisé pour traiter des millions de transactions/jour |
| **Disponibilité** | 99.999% uptime (5 nines) |
| **Scalabilité** | Data Sharing pour répartir la charge |
| **Sécurité** | Intégration RACF, chiffrement, audit |
| **SQL** | Support complet SQL ANSI/ISO |
| **Intégration** | COBOL, PL/I, Java, CICS, IMS |

---

## II-2 : Hiérarchie des Objets DB2

### Vue d'ensemble

DB2 organise ses objets selon une hiérarchie logique stricte :

```
┌─────────────────────────────────────────────────────────────────┐
│                 HIÉRARCHIE DES OBJETS DB2                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  DB2 SUBSYSTEM (SSID)                                           │
│  │                                                               │
│  ├── DATABASE (Base de données logique)                         │
│  │   │                                                           │
│  │   ├── TABLESPACE (Conteneur de tables)                       │
│  │   │   │                                                       │
│  │   │   └── TABLE (Structure de données)                       │
│  │   │       │                                                   │
│  │   │       ├── COLUMN (Colonne/Attribut)                      │
│  │   │       └── ROW (Ligne/Enregistrement)                     │
│  │   │                                                           │
│  │   └── INDEXSPACE (Conteneur d'index)                         │
│  │       │                                                       │
│  │       └── INDEX (Structure d'accès rapide)                   │
│  │                                                               │
│  └── STOGROUP (Groupe de volumes de stockage)                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Description des objets

| Objet | Description | Exemple |
|-------|-------------|---------|
| **Subsystem** | Instance DB2 en exécution | DB2A, DB2P |
| **Database** | Regroupement logique d'objets | HRDB, FINDB |
| **Stogroup** | Groupe de volumes DASD | SGSSD01 |
| **Tablespace** | Conteneur physique de tables | EMPSPACE |
| **Table** | Structure de données (lignes/colonnes) | EMPLOYEE |
| **Indexspace** | Conteneur physique d'index | EMPIDXSP |
| **Index** | Accès rapide aux données | EMPIDX01 |
| **View** | Vue virtuelle sur une ou plusieurs tables | V_EMPLOYEE |

### Schéma détaillé

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                  │
│                    DB2 SUBSYSTEM : DB2P                          │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                                                            │  │
│  │   DATABASE : HRDB (Human Resources)                       │  │
│  │   ┌──────────────────────────────────────────────────┐   │  │
│  │   │                                                   │   │  │
│  │   │  TABLESPACE : EMPSPACE                           │   │  │
│  │   │  ┌─────────────────────────────────────────────┐│   │  │
│  │   │  │  TABLE : EMPLOYEE                           ││   │  │
│  │   │  │  ┌──────┬─────────┬─────────┬─────────┐   ││   │  │
│  │   │  │  │EMP_ID│ EMP_NOM │  DEPT   │ SALAIRE │   ││   │  │
│  │   │  │  ├──────┼─────────┼─────────┼─────────┤   ││   │  │
│  │   │  │  │ 7369 │ ARTHUR  │   20    │   800   │   ││   │  │
│  │   │  │  │ 7499 │ PAUL    │   30    │  1600   │   ││   │  │
│  │   │  │  └──────┴─────────┴─────────┴─────────┘   ││   │  │
│  │   │  └─────────────────────────────────────────────┘│   │  │
│  │   │                                                   │   │  │
│  │   │  INDEXSPACE : EMPIDXSP                           │   │  │
│  │   │  ┌─────────────────────────────────────────────┐│   │  │
│  │   │  │  INDEX : EMPIDX01 (sur EMP_ID)             ││   │  │
│  │   │  │  7369 → Page 1, Row 1                      ││   │  │
│  │   │  │  7499 → Page 1, Row 2                      ││   │  │
│  │   │  └─────────────────────────────────────────────┘│   │  │
│  │   └──────────────────────────────────────────────────┘   │  │
│  │                                                            │  │
│  │   STOGROUP : SGSSD01                                      │  │
│  │   ┌──────────────────────────────────────────────────┐   │  │
│  │   │  VOL001, VOL002, VOL003 (volumes DASD)          │   │  │
│  │   └──────────────────────────────────────────────────┘   │  │
│  │                                                            │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-3 : Stockage Physique

### STOGROUP (Storage Group)

Un **STOGROUP** définit où les données seront physiquement stockées (volumes DASD).

```sql
CREATE STOGROUP SGSSD01
    VOLUMES (VOL001, VOL002, VOL003)
    VCAT DSNCAT;
```

| Élément | Description |
|---------|-------------|
| **VOLUMES** | Liste des volumes disque DASD |
| **VCAT** | Catalogue VSAM pour les datasets |

### TABLESPACE

Un **tablespace** est le conteneur physique qui stocke une ou plusieurs tables.

```
┌─────────────────────────────────────────────────────────────────┐
│                       TABLESPACE                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Types de tablespace :                                           │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  SIMPLE                                                  │    │
│  │  • 1 table par tablespace                               │    │
│  │  • Plus simple à gérer                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  SEGMENTED                                               │    │
│  │  • Plusieurs tables par tablespace                      │    │
│  │  • Chaque table dans des segments séparés               │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  PARTITIONED                                             │    │
│  │  • Table divisée en partitions                          │    │
│  │  • Parallélisme et maintenance facilitée                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  UNIVERSAL (UTS - Universal Table Space)                │    │
│  │  • Standard moderne DB2 12+                             │    │
│  │  • PBG (Partition By Growth) ou PBR (Partition By Range)│    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Structure VSAM Linear

DB2 stocke physiquement les données dans des **VSAM Linear Data Sets** (LDS).

```
┌─────────────────────────────────────────────────────────────────┐
│              STOCKAGE PHYSIQUE DB2 (VSAM LDS)                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLESPACE                        VSAM LDS                      │
│  ┌─────────────────┐              ┌─────────────────────────┐   │
│  │                 │              │ DSNCAT.HRDB.EMPSPACE.I0001│  │
│  │    EMPSPACE     │  ─────────►  │                         │   │
│  │                 │              │   Pages de données       │   │
│  │  ┌───────────┐  │              │   (4K, 8K, 16K ou 32K)  │   │
│  │  │ TABLE     │  │              │                         │   │
│  │  │ EMPLOYEE  │  │              │   Page 1: Header        │   │
│  │  └───────────┘  │              │   Page 2: Data rows     │   │
│  │                 │              │   Page 3: Data rows     │   │
│  └─────────────────┘              │   ...                   │   │
│                                    └─────────────────────────┘   │
│                                                                  │
│  INDEXSPACE                        VSAM LDS                      │
│  ┌─────────────────┐              ┌─────────────────────────┐   │
│  │    EMPIDXSP     │  ─────────►  │ DSNCAT.HRDB.EMPIDXSP.I0001│ │
│  │  ┌───────────┐  │              │                         │   │
│  │  │ INDEX     │  │              │   Pages d'index         │   │
│  │  │ EMPIDX01  │  │              │   (B-tree structure)    │   │
│  │  └───────────┘  │              │                         │   │
│  └─────────────────┘              └─────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Organisation des pages

```
┌─────────────────────────────────────────────────────────────────┐
│                    STRUCTURE D'UNE PAGE DB2                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Page (4K, 8K, 16K ou 32K)                                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  PAGE HEADER                                             │    │
│  │  • Numéro de page                                       │    │
│  │  • Type de page                                         │    │
│  │  • Espace libre                                         │    │
│  ├─────────────────────────────────────────────────────────┤    │
│  │                                                          │    │
│  │  ROW 1 : 7369 | ARTHUR  | 20 | 800                     │    │
│  │  ROW 2 : 7499 | PAUL    | 30 | 1600                    │    │
│  │  ROW 3 : 7521 | MARTIN  | 30 | 1250                    │    │
│  │  ...                                                     │    │
│  │                                                          │    │
│  │  (Espace libre pour nouvelles lignes)                   │    │
│  │                                                          │    │
│  ├─────────────────────────────────────────────────────────┤    │
│  │  PAGE TRAILER                                            │    │
│  │  • Checksum                                             │    │
│  │  • Pointeurs vers lignes                                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-4 : Catalogue Système (Dictionnaire de Données)

### Définition

Le **catalogue système** (ou dictionnaire de données) est un ensemble de tables DB2 contenant les **métadonnées** : la description de tous les objets de la base.

```
┌─────────────────────────────────────────────────────────────────┐
│                    CATALOGUE SYSTÈME DB2                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Le catalogue contient la description de :                       │
│                                                                  │
│  • TABLES      : nom, colonnes, types, contraintes              │
│  • INDEX       : nom, colonnes indexées, type                   │
│  • TABLESPACES : nom, type, STOGROUP associé                    │
│  • VIEWS       : nom, définition SQL                            │
│  • PLANS       : plans d'exécution des programmes               │
│  • PACKAGES    : sections SQL compilées                         │
│  • PRIVILEGES  : droits d'accès                                 │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  Quand vous exécutez : CREATE TABLE EMPLOYEE ...        │    │
│  │                                                          │    │
│  │  DB2 met à jour automatiquement le catalogue :          │    │
│  │  • SYSIBM.SYSTABLES    (nouvelle entrée)                │    │
│  │  • SYSIBM.SYSCOLUMNS   (colonnes de la table)           │    │
│  │  • SYSIBM.SYSTABLESPACE (si nouveau tablespace)         │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Tables principales du catalogue

| Table Catalogue | Contenu |
|-----------------|---------|
| **SYSIBM.SYSTABLES** | Liste des tables |
| **SYSIBM.SYSCOLUMNS** | Colonnes de chaque table |
| **SYSIBM.SYSINDEXES** | Index définis |
| **SYSIBM.SYSKEYS** | Colonnes des index |
| **SYSIBM.SYSTABLESPACE** | Tablespaces |
| **SYSIBM.SYSVIEWS** | Vues |
| **SYSIBM.SYSRELS** | Relations (foreign keys) |
| **SYSIBM.SYSPLANDEP** | Dépendances des plans |

### Interroger le catalogue

```sql
-- Lister toutes les tables d'un schéma
SELECT NAME, CREATOR, TYPE
FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'HRSCHEMA';

-- Voir les colonnes d'une table
SELECT NAME, COLTYPE, LENGTH, NULLS
FROM SYSIBM.SYSCOLUMNS
WHERE TBNAME = 'EMPLOYEE'
  AND TBCREATOR = 'HRSCHEMA'
ORDER BY COLNO;

-- Trouver les index d'une table
SELECT NAME, UNIQUERULE, CLUSTERING
FROM SYSIBM.SYSINDEXES
WHERE TBNAME = 'EMPLOYEE';
```

### Structure du catalogue

```
┌─────────────────────────────────────────────────────────────────┐
│              ORGANISATION DU CATALOGUE DB2                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SYSIBM (propriétaire système)                                   │
│  │                                                               │
│  ├── SYSTABLES ──────────────────┐                              │
│  │   NAME       │ CREATOR │ TYPE │                              │
│  │   ───────────┼─────────┼──────│                              │
│  │   EMPLOYEE   │ HRSCHEMA│  T   │ (T=Table)                    │
│  │   V_EMP_DEPT │ HRSCHEMA│  V   │ (V=View)                     │
│  │   DEPT       │ HRSCHEMA│  T   │                              │
│  │                                                               │
│  ├── SYSCOLUMNS ─────────────────────────────────┐              │
│  │   NAME    │ TBNAME   │ COLTYPE │LENGTH│NULLS │              │
│  │   ────────┼──────────┼─────────┼──────┼──────│              │
│  │   EMP_NUM │ EMPLOYEE │ INTEGER │  4   │  N   │              │
│  │   EMP_NOM │ EMPLOYEE │ CHAR    │  30  │  N   │              │
│  │   DEPT_NUM│ EMPLOYEE │ SMALLINT│  2   │  Y   │              │
│  │                                                               │
│  ├── SYSINDEXES ─────────────────────────┐                      │
│  │   NAME     │ TBNAME   │ UNIQUERULE    │                      │
│  │   ─────────┼──────────┼───────────────│                      │
│  │   EMPIDX01 │ EMPLOYEE │ P (Primary)   │                      │
│  │   EMPIDX02 │ EMPLOYEE │ D (Duplicate) │                      │
│  │                                                               │
│  └── ...                                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## II-5 : Buffer Pool et Mémoire

### Gestion de la mémoire

DB2 utilise des **buffer pools** pour mettre en cache les pages de données et d'index en mémoire, réduisant les accès disque.

```
┌─────────────────────────────────────────────────────────────────┐
│                   BUFFER POOLS DB2                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                    MÉMOIRE DB2                           │    │
│  │                                                          │    │
│  │  Buffer Pool BP0 (4K pages)                             │    │
│  │  ┌────┬────┬────┬────┬────┬────┬────┬────┐            │    │
│  │  │Page│Page│Page│Page│Page│Page│Page│Page│            │    │
│  │  │ 1  │ 2  │ 3  │ 4  │ 5  │ 6  │ 7  │ 8  │            │    │
│  │  └────┴────┴────┴────┴────┴────┴────┴────┘            │    │
│  │                                                          │    │
│  │  Buffer Pool BP8K0 (8K pages)                           │    │
│  │  ┌────────┬────────┬────────┬────────┐                 │    │
│  │  │ Page 1 │ Page 2 │ Page 3 │ Page 4 │                 │    │
│  │  └────────┴────────┴────────┴────────┘                 │    │
│  │                                                          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                          │                                       │
│                          ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                     DISQUE (DASD)                        │    │
│  │  VSAM Linear Data Sets                                   │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Processus :                                                     │
│  1. SELECT → DB2 cherche dans le buffer pool                    │
│  2. Si présent (HIT) → retour immédiat                          │
│  3. Si absent (MISS) → lecture disque → chargement buffer       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de buffer pools

| Buffer Pool | Page Size | Usage typique |
|-------------|-----------|---------------|
| **BP0** | 4K | Tables standards |
| **BP8K0** | 8K | Tables avec grandes lignes |
| **BP16K0** | 16K | Tables volumineuses |
| **BP32K** | 32K | LOB, très grandes lignes |

---

## II-6 : Journalisation et Recovery

### Log DB2

DB2 maintient des **journaux de transactions** (logs) pour garantir la durabilité et permettre la récupération après panne.

```
┌─────────────────────────────────────────────────────────────────┐
│                    JOURNALISATION DB2                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Transaction                        Log (Journal)                │
│  ┌─────────────────────┐           ┌─────────────────────────┐  │
│  │ BEGIN TRANSACTION   │  ──────►  │ Begin T1               │  │
│  │                     │           │                         │  │
│  │ UPDATE EMPLOYEE ... │  ──────►  │ T1: OLD=800, NEW=900   │  │
│  │                     │           │ (Before/After image)    │  │
│  │ INSERT INTO ...     │  ──────►  │ T1: INSERT row xyz     │  │
│  │                     │           │                         │  │
│  │ COMMIT              │  ──────►  │ Commit T1              │  │
│  └─────────────────────┘           └─────────────────────────┘  │
│                                                                  │
│  Types de logs :                                                 │
│  • ACTIVE LOG   : transactions en cours et récentes             │
│  • ARCHIVE LOG  : transactions anciennes (sur bande/disque)     │
│                                                                  │
│  Utilité :                                                       │
│  • ROLLBACK : annuler une transaction non validée               │
│  • RECOVERY : restaurer après panne système                     │
│  • POINT-IN-TIME : restaurer à un instant précis               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de recovery

| Type | Description | Usage |
|------|-------------|-------|
| **ROLLBACK** | Annule transaction en cours | Erreur applicative |
| **RESTART** | Redémarre DB2 après crash | Panne système |
| **RECOVER** | Restaure objets depuis backup | Corruption données |
| **Point-in-time** | Restaure à un instant précis | Erreur utilisateur |

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAPITRE II - RÉSUMÉ                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  II-1 PRÉSENTATION DB2                                          │
│       • SGBDR IBM pour mainframe z/OS                           │
│       • Haute performance, disponibilité, sécurité              │
│       • Intégration COBOL, CICS, batch                          │
│                                                                  │
│  II-2 HIÉRARCHIE DES OBJETS                                     │
│       Subsystem → Database → Tablespace → Table                 │
│                           → Indexspace → Index                  │
│       STOGROUP : volumes de stockage                            │
│                                                                  │
│  II-3 STOCKAGE PHYSIQUE                                         │
│       • Tablespace = conteneur de tables                        │
│       • Types : Simple, Segmented, Partitioned, Universal       │
│       • VSAM Linear Data Sets (LDS)                             │
│       • Pages 4K, 8K, 16K, 32K                                  │
│                                                                  │
│  II-4 CATALOGUE SYSTÈME                                         │
│       • Métadonnées (description des objets)                    │
│       • Tables SYSIBM.SYS*                                      │
│       • Interrogeable via SQL                                   │
│                                                                  │
│  II-5 BUFFER POOLS                                              │
│       • Cache mémoire pour les pages                            │
│       • Réduit les accès disque                                 │
│       • HIT vs MISS                                             │
│                                                                  │
│  II-6 JOURNALISATION                                            │
│       • Active log, Archive log                                 │
│       • Before/After images                                     │
│       • Recovery, Rollback, Point-in-time                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Questions de compréhension

1. **Hiérarchie DB2**
   - Décrivez la hiérarchie des objets DB2 du subsystem jusqu'à la table.
   - Quel est le rôle d'un STOGROUP ?

2. **Stockage**
   - Quelle est la différence entre un tablespace et un indexspace ?
   - Pourquoi DB2 utilise-t-il des VSAM Linear Data Sets ?

3. **Catalogue système**
   - Qu'est-ce que le catalogue système ?
   - Citez 3 tables du catalogue et leur contenu.

4. **Performance**
   - Qu'est-ce qu'un buffer pool ?
   - Expliquez la différence entre un HIT et un MISS.

### Exercice pratique

Écrivez les requêtes SQL pour interroger le catalogue et trouver :

1. Toutes les tables créées par l'utilisateur 'STUDENT'
2. Les colonnes de la table 'EMPLOYEE' avec leurs types
3. Les index définis sur la table 'DEPT'

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Fondamentaux](01-fondamentaux-bd.md) | [Chapitre III - Modélisation](03-modelisation.md) |

---
*Formation DB2/SQL - M2i Formation*
