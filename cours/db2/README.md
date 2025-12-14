# Module DB2/SQL - Bases de Données Relationnelles

## Introduction

DB2 est le système de gestion de bases de données relationnelles (SGBDR) d'IBM, omniprésent dans les environnements mainframe z/OS. Combiné au langage SQL, il permet de stocker, interroger et manipuler des données structurées avec une fiabilité et des performances exceptionnelles.

Ce module couvre les fondamentaux des bases de données relationnelles, l'architecture DB2, le langage SQL complet (DDL, DML, DQL) et l'intégration avec COBOL via l'Embedded SQL.

## Prérequis

- Connaissance de base de l'environnement mainframe (module Z/OS-TSO)
- Familiarité avec ISPF pour l'édition de code
- Notions de JCL pour l'exécution batch
- Bases de programmation COBOL

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

### Fondamentaux (Chapitres I-IV)
- Comprendre les concepts des bases de données relationnelles
- Maîtriser les propriétés ACID et les transactions
- Concevoir un modèle de données (MCD → MLD → tables)
- Appliquer les règles de normalisation (1NF, 2NF, 3NF)
- Définir des clés primaires, étrangères et contraintes d'intégrité

### Architecture DB2 (Chapitre II)
- Comprendre la hiérarchie des objets DB2 (subsystem, database, tablespace, table)
- Connaître le stockage physique (STOGROUP, VSAM Linear)
- Utiliser le catalogue système (métadonnées)

### SQL (Chapitres V-IX)
- Utiliser DB2I et SPUFI sous z/OS
- Créer et modifier des objets (CREATE, ALTER, DROP)
- Manipuler les données (INSERT, UPDATE, DELETE)
- Interroger les données avec SELECT (WHERE, ORDER BY, GROUP BY)
- Maîtriser les jointures (INNER, LEFT/RIGHT OUTER)
- Utiliser les fonctions d'agrégation et sous-requêtes

### Embedded SQL COBOL (Chapitre X)
- Intégrer SQL dans les programmes COBOL
- Utiliser les variables hôtes et DCLGEN
- Gérer les erreurs avec SQLCA et SQLCODE
- Comprendre le processus de précompilation et BIND

## Parcours de formation

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        MODULE DB2/SQL - 10 CHAPITRES                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  PARTIE 1 : FONDAMENTAUX                                                    │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐   ┌──────────────┐ │
│  │  Ch.I        │   │  Ch.II       │   │  Ch.III      │   │  Ch.IV       │ │
│  │  Fondamentaux│──►│  Architecture│──►│  Modélisation│──►│  Modèle      │ │
│  │  BD & SGBD   │   │  DB2         │   │  MCD/MLD     │   │  Relationnel │ │
│  └──────────────┘   └──────────────┘   └──────────────┘   └──────────────┘ │
│                                                                   │         │
├───────────────────────────────────────────────────────────────────┼─────────┤
│                                                                   ▼         │
│  PARTIE 2 : LANGAGE SQL                                    ┌──────────────┐ │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐   │  Ch.V        │ │
│  │  Ch.VIII     │◄──│  Ch.VII      │◄──│  Ch.VI       │◄──│  Types &     │ │
│  │  SELECT &    │   │  DML         │   │  DDL         │   │  DB2I/SPUFI  │ │
│  │  Jointures   │   │  INSERT/UPD  │   │  CREATE/DROP │   └──────────────┘ │
│  └──────────────┘   └──────────────┘   └──────────────┘                     │
│         │                                                                   │
│         ▼                                                                   │
│  ┌──────────────┐                                                           │
│  │  Ch.IX       │                                                           │
│  │  Agrégations │                                                           │
│  │  Sous-requêtes│                                                          │
│  └──────────────┘                                                           │
│         │                                                                   │
├─────────┼───────────────────────────────────────────────────────────────────┤
│         ▼                                                                   │
│  PARTIE 3 : INTÉGRATION COBOL                                              │
│  ┌──────────────┐                                                           │
│  │  Ch.X        │                                                           │
│  │  Embedded    │                                                           │
│  │  SQL COBOL   │                                                           │
│  └──────────────┘                                                           │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Table des matières

### Partie 1 : Fondamentaux

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 01 | [Fondamentaux des bases de données](01-fondamentaux-bd.md) | BD, SGBD, propriétés ACID, avantages |
| 02 | [Architecture DB2](02-architecture-db2.md) | Hiérarchie objets, stockage, catalogue système |
| 03 | [Modélisation des données](03-modelisation.md) | MCD, MLD, normalisation (1NF, 2NF, 3NF) |
| 04 | [Le modèle relationnel](04-modele-relationnel.md) | Tables, PK, FK, contraintes d'intégrité |

### Partie 2 : Langage SQL

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 05 | [Types de données et interaction z/OS](05-types-db2i.md) | Types DB2, DB2I, SPUFI, modes batch/online |
| 06 | [SQL DDL](06-sql-ddl.md) | CREATE, ALTER, DROP TABLE/INDEX/VIEW |
| 07 | [SQL DML](07-sql-dml.md) | INSERT, UPDATE, DELETE, transactions |
| 08 | [SQL SELECT et jointures](08-sql-select.md) | WHERE, ORDER BY, INNER/OUTER JOIN |
| 09 | [Agrégations et sous-requêtes](09-sql-avance.md) | GROUP BY, HAVING, fonctions, sous-requêtes |

### Partie 3 : Intégration COBOL

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 10 | [Embedded SQL COBOL](10-embedded-sql.md) | Variables hôtes, DCLGEN, SQLCA, précompilation |

## Tables d'exemple

Ce module utilise trois tables d'exemple pour illustrer les concepts :

### Table EMPLOYEE
```
EMP_NUM | EMP_NOM  | POSTE    | DIR  | DATE_EMB   | SAL  | COMM | DEPT_NUM
--------|----------|----------|------|------------|------|------|----------
7369    | ARTHUR   | AGENT    | 7902 | 17/12/2020 | 800  | NULL | 20
7499    | PAUL     | VENDEUR  | 7698 | 20/02/2021 | 1600 | 300  | 30
7521    | MARTIN   | VENDEUR  | 7698 | 22/02/2021 | 1250 | 500  | 30
7566    | ROGER    | MANAGER  | 7839 | 02/04/2021 | 2975 | NULL | 20
7654    | ALLEN    | VENDEUR  | 7698 | 28/09/2021 | 1250 | 1400 | 30
7698    | BRIAND   | MANAGER  | 7839 | 01/05/2021 | 2850 | NULL | 30
7782    | CHARLES  | MANAGER  | 7839 | 09/06/2021 | 2450 | NULL | 10
7788    | SCOTT    | ANALYSTE | 7566 | 19/04/2022 | 3000 | NULL | 20
7839    | ROI      | PDG      | NULL | 17/11/2021 | 5000 | NULL | 10
7844    | THOMAS   | VENDEUR  | 7698 | 08/09/2021 | 1500 | 0    | 30
7876    | ADAMS    | AGENT    | 7788 | 23/05/2022 | 1100 | NULL | 20
7900    | JAMES    | AGENT    | 7698 | 03/12/2021 | 950  | NULL | 30
7902    | FORD     | ANALYSTE | 7566 | 03/12/2021 | 3000 | NULL | 20
7934    | MILLER   | AGENT    | 7782 | 23/01/2022 | 1300 | NULL | 10
```

### Table DEPT
```
DEPT_NUM | DEPT_NOM      | LOC
---------|---------------|------------
10       | COMPTABILITE  | MARSEILLE
20       | RECHERCHE     | STRASBOURG
30       | VENTES        | LYON
40       | EXPLOITATION  | PARIS
```

### Table SAL_GRILLE
```
GRADE | MIN_SAL | MAX_SAL
------|---------|--------
1     | 700     | 1200
2     | 1201    | 1400
3     | 1401    | 2000
4     | 2001    | 3000
5     | 3001    | 9999
```

## Progression

- [ ] Chapitre I - Fondamentaux des bases de données
- [ ] Chapitre II - Architecture DB2
- [ ] Chapitre III - Modélisation des données
- [ ] Chapitre IV - Le modèle relationnel
- [ ] Chapitre V - Types de données et interaction z/OS
- [ ] Chapitre VI - SQL DDL
- [ ] Chapitre VII - SQL DML
- [ ] Chapitre VIII - SQL SELECT et jointures
- [ ] Chapitre IX - Agrégations et sous-requêtes
- [ ] Chapitre X - Embedded SQL COBOL

## Aller plus loin

### Documentation officielle IBM

| Ressource | Description |
|-----------|-------------|
| [DB2 for z/OS Documentation](https://www.ibm.com/docs/en/db2-for-zos) | Documentation complète DB2 z/OS |
| [DB2 SQL Reference](https://www.ibm.com/docs/en/db2-for-zos/13?topic=db2-sql) | Référence SQL DB2 |
| [DB2 Application Programming Guide](https://www.ibm.com/docs/en/db2-for-zos/13?topic=programming-application-guide) | Guide de programmation applicative |

### Tutoriels et cours

| Plateforme | Cours | Niveau |
|------------|-------|--------|
| [IBM Skills](https://www.ibm.com/training/db2) | DB2 Training | Tous niveaux |
| [Coursera - IBM](https://www.coursera.org/learn/introduction-to-relational-databases) | Introduction to Relational Databases | Débutant |
| [TutorialsPoint - DB2](https://www.tutorialspoint.com/db2/index.htm) | DB2 Tutorial | Débutant |

### Livres recommandés

| Titre | Auteur | Description |
|-------|--------|-------------|
| *DB2 for z/OS Version 12* | Paolo Bruni et al. | Guide IBM Redbooks complet |
| *DB2 Developer's Guide* | Craig Mullins | Référence classique |
| *SQL Queries for Mere Mortals* | John Viescas | Maîtrise progressive de SQL |

### Outils

| Outil | Description |
|-------|-------------|
| [DBeaver](https://dbeaver.io/) | Client SQL universel gratuit |
| [IBM Data Studio](https://www.ibm.com/products/data-studio) | IDE IBM pour DB2 |
| [SPUFI](https://www.ibm.com/docs/en/db2-for-zos/13?topic=tools-spufi) | Outil SQL interactif z/OS |

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [CICS](../cics/README.md) | - |

---
*Formation COBOL - M2i Formation - Décembre 2025*
