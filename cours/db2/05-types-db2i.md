# Chapitre V - Types de Données et Interaction z/OS

## V-1 : Types de Données DB2

### Vue d'ensemble

DB2 offre une variété de types de données pour stocker efficacement différentes catégories d'informations.

```
┌─────────────────────────────────────────────────────────────────┐
│                   TYPES DE DONNÉES DB2                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  CARACTÈRES                                              │    │
│  │  CHAR, VARCHAR, LONG VARCHAR, CLOB, GRAPHIC             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  NUMÉRIQUES                                              │    │
│  │  SMALLINT, INTEGER, BIGINT, DECIMAL, FLOAT, REAL, DOUBLE│    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  DATE ET TEMPS                                           │    │
│  │  DATE, TIME, TIMESTAMP                                   │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  BINAIRES                                                │    │
│  │  BINARY, VARBINARY, BLOB                                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types caractères

```
┌─────────────────────────────────────────────────────────────────┐
│                    TYPES CARACTÈRES                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  CHAR(n) - Longueur FIXE                                        │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • 1 à 254 octets                                       │    │
│  │  • Complété par des espaces si valeur plus courte      │    │
│  │  • Idéal pour codes fixes (CODE_PAYS CHAR(2))          │    │
│  │                                                          │    │
│  │  Exemple : CHAR(10)                                     │    │
│  │  Valeur 'ABC' stockée comme : 'ABC       ' (10 car.)   │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  VARCHAR(n) - Longueur VARIABLE                                 │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • 1 à 32704 octets                                     │    │
│  │  • Stocke uniquement les caractères effectifs          │    │
│  │  • + 2 octets pour stocker la longueur                 │    │
│  │  • Idéal pour textes de longueur variable (NOM, ADRESSE)│    │
│  │                                                          │    │
│  │  Exemple : VARCHAR(100)                                 │    │
│  │  Valeur 'ABC' stockée comme : [3]'ABC' (5 octets)      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  CLOB(n) - Character Large Object                               │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • Jusqu'à 2 Go                                         │    │
│  │  • Pour les textes très longs (documents, XML)         │    │
│  │  • CLOB(1M), CLOB(100K)                                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Type | Longueur | Usage | Exemple |
|------|----------|-------|---------|
| **CHAR(n)** | 1-254 fixe | Codes, identifiants fixes | CHAR(2) pour pays |
| **VARCHAR(n)** | 1-32704 variable | Noms, descriptions | VARCHAR(100) |
| **LONG VARCHAR** | >32704 | Textes longs (legacy) | - |
| **CLOB(n)** | Jusqu'à 2Go | Documents, XML | CLOB(1M) |

### Types numériques

```
┌─────────────────────────────────────────────────────────────────┐
│                   TYPES NUMÉRIQUES                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ENTIERS                                                         │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  SMALLINT  │ 2 octets │ -32768 à +32767               │    │
│  │  INTEGER   │ 4 octets │ -2.1 milliards à +2.1 milliards│    │
│  │  BIGINT    │ 8 octets │ -9.2 quintillions à +9.2 quint│    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  DÉCIMAUX (précision exacte)                                    │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  DECIMAL(p,s) ou DEC(p,s) ou NUMERIC(p,s)              │    │
│  │                                                          │    │
│  │  • p = précision (nb total de chiffres, max 31)        │    │
│  │  • s = échelle (nb de décimales, max p)                │    │
│  │                                                          │    │
│  │  Exemples :                                             │    │
│  │  • DECIMAL(7,2) → 99999.99 (salaires)                  │    │
│  │  • DECIMAL(10,4) → 999999.9999 (taux)                  │    │
│  │  • DECIMAL(5,0) → 99999 (entier)                       │    │
│  │                                                          │    │
│  │  ⚠ ESSENTIEL pour les montants financiers              │    │
│  │    (pas de problème d'arrondi comme avec FLOAT)        │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  VIRGULE FLOTTANTE (approximation)                              │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  REAL       │ 4 octets │ Simple précision (7 chiffres) │    │
│  │  DOUBLE     │ 8 octets │ Double précision (15 chiffres)│    │
│  │  FLOAT(n)   │ Variable │ n bits de précision           │    │
│  │                                                          │    │
│  │  Usage : calculs scientifiques, statistiques           │    │
│  │  ⚠ NE PAS utiliser pour montants financiers            │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Type | Octets | Plage | Usage |
|------|--------|-------|-------|
| **SMALLINT** | 2 | -32768 à +32767 | Petits entiers, codes |
| **INTEGER** | 4 | ±2.1 milliards | Compteurs, identifiants |
| **BIGINT** | 8 | ±9.2×10^18 | Très grands nombres |
| **DECIMAL(p,s)** | (p+2)/2 | Précision exacte | Montants, pourcentages |
| **FLOAT/DOUBLE** | 4/8 | Approximatif | Calculs scientifiques |

### Types date et temps

```
┌─────────────────────────────────────────────────────────────────┐
│                  TYPES DATE ET TEMPS                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  DATE                                                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • Format interne : AAAAMMJJ (4 octets)                 │    │
│  │  • Plage : 0001-01-01 à 9999-12-31                     │    │
│  │  • Format d'affichage dépend du pays (ISO, EUR, USA)   │    │
│  │                                                          │    │
│  │  ISO : 2024-12-15                                       │    │
│  │  EUR : 15.12.2024                                       │    │
│  │  USA : 12/15/2024                                       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  TIME                                                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • Format interne : HHMMSS (3 octets)                   │    │
│  │  • Plage : 00:00:00 à 24:00:00                         │    │
│  │                                                          │    │
│  │  ISO : 14.30.00                                         │    │
│  │  EUR : 14.30.00                                         │    │
│  │  USA : 2:30 PM                                          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  TIMESTAMP                                                       │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  • Date + Time + microsecondes (10 octets)             │    │
│  │  • Format : AAAA-MM-JJ-HH.MM.SS.NNNNNN                 │    │
│  │                                                          │    │
│  │  Exemple : 2024-12-15-14.30.00.123456                  │    │
│  │                                                          │    │
│  │  Usage : horodatage précis, audit, logs                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Fonctions utiles :                                              │
│  • CURRENT_DATE     : date système                              │
│  • CURRENT_TIME     : heure système                             │
│  • CURRENT_TIMESTAMP: horodatage système                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Correspondance DB2 - COBOL

| Type DB2 | Type COBOL | Exemple |
|----------|------------|---------|
| SMALLINT | PIC S9(4) COMP | PIC S9(4) COMP |
| INTEGER | PIC S9(9) COMP | PIC S9(9) COMP |
| DECIMAL(7,2) | PIC S9(5)V99 COMP-3 | PIC S9(5)V99 COMP-3 |
| CHAR(10) | PIC X(10) | PIC X(10) |
| VARCHAR(30) | 01 VAR. 49 LEN PIC S9(4) COMP. 49 DATA PIC X(30) | Structure 49 |
| DATE | PIC X(10) | '2024-12-15' |
| TIME | PIC X(8) | '14.30.00' |
| TIMESTAMP | PIC X(26) | '2024-12-15-14.30.00.123456' |

---

## V-2 : Modes d'Accès à DB2 sous z/OS

### Vue d'ensemble

```
┌─────────────────────────────────────────────────────────────────┐
│              MODES D'ACCÈS À DB2 SOUS z/OS                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  MODE ONLINE (Interactif)                               │    │
│  │  ─────────────────────────                              │    │
│  │                                                          │    │
│  │  • Via ISPF → DB2I (DB2 Interactive)                   │    │
│  │  • Panneaux de menus guidés                            │    │
│  │  • SPUFI pour exécution SQL ad-hoc                     │    │
│  │  • Idéal pour développement et tests                   │    │
│  │                                                          │    │
│  │  Accès : ISPF =7 ou commande TSO: DB2I                 │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  MODE BATCH (JCL)                                       │    │
│  │  ────────────────                                       │    │
│  │                                                          │    │
│  │  • Exécution via JCL et IKJEFT01 (TSO batch)           │    │
│  │  • Pour traitements planifiés                          │    │
│  │  • Scripts SQL, utilitaires DB2                        │    │
│  │  • Production et opérations                            │    │
│  │                                                          │    │
│  │  Programme : IKJEFT01 ou IKJEFT1B                      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  MODE PROGRAMME (Embedded SQL)                          │    │
│  │  ───────────────────────────────                        │    │
│  │                                                          │    │
│  │  • SQL intégré dans COBOL, PL/I, C, Assembleur         │    │
│  │  • Précompilation nécessaire                           │    │
│  │  • Applications batch ou CICS                          │    │
│  │                                                          │    │
│  │  EXEC SQL ... END-EXEC                                 │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## V-3 : DB2I - DB2 Interactive

### Menu principal DB2I

```
┌─────────────────────────────────────────────────────────────────┐
│                   DB2I PRIMARY OPTION MENU                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Select one of the following DB2 functions and press ENTER.     │
│                                                                  │
│   1  SPUFI             Execute SQL statements                   │
│   2  DCLGEN            Generate SQL and source language dcls    │
│   3  PROGRAM PREPARATION  Prepare a DB2 application program    │
│   4  PRECOMPILE        Invoke DB2 precompiler                   │
│   5  BIND/REBIND/FREE  BIND, REBIND, or FREE plans or packages │
│   6  RUN               Run an SQL program                       │
│   7  DB2 COMMANDS      Issue DB2 commands                       │
│   8  UTILITIES         Invoke DB2 utilities                     │
│   D  DB2I DEFAULTS     Set global parameters                    │
│   X  EXIT              Exit DB2I                                │
│                                                                  │
│  ===>                                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Description des options

| Option | Nom | Description |
|--------|-----|-------------|
| **1** | SPUFI | Exécuter du SQL depuis un dataset |
| **2** | DCLGEN | Générer déclarations SQL + structures COBOL |
| **3** | PROGRAM PREP | Préparer un programme DB2 complet |
| **4** | PRECOMPILE | Lancer le précompilateur DB2 |
| **5** | BIND | Créer/modifier les plans et packages |
| **6** | RUN | Exécuter un programme SQL |
| **7** | DB2 COMMANDS | Commandes système DB2 |
| **8** | UTILITIES | Utilitaires DBA (LOAD, UNLOAD, REORG...) |

---

## V-4 : SPUFI - SQL Processor Using File Input

### Qu'est-ce que SPUFI ?

**SPUFI** est l'outil interactif de DB2 qui permet d'exécuter des requêtes SQL stockées dans un dataset et d'afficher les résultats.

```
┌─────────────────────────────────────────────────────────────────┐
│                        SPUFI                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  FONCTIONNEMENT :                                                │
│                                                                  │
│  1. Créer un dataset contenant les requêtes SQL                 │
│     ┌─────────────────────────────────────────┐                 │
│     │ USER01.SQL.INPUT                        │                 │
│     │ ────────────────────────────────────────│                 │
│     │ SELECT EMP_NUM, EMP_NOM, SAL            │                 │
│     │ FROM EMPLOYEE                           │                 │
│     │ WHERE DEPT_NUM = 30;                    │                 │
│     └─────────────────────────────────────────┘                 │
│                         │                                        │
│                         ▼                                        │
│  2. SPUFI exécute le SQL sur DB2                                │
│                         │                                        │
│                         ▼                                        │
│  3. Résultats écrits dans un dataset de sortie                  │
│     ┌─────────────────────────────────────────┐                 │
│     │ USER01.SQL.OUTPUT                       │                 │
│     │ ────────────────────────────────────────│                 │
│     │ EMP_NUM  EMP_NOM   SAL                  │                 │
│     │ -------  --------  -------              │                 │
│     │ 7499     PAUL      1600.00              │                 │
│     │ 7521     MARTIN    1250.00              │                 │
│     │ 7654     ALLEN     1250.00              │                 │
│     │                                         │                 │
│     │ SQLCODE = 0                             │                 │
│     │ 3 ROWS SELECTED                         │                 │
│     └─────────────────────────────────────────┘                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Écran SPUFI

```
┌─────────────────────────────────────────────────────────────────┐
│                          SPUFI                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Enter the input data set name:                                  │
│  1  DATA SET NAME ... ===> 'USER01.SQL.INPUT'                   │
│  2  VOLUME SERIAL ... ===>           (Enter if not cataloged)   │
│  3  DATA SET PASSWORD ===>           (Enter if password protected)
│                                                                  │
│  Enter the output data set name:                                 │
│  4  DATA SET NAME ... ===> 'USER01.SQL.OUTPUT'                  │
│                                                                  │
│  Specify processing options:                                     │
│  5  CHANGE DEFAULTS   ===> NO   (Y/N - Display SPUFI defaults)  │
│  6  EDIT INPUT  ..... ===> YES  (Y/N - Enter SQL statements?)   │
│  7  EXECUTE  ........ ===> YES  (Y/N - Execute SQL statements?) │
│  8  AUTOCOMMIT  ..... ===> YES  (Y/N - Commit after execution?) │
│  9  BROWSE OUTPUT ... ===> YES  (Y/N - Browse output data set?) │
│                                                                  │
│  ===>                                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Options SPUFI

| Option | Valeur | Description |
|--------|--------|-------------|
| **EDIT INPUT** | YES | Ouvrir l'éditeur pour saisir le SQL |
| **EXECUTE** | YES | Exécuter les requêtes SQL |
| **AUTOCOMMIT** | YES | COMMIT automatique après exécution |
| **BROWSE OUTPUT** | YES | Afficher les résultats après exécution |

### Format du dataset d'entrée

```
┌─────────────────────────────────────────────────────────────────┐
│              RÈGLES DU DATASET SQL INPUT                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  • LRECL = 80 (longueur d'enregistrement fixe)                  │
│  • Colonnes 1-72 pour le SQL (colonnes 73-80 ignorées)          │
│  • Chaque instruction terminée par un point-virgule (;)         │
│  • Commentaires : -- (deux tirets)                               │
│                                                                  │
│  Exemple de dataset d'entrée :                                   │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │----+----1----+----2----+----3----+----4----+----5----+----6│ │
│  │-- Requete 1 : Liste des employes du dept 30                │ │
│  │SELECT EMP_NUM, EMP_NOM, SAL                                │ │
│  │FROM EMPLOYEE                                               │ │
│  │WHERE DEPT_NUM = 30                                         │ │
│  │ORDER BY SAL DESC;                                          │ │
│  │                                                            │ │
│  │-- Requete 2 : Total des salaires                          │ │
│  │SELECT SUM(SAL) AS TOTAL_SAL                               │ │
│  │FROM EMPLOYEE;                                              │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Analyse des résultats

```
┌─────────────────────────────────────────────────────────────────┐
│              INTERPRÉTATION DES RÉSULTATS SPUFI                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SQLCODE = 0                                                     │
│  ───────────                                                     │
│  Succès, la requête a été exécutée correctement                 │
│                                                                  │
│  SQLCODE > 0 (positif)                                          │
│  ─────────────────────                                          │
│  Warning (avertissement)                                        │
│  • 100 : NOT FOUND (aucune ligne correspondante)               │
│                                                                  │
│  SQLCODE < 0 (négatif)                                          │
│  ─────────────────────                                          │
│  Erreur                                                          │
│  • -204 : Object not found (table n'existe pas)                │
│  • -206 : Column not found (colonne n'existe pas)              │
│  • -530 : Foreign key violation                                 │
│  • -803 : Duplicate key                                         │
│  • -811 : Multiple rows returned (attendu une seule)           │
│                                                                  │
│  Exemple de sortie avec erreur :                                │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │ SELECT * FROM EMPLOYE;                                     │ │
│  │                                                            │ │
│  │ DSNT408I SQLCODE = -204, ERROR: EMPLOYE IS AN UNDEFINED   │ │
│  │          NAME                                              │ │
│  │                                                            │ │
│  │ → La table EMPLOYE n'existe pas (c'est EMPLOYEE)          │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## V-5 : Exécution SQL en Batch

### JCL pour exécution SQL

```jcl
//SQLBATCH JOB (ACCT),'SQL BATCH',CLASS=A,MSGCLASS=X
//*
//* Exécution de requêtes SQL en batch via IKJEFT01
//*
//STEP1   EXEC PGM=IKJEFT01
//STEPLIB  DD  DSN=DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSTSIN  DD  *
  DSN SYSTEM(DB2P)
  RUN PROGRAM(DSNTEP2) PLAN(DSNTEP2) -
      LIB('USER01.RUNLIB.LOAD')
  END
/*
//SYSIN    DD  *
  SELECT EMP_NUM, EMP_NOM, SAL
  FROM EMPLOYEE
  WHERE DEPT_NUM = 30
  ORDER BY SAL DESC;
/*
```

### Explication du JCL

| DD | Description |
|----|-------------|
| **STEPLIB** | Bibliothèque DB2 (SDSNLOAD) |
| **SYSTSPRT** | Sortie des messages TSO |
| **SYSPRINT** | Sortie des résultats SQL |
| **SYSTSIN** | Commandes DSN (connexion DB2) |
| **SYSIN** | Requêtes SQL à exécuter |

### Commande DSN

```
DSN SYSTEM(ssid)        ← Connexion au subsystem DB2
RUN PROGRAM(pgm) PLAN(plan)  ← Exécution d'un programme
END                     ← Fin de session DB2
```

---

## V-6 : DCLGEN - Générateur de Déclarations

### Rôle de DCLGEN

**DCLGEN** génère automatiquement les déclarations SQL (pour le précompilateur) et les structures de données COBOL correspondant à une table DB2.

```
┌─────────────────────────────────────────────────────────────────┐
│                         DCLGEN                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Table DB2                        Fichier généré                │
│  ┌────────────────────┐          ┌──────────────────────────┐   │
│  │ EMPLOYEE           │          │ DCLGEN member : EMPLOYEE │   │
│  ├────────────────────┤   ───►   │                          │   │
│  │ EMP_NUM  INTEGER   │          │ *EXEC SQL DECLARE...     │   │
│  │ EMP_NOM  VARCHAR   │          │ *                        │   │
│  │ SAL      DECIMAL   │          │ 01 DCLEMPLOYEE.          │   │
│  └────────────────────┘          │    10 EMP-NUM PIC S9(9)..│   │
│                                  └──────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple de sortie DCLGEN

```cobol
      ******************************************************************
      * DCLGEN TABLE(EMPLOYEE)                                         *
      *        LIBRARY(USER01.DCLGEN.COBOL(EMPLOYEE))                  *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCLEMPLOYEE)                                  *
      ******************************************************************
           EXEC SQL DECLARE EMPLOYEE TABLE
           ( EMP_NUM                        INTEGER NOT NULL,
             EMP_NOM                        VARCHAR(30) NOT NULL,
             POSTE                          VARCHAR(20),
             DIR                            INTEGER,
             DATE_EMB                       DATE,
             SAL                            DECIMAL(7, 2),
             COMM                           DECIMAL(7, 2),
             DEPT_NUM                       SMALLINT NOT NULL
           ) END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE EMPLOYEE                           *
      ******************************************************************
       01  DCLEMPLOYEE.
           10 EMP-NUM              PIC S9(9) USAGE COMP.
           10 EMP-NOM.
              49 EMP-NOM-LEN       PIC S9(4) USAGE COMP.
              49 EMP-NOM-TEXT      PIC X(30).
           10 POSTE.
              49 POSTE-LEN         PIC S9(4) USAGE COMP.
              49 POSTE-TEXT        PIC X(20).
           10 DIR                  PIC S9(9) USAGE COMP.
           10 DATE-EMB             PIC X(10).
           10 SAL                  PIC S9(5)V9(2) USAGE COMP-3.
           10 COMM                 PIC S9(5)V9(2) USAGE COMP-3.
           10 DEPT-NUM             PIC S9(4) USAGE COMP.

      ******************************************************************
      * INDICATOR VARIABLES                                            *
      ******************************************************************
       01  IEMPLOYEE.
           10 IEMP-NUM             PIC S9(4) USAGE COMP.
           10 IEMP-NOM             PIC S9(4) USAGE COMP.
           10 IPOSTE               PIC S9(4) USAGE COMP.
           10 IDIR                 PIC S9(4) USAGE COMP.
           10 IDATE-EMB            PIC S9(4) USAGE COMP.
           10 ISAL                 PIC S9(4) USAGE COMP.
           10 ICOMM                PIC S9(4) USAGE COMP.
           10 IDEPT-NUM            PIC S9(4) USAGE COMP.
```

### Écran DCLGEN

```
┌─────────────────────────────────────────────────────────────────┐
│                          DCLGEN                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Enter table name:                                               │
│  1  SOURCE TABLE NAME ===> EMPLOYEE                              │
│  2  TABLE OWNER ....... ===> HRSCHEMA                           │
│  3  AT LOCATION ....... ===>                                     │
│                                                                  │
│  Enter output data set name:                                     │
│  4  DATA SET NAME ..... ===> 'USER01.DCLGEN.COBOL(EMPLOYEE)'    │
│                                                                  │
│  Specify options:                                                │
│  5  ACTION ............ ===> ADD    (ADD, REPLACE)              │
│  6  COLUMN LABEL ...... ===> NO     (YES or NO)                 │
│  7  STRUCTURE NAME .... ===> DCLEMPLOYEE                        │
│  8  FIELD NAME PREFIX . ===>                                     │
│  9  DELIMIT DBCS ...... ===> NO                                 │
│  10 COLUMN SUFFIX ..... ===> NO                                 │
│  11 INDICATOR VARS .... ===> YES                                │
│                                                                  │
│  ===>                                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAPITRE V - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  V-1 TYPES DE DONNÉES DB2                                       │
│      • Caractères : CHAR(n), VARCHAR(n), CLOB                   │
│      • Numériques : SMALLINT, INTEGER, DECIMAL(p,s)             │
│      • Date/Temps : DATE, TIME, TIMESTAMP                       │
│      • DECIMAL pour montants financiers (précision exacte)     │
│                                                                  │
│  V-2 MODES D'ACCÈS                                              │
│      • Online : DB2I via ISPF (développement)                   │
│      • Batch : JCL avec IKJEFT01 (production)                   │
│      • Programme : Embedded SQL dans COBOL                       │
│                                                                  │
│  V-3 DB2I                                                        │
│      • Menu interactif pour toutes opérations DB2               │
│      • Options : SPUFI, DCLGEN, BIND, RUN...                    │
│                                                                  │
│  V-4 SPUFI                                                       │
│      • Exécution SQL ad-hoc depuis un dataset                   │
│      • INPUT (SQL) → EXECUTE → OUTPUT (résultats)               │
│      • SQLCODE : 0=OK, >0=warning, <0=erreur                    │
│                                                                  │
│  V-5 BATCH                                                       │
│      • DSN SYSTEM(ssid) pour connexion                          │
│      • Programme DSNTEP2 pour SQL dynamique                     │
│                                                                  │
│  V-6 DCLGEN                                                      │
│      • Génère structures COBOL depuis table DB2                 │
│      • Déclaration SQL + variables hôtes + indicateurs         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Questions de compréhension

1. **Types de données**
   - Quelle est la différence entre CHAR(10) et VARCHAR(10) ?
   - Pourquoi utiliser DECIMAL plutôt que FLOAT pour les montants ?

2. **SPUFI**
   - Quel est le rôle de SPUFI ?
   - Que signifie SQLCODE = -204 ?

3. **DCLGEN**
   - À quoi sert DCLGEN ?
   - Que génère DCLGEN pour un VARCHAR ?

### Exercice pratique

1. Créez un dataset SQL avec les requêtes suivantes :
   - Liste des employés du département 20
   - Salaire moyen par département
   - Employés sans commission

2. Exécutez via SPUFI et analysez les résultats (SQLCODE).

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IV - Modèle relationnel](04-modele-relationnel.md) | [Chapitre VI - SQL DDL](06-sql-ddl.md) |

---
*Formation DB2/SQL - M2i Formation*
