# Rapport de Projet - Mini-Projet DB2/COBOL

**Thème** : Développement d'un mini-projet COBOL-DB2 sous z/OS pour le suivi clientèle dans le secteur financier.

**Candidat** : Josué ROCHA (FORM1112)
**Date** : 16-18 Décembre 2025
**Formation** : POEI Développeur Mainframe COBOL - M2i Formation, Strasbourg

---

## Introduction

Ce projet a été réalisé dans le cadre de la formation POEI Développeur Mainframe COBOL. L'objectif était de mettre en pratique les compétences acquises en SQL/DB2 et en programmation COBOL avec accès aux bases de données.

### Environnement de travail

- **Système** : z/OS sous emulateur Hercules (TK4-)
- **Interface** : ISPF / TSO
- **Base de données** : DB2
- **Outil SQL** : SPUFI
- **Libraries utilisées** :
  - `FORM1112.FINANCE.SOURCE` : Programmes COBOL
  - `FORM1112.FINANCE.SCRIPT` : Requêtes SQL (SPUFI)
  - `FORM1112.FINANCE.RESULT` : Fichiers de sortie SPUFI

### Démarche suivie

1. **Analyse du sujet** : Étude des spécifications et du schéma de données
2. **Création des tables** : Définition des structures avec contraintes PK/FK
3. **Chargement des données** : Insertion de 20 clients et des référentiels
4. **Requêtes SQL** : Vues, index, jointures, agrégations
5. **Programmes COBOL-DB2** : Développement avec Embedded SQL

### Difficultés rencontrées et solutions

| Problème | Solution |
|----------|----------|
| `POSITION` mot réservé COBOL | Renommé la colonne en `POS` |
| Erreur B37 (espace SPUFI) | Augmentation des tracks du fichier output |
| MOVE SPACES vers champ numerique edite | Utilisation de INITIALIZE |
| ABEND 4038 (SYSIN vide) | Validation du numéro compte apres ACCEPT |
| Alias SQL "C" mot réservé | Simplification de la requête sans alias |

### Compétences mises en œuvre

- Conception de base de données relationnelle (PK, FK, contraintes CHECK)
- Requêtes SQL : SELECT, INSERT, UPDATE, CREATE VIEW, CREATE INDEX
- Fonctions SQL : SUM, AVG, COUNT, CASE WHEN, COALESCE
- Jointures : INNER JOIN, sous-requêtes corrélées
- COBOL-DB2 : SELECT INTO, CURSOR, variables host, SQLCODE
- Techniques COBOL : niveau 88, ruptures de controle, ACCEPT

---

## Sommaire

1. [Partie 1 : Création et chargement des données](#partie-1--creation-et-chargement-des-données)
2. [Partie 2 : Exploitation et manipulation SQL](#partie-2--exploitation-et-manipulation-sql)
3. [Partie 3 : Programmation COBOL-DB2](#partie-3--programmation-cobol-db2)

---

# Partie 1 : Création et chargement des données

## Exercice 1 : Création des tables

### Énoncé

Exécuter les instructions CREATE TABLE de chacune des tables. Chaque table doit comporter sa clé primaire, et les clés étrangères doivent etre correctement reliees (FK).

### Mon travail

J'ai commencé par créér les 3 tables référentielles (REGION, NATCOMPT, PROFESSI) car elles n'ont pas de dépendances. Ensuite, j'ai créé la table CLIENT qui possede des clés étrangères vers ces 3 tables.

**Points importants :**
- L'ordre de creation est crucial : les tables référencées doivent exister avant la table qui les reference
- J'ai renommé la colonne `POSITION` en `POS` car POSITION est un mot réservé en COBOL
- La contrainte CHECK sur POS garantit que seules les valeurs 'DB' et 'CR' sont acceptees

### Résolution

```sql
-- Table REGION (référentiel des régions)
CREATE TABLE REGION (
    CODE_REGION CHAR(2) NOT NULL PRIMARY KEY,
    NOM_REGION  VARCHAR(15)
);

-- Table NATCOMPT (référentiel des natures de compte)
CREATE TABLE NATCOMPT (
    CODE_NATCPT CHAR(2) NOT NULL PRIMARY KEY,
    LIB_NATCPT  VARCHAR(30)
);

-- Table PROFESSI (référentiel des professions)
CREATE TABLE PROFESSI (
    CODE_PROF CHAR(2) NOT NULL PRIMARY KEY,
    LIB_PROF  VARCHAR(20)
);

-- Table CLIENT (table principale avec FK)
CREATE TABLE CLIENT (
    NUM_COMPTE   CHAR(3) NOT NULL PRIMARY KEY,
    CODE_REGION  CHAR(2) REFERENCES REGION(CODE_REGION),
    CODE_NATCPT  CHAR(2) REFERENCES NATCOMPT(CODE_NATCPT),
    NOM_CLIENT   VARCHAR(10),
    PREN_CLIENT  VARCHAR(10),
    DATE_NAIS    DATE,
    SEXE         CHAR(1),
    CODE_PROF    CHAR(2) REFERENCES PROFESSI(CODE_PROF),
    SIT_FAM      CHAR(1),
    ADRESSE      VARCHAR(20),
    SOLDE        DECIMAL(10,2),
    POS          CHAR(2),
    CHECK(POS IN ('DB','CR'))
);
```

> **Note** : `POS` remplace `POSITION` (mot réservé COBOL)

### Captures d'écran

![pt1ex01-1](images-pt1/pt1ex01%201.PNG)
![pt1ex01-2](images-pt1/pt1ex01%202.PNG)
![pt1ex01-3](images-pt1/pt1ex01%203.PNG)
![pt1ex01-4](images-pt1/pt1ex01%204.PNG)
![pt1ex01-5](images-pt1/pt1ex01%205.PNG)

---

## Exercice 2 : Alimentation des tables

### Énoncé

Insérer les données de base avec des commandes INSERT INTO. Le nombre de clients doit etre au minimum de 20 avec des répartitions équilibrées.

### Mon travail

J'ai d'abord inséré les données dans les tables référentielles, puis les 20 clients. J'ai veillé a respecter une répartition équilibrée des données :
- 5 clients par region (Paris, Marseille, Lyon, Lille)
- 10 hommes et 10 femmes
- Environ 40% débiteurs (8) et 60% créditeurs (12)
- Toutes les professions et types de comptes representes

J'ai utilisé la fonction DATE() de DB2 pour formater les dates de naissance.

### Résolution

**Tables référentielles :**

```sql
-- REGION (4 régions)
INSERT INTO REGION VALUES ('01', 'PARIS');
INSERT INTO REGION VALUES ('02', 'MARSEILLE');
INSERT INTO REGION VALUES ('03', 'LYON');
INSERT INTO REGION VALUES ('04', 'LILLE');

-- NATCOMPT (5 natures de compte)
INSERT INTO NATCOMPT VALUES ('20', 'COMPTE EPARGNE');
INSERT INTO NATCOMPT VALUES ('25', 'COMPTE CHEQUE');
INSERT INTO NATCOMPT VALUES ('30', 'COMPTE COMMERCIAL');
INSERT INTO NATCOMPT VALUES ('35', 'COMPTE CAMPAGNE AGRICOLE');
INSERT INTO NATCOMPT VALUES ('40', 'COMPTE CDI');

-- PROFESSI (6 professions)
INSERT INTO PROFESSI VALUES ('05', 'MEDECIN');
INSERT INTO PROFESSI VALUES ('10', 'INGENIEUR');
INSERT INTO PROFESSI VALUES ('15', 'COMPTABLE');
INSERT INTO PROFESSI VALUES ('20', 'COMMERCANT');
INSERT INTO PROFESSI VALUES ('25', 'FONCTIONNAIRE');
INSERT INTO PROFESSI VALUES ('30', 'PRIVEE');
```

**Table CLIENT (20 enregistrements) :**

```sql
INSERT INTO CLIENT VALUES ('001','01','20','DURAND','ALAIN',DATE('1980-11-02'),'M','05','C','12 RUE DE PARIS',1500.00,'CR');
INSERT INTO CLIENT VALUES ('002','02','25','MARTIN','JEAN',DATE('1985-05-10'),'M','10','M','5 AV JEAN JAURES',-200.00,'DB');
INSERT INTO CLIENT VALUES ('003','03','30','BERNARD','CLAUDE',DATE('1979-03-21'),'M','15','M','8 RUE DE LYON',800.00,'CR');
-- ... (20 clients au total)
INSERT INTO CLIENT VALUES ('020','04','40','GUYOT','PAULINE',DATE('1990-07-13'),'F','05','C','20 RUE DE LILLE',1100.00,'CR');
```

**Répartition des données :**

| Critère | Répartition |
|---------|-------------|
| Regions | 5 clients par region |
| Sexe | 10 M / 10 F |
| Position | 8 DB / 12 CR |
| Professions | Toutes représentées |

### Captures d'écran

![pt1ex02-1](images-pt1/pt1ex02%201.PNG)
![pt1ex02-2](images-pt1/pt1ex02%202.PNG)
![pt1ex02-3](images-pt1/pt1ex02%203.PNG)
![pt1ex02-4](images-pt1/pt1ex02%204.PNG)
![pt1ex02-5](images-pt1/pt1ex02%205.PNG)
![pt1ex02-6](images-pt1/pt1ex02%206.PNG)
![pt1ex02-7](images-pt1/pt1ex02%207.PNG)

---

## Exercice 3 : Vérification de cohérence

### Énoncé

Effectuer des requêtes pour valider le chargement.

### Mon travail

J'ai exécuté des SELECT simples sur chaque table pour vérifier que toutes les données ont ete correctement insérées. J'ai vérifié :
- 4 régions dans REGION
- 5 natures de compte dans NATCOMPT
- 6 professions dans PROFESSI
- 20 clients dans CLIENT avec les bonnes references (FK valides)

### Résolution

```sql
SELECT * FROM REGION;
SELECT * FROM NATCOMPT;
SELECT * FROM PROFESSI;
SELECT * FROM CLIENT;
```

### Captures d'écran

![pt1ex03-1](images-pt1/pt1ex03%201.PNG)
![pt1ex03-2](images-pt1/pt1ex03%202.PNG)
![pt1ex03-3](images-pt1/pt1ex03%203.PNG)
![pt1ex03-4](images-pt1/pt1ex03%204.PNG)
![pt1ex03-5](images-pt1/pt1ex03%205.PNG)

---

# Partie 2 : Exploitation et manipulation SQL

## Exercice 1 : Extraction des clients par profession

### Énoncé

Extraire la liste des clients exercant les professions COMPTABLE, FONCTIONNAIRE et MEDECIN. Créer des vues permanentes pour chacune de ces catégories.

### Mon travail

J'ai d'abord réalisé des SELECT simples pour vérifier les données, puis j'ai créé 3 vues permanentes. J'ai utilisé des jointures INNER JOIN avec la table PROFESSI pour inclure le libellé de la profession dans chaque vue.

**Choix technique** : Filtrer par LIB_PROF plutot que CODE_PROF rend le code plus lisible et maintenable.

### Résolution

```sql
-- Requêtes de sélection
SELECT * FROM CLIENT WHERE CODE_PROF = '15';  -- COMPTABLE
SELECT * FROM CLIENT WHERE CODE_PROF = '25';  -- FONCTIONNAIRE
SELECT * FROM CLIENT WHERE CODE_PROF = '05';  -- MEDECIN

-- Vues avec jointure (libellé profession)
CREATE VIEW V_CLIENT_COMPTABLE AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE P.LIB_PROF = 'COMPTABLE';

CREATE VIEW V_CLIENT_FONCTION AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE P.LIB_PROF = 'FONCTIONNAIRE';

CREATE VIEW V_CLIENT_MEDECIN AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE P.LIB_PROF = 'MEDECIN';

-- Vérification
SELECT * FROM V_CLIENT_COMPTABLE;
SELECT * FROM V_CLIENT_FONCTION;
SELECT * FROM V_CLIENT_MEDECIN;
```

### Captures d'écran

![pt2ex01-1](images-pt1/pt2ex01%201.PNG)
![pt2ex01-2](images-pt1/pt2ex01%202.PNG)
![pt2ex01-3](images-pt1/pt2ex01%203.PNG)
![pt2ex01-4](images-pt1/pt2ex01%204.PNG)
![pt2ex01-5](images-pt1/pt2ex01%205.PNG)
![pt2ex01-6](images-pt1/pt2ex01%206.PNG)

---

## Exercice 2 : Répartition selon la position du compte (DB/CR)

### Énoncé

Réaliser deux requêtes SQL permettant d'isoler les clients débiteurs (DB) et les clients créditeurs (CR). Matérialiser ces sélections sous forme de vues.

### Mon travail

J'ai créé deux vues distinctes pour séparer les clients selon leur position. Ces vues seront utiles pour les programmes COBOL-DB2 qui traiteront séparément les débiteurs et créditeurs.

**Observation** : Sur les 20 clients, 8 sont débiteurs (solde négatif) et 12 sont créditeurs (solde positif).

### Résolution

```sql
-- Requêtes de sélection
SELECT * FROM CLIENT WHERE POS = 'DB';
SELECT * FROM CLIENT WHERE POS = 'CR';

-- Vues pour reutilisation COBOL-DB2
CREATE VIEW V_CLIENT_DEBITEUR AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       C.SOLDE, C.POS
FROM CLIENT C
WHERE C.POS = 'DB';

CREATE VIEW V_CLIENT_CREDITEUR AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       C.SOLDE, C.POS
FROM CLIENT C
WHERE C.POS = 'CR';

-- Vérification
SELECT * FROM V_CLIENT_DEBITEUR;
SELECT * FROM V_CLIENT_CREDITEUR;
```

### Captures d'écran

![pt2ex02-1](images-pt1/pt2ex02%201.PNG)
![pt2ex02-2](images-pt1/pt2ex02%202.PNG)
![pt2ex02-3](images-pt1/pt2ex02%203.PNG)
![pt2ex02-4](images-pt1/pt2ex02%204.PNG)
![pt2ex02-5](images-pt1/pt2ex02%205.PNG)
![pt2ex02-6](images-pt1/pt2ex02%206.PNG)
![pt2ex02-7](images-pt1/pt2ex02%207.PNG)

---

## Exercice 3 : Répartition des clients par region

### Énoncé

Mettre en place une répartition des clients par region. Chaque region doit faire l'objet d'une extraction distincte.

### Mon travail

J'ai créé 4 vues, une par region (Paris, Marseille, Lyon, Lille). Chaque vue inclut une jointure avec REGION pour affichér le nom de la region.

**Vérification** : Chaque region contient exactement 5 clients comme prévu dans la répartition initiale.

### Résolution

```sql
-- Requêtes par region
SELECT * FROM CLIENT WHERE CODE_REGION = '01';  -- PARIS
SELECT * FROM CLIENT WHERE CODE_REGION = '02';  -- MARSEILLE
SELECT * FROM CLIENT WHERE CODE_REGION = '03';  -- LYON
SELECT * FROM CLIENT WHERE CODE_REGION = '04';  -- LILLE

-- Vues avec jointure (nom region)
CREATE VIEW V_CLIENT_PARIS AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       R.CODE_REGION, R.NOM_REGION, C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
WHERE R.NOM_REGION = 'PARIS';

CREATE VIEW V_CLIENT_MARSEILLE AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       R.CODE_REGION, R.NOM_REGION, C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
WHERE R.NOM_REGION = 'MARSEILLE';

CREATE VIEW V_CLIENT_LYON AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       R.CODE_REGION, R.NOM_REGION, C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
WHERE R.NOM_REGION = 'LYON';

CREATE VIEW V_CLIENT_LILLE AS
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       R.CODE_REGION, R.NOM_REGION, C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
WHERE R.NOM_REGION = 'LILLE';

-- Vérification
SELECT * FROM V_CLIENT_MARSEILLE;
```

### Captures d'écran

![pt2ex03-1](images-pt1/pt2ex03%201.PNG)
![pt2ex03-2](images-pt1/pt2ex03%202.PNG)
![pt2ex03-3](images-pt1/pt2ex03%203.PNG)
![pt2ex03-4](images-pt1/pt2ex03%204.PNG)
![pt2ex03-5](images-pt1/pt2ex03%205.PNG)
![pt2ex03-6](images-pt1/pt2ex03%206.PNG)
![pt2ex03-7](images-pt1/pt2ex03%207.PNG)
![pt2ex03-8](images-pt1/pt2ex03%208.PNG)
![pt2ex03-9](images-pt1/pt2ex03%209.PNG)
![pt2ex03-10](images-pt1/pt2ex03%2010.PNG)

---

## Exercice 4 : Index secondaire sur la region

### Énoncé
Créer un index secondaire sur la colonne CODE_REGION pour optimiser les recherches par region.

### Mon travail

J'ai créé un index sur CODE_REGION pour accélérer les requêtes de recherche par region. Cet index est particulierement utile car plusieurs de nos vues et requêtes filtrent par region.

**Impact** : Les SELECT avec WHERE CODE_REGION = ... bénéficient desormais d'un accès indexé au lieu d'un scan complet de la table.

### Résolution

```sql
CREATE INDEX IDX_CLIENT_REGION ON CLIENT(CODE_REGION);

-- Vérification
SELECT * FROM CLIENT WHERE CODE_REGION = '01';
```

### Captures d'écran

![pt2ex04-1](images-pt1/pt2ex04%201.PNG)
![pt2ex04-2](images-pt1/pt2ex04%202.PNG)

---

## Exercice 5 : Index secondaire sur la profession

### Énoncé
Créer un index secondaire sur la colonne CODE_PROF afin de faciliter les traitements regroupés par profession.

### Mon travail

De même que pour la region, j'ai créé un index sur CODE_PROF. Les vues V_CLIENT_COMPTABLE, V_CLIENT_FONCTION et V_CLIENT_MEDECIN bénéficieront de cet index.

**Remarque** : Les deux index (region et profession) peuvent etre utilisés simultanément par l'optimiseur DB2 pour les requêtes combinant ces deux critères.

### Résolution

```sql
CREATE INDEX IDX_CLIENT_PROF ON CLIENT(CODE_PROF);

-- Vérification
SELECT * FROM CLIENT WHERE CODE_PROF = '15';
```

### Captures d'écran

![pt2ex05-1](images-pt1/pt2ex05%201.PNG)
![pt2ex05-2](images-pt1/pt2ex05%202.PNG)

---

## Exercice 6 : Édition des clients triés par region et profession

### Énoncé
Concevoir une requête SQL permettant d'affichér les clients dans l'ordre suivant :
1. Par region (CODE_REGION)
2. Puis par profession (CODE_PROF)
3. Puis par numéro de compte (NUM_COMPTE)

### Mon travail

J'ai écrit une requête avec double jointure (REGION et PROFESSI) pour affichér les libellés plutot que les codes. Le tri multi-niveaux ORDER BY permet de regrouper visuellement les données.

**Utilité** : Cette requête servira de base pour le programme COBOL-DB2 avec ruptures (P3-Ex05).

### Résolution

```sql
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       R.CODE_REGION, R.NOM_REGION,
       P.CODE_PROF, P.LIB_PROF,
       C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN REGION R ON C.CODE_REGION = R.CODE_REGION
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
ORDER BY C.CODE_REGION, C.CODE_PROF, C.NUM_COMPTE;
```

### Captures d'écran

![pt2ex06-1](images-pt1/pt2ex06%201.PNG)
![pt2ex06-2](images-pt1/pt2ex06%202.PNG)
![pt2ex06-3](images-pt1/pt2ex06%203.PNG)

---

## Exercice 7 : Fusion de deux populations de clients

### Énoncé
Réaliser une requête SQL permettant de fusionner les listes de clients COMPTABLES et FONCTIONNAIRES dans un même résultat.

### Mon travail

J'ai utilisé l'opérateur UNION pour fusionner deux SELECT independants. Chaque SELECT filtre sur une profession differente et inclut la jointure avec PROFESSI pour le libellé.

**Note technique** : UNION élimine automatiquement les doublons (si un client était dans les deux catégories). Pour conserver les doublons, on utilisérait UNION ALL.

### Résolution

```sql
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF,
       C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE P.LIB_PROF = 'COMPTABLE'
UNION
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF,
       C.SOLDE, C.POS
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE P.LIB_PROF = 'FONCTIONNAIRE';
```

### Captures d'écran

![pt2ex07-1](images-pt1/pt2ex07%201.PNG)
![pt2ex07-2](images-pt1/pt2ex07%202.PNG)
![pt2ex07-3](images-pt1/pt2ex07%203.PNG)

---

## Exercice 8 : Vue CLIENT réduit

### Énoncé
Créer une vue simplifiée de la table CLIENT contenant uniquement : numéro de compte, code region, nature de compte, nom/prenom, activité professionnelle, situation familiale, solde, position.

### Mon travail

J'ai créé une vue qui exclut les colonnes DATE_NAIS, SEXE et ADRESSE. Cette vue allège les requêtes qui n'ont pas besoin de ces informations personnelles.

**Avantage** : La vue peut aussi servir a limiter l'accès aux données sensibles (date de naissance, adresse) pour certains utilisateurs.

### Résolution

```sql
CREATE VIEW V_CLIENT_REDUIT AS
SELECT NUM_COMPTE,
       CODE_REGION,
       CODE_NATCPT,
       NOM_CLIENT,
       PREN_CLIENT,
       CODE_PROF,
       SIT_FAM,
       SOLDE,
       POS
FROM CLIENT;

-- Vérification
SELECT * FROM V_CLIENT_REDUIT;
```

### Captures d'écran

![pt2ex08-1](images-pt1/pt2ex08%201.PNG)
![pt2ex08-2](images-pt1/pt2ex08%202.PNG)
![pt2ex08-3](images-pt1/pt2ex08%203.PNG)
![pt2ex08-4](images-pt1/pt2ex08%204.PNG)

---

## Exercice 9 : Analyse multi-critères avec agrégations conditionnelles

### Énoncé
Afficher, pour chaque region :
- Nombre total de clients
- Nombre de clients débiteurs
- Nombre de clients créditeurs
- Solde total débiteur
- Solde total créditeur

### Mon travail

J'ai utilisé la technique CASE WHEN dans les fonctions d'agrégation pour calculer des sous-totaux conditionnels en une seule requête. Cela évite de faire plusieurs requêtes séparées.

**Version simplifiée** : Sans jointure, directement sur CLIENT avec GROUP BY CODE_REGION. Les alias entre guillemets permettent des noms descriptifs avec espaces.

### Résolution

```sql
SELECT
  CODE_REGION,
  COUNT(*)
     AS "NB CLIENTS",
  SUM(CASE WHEN POS = 'DB' THEN 1 ELSE 0 END)
     AS "NB DEBITEURS",
  SUM(CASE WHEN POS = 'CR' THEN 1 ELSE 0 END)
     AS "NB CREDITEURS",
  SUM(CASE WHEN POS = 'DB' THEN SOLDE ELSE 0 END)
     AS "SOLDE TOTAL DEBITEURS",
  SUM(CASE WHEN POS = 'CR' THEN SOLDE ELSE 0 END)
     AS "SOLDE TOTAL CREDITEURS"
FROM CLIENT
GROUP BY CODE_REGION;
```

### Captures d'écran

![pt2ex09-1](images-pt1/pt2ex09%201.PNG)
![pt2ex09-2](images-pt1/pt2ex09%202.PNG)
![pt2ex09-3](images-pt1/pt2ex09%203.PNG)

---

## Exercice 10 : Clients anormalement débiteurs par profession

### Énoncé
Lister les clients débiteurs dont le solde est supérieur a la moyenne des soldes débiteurs de leur profession.

### Mon travail

Cet exercice m'a demandé une sous-requête corrélée. La sous-requête calcule la moyenne des soldes débiteurs pour chaque profession, et la requête principale compare chaque client a cette moyenne.

**Subtilité** : Les soldes débiteurs étant négatifs, un client "plus débiteur" a un solde inférieur (ex: -450 < -200). D'ou l'utilisation de `<` au lieu de `>`.

### Résolution

```sql
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF,
       C.SOLDE
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE C.POS = 'DB'
  AND C.SOLDE < (SELECT AVG(C2.SOLDE)
                 FROM CLIENT C2
                 WHERE C2.POS = 'DB'
                   AND C2.CODE_PROF = C.CODE_PROF)
ORDER BY C.CODE_PROF, C.SOLDE;
```

> **Note** : La comparaison utilisé `<` car les soldes débiteurs sont négatifs. Un solde de -450 est "plus débiteur" qu'un solde de -200.

### Captures d'écran

![pt2ex10-1](images-pt1/pt2ex10%201.PNG)
![pt2ex10-2](images-pt1/pt2ex10%202.PNG)

---

# Partie 3 : Programmation COBOL-DB2

## Exercice 1 : Afficher la region Marseille

### Énoncé
Écrire un programme COBOL-DB2 permettant d'affichér la region Marseille (02).

### Mon travail

Premier programme COBOL-DB2 : j'ai utilisé SELECT INTO pour lire une seule ligne. La variable SQLCODE me permet de vérifier si la requête a réussi (0) ou échoué.

**Utilisation de DCLGEN** : Au lieu de déclarer manuellement les variables host, j'utilisé `EXEC SQL INCLUDE REGION END-EXEC` pour inclure les variables générées par l'utilitaire DCLGEN de DB2.

**Avantages DCLGEN** :
- Variables host générées automatiquement depuis la structure de la table
- Correspondance exacte avec les types de colonnes DB2
- Maintenabilite : si la table change, on régénère le DCLGEN

### Résolution

**Programme : AFFREG.cbl**

```cobol
       WORKING-STORAGE SECTION.
      * SQLCA pour gestion erreurs DB2
           EXEC SQL INCLUDE SQLCA END-EXEC.
      * DCLGEN pour la table REGION
           EXEC SQL INCLUDE REGION END-EXEC.

       PROCEDURE DIVISION.
       1000-SELECT-REGION.
           EXEC SQL
               SELECT CODE_REGION, NOM_REGION
               INTO :CODE-REGION, :NOM-REGION
               FROM REGION
               WHERE CODE_REGION = '02'
           END-EXEC

           IF SQLCODE = 0
               DISPLAY 'CODE   : ' CODE-REGION
               DISPLAY 'NOM    : ' NOM-REGION
           ELSE
               DISPLAY 'ERREUR SQL - SQLCODE : ' SQLCODE
           END-IF.
```

**Techniques utilisées** :
- SELECT INTO (lecture d'une seule ligne)
- DCLGEN (variables host générées par DB2)

### Captures d'écran

![P3 Ex01 - 1](images-pt3-1/pt3ex01%201.PNG)
![P3 Ex01 - 2](images-pt3-1/pt3ex01%202.PNG)
![P3 Ex01 - 3](images-pt3-1/pt3ex01%203.PNG)
![P3 Ex01 - 4](images-pt3-1/pt3ex01%204.PNG)

---

## Exercice 2 : Insérer un nouveau client

### Énoncé
Écrire un programme COBOL-DB2 permettant d'insérér un nouveau client dans la table CLIENT.

### Mon travail

J'ai utilisé INSERT INTO avec des variables host. Les données du client sont lues depuis SYSIN via des ACCEPT (une ligne par champ). J'ai ajouté la gestion transactionnelle avec COMMIT en cas de succès et ROLLBACK en cas d'erreur.

**JCL requis** : Les données du client sont passées via SYSIN (12 lignes) :
```jcl
//SYSIN DD *
021
01
25
DUPONT
MARC
1995-06-15
M
10
C
25 RUE NEUVE
1800.00
CR
/*
```

### Résolution

**Programme : INSCLI.cbl**

```cobol
       1000-LIRE-DONNEES.
      * Lecture des données depuis SYSIN (JCL In-Stream)
           ACCEPT WS-NUM-COMPTE
           ACCEPT WS-CODE-REGION
           ACCEPT WS-CODE-NATCPT
           ACCEPT WS-NOM-CLIENT
           ACCEPT WS-PREN-CLIENT
           ACCEPT WS-DATE-NAIS
           ACCEPT WS-SEXE
           ACCEPT WS-CODE-PROF
           ACCEPT WS-SIT-FAM
           ACCEPT WS-ADRESSE
           ACCEPT WS-SOLDE-IN
           ACCEPT WS-POS

      * Conversion du solde (texte -> numerique)
           COMPUTE WS-SOLDE = FUNCTION NUMVAL(WS-SOLDE-IN).

       2000-INSERT-CLIENT.
           EXEC SQL
               INSERT INTO CLIENT (...)
               VALUES (:WS-NUM-COMPTE, :WS-CODE-REGION, ...)
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
           ELSE
               EXEC SQL ROLLBACK END-EXEC
           END-IF.
```

**Techniques utilisées** :
- ACCEPT pour lire les données depuis SYSIN
- FUNCTION NUMVAL pour convertir le solde texte en numerique
- COMMIT/ROLLBACK pour la gestion transactionnelle

### Captures d'écran

![P3 Ex02 - 1](images-pt3-1/pt3ex02%201.PNG)
![P3 Ex02 - 2](images-pt3-1/pt3ex02%202.PNG)
![P3 Ex02 - 3](images-pt3-1/pt3ex02%203.PNG)
![P3 Ex02 - 4](images-pt3-1/pt3ex02%204.PNG)
![P3 Ex02 - 5](images-pt3-1/pt3ex02%205.PNG)
![P3 Ex02 - 6](images-pt3-1/pt3ex02%206.PNG)
![P3 Ex02 - 7](images-pt3-1/pt3ex02%207.PNG)
![P3 Ex02 - 8](images-pt3-1/pt3ex02%208.PNG)
![P3 Ex02 - 9](images-pt3-1/pt3ex02%209.PNG)

---

## Exercice 3 : Afficher tous les clients de Marseille

### Énoncé
Écrire un programme COBOL-DB2 permettant d'affichér tous les clients de la region Marseille (02).

### Mon travail

Pour lire plusieurs lignes, j'ai utilisé un CURSOR. Le cycle complet est : DECLARE (définition), OPEN (ouverture), FETCH en boucle (lecture), CLOSE (fermeture).

**Gestion fin de fichier** : SQLCODE = 100 indique qu'il n'y a plus de lignes a lire.

### Résolution

**Programme : AFFCLI.cbl**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFFCLI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM-COMPTE     PIC X(03).
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-FIN-CURSOR     PIC 9(01) VALUE 0.
           EXEC SQL INCLUDE SQLCA END-EXEC.

           EXEC SQL
               DECLARE C-CLIENTS CURSOR FOR
               SELECT NUM_COMPTE, NOM_CLIENT, ...
               FROM CLIENT
               WHERE CODE_REGION = '02'
               ORDER BY NUM_COMPTE
           END-EXEC.

       PROCEDURE DIVISION.
           EXEC SQL OPEN C-CLIENTS END-EXEC
           PERFORM UNTIL WS-FIN-CURSOR = 1
               EXEC SQL
                   FETCH C-CLIENTS INTO :WS-NUM-COMPTE, ...
               END-EXEC
               IF SQLCODE = 0
                   DISPLAY WS-NUM-COMPTE ' ' WS-NOM-CLIENT
               ELSE
                   MOVE 1 TO WS-FIN-CURSOR
               END-IF
           END-PERFORM
           EXEC SQL CLOSE C-CLIENTS END-EXEC
           STOP RUN.
```

**Technique utilisée** : CURSOR (DECLARE, OPEN, FETCH, CLOSE) pour lecture multiple

### Captures d'écran

![P3 Ex03 - 1](images-pt3-1/pt3ex03%201.PNG)
![P3 Ex03 - 2](images-pt3-1/pt3ex03%202.PNG)
![P3 Ex03 - 3](images-pt3-1/pt3ex03%203.PNG)
![P3 Ex03 - 4](images-pt3-1/pt3ex03%204.PNG)
![P3 Ex03 - 5](images-pt3-1/pt3ex03%205.PNG)
![P3 Ex03 - 6](images-pt3-1/pt3ex03%206.PNG)
![P3 Ex03 - 7](images-pt3-1/pt3ex03%207.PNG)

---

## Exercice 4 : Mise à jour d'un client

### Énoncé
Écrire un programme COBOL-DB2 qui permet de mettre à jour l'adresse, le solde et la position d'un client existant.

### Mon travail

J'ai utilisé UPDATE avec une clause WHERE pour cibler un client spécifique (005). Comme pour l'INSERT, j'applique COMMIT apres verification du SQLCODE.

**Test** : J'ai vérifié avec un SELECT avant et apres la mise à jour pour confirmer les changements.

### Résolution

**Programme : MAJCLI.cbl**

```cobol
       WORKING-STORAGE SECTION.
      * Variables host pour DB2
       01 WS-NUM-COMPTE     PIC X(03).
       01 WS-ADRESSE        PIC X(20).
       01 WS-SOLDE          PIC S9(8)V99 COMP-3.
       01 WS-POS            PIC X(02).
      * Variable pour saisie du solde
       01 WS-SOLDE-IN       PIC X(10).

       PROCEDURE DIVISION.
       1000-LIRE-DONNEES.
      * Lecture des données depuis SYSIN (JCL In-Stream)
           ACCEPT WS-NUM-COMPTE
           ACCEPT WS-ADRESSE
           ACCEPT WS-SOLDE-IN
           ACCEPT WS-POS
      * Conversion du solde (texte -> numerique)
           COMPUTE WS-SOLDE = FUNCTION NUMVAL(WS-SOLDE-IN)

       2000-UPDATE-CLIENT.
           EXEC SQL
               UPDATE CLIENT
               SET ADRESSE = :WS-ADRESSE,
                   SOLDE = :WS-SOLDE,
                   POS = :WS-POS
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC

           IF SQLCODE = 0
               DISPLAY 'CLIENT MIS A JOUR'
               EXEC SQL COMMIT END-EXEC
           END-IF.
```

**JCL d'execution avec SYSIN** :
```jcl
//SYSIN    DD *
005
20 AVENUE FOCH
2800.00
CR
/*
```

**Technique utilisée** : UPDATE avec clause WHERE + COMMIT + ACCEPT pour données dynamiques

### Captures d'écran

![P3 Ex04 - 1](images-pt3-1/pt3ex04%201.PNG)
![P3 Ex04 - 2](images-pt3-1/pt3ex04%202.PNG)
![P3 Ex04 - 3](images-pt3-1/pt3ex04%203.PNG)
![P3 Ex04 - 4](images-pt3-1/pt3ex04%204.PNG)
![P3 Ex04 - 5](images-pt3-1/pt3ex04%205.PNG)
![P3 Ex04 - 6](images-pt3-1/pt3ex04%206.PNG)
![P3 Ex04 - 7](images-pt3-1/pt3ex04%207.PNG)
![P3 Ex04 - 8](images-pt3-1/pt3ex04%208.PNG)

---

## Exercice 5 : Liste avec ruptures

### Énoncé
Écrire un programme COBOL-DB2 qui affiché la liste des clients triés par region puis par profession. Pour chaque changement, affichér un titre de rupture.

### Mon travail

J'ai implémenté les ruptures de controle en conservant les valeurs précédentes de CODE_REGION et CODE_PROF. A chaque FETCH, je compare les nouvelles valeurs avec les précédentes pour détecter les changements.

**Logique** : Si la region change, j'affiché le titre region ET je reinitialise la rupture profession (car on change de groupe).

### Résolution

**Programme : LSTRUPT.cbl**

```cobol
       WORKING-STORAGE SECTION.
       01 WS-PREC-REGION    PIC X(02) VALUE SPACES.
       01 WS-PREC-PROF      PIC X(02) VALUE SPACES.

       PROCEDURE DIVISION.
       2100-VERIFIER-RUPTURES.
           IF WS-CODE-REGION NOT = WS-PREC-REGION
               DISPLAY '=== REGION : ' WS-NOM-REGION ' ==='
               MOVE WS-CODE-REGION TO WS-PREC-REGION
               MOVE SPACES TO WS-PREC-PROF
           END-IF

           IF WS-CODE-PROF NOT = WS-PREC-PROF
               DISPLAY '---- PROFESSION : ' WS-LIB-PROF ' ----'
               MOVE WS-CODE-PROF TO WS-PREC-PROF
           END-IF.
```

**Technique utilisée** : Variables de rupture pour détecter les changements de groupe

### Captures d'écran

![P3 Ex05 - 1](images-pt3-2/pt3ex05%201.PNG)
![P3 Ex05 - 2](images-pt3-2/pt3ex05%202.PNG)
![P3 Ex05 - 3](images-pt3-2/pt3ex05%203.PNG)
![P3 Ex05 - 4](images-pt3-2/pt3ex05%204.PNG)
![P3 Ex05 - 5](images-pt3-2/pt3ex05%205.PNG)
![P3 Ex05 - 6](images-pt3-2/pt3ex05%206.PNG)
![P3 Ex05 - 7](images-pt3-2/pt3ex05%207.PNG)
![P3 Ex05 - 8](images-pt3-2/pt3ex05%208.PNG)
![P3 Ex05 - 9](images-pt3-2/pt3ex05%209.PNG)
![P3 Ex05 - 10](images-pt3-2/pt3ex05%2010.PNG)

---

## Exercice 6 : Statistiques DB/CR

### Énoncé
Écrire un programme COBOL-DB2 permettant de calculer le montant général et la moyenne des comptes débiteurs et créditeurs.

### Mon travail

J'ai fait deux SELECT INTO avec les fonctions SUM, AVG et COUNT : un pour les débiteurs (POS = 'DB') et un pour les créditeurs (POS = 'CR').

**Affichage** : Les variables d'édition (PIC ZZZ,ZZ9.99) permettent de formater les montants avec separateurs et décimales.

### Résolution

**Programme : STATCLI.cbl**

```cobol
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT SUM(SOLDE), AVG(SOLDE), COUNT(*)
               INTO :WS-TOTAL-DB, :WS-MOYENNE-DB, :WS-COUNT-DB
               FROM CLIENT WHERE POS = 'DB'
           END-EXEC

           EXEC SQL
               SELECT SUM(SOLDE), AVG(SOLDE), COUNT(*)
               INTO :WS-TOTAL-CR, :WS-MOYENNE-CR, :WS-COUNT-CR
               FROM CLIENT WHERE POS = 'CR'
           END-EXEC

           DISPLAY '=== STATISTIQUES DES CLIENTS ==='
           DISPLAY '--- DEBITEURS ---'
           DISPLAY 'TOTAL   : ' WS-TOTAL-DB
           DISPLAY 'MOYENNE : ' WS-MOYENNE-DB
           DISPLAY '--- CREDITEURS ---'
           DISPLAY 'TOTAL   : ' WS-TOTAL-CR
           DISPLAY 'MOYENNE : ' WS-MOYENNE-CR.
```

**Technique utilisée** : Fonctions d'agrégation SQL (SUM, AVG, COUNT)

### Captures d'écran

![P3 Ex06 - 1](images-pt3-2/pt3ex06%201.PNG)
![P3 Ex06 - 2](images-pt3-2/pt3ex06%202.PNG)
![P3 Ex06 - 3](images-pt3-2/pt3ex06%203.PNG)
![P3 Ex06 - 4](images-pt3-2/pt3ex06%204.PNG)
![P3 Ex06 - 5](images-pt3-2/pt3ex06%205.PNG)
![P3 Ex06 - 6](images-pt3-2/pt3ex06%206.PNG)

---

## Exercice 7 : Totaux par region avec niveau 88

### Énoncé
Écrire un programme COBOL-DB2 qui calcule, pour chaque region, la valeur totale des comptes débiteurs et créditeurs. Utiliser une variable conditionnelle (niveau 88).

### Mon travail

J'ai déclaré des niveaux 88 pour représenter les 4 régions (REGION-PARIS, REGION-MARSEILLE, etc.). Une boucle PERFORM VARYING parcourt les régions et SET permet d'activer chaque condition.

**Avantage du niveau 88** : Le code est plus lisible avec IF REGION-PARIS qu'avec IF WS-CODE-REGION = '01'.

### Résolution

**Programme : TOTREG.cbl**

```cobol
       WORKING-STORAGE SECTION.
       01 WS-CODE-REGION    PIC X(02).
           88 REGION-PARIS      VALUE '01'.
           88 REGION-MARSEILLE  VALUE '02'.
           88 REGION-LYON       VALUE '03'.
           88 REGION-LILLE      VALUE '04'.

       PROCEDURE DIVISION.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 4
               EVALUATE WS-IDX
                   WHEN 1 SET REGION-PARIS TO TRUE
                   WHEN 2 SET REGION-MARSEILLE TO TRUE
                   WHEN 3 SET REGION-LYON TO TRUE
                   WHEN 4 SET REGION-LILLE TO TRUE
               END-EVALUATE
               PERFORM 2000-CALCULER-TOTAUX
           END-PERFORM.
```

**Technique utilisée** : Niveau 88 pour représenter les codes régions + boucle PERFORM VARYING

### Captures d'écran

![P3 Ex07 - 1](images-pt3-2/pt3ex07%201.PNG)
![P3 Ex07 - 2](images-pt3-2/pt3ex07%202.PNG)
![P3 Ex07 - 3](images-pt3-2/pt3ex07%203.PNG)
![P3 Ex07 - 4](images-pt3-2/pt3ex07%204.PNG)
![P3 Ex07 - 5](images-pt3-2/pt3ex07%205.PNG)
![P3 Ex07 - 6](images-pt3-2/pt3ex07%206.PNG)
![P3 Ex07 - 7](images-pt3-2/pt3ex07%207.PNG)

---

## Exercice 8 : Création table MOUVEMENT

### Énoncé
Créer une table DB2 permettant de gérer les mouvements des clients avec : numéro de compte, libellé, montant, sens (DB/CR), nature (CHQ/VER/VIR), date.

### Mon travail

J'ai créé la table MOUVEMENT avec une clé étrangère vers CLIENT(NUM_COMPTE). Deux contraintes CHECK valident les valeurs de SENS (DB/CR) et NATURE (CHQ/VER/VIR).

**Données de test** : J'ai inséré plusieurs mouvements pour différents clients afin de tester les programmes suivants (relevé, total, etc.).

### Résolution

```sql
CREATE TABLE MOUVEMENT (
    NUM_COMPTE   CHAR(3) REFERENCES CLIENT(NUM_COMPTE),
    LIB_MOUV     VARCHAR(15),
    MONTANT_MVT  DECIMAL(8,2),
    SENS         CHAR(2),
    NATURE       CHAR(3),
    DATE_MVT     DATE,
    CHECK(SENS IN ('DB','CR')),
    CHECK(NATURE IN ('CHQ','VER','VIR'))
);

-- Données de test
INSERT INTO MOUVEMENT VALUES ('001','VIREMENT SAL',1500.00,'CR','VIR',DATE('2024-01-15'));
INSERT INTO MOUVEMENT VALUES ('001','CHEQUE 001',200.00,'DB','CHQ',DATE('2024-01-20'));
-- ... autres mouvements de test ...
```

### Captures d'écran

![P3 Ex08 - 1](images-pt3-1/pt3ex08%201.PNG)
![P3 Ex08 - 2](images-pt3-1/pt3ex08%202.PNG)
![P3 Ex08 - 3](images-pt3-1/pt3ex08%203.PNG)
![P3 Ex08 - 4](images-pt3-1/pt3ex08%204.PNG)
![P3 Ex08 - 5](images-pt3-1/pt3ex08%205.PNG)
![P3 Ex08 - 6](images-pt3-1/pt3ex08%206.PNG)
![P3 Ex08 - 7](images-pt3-1/pt3ex08%207.PNG)
![P3 Ex08 - 8](images-pt3-1/pt3ex08%208.PNG)

---

## Exercice 9 : Total mouvements d'un client

### Énoncé
Écrire un programme COBOL-DB2 permettant de calculer le montant total des mouvements d'un client et le nombre total de mouvements. Recevoir le numéro via ACCEPT.

### Mon travail

J'ai utilisé ACCEPT pour lire le numéro de compte depuis SYSIN. La requête SELECT utilisé SUM et COUNT avec COALESCE pour éviter les valeurs NULL si le client n'a pas de mouvements.

**Paramétrage JCL** : Le numéro de compte est passé via donnee In-Stream (//SYSIN DD *).

### Résolution

**Programme : TOTMVT.cbl**

```cobol
       1000-LIRE-NUM-COMPTE.
           ACCEPT WS-NUM-COMPTE
           DISPLAY 'NUMERO COMPTE SAISI : [' WS-NUM-COMPTE ']'

           IF WS-NUM-COMPTE = SPACES
               DISPLAY 'ERREUR : NUMERO COMPTE VIDE'
               DISPLAY 'VERIFIER SYSIN DANS LE JCL'
               STOP RUN
           END-IF.

       3000-CALCULER-TOTAUX.
           EXEC SQL
               SELECT COALESCE(SUM(MONTANT_MVT), 0), COUNT(*)
               INTO :WS-TOTAL-MVT, :WS-NB-MVT
               FROM MOUVEMENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
           END-EXEC.
```

**Technique utilisée** : ACCEPT pour saisie + SUM/COUNT + COALESCE pour gérer les NULL

**Validation importante** : Le programme vérifié que le numéro de compte n'est pas vide avant d'exécutér les requêtes SQL. Cela évite l'erreur ABEND 4038 si le SYSIN est mal configure.

### Captures d'écran

![P3 Ex09 - 1](images-pt3-1/pt3ex09%201.PNG)
![P3 Ex09 - 2](images-pt3-1/pt3ex09%202.PNG)
![P3 Ex09 - 3](images-pt3-1/pt3ex09%203.PNG)
![P3 Ex09 - 4](images-pt3-1/pt3ex09%204.PNG)
![P3 Ex09 - 5](images-pt3-1/pt3ex09%205.PNG)
![P3 Ex09 - 6](images-pt3-1/pt3ex09%206.PNG)
![P3 Ex09 - 7](images-pt3-1/pt3ex09%207.PNG)
![P3 Ex09 - 8](images-pt3-1/pt3ex09%208.PNG)
![P3 Ex09 - 9](images-pt3-1/pt3ex09%209.PNG)

---

## Exercice 10 : Relevé de compte

### Énoncé
Écrire un programme COBOL-DB2 permettant d'éditer un relevé de compte des mouvements d'un client avec colonnes Credit/Debit séparées.

### Mon travail

J'ai créé un relevé bancaire avec en-tête client et liste des mouvements. Chaque mouvement s'affiché soit dans la colonne Credit (si SENS = 'CR') soit dans la colonne Debit.

**Presentation** : L'en-tête affiché le nom du client et son numéro de compte, suivi des colonnes Date/Libelle/Credit/Debit.

**JCL requis** : Le numéro de compte est lu via ACCEPT, donc le JCL doit inclure :
```jcl
//SYSIN DD *
001
/*
```

### Résolution

**Programme : RELEVE.cbl**

```cobol
       5100-AFFICHER-LIGNE.
           INITIALIZE WS-CREDIT-ED
           INITIALIZE WS-DEBIT-ED

           IF WS-SENS = 'CR'
               MOVE WS-MONTANT-MVT TO WS-CREDIT-ED
           ELSE
               MOVE WS-MONTANT-MVT TO WS-DEBIT-ED
           END-IF

           DISPLAY WS-DATE-MVT '  '
                   WS-LIB-MOUV ' '
                   WS-CREDIT-ED ' '
                   WS-DEBIT-ED.
```

> **Note** : Utiliser `INITIALIZE` au lieu de `MOVE SPACES` pour les champs numeriques edites (PIC ZZZ,ZZ9.99). MOVE SPACES cause l'erreur IGYPA3005-S.

**Sortie attendue :**
```
================================================
Nom Client : DURAND     Numero de compte : 001
================================================
Date operation  Libelle         Credit    Debit
================================================
2024-01-15      VIREMENT SAL    1,500.00
2024-01-20      CHEQUE 001                200.00
================================================
```

### Captures d'écran

![P3 Ex10 - 1](images-pt3-1/pt3ex10%201.PNG)
![P3 Ex10 - 2](images-pt3-1/pt3ex10%202.PNG)
![P3 Ex10 - 3](images-pt3-1/pt3ex10%203.PNG)
![P3 Ex10 - 4](images-pt3-1/pt3ex10%204.PNG)
![P3 Ex10 - 5](images-pt3-1/pt3ex10%205.PNG)
![P3 Ex10 - 6](images-pt3-1/pt3ex10%206.PNG)
![P3 Ex10 - 7](images-pt3-1/pt3ex10%207.PNG)
![P3 Ex10 - 8](images-pt3-1/pt3ex10%208.PNG)
![P3 Ex10 - 9](images-pt3-1/pt3ex10%209.PNG)
![P3 Ex10 - 10](images-pt3-1/pt3ex10%2010.PNG)
![P3 Ex10 - 11](images-pt3-1/pt3ex10%2011.PNG)

---

## Exercice 11 : Mouvements de l'année 2024

### Énoncé
Écrire un programme COBOL-DB2 permettant d'affichér les mouvements de l'année 2024 de tous les clients.

### Mon travail

J'ai utilisé la fonction YEAR(DATE_MVT) dans le WHERE pour filtrer uniquement l'année 2024. Une jointure avec CLIENT permet d'affichér le nom du client à côté de chaque mouvement.

**Compteur** : Le programme compte et affiché le nombre total de mouvements 2024 a la fin.

### Résolution

**Programme : MVT2024.cbl**

```cobol
           EXEC SQL
               DECLARE C-MVT2024 CURSOR FOR
               SELECT M.NUM_COMPTE, C.NOM_CLIENT,
                      M.DATE_MVT, M.LIB_MOUV,
                      M.MONTANT_MVT, M.SENS, M.NATURE
               FROM MOUVEMENT M
               INNER JOIN CLIENT C ON M.NUM_COMPTE = C.NUM_COMPTE
               WHERE YEAR(M.DATE_MVT) = 2024
               ORDER BY M.DATE_MVT, M.NUM_COMPTE
           END-EXEC.
```

**Technique utilisée** : Fonction YEAR() pour filtrer par année + jointure CLIENT

### Captures d'écran

![P3 Ex11 - 1](images-pt3-2/pt3ex11%201.PNG)
![P3 Ex11 - 2](images-pt3-2/pt3ex11%202.PNG)
![P3 Ex11 - 3](images-pt3-2/pt3ex11%203.PNG)
![P3 Ex11 - 4](images-pt3-2/pt3ex11%204.PNG)
![P3 Ex11 - 5](images-pt3-2/pt3ex11%205.PNG)
![P3 Ex11 - 6](images-pt3-2/pt3ex11%206.PNG)
![P3 Ex11 - 7](images-pt3-2/pt3ex11%207.PNG)

---

## Exercice 12 : Mouvements 2024 d'un client spécifique

### Énoncé
Refaire l'exercice 11 en précisant le numéro de compte d'un client déterminé. Récupérer le numéro via donnee In-Stream.

### Mon travail

Ce programme reprend la logique des mouvements 2024 (Ex11) mais pour un client spécifique. Le numéro de compte est lu dynamiquement via ACCEPT. Le curseur filtre sur `NUM_COMPTE` ET `YEAR(DATE_MVT) = 2024`.

**Différence avec Ex11** : Ex11 affiché tous les clients avec un JOIN, Ex12 filtre sur un seul client passé en SYSIN.

### Résolution

**Programme : RLV012.cbl**

```cobol
       WORKING-STORAGE SECTION.
      * Variable d'entrée (ACCEPT depuis SYSIN In-Stream)
       01 WS-NUM-COMPTE     PIC X(03).
      * Variables host pour DB2
       01 WS-NOM-CLIENT     PIC X(10).
       01 WS-DATE-MVT       PIC X(10).
       01 WS-LIB-MOUV       PIC X(15).
       01 WS-MONTANT-MVT    PIC S9(6)V99 COMP-3.
       01 WS-SENS           PIC X(02).
       01 WS-NATURE         PIC X(03).

      * Curseur pour mouvements 2024 d'un client spécifique
           EXEC SQL
               DECLARE C-MVT2024-CLI CURSOR FOR
               SELECT DATE_MVT, LIB_MOUV, MONTANT_MVT, SENS, NATURE
               FROM MOUVEMENT
               WHERE NUM_COMPTE = :WS-NUM-COMPTE
                 AND YEAR(DATE_MVT) = 2024
               ORDER BY DATE_MVT
           END-EXEC.

       PROCEDURE DIVISION.
       1000-LIRE-NUM-COMPTE.
           ACCEPT WS-NUM-COMPTE
           DISPLAY 'COMPTE DEMANDE : ' WS-NUM-COMPTE.
```

**JCL avec donnee In-Stream :**
```jcl
//SYSIN DD *
001
/*
```

**Technique utilisée** : CURSOR avec double filtre (NUM_COMPTE + YEAR) + ACCEPT pour paramètre dynamique

### Captures d'écran

![P3 Ex12 - 1](images-pt3-2/pt3ex12%201.PNG)
![P3 Ex12 - 2](images-pt3-2/pt3ex12%202.PNG)
![P3 Ex12 - 3](images-pt3-2/pt3ex12%203.PNG)
![P3 Ex12 - 4](images-pt3-2/pt3ex12%204.PNG)
![P3 Ex12 - 5](images-pt3-2/pt3ex12%205.PNG)
![P3 Ex12 - 6](images-pt3-2/pt3ex12%206.PNG)
![P3 Ex12 - 7](images-pt3-2/pt3ex12%207.PNG)
![P3 Ex12 - 9](images-pt3-2/pt3ex12%209.PNG)
![P3 Ex12 - 10](images-pt3-2/pt3ex12%2010.PNG)

---

# Annexes

## Liste des programmes COBOL

| Programme | Description |
|-----------|-------------|
| AFFREG | Afficher region |
| INSCLI | Insérer client |
| AFFCLI | Afficher clients region |
| MAJCLI | Mise à jour client |
| LSTRUPT | Liste avec ruptures |
| STATCLI | Statistiques DB/CR |
| TOTREG | Totaux par region |
| TOTMVT | Total mouvements |
| RELEVE | Relevé de compte |
| MVT2024 | Mouvements 2024 |
| RLV012 | Mouvements 2024 client |

## Liste des scripts SQL

| Script | Partie | Description |
|--------|--------|-------------|
| PT1EX01 | P1 | Création des tables (REGION, NATCPT, PROF, CLIENT) |
| PT1EX02 | P1 | Alimentation des tables de reference |
| PT1EX03 | P1 | Insertion des 20 clients |
| PT2EX01 | P2 | Extraction clients par profession |
| PT2EX02 | P2 | Répartition DB/CR |
| PT2EX03 | P2 | Répartition par region |
| PT2EX04 | P2 | Index sur CODE_REGION |
| PT2EX05 | P2 | Index sur CODE_PROF |
| PT2EX06 | P2 | Édition triés region/profession |
| PT2EX07 | P2 | Fusion populations (UNION) |
| PT2EX08 | P2 | Vue CLIENT_REDUIT |
| PT2EX09 | P2 | Analyse multi-critères |
| PT2EX10 | P2 | Clients anormalement débiteurs |
| PT3EX08 | P3 | Création table MOUVEMENT |

## Liste des vues créées

| Vue | Description |
|-----|-------------|
| V_CLIENT_COMPTABLE | Clients comptables |
| V_CLIENT_FONCTION | Clients fonctionnaires |
| V_CLIENT_MEDECIN | Clients medecins |
| V_CLIENT_DEBITEUR | Clients débiteurs |
| V_CLIENT_CREDITEUR | Clients créditeurs |
| V_CLIENT_PARIS | Clients region Paris |
| V_CLIENT_MARSEILLE | Clients region Marseille |
| V_CLIENT_LYON | Clients region Lyon |
| V_CLIENT_LILLE | Clients region Lille |
| V_CLIENT_REDUIT | Vue simplifiée CLIENT |

## Liste des index créés

| Index | Colonne |
|-------|---------|
| IDX_CLIENT_REGION | CODE_REGION |
| IDX_CLIENT_PROF | CODE_PROF |

---

# Conclusion

Ce projet m'a permis de mettre en pratique l'ensemble des compétences acquises durant la formation POEI Mainframe COBOL. A travers les trois parties du projet, j'ai pu :

- **Maîtriser SQL/DB2** : Création de tables avec contraintes (PK, FK, CHECK), insertion de données, requêtes complexes avec jointures, sous-requêtes, fonctions d'agrégation et vues.

- **Développer en COBOL-DB2** : Intégration du SQL embarqué dans les programmes COBOL, utilisation des curseurs pour le traitement multi-lignes, gestion des erreurs via SQLCODE, et techniques avancées (ruptures de controle, niveau 88, ACCEPT/SYSIN).

- **Travailler dans l'environnement z/OS** : Navigation ISPF, utilisation de SPUFI pour les requêtes interactives, compilation et execution de programmes via JCL.

Le projet couvre un cas concret de gestion clientèle dans le secteur financier, avec 12 programmes COBOL et plus de 10 requêtes SQL. Les principales difficultés rencontrées (mots réservés, gestion des erreurs, formats de données) m'ont permis de développer une approche méthodique de resolution de problèmes.

Cette experience constitue une base solide pour aborder des projets mainframe en entreprise.

---

*Rapport réalisé par Josué ROCHA - Formation POEI Mainframe COBOL - M2i Formation, Strasbourg - Décembre 2025*
