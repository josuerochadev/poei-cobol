# Rapport de Projet - Mini-Projet DB2/COBOL

**Theme** : Developpement d'un mini-projet COBOL-DB2 sous z/OS pour le suivi clientele dans le secteur financier.

**Candidat** : FORM1112
**Date** : Decembre 2025
**Formation** : POEI Developpeur Mainframe COBOL - M2i Formation

---

## Introduction

Ce projet a ete realise dans le cadre de la formation POEI Developpeur Mainframe COBOL. L'objectif etait de mettre en pratique les competences acquises en SQL/DB2 et en programmation COBOL avec acces aux bases de donnees.

### Environnement de travail

- **Systeme** : z/OS sous emulateur Hercules (TK4-)
- **Interface** : ISPF / TSO
- **Base de donnees** : DB2
- **Outil SQL** : SPUFI
- **Libraries utilisees** :
  - `FORM1112.FINANCE.SOURCE` : Programmes COBOL et scripts SQL
  - `FORM1112.FINANCE.RESULT` : Fichiers de sortie SPUFI

### Demarche suivie

1. **Analyse du sujet** : Etude des specifications et du schema de donnees
2. **Creation des tables** : Definition des structures avec contraintes PK/FK
3. **Chargement des donnees** : Insertion de 20 clients et des referentiels
4. **Requetes SQL** : Vues, index, jointures, agregations
5. **Programmes COBOL-DB2** : Developpement avec Embedded SQL

### Difficultes rencontrees et solutions

| Probleme | Solution |
|----------|----------|
| `POSITION` mot reserve COBOL | Renomme en `POS` |
| Erreur B37 (espace SPUFI) | Augmentation des tracks du fichier output |
| _[Ajouter vos propres difficultes]_ | _[Vos solutions]_ |

### Competences mises en oeuvre

- Conception de base de donnees relationnelle (PK, FK, contraintes CHECK)
- Requetes SQL : SELECT, INSERT, UPDATE, CREATE VIEW, CREATE INDEX
- Fonctions SQL : SUM, AVG, COUNT, CASE WHEN, COALESCE
- Jointures : INNER JOIN, sous-requetes correlees
- COBOL-DB2 : SELECT INTO, CURSOR, variables host, SQLCODE
- Techniques COBOL : niveau 88, ruptures de controle, ACCEPT

---

## Sommaire

1. [Partie 1 : Creation et chargement des donnees](#partie-1--creation-et-chargement-des-donnees)
2. [Partie 2 : Exploitation et manipulation SQL](#partie-2--exploitation-et-manipulation-sql)
3. [Partie 3 : Programmation COBOL-DB2](#partie-3--programmation-cobol-db2)

---

# Partie 1 : Creation et chargement des donnees

## Exercice 1 : Creation des tables

### Enonce

Executer les instructions CREATE TABLE de chacune des tables. Chaque table doit comporter sa clé primaire, et les cles etrangeres doivent etre correctement reliees (FK).

### Mon travail

J'ai commence par creer les 3 tables referentielles (REGION, NATCOMPT, PROFESSI) car elles n'ont pas de dependances. Ensuite, j'ai cree la table CLIENT qui possede des cles etrangeres vers ces 3 tables.

**Points importants :**
- L'ordre de creation est crucial : les tables referencees doivent exister avant la table qui les reference
- J'ai renomme la colonne `POSITION` en `POS` car POSITION est un mot reserve en COBOL
- La contrainte CHECK sur POS garantit que seules les valeurs 'DB' et 'CR' sont acceptees

### Resolution

```sql
-- Table REGION (referentiel des regions)
CREATE TABLE REGION (
    CODE_REGION CHAR(2) NOT NULL PRIMARY KEY,
    NOM_REGION  VARCHAR(15)
);

-- Table NATCOMPT (referentiel des natures de compte)
CREATE TABLE NATCOMPT (
    CODE_NATCPT CHAR(2) NOT NULL PRIMARY KEY,
    LIB_NATCPT  VARCHAR(30)
);

-- Table PROFESSI (referentiel des professions)
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

> **Note** : `POS` remplace `POSITION` (mot reserve COBOL)

### Captures d'ecran

![pt1ex01-1](images-pt1/pt1ex01%201.PNG)
![pt1ex01-2](images-pt1/pt1ex01%202.PNG)
![pt1ex01-3](images-pt1/pt1ex01%203.PNG)
![pt1ex01-4](images-pt1/pt1ex01%204.PNG)
![pt1ex01-5](images-pt1/pt1ex01%205.PNG)

---

## Exercice 2 : Alimentation des tables

### Enonce

Inserer les donnees de base avec des commandes INSERT INTO. Le nombre de clients doit etre au minimum de 20 avec des repartitions equilibrees.

### Mon travail

J'ai d'abord insere les donnees dans les tables referentielles, puis les 20 clients. J'ai veille a respecter une repartition equilibree des donnees :
- 5 clients par region (Paris, Marseille, Lyon, Lille)
- 10 hommes et 10 femmes
- Environ 40% debiteurs (8) et 60% crediteurs (12)
- Toutes les professions et types de comptes representes

J'ai utilise la fonction DATE() de DB2 pour formater les dates de naissance.

### Resolution

**Tables referentielles :**

```sql
-- REGION (4 regions)
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

**Repartition des donnees :**

| Critere | Repartition |
|---------|-------------|
| Regions | 5 clients par region |
| Sexe | 10 M / 10 F |
| Position | 8 DB / 12 CR |
| Professions | Toutes representees |

### Captures d'ecran

![pt1ex02-1](images-pt1/pt1ex02%201.PNG)
![pt1ex02-2](images-pt1/pt1ex02%202.PNG)
![pt1ex02-3](images-pt1/pt1ex02%203.PNG)
![pt1ex02-4](images-pt1/pt1ex02%204.PNG)
![pt1ex02-5](images-pt1/pt1ex02%205.PNG)
![pt1ex02-6](images-pt1/pt1ex02%206.PNG)
![pt1ex02-7](images-pt1/pt1ex02%207.PNG)

---

## Exercice 3 : Verification de coherence

### Enonce

Effectuer des requetes pour valider le chargement.

### Mon travail

J'ai execute des SELECT simples sur chaque table pour verifier que toutes les donnees ont ete correctement inserees. J'ai verifie :
- 4 regions dans REGION
- 5 natures de compte dans NATCOMPT
- 6 professions dans PROFESSI
- 20 clients dans CLIENT avec les bonnes references (FK valides)

### Resolution

```sql
SELECT * FROM REGION;
SELECT * FROM NATCOMPT;
SELECT * FROM PROFESSI;
SELECT * FROM CLIENT;
```

### Captures d'ecran

![pt1ex03-1](images-pt1/pt1ex03%201.PNG)
![pt1ex03-2](images-pt1/pt1ex03%202.PNG)
![pt1ex03-3](images-pt1/pt1ex03%203.PNG)
![pt1ex03-4](images-pt1/pt1ex03%204.PNG)
![pt1ex03-5](images-pt1/pt1ex03%205.PNG)

---

# Partie 2 : Exploitation et manipulation SQL

## Exercice 1 : Extraction des clients par profession

### Enonce

Extraire la liste des clients exercant les professions COMPTABLE, FONCTIONNAIRE et MEDECIN. Creer des vues permanentes pour chacune de ces categories.

### Mon travail

J'ai d'abord realise des SELECT simples pour verifier les donnees, puis j'ai cree 3 vues permanentes. J'ai utilise des jointures INNER JOIN avec la table PROFESSI pour inclure le libelle de la profession dans chaque vue.

**Choix technique** : Filtrer par LIB_PROF plutot que CODE_PROF rend le code plus lisible et maintenable.

### Resolution

```sql
-- Requetes de selection
SELECT * FROM CLIENT WHERE CODE_PROF = '15';  -- COMPTABLE
SELECT * FROM CLIENT WHERE CODE_PROF = '25';  -- FONCTIONNAIRE
SELECT * FROM CLIENT WHERE CODE_PROF = '05';  -- MEDECIN

-- Vues avec jointure (libelle profession)
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

-- Verification
SELECT * FROM V_CLIENT_COMPTABLE;
SELECT * FROM V_CLIENT_FONCTION;
SELECT * FROM V_CLIENT_MEDECIN;
```

### Captures d'ecran

![pt2ex01-1](images-pt1/pt2ex01%201.PNG)
![pt2ex01-2](images-pt1/pt2ex01%202.PNG)
![pt2ex01-3](images-pt1/pt2ex01%203.PNG)
![pt2ex01-4](images-pt1/pt2ex01%204.PNG)
![pt2ex01-5](images-pt1/pt2ex01%205.PNG)
![pt2ex01-6](images-pt1/pt2ex01%206.PNG)

---

## Exercice 2 : Repartition selon la position du compte (DB/CR)

### Enonce

Realiser deux requetes SQL permettant d'isoler les clients debiteurs (DB) et les clients crediteurs (CR). Materialiser ces selections sous forme de vues.

### Mon travail

J'ai cree deux vues distinctes pour separer les clients selon leur position. Ces vues seront utiles pour les programmes COBOL-DB2 qui traiteront separement les debiteurs et crediteurs.

**Observation** : Sur les 20 clients, 8 sont debiteurs (solde negatif) et 12 sont crediteurs (solde positif).

### Resolution

```sql
-- Requetes de selection
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

-- Verification
SELECT * FROM V_CLIENT_DEBITEUR;
SELECT * FROM V_CLIENT_CREDITEUR;
```

### Captures d'ecran

![pt2ex02-1](images-pt1/pt2ex02%201.PNG)
![pt2ex02-2](images-pt1/pt2ex02%202.PNG)
![pt2ex02-3](images-pt1/pt2ex02%203.PNG)
![pt2ex02-4](images-pt1/pt2ex02%204.PNG)
![pt2ex02-5](images-pt1/pt2ex02%205.PNG)
![pt2ex02-6](images-pt1/pt2ex02%206.PNG)
![pt2ex02-7](images-pt1/pt2ex02%207.PNG)

---

## Exercice 3 : Repartition des clients par region

### Enonce

Mettre en place une repartition des clients par region. Chaque region doit faire l'objet d'une extraction distincte.

### Mon travail

J'ai cree 4 vues, une par region (Paris, Marseille, Lyon, Lille). Chaque vue inclut une jointure avec REGION pour afficher le nom de la region.

**Verification** : Chaque region contient exactement 5 clients comme prevu dans la repartition initiale.

### Resolution

```sql
-- Requetes par region
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

-- Verification
SELECT * FROM V_CLIENT_MARSEILLE;
```

### Captures d'ecran

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

### Enonce
Creer un index secondaire sur la colonne CODE_REGION pour optimiser les recherches par region.

### Mon travail

J'ai cree un index sur CODE_REGION pour accelerer les requetes de recherche par region. Cet index est particulierement utile car plusieurs de nos vues et requetes filtrent par region.

**Impact** : Les SELECT avec WHERE CODE_REGION = ... beneficient desormais d'un acces indexe au lieu d'un scan complet de la table.

### Resolution

```sql
CREATE INDEX IDX_CLIENT_REGION ON CLIENT(CODE_REGION);

-- Verification
SELECT * FROM CLIENT WHERE CODE_REGION = '01';
```

### Captures d'ecran

![pt2ex04-1](images-pt1/pt2ex04%201.PNG)
![pt2ex04-2](images-pt1/pt2ex04%202.PNG)

---

## Exercice 5 : Index secondaire sur la profession

### Enonce
Creer un index secondaire sur la colonne CODE_PROF afin de faciliter les traitements regroupes par profession.

### Mon travail

De meme que pour la region, j'ai cree un index sur CODE_PROF. Les vues V_CLIENT_COMPTABLE, V_CLIENT_FONCTION et V_CLIENT_MEDECIN beneficieront de cet index.

**Remarque** : Les deux index (region et profession) peuvent etre utilises simultanement par l'optimiseur DB2 pour les requetes combinant ces deux criteres.

### Resolution

```sql
CREATE INDEX IDX_CLIENT_PROF ON CLIENT(CODE_PROF);

-- Verification
SELECT * FROM CLIENT WHERE CODE_PROF = '15';
```

### Captures d'ecran

![pt2ex05-1](images-pt1/pt2ex05%201.PNG)
![pt2ex05-2](images-pt1/pt2ex05%202.PNG)

---

## Exercice 6 : Edition des clients tries par region et profession

### Enonce
Concevoir une requete SQL permettant d'afficher les clients dans l'ordre suivant :
1. Par region (CODE_REGION)
2. Puis par profession (CODE_PROF)
3. Puis par numero de compte (NUM_COMPTE)

### Mon travail

J'ai ecrit une requete avec double jointure (REGION et PROFESSI) pour afficher les libelles plutot que les codes. Le tri multi-niveaux ORDER BY permet de regrouper visuellement les donnees.

**Utilite** : Cette requete servira de base pour le programme COBOL-DB2 avec ruptures (P3-Ex05).

### Resolution

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

### Captures d'ecran

![pt2ex06-1](images-pt1/pt2ex06%201.PNG)
![pt2ex06-2](images-pt1/pt2ex06%202.PNG)
![pt2ex06-3](images-pt1/pt2ex06%203.PNG)

---

## Exercice 7 : Fusion de deux populations de clients

### Enonce
Realiser une requete SQL permettant de fusionner les listes de clients COMPTABLES et FONCTIONNAIRES dans un meme resultat.

### Mon travail

J'ai utilise l'operateur UNION pour fusionner deux SELECT independants. Chaque SELECT filtre sur une profession differente et inclut la jointure avec PROFESSI pour le libelle.

**Note technique** : UNION elimine automatiquement les doublons (si un client etait dans les deux categories). Pour conserver les doublons, on utiliserait UNION ALL.

### Resolution

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

### Captures d'ecran

![pt2ex07-1](images-pt1/pt2ex07%201.PNG)
![pt2ex07-2](images-pt1/pt2ex07%202.PNG)
![pt2ex07-3](images-pt1/pt2ex07%203.PNG)

---

## Exercice 8 : Vue CLIENT reduit

### Enonce
Creer une vue simplifiee de la table CLIENT contenant uniquement : numero de compte, code region, nature de compte, nom/prenom, activite professionnelle, situation familiale, solde, position.

### Mon travail

J'ai cree une vue qui exclut les colonnes DATE_NAIS, SEXE et ADRESSE. Cette vue allege les requetes qui n'ont pas besoin de ces informations personnelles.

**Avantage** : La vue peut aussi servir a limiter l'acces aux donnees sensibles (date de naissance, adresse) pour certains utilisateurs.

### Resolution

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

-- Verification
SELECT * FROM V_CLIENT_REDUIT;
```

### Captures d'ecran

![pt2ex08-1](images-pt1/pt2ex08%201.PNG)
![pt2ex08-2](images-pt1/pt2ex08%202.PNG)
![pt2ex08-3](images-pt1/pt2ex08%203.PNG)
![pt2ex08-4](images-pt1/pt2ex08%204.PNG)

---

## Exercice 9 : Analyse multi-criteres avec agregations conditionnelles

### Enonce
Afficher, pour chaque region :
- Nombre total de clients
- Nombre de clients debiteurs
- Nombre de clients crediteurs
- Solde total debiteur
- Solde total crediteur

### Mon travail

J'ai utilise la technique CASE WHEN dans les fonctions d'agregation pour calculer des sous-totaux conditionnels en une seule requete. Cela evite de faire plusieurs requetes separees.

**Version simplifiee** : Sans jointure, directement sur CLIENT avec GROUP BY CODE_REGION. Les alias entre guillemets permettent des noms descriptifs avec espaces.

### Resolution

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

### Captures d'ecran

![pt2ex09-1](images-pt1/pt2ex09%201.PNG)
![pt2ex09-2](images-pt1/pt2ex09%202.PNG)
![pt2ex09-3](images-pt1/pt2ex09%203.PNG)

---

## Exercice 10 : Clients anormalement debiteurs par profession

### Enonce
Lister les clients debiteurs dont le solde est superieur a la moyenne des soldes debiteurs de leur profession.

### Mon travail

Cet exercice m'a demande une sous-requete correlee. La sous-requete calcule la moyenne des soldes debiteurs pour chaque profession, et la requete principale compare chaque client a cette moyenne.

**Subtilite** : Les soldes debiteurs etant negatifs, un client "plus debiteur" a un solde inferieur (ex: -450 < -200). D'ou l'utilisation de `<` au lieu de `>`.

### Resolution

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

> **Note** : La comparaison utilise `<` car les soldes debiteurs sont negatifs. Un solde de -450 est "plus debiteur" qu'un solde de -200.

### Captures d'ecran

![pt2ex10-1](images-pt1/pt2ex10%201.PNG)
![pt2ex10-2](images-pt1/pt2ex10%202.PNG)

---

# Partie 3 : Programmation COBOL-DB2

## Exercice 1 : Afficher la region Marseille

### Enonce
Ecrire un programme COBOL-DB2 permettant d'afficher la region Marseille (02).

### Mon travail

Premier programme COBOL-DB2 : j'ai utilise SELECT INTO pour lire une seule ligne. La variable SQLCODE me permet de verifier si la requete a reussi (0) ou echoue.

**Utilisation de DCLGEN** : Au lieu de declarer manuellement les variables host, j'utilise `EXEC SQL INCLUDE REGION END-EXEC` pour inclure les variables generees par l'utilitaire DCLGEN de DB2.

**Avantages DCLGEN** :
- Variables host generees automatiquement depuis la structure de la table
- Correspondance exacte avec les types de colonnes DB2
- Maintenabilite : si la table change, on regenere le DCLGEN

### Resolution

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

**Techniques utilisees** :
- SELECT INTO (lecture d'une seule ligne)
- DCLGEN (variables host generees par DB2)

### Captures d'ecran suggerees
- [ ] Code source dans ISPF
- [ ] Resultat d'execution (CODE: 02, NOM: MARSEILLE)

---

## Exercice 2 : Inserer un nouveau client

### Enonce
Ecrire un programme COBOL-DB2 permettant d'inserer un nouveau client dans la table CLIENT.

### Mon travail

J'ai utilise INSERT INTO avec des variables host. Les donnees du client sont lues depuis SYSIN via des ACCEPT (une ligne par champ). J'ai ajoute la gestion transactionnelle avec COMMIT en cas de succes et ROLLBACK en cas d'erreur.

**JCL requis** : Les donnees du client sont passees via SYSIN (12 lignes) :
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

### Resolution

**Programme : INSCLI.cbl**

```cobol
       1000-LIRE-DONNEES.
      * Lecture des donnees depuis SYSIN (JCL In-Stream)
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

**Techniques utilisees** :
- ACCEPT pour lire les donnees depuis SYSIN
- FUNCTION NUMVAL pour convertir le solde texte en numerique
- COMMIT/ROLLBACK pour la gestion transactionnelle

### Captures d'ecran suggerees
- [ ] Execution du programme (message de succes)
- [ ] Verification avec SELECT du nouveau client

---

## Exercice 3 : Afficher tous les clients de Marseille

### Enonce
Ecrire un programme COBOL-DB2 permettant d'afficher tous les clients de la region Marseille (02).

### Mon travail

Pour lire plusieurs lignes, j'ai utilise un CURSOR. Le cycle complet est : DECLARE (definition), OPEN (ouverture), FETCH en boucle (lecture), CLOSE (fermeture).

**Gestion fin de fichier** : SQLCODE = 100 indique qu'il n'y a plus de lignes a lire.

### Resolution

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

**Technique utilisee** : CURSOR (DECLARE, OPEN, FETCH, CLOSE) pour lecture multiple

### Captures d'ecran suggerees
- [ ] Liste des clients Marseille (5 clients)

---

## Exercice 4 : Mise a jour d'un client

### Enonce
Ecrire un programme COBOL-DB2 qui permet de mettre a jour l'adresse, le solde et la position d'un client existant.

### Mon travail

J'ai utilise UPDATE avec une clause WHERE pour cibler un client specifique (005). Comme pour l'INSERT, j'applique COMMIT apres verification du SQLCODE.

**Test** : J'ai verifie avec un SELECT avant et apres la mise a jour pour confirmer les changements.

### Resolution

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
      * Lecture des donnees depuis SYSIN (JCL In-Stream)
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

**Technique utilisee** : UPDATE avec clause WHERE + COMMIT + ACCEPT pour donnees dynamiques

### Captures d'ecran suggerees
- [ ] Execution (message de succes)
- [ ] Verification avant/apres mise a jour

---

## Exercice 5 : Liste avec ruptures

### Enonce
Ecrire un programme COBOL-DB2 qui affiche la liste des clients tries par region puis par profession. Pour chaque changement, afficher un titre de rupture.

### Mon travail

J'ai implemente les ruptures de controle en conservant les valeurs precedentes de CODE_REGION et CODE_PROF. A chaque FETCH, je compare les nouvelles valeurs avec les precedentes pour detecter les changements.

**Logique** : Si la region change, j'affiche le titre region ET je reinitialise la rupture profession (car on change de groupe).

### Resolution

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

**Technique utilisee** : Variables de rupture pour detecter les changements de groupe

### Captures d'ecran suggerees
- [ ] Sortie avec les en-tetes de rupture region/profession

---

## Exercice 6 : Statistiques DB/CR

### Enonce
Ecrire un programme COBOL-DB2 permettant de calculer le montant general et la moyenne des comptes debiteurs et crediteurs.

### Mon travail

J'ai fait deux SELECT INTO avec les fonctions SUM, AVG et COUNT : un pour les debiteurs (POS = 'DB') et un pour les crediteurs (POS = 'CR').

**Affichage** : Les variables d'edition (PIC ZZZ,ZZ9.99) permettent de formater les montants avec separateurs et decimales.

### Resolution

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

**Technique utilisee** : Fonctions d'agregation SQL (SUM, AVG, COUNT)

### Captures d'ecran suggerees
- [ ] Affichage des statistiques (totaux et moyennes)

---

## Exercice 7 : Totaux par region avec niveau 88

### Enonce
Ecrire un programme COBOL-DB2 qui calcule, pour chaque region, la valeur totale des comptes debiteurs et crediteurs. Utiliser une variable conditionnelle (niveau 88).

### Mon travail

J'ai declare des niveaux 88 pour representer les 4 regions (REGION-PARIS, REGION-MARSEILLE, etc.). Une boucle PERFORM VARYING parcourt les regions et SET permet d'activer chaque condition.

**Avantage du niveau 88** : Le code est plus lisible avec IF REGION-PARIS qu'avec IF WS-CODE-REGION = '01'.

### Resolution

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

**Technique utilisee** : Niveau 88 pour representer les codes regions + boucle PERFORM VARYING

### Captures d'ecran suggerees
- [ ] Totaux DB/CR pour chaque region

---

## Exercice 8 : Creation table MOUVEMENT

### Enonce
Creer une table DB2 permettant de gerer les mouvements des clients avec : numero de compte, libelle, montant, sens (DB/CR), nature (CHQ/VER/VIR), date.

### Mon travail

J'ai cree la table MOUVEMENT avec une cle etrangere vers CLIENT(NUM_COMPTE). Deux contraintes CHECK valident les valeurs de SENS (DB/CR) et NATURE (CHQ/VER/VIR).

**Donnees de test** : J'ai insere plusieurs mouvements pour differents clients afin de tester les programmes suivants (releve, total, etc.).

### Resolution

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

-- Donnees de test
INSERT INTO MOUVEMENT VALUES ('001','VIREMENT SAL',1500.00,'CR','VIR',DATE('2024-01-15'));
INSERT INTO MOUVEMENT VALUES ('001','CHEQUE 001',200.00,'DB','CHQ',DATE('2024-01-20'));
-- ... autres mouvements de test ...
```

### Captures d'ecran suggerees
- [ ] Creation de la table MOUVEMENT
- [ ] SELECT * FROM MOUVEMENT (donnees de test)

---

## Exercice 9 : Total mouvements d'un client

### Enonce
Ecrire un programme COBOL-DB2 permettant de calculer le montant total des mouvements d'un client et le nombre total de mouvements. Recevoir le numero via ACCEPT.

### Mon travail

J'ai utilise ACCEPT pour lire le numero de compte depuis SYSIN. La requete SELECT utilise SUM et COUNT avec COALESCE pour eviter les valeurs NULL si le client n'a pas de mouvements.

**Parametrage JCL** : Le numero de compte est passe via donnee In-Stream (//SYSIN DD *).

### Resolution

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

**Technique utilisee** : ACCEPT pour saisie + SUM/COUNT + COALESCE pour gerer les NULL

**Validation importante** : Le programme verifie que le numero de compte n'est pas vide avant d'executer les requetes SQL. Cela evite l'erreur ABEND 4038 si le SYSIN est mal configure.

### Captures d'ecran suggerees
- [ ] JCL avec donnee In-Stream (SYSIN)
- [ ] Resultat pour un client specifique

---

## Exercice 10 : Releve de compte

### Enonce
Ecrire un programme COBOL-DB2 permettant d'editer un releve de compte des mouvements d'un client avec colonnes Credit/Debit separees.

### Mon travail

J'ai cree un releve bancaire avec en-tete client et liste des mouvements. Chaque mouvement s'affiche soit dans la colonne Credit (si SENS = 'CR') soit dans la colonne Debit.

**Presentation** : L'en-tete affiche le nom du client et son numero de compte, suivi des colonnes Date/Libelle/Credit/Debit.

**JCL requis** : Le numero de compte est lu via ACCEPT, donc le JCL doit inclure :
```jcl
//SYSIN DD *
001
/*
```

### Resolution

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

### Captures d'ecran suggerees
- [ ] Releve de compte formate avec colonnes Credit/Debit

---

## Exercice 11 : Mouvements de l'annee 2024

### Enonce
Ecrire un programme COBOL-DB2 permettant d'afficher les mouvements de l'annee 2024 de tous les clients.

### Mon travail

J'ai utilise la fonction YEAR(DATE_MVT) dans le WHERE pour filtrer uniquement l'annee 2024. Une jointure avec CLIENT permet d'afficher le nom du client a cote de chaque mouvement.

**Compteur** : Le programme compte et affiche le nombre total de mouvements 2024 a la fin.

### Resolution

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

**Technique utilisee** : Fonction YEAR() pour filtrer par annee + jointure CLIENT

### Captures d'ecran suggerees
- [ ] Liste des mouvements 2024 avec compteur total

---

## Exercice 12 : Releve compte specifique (012)

### Enonce
Refaire l'exercice 10 en precisant le numero de compte d'un client determine (ex: 012). Recuperer le numero via donnee In-Stream.

### Mon travail

Ce programme reprend la logique du releve (Ex10) mais le numero de compte est lu dynamiquement via ACCEPT au lieu d'etre code en dur. Le JCL passe le numero '012' via SYSIN.

**Avantage** : Le meme programme peut generer le releve de n'importe quel client en changeant simplement la donnee In-Stream.

### Resolution

**Programme : RLV012.cbl**

```cobol
       PROCEDURE DIVISION.
       1000-LIRE-NUM-COMPTE.
           ACCEPT WS-NUM-COMPTE
           DISPLAY 'COMPTE DEMANDE : ' WS-NUM-COMPTE.
```

**JCL avec donnee In-Stream :**
```jcl
//SYSIN DD *
012
/*
```

### Captures d'ecran suggerees
- [ ] JCL avec SYSIN In-Stream
- [ ] Releve du client 012 (LEROY SOPHIE)

---

# Annexes

## Liste des programmes COBOL

| Programme | Description |
|-----------|-------------|
| AFFREG | Afficher region |
| INSCLI | Inserer client |
| AFFCLI | Afficher clients region |
| MAJCLI | Mise a jour client |
| LSTRUPT | Liste avec ruptures |
| STATCLI | Statistiques DB/CR |
| TOTREG | Totaux par region |
| TOTMVT | Total mouvements |
| RELEVE | Releve de compte |
| MVT2024 | Mouvements 2024 |
| RLV012 | Releve specifique |

## Liste des vues creees

| Vue | Description |
|-----|-------------|
| V_CLIENT_COMPTABLE | Clients comptables |
| V_CLIENT_FONCTION | Clients fonctionnaires |
| V_CLIENT_MEDECIN | Clients medecins |
| V_CLIENT_DEBITEUR | Clients debiteurs |
| V_CLIENT_CREDITEUR | Clients crediteurs |
| V_CLIENT_PARIS | Clients region Paris |
| V_CLIENT_MARSEILLE | Clients region Marseille |
| V_CLIENT_LYON | Clients region Lyon |
| V_CLIENT_LILLE | Clients region Lille |
| V_CLIENT_REDUIT | Vue simplifiee CLIENT |

## Liste des index crees

| Index | Colonne |
|-------|---------|
| IDX_CLIENT_REGION | CODE_REGION |
| IDX_CLIENT_PROF | CODE_PROF |

---

*Rapport genere pour la formation POEI Mainframe COBOL - M2i Formation - Decembre 2025*
