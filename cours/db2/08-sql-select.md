# Chapitre VIII - SQL SELECT et Jointures

## VIII-1 : Structure du SELECT

### Syntaxe complète

```sql
SELECT [DISTINCT] colonnes | *
FROM tables
[WHERE condition]
[GROUP BY colonnes]
[HAVING condition_groupe]
[ORDER BY colonnes [ASC|DESC]];
```

### Ordre d'exécution logique

```
┌─────────────────────────────────────────────────────────────────┐
│            ORDRE D'EXÉCUTION DU SELECT                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Ordre écrit :        Ordre exécuté :                           │
│                                                                  │
│  1. SELECT            5. SELECT (projection)                    │
│  2. FROM              1. FROM (source)                          │
│  3. WHERE             2. WHERE (filtrage lignes)                │
│  4. GROUP BY          3. GROUP BY (regroupement)                │
│  5. HAVING            4. HAVING (filtrage groupes)              │
│  6. ORDER BY          6. ORDER BY (tri final)                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## VIII-2 : Opérations fondamentales

### Projection (colonnes)

```sql
-- Toutes les colonnes
SELECT * FROM EMPLOYEE;

-- Colonnes spécifiques
SELECT EMP_NUM, EMP_NOM, SAL FROM EMPLOYEE;

-- Colonnes calculées
SELECT EMP_NOM, SAL, SAL * 12 AS SAL_ANNUEL FROM EMPLOYEE;

-- Éliminer les doublons
SELECT DISTINCT DEPT_NUM FROM EMPLOYEE;
SELECT DISTINCT POSTE, DEPT_NUM FROM EMPLOYEE;
```

### Sélection (lignes) - WHERE

```sql
-- Comparaison simple
SELECT * FROM EMPLOYEE WHERE DEPT_NUM = 30;
SELECT * FROM EMPLOYEE WHERE SAL > 2000;
SELECT * FROM EMPLOYEE WHERE EMP_NOM = 'MARTIN';

-- Opérateurs de comparaison
-- =, <>, <, >, <=, >=
SELECT * FROM EMPLOYEE WHERE SAL <> 1000;
```

### Opérateurs logiques

```sql
-- AND : les deux conditions vraies
SELECT * FROM EMPLOYEE
WHERE DEPT_NUM = 30 AND SAL > 1500;

-- OR : au moins une condition vraie
SELECT * FROM EMPLOYEE
WHERE DEPT_NUM = 30 OR DEPT_NUM = 20;

-- NOT : inverse la condition
SELECT * FROM EMPLOYEE
WHERE NOT DEPT_NUM = 30;

-- Combinaison (attention aux parenthèses !)
SELECT * FROM EMPLOYEE
WHERE (DEPT_NUM = 30 OR DEPT_NUM = 20) AND SAL > 2000;
```

### Opérateurs spéciaux

```sql
-- BETWEEN : plage de valeurs (inclus)
SELECT * FROM EMPLOYEE
WHERE SAL BETWEEN 1000 AND 2000;
-- Équivalent à : SAL >= 1000 AND SAL <= 2000

-- IN : liste de valeurs
SELECT * FROM EMPLOYEE
WHERE DEPT_NUM IN (10, 20, 30);
-- Équivalent à : DEPT_NUM = 10 OR DEPT_NUM = 20 OR DEPT_NUM = 30

-- LIKE : motifs de chaînes
SELECT * FROM EMPLOYEE WHERE EMP_NOM LIKE 'M%';     -- Commence par M
SELECT * FROM EMPLOYEE WHERE EMP_NOM LIKE '%N';     -- Finit par N
SELECT * FROM EMPLOYEE WHERE EMP_NOM LIKE '%AR%';   -- Contient AR
SELECT * FROM EMPLOYEE WHERE EMP_NOM LIKE 'M____';  -- M + 4 caractères

-- IS NULL / IS NOT NULL
SELECT * FROM EMPLOYEE WHERE COMM IS NULL;
SELECT * FROM EMPLOYEE WHERE COMM IS NOT NULL;
-- ⚠ Ne pas utiliser = NULL ou <> NULL (ne fonctionne pas !)
```

---

## VIII-3 : Alias et expressions

### Alias de colonnes

```sql
-- Alias avec AS
SELECT EMP_NOM AS NOM, SAL AS SALAIRE FROM EMPLOYEE;

-- Alias sans AS (espace)
SELECT EMP_NOM NOM, SAL SALAIRE FROM EMPLOYEE;

-- Alias avec espaces (guillemets)
SELECT EMP_NOM AS "Nom Employé", SAL AS "Salaire Mensuel"
FROM EMPLOYEE;
```

### Alias de tables

```sql
-- Alias de table (utile pour les jointures)
SELECT E.EMP_NUM, E.EMP_NOM, E.SAL
FROM EMPLOYEE E
WHERE E.DEPT_NUM = 30;
```

### Expressions et calculs

```sql
-- Calculs arithmétiques
SELECT EMP_NOM,
       SAL,
       SAL * 12 AS SAL_ANNUEL,
       SAL * 1.10 AS SAL_AUGMENTE
FROM EMPLOYEE;

-- Concaténation
SELECT EMP_NUM || ' - ' || EMP_NOM AS EMP_INFO
FROM EMPLOYEE;

-- Fonctions
SELECT UPPER(EMP_NOM) AS NOM_MAJ,
       LENGTH(EMP_NOM) AS LONGUEUR
FROM EMPLOYEE;
```

---

## VIII-4 : Tri avec ORDER BY

### Syntaxe

```sql
-- Tri ascendant (défaut)
SELECT * FROM EMPLOYEE ORDER BY SAL;
SELECT * FROM EMPLOYEE ORDER BY SAL ASC;

-- Tri descendant
SELECT * FROM EMPLOYEE ORDER BY SAL DESC;

-- Tri multiple
SELECT * FROM EMPLOYEE
ORDER BY DEPT_NUM ASC, SAL DESC;

-- Tri par position de colonne
SELECT EMP_NOM, SAL, DEPT_NUM
FROM EMPLOYEE
ORDER BY 3, 2 DESC;  -- 3=DEPT_NUM, 2=SAL

-- Tri par alias
SELECT EMP_NOM, SAL * 12 AS SAL_ANNUEL
FROM EMPLOYEE
ORDER BY SAL_ANNUEL DESC;
```

### Gestion des NULL dans le tri

```sql
-- Par défaut, NULL est trié en premier (ASC) ou dernier (DESC)
SELECT EMP_NOM, COMM FROM EMPLOYEE ORDER BY COMM;

-- Forcer les NULL à la fin
SELECT EMP_NOM, COMM
FROM EMPLOYEE
ORDER BY CASE WHEN COMM IS NULL THEN 1 ELSE 0 END, COMM;
```

---

## VIII-5 : Jointures

### Pourquoi les jointures ?

```
┌─────────────────────────────────────────────────────────────────┐
│                    BESOIN DE JOINTURE                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  EMPLOYEE                         DEPT                          │
│  ┌────────┬─────────┬────────┐   ┌────────┬───────────┐        │
│  │EMP_NUM │ EMP_NOM │DEPT_NUM│   │DEPT_NUM│ DEPT_NOM  │        │
│  ├────────┼─────────┼────────┤   ├────────┼───────────┤        │
│  │ 7369   │ ARTHUR  │   20   │   │   20   │ RECHERCHE │        │
│  │ 7499   │ PAUL    │   30   │   │   30   │ VENTES    │        │
│  └────────┴─────────┴────────┘   └────────┴───────────┘        │
│                 │                       ▲                       │
│                 └───────────────────────┘                       │
│                                                                  │
│  Question : "Quel est le nom du département de chaque employé ?"│
│  Réponse  : JOINTURE sur DEPT_NUM                               │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### INNER JOIN (jointure interne)

Retourne uniquement les lignes qui correspondent dans les deux tables.

```sql
-- Syntaxe moderne (ANSI SQL)
SELECT E.EMP_NUM, E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
INNER JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM;

-- Syntaxe ancienne (équivalent)
SELECT E.EMP_NUM, E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E, DEPT D
WHERE E.DEPT_NUM = D.DEPT_NUM;
```

```
┌─────────────────────────────────────────────────────────────────┐
│                       INNER JOIN                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  EMPLOYEE          DEPT              RÉSULTAT                   │
│  ┌─────┬─────┐    ┌─────┬──────┐    ┌─────┬─────┬──────────┐   │
│  │ EMP │DEPT │    │DEPT │ NOM  │    │ EMP │DEPT │ DEPT_NOM │   │
│  ├─────┼─────┤    ├─────┼──────┤    ├─────┼─────┼──────────┤   │
│  │  A  │  20 │───►│  20 │RECH  │    │  A  │  20 │ RECH     │   │
│  │  B  │  30 │───►│  30 │VENTES│    │  B  │  30 │ VENTES   │   │
│  │  C  │  40 │ X  │     │      │    │     │     │          │   │
│  └─────┴─────┘    └─────┴──────┘    └─────┴─────┴──────────┘   │
│                                                                  │
│  L'employé C (DEPT 40) n'apparaît pas car DEPT 40 n'existe pas │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### LEFT OUTER JOIN (jointure externe gauche)

Retourne toutes les lignes de la table de gauche, même sans correspondance.

```sql
SELECT E.EMP_NUM, E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
LEFT OUTER JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM;

-- Peut s'écrire aussi LEFT JOIN (OUTER est optionnel)
SELECT E.EMP_NUM, E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
LEFT JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM;
```

```
┌─────────────────────────────────────────────────────────────────┐
│                     LEFT OUTER JOIN                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  EMPLOYEE (gauche)   DEPT              RÉSULTAT                 │
│  ┌─────┬─────┐      ┌─────┬──────┐    ┌─────┬─────┬──────────┐ │
│  │ EMP │DEPT │      │DEPT │ NOM  │    │ EMP │DEPT │ DEPT_NOM │ │
│  ├─────┼─────┤      ├─────┼──────┤    ├─────┼─────┼──────────┤ │
│  │  A  │  20 │─────►│  20 │RECH  │    │  A  │  20 │ RECH     │ │
│  │  B  │  30 │─────►│  30 │VENTES│    │  B  │  30 │ VENTES   │ │
│  │  C  │  40 │─────►│     │      │    │  C  │  40 │ NULL     │ │
│  └─────┴─────┘      └─────┴──────┘    └─────┴─────┴──────────┘ │
│                                                                  │
│  L'employé C apparaît avec NULL pour DEPT_NOM                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### RIGHT OUTER JOIN (jointure externe droite)

Retourne toutes les lignes de la table de droite, même sans correspondance.

```sql
SELECT E.EMP_NUM, E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
RIGHT OUTER JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM;
```

### Auto-jointure (self-join)

Jointure d'une table avec elle-même.

```sql
-- Trouver le nom du manager de chaque employé
SELECT E.EMP_NOM AS EMPLOYE,
       M.EMP_NOM AS MANAGER
FROM EMPLOYEE E
LEFT JOIN EMPLOYEE M ON E.DIR = M.EMP_NUM;
```

### Jointure multiple

```sql
-- Joindre 3 tables
SELECT E.EMP_NOM, D.DEPT_NOM, G.GRADE
FROM EMPLOYEE E
INNER JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM
INNER JOIN SAL_GRILLE G ON E.SAL BETWEEN G.MIN_SAL AND G.MAX_SAL;
```

---

## VIII-6 : CASE (expressions conditionnelles)

### Syntaxe CASE WHEN

```sql
-- Équivalent de IF-THEN-ELSE ou EVALUATE COBOL
SELECT EMP_NOM, SAL,
    CASE
        WHEN SAL < 1000 THEN 'BAS'
        WHEN SAL BETWEEN 1000 AND 2000 THEN 'MOYEN'
        WHEN SAL BETWEEN 2001 AND 3000 THEN 'BON'
        ELSE 'EXCELLENT'
    END AS CATEGORIE_SAL
FROM EMPLOYEE;

-- CASE simple (comparaison d'égalité)
SELECT EMP_NOM, DEPT_NUM,
    CASE DEPT_NUM
        WHEN 10 THEN 'COMPTABILITE'
        WHEN 20 THEN 'RECHERCHE'
        WHEN 30 THEN 'VENTES'
        ELSE 'AUTRE'
    END AS DEPT_LIBELLE
FROM EMPLOYEE;
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE VIII - RÉSUMÉ                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  VIII-1 STRUCTURE SELECT                                         │
│         SELECT → FROM → WHERE → GROUP BY → HAVING → ORDER BY    │
│                                                                  │
│  VIII-2 OPÉRATIONS                                               │
│         • Projection : colonnes, *, DISTINCT                    │
│         • Sélection : WHERE avec =, <>, <, >, AND, OR, NOT      │
│         • Opérateurs : BETWEEN, IN, LIKE, IS NULL               │
│                                                                  │
│  VIII-3 ALIAS                                                    │
│         • Colonnes : col AS alias                               │
│         • Tables : FROM table alias                             │
│         • Expressions calculées                                 │
│                                                                  │
│  VIII-4 TRI                                                      │
│         • ORDER BY col [ASC|DESC]                               │
│         • Tri multiple, par position, par alias                 │
│                                                                  │
│  VIII-5 JOINTURES                                                │
│         • INNER JOIN : correspondances uniquement               │
│         • LEFT JOIN : toutes les lignes de gauche              │
│         • RIGHT JOIN : toutes les lignes de droite             │
│         • Self-join : table avec elle-même                     │
│                                                                  │
│  VIII-6 CASE                                                     │
│         • CASE WHEN condition THEN result ELSE default END     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Exercice 1 : SELECT de base

1. Liste des employés gagnant plus de 2000
2. Employés dont le nom commence par 'M'
3. Employés des départements 10 ou 30 triés par salaire décroissant

### Exercice 2 : Jointures

1. Liste des employés avec le nom de leur département
2. Liste de TOUS les départements avec leurs employés (même les vides)
3. Liste des employés avec le nom de leur manager

### Exercice 3 : CASE

Créez une requête affichant le nom, salaire et une colonne "NIVEAU" :
- JUNIOR si salaire < 1500
- CONFIRME si salaire entre 1500 et 2500
- SENIOR si salaire > 2500

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VII - SQL DML](07-sql-dml.md) | [Chapitre IX - Agrégations](09-sql-avance.md) |

---
*Formation DB2/SQL - M2i Formation*
