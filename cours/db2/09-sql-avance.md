# Chapitre IX - Agrégations et Sous-requêtes

## IX-1 : Fonctions d'agrégation

### Vue d'ensemble

Les fonctions d'agrégation calculent une valeur unique à partir d'un ensemble de lignes.

```
┌─────────────────────────────────────────────────────────────────┐
│                 FONCTIONS D'AGRÉGATION                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  COUNT(*)    : Nombre de lignes                                 │
│  COUNT(col)  : Nombre de valeurs non NULL                       │
│  SUM(col)    : Somme des valeurs                                │
│  AVG(col)    : Moyenne des valeurs                              │
│  MIN(col)    : Valeur minimum                                   │
│  MAX(col)    : Valeur maximum                                   │
│                                                                  │
│  ⚠ Les fonctions d'agrégation ignorent les valeurs NULL         │
│     (sauf COUNT(*) qui compte toutes les lignes)                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemples simples

```sql
-- Nombre total d'employés
SELECT COUNT(*) AS NB_EMPLOYES FROM EMPLOYEE;

-- Nombre d'employés ayant une commission
SELECT COUNT(COMM) AS NB_AVEC_COMM FROM EMPLOYEE;

-- Somme des salaires
SELECT SUM(SAL) AS MASSE_SALARIALE FROM EMPLOYEE;

-- Salaire moyen
SELECT AVG(SAL) AS SALAIRE_MOYEN FROM EMPLOYEE;

-- Salaire min et max
SELECT MIN(SAL) AS SAL_MIN, MAX(SAL) AS SAL_MAX FROM EMPLOYEE;

-- Combinaison
SELECT COUNT(*) AS NB,
       SUM(SAL) AS TOTAL,
       AVG(SAL) AS MOYENNE,
       MIN(SAL) AS MINIMUM,
       MAX(SAL) AS MAXIMUM
FROM EMPLOYEE;
```

### DISTINCT dans les agrégations

```sql
-- Nombre de postes différents
SELECT COUNT(DISTINCT POSTE) AS NB_POSTES FROM EMPLOYEE;

-- Somme des salaires distincts
SELECT SUM(DISTINCT SAL) AS TOTAL_SAL_DISTINCTS FROM EMPLOYEE;
```

---

## IX-2 : GROUP BY

### Concept

**GROUP BY** regroupe les lignes ayant les mêmes valeurs et permet d'appliquer les fonctions d'agrégation par groupe.

```
┌─────────────────────────────────────────────────────────────────┐
│                      GROUP BY                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Données :                       Après GROUP BY DEPT_NUM :      │
│  ┌──────────┬─────┐             ┌──────────┬───────────────┐   │
│  │ DEPT_NUM │ SAL │             │ DEPT_NUM │ COUNT │ SUM   │   │
│  ├──────────┼─────┤             ├──────────┼───────┼───────┤   │
│  │    20    │ 800 │             │    20    │   2   │ 3775  │   │
│  │    30    │1600 │      ►      │    30    │   3   │ 4100  │   │
│  │    20    │2975 │             └──────────┴───────┴───────┘   │
│  │    30    │1250 │                                             │
│  │    30    │1250 │                                             │
│  └──────────┴─────┘                                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Syntaxe et règle

```sql
SELECT colonnes_groupement, fonctions_agregation
FROM table
WHERE condition_lignes
GROUP BY colonnes_groupement;
```

**Règle importante** : Toute colonne du SELECT qui n'est pas une fonction d'agrégation DOIT apparaître dans le GROUP BY.

```sql
-- Correct
SELECT DEPT_NUM, COUNT(*), AVG(SAL)
FROM EMPLOYEE
GROUP BY DEPT_NUM;

-- Incorrect (EMP_NOM n'est pas dans GROUP BY ni agrégé)
SELECT DEPT_NUM, EMP_NOM, COUNT(*)  -- ERREUR !
FROM EMPLOYEE
GROUP BY DEPT_NUM;
```

### Exemples

```sql
-- Nombre d'employés par département
SELECT DEPT_NUM, COUNT(*) AS NB_EMP
FROM EMPLOYEE
GROUP BY DEPT_NUM;

-- Salaire moyen par poste
SELECT POSTE, AVG(SAL) AS SAL_MOYEN
FROM EMPLOYEE
GROUP BY POSTE;

-- Stats par département
SELECT DEPT_NUM,
       COUNT(*) AS NB_EMP,
       SUM(SAL) AS MASSE_SAL,
       AVG(SAL) AS SAL_MOYEN,
       MIN(SAL) AS SAL_MIN,
       MAX(SAL) AS SAL_MAX
FROM EMPLOYEE
GROUP BY DEPT_NUM;

-- Groupement multiple
SELECT DEPT_NUM, POSTE, COUNT(*), AVG(SAL)
FROM EMPLOYEE
GROUP BY DEPT_NUM, POSTE
ORDER BY DEPT_NUM, POSTE;
```

---

## IX-3 : HAVING

### Concept

**HAVING** filtre les groupes après le GROUP BY (comme WHERE filtre les lignes avant).

```
┌─────────────────────────────────────────────────────────────────┐
│                  WHERE vs HAVING                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  WHERE                          HAVING                          │
│  ─────                          ──────                          │
│  • Filtre les LIGNES            • Filtre les GROUPES            │
│  • AVANT le regroupement        • APRÈS le regroupement         │
│  • Sur colonnes individuelles   • Sur résultats d'agrégation   │
│                                                                  │
│  Ordre d'exécution :                                            │
│  FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemples

```sql
-- Départements avec plus de 3 employés
SELECT DEPT_NUM, COUNT(*) AS NB_EMP
FROM EMPLOYEE
GROUP BY DEPT_NUM
HAVING COUNT(*) > 3;

-- Départements avec salaire moyen > 2000
SELECT DEPT_NUM, AVG(SAL) AS SAL_MOYEN
FROM EMPLOYEE
GROUP BY DEPT_NUM
HAVING AVG(SAL) > 2000;

-- Combinaison WHERE et HAVING
SELECT DEPT_NUM, AVG(SAL) AS SAL_MOYEN
FROM EMPLOYEE
WHERE POSTE <> 'PDG'          -- Exclure le PDG des calculs
GROUP BY DEPT_NUM
HAVING AVG(SAL) > 1500        -- Garder départements avec moyenne > 1500
ORDER BY SAL_MOYEN DESC;
```

---

## IX-4 : Sous-requêtes

### Définition

Une **sous-requête** est une requête imbriquée dans une autre requête.

```sql
SELECT ...
FROM ...
WHERE colonne operateur (SELECT ... FROM ... WHERE ...);
                          └────────────────────────────┘
                                  Sous-requête
```

### Sous-requête retournant une valeur

```sql
-- Employés gagnant plus que la moyenne
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT AVG(SAL) FROM EMPLOYEE);

-- Employé ayant le salaire maximum
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL = (SELECT MAX(SAL) FROM EMPLOYEE);

-- Employés du même département que 'MARTIN'
SELECT EMP_NOM, DEPT_NUM
FROM EMPLOYEE
WHERE DEPT_NUM = (SELECT DEPT_NUM FROM EMPLOYEE WHERE EMP_NOM = 'MARTIN');
```

### Sous-requête avec IN

```sql
-- Employés dans un département de PARIS
SELECT EMP_NOM, DEPT_NUM
FROM EMPLOYEE
WHERE DEPT_NUM IN (SELECT DEPT_NUM FROM DEPT WHERE LOC = 'PARIS');

-- Employés qui ont des subordonnés
SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NUM IN (SELECT DISTINCT DIR FROM EMPLOYEE WHERE DIR IS NOT NULL);
```

### Opérateurs ANY et ALL

```sql
-- ANY : vrai si vrai pour AU MOINS UNE valeur
-- Salaire supérieur à au moins un vendeur
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > ANY (SELECT SAL FROM EMPLOYEE WHERE POSTE = 'VENDEUR');

-- ALL : vrai si vrai pour TOUTES les valeurs
-- Salaire supérieur à tous les vendeurs
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > ALL (SELECT SAL FROM EMPLOYEE WHERE POSTE = 'VENDEUR');
```

### EXISTS et NOT EXISTS

```sql
-- EXISTS : vrai si la sous-requête retourne au moins une ligne
-- Départements ayant au moins un employé
SELECT DEPT_NOM
FROM DEPT D
WHERE EXISTS (
    SELECT 1 FROM EMPLOYEE E WHERE E.DEPT_NUM = D.DEPT_NUM
);

-- NOT EXISTS : vrai si la sous-requête ne retourne aucune ligne
-- Départements sans employés
SELECT DEPT_NOM
FROM DEPT D
WHERE NOT EXISTS (
    SELECT 1 FROM EMPLOYEE E WHERE E.DEPT_NUM = D.DEPT_NUM
);

-- Employés sans subordonnés
SELECT EMP_NOM
FROM EMPLOYEE E1
WHERE NOT EXISTS (
    SELECT 1 FROM EMPLOYEE E2 WHERE E2.DIR = E1.EMP_NUM
);
```

### Sous-requête corrélée

Une sous-requête **corrélée** fait référence à la requête externe et est ré-exécutée pour chaque ligne.

```sql
-- Employés gagnant plus que la moyenne de leur département
SELECT EMP_NOM, SAL, DEPT_NUM
FROM EMPLOYEE E1
WHERE SAL > (
    SELECT AVG(SAL)
    FROM EMPLOYEE E2
    WHERE E2.DEPT_NUM = E1.DEPT_NUM  -- Corrélation
);

-- Employés avec le salaire max dans leur département
SELECT EMP_NOM, SAL, DEPT_NUM
FROM EMPLOYEE E1
WHERE SAL = (
    SELECT MAX(SAL)
    FROM EMPLOYEE E2
    WHERE E2.DEPT_NUM = E1.DEPT_NUM
);
```

---

## IX-5 : Opérateurs ensemblistes

### UNION

Combine les résultats de deux requêtes (supprime les doublons).

```sql
-- Tous les numéros d'employés et de managers
SELECT EMP_NUM FROM EMPLOYEE
UNION
SELECT DIR FROM EMPLOYEE WHERE DIR IS NOT NULL;

-- Avec doublons : UNION ALL
SELECT DEPT_NUM FROM EMPLOYEE
UNION ALL
SELECT DEPT_NUM FROM DEPT;
```

### INTERSECT

Retourne les lignes communes aux deux requêtes.

```sql
-- Départements qui ont des employés
SELECT DEPT_NUM FROM DEPT
INTERSECT
SELECT DEPT_NUM FROM EMPLOYEE;
```

### EXCEPT (ou MINUS)

Retourne les lignes de la première requête qui ne sont pas dans la seconde.

```sql
-- Départements sans employés
SELECT DEPT_NUM FROM DEPT
EXCEPT
SELECT DEPT_NUM FROM EMPLOYEE;
```

### Règles des opérateurs ensemblistes

1. Même nombre de colonnes
2. Types compatibles
3. ORDER BY à la fin uniquement

```sql
SELECT EMP_NUM, EMP_NOM FROM EMPLOYEE WHERE DEPT_NUM = 20
UNION
SELECT EMP_NUM, EMP_NOM FROM EMPLOYEE WHERE DEPT_NUM = 30
ORDER BY EMP_NOM;
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE IX - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  IX-1 FONCTIONS D'AGRÉGATION                                    │
│       COUNT(*), COUNT(col), SUM, AVG, MIN, MAX                  │
│       Ignorent les NULL (sauf COUNT(*))                         │
│                                                                  │
│  IX-2 GROUP BY                                                   │
│       Regroupe les lignes par valeurs identiques                │
│       Règle : colonnes non agrégées → dans GROUP BY             │
│                                                                  │
│  IX-3 HAVING                                                     │
│       Filtre les groupes (après GROUP BY)                       │
│       WHERE = lignes, HAVING = groupes                          │
│                                                                  │
│  IX-4 SOUS-REQUÊTES                                              │
│       • Valeur unique : =, <, >                                 │
│       • Liste : IN, NOT IN                                      │
│       • Comparaison : ANY, ALL                                  │
│       • Existence : EXISTS, NOT EXISTS                          │
│       • Corrélées : référencent la requête externe             │
│                                                                  │
│  IX-5 OPÉRATEURS ENSEMBLISTES                                   │
│       UNION     : réunion (sans doublons)                       │
│       UNION ALL : réunion (avec doublons)                       │
│       INTERSECT : intersection                                  │
│       EXCEPT    : différence                                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Exercice 1 : Agrégations

1. Nombre total d'employés et masse salariale
2. Salaire moyen par poste, trié par moyenne décroissante
3. Départements avec plus de 2 employés

### Exercice 2 : Sous-requêtes

1. Employés gagnant plus que 'MARTIN'
2. Employés du département 'RECHERCHE' (sans connaître le numéro)
3. Employés gagnant plus que la moyenne de leur département

### Exercice 3 : Ensemblistes

1. Liste des postes existants et départements (colonnes compatibles)
2. Départements qui ont des employés ET qui sont à 'PARIS'

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VIII - SQL SELECT](08-sql-select.md) | [Chapitre X - Embedded SQL](10-embedded-sql.md) |

---
*Formation DB2/SQL - M2i Formation*
