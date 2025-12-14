# Activite 6 - Regrouper les donnees avec les Fonctions de Groupe

## Objectifs

- Maitriser les fonctions d'agregation (COUNT, SUM, AVG, MIN, MAX)
- Utiliser GROUP BY pour regrouper les donnees
- Filtrer les groupes avec HAVING
- Combiner agregations et jointures

---

## Question 1

Affichez le salaire maximum, le salaire minimum, la somme des salaires et le salaire moyen de tous les employes.
Nommez respectivement les colonnes **Maximum, Minimum, Somme et Moyenne**.
Arrondissez les resultats a 0 decimale.

<details>
<summary>Correction</summary>

**Version avec ROUND** :
```sql
SELECT ROUND(MAX(SAL), 0) AS "MAXIMUM",
       ROUND(MIN(SAL), 0) AS "MINIMUM",
       ROUND(SUM(SAL), 0) AS "SOMME",
       ROUND(AVG(SAL), 0) AS "MOYENNE"
FROM EMPLOYEE;
```

**Version avec CAST** (pour format propre) :
```sql
SELECT ROUND(MAX(SAL), 0) AS "MAXIMUM",
       ROUND(MIN(SAL), 0) AS "MINIMUM",
       ROUND(SUM(SAL), 0) AS "SOMME",
       CAST(ROUND(AVG(SAL), 0) AS DECIMAL(7,2)) AS "MOYENNE"
FROM EMPLOYEE;
```

**Resultat** :
```
MAXIMUM    MINIMUM    SOMME       MOYENNE
-------    -------    --------    -------
5000.00    800.00     29025.00    2073.00
```

</details>

---

## Question 2

Modifiez la requete (1) pour afficher le salaire maximum, le salaire minimum, la somme des salaires et le salaire moyen **pour chaque type de poste**.

<details>
<summary>Correction</summary>

```sql
SELECT POSTE,
       ROUND(MAX(SAL), 0) AS "MAXIMUM",
       ROUND(MIN(SAL), 0) AS "MINIMUM",
       ROUND(SUM(SAL), 0) AS "SOMME",
       ROUND(AVG(SAL), 0) AS "MOYENNE"
FROM EMPLOYEE
GROUP BY POSTE;
```

**Resultat** : 5 lignes (une par poste)
```
POSTE       MAXIMUM    MINIMUM    SOMME      MOYENNE
---------   -------    -------    -------    -------
AGENT       1300.00    800.00     4150.00    1038.00
ANALYSTE    3000.00    3000.00    6000.00    3000.00
DIRECTEUR   2975.00    2450.00    8275.00    2758.00
PDG         5000.00    5000.00    5000.00    5000.00
VENDEUR     1600.00    1250.00    5600.00    1400.00
```

</details>

---

## Question 3

Ecrivez une requete pour afficher le **nombre de personnes** qui occupent le meme poste.

<details>
<summary>Correction</summary>

```sql
SELECT POSTE,
       COUNT(*) AS "NOMBRE"
FROM EMPLOYEE
GROUP BY POSTE;
```

**Resultat** :
```
POSTE       NOMBRE
---------   ------
AGENT       4
ANALYSTE    2
DIRECTEUR   3
PDG         1
VENDEUR     4
```

</details>

---

## Question 4

Determinez le **nombre de personnes ayant des subordonnes**, sans en donner la liste. Nommez la colonne "Nombre de Chefs".

<details>
<summary>Correction</summary>

```sql
SELECT COUNT(DISTINCT DIR) AS "NOMBRE DE CHEFS"
FROM EMPLOYEE;
```

**Note** : SQLSTATE 01003 peut apparaitre car les valeurs NULL sont ignorees dans COUNT(colonne). C'est normal.

**Alternative avec WHERE** :
```sql
SELECT COUNT(DISTINCT DIR) AS "NOMBRE DE CHEFS"
FROM EMPLOYEE
WHERE DIR IS NOT NULL;
```

**Resultat** :
```
NOMBRE DE CHEFS
---------------
6
```

</details>

---

## Question 5

Ecrivez une requete pour afficher la **difference** existante entre le salaire maximum et le salaire minimum. Nommez la colonne **DIFFERENCE**.

<details>
<summary>Correction</summary>

```sql
SELECT MAX(SAL) - MIN(SAL) AS DIFFERENCE
FROM EMPLOYEE;
```

**Resultat** :
```
DIFFERENCE
----------
4200.00
```

</details>

---

## Question 6

Affichez le matricule des differents directeurs et le niveau de **salaire le plus bas** de leurs employes.
- Excluez toute ligne ou le directeur n'est pas identifie
- Excluez tout groupe dans lequel le salaire minimum est inferieur a 1000
- Triez les resultats par ordre decroissant des salaires

<details>
<summary>Correction</summary>

```sql
SELECT DIR,
       MIN(SAL) AS MIN_SALAIRE
FROM EMPLOYEE
WHERE DIR IS NOT NULL
GROUP BY DIR
HAVING MIN(SAL) > 1000
ORDER BY MIN_SALAIRE DESC;
```

**Explication** :
- WHERE : filtre les lignes AVANT le regroupement
- HAVING : filtre les groupes APRES le regroupement

**Resultat** :
```
DIR     MIN_SALAIRE
----    -----------
7566    3000.00
7839    2450.00
7782    1300.00
7788    1100.00
```

</details>

---

## Question 7

Ecrivez une requete pour afficher le nom du departement, la localisation, le nombre d'employes et le salaire moyen pour tous les employes de ce departement.
Nommez les colonnes **Departement, Localisation, Nombre d'Employes et Salaire**.

<details>
<summary>Correction</summary>

```sql
SELECT D.DEPT_NOM       AS "DEPARTEMENT",
       D.LOC            AS "LOCALISATION",
       COUNT(*)         AS "NOMBRE D'EMPLOYES",
       ROUND(AVG(E.SAL), 0) AS "SALAIRE"
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
GROUP BY D.DEPT_NOM, D.LOC;
```

**Resultat** :
```
DEPARTEMENT   LOCALISATION   NOMBRE D'EMPLOYES   SALAIRE
-----------   ------------   -----------------   -------
COMPTABILITE  MARSEILLE      3                   2917.00
RECHERCHE     STRASBOURG     5                   2175.00
VENTES        LYON           6                   1567.00
```

**Note** : Le dept EXPLOITATION (40) n'apparait pas car il n'a pas d'employes.

</details>

---

## Points cles a retenir

```
+------------------------------------------------------------------+
|                    ACTIVITE 6 - RESUME                            |
+------------------------------------------------------------------+
|                                                                   |
|  FONCTIONS D'AGREGATION                                          |
|    COUNT(*)     : nombre de lignes                               |
|    COUNT(col)   : nombre de valeurs non NULL                     |
|    SUM(col)     : somme                                          |
|    AVG(col)     : moyenne                                        |
|    MIN(col)     : minimum                                        |
|    MAX(col)     : maximum                                        |
|                                                                   |
|  GROUP BY                                                         |
|    Regroupe les lignes par valeurs identiques                    |
|    Toutes les colonnes non agregees doivent etre dans GROUP BY   |
|                                                                   |
|  HAVING                                                           |
|    Filtre les GROUPES (apres GROUP BY)                           |
|    WHERE filtre les LIGNES (avant GROUP BY)                      |
|                                                                   |
|  ORDRE D'EXECUTION                                                |
|    FROM -> WHERE -> GROUP BY -> HAVING -> SELECT -> ORDER BY     |
|                                                                   |
|  DISTINCT DANS COUNT                                              |
|    COUNT(DISTINCT col) : compte les valeurs uniques              |
|                                                                   |
|  NULL ET AGREGATIONS                                              |
|    Les fonctions ignorent les NULL (sauf COUNT(*))               |
|    SQLSTATE 01003 = warning "NULL eliminated"                    |
|                                                                   |
+------------------------------------------------------------------+
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Activite 5 - Operateurs ensemblistes](activite-05-operateurs-ensemblistes.md) | [README](README.md) |

---
*Formation DB2/SQL - M2i Formation*
