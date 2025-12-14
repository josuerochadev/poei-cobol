# Activité 2 - Sélection et tri des lignes

## Objectifs

- Filtrer les données avec WHERE
- Utiliser les opérateurs de comparaison
- Maîtriser BETWEEN, IN, LIKE, IS NULL
- Trier les résultats avec ORDER BY

---

## Question 1

Créez une requête destinée à afficher le nom et le salaire des employés gagnant **plus de 2850**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > 2850;
```

**Résultat** : 4 lignes
```
EMP_NOM    SAL
---------  -------
CHARLES    2975.00
ARSENE     3000.00
HENRI      5000.00
BASILE     3000.00
```

</details>

---

## Question 2

Modifiez la requête (1) de manière à afficher le nom et le salaire de tous les employés dont le salaire **n'est pas compris entre 1500 et 2850**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL NOT BETWEEN 1500 AND 2850;
```

**Résultat** : 10 lignes (employés avec SAL < 1500 OU SAL > 2850)

</details>

---

## Question 3

Affichez le nom, le poste et la date d'entrée (DATE_EMB) des employés embauchés **entre le 20 février 2021 et le 1er mai 2021**. Classez le résultat par **date d'embauche croissante**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, POSTE, DATE_EMB
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2021-02-20' AND '2021-05-01'
ORDER BY DATE_EMB;
```

**Résultat** : 4 lignes
```
EMP_NOM    POSTE       DATE_EMB
---------  ---------   ----------
PAUL       VENDEUR     2021-02-20
JEAN       VENDEUR     2021-02-22
CHARLES    DIRECTEUR   2021-04-02
ALBERT     DIRECTEUR   2021-05-01
```

</details>

---

## Question 4

Affichez le nom et le numéro de département de tous les employés des départements **10 et 30** classés par **ordre alphabétique des noms**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, DEPT_NUM
FROM EMPLOYEE
WHERE DEPT_NUM IN (10, 30)
ORDER BY EMP_NOM;
```

**Résultat** : 9 lignes triées alphabétiquement

</details>

---

## Question 5

Modifiez la requête pour afficher la liste des noms et salaires des employés gagnant **plus de 1500** et travaillant dans le département **10 ou 30**. Nommez les colonnes **Employés** et **Salaire Mensuel**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM "EMPLOYÉS", SAL "SALAIRE MENSUEL"
FROM EMPLOYEE
WHERE SAL > 1500
AND DEPT_NUM IN (10, 30);
```

**Résultat** : 4 lignes
```
EMPLOYÉS   SALAIRE MENSUEL
---------  ---------------
PAUL       1600.00
ALBERT     2850.00
GASTON     2450.00
HENRI      5000.00
```

</details>

---

## Question 6

Affichez le nom et la date d'embauche de chaque employé entré **en 2022**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2022-01-01' AND '2022-12-31';
```

**Alternative avec YEAR()** :
```sql
SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE YEAR(DATE_EMB) = 2022;
```

**Résultat** : 1 ligne (HECTOR)

</details>

---

## Question 7

Affichez le nom et le poste de tous les employés **n'ayant pas de manager** (DIR IS NULL).

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, POSTE
FROM EMPLOYEE
WHERE DIR IS NULL;
```

**Résultat** :
```
EMP_NOM    POSTE
---------  -----
HENRI      PDG
```

**Note** : Ne jamais utiliser `= NULL` ou `<> NULL`, toujours `IS NULL` ou `IS NOT NULL`.

</details>

---

## Question 8

Affichez le nom, le salaire et la commission de tous les employés qui **perçoivent des commissions**. Triez les données dans l'ordre **décroissant des salaires et des commissions**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL, COMM
FROM EMPLOYEE
WHERE COMM IS NOT NULL
ORDER BY SAL DESC, COMM DESC;
```

**Résultat** : 4 lignes (vendeurs avec commission)

</details>

---

## Question 9

Affichez le nom de tous les employés dont la **troisième lettre** du nom est un **'A'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NOM LIKE '__A%';
```

**Explication** :
- `_` : un caractère quelconque
- `__` : deux caractères quelconques
- `A` : le caractère A en 3ème position
- `%` : zéro ou plusieurs caractères

**Résultat** :
```
EMP_NOM
-------
JEAN
CHARLES
```

</details>

---

## Question 10

Affichez le nom de tous les employés dont le nom contient **deux 'G'** ET travaillant dans le département **30** OU dont le manager est **7782**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NOM LIKE '%G%G%'
AND (DEPT_NUM = 30 OR DIR = 7782);
```

**Résultat** :
```
EMP_NOM
-------
GEORGES
```

</details>

---

## Question 11

Affichez le nom, le poste et le salaire de tous les **'AGENT' ou 'ANALYSTE'** dont le salaire est **différent de 1000, 3000 ou 5000**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, POSTE, SAL
FROM EMPLOYEE
WHERE POSTE IN ('AGENT', 'ANALYSTE')
AND SAL NOT IN (1000, 3000, 5000);
```

**Résultat** : 4 lignes (ARTHUR, ANTOINE, FERNAND, HECTOR)

</details>

---

## Question 12

Afficher le nom, le salaire et la commission de tous les employés dont le montant de commission est de **plus de 10% supérieur au salaire**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL, COMM
FROM EMPLOYEE
WHERE COMM > SAL * 1.1;
```

**Résultat** :
```
EMP_NOM    SAL       COMM
---------  -------   -------
GEORGES    1250.00   1400.00
```

</details>

---

## Points clés à retenir

```
┌─────────────────────────────────────────────────────────────────┐
│                   ACTIVITÉ 2 - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  OPÉRATEURS DE COMPARAISON                                      │
│    =, <>, <, >, <=, >=                                          │
│                                                                  │
│  OPÉRATEURS LOGIQUES                                            │
│    AND, OR, NOT                                                  │
│    Attention aux parenthèses !                                  │
│                                                                  │
│  OPÉRATEURS SPÉCIAUX                                            │
│    BETWEEN min AND max  (inclus)                                │
│    IN (val1, val2, ...)                                          │
│    LIKE 'motif'  (_ = 1 car, % = 0+ car)                        │
│    IS NULL / IS NOT NULL                                        │
│                                                                  │
│  TRI                                                             │
│    ORDER BY col [ASC|DESC]                                      │
│    ORDER BY col1, col2  (tri multiple)                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Activité 1 - SELECT](activite-01-select-elementaire.md) | [Activité 4 - Jointures](activite-04-jointures.md) |

---
*Formation DB2/SQL - M2i Formation*
