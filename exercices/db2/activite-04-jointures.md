# Activite 4 - Afficher des donnees issues de plusieurs tables

## Objectifs

- Maitriser les jointures internes (INNER JOIN)
- Utiliser les jointures externes (LEFT/RIGHT OUTER JOIN)
- Realiser des auto-jointures (self-join)
- Combiner plusieurs tables

---

## Question 1

Ecrivez une requete pour afficher le nom, le numero de departement et le **nom du departement** de tous les employes.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, E.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM;
```

**Resultat** : 14 lignes avec nom employe + nom departement

</details>

---

## Question 2

Creez une liste unique de tous les **postes du departement 30**.

<details>
<summary>Correction</summary>

```sql
SELECT DISTINCT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 30;
```

**Resultat** : 3 postes
```
POSTE
---------
AGENT
DIRECTEUR
VENDEUR
```

</details>

---

## Question 3

Affichez le nom, le nom du departement et la localisation de tous les employes qui **touchent une commission**.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, D.DEPT_NOM, D.LOC
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE E.COMM IS NOT NULL;
```

**Resultat** : 4 lignes (vendeurs avec commission)
```
EMP_NOM    DEPT_NOM   LOC
---------  ---------  ----
PAUL       VENTES     LYON
JEAN       VENTES     LYON
GEORGES    VENTES     LYON
JULES      VENTES     LYON
```

</details>

---

## Question 4

Affichez le nom et le nom du departement pour tous les employes dont le nom **contient la lettre 'A'**.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE E.EMP_NOM LIKE '%A%';
```

**Resultat** : 10 lignes

</details>

---

## Question 5

Affichez le nom, le poste, le numero de departement et le nom du departement de tous les employes bases a **'STRASBOURG'**.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, E.POSTE, E.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE D.LOC = 'STRASBOURG';
```

**Resultat** : 5 lignes (departement RECHERCHE)

</details>

---

## Question 6

Affichez le nom et le matricule des employes et de leur directeur.
Nommez les colonnes : **Employes, N Emp., Directeur, N Dir.**

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM    AS "EMPLOYES",
       E.EMP_NUM    AS "N EMP.",
       M.EMP_NOM    AS "DIRECTEUR",
       M.EMP_NUM    AS "N DIR."
FROM EMPLOYEE E
JOIN EMPLOYEE M
ON E.DIR = M.EMP_NUM;
```

**Explication** : Auto-jointure de EMPLOYEE avec elle-meme
- E = employe
- M = manager (directeur)

**Resultat** : 13 lignes (HENRI n'apparait pas car il n'a pas de directeur)

</details>

---

## Question 7

Modifiez la requete precedente pour afficher **tous les employes, y compris ceux n'ayant pas de directeur** (ex. HENRI).

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM    AS "EMPLOYES",
       E.EMP_NUM    AS "N EMP.",
       M.EMP_NOM    AS "DIRECTEUR",
       M.EMP_NUM    AS "N DIR."
FROM EMPLOYEE E
LEFT OUTER JOIN EMPLOYEE M
ON E.DIR = M.EMP_NUM;
```

**Resultat** : 14 lignes (HENRI apparait avec NULL pour DIRECTEUR)

</details>

---

## Question 8

Creez une requete pour afficher, pour chaque employe, le numero de departement et le **nombre de collegues travaillant dans le meme departement**. Excluez l'employe lui-meme du decompte.

<details>
<summary>Correction</summary>

```sql
SELECT E.DEPT_NUM      AS "DEPARTEMENT",
       E.EMP_NOM       AS "EMPLOYE",
       COUNT(*)        AS NB_COLLEGUES
FROM EMPLOYEE E
JOIN EMPLOYEE C
ON E.DEPT_NUM = C.DEPT_NUM
WHERE E.EMP_NUM <> C.EMP_NUM
GROUP BY E.DEPT_NUM, E.EMP_NOM;
```

**Explication** :
- Jointure de EMPLOYEE avec elle-meme sur le meme departement
- Exclusion de l'employe lui-meme (`E.EMP_NUM <> C.EMP_NUM`)
- Regroupement par departement et employe

**Resultat** : 14 lignes avec nombre de collegues par employe

</details>

---

## Question 9

Creez une requete pour afficher le nom, le poste, le departement, le salaire et le **grade** de tous les employes.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, E.POSTE, D.DEPT_NOM,
       E.SAL,
       S.GRADE
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
JOIN SAL_GRILLE S
ON E.SAL BETWEEN S.MIN_SAL AND S.MAX_SAL;
```

**Explication** : Jointure sur 3 tables
- EMPLOYEE + DEPT (sur DEPT_NUM)
- EMPLOYEE + SAL_GRILLE (salaire dans la fourchette min/max)

**Resultat** : 14 lignes avec grade calcule selon le salaire

</details>

---

## Question 10

Creez une requete pour afficher le nom et la date d'embauche de tous les employes arrives **avant l'employe 'JEAN'**. Triez par date d'embauche.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, E.DATE_EMB
FROM EMPLOYEE E
JOIN EMPLOYEE J
ON J.DATE_EMB > E.DATE_EMB
WHERE J.EMP_NOM = 'JEAN'
ORDER BY E.DATE_EMB;
```

**Resultat** :
```
EMP_NOM    DATE_EMB
---------  ----------
ARTHUR     2020-12-17
PAUL       2021-02-20
```

</details>

---

## Question 11

Affichez les noms et dates d'embauche des employes et de leur directeur, pour tous les employes ayant ete embauches **avant leur directeur**.
Nommez les colonnes : **Employe, Date Embauche Emp., Directeur, Date Embauche Dir.**

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM    AS "EMPLOYE",
       E.DATE_EMB   AS "DATE EMBAUCHE EMP.",
       D.EMP_NOM    AS "DIRECTEUR",
       D.DATE_EMB   AS "DATE EMBAUCHE DIR."
FROM EMPLOYEE E
JOIN EMPLOYEE D
ON E.DIR = D.EMP_NUM
WHERE E.DATE_EMB < D.DATE_EMB;
```

**Resultat** : 6 lignes (employes embauches avant leur chef)

</details>

---

## Points cles a retenir

```
+------------------------------------------------------------------+
|                    ACTIVITE 4 - RESUME                            |
+------------------------------------------------------------------+
|                                                                   |
|  INNER JOIN (jointure interne)                                   |
|    SELECT ... FROM T1 JOIN T2 ON T1.col = T2.col                 |
|    Retourne uniquement les correspondances                       |
|                                                                   |
|  LEFT OUTER JOIN (jointure externe gauche)                       |
|    SELECT ... FROM T1 LEFT JOIN T2 ON ...                        |
|    Toutes les lignes de T1 + correspondances de T2               |
|    (NULL si pas de correspondance)                               |
|                                                                   |
|  AUTO-JOINTURE (self-join)                                       |
|    SELECT ... FROM T1 E JOIN T1 M ON E.col = M.col               |
|    Jointure d'une table avec elle-meme                           |
|    Utiliser des alias differents                                 |
|                                                                   |
|  JOINTURE MULTIPLE                                                |
|    SELECT ... FROM T1                                            |
|    JOIN T2 ON ...                                                |
|    JOIN T3 ON ...                                                |
|                                                                   |
+------------------------------------------------------------------+
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Activite 2 - Selection et tri](activite-02-selection-tri.md) | [Activite 5 - Operateurs ensemblistes](activite-05-operateurs-ensemblistes.md) |

---
*Formation DB2/SQL - M2i Formation*
