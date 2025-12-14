# Activite 7 - Sous-Interrogations (Sous-requetes)

## Objectifs

- Maitriser les sous-requetes simples (retournant une valeur)
- Utiliser les sous-requetes multi-colonnes
- Appliquer les sous-requetes synchronisees (correlees)
- Combiner sous-requetes avec IN, ANY, ALL, EXISTS

---

## Question 1

Creez une requete pour afficher le nom et la date d'embauche de tous les employes travaillant dans le **meme departement que 'JEAN'**, a l'exclusion de 'JEAN'.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE DEPT_NUM = (SELECT DEPT_NUM FROM EMPLOYEE WHERE EMP_NOM = 'JEAN')
AND EMP_NOM <> 'JEAN';
```

**Explication** : La sous-requete retourne le numero de departement de JEAN (30), puis on selectionne tous les employes de ce departement sauf JEAN.

</details>

---

## Question 2

Creez une requete pour afficher le matricule et le nom de tous les employes qui gagnent **plus que le salaire moyen**. Triez les resultats par ordre decroissant des salaires.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NUM, EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT AVG(SAL) FROM EMPLOYEE)
ORDER BY SAL DESC;
```

**Explication** : La sous-requete calcule le salaire moyen (2073.21), puis on filtre les employes gagnant plus.

</details>

---

## Question 3

Ecrivez une requete pour afficher le matricule et le nom de tous les employes qui travaillent dans le **meme departement que tout employe dont le nom contient un 'T'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NUM, EMP_NOM
FROM EMPLOYEE
WHERE DEPT_NUM IN (
    SELECT DEPT_NUM
    FROM EMPLOYEE
    WHERE EMP_NOM LIKE '%T%'
);
```

**Explication** :
- La sous-requete trouve les departements des employes avec 'T' dans le nom (ARTHUR, GASTON, HECTOR, ANTOINE, ALBERT, BASILE)
- On utilise IN car la sous-requete peut retourner plusieurs valeurs

</details>

---

## Question 4

Modifiez la requete (3) afin d'afficher le matricule, le nom et le salaire de tous les employes qui **gagnent plus que le salaire moyen** ET qui **travaillent dans un departement avec tout employe dont le nom contient un 'T'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NUM, EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT AVG(SAL) FROM EMPLOYEE)
AND DEPT_NUM IN (
    SELECT DEPT_NUM
    FROM EMPLOYEE
    WHERE EMP_NOM LIKE '%T%'
);
```

**Explication** : Combinaison de deux conditions avec sous-requetes.

</details>

---

## Question 5

Affichez le nom, le numero de departement et le poste de tous les employes dont le **departement est situe a 'STRASBOURG'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, DEPT_NUM, POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = (
    SELECT DEPT_NUM
    FROM DEPT
    WHERE LOC = 'STRASBOURG'
);
```

**Alternative avec jointure** :
```sql
SELECT E.EMP_NOM, E.DEPT_NUM, E.POSTE
FROM EMPLOYEE E
JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM
WHERE D.LOC = 'STRASBOURG';
```

</details>

---

## Question 6

Affichez le nom et le salaire de tous les employes dont le **directeur est 'ALBERT'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE DIR = (SELECT EMP_NUM FROM EMPLOYEE WHERE EMP_NOM = 'ALBERT');
```

**Resultat** : Employes dont le directeur (7698) est ALBERT.

</details>

---

## Question 7

Affichez le numero de departement, le nom et le poste de tous les employes travaillant dans le **departement des ventes 'VENTES'**.

<details>
<summary>Correction</summary>

```sql
SELECT DEPT_NUM, EMP_NOM, POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = (
    SELECT DEPT_NUM
    FROM DEPT
    WHERE DEPT_NOM = 'VENTES'
);
```

</details>

---

## Question 8

Creez une requete pour afficher les employes qui percoivent un salaire **superieur a TOUT employe dont le poste est 'AGENT'**. Triez le resultat par ordre decroissant des salaires.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > ALL (SELECT SAL FROM EMPLOYEE WHERE POSTE = 'AGENT')
ORDER BY SAL DESC;
```

**Explication** :
- `> ALL` signifie "superieur a toutes les valeurs"
- Les salaires des AGENTS sont : 800, 1100, 950, 1300
- Donc SAL > 1300 (le max des agents)

**Alternative avec MAX** :
```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT MAX(SAL) FROM EMPLOYEE WHERE POSTE = 'AGENT')
ORDER BY SAL DESC;
```

</details>

---

## Question 9

Ecrivez une requete pour afficher le nom, le numero de departement et le salaire de tout employe dont le **numero de departement et le salaire correspondent** au numero de departement et au salaire d'un des employes **touchant une commission**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, DEPT_NUM, SAL
FROM EMPLOYEE
WHERE (DEPT_NUM, SAL) IN (
    SELECT DEPT_NUM, SAL
    FROM EMPLOYEE
    WHERE COMM IS NOT NULL
);
```

**Explication** : Sous-requete multi-colonnes - on compare un tuple (DEPT_NUM, SAL) avec une liste de tuples.

</details>

---

## Question 10

Affichez le nom de l'employe, le nom du departement et le salaire de tout employe dont le **salaire et la commission sont tous les deux equivalents** au salaire et a la commission de n'importe quel employe base a **'STRASBOURG'**.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NOM, D.DEPT_NOM, E.SAL
FROM EMPLOYEE E
JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM
WHERE (E.SAL, COALESCE(E.COMM, 0)) IN (
    SELECT E2.SAL, COALESCE(E2.COMM, 0)
    FROM EMPLOYEE E2
    JOIN DEPT D2 ON E2.DEPT_NUM = D2.DEPT_NUM
    WHERE D2.LOC = 'STRASBOURG'
);
```

**Note** : On utilise COALESCE pour gerer les NULL dans les comparaisons.

</details>

---

## Question 11

Ecrivez une requete pour afficher les **trois meilleurs salaires** dans la table EMPLOYEE. Affichez les noms des employes et leur salaire.

<details>
<summary>Correction</summary>

**Methode avec sous-requete correlee** :
```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE E1
WHERE 3 > (
    SELECT COUNT(DISTINCT SAL)
    FROM EMPLOYEE E2
    WHERE E2.SAL > E1.SAL
)
ORDER BY SAL DESC;
```

**Explication** : On compte combien de salaires distincts sont superieurs au salaire courant. Si ce nombre est < 3, l'employe fait partie des 3 meilleurs.

**Methode DB2 avec FETCH FIRST** :
```sql
SELECT EMP_NOM, SAL
FROM EMPLOYEE
ORDER BY SAL DESC
FETCH FIRST 3 ROWS ONLY;
```

</details>

---

## Question 12

Recherchez tous les employes qui **ne sont pas des responsables** (n'ont pas de subordonnes).

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NUM NOT IN (
    SELECT DISTINCT DIR
    FROM EMPLOYEE
    WHERE DIR IS NOT NULL
);
```

**Alternative avec NOT EXISTS** :
```sql
SELECT EMP_NOM
FROM EMPLOYEE E1
WHERE NOT EXISTS (
    SELECT 1
    FROM EMPLOYEE E2
    WHERE E2.DIR = E1.EMP_NUM
);
```

</details>

---

## Question 13

Ecrivez une requete pour rechercher tous les employes dont le salaire est **superieur au salaire moyen de leur departement**. Affichez le numero de chaque employe, son salaire, son numero de departement et le salaire moyen du departement. Triez le resultat en fonction du salaire moyen.

<details>
<summary>Correction</summary>

```sql
SELECT E.EMP_NUM, E.SAL, E.DEPT_NUM,
       (SELECT AVG(SAL) FROM EMPLOYEE WHERE DEPT_NUM = E.DEPT_NUM) AS SAL_MOY_DEPT
FROM EMPLOYEE E
WHERE E.SAL > (
    SELECT AVG(SAL)
    FROM EMPLOYEE
    WHERE DEPT_NUM = E.DEPT_NUM
)
ORDER BY SAL_MOY_DEPT;
```

**Explication** : Sous-requete correlee - la sous-requete fait reference a la ligne courante de la requete principale (E.DEPT_NUM).

</details>

---

## Question 14

Ecrivez une requete pour afficher les employes dont le salaire est **inferieur a la moitie du salaire moyen** de leur departement.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM, SAL, DEPT_NUM
FROM EMPLOYEE E
WHERE SAL < (
    SELECT AVG(SAL) / 2
    FROM EMPLOYEE
    WHERE DEPT_NUM = E.DEPT_NUM
);
```

</details>

---

## Question 15

Ecrivez une requete pour afficher les employes ayant **un ou plusieurs collegues de leur departement** dont les dates d'embauche sont **posterieures** aux leurs ET dont les salaires sont **plus eleves** que les leurs.

<details>
<summary>Correction</summary>

```sql
SELECT E1.EMP_NOM, E1.DATE_EMB, E1.SAL, E1.DEPT_NUM
FROM EMPLOYEE E1
WHERE EXISTS (
    SELECT 1
    FROM EMPLOYEE E2
    WHERE E2.DEPT_NUM = E1.DEPT_NUM
    AND E2.EMP_NUM <> E1.EMP_NUM
    AND E2.DATE_EMB > E1.DATE_EMB
    AND E2.SAL > E1.SAL
);
```

**Explication** :
- EXISTS retourne TRUE si la sous-requete retourne au moins une ligne
- On cherche des collegues (meme dept, different employe) embauches apres avec un meilleur salaire

</details>

---

## Points cles a retenir

```
+------------------------------------------------------------------+
|                    ACTIVITE 7 - RESUME                            |
+------------------------------------------------------------------+
|                                                                   |
|  SOUS-REQUETE SIMPLE (retourne 1 valeur)                         |
|    WHERE col = (SELECT ... )                                     |
|    Utiliser =, <, >, <=, >=                                      |
|                                                                   |
|  SOUS-REQUETE MULTI-VALEURS                                       |
|    WHERE col IN (SELECT ... )                                    |
|    WHERE col NOT IN (SELECT ... )                                |
|                                                                   |
|  SOUS-REQUETE MULTI-COLONNES                                      |
|    WHERE (col1, col2) IN (SELECT col1, col2 FROM ... )           |
|                                                                   |
|  OPERATEURS ANY / ALL                                             |
|    > ANY : superieur a au moins une valeur                       |
|    > ALL : superieur a toutes les valeurs                        |
|    = ANY : equivalent a IN                                       |
|                                                                   |
|  EXISTS / NOT EXISTS                                              |
|    WHERE EXISTS (SELECT 1 FROM ... WHERE ...)                    |
|    Retourne TRUE si la sous-requete a des resultats              |
|                                                                   |
|  SOUS-REQUETE CORRELEE (synchronisee)                            |
|    Reference une colonne de la requete externe                   |
|    Re-executee pour chaque ligne de la requete principale        |
|    Ex: WHERE E.SAL > (SELECT AVG(SAL) WHERE DEPT = E.DEPT)       |
|                                                                   |
+------------------------------------------------------------------+
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Activite 6 - Fonctions de groupe](activite-06-fonctions-groupe.md) | [README](README.md) |

---
*Formation DB2/SQL - M2i Formation*
