# Activite 5 - Operateurs Ensemblistes

## Objectifs

- Maitriser UNION et UNION ALL
- Utiliser INTERSECT pour les intersections
- Appliquer EXCEPT (MINUS) pour les differences
- Combiner des ensembles de resultats

---

## Question 1

Affichez le departement qui ne comprend **aucun employe**.

<details>
<summary>Correction</summary>

```sql
SELECT DEPT_NUM, DEPT_NOM
FROM DEPT
EXCEPT
SELECT D.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E, DEPT D
WHERE E.DEPT_NUM = D.DEPT_NUM;
```

**Explication** :
- Tous les departements MOINS ceux qui ont des employes
- EXCEPT retourne les lignes de la 1ere requete absentes de la 2eme

**Resultat** :
```
DEPT_NUM   DEPT_NOM
--------   -----------
40         EXPLOITATION
```

</details>

---

## Question 2

Retrouvez les postes qui ont ete attribues dans le **deuxieme semestre** (juillet-decembre) des annees **2021 et 2025**.

<details>
<summary>Correction</summary>

```sql
SELECT POSTE
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2021-07-01' AND '2021-12-31'
INTERSECT
SELECT POSTE
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2025-07-01' AND '2025-12-31';
```

**Explication** :
- INTERSECT retourne les postes communs aux deux periodes
- Postes attribues en S2 2021 ET en S2 2025

**Resultat** :
```
POSTE
---------
AGENT
ANALYSTE
```

</details>

---

## Question 3

Affichez la liste des postes dans les departements **10, 20 et 30**, dans cet ordre.
Affichez le poste et le numero du departement.

<details>
<summary>Correction</summary>

```sql
SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 10
UNION
SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 20
UNION
SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 30
ORDER BY DEPT_NUM;
```

**Explication** :
- UNION combine les resultats et elimine les doublons
- ORDER BY s'applique a l'ensemble du resultat final

**Resultat** : 9 lignes (postes par departement)
```
POSTE       DEPT_NUM
---------   --------
AGENT       10
DIRECTEUR   10
PDG         10
AGENT       20
ANALYSTE    20
DIRECTEUR   20
AGENT       30
DIRECTEUR   30
VENDEUR     30
```

</details>

---

## Question 4

Affichez le numero des departements dans lesquels on ne trouve **pas de poste ANALYSTE**.

<details>
<summary>Correction</summary>

```sql
SELECT DEPT_NUM
FROM DEPT
EXCEPT
SELECT DEPT_NUM
FROM EMPLOYEE
WHERE POSTE = 'ANALYSTE';
```

**Explication** :
- Tous les departements MOINS ceux qui ont des analystes
- Le dept 20 a des analystes, donc exclu

**Resultat** :
```
DEPT_NUM
--------
10
30
40
```

</details>

---

## Question 5

Affichez tous les postes des departements 10 et 20 qui **n'existent que dans l'un ou l'autre** (pas dans les deux).

<details>
<summary>Correction</summary>

```sql
(
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 10
UNION
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 20
)
EXCEPT
(
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 10
INTERSECT
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 20
);
```

**Explication** : Difference symetrique
- (Postes du 10 UNION Postes du 20) EXCEPT (Postes communs 10 ET 20)
- Postes AGENT et DIRECTEUR sont communs, donc exclus
- Reste : ANALYSTE (uniquement dept 20) et PDG (uniquement dept 10)

**Resultat** :
```
POSTE
---------
ANALYSTE
PDG
```

</details>

---

## Points cles a retenir

```
+------------------------------------------------------------------+
|                    ACTIVITE 5 - RESUME                            |
+------------------------------------------------------------------+
|                                                                   |
|  UNION                                                            |
|    Combine deux ensembles SANS doublons                          |
|    SELECT ... UNION SELECT ...                                   |
|                                                                   |
|  UNION ALL                                                        |
|    Combine deux ensembles AVEC doublons                          |
|    Plus rapide car pas d'elimination                             |
|                                                                   |
|  INTERSECT                                                        |
|    Retourne les lignes COMMUNES aux deux ensembles               |
|    SELECT ... INTERSECT SELECT ...                               |
|                                                                   |
|  EXCEPT (ou MINUS)                                                |
|    Retourne les lignes du 1er ensemble ABSENTES du 2eme          |
|    SELECT ... EXCEPT SELECT ...                                  |
|                                                                   |
|  REGLES                                                           |
|    - Meme nombre de colonnes                                     |
|    - Types compatibles                                           |
|    - ORDER BY uniquement a la fin                                |
|                                                                   |
|  DIAGRAMME                                                        |
|                                                                   |
|    A = {1,2,3}    B = {2,3,4}                                     |
|                                                                   |
|    UNION:     {1,2,3,4}     (tous)                                |
|    INTERSECT: {2,3}         (communs)                            |
|    A EXCEPT B: {1}          (A moins B)                          |
|    B EXCEPT A: {4}          (B moins A)                          |
|                                                                   |
+------------------------------------------------------------------+
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Activite 4 - Jointures](activite-04-jointures.md) | [Activite 6 - Fonctions de groupe](activite-06-fonctions-groupe.md) |

---
*Formation DB2/SQL - M2i Formation*
