# QCM 05 - Langage SQL

**Chapitres V-IX** | 20 questions | Duree estimee : 15 minutes

---

## Section A : DDL (Data Definition Language)

### Question 1

Quelle commande SQL cree une nouvelle table ?

- A) MAKE TABLE
- B) NEW TABLE
- C) CREATE TABLE
- D) ADD TABLE

<details>
<summary>Reponse</summary>

**C) CREATE TABLE**

Syntaxe : CREATE TABLE nom_table (colonne1 type1, colonne2 type2, ...);

</details>

---

### Question 2

Quelle commande SQL supprime une table et toutes ses donnees ?

- A) DELETE TABLE
- B) REMOVE TABLE
- C) DROP TABLE
- D) ERASE TABLE

<details>
<summary>Reponse</summary>

**C) DROP TABLE**

DROP TABLE supprime definitivement la table et ses donnees. ATTENTION : irreversible sans sauvegarde.

</details>

---

### Question 3

Quelle commande modifie la structure d'une table existante ?

- A) MODIFY TABLE
- B) CHANGE TABLE
- C) ALTER TABLE
- D) UPDATE TABLE

<details>
<summary>Reponse</summary>

**C) ALTER TABLE**

ALTER TABLE permet d'ajouter/supprimer des colonnes, modifier des types, ajouter des contraintes.

</details>

---

## Section B : DML (Data Manipulation Language)

### Question 4

Quelle commande insere une nouvelle ligne dans une table ?

- A) ADD
- B) INSERT
- C) CREATE
- D) PUT

<details>
<summary>Reponse</summary>

**B) INSERT**

Syntaxe : INSERT INTO table (col1, col2) VALUES (val1, val2);

</details>

---

### Question 5

Quelle commande modifie des donnees existantes ?

- A) MODIFY
- B) CHANGE
- C) ALTER
- D) UPDATE

<details>
<summary>Reponse</summary>

**D) UPDATE**

Syntaxe : UPDATE table SET colonne = valeur WHERE condition;

</details>

---

### Question 6

Quelle commande supprime des lignes d'une table ?

- A) REMOVE
- B) DROP
- C) DELETE
- D) ERASE

<details>
<summary>Reponse</summary>

**C) DELETE**

Syntaxe : DELETE FROM table WHERE condition;
Note : Sans WHERE, toutes les lignes sont supprimees !

</details>

---

## Section C : SELECT et Clauses

### Question 7

Quelle clause filtre les lignes dans un SELECT ?

- A) FILTER
- B) WHERE
- C) HAVING
- D) IF

<details>
<summary>Reponse</summary>

**B) WHERE**

WHERE filtre les lignes AVANT le regroupement (contrairement a HAVING qui filtre les groupes).

</details>

---

### Question 8

Quelle clause trie les resultats ?

- A) SORT BY
- B) ORDER BY
- C) ARRANGE BY
- D) GROUP BY

<details>
<summary>Reponse</summary>

**B) ORDER BY**

ORDER BY col ASC (croissant) ou ORDER BY col DESC (decroissant).

</details>

---

### Question 9

Que fait la clause DISTINCT ?

- A) Trie les resultats
- B) Elimine les doublons
- C) Compte les lignes
- D) Filtre les NULL

<details>
<summary>Reponse</summary>

**B) Elimine les doublons**

SELECT DISTINCT col FROM table retourne les valeurs uniques de la colonne.

</details>

---

### Question 10

Quel operateur teste si une valeur est dans une liste ?

- A) CONTAINS
- B) INSIDE
- C) IN
- D) MEMBER

<details>
<summary>Reponse</summary>

**C) IN**

Exemple : WHERE DEPT_NUM IN (10, 20, 30)

</details>

---

### Question 11

Quel operateur teste une plage de valeurs ?

- A) RANGE
- B) FROM...TO
- C) BETWEEN...AND
- D) IN...OUT

<details>
<summary>Reponse</summary>

**C) BETWEEN...AND**

Exemple : WHERE SAL BETWEEN 1000 AND 2000 (bornes incluses).

</details>

---

### Question 12

Quel operateur effectue une recherche avec joker ?

- A) MATCH
- B) LIKE
- C) SEARCH
- D) FIND

<details>
<summary>Reponse</summary>

**B) LIKE**

Jokers : % (plusieurs caracteres), _ (un seul caractere).
Exemple : WHERE EMP_NOM LIKE 'A%'

</details>

---

### Question 13

Comment teste-t-on si une valeur est NULL ?

- A) = NULL
- B) == NULL
- C) IS NULL
- D) EQUALS NULL

<details>
<summary>Reponse</summary>

**C) IS NULL**

NULL n'est pas une valeur, c'est l'absence de valeur. On utilise IS NULL ou IS NOT NULL.

</details>

---

## Section D : Jointures

### Question 14

Quel type de jointure retourne uniquement les lignes correspondantes des deux tables ?

- A) LEFT JOIN
- B) RIGHT JOIN
- C) INNER JOIN
- D) FULL JOIN

<details>
<summary>Reponse</summary>

**C) INNER JOIN**

INNER JOIN (ou simplement JOIN) retourne uniquement les lignes qui ont une correspondance dans les deux tables.

</details>

---

### Question 15

Quel type de jointure retourne toutes les lignes de la table de gauche ?

- A) INNER JOIN
- B) LEFT OUTER JOIN
- C) RIGHT OUTER JOIN
- D) CROSS JOIN

<details>
<summary>Reponse</summary>

**B) LEFT OUTER JOIN**

LEFT JOIN retourne toutes les lignes de la table de gauche, avec NULL pour les colonnes de droite sans correspondance.

</details>

---

### Question 16

Qu'est-ce qu'une auto-jointure (self-join) ?

- A) Une jointure automatique
- B) Une jointure d'une table avec elle-meme
- C) Une jointure sans condition
- D) Une jointure avec sous-requete

<details>
<summary>Reponse</summary>

**B) Une jointure d'une table avec elle-meme**

Utilisee pour les relations hierarchiques. Exemple : trouver le nom du directeur de chaque employe.

</details>

---

## Section E : Fonctions et Groupement

### Question 17

Quelle fonction compte le nombre de lignes ?

- A) SUM
- B) TOTAL
- C) COUNT
- D) NUMBER

<details>
<summary>Reponse</summary>

**C) COUNT**

COUNT(*) compte toutes les lignes, COUNT(col) compte les valeurs non NULL.

</details>

---

### Question 18

Quelle clause regroupe les lignes pour les fonctions d'agregation ?

- A) ORDER BY
- B) GROUP BY
- C) SORT BY
- D) CLUSTER BY

<details>
<summary>Reponse</summary>

**B) GROUP BY**

GROUP BY regroupe les lignes ayant les memes valeurs pour appliquer les fonctions d'agregation.

</details>

---

### Question 19

Quelle clause filtre les groupes apres GROUP BY ?

- A) WHERE
- B) FILTER
- C) HAVING
- D) IF

<details>
<summary>Reponse</summary>

**C) HAVING**

WHERE filtre les lignes AVANT groupement, HAVING filtre les groupes APRES groupement.

</details>

---

### Question 20

Quel est l'ordre d'execution des clauses SQL ?

- A) SELECT → FROM → WHERE → GROUP BY → HAVING → ORDER BY
- B) FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY
- C) FROM → SELECT → WHERE → GROUP BY → HAVING → ORDER BY
- D) SELECT → FROM → GROUP BY → WHERE → HAVING → ORDER BY

<details>
<summary>Reponse</summary>

**B) FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY**

Ordre logique d'execution :
1. FROM (source des donnees)
2. WHERE (filtrage des lignes)
3. GROUP BY (regroupement)
4. HAVING (filtrage des groupes)
5. SELECT (projection des colonnes)
6. ORDER BY (tri final)

</details>

---

## Score

| Questions correctes | Appreciation |
|---------------------|--------------|
| 18-20 | Excellent |
| 15-17 | Tres bien |
| 12-14 | Bien |
| 8-11 | Moyen - Revoir les chapitres |
| 0-7 | Insuffisant - Reprendre le cours |

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [QCM 04 - Modele relationnel](qcm-04-modele-relationnel.md) | [README](README.md) |

---
*Formation DB2/SQL - M2i Formation*
