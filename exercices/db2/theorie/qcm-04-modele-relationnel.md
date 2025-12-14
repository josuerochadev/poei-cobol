# QCM 04 - Le Modele Relationnel

**Chapitre IV** | 12 questions | Duree estimee : 8 minutes

---

## Question 1

Qu'est-ce qu'une cle primaire (Primary Key) ?

- A) La premiere colonne d'une table
- B) Un identifiant unique pour chaque ligne
- C) Une colonne qui contient des nombres
- D) Une colonne obligatoire

<details>
<summary>Reponse</summary>

**B) Un identifiant unique pour chaque ligne**

Une cle primaire identifie de maniere unique chaque enregistrement d'une table. Elle ne peut pas etre NULL et doit etre unique.

</details>

---

## Question 2

Qu'est-ce qu'une cle etrangere (Foreign Key) ?

- A) Une cle d'une base de donnees etrangere
- B) Une colonne qui reference la cle primaire d'une autre table
- C) Une cle chiffree
- D) Une cle temporaire

<details>
<summary>Reponse</summary>

**B) Une colonne qui reference la cle primaire d'une autre table**

Une cle etrangere etablit une relation entre deux tables. Elle garantit l'integrite referentielle.

</details>

---

## Question 3

Qu'est-ce que l'integrite referentielle ?

- A) Les donnees sont chiffrees
- B) Toute valeur de cle etrangere doit exister dans la table referencee
- C) Les donnees sont sauvegardees
- D) Les colonnes ont des noms uniques

<details>
<summary>Reponse</summary>

**B) Toute valeur de cle etrangere doit exister dans la table referencee**

L'integrite referentielle garantit qu'on ne peut pas referencer une ligne inexistante. Ex: Un employe ne peut pas avoir un DEPT_NUM qui n'existe pas dans DEPT.

</details>

---

## Question 4

Une cle primaire peut-elle contenir des valeurs NULL ?

- A) Oui, toujours
- B) Non, jamais
- C) Oui, si la table est vide
- D) Oui, sur certains SGBD

<details>
<summary>Reponse</summary>

**B) Non, jamais**

Une cle primaire doit etre : UNIQUE et NOT NULL. Ces deux contraintes sont implicites lors de la declaration d'une PRIMARY KEY.

</details>

---

## Question 5

Qu'est-ce qu'une cle composee ?

- A) Une cle chiffree
- B) Une cle primaire formee de plusieurs colonnes
- C) Une cle etrangere
- D) Une cle generee automatiquement

<details>
<summary>Reponse</summary>

**B) Une cle primaire formee de plusieurs colonnes**

Une cle composee utilise la combinaison de plusieurs colonnes pour identifier de maniere unique chaque ligne. Exemple : (EMP_NUM, PROJ_NUM) pour une table d'affectation.

</details>

---

## Question 6

Quelle contrainte garantit qu'une colonne ne peut pas etre vide ?

- A) UNIQUE
- B) CHECK
- C) NOT NULL
- D) PRIMARY KEY

<details>
<summary>Reponse</summary>

**C) NOT NULL**

La contrainte NOT NULL interdit les valeurs NULL dans une colonne. C'est l'une des contraintes d'integrite les plus courantes.

</details>

---

## Question 7

Quelle contrainte garantit que toutes les valeurs d'une colonne sont differentes ?

- A) NOT NULL
- B) CHECK
- C) UNIQUE
- D) FOREIGN KEY

<details>
<summary>Reponse</summary>

**C) UNIQUE**

La contrainte UNIQUE garantit l'unicite des valeurs dans une colonne. Contrairement a PRIMARY KEY, elle autorise une valeur NULL.

</details>

---

## Question 8

A quoi sert la contrainte CHECK ?

- A) Verifier la syntaxe SQL
- B) Definir une condition que les valeurs doivent respecter
- C) Verifier les droits d'acces
- D) Verifier l'existence d'un fichier

<details>
<summary>Reponse</summary>

**B) Definir une condition que les valeurs doivent respecter**

CHECK permet de definir des regles de validation. Exemple : CHECK (SAL > 0), CHECK (SEXE IN ('M', 'F')).

</details>

---

## Question 9

Que se passe-t-il si on tente de supprimer une ligne referencee par une cle etrangere (sans option particuliere) ?

- A) La suppression est effectuee
- B) La suppression est refusee
- C) Les lignes referencant sont aussi supprimees
- D) Les cles etrangeres sont mises a NULL

<details>
<summary>Reponse</summary>

**B) La suppression est refusee**

Par defaut, l'integrite referentielle empeche la suppression d'une ligne qui est referencee par une cle etrangere dans une autre table.

</details>

---

## Question 10

Quelle option de cle etrangere supprime automatiquement les lignes dependantes ?

- A) ON DELETE SET NULL
- B) ON DELETE CASCADE
- C) ON DELETE RESTRICT
- D) ON DELETE IGNORE

<details>
<summary>Reponse</summary>

**B) ON DELETE CASCADE**

ON DELETE CASCADE propage la suppression : si on supprime un departement, tous les employes de ce departement sont automatiquement supprimes.

</details>

---

## Question 11

Quelle est la difference entre PRIMARY KEY et UNIQUE ?

- A) Aucune difference
- B) PRIMARY KEY n'autorise pas NULL et est unique par table
- C) UNIQUE est plus rapide
- D) PRIMARY KEY peut avoir des doublons

<details>
<summary>Reponse</summary>

**B) PRIMARY KEY n'autorise pas NULL et est unique par table**

Differences :
- PRIMARY KEY : NOT NULL implicite, une seule par table
- UNIQUE : autorise NULL, plusieurs possibles par table

</details>

---

## Question 12

Qu'est-ce qu'un index dans une base de donnees ?

- A) Le numero de ligne
- B) Une structure d'acces rapide aux donnees
- C) La cle primaire
- D) Un type de contrainte

<details>
<summary>Reponse</summary>

**B) Une structure d'acces rapide aux donnees**

Un index est une structure de donnees (generalement B-tree) qui accelere la recherche. Il est cree automatiquement sur les cles primaires.

</details>

---

## Score

| Questions correctes | Appreciation |
|---------------------|--------------|
| 11-12 | Excellent |
| 9-10 | Tres bien |
| 7-8 | Bien |
| 4-6 | Moyen - Revoir le chapitre |
| 0-3 | Insuffisant - Reprendre le cours |

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [QCM 03 - Modelisation](qcm-03-modelisation.md) | [QCM 05 - SQL](qcm-05-sql.md) |

---
*Formation DB2/SQL - M2i Formation*
