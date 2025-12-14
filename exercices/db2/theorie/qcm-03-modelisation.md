# QCM 03 - Modelisation des Donnees

**Chapitre III** | 12 questions | Duree estimee : 8 minutes

---

## Question 1

Que signifie MCD ?

- A) Modele de Conception de Donnees
- B) Modele Conceptuel de Donnees
- C) Module de Creation de Donnees
- D) Methode de Controle de Donnees

<details>
<summary>Reponse</summary>

**B) Modele Conceptuel de Donnees**

Le MCD est la representation conceptuelle des donnees, independante de toute implementation technique. Il utilise les concepts d'entites, attributs et associations.

</details>

---

## Question 2

Que signifie MLD ?

- A) Modele Logique de Donnees
- B) Modele de Liaison de Donnees
- C) Module de Lecture de Donnees
- D) Methode de Liaison Directe

<details>
<summary>Reponse</summary>

**A) Modele Logique de Donnees**

Le MLD est la traduction du MCD vers un modele specifique (relationnel). Il decrit les tables, colonnes et relations.

</details>

---

## Question 3

Qu'est-ce qu'une entite dans un MCD ?

- A) Une ligne de tableau
- B) Un objet du monde reel a representer
- C) Une colonne de table
- D) Une contrainte d'integrite

<details>
<summary>Reponse</summary>

**B) Un objet du monde reel a representer**

Une entite represente un concept du monde reel (employe, departement, produit) ayant des attributs et pouvant etre identifiee de maniere unique.

</details>

---

## Question 4

Qu'est-ce que la normalisation des donnees ?

- A) La conversion des donnees en majuscules
- B) Le processus d'organisation pour reduire la redondance
- C) Le chiffrement des donnees
- D) La compression des donnees

<details>
<summary>Reponse</summary>

**B) Le processus d'organisation pour reduire la redondance**

La normalisation vise a organiser les donnees pour eliminer les redondances et les anomalies de mise a jour. Elle suit des regles appelees formes normales.

</details>

---

## Question 5

Qu'impose la premiere forme normale (1NF) ?

- A) Pas de colonnes calculees
- B) Chaque attribut doit etre atomique (valeur unique)
- C) Pas de cles etrangeres
- D) Maximum 10 colonnes par table

<details>
<summary>Reponse</summary>

**B) Chaque attribut doit etre atomique (valeur unique)**

La 1NF exige que chaque cellule contienne une seule valeur atomique (pas de liste, pas de groupe repetitif).

</details>

---

## Question 6

Qu'impose la deuxieme forme normale (2NF) ?

- A) Etre en 1NF + pas de dependances partielles vers la cle primaire
- B) Etre en 1NF + maximum 5 colonnes
- C) Etre en 1NF + pas de valeurs NULL
- D) Etre en 1NF + index obligatoire

<details>
<summary>Reponse</summary>

**A) Etre en 1NF + pas de dependances partielles vers la cle primaire**

La 2NF requiert que tous les attributs non-cle dependent de TOUTE la cle primaire, pas seulement d'une partie (pertinent pour les cles composees).

</details>

---

## Question 7

Qu'impose la troisieme forme normale (3NF) ?

- A) Etre en 2NF + pas de dependances transitives
- B) Etre en 2NF + pas de jointures
- C) Etre en 2NF + cles etrangeres obligatoires
- D) Etre en 2NF + maximum 3 tables

<details>
<summary>Reponse</summary>

**A) Etre en 2NF + pas de dependances transitives**

La 3NF exige que les attributs non-cle ne dependent pas d'autres attributs non-cle. Chaque attribut doit dependre directement de la cle primaire.

</details>

---

## Question 8

Quelle est la cardinalite d'une relation "Un departement emploie plusieurs employes, un employe appartient a un seul departement" ?

- A) 1:1
- B) 1:N
- C) N:M
- D) 0:1

<details>
<summary>Reponse</summary>

**B) 1:N**

Un departement (1) peut avoir plusieurs (N) employes, mais un employe appartient a un seul (1) departement.

</details>

---

## Question 9

Comment represente-t-on une relation N:M (plusieurs a plusieurs) dans un modele relationnel ?

- A) Avec une cle etrangere dans chaque table
- B) Avec une table d'association intermediaire
- C) Avec un attribut multi-valeur
- D) C'est impossible en relationnel

<details>
<summary>Reponse</summary>

**B) Avec une table d'association intermediaire**

Une relation N:M necessite une table de jonction contenant les cles primaires des deux tables comme cle composee.

</details>

---

## Question 10

Qu'est-ce qu'une dependance fonctionnelle ?

- A) Une fonction SQL
- B) Un attribut B depend d'un attribut A si A determine B de maniere unique
- C) Une procedure stockee
- D) Une contrainte CHECK

<details>
<summary>Reponse</summary>

**B) Un attribut B depend d'un attribut A si A determine B de maniere unique**

Notation : A → B. Pour chaque valeur de A, il existe une seule valeur de B. Exemple : EMP_NUM → EMP_NOM.

</details>

---

## Question 11

Quel probleme la normalisation permet-elle d'eviter ?

- A) Les erreurs de syntaxe SQL
- B) Les anomalies d'insertion, modification et suppression
- C) Les problemes de performance
- D) Les acces non autorises

<details>
<summary>Reponse</summary>

**B) Les anomalies d'insertion, modification et suppression**

Sans normalisation, la redondance entraine :
- Anomalie d'insertion : impossible d'inserer sans toutes les donnees
- Anomalie de modification : mises a jour partielles = incoherences
- Anomalie de suppression : perte d'informations non voulue

</details>

---

## Question 12

Dans le passage du MCD au MLD, une entite devient :

- A) Une vue
- B) Une table
- C) Un index
- D) Une procedure

<details>
<summary>Reponse</summary>

**B) Une table**

Regles de transformation MCD → MLD :
- Entite → Table
- Attribut → Colonne
- Identifiant → Cle primaire
- Association 1:N → Cle etrangere

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
| [QCM 02 - Architecture](qcm-02-architecture.md) | [QCM 04 - Modele relationnel](qcm-04-modele-relationnel.md) |

---
*Formation DB2/SQL - M2i Formation*
