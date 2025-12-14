# QCM 02 - Architecture DB2

**Chapitre II** | 12 questions | Duree estimee : 8 minutes

---

## Question 1

Quelle est la hierarchie correcte des objets DB2 ?

- A) Table > Tablespace > Database > Subsystem
- B) Subsystem > Database > Tablespace > Table
- C) Database > Subsystem > Table > Tablespace
- D) Tablespace > Table > Database > Subsystem

<details>
<summary>Reponse</summary>

**B) Subsystem > Database > Tablespace > Table**

Hierarchie DB2 :
1. **Subsystem** (instance DB2)
2. **Database** (regroupement logique)
3. **Tablespace** (unite de stockage)
4. **Table** (structure de donnees)

</details>

---

## Question 2

Qu'est-ce qu'un tablespace dans DB2 ?

- A) Un espace disque physique
- B) Une unite de stockage contenant une ou plusieurs tables
- C) Un index de table
- D) Une vue materialisee

<details>
<summary>Reponse</summary>

**B) Une unite de stockage contenant une ou plusieurs tables**

Un tablespace est l'unite de stockage logique dans DB2. Il peut contenir une ou plusieurs tables et correspond physiquement a un dataset VSAM Linear.

</details>

---

## Question 3

Quel type de fichier VSAM est utilise pour stocker les tablespaces DB2 ?

- A) KSDS
- B) ESDS
- C) RRDS
- D) Linear (LDS)

<details>
<summary>Reponse</summary>

**D) Linear (LDS)**

DB2 utilise des fichiers VSAM Linear (LDS) pour stocker les tablespaces. Ce format permet un acces direct par page.

</details>

---

## Question 4

A quoi sert le catalogue systeme DB2 ?

- A) A stocker les donnees utilisateur
- B) A stocker les metadonnees (description des objets)
- C) A gerer les sauvegardes
- D) A optimiser les requetes

<details>
<summary>Reponse</summary>

**B) A stocker les metadonnees (description des objets)**

Le catalogue systeme contient toutes les informations sur les objets DB2 : tables (SYSTABLES), colonnes (SYSCOLUMNS), index (SYSINDEXES), etc.

</details>

---

## Question 5

Quelle table du catalogue contient la liste de toutes les tables ?

- A) SYSIBM.SYSTABLES
- B) SYSIBM.SYSCOLUMNS
- C) SYSIBM.SYSINDEXES
- D) SYSIBM.SYSDATABASE

<details>
<summary>Reponse</summary>

**A) SYSIBM.SYSTABLES**

SYSIBM.SYSTABLES contient une ligne pour chaque table et vue de la base de donnees.

</details>

---

## Question 6

Qu'est-ce qu'un STOGROUP dans DB2 ?

- A) Un groupe d'utilisateurs
- B) Un groupe de volumes disque pour le stockage
- C) Un groupe de tables
- D) Un groupe d'index

<details>
<summary>Reponse</summary>

**B) Un groupe de volumes disque pour le stockage**

Un STOGROUP (Storage Group) definit les volumes DASD sur lesquels DB2 peut allouer les datasets pour les tablespaces.

</details>

---

## Question 7

Quelle est la difference entre DB2I et SPUFI ?

- A) DB2I est l'interface complete, SPUFI execute les requetes SQL
- B) DB2I est pour le batch, SPUFI pour l'online
- C) Ils sont identiques
- D) SPUFI est l'interface complete, DB2I execute les requetes

<details>
<summary>Reponse</summary>

**A) DB2I est l'interface complete, SPUFI execute les requetes SQL**

- **DB2I** : Interface ISPF complete pour toutes les operations DB2
- **SPUFI** : Outil specifique pour executer des requetes SQL interactivement

</details>

---

## Question 8

Dans quel mode les requetes SPUFI sont-elles executees ?

- A) Batch uniquement
- B) Online (interactif)
- C) Les deux
- D) Ni l'un ni l'autre

<details>
<summary>Reponse</summary>

**B) Online (interactif)**

SPUFI (SQL Processor Using File Input) est un outil interactif. Les requetes sont ecrites dans un fichier, executees, et les resultats affiches immediatement.

</details>

---

## Question 9

Qu'est-ce qu'un plan d'acces (access path) dans DB2 ?

- A) Le chemin physique vers les donnees sur disque
- B) La strategie choisie par l'optimiseur pour executer une requete
- C) La sequence des tables dans une jointure
- D) L'ordre des index

<details>
<summary>Reponse</summary>

**B) La strategie choisie par l'optimiseur pour executer une requete**

L'optimiseur DB2 analyse la requete et determine le plan d'acces optimal : utilisation d'index, ordre des jointures, methode de tri, etc.

</details>

---

## Question 10

Quelle table du catalogue contient la description des colonnes ?

- A) SYSIBM.SYSTABLES
- B) SYSIBM.SYSCOLUMNS
- C) SYSIBM.SYSFIELDS
- D) SYSIBM.SYSATTRIBUTES

<details>
<summary>Reponse</summary>

**B) SYSIBM.SYSCOLUMNS**

SYSIBM.SYSCOLUMNS contient une ligne pour chaque colonne de chaque table, avec son nom, type, longueur, etc.

</details>

---

## Question 11

Qu'est-ce qu'un buffer pool dans DB2 ?

- A) Une zone de stockage sur disque
- B) Une zone memoire pour cacher les pages de donnees
- C) Un pool de connexions
- D) Une file d'attente de requetes

<details>
<summary>Reponse</summary>

**B) Une zone memoire pour cacher les pages de donnees**

Un buffer pool est une zone en memoire ou DB2 garde les pages de donnees frequemment accedees pour ameliorer les performances.

</details>

---

## Question 12

Quel est le prefixe standard des tables du catalogue systeme DB2 ?

- A) SYS.
- B) SYSIBM.
- C) DB2.
- D) CATALOG.

<details>
<summary>Reponse</summary>

**B) SYSIBM.**

Les tables du catalogue systeme ont le prefixe SYSIBM. Exemples : SYSIBM.SYSTABLES, SYSIBM.SYSCOLUMNS, SYSIBM.SYSINDEXES.

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
| [QCM 01 - Fondamentaux](qcm-01-fondamentaux.md) | [QCM 03 - Modelisation](qcm-03-modelisation.md) |

---
*Formation DB2/SQL - M2i Formation*
