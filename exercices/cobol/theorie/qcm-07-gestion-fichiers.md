# QCM - Chapitre VII : Gestion des Fichiers

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Notion de fichiers (Questions 1-6)

### Question 1
Qu'est-ce qu'un enregistrement (Record) en COBOL ?

- [ ] A) Un seul champ de données
- [ ] B) Un ensemble de champs décrivant une entité
- [ ] C) Un fichier complet
- [ ] D) Une variable en mémoire

<details>
<summary>Réponse</summary>

**B** - Un enregistrement est un ensemble de champs (fields) qui forment une entité logique.

</details>

---

### Question 2
Quelle est la différence entre enregistrement physique et logique ?

- [ ] A) Aucune différence
- [ ] B) Physique = sur disque, Logique = traité par le programme
- [ ] C) Physique = un seul, Logique = plusieurs
- [ ] D) Physique = en RAM, Logique = sur disque

<details>
<summary>Réponse</summary>

**B** - L'enregistrement physique existe sur le périphérique externe. L'enregistrement logique est l'unité de traitement du programme.

</details>

---

### Question 3
Qu'est-ce qu'une clé primaire dans un fichier ?

- [ ] A) Un champ quelconque
- [ ] B) Une valeur unique identifiant chaque enregistrement
- [ ] C) Le premier champ du fichier
- [ ] D) Un champ obligatoire pour tous les fichiers

<details>
<summary>Réponse</summary>

**B** - La clé primaire est unique à chaque enregistrement et permet de l'identifier.

</details>

---

### Question 4
Où déclare-t-on un fichier en COBOL ?

- [ ] A) WORKING-STORAGE SECTION uniquement
- [ ] B) FILE SECTION uniquement
- [ ] C) ENVIRONMENT DIVISION (FILE-CONTROL) + DATA DIVISION (FILE SECTION)
- [ ] D) PROCEDURE DIVISION

<details>
<summary>Réponse</summary>

**C** - Un fichier est déclaré en deux endroits : SELECT dans FILE-CONTROL, et FD dans FILE SECTION.

</details>

---

### Question 5
À quoi sert la clause FILE STATUS ?

- [ ] A) Définir la taille du fichier
- [ ] B) Récupérer un code retour après chaque opération E/S
- [ ] C) Spécifier l'organisation du fichier
- [ ] D) Définir la clé du fichier

<details>
<summary>Réponse</summary>

**B** - FILE STATUS stocke un code à 2 caractères indiquant le résultat de chaque opération (00=OK, 10=EOF, etc.).

</details>

---

### Question 6
Que signifie le code FILE STATUS "10" ?

- [ ] A) Opération réussie
- [ ] B) Fin de fichier (EOF)
- [ ] C) Enregistrement non trouvé
- [ ] D) Fichier non trouvé

<details>
<summary>Réponse</summary>

**B** - Le code 10 indique la fin de fichier (End Of File), signalant qu'il n'y a plus d'enregistrement à lire.

</details>

---

## Section 2 : Organisation des fichiers (Questions 7-14)

### Question 7
Quelles sont les trois organisations de fichiers en COBOL ?

- [ ] A) SEQUENTIAL, INDEXED, RELATIVE
- [ ] B) SEQUENTIAL, RANDOM, DYNAMIC
- [ ] C) LINEAR, INDEXED, HASHED
- [ ] D) ESDS, KSDS, RRDS

<details>
<summary>Réponse</summary>

**A** - Les trois organisations sont SEQUENTIAL, INDEXED et RELATIVE. (D représente les types VSAM correspondants)

</details>

---

### Question 8
Quelle organisation correspond au type VSAM KSDS ?

- [ ] A) SEQUENTIAL
- [ ] B) INDEXED
- [ ] C) RELATIVE
- [ ] D) DYNAMIC

<details>
<summary>Réponse</summary>

**B** - KSDS (Key Sequenced Data Set) correspond à ORGANIZATION IS INDEXED.

</details>

---

### Question 9
Peut-on supprimer un enregistrement dans un fichier SEQUENTIAL ?

- [ ] A) Oui, avec DELETE
- [ ] B) Oui, avec REWRITE
- [ ] C) Non, c'est impossible
- [ ] D) Oui, avec REMOVE

<details>
<summary>Réponse</summary>

**C** - Les fichiers SEQUENTIAL ne permettent ni suppression ni insertion. On ne peut qu'ajouter en fin (EXTEND).

</details>

---

### Question 10
Quelle clause est obligatoire pour un fichier INDEXED ?

- [ ] A) RELATIVE KEY
- [ ] B) RECORD KEY
- [ ] C) ALTERNATE KEY
- [ ] D) FILE STATUS

<details>
<summary>Réponse</summary>

**B** - RECORD KEY est obligatoire pour définir la clé primaire d'un fichier indexé.

</details>

---

### Question 11
À quoi sert ALTERNATE RECORD KEY ?

- [ ] A) Remplacer la clé primaire
- [ ] B) Définir une ou plusieurs clés secondaires
- [ ] C) Créer un backup de la clé
- [ ] D) Indexer par date

<details>
<summary>Réponse</summary>

**B** - ALTERNATE RECORD KEY permet de définir des clés secondaires pour accéder aux enregistrements par d'autres critères.

</details>

---

### Question 12
Quelle est la particularité de l'organisation RELATIVE ?

- [ ] A) Pas de clé
- [ ] B) Accès par position (numéro d'emplacement)
- [ ] C) Fichiers triés uniquement
- [ ] D) Lecture seule

<details>
<summary>Réponse</summary>

**B** - En RELATIVE, la RELATIVE KEY représente la position de l'enregistrement par rapport au début du fichier.

</details>

---

### Question 13
Quel est l'inconvénient des fichiers RELATIVE ?

- [ ] A) Pas d'accès direct
- [ ] B) L'espace est réservé même pour les emplacements vides
- [ ] C) Pas de modification possible
- [ ] D) Clé obligatoirement alphanumérique

<details>
<summary>Réponse</summary>

**B** - Les fichiers RELATIVE réservent de l'espace pour tous les emplacements, même ceux sans enregistrement.

</details>

---

### Question 14
Quel type de fichier offre l'accès le plus rapide ?

- [ ] A) SEQUENTIAL
- [ ] B) INDEXED
- [ ] C) RELATIVE
- [ ] D) Tous équivalents

<details>
<summary>Réponse</summary>

**C** - RELATIVE offre l'accès le plus rapide car la position est calculée directement (pas de parcours d'index).

</details>

---

## Section 3 : Modes d'accès (Questions 15-20)

### Question 15
Quels sont les trois modes d'accès aux fichiers ?

- [ ] A) READ, WRITE, UPDATE
- [ ] B) SEQUENTIAL, RANDOM, DYNAMIC
- [ ] C) INPUT, OUTPUT, I-O
- [ ] D) OPEN, CLOSE, READ

<details>
<summary>Réponse</summary>

**B** - Les trois modes d'accès sont SEQUENTIAL, RANDOM et DYNAMIC.

</details>

---

### Question 16
Quel mode d'accès est compatible avec ORGANIZATION IS SEQUENTIAL ?

- [ ] A) SEQUENTIAL uniquement
- [ ] B) RANDOM uniquement
- [ ] C) DYNAMIC uniquement
- [ ] D) Les trois modes

<details>
<summary>Réponse</summary>

**A** - Un fichier SEQUENTIAL ne peut être accédé qu'en mode SEQUENTIAL.

</details>

---

### Question 17
Que permet ACCESS MODE IS DYNAMIC ?

- [ ] A) Accès séquentiel uniquement
- [ ] B) Accès direct uniquement
- [ ] C) Combinaison accès séquentiel ET direct
- [ ] D) Accès automatique

<details>
<summary>Réponse</summary>

**C** - DYNAMIC permet d'alterner entre accès séquentiel (READ NEXT) et accès direct (READ KEY).

</details>

---

### Question 18
Pour utiliser READ NEXT, quel mode d'accès faut-il ?

- [ ] A) SEQUENTIAL ou DYNAMIC
- [ ] B) RANDOM uniquement
- [ ] C) DYNAMIC uniquement
- [ ] D) N'importe quel mode

<details>
<summary>Réponse</summary>

**A** - READ NEXT (lecture séquentielle) nécessite ACCESS MODE IS SEQUENTIAL ou DYNAMIC.

</details>

---

### Question 19
Comment accéder directement à un enregistrement par sa clé ?

- [ ] A) `READ fichier NEXT`
- [ ] B) `READ fichier KEY IS cle`
- [ ] C) `READ fichier DIRECT`
- [ ] D) `GET fichier KEY cle`

<details>
<summary>Réponse</summary>

**B** - `READ fichier KEY IS nom-cle` permet l'accès direct en mode RANDOM ou DYNAMIC.

</details>

---

### Question 20
Quel mode d'accès choisir pour un traitement de mise à jour aléatoire ?

- [ ] A) SEQUENTIAL
- [ ] B) RANDOM
- [ ] C) DYNAMIC
- [ ] D) B ou C

<details>
<summary>Réponse</summary>

**D** - Pour des mises à jour aléatoires, on utilise RANDOM (accès direct seul) ou DYNAMIC (si on veut aussi du séquentiel).

</details>

---

## Section 4 : Opérations et modes d'ouverture (Questions 21-25)

### Question 21
Quels sont les quatre modes d'ouverture d'un fichier ?

- [ ] A) READ, WRITE, UPDATE, DELETE
- [ ] B) INPUT, OUTPUT, I-O, EXTEND
- [ ] C) OPEN, CLOSE, START, STOP
- [ ] D) CREATE, MODIFY, DELETE, CLOSE

<details>
<summary>Réponse</summary>

**B** - Les quatre modes d'ouverture sont INPUT (lecture), OUTPUT (écriture/création), I-O (mise à jour) et EXTEND (ajout en fin).

</details>

---

### Question 22
Quel mode d'ouverture permet la lecture ET l'écriture ?

- [ ] A) INPUT
- [ ] B) OUTPUT
- [ ] C) I-O
- [ ] D) EXTEND

<details>
<summary>Réponse</summary>

**C** - Le mode I-O (Input-Output) permet à la fois READ, REWRITE et DELETE.

</details>

---

### Question 23
Que fait REWRITE ?

- [ ] A) Supprime un enregistrement
- [ ] B) Modifie un enregistrement existant (après READ)
- [ ] C) Ajoute un nouvel enregistrement
- [ ] D) Réécrit tout le fichier

<details>
<summary>Réponse</summary>

**B** - REWRITE modifie l'enregistrement qui vient d'être lu. Un READ réussi doit précéder le REWRITE.

</details>

---

### Question 24
À quoi sert l'instruction START ?

- [ ] A) Démarrer un programme
- [ ] B) Positionner le curseur avant une lecture séquentielle
- [ ] C) Créer un nouveau fichier
- [ ] D) Initialiser une variable

<details>
<summary>Réponse</summary>

**B** - START positionne le curseur sur un enregistrement selon une condition de clé (>=, >, =, etc.) avant READ NEXT.

</details>

---

### Question 25
Quelle clause gère les erreurs lors d'une lecture directe ?

- [ ] A) AT END
- [ ] B) ON ERROR
- [ ] C) INVALID KEY
- [ ] D) EXCEPTION

<details>
<summary>Réponse</summary>

**C** - `INVALID KEY` gère les erreurs d'accès direct (clé non trouvée, clé en double). `AT END` est pour la lecture séquentielle.

</details>

---

## Résumé des scores

| Score | Niveau |
|-------|--------|
| 23-25 | Excellent - Maîtrise complète |
| 18-22 | Bien - Quelques révisions mineures |
| 13-17 | Moyen - Relire le chapitre |
| < 13 | Insuffisant - Revoir le cours en détail |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [QCM Chapitre VI](qcm-06-gestion-tables.md) | [QCM Chapitre VIII](qcm-08-operations-es.md) |

---
*Formation COBOL - M2i Formation*
