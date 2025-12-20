# QCM - Chapitre X : Traitement des Fichiers COBOL

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Types de Data Sets (Questions 1-5)

### Question 1
Quel type de Data Set VSAM correspond à l'organisation INDEXED en COBOL ?

- [ ] A) ESDS
- [ ] B) KSDS
- [ ] C) RRDS
- [ ] D) PS

<details>
<summary>Réponse</summary>

**B** - KSDS (Key-Sequenced Data Set) correspond à ORGANIZATION IS INDEXED en COBOL.

</details>

---

### Question 2
Quelle est la caractéristique principale d'un fichier ESDS ?

- [ ] A) Accès par clé primaire
- [ ] B) Accès par numéro de position relative
- [ ] C) Enregistrements stockés dans l'ordre d'écriture, accès séquentiel uniquement
- [ ] D) Possibilité de suppression d'enregistrements

<details>
<summary>Réponse</summary>

**C** - Les fichiers ESDS (Entry-Sequenced Data Set) stockent les enregistrements dans l'ordre d'écriture et ne permettent que l'accès séquentiel.

</details>

---

### Question 3
Dans un fichier RRDS, comment sont organisés les enregistrements ?

- [ ] A) Par valeur de clé alphabétique
- [ ] B) Par numéro d'enregistrement relatif (RRN)
- [ ] C) Dans l'ordre chronologique
- [ ] D) Par taille d'enregistrement

<details>
<summary>Réponse</summary>

**B** - Les fichiers RRDS (Relative Record Data Set) organisent les enregistrements par numéro relatif (RRN).

</details>

---

### Question 4
Peut-on supprimer un enregistrement dans un fichier KSDS ?

- [ ] A) Non, jamais
- [ ] B) Oui, avec DELETE
- [ ] C) Oui, mais seulement avec REWRITE
- [ ] D) Oui, mais seulement en recréant le fichier

<details>
<summary>Réponse</summary>

**B** - Les fichiers KSDS permettent la suppression d'enregistrements avec l'instruction DELETE.

</details>

---

### Question 5
Quels modes d'accès sont disponibles pour un fichier KSDS ?

- [ ] A) SEQUENTIAL uniquement
- [ ] B) RANDOM uniquement
- [ ] C) SEQUENTIAL, RANDOM ou DYNAMIC
- [ ] D) DYNAMIC uniquement

<details>
<summary>Réponse</summary>

**C** - Les fichiers KSDS supportent les trois modes d'accès : SEQUENTIAL, RANDOM et DYNAMIC.

</details>

---

## Section 2 : Clauses FILE-CONTROL (Questions 6-12)

### Question 6
À quoi sert la clause OPTIONAL sur un SELECT ?

- [ ] A) Rendre le fichier accessible en lecture seule
- [ ] B) Indiquer que le fichier peut ne pas exister à l'ouverture
- [ ] C) Optimiser les performances
- [ ] D) Définir une clé secondaire

<details>
<summary>Réponse</summary>

**B** - OPTIONAL indique que le fichier n'a pas besoin d'être obligatoirement présent à l'exécution. Si absent, FILE STATUS retourne 05.

</details>

---

### Question 7
Si ORGANIZATION n'est pas codée, quelle valeur est utilisée par défaut ?

- [ ] A) INDEXED
- [ ] B) RELATIVE
- [ ] C) SEQUENTIAL
- [ ] D) Une erreur se produit

<details>
<summary>Réponse</summary>

**C** - Si ORGANIZATION n'est pas codée, SEQUENTIAL est implicite.

</details>

---

### Question 8
Si ACCESS MODE n'est pas codé, quelle valeur est utilisée par défaut ?

- [ ] A) RANDOM
- [ ] B) DYNAMIC
- [ ] C) SEQUENTIAL
- [ ] D) Dépend de l'organisation

<details>
<summary>Réponse</summary>

**C** - Si ACCESS n'est pas codé, SEQUENTIAL est implicite pour toutes les organisations.

</details>

---

### Question 9
Quelle clause est obligatoire pour un fichier INDEXED ?

- [ ] A) RELATIVE KEY
- [ ] B) RECORD KEY
- [ ] C) ALTERNATE RECORD KEY
- [ ] D) FILE STATUS

<details>
<summary>Réponse</summary>

**B** - RECORD KEY est obligatoire pour définir la clé primaire d'un fichier INDEXED.

</details>

---

### Question 10
À quoi sert la clause WITH DUPLICATES sur ALTERNATE RECORD KEY ?

- [ ] A) Créer des clés primaires en double
- [ ] B) Autoriser plusieurs enregistrements avec la même valeur de clé secondaire
- [ ] C) Dupliquer les enregistrements
- [ ] D) Créer une copie de sauvegarde

<details>
<summary>Réponse</summary>

**B** - WITH DUPLICATES autorise plusieurs enregistrements à avoir la même valeur de clé secondaire.

</details>

---

### Question 11
Quelle clause spécifie le numéro d'enregistrement relatif pour un fichier RELATIVE ?

- [ ] A) RECORD KEY
- [ ] B) ALTERNATE KEY
- [ ] C) RELATIVE KEY
- [ ] D) POSITION KEY

<details>
<summary>Réponse</summary>

**C** - RELATIVE KEY spécifie la variable contenant le numéro d'enregistrement relatif (RRN) pour les fichiers RELATIVE.

</details>

---

### Question 12
À quoi sert la clause RESERVE ?

- [ ] A) Réserver de l'espace disque
- [ ] B) Spécifier le nombre de buffers mémoire pour les E/S
- [ ] C) Protéger le fichier
- [ ] D) Réserver des enregistrements

<details>
<summary>Réponse</summary>

**B** - RESERVE spécifie le nombre de tampons (buffers) I/O à allouer à l'exécution pour optimiser les performances.

</details>

---

## Section 3 : Clauses FD (Questions 13-17)

### Question 13
Que signifie RECORDING MODE IS F ?

- [ ] A) Fichier formaté
- [ ] B) Enregistrements de longueur fixe
- [ ] C) Fichier final
- [ ] D) Fichier avec labels

<details>
<summary>Réponse</summary>

**B** - RECORDING MODE IS F (Fixed) indique que les enregistrements ont une longueur fixe.

</details>

---

### Question 14
Quelle clause permet de définir des enregistrements de longueur variable ?

- [ ] A) RECORDING MODE IS V
- [ ] B) RECORD CONTAINS n TO m CHARACTERS
- [ ] C) RECORD IS VARYING
- [ ] D) Toutes ces réponses

<details>
<summary>Réponse</summary>

**D** - Toutes ces clauses peuvent être utilisées pour définir des enregistrements de longueur variable.

</details>

---

### Question 15
Quelle valeur recommande-t-on pour BLOCK CONTAINS afin de laisser le système optimiser ?

- [ ] A) BLOCK CONTAINS 1 RECORD
- [ ] B) BLOCK CONTAINS 0 RECORDS
- [ ] C) BLOCK CONTAINS 100 RECORDS
- [ ] D) Ne pas coder la clause

<details>
<summary>Réponse</summary>

**B** - BLOCK CONTAINS 0 RECORDS laisse le système décider de la taille de bloc optimale.

</details>

---

### Question 16
Quelle valeur de LABEL RECORD utilise-t-on pour un fichier d'impression ?

- [ ] A) STANDARD
- [ ] B) OMITTED
- [ ] C) PRINTER
- [ ] D) OUTPUT

<details>
<summary>Réponse</summary>

**B** - LABEL RECORD IS OMITTED est utilisé pour les fichiers d'impression qui n'ont pas de labels.

</details>

---

### Question 17
Dans la clause DATA RECORD, que déclare-t-on ?

- [ ] A) Les données à écrire
- [ ] B) Le nom de l'enregistrement associé au fichier
- [ ] C) Le type de données
- [ ] D) La taille des données

<details>
<summary>Réponse</summary>

**B** - DATA RECORD nomme l'enregistrement (niveau 01) associé au fichier. C'est une clause documentaire.

</details>

---

## Section 4 : Accès et Opérations (Questions 18-22)

### Question 18
Comment accéder à un enregistrement par clé secondaire dans un fichier KSDS ?

- [ ] A) Avec READ fichier ALTERNATE KEY
- [ ] B) Avec READ fichier KEY IS nom-cle-secondaire
- [ ] C) Impossible, seule la clé primaire est utilisable
- [ ] D) Avec READ fichier SECONDARY KEY

<details>
<summary>Réponse</summary>

**B** - On utilise `READ fichier KEY IS nom-cle-secondaire` pour lire par clé secondaire.

</details>

---

### Question 19
Qu'est-ce qu'un AIX dans le contexte VSAM ?

- [ ] A) Un fichier de sauvegarde
- [ ] B) Un index alternatif permettant l'accès par clé secondaire
- [ ] C) Un mode d'accès spécial
- [ ] D) Un type de compression

<details>
<summary>Réponse</summary>

**B** - AIX (Alternate Index) est un fichier séparé qui maintient un index sur une clé secondaire, permettant l'accès par cette clé.

</details>

---

### Question 20
En mode d'accès DYNAMIC, quelles commandes permettent le positionnement et la lecture séquentielle ?

- [ ] A) SEEK et READ
- [ ] B) START et READ NEXT
- [ ] C) POSITION et READ
- [ ] D) LOCATE et READ

<details>
<summary>Réponse</summary>

**B** - En mode DYNAMIC, on utilise START pour positionner le pointeur, puis READ NEXT pour lire séquentiellement.

</details>

---

### Question 21
Quel mode d'ouverture permet à la fois READ, WRITE, REWRITE et DELETE sur un fichier KSDS ?

- [ ] A) INPUT
- [ ] B) OUTPUT
- [ ] C) I-O
- [ ] D) EXTEND

<details>
<summary>Réponse</summary>

**C** - Le mode I-O permet toutes les opérations : READ, WRITE, REWRITE et DELETE sur un fichier INDEXED.

</details>

---

### Question 22
START peut-il être utilisé sur un fichier SEQUENTIAL ?

- [ ] A) Oui, sans restriction
- [ ] B) Non, uniquement sur INDEXED et RELATIVE
- [ ] C) Oui, mais seulement en mode I-O
- [ ] D) Oui, avec DYNAMIC uniquement

<details>
<summary>Réponse</summary>

**B** - START n'est utilisable que sur les fichiers INDEXED et RELATIVE, pas sur les fichiers SEQUENTIAL.

</details>

---

## Section 5 : FILE STATUS (Questions 23-25)

### Question 23
Que signifie le FILE STATUS "05" ?

- [ ] A) Erreur d'écriture
- [ ] B) Fichier OPTIONAL non présent, créé vide
- [ ] C) Fin de fichier
- [ ] D) Clé en double

<details>
<summary>Réponse</summary>

**B** - Le code 05 indique qu'un fichier OPTIONAL n'était pas présent et a été créé vide ou considéré comme vide.

</details>

---

### Question 24
Quel code FILE STATUS indique une clé en double lors d'un WRITE ?

- [ ] A) 21
- [ ] B) 22
- [ ] C) 23
- [ ] D) 24

<details>
<summary>Réponse</summary>

**B** - Le code 22 indique une clé en double (tentative d'insertion d'une clé primaire déjà existante).

</details>

---

### Question 25
Quel code FILE STATUS indique qu'un REWRITE a été tenté sans READ préalable ?

- [ ] A) 42
- [ ] B) 43
- [ ] C) 44
- [ ] D) 49

<details>
<summary>Réponse</summary>

**B** - Le code 43 indique qu'il n'y a pas eu de READ préalable avant un REWRITE ou DELETE.

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
| [QCM Chapitre IX](qcm-09-sous-programmes.md) | [QCM Chapitre XI](qcm-11-tri-interne.md) |

---
*Formation COBOL - M2i Formation*
