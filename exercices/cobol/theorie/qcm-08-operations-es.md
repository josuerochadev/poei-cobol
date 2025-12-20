# QCM - Chapitre VIII : Opérations d'E/S sur les Fichiers

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : OPEN - Ouverture (Questions 1-5)

### Question 1
Quels sont les quatre modes d'ouverture d'un fichier en COBOL ?

- [ ] A) READ, WRITE, UPDATE, DELETE
- [ ] B) INPUT, OUTPUT, I-O, EXTEND
- [ ] C) SEQUENTIAL, RANDOM, DYNAMIC, RELATIVE
- [ ] D) CREATE, MODIFY, APPEND, CLOSE

<details>
<summary>Réponse</summary>

**B** - Les quatre modes sont : INPUT (lecture), OUTPUT (création/écrasement), I-O (lecture/écriture) et EXTEND (ajout en fin).

</details>

---

### Question 2
Quel mode d'ouverture écrase le contenu existant d'un fichier séquentiel ?

- [ ] A) INPUT
- [ ] B) OUTPUT
- [ ] C) I-O
- [ ] D) EXTEND

<details>
<summary>Réponse</summary>

**B** - OUTPUT crée un nouveau fichier ou écrase le contenu existant pour les fichiers séquentiels.

</details>

---

### Question 3
Quel mode d'ouverture permet d'ajouter des enregistrements à la fin d'un fichier ?

- [ ] A) INPUT
- [ ] B) OUTPUT
- [ ] C) I-O
- [ ] D) EXTEND

<details>
<summary>Réponse</summary>

**D** - EXTEND permet d'ajouter des enregistrements à la fin d'un fichier séquentiel sans écraser le contenu.

</details>

---

### Question 4
Le mode EXTEND est-il autorisé pour les fichiers en accès aléatoire ?

- [ ] A) Oui, pour tous les types de fichiers
- [ ] B) Non, uniquement pour l'accès séquentiel
- [ ] C) Oui, mais seulement pour les fichiers indexés
- [ ] D) Oui, mais seulement pour les fichiers relatifs

<details>
<summary>Réponse</summary>

**B** - Le mode EXTEND n'est pas autorisé pour les fichiers en accès Aléatoire ou Dynamique.

</details>

---

### Question 5
Que signifie le FILE STATUS "35" après un OPEN ?

- [ ] A) Fichier déjà ouvert
- [ ] B) Fichier non trouvé
- [ ] C) Mode d'ouverture incompatible
- [ ] D) Ouverture réussie

<details>
<summary>Réponse</summary>

**B** - Le code 35 indique que le fichier n'a pas été trouvé (fichier inexistant pour INPUT ou I-O).

</details>

---

## Section 2 : READ - Lecture (Questions 6-11)

### Question 6
Quelle clause utilise-t-on pour détecter la fin de fichier en lecture séquentielle ?

- [ ] A) INVALID KEY
- [ ] B) AT END
- [ ] C) END-OF-FILE
- [ ] D) EOF

<details>
<summary>Réponse</summary>

**B** - La clause `AT END` détecte la fin de fichier lors d'une lecture séquentielle.

</details>

---

### Question 7
Quelle clause utilise-t-on pour détecter une clé non trouvée en accès direct ?

- [ ] A) AT END
- [ ] B) NOT FOUND
- [ ] C) INVALID KEY
- [ ] D) KEY ERROR

<details>
<summary>Réponse</summary>

**C** - La clause `INVALID KEY` gère les erreurs d'accès direct (clé non trouvée).

</details>

---

### Question 8
À quoi sert la clause INTO dans l'instruction READ ?

- [ ] A) Spécifier la clé de lecture
- [ ] B) Copier l'enregistrement lu dans une zone WORKING-STORAGE
- [ ] C) Définir le mode d'accès
- [ ] D) Indiquer la fin de fichier

<details>
<summary>Réponse</summary>

**B** - `READ fichier INTO ws-zone` copie l'enregistrement lu dans une zone de la WORKING-STORAGE SECTION.

</details>

---

### Question 9
Quelle instruction permet de lire l'enregistrement suivant en mode DYNAMIC ?

- [ ] A) READ fichier
- [ ] B) READ fichier NEXT
- [ ] C) READ fichier SEQUENTIAL
- [ ] D) READ fichier FORWARD

<details>
<summary>Réponse</summary>

**B** - `READ fichier NEXT` lit l'enregistrement suivant en mode d'accès DYNAMIC.

</details>

---

### Question 10
Pour lire un fichier séquentiel ESDS sur mainframe, quel préfixe utilise-t-on dans le ASSIGN TO ?

- [ ] A) DD-
- [ ] B) AS-
- [ ] C) ES-
- [ ] D) SEQ-

<details>
<summary>Réponse</summary>

**B** - Pour les Data Set ESDS, on utilise le préfixe `AS-` : `ASSIGN TO AS-DDNAME`.

</details>

---

### Question 11
Que signifie le FILE STATUS "10" après un READ ?

- [ ] A) Lecture réussie
- [ ] B) Fin de fichier (EOF)
- [ ] C) Enregistrement non trouvé
- [ ] D) Erreur de lecture

<details>
<summary>Réponse</summary>

**B** - Le code 10 indique la fin de fichier (End Of File).

</details>

---

## Section 3 : WRITE - Écriture (Questions 12-16)

### Question 12
Dans l'instruction WRITE, écrit-on le fichier ou l'enregistrement ?

- [ ] A) Le fichier
- [ ] B) L'enregistrement (niveau 01 du FD)
- [ ] C) Les deux sont possibles
- [ ] D) La zone WORKING-STORAGE

<details>
<summary>Réponse</summary>

**B** - On écrit l'**enregistrement** (niveau 01 du FD), pas le fichier : `WRITE ENR-CLIENT`.

</details>

---

### Question 13
À quoi sert la clause FROM dans l'instruction WRITE ?

- [ ] A) Spécifier le fichier source
- [ ] B) Écrire directement depuis une zone WORKING-STORAGE
- [ ] C) Définir la clé d'écriture
- [ ] D) Indiquer le mode d'ouverture

<details>
<summary>Réponse</summary>

**B** - `WRITE enr FROM ws-zone` copie depuis la WORKING-STORAGE avant d'écrire.

</details>

---

### Question 14
Dans quel(s) mode(s) d'ouverture peut-on utiliser WRITE sur un fichier séquentiel ?

- [ ] A) INPUT uniquement
- [ ] B) OUTPUT uniquement
- [ ] C) OUTPUT ou EXTEND
- [ ] D) I-O uniquement

<details>
<summary>Réponse</summary>

**C** - Pour un fichier séquentiel, WRITE nécessite OUTPUT (création) ou EXTEND (ajout en fin).

</details>

---

### Question 15
Que signifie le FILE STATUS "22" après un WRITE ?

- [ ] A) Écriture réussie
- [ ] B) Fin de fichier
- [ ] C) Clé en double
- [ ] D) Fichier plein

<details>
<summary>Réponse</summary>

**C** - Le code 22 indique une clé en double (INVALID KEY pour fichier indexé).

</details>

---

### Question 16
Pour un fichier indexé en mode OUTPUT, comment doivent être les clés des enregistrements écrits ?

- [ ] A) Dans n'importe quel ordre
- [ ] B) Dans l'ordre croissant
- [ ] C) Dans l'ordre décroissant
- [ ] D) Toutes identiques

<details>
<summary>Réponse</summary>

**B** - En mode OUTPUT sur fichier indexé, les clés doivent être écrites dans l'ordre croissant.

</details>

---

## Section 4 : REWRITE - Modification (Questions 17-20)

### Question 17
Quelle condition est obligatoire avant d'exécuter REWRITE ?

- [ ] A) OPEN en mode OUTPUT
- [ ] B) Un READ réussi sur l'enregistrement
- [ ] C) CLOSE du fichier
- [ ] D) START sur la clé

<details>
<summary>Réponse</summary>

**B** - Un REWRITE doit **toujours** être précédé d'un READ réussi sur le même enregistrement.

</details>

---

### Question 18
Quel mode d'ouverture est requis pour utiliser REWRITE ?

- [ ] A) INPUT
- [ ] B) OUTPUT
- [ ] C) I-O
- [ ] D) EXTEND

<details>
<summary>Réponse</summary>

**C** - REWRITE nécessite que le fichier soit ouvert en mode I-O (Input-Output).

</details>

---

### Question 19
Peut-on modifier la clé primaire d'un enregistrement avec REWRITE ?

- [ ] A) Oui, sans restriction
- [ ] B) Non, cela génère INVALID KEY
- [ ] C) Oui, mais uniquement en mode DYNAMIC
- [ ] D) Oui, si on utilise DELETE puis WRITE

<details>
<summary>Réponse</summary>

**B** - La clé primaire ne peut PAS être modifiée avec REWRITE (génère INVALID KEY). Les clés secondaires peuvent l'être.

</details>

---

### Question 20
Que signifie le FILE STATUS "43" après un REWRITE ?

- [ ] A) Modification réussie
- [ ] B) Pas de READ préalable
- [ ] C) Fichier non ouvert
- [ ] D) Clé en double

<details>
<summary>Réponse</summary>

**B** - Le code 43 indique qu'aucun READ n'a précédé le REWRITE.

</details>

---

## Section 5 : DELETE, START et CLOSE (Questions 21-25)

### Question 21
Sur quels types de fichiers peut-on utiliser DELETE ?

- [ ] A) Séquentiel uniquement
- [ ] B) Indexé et Relatif uniquement
- [ ] C) Tous les types
- [ ] D) Indexé uniquement

<details>
<summary>Réponse</summary>

**B** - DELETE n'est disponible que pour les fichiers INDEXED et RELATIVE. Les fichiers SEQUENTIAL ne supportent pas la suppression.

</details>

---

### Question 22
En mode d'accès RANDOM, faut-il faire un READ avant DELETE ?

- [ ] A) Oui, obligatoirement
- [ ] B) Non, il suffit de positionner la clé
- [ ] C) Oui, mais seulement pour les fichiers indexés
- [ ] D) Non, DELETE ne fonctionne pas en mode RANDOM

<details>
<summary>Réponse</summary>

**B** - En mode RANDOM, pas besoin de READ préalable. Il suffit de positionner la clé avant le DELETE.

</details>

---

### Question 23
À quoi sert l'instruction START ?

- [ ] A) Démarrer le programme
- [ ] B) Positionner le pointeur sur un enregistrement sans le lire
- [ ] C) Créer un nouveau fichier
- [ ] D) Lire le premier enregistrement

<details>
<summary>Réponse</summary>

**B** - START positionne le pointeur sur un enregistrement spécifique sans le lire, en préparation d'une lecture séquentielle.

</details>

---

### Question 24
Quelles conditions de positionnement sont valides pour START ?

- [ ] A) = et > uniquement
- [ ] B) =, >, >=, <, <=
- [ ] C) = uniquement
- [ ] D) > et < uniquement

<details>
<summary>Réponse</summary>

**B** - START accepte toutes les conditions : `=`, `>`, `>=`, `<`, `<=`.

</details>

---

### Question 25
Que fait la clause WITH LOCK sur un CLOSE ?

- [ ] A) Verrouille le fichier pour les autres programmes
- [ ] B) Empêche la réouverture du fichier dans le même programme
- [ ] C) Protège le fichier par mot de passe
- [ ] D) Compresse le fichier

<details>
<summary>Réponse</summary>

**B** - `CLOSE fichier WITH LOCK` empêche la réouverture du fichier dans le même programme (FILE STATUS 38 si tentative).

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
| [QCM Chapitre VII](qcm-07-gestion-fichiers.md) | [QCM Chapitre IX](qcm-09-sous-programmes.md) |

---
*Formation COBOL - M2i Formation*
