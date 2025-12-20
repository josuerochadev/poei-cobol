# QCM - Chapitre XI : Tri Interne (SORT / MERGE)

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Concepts fondamentaux (Questions 1-5)

### Question 1
Combien de fichiers sont nécessaires pour réaliser un tri SORT en COBOL ?

- [ ] A) Un seul fichier
- [ ] B) Deux fichiers
- [ ] C) Trois fichiers
- [ ] D) Quatre fichiers

<details>
<summary>Réponse</summary>

**C** - Trois fichiers sont nécessaires : le fichier d'entrée (Input), le fichier de travail (Work) et le fichier de sortie (Output).

</details>

---

### Question 2
Comment déclare-t-on le fichier de travail utilisé par SORT ?

- [ ] A) Avec FD (File Description)
- [ ] B) Avec SD (Sort Description)
- [ ] C) Avec WS (Working-Storage)
- [ ] D) Avec TD (Temporary Description)

<details>
<summary>Réponse</summary>

**B** - Le fichier de travail SORT doit être déclaré avec **SD** (Sort Description) et non FD.

</details>

---

### Question 3
Que se passe-t-il pour le fichier de travail à la fin de l'opération SORT ?

- [ ] A) Il est conservé pour réutilisation
- [ ] B) Il est renommé avec un suffixe .BAK
- [ ] C) Il est automatiquement supprimé
- [ ] D) Il doit être supprimé manuellement

<details>
<summary>Réponse</summary>

**C** - Le fichier de travail est automatiquement supprimé à la fin de l'opération SORT.

</details>

---

### Question 4
Quelles sont les deux instructions principales pour trier et fusionner des fichiers ?

- [ ] A) ORDER et COMBINE
- [ ] B) SORT et MERGE
- [ ] C) ARRANGE et JOIN
- [ ] D) CLASSIFY et UNITE

<details>
<summary>Réponse</summary>

**B** - Les deux instructions principales sont **SORT** (tri) et **MERGE** (fusion).

</details>

---

### Question 5
Quelle clause peut être utilisée dans la déclaration SD ?

- [ ] A) RECORDING MODE IS F
- [ ] B) BLOCK CONTAINS 0 RECORDS
- [ ] C) Les clés de tri uniquement
- [ ] D) LABEL RECORD IS STANDARD

<details>
<summary>Réponse</summary>

**C** - La déclaration SD ne prend pas les clauses habituelles de FD (RECORDING MODE, BLOCK CONTAINS, etc.). On y définit uniquement la structure avec les clés de tri.

</details>

---

## Section 2 : Instruction SORT (Questions 6-10)

### Question 6
Quelle clause utilise-t-on pour trier par ordre croissant ?

- [ ] A) INCREASING KEY
- [ ] B) ASCENDING KEY
- [ ] C) GROWING KEY
- [ ] D) UP KEY

<details>
<summary>Réponse</summary>

**B** - La clause **ASCENDING KEY** trie les enregistrements par ordre croissant (A→Z, 0→9).

</details>

---

### Question 7
Quand plusieurs clés sont spécifiées dans SORT, laquelle est la plus significative ?

- [ ] A) La dernière clé spécifiée
- [ ] B) La clé la plus longue
- [ ] C) La première clé (la plus à gauche)
- [ ] D) La clé numérique

<details>
<summary>Réponse</summary>

**C** - La signification des clés diminue de gauche à droite. La première clé spécifiée est la plus significative.

</details>

---

### Question 8
À quoi sert la clause WITH DUPLICATES IN ORDER ?

- [ ] A) Supprimer les enregistrements en double
- [ ] B) Conserver l'ordre original des enregistrements ayant la même clé
- [ ] C) Créer une copie des doublons
- [ ] D) Compter le nombre de doublons

<details>
<summary>Réponse</summary>

**B** - WITH DUPLICATES IN ORDER préserve l'ordre relatif des enregistrements ayant la même valeur de clé.

</details>

---

### Question 9
Que font les clauses USING et GIVING dans un SORT simple ?

- [ ] A) USING = fichier de travail, GIVING = fichier résultat
- [ ] B) USING = fichier d'entrée (ouvert automatiquement), GIVING = fichier de sortie (ouvert automatiquement)
- [ ] C) USING = paramètres de tri, GIVING = code retour
- [ ] D) USING = clé de tri, GIVING = ordre de tri

<details>
<summary>Réponse</summary>

**B** - USING spécifie le fichier d'entrée et GIVING le fichier de sortie. Ces fichiers sont automatiquement ouverts, lus/écrits et fermés par le SORT.

</details>

---

### Question 10
Quels types de données sont autorisés pour les clés de tri ?

- [ ] A) Alphabétiques uniquement
- [ ] B) Numériques uniquement
- [ ] C) Alphabétiques, alphanumériques, numériques et virgule flottante
- [ ] D) Uniquement les PIC X et PIC 9

<details>
<summary>Réponse</summary>

**C** - Les clés de tri peuvent être alphabétiques, alphanumériques, numériques, virgule flottante ou édition numérique.

</details>

---

## Section 3 : INPUT/OUTPUT PROCEDURE (Questions 11-17)

### Question 11
À quoi sert l'INPUT PROCEDURE dans un SORT ?

- [ ] A) À lire le fichier d'entrée automatiquement
- [ ] B) À traiter les enregistrements AVANT le tri (filtrage, transformation)
- [ ] C) À afficher les enregistrements d'entrée
- [ ] D) À valider la structure du fichier

<details>
<summary>Réponse</summary>

**B** - L'INPUT PROCEDURE permet de traiter les enregistrements **avant** le tri, par exemple pour filtrer ou transformer les données.

</details>

---

### Question 12
Quelle instruction utilise-t-on dans l'INPUT PROCEDURE pour envoyer un enregistrement au tri ?

- [ ] A) WRITE
- [ ] B) SEND
- [ ] C) RELEASE
- [ ] D) OUTPUT

<details>
<summary>Réponse</summary>

**C** - L'instruction **RELEASE** envoie un enregistrement au tri depuis l'INPUT PROCEDURE.

</details>

---

### Question 13
À quoi sert l'OUTPUT PROCEDURE dans un SORT ?

- [ ] A) À écrire le fichier de sortie automatiquement
- [ ] B) À traiter les enregistrements APRÈS le tri (agrégation, édition)
- [ ] C) À afficher les résultats du tri
- [ ] D) À valider le résultat du tri

<details>
<summary>Réponse</summary>

**B** - L'OUTPUT PROCEDURE permet de traiter les enregistrements **après** le tri, par exemple pour générer des rapports ou des sous-totaux.

</details>

---

### Question 14
Quelle instruction utilise-t-on dans l'OUTPUT PROCEDURE pour récupérer un enregistrement trié ?

- [ ] A) READ
- [ ] B) RECEIVE
- [ ] C) GET
- [ ] D) RETURN

<details>
<summary>Réponse</summary>

**D** - L'instruction **RETURN** récupère les enregistrements triés dans l'OUTPUT PROCEDURE, similaire à READ pour un fichier normal.

</details>

---

### Question 15
Peut-on combiner INPUT PROCEDURE et OUTPUT PROCEDURE dans un même SORT ?

- [ ] A) Non, c'est mutuellement exclusif
- [ ] B) Oui, on peut utiliser les deux ensemble
- [ ] C) Oui, mais seulement avec MERGE
- [ ] D) Non, une erreur de compilation se produit

<details>
<summary>Réponse</summary>

**B** - On peut utiliser les deux procédures ensemble : INPUT PROCEDURE pour le filtrage avant tri, OUTPUT PROCEDURE pour l'édition après tri.

</details>

---

### Question 16
Dans quelle section peut-on utiliser l'instruction RELEASE ?

- [ ] A) N'importe où dans PROCEDURE DIVISION
- [ ] B) Uniquement dans l'INPUT PROCEDURE
- [ ] C) Uniquement dans l'OUTPUT PROCEDURE
- [ ] D) Dans INPUT et OUTPUT PROCEDURE

<details>
<summary>Réponse</summary>

**B** - L'instruction RELEASE ne peut être utilisée que dans la plage d'une **INPUT PROCEDURE** associée à une instruction SORT.

</details>

---

### Question 17
Quelle clause de RETURN indique la fin du fichier trié ?

- [ ] A) END-OF-FILE
- [ ] B) AT END
- [ ] C) WHEN FINISHED
- [ ] D) ON COMPLETE

<details>
<summary>Réponse</summary>

**B** - La clause **AT END** dans RETURN est déclenchée quand tous les enregistrements triés ont été récupérés.

</details>

---

## Section 4 : Instruction MERGE (Questions 18-22)

### Question 18
Quelle est la différence principale entre SORT et MERGE ?

- [ ] A) SORT est plus rapide
- [ ] B) MERGE nécessite des fichiers d'entrée déjà triés
- [ ] C) MERGE ne peut traiter qu'un seul fichier
- [ ] D) SORT ne peut pas utiliser de procédures

<details>
<summary>Réponse</summary>

**B** - MERGE nécessite que tous les fichiers d'entrée soient **déjà triés** sur les clés de fusion, contrairement à SORT.

</details>

---

### Question 19
Combien de fichiers d'entrée minimum sont nécessaires pour MERGE ?

- [ ] A) Un seul
- [ ] B) Deux
- [ ] C) Trois
- [ ] D) Quatre

<details>
<summary>Réponse</summary>

**B** - MERGE nécessite au minimum **deux** fichiers d'entrée à fusionner.

</details>

---

### Question 20
MERGE peut-il utiliser une INPUT PROCEDURE ?

- [ ] A) Oui, comme SORT
- [ ] B) Non, uniquement OUTPUT PROCEDURE
- [ ] C) Oui, mais uniquement pour le premier fichier
- [ ] D) Oui, mais c'est optionnel

<details>
<summary>Réponse</summary>

**B** - MERGE n'a pas d'INPUT PROCEDURE car les fichiers doivent être pré-triés. Seule l'OUTPUT PROCEDURE est disponible.

</details>

---

### Question 21
Comment les fichiers d'entrée doivent-ils être préparés avant un MERGE ?

- [ ] A) Ils doivent être vides
- [ ] B) Ils doivent être triés sur les mêmes clés que le MERGE
- [ ] C) Ils doivent être de taille identique
- [ ] D) Ils doivent avoir des clés différentes

<details>
<summary>Réponse</summary>

**B** - Tous les fichiers d'entrée doivent être des fichiers séquentiels **déjà triés** sur les mêmes clés que celles spécifiées dans le MERGE.

</details>

---

### Question 22
Quel est l'usage typique de MERGE ?

- [ ] A) Trier des données non ordonnées
- [ ] B) Combiner des fichiers déjà triés provenant de sources différentes
- [ ] C) Supprimer des enregistrements en double
- [ ] D) Convertir des fichiers VSAM en séquentiels

<details>
<summary>Réponse</summary>

**B** - MERGE est utilisé pour **combiner des fichiers déjà triés**, par exemple des fichiers de différentes agences ou régions.

</details>

---

## Section 5 : SORT-RETURN et bonnes pratiques (Questions 23-25)

### Question 23
Que contient la variable SORT-RETURN après une opération SORT ou MERGE ?

- [ ] A) Le nombre d'enregistrements triés
- [ ] B) Le code retour de l'opération (0 = succès, 16 = erreur)
- [ ] C) La taille du fichier de sortie
- [ ] D) Le temps d'exécution

<details>
<summary>Réponse</summary>

**B** - SORT-RETURN contient le code retour : **0** si l'opération a réussi, **16** en cas d'erreur.

</details>

---

### Question 24
Les variables KEY peuvent-elles contenir une clause OCCURS ?

- [ ] A) Oui, sans restriction
- [ ] B) Non, c'est interdit
- [ ] C) Oui, mais seulement OCCURS DEPENDING ON
- [ ] D) Oui, avec un maximum de 3 occurrences

<details>
<summary>Réponse</summary>

**B** - Les variables KEY ne doivent pas contenir de clause OCCURS ni être subordonnées à un élément avec OCCURS.

</details>

---

### Question 25
Faut-il ouvrir et fermer les fichiers spécifiés dans USING/GIVING ?

- [ ] A) Oui, ils doivent être ouverts avant le SORT
- [ ] B) Non, c'est automatique
- [ ] C) Oui pour USING, non pour GIVING
- [ ] D) Non pour USING, oui pour GIVING

<details>
<summary>Réponse</summary>

**B** - Les fichiers spécifiés dans USING et GIVING sont **automatiquement** ouverts, traités et fermés par l'instruction SORT.

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
| [QCM Chapitre X](qcm-10-traitement-fichiers.md) | [QCM Chapitre XII](qcm-12-fichier-impression.md) |

---
*Formation COBOL - M2i Formation*
