# QCM - Chapitre III : Déclaration des Variables

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Définition des Niveaux (Questions 1-8)

### Question 1
Quel niveau représente un enregistrement principal (racine) en COBOL ?

- [ ] A) Niveau 02
- [ ] B) Niveau 01
- [ ] C) Niveau 77
- [ ] D) Niveau 88

<details>
<summary>Réponse</summary>

**B** - Le niveau 01 représente un enregistrement principal (racine) et doit commencer en Zone A.

</details>

---

### Question 2
Quel niveau est utilisé pour déclarer une variable indépendante (non hiérarchique) ?

- [ ] A) 01
- [ ] B) 05
- [ ] C) 66
- [ ] D) 77

<details>
<summary>Réponse</summary>

**D** - Le niveau 77 déclare une variable élémentaire indépendante, sans hiérarchie. Elle doit commencer en Zone A.

</details>

---

### Question 3
À quoi sert le niveau 88 ?

- [ ] A) Déclarer un sous-groupe
- [ ] B) Renommer une zone mémoire
- [ ] C) Définir une condition (valeur booléenne)
- [ ] D) Créer une variable d'édition

<details>
<summary>Réponse</summary>

**C** - Le niveau 88 définit une condition associée à une variable. Il permet de tester si une variable contient certaines valeurs.

</details>

---

### Question 4
À quoi sert le niveau 66 (RENAMES) ?

- [ ] A) Supprimer une variable
- [ ] B) Créer un alias sur une partie d'enregistrement
- [ ] C) Déclarer une constante
- [ ] D) Définir un tableau

<details>
<summary>Réponse</summary>

**B** - Le niveau 66 avec RENAMES crée une vue alternative (alias) sur une ou plusieurs zones consécutives d'un enregistrement.

</details>

---

### Question 5
Quel mot-clé désigne une zone sans nom (non référençable) ?

- [ ] A) BLANK
- [ ] B) EMPTY
- [ ] C) FILLER
- [ ] D) NULL

<details>
<summary>Réponse</summary>

**C** - FILLER désigne une zone sans nom, utilisée pour le remplissage ou les zones ignorées dans un enregistrement.

</details>

---

### Question 6
Dans la déclaration suivante, quelle affirmation est VRAIE ?

```cobol
       01  WS-STATUT        PIC X.
           88  ACTIF        VALUE 'A'.
           88  INACTIF      VALUE 'I'.
```

- [ ] A) ACTIF et INACTIF sont des variables de type caractère
- [ ] B) SET ACTIF TO TRUE affecte 'A' à WS-STATUT
- [ ] C) IF ACTIF teste si WS-STATUT vaut 'A'
- [ ] D) B et C sont correctes

<details>
<summary>Réponse</summary>

**D** - Le niveau 88 définit des conditions. `SET ACTIF TO TRUE` affecte 'A' à WS-STATUT, et `IF ACTIF` teste si WS-STATUT = 'A'.

</details>

---

### Question 7
Comment déclarer un niveau 88 avec plusieurs valeurs possibles ?

- [ ] A) `88 VALIDE VALUE 'A' OR 'B' OR 'C'.`
- [ ] B) `88 VALIDE VALUE 'A' 'B' 'C'.`
- [ ] C) `88 VALIDE VALUE 'A', 'B', 'C'.`
- [ ] D) B et C sont correctes

<details>
<summary>Réponse</summary>

**D** - Les valeurs peuvent être listées avec ou sans virgules : `VALUE 'A' 'B' 'C'` ou `VALUE 'A', 'B', 'C'`.

</details>

---

### Question 8
Comment déclarer une plage de valeurs avec le niveau 88 ?

- [ ] A) `88 PASSABLE VALUE 10 TO 12.`
- [ ] B) `88 PASSABLE VALUE 10 THRU 12.`
- [ ] C) `88 PASSABLE VALUE 10..12.`
- [ ] D) `88 PASSABLE VALUE RANGE 10 12.`

<details>
<summary>Réponse</summary>

**B** - On utilise THRU (ou THROUGH) : `VALUE 10 THRU 12` couvre les valeurs 10, 11 et 12.

</details>

---

## Section 2 : Clause PICTURE (Questions 9-14)

### Question 9
Que représente le symbole `9` dans une clause PICTURE ?

- [ ] A) Une lettre
- [ ] B) Un chiffre (0-9)
- [ ] C) Un espace
- [ ] D) Un caractère quelconque

<details>
<summary>Réponse</summary>

**B** - Le symbole `9` représente un chiffre (0-9) dans une variable numérique.

</details>

---

### Question 10
Que représente le symbole `X` dans une clause PICTURE ?

- [ ] A) Uniquement des lettres
- [ ] B) Uniquement des chiffres
- [ ] C) Tout caractère (alphanumérique)
- [ ] D) Un caractère hexadécimal

<details>
<summary>Réponse</summary>

**C** - Le symbole `X` représente n'importe quel caractère (lettre, chiffre, caractère spécial, espace).

</details>

---

### Question 11
Quelle PICTURE déclare un nombre signé de 5 chiffres avec 2 décimales ?

- [ ] A) `PIC 9(5).99`
- [ ] B) `PIC S9(5)V99`
- [ ] C) `PIC 9(5),99`
- [ ] D) `PIC +9(5)V99`

<details>
<summary>Réponse</summary>

**B** - `S` indique un nombre signé, `V` est la virgule décimale virtuelle (implicite, pas stockée).

</details>

---

### Question 12
Quelle est la différence entre `V` et `.` dans une PICTURE ?

- [ ] A) Aucune différence
- [ ] B) `V` est virtuelle (pas stockée), `.` est réelle (stockée)
- [ ] C) `V` est pour les entiers, `.` pour les décimaux
- [ ] D) `V` est pour le stockage, `.` est interdit

<details>
<summary>Réponse</summary>

**B** - `V` est une virgule virtuelle (position décimale implicite, pas stockée). Le point `.` est un caractère réel stocké (utilisé en édition).

</details>

---

### Question 13
Combien d'octets occupe une variable `PIC 9(5) USAGE DISPLAY` ?

- [ ] A) 2 octets
- [ ] B) 3 octets
- [ ] C) 4 octets
- [ ] D) 5 octets

<details>
<summary>Réponse</summary>

**D** - En USAGE DISPLAY (par défaut), chaque caractère occupe 1 octet. Donc 5 chiffres = 5 octets.

</details>

---

### Question 14
Quel USAGE permet de stocker les nombres en format décimal condensé (packed) ?

- [ ] A) DISPLAY
- [ ] B) BINARY
- [ ] C) COMP-3
- [ ] D) COMP-1

<details>
<summary>Réponse</summary>

**C** - COMP-3 (ou PACKED-DECIMAL) stocke 2 chiffres par octet, plus un demi-octet pour le signe.

</details>

---

## Section 3 : Clause VALUE (Questions 15-18)

### Question 15
Quelle constante figurative remplit une variable d'espaces ?

- [ ] A) ZEROS
- [ ] B) SPACES
- [ ] C) BLANKS
- [ ] D) EMPTY

<details>
<summary>Réponse</summary>

**B** - SPACE ou SPACES remplit une variable avec des caractères espace.

</details>

---

### Question 16
Comment remplir une variable de 50 caractères avec des tirets ?

- [ ] A) `VALUE '-' TIMES 50.`
- [ ] B) `VALUE REPEAT '-'.`
- [ ] C) `VALUE ALL '-'.`
- [ ] D) `VALUE 50 OF '-'.`

<details>
<summary>Réponse</summary>

**C** - `VALUE ALL '-'` remplit toute la variable avec le caractère spécifié (ici des tirets).

</details>

---

### Question 17
Quelle constante figurative représente la valeur hexadécimale maximale (xFF) ?

- [ ] A) MAX-VALUE
- [ ] B) HIGH-VALUE
- [ ] C) TOP-VALUE
- [ ] D) FULL-VALUE

<details>
<summary>Réponse</summary>

**B** - HIGH-VALUE (ou HIGH-VALUES) représente la valeur hexadécimale xFF (maximum).

</details>

---

### Question 18
Peut-on utiliser VALUE avec un groupe (niveau 01 sans PIC) ?

- [ ] A) Non, c'est interdit
- [ ] B) Oui, avec une valeur alphanumérique
- [ ] C) Oui, avec ZEROS uniquement
- [ ] D) Oui, mais la valeur doit correspondre à la taille totale

<details>
<summary>Réponse</summary>

**B et D** - On peut initialiser un groupe avec une valeur alphanumérique qui sera distribuée sur les sous-éléments.

</details>

---

## Section 4 : Édition des valeurs numériques (Questions 19-23)

### Question 19
Que fait le symbole `Z` dans une PICTURE d'édition ?

- [ ] A) Ajoute des zéros à gauche
- [ ] B) Remplace les zéros non significatifs par des espaces
- [ ] C) Zone réservée
- [ ] D) Zéro obligatoire

<details>
<summary>Réponse</summary>

**B** - `Z` supprime les zéros non significatifs et les remplace par des espaces.

</details>

---

### Question 20
Quel est le résultat de `MOVE 42 TO WS-VAR` si `WS-VAR PIC ***9` ?

- [ ] A) `0042`
- [ ] B) `  42`
- [ ] C) `**42`
- [ ] D) `***42`

<details>
<summary>Réponse</summary>

**C** - `*` remplace les zéros non significatifs par des astérisques. Résultat : `**42`.

</details>

---

### Question 21
Quelle PICTURE permet d'afficher un montant avec séparateur de milliers et symbole € flottant ?

- [ ] A) `€ZZZ,ZZ9.99`
- [ ] B) `€€€,€€9.99`
- [ ] C) `€€€€€€9.99`
- [ ] D) `ZZZ,ZZ9.99€`

<details>
<summary>Réponse</summary>

**B** - Les symboles répétés (`€€€`) sont "flottants" : le € se place juste avant le premier chiffre significatif.

</details>

---

### Question 22
Que signifie `CR` dans une PICTURE d'édition comme `ZZ9CR` ?

- [ ] A) Carriage Return
- [ ] B) Crédit - affiché si la valeur est négative
- [ ] C) Correction
- [ ] D) Caractère de remplacement

<details>
<summary>Réponse</summary>

**B** - `CR` (Crédit) s'affiche uniquement si la valeur est négative. `DB` (Débit) fonctionne de la même façon.

</details>

---

### Question 23
Que fait la clause `BLANK WHEN ZERO` ?

- [ ] A) Remplace les espaces par des zéros
- [ ] B) Affiche des espaces si la valeur est zéro
- [ ] C) Interdit les valeurs nulles
- [ ] D) Supprime les décimales nulles

<details>
<summary>Réponse</summary>

**B** - `BLANK WHEN ZERO` affiche uniquement des espaces si la valeur entière est zéro.

</details>

---

## Section 5 : REDEFINES (Questions 24-25)

### Question 24
Quelle est la règle principale du REDEFINES ?

- [ ] A) Il doit être du même niveau que la variable redéfinie
- [ ] B) Il doit apparaître juste après la variable redéfinie
- [ ] C) La taille doit être ≤ à la variable redéfinie
- [ ] D) Toutes les réponses ci-dessus

<details>
<summary>Réponse</summary>

**D** - Le REDEFINES doit : (1) être du même niveau, (2) apparaître juste après, (3) avoir une taille ≤ à l'original.

</details>

---

### Question 25
Quel est un cas d'usage typique du REDEFINES ?

- [ ] A) Décomposer une date AAAAMMJJ en ANNEE, MOIS, JOUR
- [ ] B) Créer plusieurs vues sur un enregistrement multi-format
- [ ] C) Convertir une zone numérique en alphanumérique
- [ ] D) Toutes les réponses ci-dessus

<details>
<summary>Réponse</summary>

**D** - REDEFINES permet de créer plusieurs "vues" sur la même zone mémoire : décomposition de dates, enregistrements multi-formats, conversion de types.

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
| [QCM Chapitre II](qcm-02-ispf-commandes.md) | [QCM Chapitre IV](qcm-04-operations-donnees.md) |

---
*Formation COBOL - M2i Formation*
