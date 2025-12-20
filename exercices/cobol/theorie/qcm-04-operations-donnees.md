# QCM - Chapitre IV : Opérations sur les Données

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Affectation MOVE (Questions 1-6)

### Question 1
Quelle instruction permet d'affecter une valeur à une variable en COBOL ?

- [ ] A) SET
- [ ] B) ASSIGN
- [ ] C) MOVE
- [ ] D) COPY

<details>
<summary>Réponse</summary>

**C** - L'instruction `MOVE` permet de charger une variable par une autre variable ou par une valeur.

</details>

---

### Question 2
Peut-on affecter une même valeur à plusieurs variables en une seule instruction MOVE ?

- [ ] A) Non, il faut plusieurs MOVE
- [ ] B) Oui, avec `MOVE valeur TO var1 var2 var3`
- [ ] C) Oui, avec `MOVE valeur TO var1, var2, var3`
- [ ] D) B et C sont correctes

<details>
<summary>Réponse</summary>

**D** - On peut lister plusieurs destinations après TO : `MOVE ZEROS TO VAR1 VAR2 VAR3` ou `MOVE ZEROS TO VAR1, VAR2, VAR3`.

</details>

---

### Question 3
Dans quel sens se fait le transfert d'une donnée alphanumérique ?

- [ ] A) De droite à gauche
- [ ] B) De gauche à droite
- [ ] C) Centré
- [ ] D) Selon la taille de la variable

<details>
<summary>Réponse</summary>

**B** - Le transfert alphanumérique se fait de gauche à droite, avec complément d'espaces à droite si nécessaire.

</details>

---

### Question 4
Que contient `NOM` après `MOVE 'ABCDEF' TO NOM` si `NOM PIC X(5)` ?

- [ ] A) `ABCDE`
- [ ] B) `BCDEF`
- [ ] C) `ABCDEF`
- [ ] D) Erreur de compilation

<details>
<summary>Réponse</summary>

**A** - La chaîne est tronquée à droite. Les caractères excédentaires ('F') sont perdus.

</details>

---

### Question 5
Que contient `PRIX` après `MOVE 12345 TO PRIX` si `PRIX PIC 9(4)` ?

- [ ] A) `1234`
- [ ] B) `2345`
- [ ] C) `12345`
- [ ] D) Erreur de compilation

<details>
<summary>Réponse</summary>

**B** - Les valeurs numériques sont tronquées à gauche. Le chiffre de poids fort ('1') est perdu.

</details>

---

### Question 6
À quoi sert `MOVE CORRESPONDING` ?

- [ ] A) Copier une variable vers toutes les autres
- [ ] B) Copier les champs de même nom entre deux groupes
- [ ] C) Copier uniquement les champs numériques
- [ ] D) Copier les champs correspondant à un masque

<details>
<summary>Réponse</summary>

**B** - `MOVE CORRESPONDING groupe-source TO groupe-dest` copie automatiquement les champs ayant le même nom.

</details>

---

## Section 2 : ADD et SUBTRACT (Questions 7-12)

### Question 7
Que fait `ADD 100 TO VAR1` si VAR1 contient 50 ?

- [ ] A) VAR1 = 100
- [ ] B) VAR1 = 150
- [ ] C) VAR1 = 50
- [ ] D) Erreur

<details>
<summary>Réponse</summary>

**B** - `ADD a TO b` équivaut à b = b + a. Donc VAR1 = 50 + 100 = 150.

</details>

---

### Question 8
Quelle est la différence entre `ADD a TO b` et `ADD a TO b GIVING c` ?

- [ ] A) Aucune différence
- [ ] B) Avec GIVING, le résultat va dans c et b reste inchangé
- [ ] C) Avec GIVING, le résultat va dans b et c
- [ ] D) GIVING additionne aussi c

<details>
<summary>Réponse</summary>

**B** - Avec GIVING, le résultat est stocké dans c, et b conserve sa valeur originale.

</details>

---

### Question 9
Que fait `SUBTRACT 10 20 15 FROM VAR1` si VAR1 = 100 ?

- [ ] A) VAR1 = 55
- [ ] B) VAR1 = 45
- [ ] C) VAR1 = 85
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**A** - Les valeurs avant FROM sont additionnées (10+20+15=45), puis déduites de VAR1. Donc 100-45=55.

</details>

---

### Question 10
Que se passe-t-il avec `SUBTRACT 20 FROM VAR1 VAR2 VAR3` si VAR1=50, VAR2=100, VAR3=80 ?

- [ ] A) Seule VAR1 est modifiée
- [ ] B) Les trois variables sont diminuées de 20
- [ ] C) Erreur de syntaxe
- [ ] D) Le total est déduit de VAR3 uniquement

<details>
<summary>Réponse</summary>

**B** - Sans GIVING, chaque variable après FROM est modifiée. Résultat : VAR1=30, VAR2=80, VAR3=60.

</details>

---

### Question 11
Comment obtenir le résultat de VAR1 + VAR2 dans VAR3 sans modifier VAR1 et VAR2 ?

- [ ] A) `ADD VAR1 VAR2 TO VAR3`
- [ ] B) `ADD VAR1 VAR2 GIVING VAR3`
- [ ] C) `ADD VAR1 TO VAR2 GIVING VAR3`
- [ ] D) B et C sont correctes

<details>
<summary>Réponse</summary>

**D** - Les deux syntaxes placent le résultat dans VAR3 sans modifier les opérandes.

</details>

---

### Question 12
Que fait l'option ROUNDED ?

- [ ] A) Arrondit à l'entier inférieur
- [ ] B) Arrondit arithmétiquement (au plus proche)
- [ ] C) Tronque les décimales
- [ ] D) Arrondit à l'entier supérieur

<details>
<summary>Réponse</summary>

**B** - ROUNDED effectue un arrondi arithmétique : si la partie tronquée est ≥ 0.5, on arrondit vers le haut.

</details>

---

## Section 3 : MULTIPLY et DIVIDE (Questions 13-18)

### Question 13
Que fait `MULTIPLY VAR1 BY VAR2` ?

- [ ] A) VAR1 = VAR1 * VAR2
- [ ] B) VAR2 = VAR1 * VAR2
- [ ] C) Les deux sont modifiées
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**B** - Avec MULTIPLY a BY b, c'est b (après BY) qui reçoit le résultat. VAR2 = VAR1 * VAR2.

</details>

---

### Question 14
Quelle est la différence entre `DIVIDE a INTO b` et `DIVIDE a BY b GIVING c` ?

- [ ] A) INTO : b = b / a ; BY : c = a / b
- [ ] B) INTO : b = a / b ; BY : c = b / a
- [ ] C) Aucune différence
- [ ] D) BY n'existe pas

<details>
<summary>Réponse</summary>

**A** - Avec INTO, le diviseur est à gauche (b = b / a). Avec BY, le diviseur est à droite (c = a / b), plus naturel.

</details>

---

### Question 15
Comment récupérer le reste d'une division entière ?

- [ ] A) `DIVIDE a BY b GIVING c REST r`
- [ ] B) `DIVIDE a BY b GIVING c REMAINDER r`
- [ ] C) `DIVIDE a BY b GIVING c MODULO r`
- [ ] D) Utiliser une deuxième instruction

<details>
<summary>Réponse</summary>

**B** - La clause REMAINDER permet de récupérer le reste de la division entière.

</details>

---

### Question 16
Que vaut VAR3 après `DIVIDE 10 INTO 25 GIVING VAR3 REMAINDER VAR4` ?

- [ ] A) VAR3 = 2, VAR4 = 5
- [ ] B) VAR3 = 2.5, VAR4 = 0
- [ ] C) VAR3 = 0.4, VAR4 = 0
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**A** - 25 / 10 = 2 reste 5. Le quotient entier va dans VAR3, le reste dans VAR4.

</details>

---

### Question 17
Peut-on multiplier une variable par plusieurs autres en une seule instruction ?

- [ ] A) Non, une seule multiplication par instruction
- [ ] B) Oui, avec `MULTIPLY a BY b c d`
- [ ] C) Oui, avec `MULTIPLY a b c GIVING d`
- [ ] D) B et C sont incorrectes

<details>
<summary>Réponse</summary>

**B** - `MULTIPLY a BY b c d` multiplie chaque variable (b, c, d) par a et stocke le résultat dans chacune.

</details>

---

### Question 18
Quel est le problème de `DIVIDE 0 INTO VAR1` ?

- [ ] A) Aucun problème
- [ ] B) Division par zéro, erreur à l'exécution
- [ ] C) VAR1 devient 0
- [ ] D) Erreur de compilation

<details>
<summary>Réponse</summary>

**B** - Division par zéro provoque une erreur d'exécution. Utilisez ON SIZE ERROR pour la gérer.

</details>

---

## Section 4 : COMPUTE (Questions 19-23)

### Question 19
Quel est l'avantage principal de COMPUTE ?

- [ ] A) Plus rapide à exécuter
- [ ] B) Permet d'écrire des expressions complexes en une instruction
- [ ] C) Oblige à utiliser ROUNDED
- [ ] D) Empêche les erreurs de dépassement

<details>
<summary>Réponse</summary>

**B** - COMPUTE permet d'écrire des expressions arithmétiques complexes avec plusieurs opérations.

</details>

---

### Question 20
Quel opérateur représente la puissance en COBOL ?

- [ ] A) `^`
- [ ] B) `**`
- [ ] C) `POW`
- [ ] D) `EXP`

<details>
<summary>Réponse</summary>

**B** - L'opérateur `**` représente la puissance. Exemple : `A ** 2` = A².

</details>

---

### Question 21
Quelle est la priorité des opérateurs dans COMPUTE ?

- [ ] A) + - * / ** (gauche à droite)
- [ ] B) ** puis * / puis + -
- [ ] C) Tous ont la même priorité
- [ ] D) * / puis ** puis + -

<details>
<summary>Réponse</summary>

**B** - Priorité : 1) Parenthèses 2) ** (puissance) 3) * et / 4) + et -

</details>

---

### Question 22
Que vaut VAR2 après `COMPUTE VAR2 = 10 + 3 * 2` ?

- [ ] A) 26
- [ ] B) 16
- [ ] C) 13
- [ ] D) 32

<details>
<summary>Réponse</summary>

**B** - La multiplication a priorité sur l'addition : 3*2=6, puis 10+6=16.

</details>

---

### Question 23
Que fait ON SIZE ERROR dans un COMPUTE ?

- [ ] A) Empêche le calcul si le résultat est trop grand
- [ ] B) Exécute des instructions si le résultat dépasse la capacité
- [ ] C) Arrondit automatiquement
- [ ] D) Affiche un message d'erreur système

<details>
<summary>Réponse</summary>

**B** - ON SIZE ERROR intercepte le dépassement de capacité et exécute les instructions spécifiées.

</details>

---

## Section 5 : Cas pratiques (Questions 24-25)

### Question 24
Quelle instruction calcule le TTC à partir du HT et d'un taux de TVA en % ?

- [ ] A) `COMPUTE TTC = HT + TVA`
- [ ] B) `COMPUTE TTC = HT * (1 + TVA / 100)`
- [ ] C) `MULTIPLY HT BY TVA GIVING TTC`
- [ ] D) `ADD HT TVA GIVING TTC`

<details>
<summary>Réponse</summary>

**B** - TTC = HT × (1 + TVA/100). Par exemple si HT=100 et TVA=20%, TTC = 100 × 1.20 = 120.

</details>

---

### Question 25
Comment calculer un prix avec remise : `PRIX = PRIX-UNIT × QTE × (1 - REMISE/100)` ?

- [ ] A) Trois instructions MULTIPLY
- [ ] B) `COMPUTE PRIX = PRIX-UNIT * QTE * (1 - REMISE / 100)`
- [ ] C) `MULTIPLY PRIX-UNIT BY QTE BY (1 - REMISE / 100) GIVING PRIX`
- [ ] D) Impossible en une seule instruction

<details>
<summary>Réponse</summary>

**B** - COMPUTE permet d'exprimer cette formule directement. La syntaxe C n'est pas valide.

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
| [QCM Chapitre III](qcm-03-declaration-variables.md) | [QCM Chapitre V](qcm-05-traitement-conditionnel.md) |

---
*Formation COBOL - M2i Formation*
