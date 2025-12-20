# QCM - Chapitre V : Traitement Conditionnel

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : IF ... THEN ... ELSE ... END-IF (Questions 1-10)

### Question 1
Quel mot-clé est facultatif dans une instruction IF ?

- [ ] A) IF
- [ ] B) THEN
- [ ] C) END-IF
- [ ] D) ELSE

<details>
<summary>Réponse</summary>

**B** - Le mot-clé `THEN` est facultatif. `IF condition instruction END-IF` est valide.

</details>

---

### Question 2
Comment terminer correctement un bloc IF en COBOL-85 ?

- [ ] A) Avec un point (.)
- [ ] B) Avec END-IF
- [ ] C) Les deux sont valides
- [ ] D) Avec ENDIF (sans tiret)

<details>
<summary>Réponse</summary>

**C** - Les deux sont valides, mais END-IF est recommandé pour les IF imbriqués et la lisibilité.

</details>

---

### Question 3
Que fait l'instruction `NEXT SENTENCE` ?

- [ ] A) Passe à la ligne suivante du code
- [ ] B) Transfère le contrôle après le point (.) le plus proche
- [ ] C) Saute au paragraphe suivant
- [ ] D) Termine le programme

<details>
<summary>Réponse</summary>

**B** - `NEXT SENTENCE` transfère le contrôle à l'instruction qui suit le point le plus proche.

</details>

---

### Question 4
Quel opérateur permet de tester l'égalité en COBOL ?

- [ ] A) `==`
- [ ] B) `=` ou `EQUAL TO`
- [ ] C) `EQ`
- [ ] D) `:=`

<details>
<summary>Réponse</summary>

**B** - En COBOL, on utilise `=` ou `IS EQUAL TO` pour tester l'égalité.

</details>

---

### Question 5
Quelle condition teste si une variable contient uniquement des lettres et espaces ?

- [ ] A) `IS LETTER`
- [ ] B) `IS ALPHABETIC`
- [ ] C) `IS ALPHA`
- [ ] D) `IS TEXT`

<details>
<summary>Réponse</summary>

**B** - `IS ALPHABETIC` teste si la variable contient uniquement des lettres (A-Z, a-z) et espaces.

</details>

---

### Question 6
Comment tester si une variable numérique est négative ?

- [ ] A) `IF VAR < 0`
- [ ] B) `IF VAR IS NEGATIVE`
- [ ] C) `IF VAR IS MINUS`
- [ ] D) A et B sont correctes

<details>
<summary>Réponse</summary>

**D** - Les deux syntaxes sont valides : `IF VAR < 0` et `IF VAR IS NEGATIVE`.

</details>

---

### Question 7
Quelle est la différence entre `ALPHABETIC-LOWER` et `ALPHABETIC-UPPER` ?

- [ ] A) Aucune différence
- [ ] B) LOWER = minuscules+espaces, UPPER = majuscules+espaces
- [ ] C) LOWER = 0-9, UPPER = A-Z
- [ ] D) Ces conditions n'existent pas

<details>
<summary>Réponse</summary>

**B** - `ALPHABETIC-LOWER` accepte minuscules+espaces, `ALPHABETIC-UPPER` accepte majuscules+espaces.

</details>

---

### Question 8
Combien de niveaux d'imbrication IF sont autorisés en COBOL ?

- [ ] A) 3 maximum
- [ ] B) 10 maximum
- [ ] C) Pas de limite
- [ ] D) Dépend du compilateur

<details>
<summary>Réponse</summary>

**C** - Il n'y a pas de limite à la profondeur des instructions IF imbriquées.

</details>

---

### Question 9
Comment fonctionne un niveau 88 dans un IF ?

- [ ] A) Il définit une variable booléenne séparée
- [ ] B) Il permet de tester si la variable associée contient certaines valeurs
- [ ] C) Il remplace la clause PICTURE
- [ ] D) Il est incompatible avec IF

<details>
<summary>Réponse</summary>

**B** - Le niveau 88 définit des valeurs pour lesquelles une condition est vraie. `IF ACTIF` teste si la variable parente contient la valeur associée à ACTIF.

</details>

---

### Question 10
Que signifie `88 VALIDE VALUES ARE 10 THRU 20` ?

- [ ] A) VALIDE est vraie si la variable = 10 ou 20
- [ ] B) VALIDE est vraie si la variable est entre 10 et 20 inclus
- [ ] C) VALIDE est vraie si la variable > 10 et < 20
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**B** - `THRU` définit une plage inclusive. VALIDE est vraie pour 10, 11, 12... 19, 20.

</details>

---

## Section 2 : EVALUATE (Questions 11-16)

### Question 11
À quoi correspond EVALUATE en programmation moderne ?

- [ ] A) Une boucle FOR
- [ ] B) Une instruction SWITCH/CASE
- [ ] C) Une condition ternaire
- [ ] D) Une fonction

<details>
<summary>Réponse</summary>

**B** - EVALUATE est similaire à l'instruction SWITCH dans les langages comme C ou Java.

</details>

---

### Question 12
Que signifie `WHEN OTHER` dans un EVALUATE ?

- [ ] A) Toujours exécuté
- [ ] B) Cas par défaut (si aucun WHEN ne correspond)
- [ ] C) Exécuté pour les valeurs OTHER
- [ ] D) Optionnel et ignoré

<details>
<summary>Réponse</summary>

**B** - `WHEN OTHER` est le cas par défaut, exécuté si aucune autre condition WHEN n'est satisfaite.

</details>

---

### Question 13
Que permet `EVALUATE TRUE` ?

- [ ] A) Évaluer si une variable est vraie
- [ ] B) Tester des conditions booléennes complexes
- [ ] C) Toujours entrer dans le premier WHEN
- [ ] D) Initialiser une variable

<details>
<summary>Réponse</summary>

**B** - `EVALUATE TRUE` permet de tester plusieurs conditions booléennes. Le premier WHEN dont la condition est vraie est exécuté.

</details>

---

### Question 14
À quoi sert `ALSO` dans EVALUATE ?

- [ ] A) Ajouter un commentaire
- [ ] B) Combiner plusieurs variables/conditions
- [ ] C) Définir un alias
- [ ] D) Terminer l'EVALUATE

<details>
<summary>Réponse</summary>

**B** - `ALSO` permet d'évaluer plusieurs variables simultanément : `EVALUATE var1 ALSO var2`.

</details>

---

### Question 15
Que signifie `WHEN ANY` dans un EVALUATE ?

- [ ] A) N'importe quelle valeur (joker)
- [ ] B) Aucune valeur
- [ ] C) Valeur indéfinie
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**A** - `ANY` est un joker qui accepte n'importe quelle valeur. Utile avec ALSO pour ignorer une variable.

</details>

---

### Question 16
Dans `EVALUATE NOTE-EXAM ALSO NOTE-STAGE WHEN 10 THRU 20 ALSO 10 THRU 20`, quand ce WHEN est-il vrai ?

- [ ] A) Si NOTE-EXAM OU NOTE-STAGE est entre 10 et 20
- [ ] B) Si NOTE-EXAM ET NOTE-STAGE sont entre 10 et 20
- [ ] C) Si NOTE-EXAM + NOTE-STAGE est entre 10 et 20
- [ ] D) Toujours

<details>
<summary>Réponse</summary>

**B** - Les conditions ALSO sont combinées avec un ET logique. Les deux conditions doivent être vraies.

</details>

---

## Section 3 : PERFORM (Questions 17-22)

### Question 17
Quelle forme de PERFORM exécute un paragraphe exactement 5 fois ?

- [ ] A) `PERFORM para UNTIL 5`
- [ ] B) `PERFORM para 5 TIMES`
- [ ] C) `PERFORM para REPEAT 5`
- [ ] D) `PERFORM para FOR 5`

<details>
<summary>Réponse</summary>

**B** - `PERFORM paragraphe n TIMES` exécute le paragraphe exactement n fois.

</details>

---

### Question 18
Quelle est la différence entre `WITH TEST BEFORE` et `WITH TEST AFTER` ?

- [ ] A) Aucune différence
- [ ] B) BEFORE = while (test avant), AFTER = do-while (test après)
- [ ] C) BEFORE est plus rapide
- [ ] D) AFTER permet plus d'itérations

<details>
<summary>Réponse</summary>

**B** - `WITH TEST BEFORE` teste avant chaque itération (boucle while). `WITH TEST AFTER` teste après (boucle do-while, exécute au moins une fois).

</details>

---

### Question 19
Quel est le comportement par défaut de PERFORM UNTIL ?

- [ ] A) WITH TEST AFTER
- [ ] B) WITH TEST BEFORE
- [ ] C) Sans test
- [ ] D) Dépend de la condition

<details>
<summary>Réponse</summary>

**B** - Par défaut, `PERFORM UNTIL` utilise `WITH TEST BEFORE` : la condition est vérifiée avant chaque exécution.

</details>

---

### Question 20
Que fait `PERFORM para VARYING I FROM 1 BY 2 UNTIL I > 10` ?

- [ ] A) Exécute para 10 fois
- [ ] B) Exécute para 5 fois (I = 1, 3, 5, 7, 9)
- [ ] C) Exécute para 6 fois (I = 1, 3, 5, 7, 9, 11)
- [ ] D) Erreur car BY 2 n'est pas valide

<details>
<summary>Réponse</summary>

**B** - I prend les valeurs 1, 3, 5, 7, 9. Quand I=11, la condition I>10 est vraie et la boucle s'arrête.

</details>

---

### Question 21
À quoi sert le mot-clé `THRU` dans `PERFORM para1 THRU para3` ?

- [ ] A) Exécuter para1 puis para3
- [ ] B) Exécuter para1, para2 et para3 en séquence
- [ ] C) Choisir aléatoirement entre para1 et para3
- [ ] D) Définir une plage de valeurs

<details>
<summary>Réponse</summary>

**B** - `PERFORM para1 THRU para3` exécute tous les paragraphes de para1 à para3 inclus, dans l'ordre.

</details>

---

### Question 22
Comment créer une boucle inline sans paragraphe séparé ?

- [ ] A) `PERFORM ... END-PERFORM`
- [ ] B) `LOOP ... END-LOOP`
- [ ] C) `FOR ... END-FOR`
- [ ] D) Impossible en COBOL

<details>
<summary>Réponse</summary>

**A** - `PERFORM VARYING ... instructions ... END-PERFORM` permet d'écrire les instructions directement sans paragraphe séparé.

</details>

---

## Section 4 : GO TO et bonnes pratiques (Questions 23-25)

### Question 23
Pourquoi GO TO est-il déconseillé en COBOL moderne ?

- [ ] A) Il est obsolète et ne fonctionne plus
- [ ] B) Il crée du code "spaghetti" difficile à maintenir
- [ ] C) Il est plus lent que PERFORM
- [ ] D) Il utilise trop de mémoire

<details>
<summary>Réponse</summary>

**B** - GO TO rend le flux de contrôle difficile à suivre, créant du code "spaghetti". PERFORM est préférable.

</details>

---

### Question 24
Que fait `GO TO para1 para2 para3 DEPENDING ON X` ?

- [ ] A) Exécute les trois paragraphes
- [ ] B) Va à para1 si X=1, para2 si X=2, para3 si X=3
- [ ] C) Choisit aléatoirement un paragraphe
- [ ] D) Erreur de syntaxe

<details>
<summary>Réponse</summary>

**B** - Le branchement dépend de la valeur de X. Si X=1→para1, X=2→para2, X=3→para3.

</details>

---

### Question 25
Quelle est la meilleure alternative à `GO TO para DEPENDING ON var` ?

- [ ] A) `IF var = 1 GO TO para1`
- [ ] B) `EVALUATE var WHEN 1 PERFORM para1...`
- [ ] C) `PERFORM para var TIMES`
- [ ] D) Il n'y a pas d'alternative

<details>
<summary>Réponse</summary>

**B** - EVALUATE avec PERFORM est plus structuré et lisible que GO TO DEPENDING ON.

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
| [QCM Chapitre IV](qcm-04-operations-donnees.md) | [QCM Chapitre VI](qcm-06-gestion-tables.md) |

---
*Formation COBOL - M2i Formation*
