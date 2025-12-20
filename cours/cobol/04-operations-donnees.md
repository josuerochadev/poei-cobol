# Chapitre IV - Opérations sur les données

Ce chapitre traite le contenu de la **PROCEDURE DIVISION**. Toutes les opérations de modification du contenu des variables sont développées.

---

## IV-1 : Affectation MOVE

L'instruction `MOVE` permet de charger une variable par une autre variable ou par une valeur.

### Syntaxe

```cobol
       MOVE source TO destination-1 [destination-2 ...]
```

### Exemples de base

```cobol
       MOVE 1200 TO PRIX.              *> PRIX = 1200
       MOVE SPACES TO LIGNE1 LIGNE2.   *> LIGNE1 & LIGNE2 = SPACES
       MOVE VAR1 TO VAR2.              *> VAR2 = VAR1
       MOVE ZEROS TO WS-A WS-B WS-C.   *> MOVE multiple
```

### Règles de transfert

| Type | Direction | Complément | Troncature |
|------|-----------|------------|------------|
| **Alpha → Alpha** | Gauche → Droite | Espaces à droite | À droite |
| **Num → Num (entier)** | Droite → Gauche | Zéros à gauche | À gauche |
| **Num → Num (décimal)** | Alignement sur V | Zéros des 2 côtés | Des 2 côtés |

> **N.B.** : Une troncature des données sera faite si la taille de la variable réceptrice est inférieure à la taille des données à recevoir.

### Exemples avec troncature

**a) Transfert numérique sans troncature**
```cobol
       01  PRIX PIC 9(4) VALUE ZEROES.
           MOVE 1200 TO PRIX.
       *> Résultat : PRIX = 1200
```

**b) Transfert numérique avec troncature**
```cobol
       01  PRIX PIC 9(4) VALUE ZERO.
           MOVE 12345 TO PRIX.
       *> Résultat : PRIX = 2345 (troncature à gauche)
```

**c) Transfert alphanumérique avec troncature**
```cobol
       01  NOM PIC X(5) VALUE 'XXXXX'.
           MOVE 'ABCDEF' TO NOM.
       *> Résultat : NOM = 'ABCDE' (troncature à droite)
```

### MOVE avec décimales

Alignement sur la virgule décimale (V) :

```cobol
       01  WS-SOURCE  PIC 9(3)V99 VALUE 123.45.
       01  WS-DEST    PIC 9(5)V99.

       MOVE WS-SOURCE TO WS-DEST.   *> 00123.45
```

Pour les valeurs numériques décimales, le transfert de la partie décimale se fait de gauche à droite.

### MOVE CORRESPONDING

Copie les champs de même nom entre deux groupes :

```cobol
       MOVE CORRESPONDING groupe-source TO groupe-dest.
```

---

## IV-2 : Opérations de calcul

Les opérations arithmétiques sont traitées par les verbes : `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE` et `COMPUTE`.

### IV-2-a : ADD (Addition)

#### Syntaxes

```cobol
       ADD a TO b.                    *> b = b + a
       ADD a b TO c.                  *> c = c + a + b
       ADD a TO b GIVING c.           *> c = a + b (b inchangé)
       ADD a b c GIVING d.            *> d = a + b + c
```

#### Options

- `ROUNDED` : arrondit la valeur obtenue
- `ON SIZE ERROR` : intercepte un dépassement de capacité

#### Exemples détaillés

```cobol
      *> Exemple 1 : VAR1 = 50
       ADD 100 TO VAR1.
      *> Résultat : VAR1 = 150

      *> Exemple 2 : VAR1=20, VAR2=50, VAR3=100
       ADD VAR1 VAR2 GIVING VAR3.
      *> Résultat : VAR1=20, VAR2=50, VAR3=70

      *> Exemple 3 : ADD vers plusieurs destinations
       ADD VAR1 VAR2 GIVING VAR3 VAR4 VAR5.
      *> Résultat : VAR3 = VAR4 = VAR5 = 70
```

### IV-2-b : SUBTRACT (Soustraction)

#### Syntaxes

```cobol
       SUBTRACT a FROM b.             *> b = b - a
       SUBTRACT a FROM b GIVING c.    *> c = b - a
       SUBTRACT a b FROM c.           *> c = c - (a + b)
```

#### Règle importante

Les valeurs avant `FROM` sont additionnées, puis le total est déduit des variables après `FROM`.

#### Exemples détaillés

```cobol
      *> Exemple 1 : VAR1 = 50
       SUBTRACT 20 FROM VAR1.
      *> Résultat : VAR1 = 30

      *> Exemple 2 : VAR1=50, VAR2=200
       SUBTRACT 20 FROM VAR1 GIVING VAR2.
      *> Résultat : VAR1=50, VAR2=30

      *> Exemple 3 : Plusieurs valeurs avant FROM
       SUBTRACT 10 20 15 FROM VAR1 GIVING VAR2.
      *> Calcul : (10+20+15)=45, puis 50-45=5
      *> Résultat : VAR1=50, VAR2=5

      *> Exemple 4 : Sans GIVING, plusieurs variables modifiées
       SUBTRACT 10 20 15 FROM VAR1 VAR2 VAR3.
      *> La somme 45 est déduite de CHAQUE variable
      *> Si VAR1=50, VAR2=200, VAR3=150
      *> Résultat : VAR1=5, VAR2=155, VAR3=105
```

### IV-2-c : MULTIPLY (Multiplication)

#### Syntaxes

```cobol
       MULTIPLY a BY b.               *> b = b * a
       MULTIPLY a BY b GIVING c.      *> c = a * b
       MULTIPLY a BY b GIVING c d.    *> c = d = a * b
```

#### Exemples

```cobol
      *> La variable après BY est modifiée
       MULTIPLY VAR1 BY VAR2.
      *> VAR2 = VAR2 * VAR1

      *> Multiplication vers plusieurs destinations
       MULTIPLY VAR1 BY VAR2 VAR3 VAR4.
      *> Chaque variable est multipliée par VAR1
```

### IV-2-d : DIVIDE (Division)

#### Syntaxes avec INTO (diviseur à gauche)

```cobol
       DIVIDE diviseur INTO dividende.
      *> dividende = dividende / diviseur

       DIVIDE a INTO b GIVING c.
      *> c = b / a

       DIVIDE a INTO b GIVING c REMAINDER r.
      *> c = quotient, r = reste
```

#### Syntaxes avec BY (diviseur à droite, plus lisible)

```cobol
       DIVIDE dividende BY diviseur GIVING résultat.
      *> résultat = dividende / diviseur

       DIVIDE a BY b GIVING c REMAINDER r.
      *> c = a / b, r = reste
```

#### Exemples

```cobol
      *> INTO : le diviseur est À GAUCHE de INTO
       DIVIDE VAR1 INTO VAR2.
      *> Équivalent : VAR2 = VAR2 / VAR1

      *> BY : le diviseur est À DROITE de BY (plus naturel)
       DIVIDE VAR2 BY VAR1 GIVING VAR3 REMAINDER VAR4.
      *> VAR3 = VAR2 / VAR1, VAR4 = reste
```

### Tableau récapitulatif

| Opération | Syntaxe simple | Syntaxe GIVING |
|-----------|----------------|----------------|
| ADD | `ADD a TO b` → b = b + a | `ADD a TO b GIVING c` |
| SUBTRACT | `SUBTRACT a FROM b` → b = b - a | `SUBTRACT a FROM b GIVING c` |
| MULTIPLY | `MULTIPLY a BY b` → b = b * a | `MULTIPLY a BY b GIVING c` |
| DIVIDE | `DIVIDE a INTO b` → b = b / a | `DIVIDE a BY b GIVING c` |

---

### IV-2-e : COMPUTE

L'instruction `COMPUTE` permet d'affecter des expressions complexes à des variables.

#### Syntaxe

```cobol
       COMPUTE {variable [ROUNDED]} ... = expression arithmétique
           [ON SIZE ERROR instructions]
           [NOT ON SIZE ERROR instructions]
       [END-COMPUTE].
```

#### Opérateurs

| Opérateur | Signification | Priorité |
|-----------|---------------|----------|
| `**` | Puissance | 1 (haute) |
| `*` | Multiplication | 2 |
| `/` | Division | 2 |
| `+` | Addition | 3 (basse) |
| `-` | Soustraction | 3 (basse) |
| `( )` | Parenthèses | Évalué en premier |

#### Règles d'évaluation

1. Les parenthèses sont évaluées en premier
2. Ensuite les puissances (`**`)
3. Puis `*` et `/` de gauche à droite
4. Enfin `+` et `-` de gauche à droite

#### Exemples

```cobol
      *> Exemple a : VAR1 = 20
       COMPUTE VAR2 = VAR1 + 9 ** 2.
      *> Calcul : 9² = 81, puis 20 + 81 = 101
      *> Résultat : VAR2 = 101

      *> Calcul TVA
       COMPUTE WS-TTC = WS-HT * (1 + WS-TVA / 100).

      *> Moyenne
       COMPUTE WS-MOY = (NOTE1 + NOTE2 + NOTE3) / 3.

      *> Puissance
       COMPUTE WS-CARRE = WS-A ** 2.

      *> Intérêts composés
       COMPUTE WS-CAPITAL = WS-INIT * (1 + WS-TAUX) ** WS-ANNEES.

      *> Prix avec remise
       COMPUTE PRIX = (PRIX-UNIT * QTE) * (1 - REMISE / 100).
```

#### Exemple avec ROUNDED

```cobol
       01  VAR2 PIC 99V9.
       COMPUTE VAR2 ROUNDED = 15.325 + 40.555.
      *> Total = 55.880 → tronqué à 55.8 → arrondi à 55.9
```

---

## Options communes

### ROUNDED (arrondi)

```cobol
       DIVIDE 10 BY 3 GIVING WS-R ROUNDED.
       COMPUTE WS-R ROUNDED = WS-A / WS-B.
```

### ON SIZE ERROR (dépassement)

```cobol
       01  VAR1 PIC 9(3) VALUE 500.
       01  VAR2 PIC 9(3) VALUE 600.
       01  VAR3 PIC 9(3) VALUE 700.

       COMPUTE VAR1 = VAR2 + VAR3
           ON SIZE ERROR
               DISPLAY 'DEBORDEMENT'
               DISPLAY 'DONNEES PROBABLEMENT INCORRECTES'
               MOVE 0 TO VAR1
           NOT ON SIZE ERROR
               DISPLAY 'Calcul OK'
       END-COMPUTE.
      *> Total = 1300 (dépasse PIC 9(3) max = 999)
      *> L'instruction ON SIZE ERROR est exécutée : VAR1 = 0
```

> **Note :** `ON SIZE ERROR` est valable pour toutes les opérations arithmétiques : ADD, SUBTRACT, MULTIPLY, DIVIDE et COMPUTE.

---

## IV-3 : Récapitulatif des opérations

### Équivalences avec notation mathématique

```cobol
       ADD 100 TO POIDS.
      *> Équivalent : POIDS = POIDS + 100

       SUBTRACT REMISE FROM TOTAL-ACHAT.
      *> Équivalent : TOTAL-ACHAT = TOTAL-ACHAT - REMISE

       ADD RECETTE-MATIN RECETTE-SOIR GIVING RECETTE-JOURNEE.
      *> Équivalent : RECETTE-JOURNEE = RECETTE-MATIN + RECETTE-SOIR

       ADD RECETTE-JOURNEE TO RECETTE-MOIS RECETTE-ANNEE.
      *> Équivalent : RECETTE-MOIS = RECETTE-MOIS + RECETTE-JOURNEE
      *>              RECETTE-ANNEE = RECETTE-ANNEE + RECETTE-JOURNEE

       MULTIPLY QUANTITE BY PRIX-UNITAIRE GIVING PRIX.
      *> Équivalent : PRIX = PRIX-UNITAIRE * QUANTITE

       DIVIDE SOMME-JOURNEE BY NOMBRE-PARTS GIVING SOMME-PART.
      *> Équivalent : SOMME-PART = SOMME-JOURNEE / NOMBRE-PARTS

       DIVIDE NB-LIVRE BY NB-ELEVE GIVING PART-ELEVE REMAINDER RESTE.
      *> Équivalent : PART-ELEVE = NB-LIVRE / NB-ELEVE (reste dans RESTE)

       COMPUTE PRIX = (PRIX-UNIT * QTE) * (1 - REMISE / 100).
      *> PRIX = Expression1 * Expression2
```

---

## Résumé - Points clés

| Instruction | Usage | Exemple |
|-------------|-------|---------|
| **MOVE** | Affectation | `MOVE a TO b` |
| **ADD** | Addition | `ADD a TO b GIVING c` |
| **SUBTRACT** | Soustraction | `SUBTRACT a FROM b GIVING c` |
| **MULTIPLY** | Multiplication | `MULTIPLY a BY b GIVING c` |
| **DIVIDE** | Division | `DIVIDE a BY b GIVING c REMAINDER r` |
| **COMPUTE** | Expression | `COMPUTE c = (a + b) * d ** 2` |

### Quand utiliser COMPUTE ?

- Expression complexe avec plusieurs opérations
- Utilisation de puissances (`**`)
- Formule mathématique à traduire

### Quand utiliser les verbes classiques ?

- Opération simple
- Besoin de REMAINDER
- Code plus explicite

---

## IV-4 : Exercice pratique - Calcul de facture

### Énoncé

En utilisant COMPUTE, calculer les totaux d'une facture (HT, Taxes, TTC) pour deux articles :

| Article | Prix Unitaire | Quantité | Remise (%) |
|---------|---------------|----------|------------|
| Article 1 | 3,75 € | 100 | 5 |
| Article 2 | 2,15 € | 10 | 15 |

**Contraintes :**
- Séparateur décimal : virgule (`,`)
- Les valeurs peuvent atteindre les millions d'euros
- Variables d'édition avec séparateur milliers (espace)

<details>
<summary>Solution</summary>

```cobol
       WORKING-STORAGE SECTION.
      *> Article 1
       01  ART1-PU          PIC 9(3)V99 VALUE 3.75.
       01  ART1-QTE         PIC 9(3)   VALUE 100.
       01  ART1-REMISE      PIC 99     VALUE 5.
      *> Article 2
       01  ART2-PU          PIC 9(3)V99 VALUE 2.15.
       01  ART2-QTE         PIC 9(3)   VALUE 10.
       01  ART2-REMISE      PIC 99     VALUE 15.
      *> Totaux
       01  WS-TOTAL-HT      PIC 9(7)V99.
       01  WS-TVA           PIC 99V99  VALUE 20.00.
       01  WS-TOTAL-TAXES   PIC 9(7)V99.
       01  WS-TOTAL-TTC     PIC 9(7)V99.
      *> Édition
       01  WS-EDT-HT        PIC ZZZ ZZZ ZZ9,99.
       01  WS-EDT-TAXES     PIC ZZZ ZZZ ZZ9,99.
       01  WS-EDT-TTC       PIC ZZZ ZZZ ZZ9,99.

       PROCEDURE DIVISION.
      *> Calcul HT avec remises
           COMPUTE WS-TOTAL-HT =
               (ART1-PU * ART1-QTE) * (1 - ART1-REMISE / 100)
             + (ART2-PU * ART2-QTE) * (1 - ART2-REMISE / 100).

      *> Calcul Taxes
           COMPUTE WS-TOTAL-TAXES = WS-TOTAL-HT * WS-TVA / 100.

      *> Calcul TTC
           COMPUTE WS-TOTAL-TTC = WS-TOTAL-HT + WS-TOTAL-TAXES.

      *> Affichage
           MOVE WS-TOTAL-HT TO WS-EDT-HT.
           MOVE WS-TOTAL-TAXES TO WS-EDT-TAXES.
           MOVE WS-TOTAL-TTC TO WS-EDT-TTC.
           DISPLAY 'Total HT    : ' WS-EDT-HT ' EUR'.
           DISPLAY 'Total Taxes : ' WS-EDT-TAXES ' EUR'.
           DISPLAY 'Total TTC   : ' WS-EDT-TTC ' EUR'.
           STOP RUN.
```
</details>

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - Déclaration Variables](03-declaration-variables.md) | [Chapitre V - Traitement Conditionnel](05-traitement-conditionnel.md) |

---
*Formation COBOL - Module COBOL*
