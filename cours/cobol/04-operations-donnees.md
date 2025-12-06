# Chapitre IV - Opérations sur les données

## IV-1 : Affectation MOVE

### Syntaxe

```cobol
       MOVE source TO destination-1 [destination-2 ...]
```

### Exemples

```cobol
       MOVE 100 TO WS-COMPTEUR.
       MOVE 'DUPONT' TO WS-NOM.
       MOVE ZEROS TO WS-A WS-B WS-C.        *> MOVE multiple
```

### Règles de conversion

| Source → Dest | Cadrage | Complément | Troncature |
|---------------|---------|------------|------------|
| Num → Num | Droite | Zéros à gauche | À gauche |
| Alpha → Alpha | Gauche | Espaces à droite | À droite |

### MOVE avec décimales

Alignement sur la virgule décimale (V).

```cobol
       01  WS-SOURCE  PIC 9(3)V99 VALUE 123.45.
       01  WS-DEST    PIC 9(5)V99.

       MOVE WS-SOURCE TO WS-DEST.   *> 00123.45
```

### MOVE CORRESPONDING

```cobol
       MOVE CORRESPONDING groupe-source TO groupe-dest.
       *> Copie les champs de même nom
```

---

## IV-2 : Opérations de calcul

### IV-2-a : ADD (Addition)

```cobol
       ADD a TO b.                    *> b = b + a
       ADD a TO b GIVING c.           *> c = a + b
       ADD a b c GIVING d.            *> d = a + b + c
```

### IV-2-b : SUBTRACT (Soustraction)

```cobol
       SUBTRACT a FROM b.             *> b = b - a
       SUBTRACT a FROM b GIVING c.    *> c = b - a
```

### IV-2-c : MULTIPLY (Multiplication)

```cobol
       MULTIPLY a BY b.               *> b = b * a
       MULTIPLY a BY b GIVING c.      *> c = a * b
```

### IV-2-d : DIVIDE (Division)

```cobol
       DIVIDE a INTO b.               *> b = b / a
       DIVIDE a INTO b GIVING c.      *> c = b / a
       DIVIDE a BY b GIVING c.        *> c = a / b (plus lisible)

       DIVIDE a BY b GIVING c REMAINDER r.   *> Avec reste
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

Expression arithmétique complète en une instruction.

```cobol
       COMPUTE résultat = expression.
```

#### Opérateurs

| Opérateur | Signification |
|-----------|---------------|
| `+` | Addition |
| `-` | Soustraction |
| `*` | Multiplication |
| `/` | Division |
| `**` | Puissance |
| `( )` | Parenthèses |

#### Priorité des opérateurs

```
1. ( )     Parenthèses
2. **      Puissance
3. * /     Multiplication, Division
4. + -     Addition, Soustraction
```

#### Exemples

```cobol
       *> Calcul TVA
       COMPUTE WS-TTC = WS-HT * (1 + WS-TVA / 100).

       *> Moyenne
       COMPUTE WS-MOY = (NOTE1 + NOTE2 + NOTE3) / 3.

       *> Puissance
       COMPUTE WS-CARRE = WS-A ** 2.

       *> Intérêts composés
       COMPUTE WS-CAPITAL = WS-INIT * (1 + WS-TAUX) ** WS-ANNEES.
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
       ADD WS-A TO WS-B GIVING WS-C
           ON SIZE ERROR
               DISPLAY 'Dépassement !'
               MOVE 0 TO WS-C
           NOT ON SIZE ERROR
               DISPLAY 'OK'
       END-ADD.

       COMPUTE WS-R = WS-A / WS-B
           ON SIZE ERROR
               DISPLAY 'Division par zéro'
       END-COMPUTE.
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
- Utilisation de puissances (**)
- Formule mathématique à traduire

### Quand utiliser les verbes classiques ?

- Opération simple
- Besoin de REMAINDER
- Code plus explicite

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - Déclaration Variables](03-declaration-variables.md) | [Chapitre V - Traitement Conditionnel](05-traitement-conditionnel.md) |

---
*Formation COBOL - Module COBOL*
