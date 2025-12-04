# Chapitre V - Traitement Conditionnel

## V-1 : IF ... THEN ... ELSE ... END-IF

### Syntaxe

```cobol
       IF condition THEN
           instruction(s)-si-vrai
       ELSE
           instruction(s)-si-faux
       END-IF.
```

### Opérateurs de comparaison

| Opérateur | Symbole | Signification |
|-----------|---------|---------------|
| `EQUAL TO` | `=` | Égal à |
| `NOT EQUAL TO` | `<>` | Différent de |
| `GREATER THAN` | `>` | Supérieur à |
| `LESS THAN` | `<` | Inférieur à |
| `>=` | | Supérieur ou égal |
| `<=` | | Inférieur ou égal |

### Opérateurs logiques

| Opérateur | Signification |
|-----------|---------------|
| `AND` | ET logique |
| `OR` | OU logique |
| `NOT` | Négation |

### Tests spéciaux

```cobol
       IF WS-VAR IS NUMERIC ...
       IF WS-VAR IS ALPHABETIC ...
       IF WS-VAR IS POSITIVE ...
       IF WS-VAR IS NEGATIVE ...
       IF WS-VAR IS ZERO ...
```

### Exemple avec niveau 88

```cobol
       01  WS-STATUT    PIC X.
           88  ACTIF    VALUE 'A'.
           88  INACTIF  VALUE 'I'.

       IF ACTIF
           DISPLAY 'Compte actif'
       END-IF.
```

---

## V-2 : EVALUATE

### Syntaxe de base

```cobol
       EVALUATE sujet
           WHEN valeur-1
               instruction(s)
           WHEN valeur-2
               instruction(s)
           WHEN OTHER
               instruction(s)-par-défaut
       END-EVALUATE.
```

### EVALUATE avec plages (THRU)

```cobol
       EVALUATE WS-NOTE
           WHEN 16 THRU 20
               DISPLAY 'Très bien'
           WHEN 14 THRU 15
               DISPLAY 'Bien'
           WHEN 12 THRU 13
               DISPLAY 'Assez bien'
           WHEN 10 THRU 11
               DISPLAY 'Passable'
           WHEN OTHER
               DISPLAY 'Insuffisant'
       END-EVALUATE.
```

### EVALUATE TRUE (conditions booléennes)

```cobol
       EVALUATE TRUE
           WHEN WS-SOLDE > 10000
               DISPLAY 'Client premium'
           WHEN WS-SOLDE > 1000
               DISPLAY 'Client standard'
           WHEN WS-SOLDE > 0
               DISPLAY 'Client basique'
           WHEN OTHER
               DISPLAY 'Compte débiteur'
       END-EVALUATE.
```

### EVALUATE avec plusieurs variables (ALSO)

```cobol
       EVALUATE WS-SEXE ALSO WS-AGE
           WHEN 'M' ALSO 0 THRU 17
               DISPLAY 'Garçon mineur'
           WHEN 'M' ALSO 18 THRU 99
               DISPLAY 'Homme majeur'
           WHEN 'F' ALSO ANY
               DISPLAY 'Femme'
           WHEN OTHER
               DISPLAY 'Données invalides'
       END-EVALUATE.
```

### Résumé EVALUATE

| Forme | Usage |
|-------|-------|
| `EVALUATE variable` | Test sur valeurs exactes |
| `WHEN val1 THRU val2` | Plage de valeurs |
| `EVALUATE var1 ALSO var2` | Plusieurs variables |
| `EVALUATE TRUE` | Conditions booléennes |
| `WHEN ANY` | N'importe quelle valeur |
| `WHEN OTHER` | Cas par défaut |

---

## V-3 : PERFORM

### PERFORM simple

```cobol
       PERFORM 1000-TRAITEMENT.
```

### PERFORM THRU

```cobol
       PERFORM 1000-DEBUT THRU 1999-FIN.
```

### PERFORM ... TIMES

```cobol
       PERFORM 1000-AFFICHER 5 TIMES.
```

### PERFORM ... UNTIL

```cobol
       PERFORM 1000-BOUCLE UNTIL WS-FIN = 'O'.

       *> Test avant (while) - par défaut
       PERFORM 1000-BOUCLE WITH TEST BEFORE UNTIL condition.

       *> Test après (do...while)
       PERFORM 1000-BOUCLE WITH TEST AFTER UNTIL condition.
```

### PERFORM ... VARYING (boucle FOR)

```cobol
       PERFORM 1000-AFFICHER
           VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 10.

       *> Boucles imbriquées
       PERFORM 1000-TRAITEMENT
           VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
           AFTER WS-J FROM 1 BY 1 UNTIL WS-J > 4.
```

### PERFORM inline

```cobol
       PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
           DISPLAY 'Itération ' WS-I
           ADD WS-I TO WS-TOTAL
       END-PERFORM.
```

### Tableau récapitulatif

| Forme | Usage |
|-------|-------|
| `PERFORM para` | Appel simple |
| `PERFORM para THRU para2` | Plusieurs paragraphes |
| `PERFORM para n TIMES` | Répéter n fois |
| `PERFORM para UNTIL cond` | Boucle while |
| `PERFORM para VARYING` | Boucle for |
| `PERFORM ... END-PERFORM` | Inline |

---

## V-4 : GO TO

### Syntaxe

```cobol
       GO TO nom-paragraphe.
```

### GO TO ... DEPENDING ON

```cobol
       GO TO para-1
              para-2
              para-3
           DEPENDING ON WS-CHOIX.
```

### ⚠️ Recommandation

**Éviter GO TO** - Crée du code spaghetti difficile à maintenir.

| GO TO | Alternative moderne |
|-------|---------------------|
| Branchement simple | `PERFORM paragraphe` |
| Branchement conditionnel | `IF / EVALUATE` |
| Boucle | `PERFORM UNTIL / VARYING` |
| GO TO DEPENDING ON | `EVALUATE variable` |

---

## Résumé - Points clés

| Instruction | Usage | Recommandation |
|-------------|-------|----------------|
| **IF...END-IF** | Test conditionnel | ✅ Recommandé |
| **EVALUATE** | Tests multiples (switch) | ✅ Recommandé |
| **PERFORM** | Appels, boucles | ✅ Recommandé |
| **GO TO** | Branchement | ⚠️ À éviter |

### Bonnes pratiques

1. Utiliser `END-IF`, `END-EVALUATE`, `END-PERFORM` systématiquement
2. Préférer `EVALUATE` aux `IF` imbriqués
3. Utiliser les niveaux 88 pour les conditions lisibles
4. Éviter `GO TO` - utiliser `PERFORM` à la place
5. Choisir `WITH TEST BEFORE` (while) ou `WITH TEST AFTER` (do-while) selon le besoin
