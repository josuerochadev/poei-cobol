# Chapitre V - Traitement Conditionnel

Ce chapitre traite les opérations de test des conditions sous différentes formes : IF, EVALUATE, PERFORM et GO TO.

---

## V-1 : IF ... THEN ... ELSE ... END-IF

### Syntaxe

```cobol
       IF condition THEN
           instruction(s)-si-vrai
       [ELSE
           instruction(s)-si-faux]
       [END-IF].
```

### Règles de fonctionnement

| Règle | Description |
|-------|-------------|
| a) | IF vérifie la condition. Si vraie → bloc IF. Si fausse → bloc ELSE |
| b) | `END-IF` termine le bloc. Un point peut le remplacer mais END-IF est préférable |
| c) | `THEN` est facultatif |
| d) | Pas de limite à la profondeur des IF imbriqués |
| e) | Sans END-IF, le bloc se prolonge jusqu'au point (.) |
| f) | `NEXT SENTENCE` transfère le contrôle après le point le plus proche |

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

#### Sign Condition (test de signe)

```cobol
       IF WS-VAR IS POSITIVE ...    *> > 0
       IF WS-VAR IS NEGATIVE ...    *> < 0
       IF WS-VAR IS ZERO ...        *> = 0
```

#### Class Condition (test de type)

```cobol
       IF WS-VAR IS NUMERIC ...           *> Contient uniquement des chiffres
       IF WS-VAR IS ALPHABETIC ...        *> Lettres et espaces uniquement
       IF WS-VAR IS ALPHABETIC-LOWER ...  *> Minuscules et espaces
       IF WS-VAR IS ALPHABETIC-UPPER ...  *> Majuscules et espaces
```

### Exemple de base

```cobol
       MOVE 100 TO VAR1.
       MOVE 200 TO VAR2.
       IF VAR1 > VAR2 THEN
           MOVE VAR1 TO MAX
           MOVE VAR2 TO MIN
       ELSE
           MOVE VAR1 TO MIN
           MOVE VAR2 TO MAX
       END-IF.
      *> Résultat : MAX=200, MIN=100 (100>200 est faux → ELSE)
```

### NEXT SENTENCE

`NEXT SENTENCE` transfère le contrôle à l'instruction après le point le plus proche :

```cobol
       MOVE 100 TO VAR1.
       MOVE 200 TO VAR2.
       MOVE 300 TO VAR3.
       IF VAR1 > ZERO AND VAR2 > ZERO THEN
           NEXT SENTENCE
       ELSE
           DISPLAY 'VAR1 =' VAR1
           DISPLAY 'VAR2 =' VAR2
       END-IF
       ADD VAR1 TO VAR3.
       DISPLAY 'VAR3 =' VAR3.
      *> Condition vraie → NEXT SENTENCE → saute au point après END-IF
      *> Résultat : VAR3 = 400
```

### IF imbriqué

```cobol
       MOVE 25 TO WS-NUM1 WS-NUM3.
       MOVE 15 TO WS-NUM2 WS-NUM4.
       IF WS-NUM1 > WS-NUM2 THEN
           DISPLAY 'IN LOOP 1 - IF BLOCK'
           IF WS-NUM3 = WS-NUM4 THEN
               DISPLAY 'IN LOOP 2 - IF BLOCK'
           ELSE
               DISPLAY 'IN LOOP 2 - ELSE BLOCK'
           END-IF
       ELSE
           DISPLAY 'IN LOOP 1 - ELSE BLOCK'
       END-IF.
      *> Résultat :
      *> IN LOOP 1 - IF BLOCK
      *> IN LOOP 2 - ELSE BLOCK
```

### Exemple avec niveau 88 (Condition-Name)

```cobol
       01  WS-STATUT    PIC X.
           88  ACTIF    VALUE 'A'.
           88  INACTIF  VALUE 'I'.

       IF ACTIF
           DISPLAY 'Compte actif'
       END-IF.

      *> Avec plage de valeurs
       01  NUM PIC 9(3).
           88  PASS VALUES ARE 041 THRU 100.
           88  FAIL VALUES ARE 000 THRU 40.

       MOVE 065 TO NUM.
       IF PASS
           DISPLAY 'Passed with ' NUM ' marks'.
      *> Résultat : Passed with 065 marks
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

Possibilité de combiner plusieurs variables tests en même temps :

```cobol
       EVALUATE NOTE-EXAM ALSO NOTE-STAGE
           WHEN 10 THRU 20 ALSO 10 THRU 20
               MOVE "RECU" TO DECISION
           WHEN 9 THRU 10 ALSO 12 THRU 20
               MOVE "RATTRAPE PAR LE STAGE" TO DECISION
           WHEN 14 THRU 20 ALSO 9 THRU 10
               MOVE "RATTRAPE PAR NOTES EXAM" TO DECISION
           WHEN OTHER
               MOVE "ELIMINE" TO DECISION
       END-EVALUATE.
```

| NOTE-EXAM | NOTE-STAGE | DECISION |
|-----------|------------|----------|
| 10 à 20 | 10 à 20 | RECU |
| 9 à 10 | 12 à 20 | RATTRAPE PAR LE STAGE |
| 14 à 20 | 9 à 10 | RATTRAPE PAR NOTES EXAM |
| Autre | Autre | ELIMINE |

```cobol
      *> Autre exemple avec sexe et âge
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

**Exemple avec WITH TEST AFTER :**

```cobol
       01 CNT PIC 9(1) VALUE 0.

       PERFORM B-PARA WITH TEST AFTER UNTIL CNT > 3.
       ...
       B-PARA.
           DISPLAY 'CNT : ' CNT.
           ADD 1 TO CNT.
      *> Résultat :
      *> CNT : 0
      *> CNT : 1
      *> CNT : 2
      *> CNT : 3
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

---

## V-5 : Exercices pratiques

### Exercice 1 : EVALUATE avec ALSO

Créer un programme qui évalue la décision d'admission basée sur NOTE-EXAM et NOTE-STAGE :

```cobol
       EVALUATE NOTE-EXAM ALSO NOTE-STAGE
           WHEN 10 THRU 20 ALSO 10 THRU 20
               MOVE "RECU" TO DECISION
           WHEN 9 THRU 10 ALSO 12 THRU 20
               MOVE "RATTRAPE PAR LE STAGE" TO DECISION
           WHEN 14 THRU 20 ALSO 9 THRU 10
               MOVE "RATTRAPE PAR NOTES EXAM" TO DECISION
           WHEN OTHER
               MOVE "ELIMINE" TO DECISION
       END-EVALUATE.
```

### Exercice 2 : Boucles imbriquées

Créer un programme avec trois boucles imbriquées :

| Variable | Limite | Affichage |
|----------|--------|-----------|
| VAR1 | ≤ 5 | À chaque itération |
| VAR2 | ≤ 3 | À chaque itération (imbriquée dans VAR1) |
| VAR3 | ≤ 6 | À chaque itération (imbriquée dans VAR2) |

<details>
<summary>Solution</summary>

```cobol
       WORKING-STORAGE SECTION.
       01  VAR1    PIC 99 VALUE 0.
       01  VAR2    PIC 99 VALUE 0.
       01  VAR3    PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       DEBUT.
           PERFORM BOUCLE1 VARYING VAR1 FROM 1 BY 1 UNTIL VAR1 > 5.
           STOP RUN.

       BOUCLE1.
           DISPLAY 'VAR1 = ' VAR1.
           PERFORM BOUCLE2 VARYING VAR2 FROM 1 BY 1 UNTIL VAR2 > 3.

       BOUCLE2.
           DISPLAY '  VAR2 = ' VAR2.
           PERFORM BOUCLE3 VARYING VAR3 FROM 1 BY 1 UNTIL VAR3 > 6.

       BOUCLE3.
           DISPLAY '    VAR3 = ' VAR3.
```
</details>

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IV - Opérations Données](04-operations-donnees.md) | [Chapitre VI - Gestion Tables](06-gestion-tables.md) |

---
*Formation COBOL - Module COBOL*
