# Chapitre III - Déclaration des variables

## III-1 : Définition des Niveaux (Level Number)

### Hiérarchie des variables

```cobol
       01  CLIENT.                           *> Niveau 01 : groupe principal
           05  CLIENT-ID        PIC 9(5).    *> Niveau 05 : sous-élément
           05  CLIENT-NOM       PIC X(30).
           05  CLIENT-ADRESSE.               *> Niveau 05 : sous-groupe
               10  ADR-RUE      PIC X(30).   *> Niveau 10 : sous-sous-élément
               10  ADR-VILLE    PIC X(20).
               10  ADR-CP       PIC 9(5).
```

### Les niveaux standards

| Niveau | Usage | Zone |
|--------|-------|------|
| **01** | Enregistrement principal (racine) | Zone A |
| **02-49** | Sous-éléments hiérarchiques | Zone B |
| **66** | RENAMES (regroupement alternatif) | Zone B |
| **77** | Variable indépendante | Zone A |
| **88** | Condition (valeur booléenne) | Zone B |

### Niveau 77 : Variable indépendante

```cobol
       77  WS-TOTAL            PIC 9(7)V99.
```

### Niveau 88 : Conditions

```cobol
       01  WS-STATUT           PIC X.
           88  STATUT-ACTIF    VALUE 'A'.
           88  STATUT-INACTIF  VALUE 'I'.

       *> Utilisation
       IF STATUT-ACTIF
           DISPLAY 'Actif'
       END-IF.

       SET STATUT-INACTIF TO TRUE.    *> Affecte 'I'
```

### Règles

1. Un niveau INFÉRIEUR est un SOUS-ÉLÉMENT du niveau supérieur
2. Les niveaux n'ont pas besoin d'être consécutifs (01 → 05 → 10)
3. Un GROUPE ne peut pas avoir de PIC
4. Un élément ÉLÉMENTAIRE doit avoir un PIC

---

## III-2 : Définition de format image (Picture Clause)

### Caractères de base

| Symbole | Type | Description |
|---------|------|-------------|
| `9` | Numérique | Un chiffre (0-9) |
| `X` | Alphanumérique | Tout caractère |
| `A` | Alphabétique | Lettre ou espace |
| `S` | Signe | +/- |
| `V` | Virgule implicite | Position décimale virtuelle |

### Exemples

```cobol
       01  WS-CODE         PIC 9(5).       *> 00000 à 99999
       01  WS-NOM          PIC X(30).      *> 30 caractères
       01  WS-MONTANT      PIC S9(5)V99.   *> -99999.99 à +99999.99
```

### Notation abrégée

```cobol
       PIC 9999999    équivaut à    PIC 9(7)
       PIC XXXXXXXXXX équivaut à    PIC X(10)
```

### USAGE CLAUSE

| USAGE | Description | Taille PIC 9(5) |
|-------|-------------|-----------------|
| `DISPLAY` | 1 car = 1 octet (défaut) | 5 octets |
| `COMP` / `BINARY` | Binaire | 4 octets |
| `COMP-3` / `PACKED-DECIMAL` | Packed | 3 octets |

---

## III-3 : Définition de format Valeur (Value Clause)

### Syntaxe

```cobol
       01  WS-VAR    PIC format    VALUE valeur.
```

### Types de valeurs

```cobol
       *> Numériques
       01  WS-ENTIER       PIC 9(5)     VALUE 100.
       01  WS-DECIMAL      PIC 9(3)V99  VALUE 123.45.
       01  WS-NEGATIF      PIC S9(5)    VALUE -500.

       *> Alphanumériques
       01  WS-NOM          PIC X(10)    VALUE 'DUPONT'.
       01  WS-APOS         PIC X(10)    VALUE 'C''EST'.
```

### Constantes figuratives

| Constante | Valeur |
|-----------|--------|
| `ZERO` / `ZEROS` / `ZEROES` | 0 |
| `SPACE` / `SPACES` | Espace |
| `HIGH-VALUE` / `HIGH-VALUES` | xFF (max) |
| `LOW-VALUE` / `LOW-VALUES` | x00 (min) |
| `ALL 'x'` | Répétition du caractère |

```cobol
       01  WS-CPT     PIC 9(5)   VALUE ZEROS.      *> 00000
       01  WS-NOM     PIC X(20)  VALUE SPACES.     *> 20 espaces
       01  WS-LIGNE   PIC X(50)  VALUE ALL '-'.    *> 50 tirets
```

### VALUE avec niveau 88

```cobol
       01  WS-NOTE          PIC 99.
           88  NOTE-INSUFFISANT   VALUE 0 THRU 9.
           88  NOTE-PASSABLE      VALUE 10 THRU 12.
           88  NOTE-BIEN          VALUE 13 THRU 15.
           88  NOTE-TRES-BIEN     VALUE 16 THRU 20.
```

---

## III-4 : Édition des valeurs numériques

### Symboles d'édition

| Symbole | Description | Exemple |
|---------|-------------|---------|
| `Z` | Zéro → espace | `ZZZ9` : 42 → `  42` |
| `*` | Zéro → astérisque | `***9` : 42 → `**42` |
| `.` | Point décimal réel | `Z9.99` |
| `,` | Séparateur milliers | `ZZZ,ZZ9` |
| `+` | Signe toujours | `+ZZ9` : 42 → `+ 42` |
| `-` | Signe si négatif | `-ZZ9` : 42 → `  42` |
| `$` | Symbole monétaire | `$ZZ9.99` |
| `B` | Insertion espace | `99B99` : 1234 → `12 34` |
| `/` | Insertion slash | `99/99/99` |
| `CR` | Crédit si négatif | `ZZ9CR` |
| `DB` | Débit si négatif | `ZZ9DB` |

### Exemples pratiques

```cobol
       *> Stockage
       01  WS-MONTANT      PIC 9(6)V99  VALUE 1234.56.

       *> Édition
       01  WS-MONTANT-EDT  PIC ZZZ,ZZ9.99.

       MOVE WS-MONTANT TO WS-MONTANT-EDT.
       DISPLAY WS-MONTANT-EDT.            *> '  1,234.56'
```

### Tableau récapitulatif

| Besoin | PIC d'édition | Exemple |
|--------|---------------|---------|
| Supprimer zéros | `ZZZZ9` | `   42` |
| Protéger avec * | `****9` | `***42` |
| Milliers | `ZZZ,ZZ9` | ` 12,345` |
| Monétaire | `$$$,$$9.99` | `$1,234.56` |
| Date | `99/99/9999` | `15/12/2024` |
| Téléphone | `99B99B99B99B99` | `06 12 34 56 78` |

### Règle importante

```
Variables d'ÉDITION = AFFICHAGE uniquement, pas de calculs !

Stockage (calcul)   →  PIC 9(5)V99
Édition (affichage) →  PIC ZZZ,ZZ9.99

TOUJOURS : MOVE stockage TO édition avant DISPLAY
```

---

## III-5 : REDEFINES

### Concept

Plusieurs VUES sur la MÊME zone mémoire.

```cobol
       01  WS-DATE              PIC 9(8) VALUE 20241215.
       01  WS-DATE-R            REDEFINES WS-DATE.
           05  WS-ANNEE         PIC 9(4).
           05  WS-MOIS          PIC 99.
           05  WS-JOUR          PIC 99.
```

### Cas d'usage

1. **Décomposer une date**
```cobol
       01  WS-DATE          PIC 9(8).
       01  WS-DATE-R        REDEFINES WS-DATE.
           05  WS-ANNEE     PIC 9(4).
           05  WS-MOIS      PIC 99.
           05  WS-JOUR      PIC 99.
```

2. **Conversion numérique ↔ alphanumérique**
```cobol
       01  WS-NUMERO        PIC 9(5).
       01  WS-NUMERO-X      REDEFINES WS-NUMERO PIC X(5).
```

3. **Fichier multi-formats**
```cobol
       01  ENR-FICHIER      PIC X(100).
       01  ENR-CLIENT       REDEFINES ENR-FICHIER.
           05  CLI-TYPE     PIC X.
           05  CLI-CODE     PIC 9(5).
           ...
       01  ENR-PRODUIT      REDEFINES ENR-FICHIER.
           05  PRD-TYPE     PIC X.
           05  PRD-CODE     PIC X(10).
           ...
```

4. **Tableau à partir de données linéaires**
```cobol
       01  WS-MOIS-NOMS     PIC X(36)
           VALUE 'JANFEVMARAVRMAIJUNJULAOUSEPOCTNOVDEC'.
       01  WS-MOIS-TAB      REDEFINES WS-MOIS-NOMS.
           05  WS-MOIS-NOM  PIC X(3) OCCURS 12 TIMES.
```

### Règles du REDEFINES

1. **Même niveau** que la variable redéfinie
2. **Juste après** la variable redéfinie
3. **Taille ≤** taille de la variable redéfinie
4. **Pas de VALUE** sur la variable qui redéfinit
5. Plusieurs REDEFINES possibles sur une même variable

---

## Résumé - Points clés

| Concept | À retenir |
|---------|-----------|
| **Niveaux** | 01 (racine), 02-49 (hiérarchie), 77 (indépendant), 88 (condition) |
| **PIC stockage** | 9 (chiffre), X (alphanum), S (signe), V (virgule virtuelle) |
| **PIC édition** | Z (zéro→espace), * (zéro→*), . , + - $ B / |
| **VALUE** | Initialisation, ZEROS, SPACES, ALL, THRU pour 88 |
| **REDEFINES** | Plusieurs vues sur même mémoire |
| **USAGE** | DISPLAY (défaut), COMP (binaire), COMP-3 (packed) |
