# Chapitre I - Structure d'un Programme COBOL

## I-1 : Langage de programmation Mainframe

### Contexte
- **Mainframe** = ordinateur central pour traitement massif
- Haute disponibilité (99,999% uptime)
- Traitement batch (lots de données)

### Langages Mainframe principaux
| Langage | Usage |
|---------|-------|
| **COBOL** | Logique métier, batch |
| **JCL** | Orchestration jobs |
| **Assembleur** | Système, performance |
| **REXX** | Scripts |

### COBOL aujourd'hui
- 200+ milliards de lignes en production
- 95% des transactions ATM
- 80% des transactions financières mondiales

---

## I-2 : Présentation de COBOL et ses 60 ans

### Origine (1959)
- Créé par le **DoD** (Département de la Défense US)
- **CODASYL** : comité de standardisation
- **Grace Hopper** : figure fondatrice

### Signification
```
C.O.B.O.L.
│ │ │ │ │
│ │ │ │ └── Language      (Langage)
│ │ │ └──── Oriented      (Orienté)
│ │ └────── Business      (Affaires)
│ └──────── Organized     (Organisé)
└────────── Common        (Commun)
```

### Versions majeures
| Année | Version | Apports |
|-------|---------|---------|
| 1960 | COBOL-60 | Première version |
| 1985 | COBOL-85 | END-IF, END-PERFORM (structuré) |
| 2002 | COBOL-2002 | Orienté objet |
| 2014 | COBOL-2014 | XML, JSON |
| 2023 | COBOL-2023 | Dernière norme ISO |

---

## I-3 : Description technique

### 3-1 : Évolution
- **Avant COBOL-85** : GO TO partout (code spaghetti)
- **Après COBOL-85** : programmation structurée (END-IF, END-PERFORM)

### 3-2 : Spécificités
- Orienté **gestion** (pas calcul scientifique)
- **Précision décimale** garantie (crucial pour la finance)
- **Verbeux mais lisible** (auto-documenté)

### 3-3 : Caractéristiques
| Caractéristique | Description |
|-----------------|-------------|
| Paradigme | Procédural |
| Typage | Statique (PICTURE) |
| Structure | 4 divisions obligatoires |
| Fichiers | Gestion native |
| Calcul | Décimal fixe |
| Exécution | Compilé |

---

## I-4 : Composition d'un programme

### 4-1 : Les 4 DIVISIONS

```cobol
       IDENTIFICATION DIVISION.    *> QUI ? Métadonnées
       ENVIRONMENT DIVISION.       *> OÙ ? Configuration
       DATA DIVISION.              *> QUOI ? Variables
       PROCEDURE DIVISION.         *> COMMENT ? Traitement
```

#### IDENTIFICATION DIVISION
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOM-PROGRAMME.    *> Obligatoire
       AUTHOR. NOM-AUTEUR.           *> Optionnel
```

#### ENVIRONMENT DIVISION
```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT fichier ASSIGN TO "nom-fichier".
```

#### DATA DIVISION
```cobol
       DATA DIVISION.
       FILE SECTION.              *> Structure fichiers
       WORKING-STORAGE SECTION.   *> Variables programme
       LOCAL-STORAGE SECTION.     *> Variables locales
       LINKAGE SECTION.           *> Paramètres reçus
```

#### PROCEDURE DIVISION
```cobol
       PROCEDURE DIVISION.
       PARAGRAPHE-PRINCIPAL.
           instructions
           PERFORM AUTRE-PARAGRAPHE
           STOP RUN.

       AUTRE-PARAGRAPHE.
           instructions.
```

---

### 4-2 : Règles de codage (colonnes 1-80)

```
Colonnes:  1-6      7       8-11      12-72       73-80
         ┌──────┬───────┬────────┬────────────┬─────────┐
         │ Seq. │ Indic │ Zone A │   Zone B   │ Ignoré  │
         └──────┴───────┴────────┴────────────┴─────────┘
```

| Zone | Colonnes | Contenu |
|------|----------|---------|
| Séquence | 1-6 | Numérotation (optionnel) |
| Indicateur | 7 | `*` commentaire, `-` continuation |
| Zone A | 8-11 | DIVISION, SECTION, paragraphes, 01/77 |
| Zone B | 12-72 | Instructions, niveaux 02-49 |
| Identification | 73-80 | Ignoré |

#### Colonne 7 - Indicateurs
| Caractère | Signification |
|-----------|---------------|
| `*` | Ligne de commentaire |
| `-` | Continuation de ligne |
| `D` | Ligne de debug |
| ` ` | Ligne normale |

#### Zone A (colonne 8)
- Noms de DIVISION
- Noms de SECTION
- Noms de paragraphes
- Niveaux 01 et 77

#### Zone B (colonne 12+)
- Instructions
- Niveaux 02 à 49
- Clauses et options

---

### 4-3 : Règles de nommage

#### Variables
| Règle | Valide | Invalide |
|-------|--------|----------|
| 1-30 caractères | `CLIENT-NOM` | Nom trop long (>30) |
| Lettres, chiffres, tirets | `TOTAL-2024` | `TOTAL_2024` |
| Pas tiret début/fin | `MON-TOTAL` | `-TOTAL` |
| Pas mot réservé | `WS-ADD` | `ADD` |
| Au moins une lettre | `LIGNE1` | `12345` |

#### Mots réservés (exemples)
```
ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
MOVE, DISPLAY, ACCEPT, IF, ELSE, END-IF
PERFORM, UNTIL, TIMES, VARYING, THRU
READ, WRITE, OPEN, CLOSE, FILE
```

#### Littéraux
```cobol
       *> Chaînes de caractères
       MOVE "Texte entre guillemets" TO VARIABLE.
       MOVE 'Texte entre apostrophes' TO VARIABLE.

       *> Numériques
       MOVE 12345 TO NOMBRE.
       MOVE -98.76 TO MONTANT.
```

#### Ponctuation
- **Point (.)** : termine une instruction (obligatoire)
- **Virgule (,)** : optionnelle (lisibilité)

---

### 4-4 : Exemple complet

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL-TVA.
       AUTHOR. FORMATION-COBOL.
      *─────────────────────────────────────────────────────
      * Programme exemple : Calcul de TVA
      *─────────────────────────────────────────────────────

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRIX-HT          PIC 9(5)V99   VALUE 100.00.
       01  WS-TAUX-TVA         PIC 9(2)V99   VALUE 20.00.
       01  WS-MONTANT-TVA      PIC 9(5)V99   VALUE 0.
       01  WS-PRIX-TTC         PIC 9(6)V99   VALUE 0.
       01  WS-AFFICHAGE        PIC Z(5)9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY "=== CALCUL DE TVA ==="
           PERFORM 1000-CALCUL-TVA
           PERFORM 2000-AFFICHAGE
           STOP RUN.

       1000-CALCUL-TVA.
           COMPUTE WS-MONTANT-TVA =
               WS-PRIX-HT * WS-TAUX-TVA / 100.
           ADD WS-PRIX-HT WS-MONTANT-TVA
               GIVING WS-PRIX-TTC.

       2000-AFFICHAGE.
           MOVE WS-PRIX-HT TO WS-AFFICHAGE
           DISPLAY "Prix HT     : " WS-AFFICHAGE " EUR"
           MOVE WS-TAUX-TVA TO WS-AFFICHAGE
           DISPLAY "Taux TVA    : " WS-AFFICHAGE " %"
           MOVE WS-MONTANT-TVA TO WS-AFFICHAGE
           DISPLAY "Montant TVA : " WS-AFFICHAGE " EUR"
           MOVE WS-PRIX-TTC TO WS-AFFICHAGE
           DISPLAY "Prix TTC    : " WS-AFFICHAGE " EUR".
```

#### Résultat
```
=== CALCUL DE TVA ===
Prix HT     :    100.00 EUR
Taux TVA    :     20.00 %
Montant TVA :     20.00 EUR
Prix TTC    :    120.00 EUR
```

---

## Résumé - Points clés

| Élément | À retenir |
|---------|-----------|
| **4 DIVISIONS** | IDENTIFICATION → ENVIRONMENT → DATA → PROCEDURE |
| **Colonne 7** | `*` = commentaire, `-` = continuation |
| **Zone A** | col 8-11 : DIVISION, SECTION, paragraphes, 01/77 |
| **Zone B** | col 12-72 : instructions, niveaux 02-49 |
| **Nommage** | 1-30 car., tirets OK, underscores NON |
| **Point** | Obligatoire pour terminer chaque instruction |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Interface ISPF](02-ispf-commandes.md) |

---
*Formation COBOL - Module COBOL*
