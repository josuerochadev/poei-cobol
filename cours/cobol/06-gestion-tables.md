# Chapitre VI - Gestion des Tables (OCCURS)

Ce chapitre traite les données déclarées dans un programme COBOL sous forme de Table (clause `OCCURS`).

---

## VI-1 Définition d'une Table

Une **table** (tableau) stocke plusieurs occurrences d'un même type de données sous un même nom.

Caractéristiques :
- Les éléments sont référencés par leur **position** dans la table
- La déclaration se fait avec la clause `OCCURS`
- Les tables peuvent contenir des éléments élémentaires ou des groupes (niveaux 02-49)

```
┌─────────────────────────────────────────────┐
│  TABLE-CLIENTS                              │
├─────────────────────────────────────────────┤
│  [1] DUPONT    │ PARIS     │ 001234        │
│  [2] MARTIN    │ LYON      │ 005678        │
│  [3] DURAND    │ MARSEILLE │ 009012        │
└─────────────────────────────────────────────┘
```

| Table | Fichier |
|-------|---------|
| En mémoire (WORKING-STORAGE) | Sur disque |
| Accès direct par indice | Accès séquentiel ou indexé |
| Taille limitée | Taille quasi illimitée |

---

## VI-2 Déclaration d'une Table (OCCURS)

```cobol
niveau nom-donnee OCCURS nombre TIMES
        [ASCENDING/DESCENDING KEY IS nom-cle]
        [INDEXED BY nom-index]
        PIC ...
```

**Règles :**
- Pas de OCCURS au niveau 01 ou 77
- Niveaux 02 à 49 autorisés
- VALUE interdit directement sur OCCURS

**Exemple 1 : Table simple de mois**
```cobol
       01  ANNEE.
           05  MOIS OCCURS 12 TIMES PIC X(10).
```

**Exemple 2 : Table de notes d'étudiants**
```cobol
       01  TAB-NOTES.
           02  NOTES OCCURS 25.
               05  NOTE-MATHS    PIC 99V99.
               05  NOTE-FRANCAIS PIC 99V99.
               05  NOTE-PHYSIQUE PIC 99V99.

      *> Mise à jour
       MOVE 12.50 TO NOTE-MATHS(1).
       MOVE 15.00 TO NOTE-FRANCAIS(1).
       MOVE 14.00 TO NOTE-PHYSIQUE(1).
```

**Exemple complet avec résultat :**
```cobol
       01  TABLE-TRAVAIL.
           05  VAR1 PIC A(10) VALUE 'TUTORIALS' OCCURS 5 TIMES.

       PROCEDURE DIVISION.
           DISPLAY "TABLE-TRAVAIL : " TABLE-TRAVAIL.
      *> Résultat : TABLE-TRAVAIL : TUTORIALS TUTORIALS TUTORIALS TUTORIALS TUTORIALS
```

---

## VI-3 Tables à plusieurs dimensions

### Table 1D (vecteur)
```cobol
       01  TABLE-MOIS.
           05  MOIS OCCURS 12 TIMES   PIC X(10).
```

### Initialisation avec REDEFINES
```cobol
       01  TABLE-MOIS-INIT.
           05  FILLER  PIC X(10) VALUE 'JANVIER'.
           05  FILLER  PIC X(10) VALUE 'FEVRIER'.
           ...
       01  TABLE-MOIS-R REDEFINES TABLE-MOIS-INIT.
           05  MOIS OCCURS 12 TIMES   PIC X(10).
```

### Table 2D (matrice)

Une table bidimensionnelle est une table définie à l'intérieur d'une autre table :

```cobol
       01  TABLE-TEST.
           05  VAR1 OCCURS 10 TIMES.              *> Table externe
               10  VAR2 PIC A(10).
               10  VAR3 OCCURS 5 TIMES.           *> Table interne
                   15  WS-D PIC X(6).
```

**Accès aux éléments :**
- `VAR1(2)` : le 2ème élément de la table externe (et tous ses sous-éléments)
- `VAR3(2,4)` : le 4ème élément de la table interne, dans le 2ème élément externe

```cobol
       01  TABLE-VENTES.
           05  REGION OCCURS 4 TIMES.
               10  TRIMESTRE OCCURS 4 TIMES  PIC 9(6).
```

**Accès :** `TRIMESTRE(2, 3)` = Région 2, Trimestre 3

### Table 3D
```cobol
       01  TABLE-3D.
           05  ANNEE OCCURS 5 TIMES.
               10  REGION OCCURS 4 TIMES.
                   15  MOIS OCCURS 12 TIMES  PIC 9(6).
```

**Accès :** `MOIS(annee, region, mois)`

---

## VI-4 Table à taille variable (DEPENDING ON)

```cobol
       01  WS-NB-ARTICLES     PIC 99 VALUE 0.

       01  TABLE-COMMANDE.
           05  ARTICLE OCCURS 1 TO 50 TIMES
               DEPENDING ON WS-NB-ARTICLES.
               10  ART-CODE    PIC X(5).
               10  ART-QTE     PIC 9(3).
```

**Points clés :**
- Mémoire allouée pour le MAX (50)
- Taille logique = valeur de WS-NB-ARTICLES
- Variable compteur déclarée AVANT la table

**Utilisation :**
```cobol
       ADD 1 TO WS-NB-ARTICLES
       MOVE 'A001' TO ART-CODE(WS-NB-ARTICLES)
```

---

## VI-5 Indice d'adressage

Variable numérique classique pour accéder aux éléments.

```cobol
       01  WS-I           PIC 99 VALUE 0.

       PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 20
           ADD NOTE(WS-I) TO WS-SOMME
       END-PERFORM
```

**Calcul d'adresse :** `Adresse = Base + (i - 1) × taille_element`

**Inconvénient :** Calcul à chaque accès (moins performant)

---

## VI-6 Index (INDEXED BY)

Pointeur optimisé contenant le déplacement en octets.

```cobol
       01  TABLE-PRODUITS.
           05  PRODUIT OCCURS 100 TIMES
               INDEXED BY IDX-PROD.
               10  PROD-CODE   PIC X(5).
               10  PROD-NOM    PIC X(20).
```

### Indice vs Index

| Aspect | Indice | Index |
|--------|--------|-------|
| Déclaration | Variable PIC 9 séparée | INDEXED BY dans OCCURS |
| Contient | Numéro (1, 2, 3...) | Déplacement octets |
| Performance | Calcul à chaque accès | Accès direct |
| Manipulation | MOVE, ADD, COMPUTE | SET uniquement |
| Affichage | DISPLAY possible | Conversion nécessaire |

### SEARCH (recherche)

```cobol
      * Recherche séquentielle
       SET IDX-PROD TO 1
       SEARCH PRODUIT
           AT END DISPLAY 'Non trouve'
           WHEN PROD-CODE(IDX-PROD) = 'A001'
               DISPLAY 'Trouve !'
       END-SEARCH

      * Recherche binaire (table triée)
       SEARCH ALL PRODUIT
           AT END DISPLAY 'Non trouve'
           WHEN PROD-CODE(IDX-PROD) = 'A001'
               DISPLAY 'Trouve !'
       END-SEARCH
```

| Instruction | Type | Prérequis | Complexité |
|-------------|------|-----------|------------|
| SEARCH | Séquentielle | SET index à 1 | O(n) |
| SEARCH ALL | Binaire | KEY, table triée | O(log n) |

---

## VI-7 Instruction SET

**Seule façon de manipuler un index.**

### SET TO (initialisation)
```cobol
       SET IDX-PROD TO 1              *> Valeur littérale
       SET IDX-PROD TO WS-I           *> Depuis variable
       SET IDX-SAVE TO IDX-PROD       *> Copie index
       SET WS-I TO IDX-PROD           *> Index vers variable
```

### SET UP BY (incrémenter)
```cobol
       SET IDX-PROD UP BY 1           *> Avancer de 1
       SET IDX-PROD UP BY 5           *> Avancer de 5
```

### SET DOWN BY (décrémenter)
```cobol
       SET IDX-PROD DOWN BY 1         *> Reculer de 1
```

### Comparaison

| Opération | Index | Indice |
|-----------|-------|--------|
| Initialiser | `SET idx TO 1` | `MOVE 1 TO ws-i` |
| Incrémenter | `SET idx UP BY 1` | `ADD 1 TO ws-i` |
| Décrémenter | `SET idx DOWN BY 1` | `SUBTRACT 1 FROM ws-i` |
| Afficher | `SET ws-i TO idx` puis DISPLAY | `DISPLAY ws-i` |

---

## Résumé

| Concept | Syntaxe clé |
|---------|-------------|
| Table fixe | `OCCURS n TIMES` |
| Table variable | `OCCURS n1 TO n2 TIMES DEPENDING ON var` |
| Indice | Variable PIC 9, accès `TABLE(ws-i)` |
| Index | `INDEXED BY idx`, manipulation par SET |
| Recherche séquentielle | `SEARCH table AT END ... WHEN ...` |
| Recherche binaire | `SEARCH ALL table` (KEY + triée) |
| Initialiser table | REDEFINES avec VALUE sur FILLER |

---

## VI-8 : Exercices pratiques

### Exercice 1 : Indice d'adressage (subscript)

```cobol
       01  TABLE-TEST.
           05  VAR1 OCCURS 3 TIMES.
               10  VAR2 PIC 9(2).
               10  VAR3 OCCURS 2 TIMES.
                   15  VAR4 PIC A(3).

       PROCEDURE DIVISION.
           MOVE '12ABCDEF34GHIJKL56MNOPQR' TO TABLE-TEST.
           DISPLAY 'TABLE-TEST : ' TABLE-TEST.
           DISPLAY 'VAR1(1) : ' VAR1(1).
           DISPLAY 'VAR3(1,1) : ' VAR3(1, 1).
           DISPLAY 'VAR3(1,2) : ' VAR3(1, 2).
           DISPLAY 'VAR1(2) : ' VAR1(2).
           DISPLAY 'VAR3(2,1) : ' VAR3(2, 1).
           DISPLAY 'VAR3(2,2) : ' VAR3(2, 2).
```

**Résultat attendu :**
```
TABLE-TEST : 12ABCDEF34GHIJKL56MNOPQR
VAR1(1) : 12ABCDEF
VAR3(1,1) : ABC
VAR3(1,2) : DEF
VAR1(2) : 34GHIJKL
VAR3(2,1) : GHI
VAR3(2,2) : JKL
```

### Exercice 2 : Instruction SET sur les Index

```cobol
       01  TABLE-TEST.
           05  VAR1 OCCURS 3 TIMES INDEXED BY I.
               10  VAR2 PIC A(2).
               10  VAR3 OCCURS 2 TIMES INDEXED BY J.
                   15  VAR4 PIC X(3).

       PROCEDURE DIVISION.
           MOVE '12ABCDEF34GHIJKL56MNOPQR' TO TABLE-TEST.
           SET I J TO 1.
           DISPLAY VAR3(I,J).
           SET I J UP BY 1.
           DISPLAY VAR3(I,J).
```

**Résultat attendu :**
```
ABC
JKL
```

### TD : Relevé Bancaire (aperçu)

Le TD complet utilise 5 tables pour gérer un relevé bancaire :

| Table | Contenu | Taille |
|-------|---------|--------|
| TAG | Agences | 37 car |
| TCLT | Clients | 54 car |
| TRIB | RIB clients | 50 car |
| TMVTC | Mouvements | 27 car |
| WBUFFER | Buffer global | 55 car |

Objectif : Charger un buffer, puis dispatcher les enregistrements dans les tables appropriées selon un identifiant de type (A=Agence, C=Client, M=Mouvement, R=RIB).

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Traitement Conditionnel](05-traitement-conditionnel.md) | [Chapitre VII - Gestion Fichiers](07-gestion-fichiers.md) |

---
*Formation COBOL - Module COBOL*
