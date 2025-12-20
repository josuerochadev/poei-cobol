# Chapitre I - Structure d'un Programme COBOL

Ce chapitre présente les différentes unités qui composent un programme COBOL appelées **DIVISION**, ainsi que les règles de codage d'une ligne de code.

---

## I-1 : Langage de programmation Mainframe

### Contexte
- **Mainframe** = ordinateur central pour traitement massif
- Haute disponibilité (99,999% uptime)
- Traitement batch (lots de données)
- Les mainframes restent les premiers supports de traitement des langages de programmation

### Langages Mainframe principaux

| Langage | Usage |
|---------|-------|
| **COBOL** | Logique métier, batch, applications de gestion |
| **PL/I** | Applications mixtes (scientifique + gestion) |
| **JCL** | Orchestration des jobs |
| **Assembleur** | Système, performance critique |
| **REXX** | Scripts et automatisation |
| **Java, C, C++** | Applications modernes, interfaces |

### COBOL aujourd'hui

Selon le *Guide to Z/OS application programming* d'IBM et *Discussion of mainframe Languages* de CA Technologies, **COBOL reste le langage le plus utilisé sur les mainframes** :

- **200+ milliards** de lignes de code en production
- **95%** des transactions ATM
- **80%** des transactions financières mondiales
- Combinaison de langages traditionnels (COBOL, PL/I) et modernes (Java, C++)

---

## I-2 : Présentation de COBOL et ses 60 ans

### Origine (1959)

- Créé par le **DoD** (Département de la Défense US)
- **CODASYL** (Conference on Data Systems Language) : comité de standardisation
- **Grace Hopper** : figure fondatrice, pionnière de la programmation

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

### Évolution et versions majeures

| Année | Version | Événement / Apports |
|-------|---------|---------------------|
| 1959 | - | Développé par CODASYL |
| 1961 | COBOL-61 | Première version standardisée |
| 1968 | COBOL-68 | Approuvé par l'ANSI |
| 1974 | COBOL-74 | Améliorations structurelles |
| 1985 | COBOL-85 | END-IF, END-PERFORM (programmation structurée) |
| 2002 | COBOL-2002 | Orienté objet |
| 2014 | COBOL-2014 | Support XML, JSON |
| 2023 | COBOL-2023 | Dernière norme ISO |

### Actualité et modernisation

- **Avril 2021** : IBM annonce un nouveau compilateur *IBM COBOL for Linux on x86*
- Héritage important d'applications COBOL dans les banques et administrations publiques
- Modernisation continue des compilateurs COBOL dans les environnements z/OS et Linux par IBM

> **Exemple concret** : Lors de la pandémie COVID-19, les États de l'Iowa, New Jersey, Kansas et Oklahoma ont fait appel à des développeurs COBOL pour maintenir leurs systèmes de gestion du chômage, démontrant la persistance et l'importance de ce langage.

---

## I-3 : Description technique

### 3-1 : Spécificités de COBOL

- **Premier langage de haut niveau** codé en mots anglais simples
- Utilisé comme **langage d'auto-documentation** (code lisible)
- Efficace et souple pour gérer un **volume important de données**
- **Compatible avec ses versions précédentes** (rétrocompatibilité)
- **Référentiel de messages d'erreur** bien développé
- **Précision décimale** garantie (crucial pour la finance)

### 3-2 : Évolution structurelle

- **Avant COBOL-85** : GO TO partout (code spaghetti)
- **Après COBOL-85** : programmation structurée (END-IF, END-PERFORM, END-EVALUATE)

### 3-3 : Caractéristiques techniques

| Caractéristique | Description |
|-----------------|-------------|
| Paradigme | Procédural |
| Typage | Statique (clause PICTURE) |
| Structure | 4 divisions |
| Fichiers | Gestion native intégrée |
| Calcul | Décimal fixe (précision garantie) |
| Exécution | Compilé |
| Portabilité | Standard exécutable sur différentes machines (AS/400, z/OS, Linux) |
| Outils | Débogage et tests sur différentes plateformes |

---

## I-4 : Composition d'un programme COBOL

### 4-1 : Les 4 DIVISIONS

Un programme COBOL comporte **quatre DIVISIONS** :

```cobol
       IDENTIFICATION DIVISION.    *> QUI ? Métadonnées du programme
       ENVIRONMENT DIVISION.       *> OÙ ? Configuration environnement
       DATA DIVISION.              *> QUOI ? Déclaration des données
       PROCEDURE DIVISION.         *> COMMENT ? Logique de traitement
```

---

### IDENTIFICATION DIVISION

La première division, **obligatoire**, fournit une documentation minimale du programme.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOM-PROGRAMME.
       AUTHOR. NOM-PROGRAMMEUR.
       INSTALLATION. DETAILS-INSTALLATION.
       DATE-WRITTEN. JJ/MM/AAAA.
       DATE-COMPILED. JJ/MM/AAAA.
       SECURITY. NIVEAU-SECURITE.
```

**Caractéristiques :**
- Première division **obligatoire**
- Les deux premières lignes sont **obligatoires**
- Chaque nom de paragraphe doit être terminé par un **point (.)**
- `PROGRAM-ID` : seul paragraphe obligatoire (1 à 30 caractères)
- Les autres paragraphes sont optionnels mais recommandés pour la documentation

---

### ENVIRONMENT DIVISION

Précise l'environnement informatique pour la compilation et l'exécution. Composée de **deux sections**.

```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SOURCE-COMPUTER. IBM-370.
         OBJECT-COMPUTER. IBM-370.
         SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
             SELECT FICHIER-ENTREE ASSIGN TO INPUTFL1
                 ORGANIZATION IS SEQUENTIAL
                 ACCESS IS SEQUENTIAL
                 FILE STATUS IS WS-STATUS.
         I-O-CONTROL.
```

#### CONFIGURATION SECTION

Spécifie les caractéristiques techniques du système. Non obligatoire dans la majorité des cas.

**SPECIAL-NAMES** permet de personnaliser certains comportements :
- `DECIMAL-POINT IS COMMA` : utilise la virgule comme séparateur décimal (format européen)

#### INPUT-OUTPUT SECTION

Décrit les fichiers du programme et les assigne à des fichiers externes. Fait la liaison entre :
- **Fichiers logiques** (définis dans le programme COBOL)
- **Fichiers physiques** (stockés sur disque, définis dans le JCL)

**Liaison JCL correspondante :**
```jcl
//INPUTFL1 DD DSN=CLIENT.LOCAL.FILE,DISP=SHR
```

---

### DATA DIVISION

Définit les variables utilisées dans le programme. Composée de **quatre sections** :

#### a) FILE SECTION

Définit la structure des fichiers de données avec la **FD** (File Description).

```cobol
       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ENTREE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F
           DATA RECORD IS ENR-ENTREE.
       01  ENR-ENTREE               PIC X(80).
```

#### b) WORKING-STORAGE SECTION

Réservée à la partie déclarative : variables temporaires et structures de données.

```cobol
       WORKING-STORAGE SECTION.
       01  PERSONNE.
           05  WS-NOM-COMPLET.
               10  WS-NOM          PIC X(30).
               10  WS-PRENOM       PIC X(30).
           05  WS-DATE-NAISSANCE.
               10  WS-JOUR         PIC 99.
               10  WS-MOIS         PIC 99.
               10  WS-ANNEE        PIC 9999.
           05  WS-ADRESSE          PIC X(50).
```

> **Note** : Les zones mémoire sont allouées à l'exécution et désallouées à la fin du programme.

#### c) LOCAL-STORAGE SECTION

Similaire à WORKING-STORAGE, mais les variables sont allouées et **initialisées à chaque exécution** du programme (utile pour les sous-programmes appelés plusieurs fois).

#### d) LINKAGE SECTION

Décrit les données reçues d'un programme externe (programme appelant).

**Programme appelant (MAIN) :**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ID-EMPLOYE           PIC 9(3) VALUE 520.
       01  WS-NOM-EMPLOYE          PIC A(20) VALUE 'MARC'.
       PROCEDURE DIVISION.
           CALL 'VERIF' USING WS-ID-EMPLOYE, WS-NOM-EMPLOYE.
           STOP RUN.
```

**Programme appelé (VERIF) :**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VERIF.
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-ID-EMPLOYE           PIC 9(3).
       01  LK-NOM-EMPLOYE          PIC A(20).
       PROCEDURE DIVISION USING LK-ID-EMPLOYE, LK-NOM-EMPLOYE.
           DISPLAY 'ID: ' LK-ID-EMPLOYE ' NOM: ' LK-NOM-EMPLOYE.
           EXIT PROGRAM.
```

---

### PROCEDURE DIVISION

Décrit la **logique de traitement** du programme avec des instructions exécutables.

- Les noms de paragraphe et de section sont définis par l'utilisateur
- Doit contenir au moins une instruction
- **Fin d'exécution** :
  - `STOP RUN` : pour les programmes **appelants** (programme principal)
  - `EXIT PROGRAM` : pour les programmes **appelés** (sous-programmes)

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INITIALISATION
           PERFORM 2000-TRAITEMENT
           PERFORM 9000-FIN
           STOP RUN.

       1000-INITIALISATION.
           DISPLAY 'Debut du programme'.

       2000-TRAITEMENT.
           DISPLAY 'Traitement en cours'.

       9000-FIN.
           DISPLAY 'Fin du programme'.
```

---

## 4-2 : Règles de codage (colonnes 1-80)

```
Colonnes:  1-6      7       8-11      12-72       73-80
         ┌──────┬───────┬────────┬────────────┬─────────┐
         │ Seq. │ Indic │ Zone A │   Zone B   │ Ignoré  │
         └──────┴───────┴────────┴────────────┴─────────┘
```

| Zone | Colonnes | Contenu |
|------|----------|---------|
| Séquence | 1-6 | Numérotation des lignes (optionnel) |
| Indicateur | 7 | Caractère spécial (voir tableau) |
| Zone A | 8-11 | DIVISION, SECTION, paragraphes, niveaux 01/77 |
| Zone B | 12-72 | Instructions, niveaux 02-49, clauses |
| Identification | 73-80 | Réservé au système (ignoré) |

### Colonne 7 - Indicateurs

| Caractère | Signification |
|-----------|---------------|
| `*` | Ligne de commentaire |
| `-` | Continuation de ligne (chaîne longue) |
| `/` | Saut de page à l'impression |
| `D` ou `d` | Ligne de debug (activée avec option compilateur) |
| ` ` (espace) | Ligne normale de code |

### Zone A (colonnes 8-11)

Doivent commencer en Zone A :
- Noms de DIVISION
- Noms de SECTION
- Noms de paragraphes
- Niveaux 01 et 77
- Indicateurs FD et SD

### Zone B (colonnes 12-72)

Doivent être en Zone B :
- Instructions COBOL
- Niveaux 02 à 49
- Clauses et options

---

## 4-3 : Règles d'utilisation des mots et littéraux

### 4-3-1 : Mots COBOL réservés

Mots prédéfinis du langage :
```
ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE
MOVE, DISPLAY, ACCEPT, IF, ELSE, END-IF
PERFORM, UNTIL, TIMES, VARYING, THRU
READ, WRITE, OPEN, CLOSE, FILE, STOP, RUN
```

**Constantes figuratives** (valeurs prédéfinies) :
```
ZERO, ZEROS, ZEROES     → valeur numérique 0
SPACE, SPACES           → caractère espace
HIGH-VALUE, HIGH-VALUES → valeur hexadécimale maximale (X'FF')
LOW-VALUE, LOW-VALUES   → valeur hexadécimale minimale (X'00')
QUOTE, QUOTES           → guillemet
ALL 'x'                 → répétition d'un caractère
```

### 4-3-2 : Règles de nommage des variables

| Règle | Exemple valide | Exemple invalide |
|-------|----------------|------------------|
| 1 à 30 caractères | `CLIENT-NOM` | Nom trop long (>30 car.) |
| Lettres, chiffres, tirets | `TOTAL-2024` | `TOTAL_2024` (underscore) |
| Pas de tiret au début/fin | `MON-TOTAL` | `-TOTAL`, `TOTAL-` |
| Pas de mot réservé seul | `WS-ADD` | `ADD` |
| Au moins une lettre | `LIGNE1` | `12345` |

### 4-3-3 : Littéraux

**Littéraux alphanumériques** (max 160 caractères) :
```cobol
       MOVE "Texte entre guillemets" TO WS-VAR.
       MOVE 'Texte entre apostrophes' TO WS-VAR.
       MOVE 'L''apostrophe est doublée' TO WS-VAR.
```

> Pour inclure une apostrophe dans une chaîne délimitée par apostrophes, il faut la doubler.

**Littéraux numériques** (chiffres 0-9, signe, point décimal) :

| Valide | Invalide | Raison |
|--------|----------|--------|
| `652` | `9,000` | Pas de virgule |
| `524.23` | `25+` | Signe à droite |
| `+871` | `25.` | Point en fin |
| `-362` | `.25` | Point au début |

> Le signe (+/-) doit être à gauche, le point décimal ne peut être ni au début ni à la fin.

### 4-3-4 : Commentaires

Une ligne de commentaire est indiquée par `*` en colonne 7 :

```cobol
      *================================================================
      * CE PROGRAMME CALCULE LA TVA
      * AUTEUR : FORMATION COBOL
      *================================================================
       PROCEDURE DIVISION.
      * Paragraphe principal
       0000-DEBUT.
```

### 4-3-5 : Ponctuation

- **Point (.)** : **obligatoire** pour terminer chaque instruction/paragraphe
- **Virgule (,)** : optionnelle, pour la lisibilité uniquement

---

## 4-4 : Exemple complet

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL-TVA.
       AUTHOR. FORMATION-COBOL.
       DATE-WRITTEN. 2024-01-15.
      *================================================================
      * Programme exemple : Calcul de TVA
      * Démontre la structure des 4 divisions
      *================================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRIX-HT              PIC 9(5)V99   VALUE 100,00.
       01  WS-TAUX-TVA             PIC 9(2)V99   VALUE 20,00.
       01  WS-MONTANT-TVA          PIC 9(5)V99   VALUE 0.
       01  WS-PRIX-TTC             PIC 9(6)V99   VALUE 0.
       01  WS-AFFICHAGE            PIC Z(5)9,99.

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

**Résultat :**
```
=== CALCUL DE TVA ===
Prix HT     :    100,00 EUR
Taux TVA    :     20,00 %
Montant TVA :     20,00 EUR
Prix TTC    :    120,00 EUR
```

---

## I-5 : Créer et exécuter un programme COBOL sur Mainframe

### Étapes de création

1. **Ouvrir une session** sur le mainframe z/OS via l'interface ISPF

2. **Créer les trois librairies** (PDS) nécessaires :

| Dataset | Contenu | RECFM | LRECL |
|---------|---------|-------|-------|
| `userid.COBOL.SOURCE` | Code source et JCL | FB | 80 |
| `userid.COBOL.LINK` | Modules objets | FB | 80 |
| `userid.COBOL.LOAD` | Programmes exécutables | U | - |

> **Convention de nommage** : `PROJET.GROUPE.TYPE`

3. **Créer un membre** dans la librairie SOURCE (le programme COBOL)

4. **Créer le JCL** de compilation et link-edit

5. **Soumettre le job** pour compilation et exécution

### Paramètres des datasets

#### SOURCE et LINK (RECFM=FB)
```
Organization  : PO (Partitioned)
Record format : FB (Fixed Blocked)
Record length : 80
Block size    : 3120
```

#### LOAD (RECFM=U)
```
Organization  : PO (Partitioned)
Record format : U (Undefined)
Block size    : 6144
```

### Exemple de JCL de compilation

```jcl
//COMPILE  JOB (ACCT),'COMPILATION',CLASS=A,MSGCLASS=X
//STEP1    EXEC IGYWCL
//COBOL.SYSIN DD DSN=&SYSUID..COBOL.SOURCE(CALCTVAV),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..COBOL.LOAD(CALCTVAV),DISP=SHR
```

---

## Résumé - Points clés

| Élément | À retenir |
|---------|-----------|
| **4 DIVISIONS** | IDENTIFICATION → ENVIRONMENT → DATA → PROCEDURE |
| **Obligatoires** | IDENTIFICATION DIVISION et PROGRAM-ID |
| **Colonne 7** | `*` commentaire, `-` continuation, `/` saut page, `D` debug |
| **Zone A (8-11)** | DIVISION, SECTION, paragraphes, niveaux 01/77 |
| **Zone B (12-72)** | Instructions, niveaux 02-49 |
| **Nommage** | 1-30 car., tirets OK, underscores NON |
| **Point** | Obligatoire pour terminer chaque instruction |
| **Fin programme** | `STOP RUN` (principal) ou `EXIT PROGRAM` (sous-programme) |
| **Constantes** | ZEROS, SPACES, HIGH-VALUE, LOW-VALUE |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Interface ISPF](02-ispf-commandes.md) |

---
*Formation COBOL - M2i Formation*
