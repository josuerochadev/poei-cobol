# Exercices COBOL

## Organisation

Les exercices sont organisés en trois parties :

```
exercices/cobol/
├── theorie/          # QCM pour valider les connaissances
├── pratique/         # Exercices hands-on par chapitre
└── fil-rouge/        # → Voir projets/fil-rouge/ (projet capstone)
```

---

## Partie Théorique (QCM)

Les QCM permettent de valider la compréhension des concepts avant la pratique.

| Fichier | Chapitre | Questions |
|---------|----------|-----------|
| `theorie/qcm-01-structure-programme.md` | I - Structure d'un Programme | 25 |
| `theorie/qcm-02-ispf-commandes.md` | II - Interface ISPF | 23 |
| `theorie/qcm-03-declaration-variables.md` | III - Déclaration Variables | 25 |
| `theorie/qcm-04-operations-donnees.md` | IV - Opérations Données | 25 |
| `theorie/qcm-05-traitement-conditionnel.md` | V - Traitement Conditionnel | 25 |
| `theorie/qcm-06-gestion-tables.md` | VI - Gestion des Tables | 25 |
| `theorie/qcm-07-gestion-fichiers.md` | VII - Gestion des Fichiers | 25 |
| `theorie/qcm-08-operations-es.md` | VIII - Opérations E/S | 25 |
| `theorie/qcm-09-sous-programmes.md` | IX - Sous-programmes | 25 |
| `theorie/qcm-10-traitement-fichiers.md` | X - Traitement Fichiers | 25 |
| `theorie/qcm-11-tri-interne.md` | XI - Tri Interne | 25 |
| `theorie/qcm-12-fichier-impression.md` | XII - Fichier Impression | 25 |

---

## Partie Pratique

Les exercices pratiques sont organisés par chapitre du cours.

**Convention de nommage :** `CXX-SUJET.cbl` où XX = numéro de chapitre

### Chapitre 02 - ISPF et premiers programmes

| Programme | Description |
|-----------|-------------|
| `C02-DISPLAY.cbl` | DISPLAY, chaînes de caractères, continuation, debug |
| `C02-DECIMAL.cbl` | Nombres décimaux (V), signés (S), édition |

### Chapitre 03 - Déclaration des variables

| Programme | Description |
|-----------|-------------|
| `C03-NIVEAU88.cbl` | Niveau 88 (conditions), niveau 66 (RENAMES) |
| `C03-REDEFINES.cbl` | INITIALIZE, édition flottante (++), REDEFINES |

### Chapitre 04 - Opérations sur les données

| Programme | Description |
|-----------|-------------|
| `C04-ARITHM.cbl` | ADD, SUBTRACT, MULTIPLY, DIVIDE, ON SIZE ERROR |
| `C04-FACTURE.cbl` | COMPUTE, calcul facture avec TVA et remises |

### Chapitre 05 - Traitement conditionnel

| Programme | Description |
|-----------|-------------|
| `C05-EVALUATE.cbl` | EVALUATE avec ALSO (notes examen + stage) |
| `C05-PERFORM.cbl` | PERFORM VARYING (boucle simple avec somme) |
| `C05-BOUCLES.cbl` | PERFORM avec 3 boucles imbriquées |

### Chapitre 06 - Gestion des Tables

| Programme | Description |
|-----------|-------------|
| `C06-TINDICE.cbl` | Indice d'adressage des tables (subscript) |
| `C06-TINDEX.cbl` | Instruction SET sur les index |
| `C06-BANQUE01.cbl` | TD Banque - Déclaration des 5 tables |
| `C06-BANQUE02.cbl` | TD Banque - Chargement buffer et dispatch |
| `C06-BANQUE03.cbl` | TD Banque - Fichiers E/S (SEQUENTIAL → INDEXED) |

### Chapitre 07 - Gestion des Fichiers

| Programme | Description |
|-----------|-------------|
| `C07-EMPLOYE-WRITE.cbl` | Écriture fichier SEQUENTIAL (OUTPUT) |
| `C07-EMPLOYE-READ.cbl` | Lecture fichier SEQUENTIAL (INPUT) |
| `C07-EMPLOYE-EXTEND.cbl` | Ajout en fin de fichier (EXTEND) |

### Chapitre 08 - Opérations E/S avancées

| Programme | Description |
|-----------|-------------|
| `C08-EMPL-WRITE.cbl` | Création fichier EMPLOYE étendu |
| `C08-EMPL-PRINT.cbl` | Lecture et affichage format tableau |
| `C08-RRDS-WRITE.cbl` | Création fichier RRDS (RELATIVE) |
| `C08-RRDS-READ.cbl` | Lecture enregistrement N°6 (ACCESS RANDOM) |
| `C08-RRDS-ADD.cbl` | Écriture enregistrement à position spécifique |
| `C08-RRDS-REWRITE.cbl` | Modification enregistrement N°4 |
| `C08-RRDS-DELETE.cbl` | Suppression enregistrement N°3 |
| `C08-RRDS-LIST.cbl` | Liste complète du fichier RRDS |
| `C08-RELEVE-INIT.cbl` | Création fichier BUFFER.DAT |
| `C08-RELEVE.cbl` | Dispatch vers fichiers KSDS |

### Chapitre 09 - Programmes et Sous-programmes

| Programme | Description |
|-----------|-------------|
| `C09-PERSREV.cbl` | Programme principal calcul revenu annuel |
| `C09-CALREV.cbl` | Sous-programme calcul (LINKAGE SECTION) |
| `C09-APPELANT.cbl` | Programme appelant (TVA, validation, tri) |
| `C09-CALCUL.cbl` | Sous-programme calcul TVA |
| `C09-VALID.cbl` | Sous-programme validation données |
| `C09-TRIEUR.cbl` | Sous-programme tri de tableau |
| `C09-MODIF.cbl` | Démonstration BY REFERENCE |
| `C09-BYREF-DEMO.cbl` | BY REFERENCE vs BY CONTENT |

### Chapitre 10 - Traitement des Fichiers (synthèse)

| Programme | Description |
|-----------|-------------|
| `C10-OPTIONAL.cbl` | Clause OPTIONAL |
| `C10-ALTKEY.cbl` | ALTERNATE RECORD KEY |
| `C10-PERS-CREATE.cbl` | Création fichier KSDS PERSONNEL |
| `C10-PERS-LIST.cbl` | Lecture séquentielle |
| `C10-PERS-BYSS.cbl` | Lecture par clé secondaire |
| `C10-PERS-UPDATE.cbl` | Modification (REWRITE) |
| `C10-PERS-ADD.cbl` | Ajout nouveau salarié |
| `C10-PERS-START.cbl` | START >= |

### Chapitre 11 - Tri Interne (SORT / MERGE)

| Programme | Description |
|-----------|-------------|
| `C11-SORT-SIMPLE.cbl` | SORT avec USING/GIVING |
| `C11-SORT-INPUT.cbl` | INPUT PROCEDURE avec RELEASE |
| `C11-SORT-OUTPUT.cbl` | OUTPUT PROCEDURE avec RETURN |
| `C11-MERGE.cbl` | MERGE - fusion de fichiers |
| `C11-SORT-COMPLET.cbl` | INPUT + OUTPUT combinées |
| `C11-CLIENT-CREATE.cbl` | Création fichier CLIENT.PS |
| `C11-CLIENT-TRI-ASC.cbl` | TRI ASCENDING |
| `C11-CLIENT-TRI-DESC.cbl` | TRI DESCENDING |
| `C11-CLIENT-TRI-PROC.cbl` | TRI PROCÉDURAL |
| `C11-CLIENT-SPLIT.cbl` | Séparation par clé |
| `C11-CLIENT-MERGE.cbl` | MERGE des Data Sets |

### Chapitre 12 - Fichier d'impression

| Programme | Description |
|-----------|-------------|
| `C12-EDITION-SIMPLE.cbl` | Insertion simple (B, /, 0) |
| `C12-EDITION-MONTANT.cbl` | Suppression zéros (Z), astérisques (*) |
| `C12-EDITION-SIGNE.cbl` | Signes (+, -), CR/DB |
| `C12-FACTURE.cbl` | Édition facture complète |
| `C12-RAPPORT.cbl` | Rapport multi-pages |
| `C12-RELEVE-PREP.cbl` | Préparation fichier AIMPRIM |
| `C12-RELEVE-PRINT.cbl` | Impression relevé bancaire |

---

## Projet Fil Rouge

Le projet fil rouge est un système de gestion financière complet avec 21 exercices progressifs.

**Emplacement :** [`projets/fil-rouge/`](../../projets/fil-rouge/)

Ce projet met en pratique l'ensemble des compétences COBOL et JCL acquises pendant la formation.

---

## Compilation

### Programme simple

```bash
cd pratique/chapitre-06
cobc -x C06-TINDICE.cbl -o C06-TINDICE
./C06-TINDICE
```

### Avec sous-programmes (Chapitre 09)

```bash
cd pratique/chapitre-09

# Compiler les modules
cobc -c C09-CALREV.cbl
cobc -c C09-PERSREV.cbl

# Édition de liens
cobc -x -o C09-PERSREV C09-PERSREV.o C09-CALREV.o

# Exécuter
./C09-PERSREV
```

### Avec mode debug (ligne D en colonne 7)

```bash
cobc -x -fdebugging-line C06-BANQUE01.cbl -o C06-BANQUE01
```

---

## Progression recommandée

1. **Théorie** : Faire le QCM du chapitre pour valider la compréhension
2. **Pratique** : Réaliser les exercices hands-on
3. **Révision** : Refaire le QCM après la pratique
4. **Fil Rouge** : Appliquer les compétences sur le projet complet

---
*Formation COBOL - M2i Formation*
