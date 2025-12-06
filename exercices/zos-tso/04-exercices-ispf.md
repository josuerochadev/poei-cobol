# Exercices ISPF/PDF - Chapitre IV

## Objectifs

Ces exercices permettent de pratiquer l'utilisation d'**ISPF/PDF** sur un émulateur Hercules (TK4-/TK5) :
- Gestion des membres dans les bibliothèques (PDS)
- Navigation et affichage des volumes
- Utilisation avancée de l'éditeur ISPF
- Commandes COLS, TABS, BNDS, MASK

---

## Prérequis

- Émulateur Hercules avec TK4- ou TK5
- Session TSO/ISPF active
- Userid : FTEST (ou votre userid)
- Avoir créé `FTEST.PROD.LIBTEST` (voir exercices chapitre III)

---

## Rappel : Commandes ligne de l'éditeur

```
┌─────────────────────────────────────────────────────────────────┐
│                COMMANDES LIGNE ÉDITEUR                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  INSERTION/SUPPRESSION                                          │
│  I/In    Insérer ligne(s)       D/Dn/DD   Supprimer            │
│                                                                  │
│  COPIE/DÉPLACEMENT                                              │
│  C/Cn/CC Copier                 M/Mn/MM   Déplacer              │
│  A       Après (destination)    B         Avant (destination)   │
│                                                                  │
│  RÉPÉTITION/DÉCALAGE                                            │
│  R/Rn/RR Répéter                >/>>      Décaler droite        │
│                                 </<<      Décaler gauche        │
│                                                                  │
│  EXCLUSION/AFFICHAGE                                            │
│  X/Xn/XX Exclure (masquer)      S/Sn      Afficher (show)       │
│  F       First (première)       L         Last (dernière)       │
│                                                                  │
│  SPÉCIALES                                                      │
│  COLS    Règle des colonnes     TABS      Tabulations           │
│  BNDS    Bornes (limites)       MASK      Masque d'insertion    │
│  UC      Majuscules             LC        Minuscules            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Partie 1 : Gestion des membres de bibliothèque

### Exercice 1 : Lister le contenu d'une Library

**Objectif** : Afficher la liste des membres de `FTEST.PROD.LIBTEST`

#### Méthode 1 : Option 3.1 (Library Utility)

1. Aller à **Option 3.1** (taper `=3.1`)

2. Remplir l'écran :
```
┌─────────────────────────────────────────────────────────────────┐
│                 LIBRARY UTILITY                                  │
│                                                                  │
│  Option ===>           (laisser vide pour afficher les membres) │
│                                                                  │
│  ISPF Library:                                                  │
│     Project  ===> FTEST                                         │
│     Group    ===> PROD                                          │
│     Type     ===> LIBTEST                                       │
│     Member   ===>           (laisser vide)                      │
│                                                                  │
│  Other Partitioned Data Set:                                    │
│     Data Set Name ===> 'FTEST.PROD.LIBTEST'                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée** pour voir la liste des membres

#### Méthode 2 : Option 3.4 (Dslist) avec commande `m`

1. Aller à **Option 3.4** (`=3.4`)
2. Dsname Level : `FTEST.PROD`
3. Taper `m` devant `FTEST.PROD.LIBTEST` pour voir les membres

**Résultat attendu** : Liste des membres avec statistiques (taille, date création, etc.)

---

### Exercice 2 : Créer un nouveau membre

**Objectif** : Créer un membre `MEMBRE01` dans `FTEST.PROD.LIBTEST`

#### Méthode ISPF (Option 2 - Edit)

1. Aller à **Option 2** (Edit) ou taper `=2`

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│                    EDIT - ENTRY PANEL                            │
│                                                                  │
│  Other Partitioned Data Set:                                    │
│     Data Set Name ===> 'FTEST.PROD.LIBTEST(MEMBRE01)'          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée** - L'éditeur s'ouvre sur un nouveau membre vide

4. Saisir du contenu :
```
****** ***************************** Top of Data ******************
''''''        CECI EST MON PREMIER MEMBRE
''''''        CREE DANS LA LIBRARY FTEST.PROD.LIBTEST
''''''        DATE: 2025
****** **************************** Bottom of Data ****************
```
*(Remplacer `''''''` par les numéros de ligne)*

5. Appuyer **PF3** (End) pour sauvegarder et quitter

**Message attendu** : `MEMBER MEMBRE01 SAVED`

---

### Exercice 3 : Afficher les caractéristiques d'une Library

**Objectif** : Consulter les attributs de `FTEST.PROD.LIBTEST`

#### Méthode ISPF (Option 3.4)

1. Aller à **Option 3.4** (`=3.4`)
2. Dsname Level : `FTEST.PROD`
3. Taper `i` (info) devant `FTEST.PROD.LIBTEST`

**Résultat** :
```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA SET INFORMATION                          │
│                                                                  │
│  Data Set Name  : FTEST.PROD.LIBTEST                            │
│                                                                  │
│  General Data:                          Current Allocation:     │
│   Volume serial : PUB001                 Allocated tracks : 2   │
│   Device type   : 3390                   Used tracks      : 1   │
│   Organization  : PO          ◄── Partitioned Organization     │
│   Record format : F                      Extents          : 1   │
│   Record length : 80                                            │
│   Block size    : 80                    Creation date: 2025/xxx │
│   Members       : 1           ◄── Nombre de membres            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Points à observer** :
- **Organization : PO** = Partitioned (bibliothèque)
- **Members** = Nombre de membres dans le PDS

---

### Exercice 4 : Créer une nouvelle Library

**Objectif** : Créer `FTEST.PRODPAIE.LIBRARY`

#### Méthode ISPF (Option 3.2)

1. Aller à **Option 3.2** (`=3.2`)

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│                 DATA SET UTILITY                                 │
│                                                                  │
│   Option ===> A                    ◄── A = Allocate             │
│                                                                  │
│   Data Set Name ===> FTEST.PRODPAIE.LIBRARY                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée**, remplir les attributs :
```
┌─────────────────────────────────────────────────────────────────┐
│                 ALLOCATE NEW DATA SET                            │
│                                                                  │
│   Space units       ===> TRACK                                  │
│   Primary quantity  ===> 5                                      │
│   Secondary quantity===> 2                                      │
│   Directory blocks  ===> 10        ◄── Pour un PDS             │
│   Record format     ===> FB                                     │
│   Record length     ===> 80                                     │
│   Block size        ===> 27920                                  │
│   Data set type     ===> PDS                                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

4. Appuyer **Entrée**

**Message attendu** : `DATA SET ALLOCATED`

---

### Exercice 5 : Copier un membre

**Objectif** : Copier `MEMBRE01` vers `MEMBRE02` dans la même bibliothèque

#### Méthode ISPF (Option 3.3 - Move/Copy)

1. Aller à **Option 3.3** (`=3.3`)

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│                 MOVE/COPY UTILITY                                │
│                                                                  │
│   Option ===> C                    ◄── C = Copy                 │
│                                                                  │
│   From Data Set:                                                │
│      Data Set Name ===> 'FTEST.PROD.LIBTEST'                   │
│      Member       ===> MEMBRE01                                 │
│                                                                  │
│   To Data Set:                                                  │
│      Data Set Name ===> 'FTEST.PROD.LIBTEST'                   │
│      Member       ===> MEMBRE02     ◄── Nouveau nom            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée**

**Message attendu** : `MEMBER COPIED`

#### Alternative : Depuis la liste des membres

1. Option 3.1, afficher les membres de `FTEST.PROD.LIBTEST`
2. Taper `c` devant `MEMBRE01`
3. Indiquer le nom destination `MEMBRE02`

---

### Exercice 6 : Renommer un membre

**Objectif** : Renommer `MEMBRE02` en `MEMBTEST`

#### Méthode ISPF (Option 3.1)

1. Aller à **Option 3.1** (`=3.1`)
2. Afficher les membres de `FTEST.PROD.LIBTEST`
3. Taper `r` (rename) devant `MEMBRE02`

```
┌─────────────────────────────────────────────────────────────────┐
│ LIBRARY - FTEST.PROD.LIBTEST                       Row 1 of 2  │
│ Command ===>                                                    │
│                                                                  │
│    Name     Prompt       Size   Created          Changed        │
│ r  MEMBRE02              3     2025/xxx        2025/xxx         │
│    MEMBRE01              3     2025/xxx        2025/xxx         │
│                                                                  │
│  Rename MEMBRE02 to: MEMBTEST_                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

4. Saisir `MEMBTEST` et appuyer **Entrée**

**Message attendu** : `MEMBER RENAMED`

---

## Partie 2 : Navigation volumes et fichiers

### Exercice 7 : Afficher le contenu d'un volume (disque)

**Objectif** : Lister les datasets du volume `PUB001` (ou volume disponible sur votre système)

#### Méthode ISPF (Option 3.4)

1. Aller à **Option 3.4** (`=3.4`)

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│                 DATA SET LIST UTILITY                            │
│                                                                  │
│     Dsname Level ===> *              ◄── Tous les datasets      │
│     Volume       ===> PUB001         ◄── Volume spécifique      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée**

**Résultat** : Liste de tous les datasets présents sur le volume PUB001

**Note Hercules** : Sur TK4-/TK5, les volumes courants sont `PUB001`, `PUB002`, `WORK01`, etc.

---

### Exercice 8 : Afficher les caractéristiques d'un fichier

**Objectif** : Sélectionner un dataset et afficher ses attributs

#### Méthode ISPF (Option 3.4)

1. Depuis la liste de l'exercice 7, choisir un dataset (ex: `SYS1.PROCLIB`)
2. Taper `i` (info) devant le dataset

**Observer** :
- Volume serial
- Device type (3390)
- Organization (PO, PS, VS...)
- Record format et length
- Allocation (tracks, extents)

---

### Exercice 9 : Afficher la VTOC d'un volume

**Objectif** : Consulter la VTOC (Volume Table Of Contents) du volume PUB001

La **VTOC** contient la liste de tous les fichiers sur un volume avec leurs emplacements physiques.

#### Méthode TSO (depuis ISPF Option 6)

1. Aller à **Option 6** (`=6`) ou préfixer avec `TSO`

2. Taper la commande :
```
LISTCAT LEVEL(*) VOLUME(PUB001)
```

#### Méthode ISPF (Option 3.4)

La liste obtenue à l'exercice 7 avec `Volume ===> PUB001` représente essentiellement le contenu de la VTOC pour ce volume.

**Note** : La VTOC est une zone spéciale du disque qui référence tous les datasets. Chaque entrée contient :
- Nom du dataset
- Emplacement (cylindre, piste)
- Taille allouée
- Attributs (DCB)

---

### Exercice 10 : Différence entre COPY et MOVE

**Objectif** : Comprendre la différence entre COPY et MOVE

```
┌─────────────────────────────────────────────────────────────────┐
│                    COPY vs MOVE                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   COPY (Copier)                                                 │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Duplique le membre/dataset                          │   │
│   │  • L'original RESTE en place                           │   │
│   │  • Résultat : 2 copies identiques                      │   │
│   │                                                         │   │
│   │  Avant: [SOURCE]           Après: [SOURCE]             │   │
│   │                                   [DESTINATION]         │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   MOVE (Déplacer)                                               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                                                         │   │
│   │  • Déplace le membre/dataset                           │   │
│   │  • L'original est SUPPRIMÉ                             │   │
│   │  • Résultat : 1 seule copie à la destination           │   │
│   │                                                         │   │
│   │  Avant: [SOURCE]           Après: [DESTINATION]        │   │
│   │                                   (SOURCE supprimé)     │   │
│   │                                                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   En résumé :                                                   │
│   • COPY = Duplication (Ctrl+C / Ctrl+V)                       │
│   • MOVE = Déplacement (Ctrl+X / Ctrl+V)                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Exercice pratique

1. Créer un membre `TESTCOPY` dans `FTEST.PROD.LIBTEST`
2. **COPY** `TESTCOPY` vers `TESTCOP2` - Observer : les deux existent
3. **MOVE** `TESTCOP2` vers `TESTMOVE` - Observer : `TESTCOP2` n'existe plus

---

## Partie 3 : Éditeur ISPF - Manipulation de texte

### Exercice 11 : Créer un membre avec l'éditeur

**Objectif** : Créer le membre `MEMTEST` pour les exercices suivants

1. Aller à **Option 2** (Edit)
2. Data Set Name : `'FTEST.PROD.LIBTEST(MEMTEST)'`
3. Saisir le texte suivant (environ 10 lignes) :

```
****** ***************************** Top of Data ******************
000001 *==========================================================*
000002 * EXERCICE EDITEUR ISPF                                     *
000003 * CE FICHIER SERT A PRATIQUER LES COMMANDES                *
000004 *==========================================================*
000005 LIGNE DE TEXTE NUMERO CINQ
000006 LIGNE DE TEXTE NUMERO SIX
000007 LIGNE DE TEXTE NUMERO SEPT
000008 LIGNE DE TEXTE NUMERO HUIT
000009 LIGNE DE TEXTE NUMERO NEUF
000010 LIGNE DE TEXTE NUMERO DIX
****** **************************** Bottom of Data ****************
```

4. Appuyer **PF3** pour sauvegarder

---

### Exercice 12 : Déplacer du texte APRÈS une position

**Objectif** : Déplacer les lignes 5-6 vers après la ligne 9

#### Procédure

1. Éditer `MEMTEST`

2. Marquer le bloc à déplacer :
```
MM0005 LIGNE DE TEXTE NUMERO CINQ        ◄── MM = début bloc Move
000006 LIGNE DE TEXTE NUMERO SIX
MM0007 LIGNE DE TEXTE NUMERO SEPT        ◄── MM = fin bloc... NON!
```

**Correction** : Pour déplacer lignes 5-6, mettre MM sur 5 et 6 :
```
MM0005 LIGNE DE TEXTE NUMERO CINQ        ◄── MM début
MM0006 LIGNE DE TEXTE NUMERO SIX         ◄── MM fin
000007 LIGNE DE TEXTE NUMERO SEPT
000008 LIGNE DE TEXTE NUMERO HUIT
A 0009 LIGNE DE TEXTE NUMERO NEUF        ◄── A = After (destination)
000010 LIGNE DE TEXTE NUMERO DIX
```

3. Appuyer **Entrée**

**Résultat** :
```
000001 *==========================================================*
000002 * EXERCICE EDITEUR ISPF                                     *
000003 * CE FICHIER SERT A PRATIQUER LES COMMANDES                *
000004 *==========================================================*
000005 LIGNE DE TEXTE NUMERO SEPT        ◄── Ancienne ligne 7
000006 LIGNE DE TEXTE NUMERO HUIT        ◄── Ancienne ligne 8
000007 LIGNE DE TEXTE NUMERO NEUF        ◄── Ancienne ligne 9
000008 LIGNE DE TEXTE NUMERO CINQ        ◄── Déplacée ici
000009 LIGNE DE TEXTE NUMERO SIX         ◄── Déplacée ici
000010 LIGNE DE TEXTE NUMERO DIX
```

---

### Exercice 13 : Déplacer du texte AVANT une position

**Objectif** : Déplacer les lignes 8-9 (CINQ et SIX) avant la ligne 5

#### Procédure

1. Annuler les changements : `CANCEL` sur la ligne de commande ou **PF3** puis ré-éditer

2. Marquer :
```
000004 *==========================================================*
B 0005 LIGNE DE TEXTE NUMERO SEPT        ◄── B = Before (destination)
000006 LIGNE DE TEXTE NUMERO HUIT
000007 LIGNE DE TEXTE NUMERO NEUF
MM0008 LIGNE DE TEXTE NUMERO CINQ        ◄── MM début
MM0009 LIGNE DE TEXTE NUMERO SIX         ◄── MM fin
000010 LIGNE DE TEXTE NUMERO DIX
```

3. Appuyer **Entrée**

**Résultat** : Les lignes CINQ et SIX sont maintenant AVANT SEPT.

---

### Exercice 14 : Exclusion et commandes F, L, S

**Objectif** : Pratiquer l'exclusion de lignes et les commandes d'affichage

#### Étape 1 : Créer un fichier de 19 lignes

1. Éditer `MEMTEST` et créer 19 lignes avec indentation variée :

```
000001 LIGNE 01 - DEBUT DU FICHIER
000002    LIGNE 02 - INDENTEE
000003    LIGNE 03 - INDENTEE
000004 LIGNE 04 - NORMALE
000005       LIGNE 05 - TRES INDENTEE
000006       LIGNE 06 - TRES INDENTEE
000007 LIGNE 07 - NORMALE
000008 LIGNE 08 - NORMALE
000009    LIGNE 09 - INDENTEE
000010    LIGNE 10 - INDENTEE
000011 LIGNE 11 - NORMALE
000012 LIGNE 12 - NORMALE
000013       LIGNE 13 - TRES INDENTEE
000014 LIGNE 14 - NORMALE
000015    LIGNE 15 - INDENTEE
000016 LIGNE 16 - NORMALE
000017 LIGNE 17 - NORMALE
000018    LIGNE 18 - INDENTEE
000019 LIGNE 19 - FIN DU FICHIER
```

#### Étape 2 : Exclure des lignes

1. Exclure les lignes 5 à 10 :
```
000004 LIGNE 04 - NORMALE
XX0005       LIGNE 05 - TRES INDENTEE    ◄── XX début exclusion
000006       LIGNE 06 - TRES INDENTEE
000007 LIGNE 07 - NORMALE
000008 LIGNE 08 - NORMALE
000009    LIGNE 09 - INDENTEE
XX0010    LIGNE 10 - INDENTEE            ◄── XX fin exclusion
000011 LIGNE 11 - NORMALE
```

2. Appuyer **Entrée**

**Résultat** :
```
000004 LIGNE 04 - NORMALE
- - - - - - - - - - - - - - 6 Line(s) not Displayed
000011 LIGNE 11 - NORMALE
```

#### Étape 3 : Commandes F, L, S

Sur la ligne `- - - 6 Line(s) not Displayed`, taper :

| Commande | Action | Résultat |
|----------|--------|----------|
| **F** | First - Affiche première ligne exclue | Montre ligne 5 seulement |
| **L** | Last - Affiche dernière ligne exclue | Montre ligne 10 seulement |
| **S** ou **S6** | Show - Affiche toutes les lignes | Montre les 6 lignes |

#### Étape 4 : Réinitialiser

Taper `RESET` sur la ligne de commande pour tout réafficher.

---

### Exercice 15 : COLS, TABS, BNDS, MASK

#### 15.1 - Commande COLS (règle des colonnes)

1. Éditer `MEMTEST`

2. Sur la ligne de commande, taper `COLS` :
```
Command ===> COLS
```

**Résultat** :
```
=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
000001 LIGNE 01 - DEBUT DU FICHIER
```

3. Taper `COLS` à nouveau → La règle disparaît (toggle)

4. Taper `RESET` → Réinitialise l'affichage

**Alternative** : Taper `COLS` comme commande ligne sur une ligne spécifique.

---

#### 15.2 - Commande TABS

1. Sur une ligne vide, taper `TABS` dans la zone de numéro :
```
TABS   ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
```

2. Remplir la ligne TABS avec des `*` aux positions de tabulation désirées :
```
TABS   *         *         *         *         *
       ↑         ↑         ↑         ↑         ↑
       col 1     col 11    col 21    col 31    col 41
```

3. Appuyer **Entrée**

4. Insérer une nouvelle ligne (`I`) et utiliser **TAB** pour sauter aux positions définies

**Pour supprimer les TABS** : Effacer le contenu de la ligne TABS ou taper `RESET`

---

#### 15.3 - Commande BNDS (Bornes/Limites)

Définit les limites gauche/droite pour les opérations d'édition.

1. Taper `BNDS` sur la ligne de commande :
```
=BNDS> <                             >
       ↑                             ↑
       Borne gauche (col 1)          Borne droite (col 72)
```

2. Déplacer les bornes :
   - Borne gauche à position 1
   - Borne droite à position 30

```
=BNDS> <                            >
       1                            30
```

3. Tester avec la commande `TF` (Text Flow) :
   - `TF30` : Reformate le texte sur 30 colonnes
   - `TF20` : Reformate le texte sur 20 colonnes

**Résultat** : Le texte est reformaté selon la largeur spécifiée.

---

#### 15.4 - Commande MASK

Le MASK définit un modèle pré-rempli pour les nouvelles lignes insérées.

1. Taper `MASK` dans la zone de numéro d'une ligne :
```
MASK            /* COMMENTAIRE                          */
                ↑                                       ↑
                position 10                             position 50
```

2. Appuyer **Entrée**

3. Insérer une nouvelle ligne (`I`) :
```
000005 LIGNE 05
''''''          /* COMMENTAIRE                          */
000006 LIGNE 06
```
La nouvelle ligne est pré-remplie avec le MASK !

4. Saisir votre texte entre les `/*` et `*/`

**Pour supprimer le MASK** : Effacer le contenu de la ligne MASK ou `RESET`

---

#### 15.5 - Vérifier le PROFILE

1. Taper `PROFILE` sur la ligne de commande :

```
=PROF> ....MEMTEST (FIXED - 80)....RECOVERY ON....NUMBER ON STD COBOL....
=PROF> ....CAPS ON....HEX OFF....NULLS ON STD....TABS OFF....
=PROF> ....AUTOSAVE ON....AUTONUM OFF....AUTOLIST OFF....STATS ON....
=PROF> ....IMACRO NONE....PACK OFF....NOTE ON....UNDO STORAGE....
```

**Paramètres importants** :
| Paramètre | Description |
|-----------|-------------|
| **CAPS ON/OFF** | Conversion automatique en majuscules |
| **NULLS ON/OFF** | Caractères null en fin de ligne |
| **TABS ON/OFF** | Activation des tabulations |
| **NUMBER ON/OFF** | Numérotation COBOL (col 1-6) |
| **RECOVERY ON/OFF** | Sauvegarde automatique |

---

## Récapitulatif des exercices

| Ex | Objectif | Option ISPF |
|----|----------|-------------|
| 1 | Lister membres d'une Library | 3.1 ou 3.4 + `m` |
| 2 | Créer un membre | 2 (Edit) |
| 3 | Caractéristiques d'une Library | 3.4 + `i` |
| 4 | Créer une nouvelle Library | 3.2 + A |
| 5 | Copier un membre | 3.3 + C |
| 6 | Renommer un membre | 3.1 + `r` |
| 7 | Contenu d'un volume | 3.4 + Volume |
| 8 | Caractéristiques d'un fichier | 3.4 + `i` |
| 9 | VTOC d'un volume | 3.4 ou TSO LISTCAT |
| 10 | Différence COPY/MOVE | Théorie |
| 11 | Créer membre MEMTEST | 2 (Edit) |
| 12 | Déplacer texte (après) | MM + A |
| 13 | Déplacer texte (avant) | MM + B |
| 14 | Exclusion et F/L/S | XX + F/L/S |
| 15 | COLS/TABS/BNDS/MASK | Commandes éditeur |

---

## Aide-mémoire

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MÉMOIRE EXERCICES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  GESTION MEMBRES                                                │
│  ────────────────────────────────────────────────────────────── │
│  =3.1         Library Utility (lister membres)                  │
│  =3.3         Move/Copy Utility                                 │
│  m            Members (depuis 3.4)                              │
│  c            Copy membre                                       │
│  r            Rename membre                                     │
│                                                                  │
│  DÉPLACEMENT TEXTE                                              │
│  ────────────────────────────────────────────────────────────── │
│  MM/MM        Bloc à déplacer (Move)                           │
│  CC/CC        Bloc à copier (Copy)                             │
│  A            Destination après (After)                        │
│  B            Destination avant (Before)                       │
│                                                                  │
│  EXCLUSION                                                      │
│  ────────────────────────────────────────────────────────────── │
│  X/Xn/XX      Exclure ligne(s)                                 │
│  F            First - première ligne exclue                    │
│  L            Last - dernière ligne exclue                     │
│  S/Sn         Show - afficher ligne(s) exclue(s)              │
│  RESET        Tout réafficher                                  │
│                                                                  │
│  COMMANDES SPÉCIALES                                            │
│  ────────────────────────────────────────────────────────────── │
│  COLS         Règle des colonnes (toggle)                      │
│  TABS         Définir tabulations                              │
│  BNDS         Définir bornes gauche/droite                     │
│  MASK         Masque pour nouvelles lignes                     │
│  PROFILE      Afficher paramètres du profil                    │
│  TFnn         Text Flow sur nn colonnes                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Exercices TSO - Chapitre III](03-exercices-tso.md) | [Exercices JCL - Chapitre V](05-exercices-jcl.md) |
