# Chapitre IV - ISPF/PDF (Interactive System Productivity Facility)

## Introduction

Ce chapitre présente ISPF, l'interface plein écran de z/OS :
1. Généralités et concepts
2. Utilisation pratique d'ISPF
3. Structure et organisation
4. Écrans de dialogue TSO/ISPF (LOG/LIST, View, Edit, Utilities, SDSF, Éditeur)

---

## IV-1 Généralités

### Définition d'ISPF

**ISPF (Interactive System Productivity Facility)** est une interface utilisateur plein écran qui s'exécute sous TSO. Elle fournit :

- Un environnement de développement intégré
- Des menus et panels interactifs
- Un éditeur puissant
- Des utilitaires de gestion de fichiers
- Une interface standardisée et intuitive

```
┌─────────────────────────────────────────────────────────────────┐
│                    POSITIONNEMENT ISPF                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   UTILISATEUR                                                   │
│       │                                                          │
│       ▼                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                      ISPF/PDF                            │   │
│   │                                                          │   │
│   │  • Interface plein écran (panels)                       │   │
│   │  • Menus hiérarchiques                                  │   │
│   │  • Éditeur avancé                                       │   │
│   │  • Utilitaires intégrés                                 │   │
│   │                                                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│       │                                                          │
│       ▼                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                       TSO/E                              │   │
│   │                                                          │   │
│   │  • Sous-système de base                                 │   │
│   │  • Commandes natives                                    │   │
│   │  • Gestion de session                                   │   │
│   │                                                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│       │                                                          │
│       ▼                                                          │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │                       z/OS                               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### PDF (Program Development Facility)

**PDF** est la composante d'ISPF dédiée au développement :

| Composant | Fonction |
|-----------|----------|
| **ISPF** | Framework de panels et dialogues |
| **PDF** | Outils de développement (Edit, Browse, Utilities) |
| **SCLM** | Gestion de configuration logicielle |
| **ISPF/PDF** | Terme courant pour l'ensemble |

### Avantages d'ISPF

```
┌─────────────────────────────────────────────────────────────────┐
│                    AVANTAGES ISPF                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   PRODUCTIVITÉ                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Navigation rapide par menus et raccourcis            │   │
│   │  • Éditeur plein écran vs ligne par ligne              │   │
│   │  • Copier/coller, recherche/remplacement               │   │
│   │  • Macros et automatisation                            │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   STANDARDISATION                                               │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Interface cohérente sur tous les systèmes z/OS      │   │
│   │  • Touches de fonction uniformes (PF1=Help, PF3=Exit)  │   │
│   │  • Comportement prévisible                              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   INTÉGRATION                                                   │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Accès à toutes les commandes TSO                     │   │
│   │  • Soumission de jobs intégrée                         │   │
│   │  • Gestion complète des datasets                       │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## IV-2 Utilisation ISPF

### Lancement d'ISPF

#### Depuis l'écran LOGON TSO

```
┌─────────────────────────────────────────────────────────────────┐
│                    TSO/E LOGON                                   │
│                                                                  │
│   Userid    ===> FTEST                                          │
│   Password  ===> ________                                       │
│   Procedure ===> IKJACCNT                                       │
│   Command   ===> ISPF         ◄── Lance ISPF automatiquement    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Depuis le prompt TSO (READY)

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                  │
│   READY                                                         │
│   ispf                                                          │
│                                                                  │
│   (L'écran ISPF Primary Option Menu s'affiche)                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Touches de fonction (PF Keys)

Les touches de fonction sont standardisées dans ISPF :

```
┌─────────────────────────────────────────────────────────────────┐
│                    TOUCHES DE FONCTION ISPF                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   TOUCHES PRINCIPALES (universelles)                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  PF1  = Help         Aide contextuelle                  │   │
│   │  PF2  = Split        Diviser l'écran                    │   │
│   │  PF3  = Exit/End     Sortir / Retour                    │   │
│   │  PF4  = Return       Retour au menu principal           │   │
│   │  PF7  = Up/Backward  Défiler vers le haut               │   │
│   │  PF8  = Down/Forward Défiler vers le bas                │   │
│   │  PF9  = Swap         Basculer entre écrans splittés     │   │
│   │  PF10 = Left         Défiler vers la gauche             │   │
│   │  PF11 = Right        Défiler vers la droite             │   │
│   │  PF12 = Retrieve     Rappeler dernière commande         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   TOUCHES ÉDITEUR (en mode Edit)                                │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  PF5  = RFind        Répéter recherche                  │   │
│   │  PF6  = RChange      Répéter remplacement               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Note : PF13-PF24 = PF1-PF12 avec Shift                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Navigation dans ISPF

#### Méthode 1 : Navigation par menus

Naviguer étape par étape en entrant les numéros d'options :

```
Menu Principal → 3 (Utilities) → 4 (Dslist)
```

#### Méthode 2 : Raccourcis directs

Accéder directement à une option avec le préfixe `=` :

```
┌─────────────────────────────────────────────────────────────────┐
│                    RACCOURCIS ISPF                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   =0       Paramètres (Settings)                                │
│   =1       View (lecture seule)                                 │
│   =2       Edit (édition)                                       │
│   =3       Utilities                                            │
│   =3.1     Library (membres PDS)                                │
│   =3.2     Data Set (opérations datasets)                       │
│   =3.3     Move/Copy                                            │
│   =3.4     Dslist (liste datasets)  ◄── LE PLUS UTILISÉ        │
│   =4       Foreground (compilation)                             │
│   =5       Batch (soumission jobs)                              │
│   =6       Command (commandes TSO)                              │
│   =S       SDSF (gestion spool)                                 │
│   =X       Exit ISPF                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Méthode 3 : Commande TSO depuis ISPF

Exécuter une commande TSO depuis n'importe quel panel :

```
Command ===> TSO LISTDS 'SYS1.PROCLIB' MEMBERS
```

### Écran split (division d'écran)

ISPF permet de travailler sur plusieurs écrans simultanément :

```
┌─────────────────────────────────────────────────────────────────┐
│                    ÉCRAN SPLIT                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   PF2 = Split (diviser à la position du curseur)               │
│   PF9 = Swap (basculer entre les deux parties)                 │
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  EDIT       USER01.COBOL.SOURCE(PROG1)                  │   │
│   │  Command ===>                                            │   │
│   │  000001        IDENTIFICATION DIVISION.                  │   │
│   │  000002        PROGRAM-ID. PROG1.                        │   │
│   │  000003        ...                                       │   │
│   ├─────────────────────────────────────────────────────────┤   │
│   │  BROWSE     USER01.COPYLIB(CUSTOMER)                    │   │
│   │  Command ===>                                            │   │
│   │  000001        01  CUSTOMER-RECORD.                      │   │
│   │  000002            05 CUST-ID     PIC 9(5).             │   │
│   │  000003            ...                                   │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Pour fermer un split : SWAP vers l'écran puis =X ou END      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## IV-3 Structure ISPF

### Menu Principal (Primary Option Menu)

```
┌─────────────────────────────────────────────────────────────────┐
│  Menu  Utilities  Compilers  Options  Status  Help              │
├─────────────────────────────────────────────────────────────────┤
│                    ISPF Primary Option Menu                      │
│                                                                  │
│  Option ===> _                                                  │
│                                                                  │
│   0  Settings      Terminal and user parameters                 │
│   1  View          Display source data or listings              │
│   2  Edit          Create or change source data                 │
│   3  Utilities     Perform utility functions                    │
│   4  Foreground    Interactive language processing              │
│   5  Batch         Submit job for language processing           │
│   6  Command       Enter TSO or Workstation commands            │
│   7  Dialog Test   Perform dialog testing                       │
│   9  IBM Products  IBM program development products             │
│   10 SCLM          SW Configuration Library Manager             │
│   11 Workplace     ISPF Object/Action Workplace                 │
│   M  More          Additional IBM Products                      │
│   S  SDSF          Spool Display and Search Facility            │
│                                                                  │
│  Enter X to Terminate using log/list defaults                   │
│                                                                  │
│  Userid  : FTEST     Time  : 14:30                              │
│  Terminal: 3278      Screen: 1                                  │
│  System  : ZOS1      Runmode: ISPF                              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Hiérarchie des options

```
┌─────────────────────────────────────────────────────────────────┐
│                    STRUCTURE ISPF                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ISPF Primary Option Menu                                      │
│   │                                                              │
│   ├── 0  Settings                                               │
│   │       └── Paramètres terminal, profil utilisateur          │
│   │                                                              │
│   ├── 1  View                                                   │
│   │       └── Affichage lecture seule                          │
│   │                                                              │
│   ├── 2  Edit                                                   │
│   │       └── Édition de fichiers                              │
│   │                                                              │
│   ├── 3  Utilities                                              │
│   │       ├── 3.1  Library      - Gestion membres PDS          │
│   │       ├── 3.2  Data Set     - Opérations sur datasets      │
│   │       ├── 3.3  Move/Copy    - Copie/déplacement            │
│   │       ├── 3.4  Dslist       - Liste de datasets            │
│   │       ├── 3.5  Reset        - Réinitialisation stats       │
│   │       ├── 3.6  Hardcopy     - Impression                   │
│   │       ├── 3.7  Transfer     - Transfert de fichiers        │
│   │       ├── 3.8  Outlist      - Sorties utilisateur          │
│   │       ├── 3.9  Commands     - Table de commandes           │
│   │       ├── 3.11 Format       - Formatage DFDSS              │
│   │       ├── 3.12 SuperC       - Comparaison de fichiers      │
│   │       ├── 3.13 SuperCE      - Comparaison étendue          │
│   │       ├── 3.14 Search-For   - Recherche dans fichiers      │
│   │       └── 3.15 Search-ForE  - Recherche étendue            │
│   │                                                              │
│   ├── 4  Foreground                                             │
│   │       └── Compilation interactive                          │
│   │                                                              │
│   ├── 5  Batch                                                  │
│   │       └── Soumission de jobs                               │
│   │                                                              │
│   ├── 6  Command                                                │
│   │       └── Commandes TSO                                    │
│   │                                                              │
│   └── S  SDSF                                                   │
│           └── Gestion du spool                                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Profil ISPF

ISPF stocke les préférences utilisateur dans un dataset profil :

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROFIL ISPF                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   Dataset profil : userid.ISPF.ISPPROF                         │
│                                                                  │
│   Contenu :                                                     │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Paramètres terminal (couleurs, taille écran)        │   │
│   │  • Préférences éditeur (tabs, nulls, caps)             │   │
│   │  • Historique des commandes                            │   │
│   │  • Listes de datasets récents                          │   │
│   │  • Macros personnalisées                               │   │
│   │  • Paramètres par application                          │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Accès : Option 0 (Settings) depuis le menu principal         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## IV-4 Écrans de dialogue TSO/ISPF

### IV-4-1 Fichiers LOG et LIST

ISPF maintient deux fichiers de journalisation :

```
┌─────────────────────────────────────────────────────────────────┐
│                    FICHIERS LOG ET LIST                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   FICHIER LOG                                                   │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Journal de session ISPF                              │   │
│   │  • Messages système et utilisateur                      │   │
│   │  • Horodatage des actions                               │   │
│   │  • Historique des commandes                             │   │
│   │                                                          │   │
│   │  Dataset : userid.SPFLOG1.LIST (typique)               │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   FICHIER LIST                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  • Sorties des commandes TSO                            │   │
│   │  • Résultats de compilations                            │   │
│   │  • Listings générés                                     │   │
│   │  • Sorties des utilitaires                              │   │
│   │                                                          │   │
│   │  Dataset : userid.SPFLIST1.LIST (typique)              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Configuration LOG/LIST

Accessible via **Option 0** (Settings) puis **Log/List** :

```
┌─────────────────────────────────────────────────────────────────┐
│                 LOG/LIST SETTINGS                                │
│                                                                  │
│   Log Data Set:                                                 │
│     Log data set name  ===> FTEST.SPFLOG1.LIST                 │
│     Log data set type  ===> 1  1. Dataset  2. SYSOUT           │
│                                                                  │
│   List Data Set:                                                │
│     List data set name ===> FTEST.SPFLIST1.LIST                │
│     List data set type ===> 1  1. Dataset  2. SYSOUT           │
│                                                                  │
│   At session termination:                                       │
│     Process log        ===> D  D=Delete, K=Keep, P=Print       │
│     Process list       ===> D  D=Delete, K=Keep, P=Print       │
│                                                                  │
│   JES and SYSOUT information:                                   │
│     SYSOUT class       ===> A                                   │
│     Job statement info ===> //FTEST001 JOB CLASS=A             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Option | Description |
|--------|-------------|
| **D (Delete)** | Supprimer le fichier à la fin de session |
| **K (Keep)** | Conserver le fichier |
| **P (Print)** | Imprimer puis supprimer |

---

### IV-4-2 View : Affichage du contenu de la Library

L'**Option 1 (View)** permet d'afficher des fichiers en lecture seule.

#### Écran View Entry Panel

```
┌─────────────────────────────────────────────────────────────────┐
│                    VIEW - ENTRY PANEL                            │
│                                                                  │
│  ISPF Library:                                                  │
│     Project  ===> FTEST                                         │
│     Group    ===> COBOL                                         │
│     Type     ===> SOURCE                                        │
│     Member   ===> PROG1       (Blank for member selection list) │
│                                                                  │
│  Other Partitioned or Sequential Data Set:                      │
│     Data Set Name ===> 'SYS1.PROCLIB(COBUC)'                   │
│     Volume Serial ===>         (If not cataloged)               │
│                                                                  │
│  Initial Macro   ===>                                           │
│  Profile Name    ===>         (Blank defaults to data set type) │
│  Format Name     ===>                                           │
│                                                                  │
│  Options:                                                       │
│     Confirm Cancel/Move/Replace ===> YES  (YES or NO)           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Raccourci View depuis Dslist (3.4)

```
┌─────────────────────────────────────────────────────────────────┐
│ DSLIST - Data Sets Matching FTEST                    Row 1 of 5 │
│ Command ===>                                                    │
│                                                                  │
│ Command     Name                                     Volume     │
│ -------     ----                                     ------     │
│ v           FTEST.COBOL.SOURCE                       PUB001     │
│             FTEST.COBOL.LOAD                         PUB001     │
│                                                                  │
│  v = View (lecture seule)                                       │
│  b = Browse (équivalent)                                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Différence View vs Browse

| Aspect | View | Browse |
|--------|------|--------|
| **Modification** | Non | Non |
| **Commandes éditeur** | Oui (FIND, COLS...) | Limitées |
| **Coloration syntaxe** | Oui | Non |
| **Profile** | Oui | Non |
| **Usage** | Consultation avancée | Consultation rapide |

---

### IV-4-3 Edit : Création et mise à jour

L'**Option 2 (Edit)** est l'outil principal de développement ISPF.

#### Écran Edit Entry Panel

```
┌─────────────────────────────────────────────────────────────────┐
│                    EDIT - ENTRY PANEL                            │
│                                                                  │
│  ISPF Library:                                                  │
│     Project  ===> FTEST                                         │
│     Group    ===> COBOL                                         │
│     Type     ===> SOURCE                                        │
│     Member   ===> _______     (Blank for member selection list) │
│                                                                  │
│  Other Partitioned or Sequential Data Set:                      │
│     Data Set Name ===> ________________________________________│
│     Volume Serial ===>         (If not cataloged)               │
│                                                                  │
│  Workstation File:                                              │
│     File Name     ===>                                          │
│                                                                  │
│  Initial Macro   ===>                                           │
│  Profile Name    ===>         (Blank defaults to data set type) │
│  Format Name     ===>                                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Raccourci Edit depuis Dslist (3.4)

```
│ Command     Name                                     Volume     │
│ e           FTEST.COBOL.SOURCE                       PUB001     │

  e = Edit
```

#### Écran d'édition

```
┌─────────────────────────────────────────────────────────────────┐
│  EDIT       FTEST.COBOL.SOURCE(PROG1) - 01.05        Columns 1 72│
│  Command ===>                                        Scroll PAGE │
│  ****** ***************************** Top of Data ****************│
│  000001        IDENTIFICATION DIVISION.                          │
│  000002        PROGRAM-ID. PROG1.                                │
│  000003        AUTHOR. FTEST.                                    │
│  000004       *-------------------------------------------------*│
│  000005        ENVIRONMENT DIVISION.                             │
│  000006        CONFIGURATION SECTION.                            │
│  000007        DATA DIVISION.                                    │
│  000008        WORKING-STORAGE SECTION.                          │
│  000009        01  WS-MESSAGE    PIC X(30).                      │
│  000010        PROCEDURE DIVISION.                               │
│  000011        0000-PRINCIPAL.                                   │
│  000012            DISPLAY 'HELLO WORLD'.                        │
│  000013            STOP RUN.                                     │
│  ****** **************************** Bottom of Data **************│
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### IV-4-4 Utilities : Gestion des Membres et Fichiers

L'**Option 3 (Utilities)** regroupe les utilitaires de gestion.

#### Menu Utilities

```
┌─────────────────────────────────────────────────────────────────┐
│                    UTILITY SELECTION MENU                        │
│  Option ===>                                                    │
│                                                                  │
│   1  Library      Display or manage members of a library        │
│   2  Data Set     Allocate, rename, delete, or list data sets   │
│   3  Move/Copy    Move, copy, or promote members or data sets   │
│   4  Dslist       List data sets matching a pattern             │
│   5  Reset        Reset statistics for a PDS member             │
│   6  Hardcopy     Print or download data set listings           │
│   7  Transfer     Transfer data sets between systems            │
│   8  Outlist      Display, delete, or print output listings     │
│   9  Commands     Create/change an application command table    │
│                                                                  │
│   11 Format       Format definition for formatted data          │
│   12 SuperC       Compare data sets (standard)                  │
│   13 SuperCE      Compare data sets (extended)                  │
│   14 Search-For   Search data sets for strings                  │
│   15 Search-ForE  Search data sets (extended)                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Option 3.1 - Library Utility

Gestion des membres d'un PDS :

```
┌─────────────────────────────────────────────────────────────────┐
│                 LIBRARY UTILITY                                  │
│                                                                  │
│  Option ===> _                                                  │
│                                                                  │
│     blank  Display member list      R  Rename member            │
│     C      Compress data set        D  Delete member            │
│     X      Print index listing      S  Short statistics         │
│     L      Long statistics          F  Free unused space        │
│                                                                  │
│  ISPF Library:                                                  │
│     Project  ===> FTEST                                         │
│     Group    ===> COBOL                                         │
│     Type     ===> SOURCE                                        │
│     Member   ===>                                               │
│                                                                  │
│  Other Partitioned Data Set:                                    │
│     Data Set Name ===>                                          │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Option 3.2 - Data Set Utility

Opérations sur les datasets :

```
┌─────────────────────────────────────────────────────────────────┐
│                 DATA SET UTILITY                                 │
│                                                                  │
│  Option ===> _                                                  │
│                                                                  │
│     A  Allocate new data set          C  Catalog data set       │
│     R  Rename data set                U  Uncatalog data set     │
│     D  Delete data set                S  Short data set info    │
│     blank  Data set information (short)                         │
│     I  Data set information (long)                              │
│                                                                  │
│  Data Set Name ===> ___________________________________________│
│                                                                  │
│  Volume Serial ===>         (If not cataloged)                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

##### Exemple : Allocation d'un dataset (Option A)

```
┌─────────────────────────────────────────────────────────────────┐
│                 ALLOCATE NEW DATA SET                            │
│                                                                  │
│   Data Set Name : FTEST.NEW.SOURCE                              │
│                                                                  │
│   Management class  ===>                                        │
│   Storage class     ===>                                        │
│   Volume serial     ===> PUB001    (ou laisser vide si SMS)     │
│   Device type       ===>                                        │
│   Data class        ===>                                        │
│   Space units       ===> TRACK     (TRACK, CYL, ou BLOCK)       │
│   Primary quantity  ===> 5                                      │
│   Secondary quantity===> 2                                      │
│   Directory blocks  ===> 10        (Obligatoire pour PDS)       │
│   Record format     ===> FB                                     │
│   Record length     ===> 80                                     │
│   Block size        ===> 27920                                  │
│   Data set type     ===> PDS       (PDS, LIBRARY, ou vide)      │
│   Expiration date   ===>                                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Option 3.4 - Dslist (Data Set List)

L'utilitaire **le plus utilisé** pour naviguer dans les datasets :

```
┌─────────────────────────────────────────────────────────────────┐
│                 DATA SET LIST UTILITY                            │
│                                                                  │
│  Option ===>                                                    │
│                                                                  │
│  Enter one or both of the parameters below:                     │
│     Dsname Level ===> FTEST                                     │
│     Volume       ===>         (Blank for cataloged data sets)   │
│                                                                  │
│  Initial View   ===> 1  1. Volume    2. Space    3. Attrib     │
│                        4. Total                                 │
│  Enter "/" to select option:                                    │
│     _ Display Additional Qualifiers                             │
│     _ Include Additional Dsname Levels                          │
│     _ Include Migrated Data Sets without Recall                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

##### Commandes ligne Dslist

```
┌─────────────────────────────────────────────────────────────────┐
│ DSLIST - Data Sets Matching FTEST                    Row 1 of 8 │
│ Command ===>                                        Scroll ===> │
│                                                                  │
│ Command     Name                                     Volume     │
│ -------     ----                                     ------     │
│             FTEST.COBOL.COPYLIB                      PUB001     │
│             FTEST.COBOL.LOAD                         PUB001     │
│             FTEST.COBOL.SOURCE                       PUB001     │
│             FTEST.DATA.CLIENTS                       PUB001     │
│             FTEST.JCL.CNTL                           PUB001     │
│             FTEST.ISPF.ISPPROF                       PUB001     │
│                                                                  │
│  COMMANDES LIGNE (taper devant le nom) :                        │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  e = Edit          b = Browse         v = View          │    │
│  │  d = Delete        r = Rename         i = Info          │    │
│  │  m = Members       c = Copy           p = Print         │    │
│  │  z = Compress      s = Short info     j = Submit JCL    │    │
│  │  = = Repeat last   / = Toggle selection                 │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

| Commande | Action |
|----------|--------|
| **e** | Éditer le dataset ou membre |
| **b** | Parcourir (Browse) en lecture seule |
| **v** | View (lecture seule avec commandes éditeur) |
| **d** | Supprimer le dataset |
| **r** | Renommer le dataset |
| **i** | Informations détaillées |
| **m** | Liste des membres (pour PDS) |
| **c** | Copier |
| **z** | Compresser (PDS) |
| **j** | Soumettre comme JCL |

---

### IV-4-5 SDSF : Spool Display and Search Facility

**SDSF** (Option S) permet de gérer les jobs et le spool JES2/JES3.

#### Menu principal SDSF

```
┌─────────────────────────────────────────────────────────────────┐
│               SDSF PRIMARY OPTION MENU                           │
│  Command ===>                                                   │
│                                                                  │
│   DA    Active users                                            │
│   I     Input queue                                             │
│   O     Output queue                                            │
│   H     Held output queue                                       │
│   ST    Status of jobs                                          │
│   LOG   System log                                              │
│   SR    System requests                                         │
│   MAS   Members of MAS (sysplex)                                │
│   PR    Printers                                                │
│   PUN   Punches                                                 │
│   RDR   Readers                                                 │
│   LINE  Lines                                                   │
│   NODE  Nodes                                                   │
│   SO    Spool offload                                           │
│   SP    Spool volumes                                           │
│                                                                  │
│   SET   Set options                                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commandes SDSF principales

| Commande | Description |
|----------|-------------|
| **DA** | Display Active - Jobs en cours d'exécution |
| **I** | Input queue - Jobs en attente |
| **O** | Output queue - Sorties disponibles |
| **H** | Held queue - Sorties en attente |
| **ST** | Status - État de tous les jobs |
| **LOG** | Journal système |

#### Écran ST (Status)

```
┌─────────────────────────────────────────────────────────────────┐
│ SDSF STATUS DISPLAY ALL CLASSES                     LINE 1-10   │
│ Command ===>                                        Scroll ===> │
│ PREFIX=FTEST  OWNER=*  SYSNAME=                                 │
│                                                                  │
│ NP   JOBNAME  JobID    Owner    Prty Queue      C Pos          │
│      FTEST1   JOB12345 FTEST    10   OUTPUT     A              │
│      FTEST2   JOB12346 FTEST    10   EXECUTION                 │
│      FTESTC   JOB12347 FTEST    5    INPUT      A   3          │
│                                                                  │
│  COMMANDES LIGNE :                                              │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  ?  = Afficher les sorties (SYSOUT)                     │    │
│  │  S  = Sélectionner/afficher le job                      │    │
│  │  P  = Purger (supprimer) le job et ses sorties         │    │
│  │  C  = Annuler le job                                    │    │
│  │  A  = Libérer (release) le job en attente              │    │
│  │  H  = Mettre en hold                                    │    │
│  │  SJ = Afficher le JCL soumis                            │    │
│  │  SE = Afficher les messages JES                         │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Consultation des sorties

```
┌─────────────────────────────────────────────────────────────────┐
│ SDSF OUTPUT DISPLAY FTEST1    JOB12345  DSID   2  LINE 1       │
│ Command ===>                                        Scroll ===> │
│ ********************************* TOP OF DATA *******************│
│                   J E S 2  J O B  L O G                         │
│                                                                  │
│  14.30.25 JOB12345 ---- FRIDAY,   15 NOV 2025 ----              │
│  14.30.25 JOB12345  IRR010I  USERID FTEST    IS ASSIGNED        │
│  14.30.25 JOB12345  $HASP373 FTEST1   STARTED                   │
│  14.30.26 JOB12345  IEF403I FTEST1 - STARTED - TIME=14.30.25   │
│  14.30.27 JOB12345  IEF404I FTEST1 - ENDED   - TIME=14.30.27   │
│  14.30.27 JOB12345  $HASP395 FTEST1   ENDED                     │
│  ------ JES2 JOB STATISTICS ------                              │
│   15 NOV 2025 JOB EXECUTION DATE                                │
│           4 CARDS READ                                          │
│          45 SYSOUT PRINT RECORDS                                │
│                                                                  │
│ ******************************** BOTTOM OF DATA ******************│
└─────────────────────────────────────────────────────────────────┘
```

#### Filtrage des jobs

```
┌─────────────────────────────────────────────────────────────────┐
│                    FILTRAGE SDSF                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Sur la ligne de commande SDSF :                                │
│                                                                  │
│  PREFIX FTEST        - Jobs commençant par FTEST               │
│  PREFIX FTEST*       - Idem avec wildcard                      │
│  OWNER FTEST         - Jobs appartenant à FTEST                │
│  OWNER *             - Tous les propriétaires                  │
│  DEST LOCAL          - Sorties destination LOCAL               │
│                                                                  │
│  Exemples combinés :                                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │ Command ===> PRE FTEST* OWNER FTEST                     │    │
│  │ (Affiche les jobs FTEST* appartenant à FTEST)          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

### IV-4-6 L'éditeur ISPF

L'éditeur ISPF est l'outil central du développement mainframe.

#### Structure de l'écran éditeur

```
┌─────────────────────────────────────────────────────────────────┐
│  EDIT       FTEST.COBOL.SOURCE(PROG1) - 01.05        Columns 1 72│
│  Command ===>                                        Scroll PAGE │
├─────────────────────────────────────────────────────────────────┤
│  Zone     │ Description                                         │
├───────────┼─────────────────────────────────────────────────────┤
│  Ligne 1  │ Mode (EDIT/VIEW), dataset, version, colonnes       │
│  Ligne 2  │ Ligne de commande, mode défilement                 │
│  Col 1-6  │ Numéros de ligne ou commandes ligne                │
│  Col 7+   │ Données du fichier                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Commandes primaires (ligne de commande)

```
┌─────────────────────────────────────────────────────────────────┐
│                COMMANDES PRIMAIRES ÉDITEUR                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  NAVIGATION                                                     │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  TOP / T      Aller au début                            │    │
│  │  BOTTOM / BOT Aller à la fin                            │    │
│  │  LOCATE n     Aller à la ligne n                        │    │
│  │  LEFT / RIGHT Défiler horizontalement                   │    │
│  │  UP n / DOWN n Défiler de n lignes                      │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  RECHERCHE ET REMPLACEMENT                                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  FIND 'texte'         Rechercher                        │    │
│  │  FIND 'texte' ALL     Rechercher toutes occurrences    │    │
│  │  FIND 'texte' FIRST   Première occurrence              │    │
│  │  FIND 'texte' LAST    Dernière occurrence              │    │
│  │  FIND 'texte' PREV    Occurrence précédente            │    │
│  │  CHANGE 'old' 'new'   Remplacer (premier)              │    │
│  │  CHANGE 'old' 'new' ALL  Remplacer tout                │    │
│  │  RFIND (PF5)          Répéter recherche                │    │
│  │  RCHANGE (PF6)        Répéter remplacement             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  SAUVEGARDE                                                     │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  SAVE          Sauvegarder                              │    │
│  │  CANCEL / CAN  Annuler les modifications               │    │
│  │  END (PF3)     Sauvegarder et quitter                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  AFFICHAGE                                                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  COLS          Afficher règle des colonnes             │    │
│  │  RESET         Réinitialiser l'affichage               │    │
│  │  HEX ON/OFF    Mode hexadécimal                        │    │
│  │  CAPS ON/OFF   Majuscules automatiques                 │    │
│  │  NULLS ON/OFF  Caractères null en fin de ligne        │    │
│  │  TABS ON/OFF   Gestion des tabulations                │    │
│  │  NUMBER ON/OFF Numérotation COBOL (col 1-6)           │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  BLOCS                                                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  EXCLUDE 'texte'      Masquer lignes contenant texte   │    │
│  │  EXCLUDE ALL          Masquer toutes les lignes        │    │
│  │  FLIP                 Inverser lignes masquées/visible │    │
│  │  RESET                Afficher toutes les lignes       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commandes ligne (colonne 1-6)

```
┌─────────────────────────────────────────────────────────────────┐
│                COMMANDES LIGNE ÉDITEUR                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  INSERTION                                                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  I     Insérer une ligne après                          │    │
│  │  In    Insérer n lignes après                           │    │
│  │  A     Insérer après (destination copie/déplacement)    │    │
│  │  B     Insérer avant (destination copie/déplacement)    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  SUPPRESSION                                                    │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  D     Supprimer une ligne                              │    │
│  │  Dn    Supprimer n lignes                               │    │
│  │  DD    Supprimer bloc (première ligne)                  │    │
│  │  DD    Supprimer bloc (dernière ligne)                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  COPIE                                                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  C     Copier une ligne                                 │    │
│  │  Cn    Copier n lignes                                  │    │
│  │  CC    Copier bloc (première ligne)                     │    │
│  │  CC    Copier bloc (dernière ligne)                     │    │
│  │  A ou B  Destination (après ou avant)                   │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  DÉPLACEMENT                                                    │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  M     Déplacer une ligne                               │    │
│  │  Mn    Déplacer n lignes                                │    │
│  │  MM    Déplacer bloc (première ligne)                   │    │
│  │  MM    Déplacer bloc (dernière ligne)                   │    │
│  │  A ou B  Destination                                    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  RÉPÉTITION                                                     │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  R     Répéter (dupliquer) une ligne                    │    │
│  │  Rn    Répéter n fois                                   │    │
│  │  RR    Répéter bloc                                     │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  DÉCALAGE                                                       │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  >     Décaler à droite (2 positions)                   │    │
│  │  <     Décaler à gauche (2 positions)                   │    │
│  │  >n    Décaler de n positions à droite                  │    │
│  │  <n    Décaler de n positions à gauche                  │    │
│  │  >>    Décaler bloc à droite                            │    │
│  │  <<    Décaler bloc à gauche                            │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  AUTRES                                                         │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  X     Exclure (masquer) une ligne                      │    │
│  │  Xn    Exclure n lignes                                 │    │
│  │  XX    Exclure bloc                                     │    │
│  │  S     Afficher ligne exclue                            │    │
│  │  F     Afficher première ligne d'un bloc exclu          │    │
│  │  L     Afficher dernière ligne d'un bloc exclu          │    │
│  │  COLS  Afficher règle des colonnes                      │    │
│  │  BNDS  Définir limites (bounds)                         │    │
│  │  MASK  Définir masque d'insertion                       │    │
│  │  TABS  Afficher/définir tabulations                     │    │
│  │  LC    Convertir en minuscules                          │    │
│  │  UC    Convertir en majuscules                          │    │
│  │  TE    Édition texte (justification)                    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Exemples pratiques

```
┌─────────────────────────────────────────────────────────────────┐
│                    EXEMPLES D'ÉDITION                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  EXEMPLE 1 : Copier un bloc                                     │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  CC0010            MOVE A TO B.                         │    │
│  │  000011            MOVE C TO D.                         │    │
│  │  CC0012            MOVE E TO F.                         │    │
│  │  A 0013            PERFORM CALCUL.   ◄── Destination    │    │
│  │                                                          │    │
│  │  Résultat : Les lignes 10-12 sont copiées après 13     │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  EXEMPLE 2 : Recherche et remplacement                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  Command ===> CHANGE 'WS-OLD' 'WS-NEW' ALL              │    │
│  │                                                          │    │
│  │  Remplace toutes les occurrences de WS-OLD par WS-NEW  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  EXEMPLE 3 : Masquer des commentaires                           │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  Command ===> EXCLUDE ALL '*'                           │    │
│  │                                                          │    │
│  │  Masque toutes les lignes contenant '*' (commentaires) │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  EXEMPLE 4 : Afficher colonnes COBOL                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  Command ===> COLS                                      │    │
│  │                                                          │    │
│  │  =COLS> ----+----1----+----2----+----3----+----4----+-- │    │
│  │  000001        IDENTIFICATION DIVISION.                  │    │
│  │                                                          │    │
│  │  Montre les positions de colonnes                       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

#### Commande COPY (inclusion de membre)

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDE COPY                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Inclure le contenu d'un autre membre dans le fichier courant  │
│                                                                  │
│  Syntaxe :                                                      │
│  COPY membre                - Depuis la même bibliothèque      │
│  COPY membre FROM dsname    - Depuis une autre bibliothèque    │
│                                                                  │
│  Exemple :                                                      │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  A 0010           WORKING-STORAGE SECTION.              │    │
│  │                                                          │    │
│  │  Command ===> COPY CUSTCOPY                             │    │
│  │                                                          │    │
│  │  Insère le contenu de CUSTCOPY après la ligne 10       │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Synthèse

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLÉS DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ✓ ISPF = Interface plein écran pour z/OS                      │
│    • S'exécute sous TSO                                        │
│    • Menus hiérarchiques et panels                             │
│    • Environnement de développement intégré                    │
│                                                                  │
│  ✓ NAVIGATION                                                   │
│    • Menus : 0-Settings, 1-View, 2-Edit, 3-Utilities, S-SDSF  │
│    • Raccourcis : =3.4 (Dslist), =2 (Edit), =S (SDSF)         │
│    • Touches : PF1=Help, PF3=Exit, PF7/8=Scroll                │
│                                                                  │
│  ✓ FICHIERS LOG ET LIST                                         │
│    • LOG : Journal de session                                  │
│    • LIST : Sorties des commandes                              │
│    • Configuration via Option 0                                │
│                                                                  │
│  ✓ VIEW ET EDIT                                                 │
│    • View : Lecture seule avec commandes éditeur               │
│    • Edit : Modification complète des fichiers                 │
│                                                                  │
│  ✓ UTILITIES (Option 3)                                         │
│    • 3.1 Library : Gestion membres PDS                         │
│    • 3.2 Data Set : Création/suppression datasets              │
│    • 3.4 Dslist : Liste et actions sur datasets                │
│                                                                  │
│  ✓ SDSF                                                         │
│    • Gestion du spool et des jobs                              │
│    • ST : Status des jobs                                      │
│    • Commandes : ?, S, P, C, A, H                              │
│                                                                  │
│  ✓ ÉDITEUR ISPF                                                 │
│    • Commandes primaires : FIND, CHANGE, SAVE, COLS            │
│    • Commandes ligne : I, D, C, M, R, >, <, X                  │
│    • Blocs : DD, CC, MM, RR, XX                                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-mémoire ISPF

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MÉMOIRE ISPF                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  RACCOURCIS NAVIGATION                                          │
│  ────────────────────────────────────────────────────────────── │
│  =0         Settings              =3.4       Dslist             │
│  =1         View                  =4         Foreground         │
│  =2         Edit                  =5         Batch              │
│  =3         Utilities             =6         Command            │
│  =3.1       Library               =S         SDSF               │
│  =3.2       Data Set              =X         Exit               │
│                                                                  │
│  TOUCHES DE FONCTION                                            │
│  ────────────────────────────────────────────────────────────── │
│  PF1  Help        PF5  RFind       PF9  Swap                   │
│  PF2  Split       PF6  RChange     PF10 Left                   │
│  PF3  Exit        PF7  Up          PF11 Right                  │
│  PF4  Return      PF8  Down        PF12 Retrieve               │
│                                                                  │
│  DSLIST (3.4) - COMMANDES LIGNE                                 │
│  ────────────────────────────────────────────────────────────── │
│  e  Edit          b  Browse        v  View                     │
│  d  Delete        r  Rename        i  Info                     │
│  m  Members       c  Copy          j  Submit JCL               │
│  z  Compress      s  Short info                                │
│                                                                  │
│  ÉDITEUR - COMMANDES PRIMAIRES                                  │
│  ────────────────────────────────────────────────────────────── │
│  FIND 'xxx'           Rechercher                               │
│  FIND 'xxx' ALL       Toutes occurrences                       │
│  CHANGE 'x' 'y'       Remplacer                                │
│  CHANGE 'x' 'y' ALL   Remplacer tout                           │
│  SAVE                 Sauvegarder                              │
│  CANCEL               Annuler modifications                    │
│  COLS                 Afficher règle colonnes                  │
│  RESET                Réinitialiser affichage                  │
│  EXCLUDE 'xxx'        Masquer lignes                           │
│                                                                  │
│  ÉDITEUR - COMMANDES LIGNE                                      │
│  ────────────────────────────────────────────────────────────── │
│  I/In    Insérer       D/Dn/DD   Supprimer                     │
│  C/Cn/CC Copier        M/Mn/MM   Déplacer                      │
│  R/Rn/RR Répéter       X/Xn/XX   Exclure                       │
│  A       Après         B         Avant                         │
│  >/>>    Droite        </<< n    Gauche                        │
│  UC      Majuscules    LC        Minuscules                    │
│                                                                  │
│  SDSF - COMMANDES                                               │
│  ────────────────────────────────────────────────────────────── │
│  ST       Status jobs              ?  Voir sorties             │
│  DA       Jobs actifs              S  Sélectionner             │
│  O        Output queue             P  Purger                   │
│  H        Held queue               C  Annuler                  │
│  LOG      Journal système          A  Libérer                  │
│  PREFIX x Filtrer par préfixe                                  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - TSO](03-tso.md) | [Chapitre V - JCL](05-jcl.md) |
