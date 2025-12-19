# Chapitre IV - Les Utilitaires

## Introduction

IBM propose un ensemble d'utilitaires qui permettent de manipuler les Data Sets VSAM et non-VSAM. Ces utilitaires peuvent etre integres dans les JCL de travail pour automatiser les operations de gestion de fichiers.

```
┌─────────────────────────────────────────────────────────────────┐
│                    UTILITAIRES IBM                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐       │
│   │ IEFBR14  │  │ IEBGENER │  │ IEBCOPY  │  │ IEBCOMPR │       │
│   │  (Dummy) │  │  (Copie) │  │  (PDS)   │  │ (Compare)│       │
│   └──────────┘  └──────────┘  └──────────┘  └──────────┘       │
│                                                                 │
│   ┌──────────────────────┐  ┌──────────────────────────┐       │
│   │        IDCAMS        │  │          SORT            │       │
│   │  (VSAM, Catalogue)   │  │    (Tri, Fusion)         │       │
│   └──────────────────────┘  └──────────────────────────┘       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 1. Definition des Data Sets

Avant d'utiliser les utilitaires, il est essentiel de comprendre les types de Data Sets qu'ils manipulent.

### 1.1 Data Set Sequentiel (PS - Physical Sequential)

Organisation lineaire des enregistrements, acces sequentiel uniquement.

```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA SET SEQUENTIEL (PS)                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌────────┬────────┬────────┬────────┬────────┬────────┐       │
│  │ Enreg  │ Enreg  │ Enreg  │ Enreg  │ Enreg  │  ...   │       │
│  │   1    │   2    │   3    │   4    │   5    │        │       │
│  └────────┴────────┴────────┴────────┴────────┴────────┘       │
│                                                                 │
│  Caracteristiques :                                             │
│  - Lecture/ecriture sequentielle                                │
│  - DSORG=PS                                                     │
│  - Ideal pour : fichiers de donnees, logs, rapports             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Definition JCL :**

```jcl
//SEQFILE  DD DSN=USER.DATA.SEQUEN,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PS),
//            UNIT=SYSDA
```

### 1.2 Data Set Partitionne (PO - Partitioned Organization)

Bibliotheque contenant plusieurs membres, chacun accessible directement.

```
┌─────────────────────────────────────────────────────────────────┐
│                  DATA SET PARTITIONNE (PDS)                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐                                            │
│  │   DIRECTORY     │  <- Index des membres                      │
│  ├─────────────────┤                                            │
│  │ MEMBER1  -> TTR │                                            │
│  │ MEMBER2  -> TTR │                                            │
│  │ MEMBER3  -> TTR │                                            │
│  │ ...             │                                            │
│  └─────────────────┘                                            │
│           │                                                     │
│           v                                                     │
│  ┌─────────────────────────────────────────────────────┐       │
│  │ DATA AREA                                           │       │
│  │ ┌─────────┐ ┌─────────┐ ┌─────────┐                │       │
│  │ │ MEMBER1 │ │ MEMBER2 │ │ MEMBER3 │ ...            │       │
│  │ │ (code)  │ │ (code)  │ │ (code)  │                │       │
│  │ └─────────┘ └─────────┘ └─────────┘                │       │
│  └─────────────────────────────────────────────────────┘       │
│                                                                 │
│  Caracteristiques :                                             │
│  - Acces direct aux membres via le directory                    │
│  - DSORG=PO                                                     │
│  - Ideal pour : sources, JCL, COPYBOOK, procedures              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Definition JCL :**

```jcl
//PDSLIB   DD DSN=USER.SOURCE.COBOL,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10,20)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0,DSORG=PO),
//            UNIT=SYSDA
```

**Note :** Le troisieme parametre de SPACE (20) reserve l'espace pour le directory.

### 1.3 PDS Extended (PDSE)

Evolution du PDS avec des avantages significatifs.

```
┌─────────────────────────────────────────────────────────────────┐
│                    PDS vs PDSE                                  │
├────────────────────────────┬────────────────────────────────────┤
│          PDS               │              PDSE                  │
├────────────────────────────┼────────────────────────────────────┤
│ Directory de taille fixe   │ Directory extensible               │
│ Fragmentation possible     │ Reutilisation automatique          │
│ Compression manuelle       │ Pas de compression necessaire      │
│ Moins de ressources        │ Plus de fonctionnalites            │
│ DSNTYPE non specifie       │ DSNTYPE=LIBRARY                    │
└────────────────────────────┴────────────────────────────────────┘
```

**Definition PDSE :**

```jcl
//PDSELIB  DD DSN=USER.SOURCE.PDSE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            DSNTYPE=LIBRARY,
//            UNIT=SYSDA
```

---

## 2. Historique d'Utilisation des Utilitaires

### 2.1 Evolution des utilitaires

```
┌─────────────────────────────────────────────────────────────────┐
│               HISTORIQUE DES UTILITAIRES IBM                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Annees 1960-1970 :                                             │
│  ├── IEFBR14   : Utilitaire minimal (BR 14 = Branch Return)     │
│  ├── IEBGENER  : Copie sequentielle                             │
│  └── IEBCOPY   : Gestion des PDS                                │
│                                                                 │
│  Annees 1970-1980 :                                             │
│  ├── IDCAMS    : Gestion VSAM et catalogues                     │
│  └── DFSORT    : Tri haute performance IBM                      │
│                                                                 │
│  Annees 1980-present :                                          │
│  ├── ICETOOL   : Extensions SORT                                │
│  ├── SYNCSORT  : Alternative commerciale                        │
│  └── PDSE      : Evolution des PDS                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 Cas d'utilisation courants

| Besoin | Utilitaire recommande |
|--------|----------------------|
| Creer/supprimer un dataset vide | IEFBR14 |
| Copier un fichier sequentiel | IEBGENER |
| Copier/compresser un PDS | IEBCOPY |
| Comparer deux fichiers | IEBCOMPR |
| Gerer fichiers VSAM | IDCAMS |
| Trier, fusionner, reformater | SORT |

---

## 3. IEFBR14 - Utilitaire Minimal

### 3.1 Presentation

IEFBR14 est le programme le plus simple de z/OS. Son code assembleur contient une seule instruction : `BR 14` (Branch to Register 14), qui retourne immediatement au systeme.

```
┌─────────────────────────────────────────────────────────────────┐
│                        IEFBR14                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Code source complet :                                          │
│  ┌─────────────────────────────────────────────┐               │
│  │  IEFBR14  CSECT                             │               │
│  │           SR    15,15     <- RC=0           │               │
│  │           BR    14        <- Return         │               │
│  │           END                               │               │
│  └─────────────────────────────────────────────┘               │
│                                                                 │
│  Utilite : Les DD associees sont traitees par JES,              │
│            permettant de creer ou supprimer des datasets        │
│            sans aucun traitement programme.                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 3.2 Utilisations principales

**a) Creation d'un dataset vide :**

```jcl
//CREER    EXEC PGM=IEFBR14
//NOUVEAU  DD DSN=USER.NOUVEAU.FICHIER,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
```

**b) Suppression d'un dataset :**

```jcl
//SUPPR    EXEC PGM=IEFBR14
//ANCIEN   DD DSN=USER.ANCIEN.FICHIER,
//            DISP=(OLD,DELETE,DELETE)
```

**c) Creation d'un PDS vide :**

```jcl
//CREAPDS  EXEC PGM=IEFBR14
//BIBLIO   DD DSN=USER.SOURCE.PDS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(50,10,20)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
```

**d) Creation d'un GDG Base (Generation Data Group) :**

```jcl
//CREABASE EXEC PGM=IEFBR14
//GDGBASE  DD DSN=USER.GDG.BASE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,0),
//            DCB=(RECFM=FB,LRECL=80),
//            UNIT=SYSDA
```

### 3.3 Bonnes pratiques

```
┌─────────────────────────────────────────────────────────────────┐
│            IEFBR14 - BONNES PRATIQUES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ✓ Utiliser pour les operations de catalogue uniquement         │
│  ✓ Preferer IDCAMS DELETE pour les suppressions conditionnelles │
│  ✓ Toujours specifier DISP correctement :                       │
│    - (NEW,CATLG,DELETE) pour creation                           │
│    - (OLD,DELETE,DELETE) pour suppression                       │
│                                                                 │
│  ✗ Ne pas utiliser pour copier ou traiter des donnees           │
│  ✗ Ne pas oublier les parametres DCB pour les nouveaux DS       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 4. IEBGENER - Copie Sequentielle

### 4.1 Presentation

IEBGENER copie des fichiers sequentiels ou des membres de PDS. C'est l'utilitaire de copie le plus utilise pour les fichiers non-VSAM.

```
┌─────────────────────────────────────────────────────────────────┐
│                        IEBGENER                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SYSUT1 (Input)            SYSUT2 (Output)                     │
│   ┌──────────────┐          ┌──────────────┐                   │
│   │ Fichier      │   ──>    │ Fichier      │                   │
│   │ Source       │  Copie   │ Destination  │                   │
│   └──────────────┘          └──────────────┘                   │
│                                                                 │
│   DD Requises :                                                 │
│   - SYSUT1   : Fichier source (input)                           │
│   - SYSUT2   : Fichier destination (output)                     │
│   - SYSPRINT : Messages de l'utilitaire                         │
│   - SYSIN    : Commandes de controle (ou DUMMY)                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Syntaxe de base

```jcl
//COPIE    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=fichier.source,DISP=SHR
//SYSUT2   DD DSN=fichier.dest,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
```

### 4.3 Exemples pratiques

**a) Copie simple de fichier sequentiel :**

```jcl
//STEP01   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=PROD.DATA.CLIENT,DISP=SHR
//SYSUT2   DD DSN=TEST.DATA.CLIENT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=*.SYSUT1,
//            UNIT=SYSDA
```

**b) Copie d'un membre de PDS :**

```jcl
//STEP02   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=USER.SOURCE.COBOL(PGMCALC),DISP=SHR
//SYSUT2   DD DSN=USER.BACKUP.COBOL(PGMCALC),
//            DISP=SHR
```

**c) Creation de fichier avec donnees in-stream :**

```jcl
//STEP03   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
DONNEE LIGNE 1
DONNEE LIGNE 2
DONNEE LIGNE 3
/*
//SYSUT2   DD DSN=USER.NOUVEAU.DATA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
```

**d) Impression d'un fichier :**

```jcl
//PRINT    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=USER.RAPPORT.DATA,DISP=SHR
//SYSUT2   DD SYSOUT=*
```

### 4.4 Cartes de controle SYSIN

IEBGENER accepte des cartes de controle pour des operations avancees :

```jcl
//SYSIN    DD *
  GENERATE MAXFLDS=3,MAXLITS=20
  RECORD FIELD=(10,1,,1),
         FIELD=(20,15,,11),
         FIELD=(8,'CONSTANT',31)
/*
```

| Carte | Fonction |
|-------|----------|
| GENERATE | Definit les maximums (champs, litteraux) |
| RECORD | Reorganise les champs en sortie |
| FIELD | Definition d'un champ (longueur, position in, position out) |
| LABELS | Gestion des labels de bande |

### 4.5 Codes retour

| RC | Signification |
|----|---------------|
| 0 | Succes |
| 4 | Warning (ex: fichier vide) |
| 8 | Erreur de syntaxe dans SYSIN |
| 12 | Erreur d'entree/sortie |
| 16 | Erreur grave |

---

## 5. IEBCOPY - Gestion des PDS

### 5.1 Presentation

IEBCOPY est l'utilitaire principal pour la gestion des Partitioned Data Sets (PDS et PDSE).

```
┌─────────────────────────────────────────────────────────────────┐
│                        IEBCOPY                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Fonctions principales :                                       │
│   ┌──────────────────────────────────────────────────────┐     │
│   │ COPY      : Copier membres entre PDS                 │     │
│   │ COMPRESS  : Recuperer l'espace inutilise (in-place)  │     │
│   │ UNLOAD    : Decharger PDS vers sequentiel            │     │
│   │ LOAD      : Recharger sequentiel vers PDS            │     │
│   └──────────────────────────────────────────────────────┘     │
│                                                                 │
│   DD Requises :                                                 │
│   - SYSPRINT : Messages                                         │
│   - SYSIN    : Commandes de controle                            │
│   - ddname   : PDS source(s) et cible(s)                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 5.2 Copie de PDS complet

```jcl
//COPYPDS  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=USER.SOURCE.COBOL,DISP=SHR
//OUTPDS   DD DSN=USER.BACKUP.COBOL,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5,50)),
//            DCB=*.INPDS,
//            UNIT=SYSDA
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
/*
```

### 5.3 Copie selective de membres

```jcl
//COPYMEM  EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=USER.SOURCE.COBOL,DISP=SHR
//OUTPDS   DD DSN=USER.BACKUP.COBOL,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  SELECT MEMBER=(PGMCALC,PGMPRINT,PGMVALID)
/*
```

### 5.4 Exclusion de membres

```jcl
//COPYEXCL EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=USER.SOURCE.COBOL,DISP=SHR
//OUTPDS   DD DSN=USER.BACKUP.COBOL,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  EXCLUDE MEMBER=(TESTPGM,OLDPGM,TEMPWORK)
/*
```

### 5.5 Renommer un membre lors de la copie

```jcl
//COPYRN   EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=USER.SOURCE.COBOL,DISP=SHR
//OUTPDS   DD DSN=USER.BACKUP.COBOL,DISP=SHR
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  SELECT MEMBER=((PGMOLD,PGMNEW,R))
/*
```

**Note :** Le `R` indique Replace si le membre existe deja.

### 5.6 Compression d'un PDS (in-place)

La compression recupere l'espace des membres supprimes :

```jcl
//COMPRESS EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//MYPDS    DD DSN=USER.SOURCE.COBOL,DISP=OLD
//SYSIN    DD *
  COPY OUTDD=MYPDS,INDD=MYPDS
/*
```

```
┌─────────────────────────────────────────────────────────────────┐
│               COMPRESSION PDS - AVANT/APRES                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  AVANT compression :                                            │
│  ┌────────┬────────┬────────┬────────┬────────┬────────┐       │
│  │ MEM1   │ (vide) │ MEM2   │ (vide) │ (vide) │ MEM3   │       │
│  └────────┴────────┴────────┴────────┴────────┴────────┘       │
│            ^^^^^^^          ^^^^^^^^^^^^^^^^^                   │
│            Espace perdu (membres supprimes)                     │
│                                                                 │
│  APRES compression :                                            │
│  ┌────────┬────────┬────────┬──────────────────────────┐       │
│  │ MEM1   │ MEM2   │ MEM3   │     ESPACE LIBRE         │       │
│  └────────┴────────┴────────┴──────────────────────────┘       │
│                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^        │
│                              Espace recupere                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 5.7 Decharger (UNLOAD) un PDS

```jcl
//UNLOAD   EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS    DD DSN=USER.SOURCE.COBOL,DISP=SHR
//OUTSEQ   DD DSN=USER.SOURCE.UNLOAD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=VB,LRECL=256,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
  COPY OUTDD=OUTSEQ,INDD=INPDS
/*
```

### 5.8 Recharger (LOAD) un PDS

```jcl
//RELOAD   EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INSEQ    DD DSN=USER.SOURCE.UNLOAD,DISP=SHR
//OUTPDS   DD DSN=USER.SOURCE.RESTORED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5,50)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
  COPY OUTDD=OUTPDS,INDD=INSEQ
/*
```

### 5.9 Codes retour IEBCOPY

| RC | Signification |
|----|---------------|
| 0 | Succes complet |
| 4 | Copie partielle reussie |
| 8 | Erreur, certains membres non copies |
| 12 | PDS non utilisable apres operation |
| 16 | Erreur grave |

---

## 6. IEBCOMPR - Comparaison de Fichiers

### 6.1 Presentation

IEBCOMPR compare deux fichiers sequentiels ou deux membres de PDS enregistrement par enregistrement.

```
┌─────────────────────────────────────────────────────────────────┐
│                        IEBCOMPR                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SYSUT1                    SYSUT2                              │
│   ┌──────────────┐          ┌──────────────┐                   │
│   │ Fichier 1    │   <=>    │ Fichier 2    │                   │
│   │ (Reference)  │ Compare  │ (A verifier) │                   │
│   └──────────────┘          └──────────────┘                   │
│           │                         │                           │
│           └────────────┬────────────┘                           │
│                        v                                        │
│              ┌──────────────────┐                               │
│              │    SYSPRINT      │                               │
│              │ Rapport ecarts   │                               │
│              └──────────────────┘                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 6.2 Comparaison de fichiers sequentiels

```jcl
//COMPARE  EXEC PGM=IEBCOMPR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PROD.DATA.MASTER,DISP=SHR
//SYSUT2   DD DSN=TEST.DATA.MASTER,DISP=SHR
//SYSIN    DD *
  COMPARE TYPORG=PS
/*
```

### 6.3 Comparaison de membres PDS

```jcl
//COMPPDS  EXEC PGM=IEBCOMPR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=PROD.SOURCE.COBOL,DISP=SHR
//SYSUT2   DD DSN=TEST.SOURCE.COBOL,DISP=SHR
//SYSIN    DD *
  COMPARE TYPORG=PO
/*
```

### 6.4 Parametres COMPARE

| Parametre | Valeurs | Description |
|-----------|---------|-------------|
| TYPORG | PS, PO | Type d'organisation (Sequentiel, Partitionne) |

### 6.5 Codes retour IEBCOMPR

| RC | Signification |
|----|---------------|
| 0 | Fichiers identiques |
| 8 | Differences trouvees |
| 12 | Erreur d'acces aux fichiers |
| 16 | Erreur grave de l'utilitaire |

### 6.6 Exemple de rapport

```
IEB1071I MISCOMPARE SYSUT1 RECORD NUMBER 150
         KEY OF RECORD IN SYSUT1   00000150
         KEY OF RECORD IN SYSUT2   00000150
         DATA IN SYSUT1 RECORD:
         CLIENT DUPONT JEAN        PARIS
         DATA IN SYSUT2 RECORD:
         CLIENT DUPONT JEAN        LYON
```

---

## 7. IDCAMS - Access Method Services

### 7.1 Presentation

IDCAMS (Access Method Services) est l'utilitaire le plus puissant pour la gestion des fichiers VSAM et du catalogue.

```
┌─────────────────────────────────────────────────────────────────┐
│                         IDCAMS                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Fonctions principales :                                       │
│   ┌──────────────────────────────────────────────────────┐     │
│   │ DEFINE    : Creer fichiers VSAM et catalogues        │     │
│   │ DELETE    : Supprimer fichiers et entrees catalogue  │     │
│   │ REPRO     : Copier fichiers (VSAM et non-VSAM)       │     │
│   │ PRINT     : Afficher le contenu d'un fichier         │     │
│   │ LISTCAT   : Lister les entrees du catalogue          │     │
│   │ ALTER     : Modifier les attributs                   │     │
│   └──────────────────────────────────────────────────────┘     │
│                                                                 │
│   DD Requises :                                                 │
│   - SYSPRINT : Messages et rapports                             │
│   - SYSIN    : Commandes IDCAMS                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 7.2 Structure de base

```jcl
//IDCAMS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  commandes IDCAMS ici
/*
```

### 7.3 DELETE - Suppression

**a) Suppression simple :**

```jcl
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.FICHIER.DATA
/*
```

**b) Suppression conditionnelle (ignorer si inexistant) :**

```jcl
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.FICHIER.DATA
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
```

**c) Suppression VSAM :**

```jcl
//DELVS    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.VSAM.KSDS CLUSTER PURGE
/*
```

### 7.4 DEFINE CLUSTER - Creation VSAM

**a) KSDS (Key Sequenced Data Set) :**

```jcl
//DEFKSDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                            -
           NAME(USER.VSAM.KSDS)               -
           INDEXED                            -
           KEYS(8 0)                          -
           RECORDSIZE(100 100)                -
           TRACKS(10 5)                       -
           SHAREOPTIONS(2 3)                  -
         )                                    -
         DATA (                               -
           NAME(USER.VSAM.KSDS.DATA)          -
         )                                    -
         INDEX (                              -
           NAME(USER.VSAM.KSDS.INDEX)         -
         )
/*
```

**b) ESDS (Entry Sequenced Data Set) :**

```jcl
//DEFESDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                            -
           NAME(USER.VSAM.ESDS)               -
           NONINDEXED                         -
           RECORDSIZE(80 80)                  -
           TRACKS(5 2)                        -
         )                                    -
         DATA (                               -
           NAME(USER.VSAM.ESDS.DATA)          -
         )
/*
```

**c) RRDS (Relative Record Data Set) :**

```jcl
//DEFRRDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                            -
           NAME(USER.VSAM.RRDS)               -
           NUMBERED                           -
           RECORDSIZE(100 100)                -
           TRACKS(5 2)                        -
         )                                    -
         DATA (                               -
           NAME(USER.VSAM.RRDS.DATA)          -
         )
/*
```

### 7.5 REPRO - Copie de fichiers

```
┌─────────────────────────────────────────────────────────────────┐
│                     IDCAMS REPRO                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Conversions possibles :                                       │
│                                                                 │
│   PS  ────>  VSAM    : Chargement initial                       │
│   VSAM ────>  PS     : Dechargement                             │
│   VSAM ────>  VSAM   : Copie VSAM vers VSAM                     │
│   PS  ────>  PS      : Alternative a IEBGENER                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**a) Chargement d'un fichier PS vers VSAM :**

```jcl
//LOADVSAM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=USER.SEQ.INPUT,DISP=SHR
//OUTFILE  DD DSN=USER.VSAM.KSDS,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
```

**b) Dechargement VSAM vers PS :**

```jcl
//UNLDVSAM EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=USER.VSAM.KSDS,DISP=SHR
//OUTFILE  DD DSN=USER.SEQ.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
```

**c) Copie selective (par cle) :**

```jcl
//REPROSEL EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=USER.VSAM.KSDS,DISP=SHR
//OUTFILE  DD DSN=USER.VSAM.EXTRACT,DISP=SHR
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE) -
        FROMKEY(00001000) TOKEY(00002000)
/*
```

### 7.6 PRINT - Affichage

```jcl
//PRINT    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD DSN=USER.VSAM.KSDS,DISP=SHR
//SYSIN    DD *
  PRINT INFILE(INFILE) CHARACTER COUNT(100)
/*
```

| Option | Description |
|--------|-------------|
| CHARACTER | Affichage en caracteres |
| HEX | Affichage hexadecimal |
| DUMP | Les deux formats |
| COUNT(n) | Limiter a n enregistrements |
| SKIP(n) | Sauter n enregistrements |

### 7.7 LISTCAT - Catalogue

```jcl
//LISTCAT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(USER.VSAM.*) ALL
/*
```

| Option | Description |
|--------|-------------|
| ENTRIES(mask) | Filtrer par masque |
| ALL | Toutes les informations |
| NAME | Noms uniquement |
| VOLUME | Informations volume |
| HISTORY | Historique creation/expiration |

### 7.8 Gestion des erreurs IDCAMS

```jcl
//IDCAMS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE USER.FICHIER.OLD
  IF LASTCC = 8 THEN DO
    SET MAXCC = 0
    /* Fichier inexistant - OK */
  END

  DEFINE CLUSTER (NAME(USER.FICHIER.NEW) ...)
  IF LASTCC > 0 THEN DO
    SET MAXCC = 16
    /* Erreur creation - ABORT */
  END
/*
```

### 7.9 Codes retour IDCAMS

| RC | Signification |
|----|---------------|
| 0 | Succes |
| 4 | Warning |
| 8 | Erreur (ex: fichier inexistant pour DELETE) |
| 12 | Erreur logique |
| 16 | Erreur grave |

---

## 8. SORT - Tri et Fusion

### 8.1 Presentation

SORT (DFSORT ou SYNCSORT) est l'utilitaire de tri et de manipulation de donnees le plus utilise.

```
┌─────────────────────────────────────────────────────────────────┐
│                          SORT                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Fonctions principales :                                       │
│   ┌──────────────────────────────────────────────────────┐     │
│   │ SORT   : Trier les enregistrements                   │     │
│   │ MERGE  : Fusionner fichiers deja tries              │     │
│   │ COPY   : Copier avec ou sans transformation          │     │
│   │ INCLUDE/OMIT  : Filtrer les enregistrements          │     │
│   │ INREC/OUTREC  : Reformater les enregistrements       │     │
│   │ SUM    : Agreger les donnees                         │     │
│   └──────────────────────────────────────────────────────┘     │
│                                                                 │
│   DD Requises :                                                 │
│   - SORTIN   : Fichier d'entree                                 │
│   - SORTOUT  : Fichier de sortie                                │
│   - SYSOUT   : Messages                                         │
│   - SYSIN    : Commandes SORT                                   │
│   - SORTWKnn : Fichiers de travail (optionnel)                  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 8.2 Structure de base

```jcl
//SORT     EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=fichier.entree,DISP=SHR
//SORTOUT  DD DSN=fichier.sortie,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
```

### 8.3 Carte SORT FIELDS

```
SORT FIELDS=(position,longueur,format,ordre,...)
```

**Formats de donnees :**

| Format | Description |
|--------|-------------|
| CH | Caractere (EBCDIC) |
| ZD | Decimal zone (signe dans zone) |
| PD | Decimal packe |
| BI | Binaire |
| FI | Binaire signe (Fixed Integer) |
| AC | ASCII Character |

**Ordre de tri :**

| Code | Description |
|------|-------------|
| A | Ascendant (croissant) |
| D | Descendant (decroissant) |

### 8.4 Exemples de tri

**a) Tri simple sur une cle :**

```jcl
//TRISIMPL EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.SORTED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=(1,8,CH,A)
/*
```

**b) Tri sur plusieurs cles :**

```jcl
//TRIMULTI EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.SORTED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* Tri par departement (asc), puis par nom (asc), puis par date (desc)
  SORT FIELDS=(1,3,CH,A,10,20,CH,A,50,8,PD,D)
/*
```

### 8.5 INCLUDE et OMIT - Filtrage

```
┌─────────────────────────────────────────────────────────────────┐
│                    INCLUDE vs OMIT                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   INCLUDE : Ne garde QUE les enregistrements correspondants     │
│   OMIT    : Exclut les enregistrements correspondants           │
│                                                                 │
│   Operateurs de comparaison :                                   │
│   EQ (=), NE, GT (>), GE (>=), LT (<), LE (<=)                  │
│                                                                 │
│   Operateurs logiques :                                         │
│   AND (&), OR (|)                                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**a) INCLUDE - Garder certains enregistrements :**

```jcl
//SORTINCL EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.PARIS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* Garder uniquement les clients de Paris (code 75)
  INCLUDE COND=(40,2,CH,EQ,C'75')
  SORT FIELDS=(1,8,CH,A)
/*
```

**b) OMIT - Exclure certains enregistrements :**

```jcl
//SORTOMIT EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.HORS75,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* Exclure les clients de Paris
  OMIT COND=(40,2,CH,EQ,C'75')
  SORT FIELDS=(1,8,CH,A)
/*
```

**c) Conditions multiples :**

```jcl
//SORTCOND EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.SELECT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* Garder Paris OU Lyon avec montant > 1000
  INCLUDE COND=((40,2,CH,EQ,C'75',OR,40,2,CH,EQ,C'69'),
                AND,(50,7,PD,GT,+1000))
  SORT FIELDS=(1,8,CH,A)
/*
```

### 8.6 INREC et OUTREC - Reformatage

```
┌─────────────────────────────────────────────────────────────────┐
│                  INREC vs OUTREC                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   INREC  : Reformatage AVANT le tri (performance)               │
│   OUTREC : Reformatage APRES le tri                             │
│                                                                 │
│   Utiliser INREC quand possible :                               │
│   - Reduit la taille des enregistrements a trier                │
│   - Ameliore les performances                                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**a) Extraction de champs :**

```jcl
//SORTEXTR EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.EXTRAIT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=30,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
* Extraire : code(8), nom(20), ville(2) = 30 caracteres
  SORT FIELDS=(1,8,CH,A)
  OUTREC FIELDS=(1,8,10,20,40,2)
/*
```

**b) Ajout de constantes :**

```jcl
//SORTADD  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.OUTPUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=(RECFM=FB,LRECL=90,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
* Ajouter 'TRAITE' en fin d'enregistrement
  SORT FIELDS=COPY
  OUTREC FIELDS=(1,80,C'TRAITE  ')
/*
```

### 8.7 FINDREP - Recherche et remplacement

FINDREP permet de rechercher et remplacer des chaines de caracteres dans les enregistrements.

```jcl
//SORTFIND EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.OUTPUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTREC FINDREP=(IN=C'ANCIEN',OUT=C'NOUVEAU')
/*
```

**Syntaxe FINDREP :**

```
OUTREC FINDREP=(IN=C'chaine_recherche',OUT=C'chaine_remplacement')
```

**Exemples pratiques :**

```jcl
* Remplacer une valeur par une autre
  OUTREC FINDREP=(IN=C'OUI',OUT=C'NON')

* Remplacer un code par un libelle
  OUTREC FINDREP=(IN=C'75',OUT=C'PARIS')

* Remplacements multiples
  OUTREC FINDREP=(IN=C'ANCIEN',OUT=C'NOUVEAU',
                 IN=C'OLD',OUT=C'NEW')

* Remplacement avec chaines de longueurs differentes
  OUTREC FINDREP=(IN=C'ABC',OUT=C'ABCDEF',INOUT=(1,80))
```

**Notes :**
- Si la chaine de remplacement est plus courte, des espaces sont ajoutes
- Si plus longue, specifier INOUT pour definir la zone de travail
- Peut etre combine avec SORT ou COPY

---

### 8.8 MERGE - Fusion de fichiers tries

```jcl
//MERGE    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN01 DD DSN=USER.DATA.FILE1,DISP=SHR
//SORTIN02 DD DSN=USER.DATA.FILE2,DISP=SHR
//SORTIN03 DD DSN=USER.DATA.FILE3,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.MERGED,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,2),RLSE),
//            DCB=*.SORTIN01,
//            UNIT=SYSDA
//SYSIN    DD *
  MERGE FIELDS=(1,8,CH,A)
/*
```

**Note :** Les fichiers en entree DOIVENT etre deja tries sur la meme cle.

### 8.9 SUM - Suppression des doublons

```jcl
//SORTSUM  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.UNIQUE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* Supprimer les doublons bases sur la cle
  SORT FIELDS=(1,8,CH,A)
  SUM FIELDS=NONE
/*
```

### 8.10 COPY - Copie sans tri

```jcl
//SORTCOPY EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//SORTOUT  DD DSN=USER.DATA.COPIE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=COPY
/*
```

### 8.11 OUTFIL - Sorties multiples

```jcl
//SORTMULT EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=USER.DATA.INPUT,DISP=SHR
//PARIS    DD DSN=USER.DATA.PARIS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//LYON     DD DSN=USER.DATA.LYON,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//AUTRES   DD DSN=USER.DATA.AUTRES,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(2,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
  SORT FIELDS=(1,8,CH,A)
  OUTFIL FNAMES=PARIS,INCLUDE=(40,2,CH,EQ,C'75')
  OUTFIL FNAMES=LYON,INCLUDE=(40,2,CH,EQ,C'69')
  OUTFIL FNAMES=AUTRES,SAVE
/*
```

### 8.12 Codes retour SORT

| RC | Signification |
|----|---------------|
| 0 | Succes |
| 4 | Warning (ex: fichier vide) |
| 16 | Erreur fatale |

---

## 9. Resume des Utilitaires

```
┌─────────────────────────────────────────────────────────────────┐
│                RESUME DES UTILITAIRES IBM                       │
├──────────┬──────────────────────────────────────────────────────┤
│ IEFBR14  │ Creer/supprimer datasets via les cartes DD           │
├──────────┼──────────────────────────────────────────────────────┤
│ IEBGENER │ Copier fichiers sequentiels, imprimer, charger       │
├──────────┼──────────────────────────────────────────────────────┤
│ IEBCOPY  │ Gerer PDS : copier, compresser, unload/load          │
├──────────┼──────────────────────────────────────────────────────┤
│ IEBCOMPR │ Comparer fichiers ou membres PDS                     │
├──────────┼──────────────────────────────────────────────────────┤
│ IDCAMS   │ Gerer VSAM et catalogue (define, delete, repro...)   │
├──────────┼──────────────────────────────────────────────────────┤
│ SORT     │ Trier, fusionner, filtrer, reformater                │
└──────────┴──────────────────────────────────────────────────────┘
```

---

## 10. Exercices Pratiques

### Exercice 1 : Operations de base
Creer un JCL qui :
1. Supprime un dataset s'il existe (IDCAMS)
2. Cree un nouveau dataset avec des donnees in-stream (IEBGENER)
3. Copie ce dataset vers un autre (IEBGENER)

### Exercice 2 : Gestion PDS
Creer un JCL qui :
1. Cree un PDS vide (IEFBR14)
2. Copie des membres specifiques depuis un autre PDS (IEBCOPY)
3. Compresse le PDS destination (IEBCOPY)

### Exercice 3 : Tri et filtrage
Creer un JCL qui :
1. Trie un fichier sur deux cles
2. Filtre pour ne garder que certains enregistrements
3. Reformate la sortie pour n'avoir que certains champs

### Exercice 4 : VSAM
Creer un JCL qui :
1. Definit un KSDS VSAM (IDCAMS)
2. Charge des donnees depuis un fichier sequentiel (IDCAMS REPRO)
3. Liste le contenu du catalogue pour verifier (IDCAMS LISTCAT)

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Chapitre III - Procedures](03-procedures.md) | [Travaux Pratiques](../../exercices/jcl/chapitre-05/README.md) |

---
*Formation COBOL - Module JCL*
