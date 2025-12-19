# Chapitre IV - Langage de Commandes AMS

## IV-1. Introduction a IDCAMS

### Qu'est-ce que IDCAMS ?

**IDCAMS = Integrated Data Cluster Access Method Services**

IDCAMS est l'utilitaire principal pour gerer les objets VSAM. Il permet de creer, modifier, supprimer et manipuler les clusters, catalogues, index et autres objets VSAM.

### Structure JCL Standard

```jcl
//JOBNAME  JOB (acct-info),'PROGRAMMER-NAME'
//STEPNAME EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  commandes-AMS
/*
```

### Commandes AMS Principales

| Commande | Fonction |
|----------|----------|
| **DEFINE** | Cree catalogues, clusters, AIX, paths, espaces |
| **DELETE** | Supprime entrees de catalogue |
| **ALTER** | Modifie entrees existantes |
| **LISTCAT** | Liste et repertorie les entrees |
| **PRINT** | Imprime Data Sets VSAM et non-VSAM |
| **REPRO** | Copie Data Sets, convertit formats |
| **EXPORT** | Cree copie de sauvegarde portable |
| **IMPORT** | Restaure depuis sauvegarde |
| **VERIFY** | Verifie/repare fichiers mal fermes |
| **BLDINDEX** | Construit index alternatif |

---

## IV-2. Regles de Codage

### Regles Syntaxiques

| Regle | Description |
|-------|-------------|
| Colonnes | Parametres entre positions 2 et 72 |
| Continuation | Tiret haut `-` pour continuer sur ligne suivante |
| Separateurs | Valeurs separees par espace ou virgule |
| Commentaires | Entre `/*` et `*/` |
| Sous-parametres | Signe `+` pour separer parametre et sous-parametre |

### Exemple de Syntaxe

```jcl
//IDCAMS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  /* Ceci est un commentaire */
  DEFINE CLUSTER ( -
    NAME (FTEST.KSDS) -
    TRACKS (1 1) -
    VOLUMES (ZASYS1) -
    INDEXED -
    KEYS (10 0) -
    RECORDSIZE (100 100) -
  ) -
  DATA (NAME (FTEST.KSDS.DATA)) -
  INDEX (NAME (FTEST.KSDS.INDEX))
/*
```

---

## IV-3. Commandes Modales

### LASTCC et MAXCC

| Variable | Description |
|----------|-------------|
| **LASTCC** | Code condition de la derniere commande executee |
| **MAXCC** | Code condition le plus eleve de toutes les commandes |

### Codes de Condition

| Code | Gravite | Signification |
|------|---------|---------------|
| 0 | Normal | Succes |
| 4 | Mineure | Avertissement, fonction completee |
| 8 | Majeure | Specifications contournees |
| 12 | Logique | Erreur de parametres |
| 16 | Grave | Echec, flux AMS vide |

### SET - Reinitialiser les Variables

```jcl
SET LASTCC = 0
SET MAXCC = 0
```

### IF-THEN-ELSE

```jcl
IF condition THEN
  action
ELSE
  action-alternative
```

### Operateurs de Comparaison

| Operateur | Symbole | Description |
|-----------|---------|-------------|
| EQ | = | Egal |
| NE | <> | Different |
| GT | > | Superieur |
| GE | >= | Superieur ou egal |
| LT | < | Inferieur |
| LE | <= | Inferieur ou egal |

### DO-END - Grouper des Commandes

```jcl
IF LASTCC = 0 THEN DO -
  LISTCAT ALL -
  REPRO INDATASET(...) OUTDATASET(...)
END
```

### Exemple Complet

```jcl
//IDCAMS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (NAME(FTEST.KSDS) ...)
  IF LASTCC = 0 THEN DO -
    LISTCAT ENTRIES(FTEST.KSDS) ALL -
  END -
  ELSE -
    SET MAXCC = 16
/*
```

---

## IV-4. DEFINE USERCATALOG

### Syntaxe

```jcl
DEFINE USERCATALOG (
  NAME(entryname)
  VOLUME(volser)
  {CYLINDERS|RECORDS|TRACKS}(primary [secondary])
  [BUFFERSPACE(size|3072)]
  [CONTROLINTERVALSIZE(size)]
  [FREESPACE(CI-percent CA-percent|0 0)]
  [RECORDSIZE(average maximum|4086 32400)]
  [SHAREOPTIONS(crossregion crosssystem|3 4)]
  [TO(date)|FOR(days)]
  [OWNER(ownerid)]
) -
[DATA(...)] -
[INDEX(...)] -
[CATALOG(mastercatname)]
```

### Parametres Obligatoires

| Parametre | Description |
|-----------|-------------|
| NAME | Nom du catalogue |
| VOLUME | Volume de stockage |
| Allocation | CYLINDERS/RECORDS/TRACKS |

### Exemple

```jcl
//DEFCATUR JOB 'DEF CATALOG',CLASS=A,MSGCLASS=A
//IDCAMS   EXEC PGM=IDCAMS,REGION=4096K
//SYSPRINT DD SYSOUT=A
//ZAPRD4   DD UNIT=3390,VOL=SER=ZAPRD4,DISP=OLD
//SYSIN    DD *
  DEFINE USERCATALOG ( -
    NAME (CATUSER.TEST.PRD4) -
    VOLUME (ZAPRD4) -
    TRACKS (2000 200) -
    FOR(9999))
  IF LASTCC = 0 THEN -
    LISTCAT ALL CATALOG (CATUSER.TEST.PRD4)
/*
```

---

## IV-5. DEFINE SPACE

### Syntaxe

```jcl
DEFINE SPACE (
  {CANDIDATE}
  {CYLINDERS|RECORDS|TRACKS}(primary [secondary])
  VOLUME(volser)
  [FILE(ddname)]
) -
CATALOG(catname)
```

### Usage

Allouer ou elargir l'espace d'un User Catalog.

### Exemple

```jcl
//DEFSPC   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SPDISK02 DD UNIT=3390,VOL=SER=DISK02,DISP=OLD
//SYSIN    DD *
  DEFINE SPACE ( -
    FILE (SPDISK02) -
    VOLUME (DISK02) -
    TRACKS (13200 0) -
  ) -
  CATALOG (UCDISK02)
/*
```

---

## IV-6. DEFINE CLUSTER

### Syntaxe Generale

```jcl
DEFINE CLUSTER (
  NAME(entryname)
  {CYLINDERS|RECORDS|TRACKS}(primary [secondary])
  VOLUME(volser [volser...])
  [INDEXED|LINEAR|NONINDEXED|NUMBERED]
  [RECORDSIZE(average maximum|4086 4086)]
  [KEYS(length offset|64 0)]
  [CONTROLINTERVALSIZE(size)]
  [FREESPACE(CI-percent CA-percent|0 0)]
  [SHAREOPTIONS(crossregion crosssystem|1 3)]
  [BUFFERSPACE(size|3072)]
  [ERASE|NOERASE]
  [REUSE|NOREUSE]
  [SPEED|RECOVERY]
  [SPANNED|NOSPANNED]
  [WRITECHECK|NOWRITECHECK]
  [TO(date)|FOR(days)]
  [OWNER(ownerid)]
  [MODEL(entryname [catname])]
) -
[DATA(NAME(data-name) ...)] -
[INDEX(NAME(index-name) ...)] -
[CATALOG(catname)]
```

### Parametres Cles

| Parametre | Description |
|-----------|-------------|
| **INDEXED** | Cluster KSDS |
| **NONINDEXED** | Cluster ESDS |
| **NUMBERED** | Cluster RRDS |
| **LINEAR** | Cluster LDS |
| **KEYS(len off)** | Longueur et offset de la cle (KSDS) |
| **RECORDSIZE(avg max)** | Si egaux = fixe, si differents = variable |
| **FREESPACE(ci ca)** | % espace libre CI et CA (KSDS, VRRDS) |
| **REUSE** | Vide le cluster a chaque ouverture OUTPUT |
| **SPEED** | Pas de pre-formatage (plus rapide) |
| **SPANNED** | Enregistrements peuvent traverser CI |

### Parametres par Type de Cluster

| Parametre | ESDS | KSDS | RRDS | LDS |
|-----------|:----:|:----:|:----:|:---:|
| NAME | X | X | X | X |
| VOLUMES | X | X | X | X |
| TRACKS/CYL/REC | X | X | X | X |
| NONINDEXED | X | - | - | - |
| INDEXED | - | X | - | - |
| NUMBERED | - | - | X | - |
| LINEAR | - | - | - | X |
| KEYS | - | X | - | - |
| RECORDSIZE | X | X | X | - |
| FREESPACE | - | X | X* | - |
| SHAREOPTIONS | X | X | X | X |
| REUSE | X | X | X | X |
| SPANNED | X | X | - | - |

*FREESPACE uniquement pour VRRDS

---

## IV-7. Exemples DEFINE CLUSTER par Type

### ESDS (Entry Sequenced)

```jcl
//STEPESDS EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (NAME(FTEST.ESDS) -
    TRACKS (1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NONINDEXED -
    RECORDSIZE(80 80) -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA (NAME(FTEST.ESDS.DATA))
/*
```

### KSDS (Key Sequenced)

```jcl
//STEPKSDS EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (NAME(FTEST.KSDS) -
    TRACKS (1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    INDEXED -
    RECORDSIZE(100 100) -
    KEYS(15 0) -
    FREESPACE(10 10) -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA (NAME(FTEST.KSDS.DATA)) -
  INDEX (NAME(FTEST.KSDS.INDEX))
/*
```

### RRDS (Relative Record)

```jcl
//STEPRRDS EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (NAME(FTEST.RRDS) -
    TRACKS (1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    NUMBERED -
    RECORDSIZE(40 40) -
    SHAREOPTIONS(1 3) -
    NOREUSE) -
  DATA (NAME(FTEST.RRDS.DATA))
/*
```

### LDS (Linear)

```jcl
//STEPLDS  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (NAME(FTEST.LDS) -
    TRACKS (1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    LINEAR -
    SHAREOPTIONS(1 3) -
    REUSE) -
  DATA (NAME(FTEST.LDS.DATA))
/*
```

---

## IV-8. DEFINE ALTERNATEINDEX

### Syntaxe

```jcl
DEFINE AIX (
  NAME(aix-name)
  RELATE(base-cluster-name)
  {CYLINDERS|RECORDS|TRACKS}(primary [secondary])
  VOLUME(volser)
  KEYS(length offset)
  [RECORDSIZE(average maximum)]
  [CONTROLINTERVALSIZE(size)]
  [FREESPACE(CI-percent CA-percent)]
  [SHAREOPTIONS(crossregion crosssystem)]
  [UNIQUEKEY|NONUNIQUEKEY]
  [UPGRADE|NOUPGRADE]
  [REUSE|NOREUSE]
) -
DATA(NAME(aix-data-name)) -
INDEX(NAME(aix-index-name)) -
[CATALOG(catname)]
```

### Parametres Specifiques AIX

| Parametre | Description |
|-----------|-------------|
| RELATE | Nom du cluster de base |
| UNIQUEKEY | Cle AIX unique |
| NONUNIQUEKEY | Cle AIX peut avoir des doublons |
| UPGRADE | Mise a jour automatique de l'AIX |
| NOUPGRADE | Pas de mise a jour automatique |

### Exemple

```jcl
//STEPAIX  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE AIX (NAME(FTEST.AIX.KSDS) -
    RELATE(FTEST.KSDS) -
    TRACKS (1 1) -
    VOLUMES(ZASYS1) -
    CONTROLINTERVALSIZE(4096) -
    FREESPACE(10 10) -
    RECORDSIZE(80 80) -
    KEYS(10 35) -
    UPGRADE -
    REUSE -
    NONUNIQUEKEY) -
  DATA (NAME(FTEST.AIX.KSDS.DATA)) -
  INDEX (NAME(FTEST.AIX.KSDS.INDEX))
/*
```

---

## IV-9. DEFINE PATH

### Syntaxe

```jcl
DEFINE PATH (
  NAME(path-name)
  PATHENTRY(aix-name)
  [UPDATE|NOUPDATE]
  [MODEL(model-name [catalog-name])]
) -
[CATALOG(catname)]
```

### Parametres

| Parametre | Description |
|-----------|-------------|
| NAME | Nom du path |
| PATHENTRY | Nom de l'AIX associe |
| UPDATE | Mise a jour du cluster via le path autorisee |
| NOUPDATE | Lecture seule via le path |

### Exemple

```jcl
//STEPPATH EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE PATH ( -
    NAME(FTEST.PATH) -
    PATHENTRY(FTEST.AIX.KSDS) -
    UPDATE)
/*
```

---

## IV-10. BLDINDEX

### Syntaxe

```jcl
BLDINDEX
  {INFILE(ddname)|INDATASET(input-dataset-name)}
  {OUTFILE(ddname)|OUTDATASET(output-dataset-name)}
  [EXTERNALSORT|INTERNALSORT]
  [SORTCALL|NOSORTCALL]
  [CATALOG(catalog-name)]
```

### Parametres

| Parametre | Description |
|-----------|-------------|
| INDATASET | Nom du cluster de base |
| OUTDATASET | Nom de l'AIX a construire |
| INTERNALSORT | Tri en memoire |
| EXTERNALSORT | Tri externe (DFSORT) |
| NOSORTCALL | Ne pas utiliser SORT |

### Exemple

```jcl
//STEPBLDI EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  BLDINDEX -
    INDATASET(FTEST.KSDS) -
    OUTDATASET(FTEST.AIX.KSDS) -
    NOSORTCALL
/*
```

> **Note :** Le cluster de base doit avoir au moins un enregistrement pour construire l'index.

---

## Resume du Chapitre

| Commande | Usage |
|----------|-------|
| **DEFINE USERCATALOG** | Creer catalogue utilisateur |
| **DEFINE SPACE** | Allouer espace Data Space |
| **DEFINE CLUSTER** | Creer cluster ESDS/KSDS/RRDS/LDS |
| **DEFINE AIX** | Creer index alternatif |
| **DEFINE PATH** | Creer chemin vers AIX |
| **BLDINDEX** | Construire les entrees de l'AIX |
| **IF-THEN-ELSE** | Controle conditionnel |
| **SET** | Reinitialiser LASTCC/MAXCC |

---

## Aide-Memoire

```
Types de cluster:
- INDEXED = KSDS
- NONINDEXED = ESDS
- NUMBERED = RRDS
- LINEAR = LDS

Codes retour IDCAMS:
- 0 = OK
- 4 = Warning
- 8 = Erreur mineure
- 12 = Erreur logique
- 16 = Erreur grave

Sequence AIX:
1. DEFINE AIX (avec RELATE)
2. DEFINE PATH (avec PATHENTRY)
3. BLDINDEX (apres chargement donnees)
```

---
*Formation VSAM - M2i Formation*
