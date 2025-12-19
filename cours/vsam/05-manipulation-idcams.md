# Chapitre V - Commandes de Manipulation IDCAMS

## V-1. LISTCAT - Lister le Catalogue

### Syntaxe

```jcl
LISTCAT
  [ALIAS|ALTERNATEINDEX|CLUSTER|DATA|GENERATIONDATAGROUP|
   INDEX|NONVSAM|PAGESPACE|PATH|SPACE|USERCATALOG]
  [ENTRIES(entry-name...)|LEVEL(level)]
  [CREATION(days)]
  [EXPIRATION(days)]
  [NAME|HISTORY|VOLUME|ALLOCATION|ALL]
  [OUTFILE(ddname)]
  [CATALOG(catalog-name)]
```

### Niveaux de Detail

| Option | Information affichee |
|--------|---------------------|
| `NAME` | Nom uniquement |
| `HISTORY` | Nom, type, proprietaire, dates creation/expiration |
| `VOLUME` | HISTORY + volumes et types de peripheriques |
| `ALLOCATION` | VOLUME + details allocation espace |
| `ALL` | Toutes les informations |

### Filtres

| Parametre | Description |
|-----------|-------------|
| ENTRIES | Liste d'entrees specifiques |
| LEVEL | Prefixe de nom (ex: FTEST) |
| CREATION | Fichiers crees dans les N derniers jours |
| EXPIRATION | Fichiers expirant dans les N prochains jours |

### Exemples

#### Lister par niveau

```jcl
//STEPLIST EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT -
    LEVEL(FTEST) -
    ALL
/*
```

#### Lister une entree specifique

```jcl
//STEPLIST EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT -
    ENTRIES(FTEST.KSDS) -
    ALL
/*
```

#### Lister uniquement les clusters

```jcl
//STEPLIST EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT -
    CLUSTER -
    LEVEL(FTEST) -
    VOLUME
/*
```

---

## V-2. REPRO - Copier les Donnees

### Syntaxe

```jcl
REPRO
  {INFILE(ddname)|INDATASET(dataset-name)}
  {OUTFILE(ddname)|OUTDATASET(dataset-name)}
  [FILE(ddname)]
  [FROMKEY(key)|FROMADDRESS(address)|FROMNUMBER(rrn)|SKIP(number)]
  [TOKEY(key)|TOADDRESS(address)|TONUMBER(rrn)|COUNT(number)]
  [REPLACE|NOREPLACE]
  [REUSE|NOREUSE]
```

### Fonctions REPRO

- Copier donnees entre Data Sets
- Charger clusters VSAM vides
- Creer sauvegardes
- Convertir formats (VSAM <-> sequentiel)

### Parametres de Selection

| Source | Parametre FROM | Parametre TO |
|--------|----------------|--------------|
| KSDS | FROMKEY | TOKEY |
| ESDS | FROMADDRESS | TOADDRESS |
| RRDS | FROMNUMBER | TONUMBER |
| Tout | SKIP | COUNT |

### Exemples

#### Charger un KSDS depuis des donnees in-stream

```jcl
//STEPREPR EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ENTDATA  DD *
001BBBBBBBBBB DEB COMMERCANT
002NNNNNNNNN DEB FONCTIONNAIRE
003FFFFFFFFFFCRE ENSEIGNANT
004JJJJJJJJJJ DEB CHEFENTREP
005AAAAAAAAAA CRE COMMERCANT
/*
//SYSIN    DD *
  REPRO INFILE(ENTDATA) -
    OUTDATASET(FTEST.KSDS)
/*
```

#### Copier VSAM vers sequentiel

```jcl
//STEPBACK EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//BACKUP   DD DSN=FTEST.BACKUP,DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,5)),DCB=(LRECL=100,RECFM=FB)
//SYSIN    DD *
  REPRO INDATASET(FTEST.KSDS) -
    OUTFILE(BACKUP)
/*
```

#### Copier une plage de cles

```jcl
//STEPSEL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO INDATASET(FTEST.KSDS) -
    OUTDATASET(FTEST.KSDS.SUBSET) -
    FROMKEY(100) -
    TOKEY(500)
/*
```

#### Copier avec SKIP et COUNT

```jcl
//STEPSKIP EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  REPRO INDATASET(FTEST.ESDS) -
    OUTDATASET(FTEST.ESDS.PART) -
    SKIP(100) -
    COUNT(50)
/*
```

---

## V-3. ALTER - Modifier les Attributs

### Syntaxe

```jcl
ALTER vsam-file-name
  [ADDVOLUMES(volser...)]
  [REMOVEVOLUMES(volser...)]
  [NEWNAME(new-name)]
  [BUFFERSPACE(size)]
  [ERASE|NOERASE]
  [EMPTY|NOEMPTY]
  [FREESPACE(CI-percent CA-percent)]
  [INHIBIT|UNINHIBIT]
  [KEYS(length offset)]
  [NULLIFY(parameters)]
  [REUSE|NOREUSE]
  [RECORDSIZE(average maximum)]
  [PURGE|NOPURGE]
  [TO(date)|FOR(days)]
  [CATALOG(catalog-name)]
```

### Parametres Frequents

| Parametre | Fonction |
|-----------|----------|
| `NEWNAME` | Renomme le Data Set |
| `ADDVOLUMES` | Ajoute volumes supplementaires |
| `REMOVEVOLUMES` | Supprime volumes |
| `INHIBIT` | Acces lecture seule |
| `UNINHIBIT` | Supprime restriction lecture seule |
| `FREESPACE` | Modifie l'espace libre CI/CA |
| `SHAREOPTIONS` | Modifie les options de partage |

### Exemples

#### Renommer un cluster

```jcl
//STEPALTE EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ALTER FTEST.ESDS -
    NEWNAME(FILE.TEST.SEQ)
  ALTER FTEST.ESDS.* -
    NEWNAME(FILE.TEST.SEQ.*)
  IF LASTCC = 0 THEN -
    LISTCAT LEVEL(FILE.TEST.SEQ) NAME
/*
```

#### Mettre en lecture seule

```jcl
//STEPINHI EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ALTER FTEST.KSDS -
    INHIBIT
/*
```

#### Modifier le FREESPACE

```jcl
//STEPFS   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ALTER FTEST.KSDS -
    FREESPACE(20 15)
/*
```

---

## V-4. DELETE - Supprimer

### Syntaxe

```jcl
DELETE (entryname...)
  [ALIAS|ALTERNATEINDEX|CLUSTER|GENERATIONDATAGROUP|
   LIBRARYENTRY|NONVSAM|PAGESPACE|USERCATALOG|VOLUMEENTRY]
  [ERASE|NOERASE]
  [FILE(ddname)]
  [FORCE|NOFORCE]
  [MASK|NOMASK]
  [PURGE|NOPURGE]
  [RECOVERY|NORECOVERY]
  [SCRATCH|NOSCRATCH]
  [CATALOG(catalog-name)]
```

### Options Importantes

| Option | Description |
|--------|-------------|
| `ERASE` | Remplace par zeros binaires avant suppression |
| `FORCE` | Force suppression meme si non vide (GDG, USERCATALOG) |
| `MASK` | Entryname est un nom generique (ex: FTEST.*) |
| `PURGE` | Supprime meme si periode de retention non expiree |
| `SCRATCH` | Supprime aussi l'entree VTOC |

### Exemples

#### Supprimer un cluster

```jcl
//STEPDEL  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.KSDS) CLUSTER PURGE
  IF LASTCC <= 8 THEN -
    SET MAXCC = 0
/*
```

#### Supprimer avec ERASE (donnees sensibles)

```jcl
//STEPDELE EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.CONFIDENTIAL) -
    CLUSTER -
    ERASE -
    PURGE
/*
```

#### Supprimer par masque

```jcl
//STEPMASK EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (FTEST.TEMP.*) -
    MASK -
    PURGE
/*
```

---

## V-5. VERIFY - Verifier et Reparer

### Syntaxe

```jcl
VERIFY {FILE(ddname)|DATASET(vsam-file-name)}
```

### Usage

VERIFY est utilise pour :
- Verifier et reparer fichiers VSAM mal fermes apres erreur
- Identifier la fin du Data Set
- Reinitialiser l'entree de catalogue
- Ajouter enregistrements de fin de donnees corrects

### Quand Utiliser VERIFY

- Apres un ABEND pendant l'ecriture
- Quand OPEN echoue avec erreur "not properly closed"
- En debut de job pour s'assurer de l'integrite

### Exemple

```jcl
//STEPVER  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  VERIFY DATASET(FTEST.KSDS)
  IF LASTCC <= 4 THEN -
    SET MAXCC = 0
/*
```

### VERIFY en Debut de Job

```jcl
//STEPVER  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  VERIFY DATASET(FTEST.KSDS)
  VERIFY DATASET(FTEST.ESDS)
/*
//STEP1    EXEC PGM=MYPROG
//KSDS     DD DSN=FTEST.KSDS,DISP=SHR
//ESDS     DD DSN=FTEST.ESDS,DISP=SHR
```

---

## V-6. PRINT - Imprimer le Contenu

### Syntaxe

```jcl
PRINT
  {INFILE(ddname)|INDATASET(dataset-name)}
  [CHARACTER|DUMP|HEX]
  [FROMKEY(key)|FROMADDRESS(address)|FROMNUMBER(rrn)|SKIP(number)]
  [TOKEY(key)|TOADDRESS(address)|TONUMBER(rrn)|COUNT(number)]
  [OUTFILE(ddname)]
```

### Formats de Sortie

| Format | Description |
|--------|-------------|
| CHARACTER | Format texte lisible |
| HEX | Format hexadecimal |
| DUMP | Format dump (hex + caracteres) |

### Exemples

#### Imprimer en format caractere

```jcl
//STEPPRT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT -
    INDATASET(FTEST.KSDS) -
    CHARACTER
/*
```

#### Imprimer en format hexadecimal

```jcl
//STEPPRT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT -
    INDATASET(FTEST.KSDS) -
    HEX -
    FROMKEY(100) -
    TOKEY(200)
/*
```

#### Imprimer une plage avec COUNT

```jcl
//STEPPRT  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  PRINT -
    INDATASET(FTEST.ESDS) -
    CHARACTER -
    SKIP(10) -
    COUNT(5)
/*
```

---

## V-7. EXPORT - Sauvegarder

### Syntaxe

```jcl
EXPORT entryname
  {OUTFILE(ddname)|OUTDATASET(entryname)}
  [ERASE|NOERASE]
  [INFILE(ddname)]
  [INHIBITSOURCE|NOINHIBITSOURCE]
  [INHIBITTARGET|NOINHIBITTARGET]
  [PURGE|NOPURGE]
  [TEMPORARY|PERMANENT]
```

### Difference avec REPRO

| Aspect | REPRO | EXPORT |
|--------|-------|--------|
| Contenu | Donnees uniquement | Donnees + metadata catalogue |
| Usage | Copie simple | Sauvegarde complete |
| Restauration | REPRO | IMPORT |
| Format | Fichier standard | Format portable VSAM |

### Options

| Option | Description |
|--------|-------------|
| `TEMPORARY` | Conserve l'original apres export |
| `PERMANENT` | Supprime l'original apres export reussi |
| `INHIBITSOURCE` | Marque source en lecture seule apres export |
| `INHIBITTARGET` | Marque copies creees depuis export en lecture seule |

### Exemple

```jcl
//STEPEXP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//BACKUP   DD DSN=FTEST.EXPORT.BACKUP,DISP=(NEW,CATLG),
//            SPACE=(CYL,(5,5)),DCB=(RECFM=VB,LRECL=32760)
//SYSIN    DD *
  EXPORT FTEST.KSDS -
    OUTFILE(BACKUP) -
    TEMPORARY
/*
```

---

## V-8. IMPORT - Restaurer

### Syntaxe

```jcl
IMPORT
  {INFILE(ddname)|INDATASET(entryname)}
  {OUTFILE(ddname)|OUTDATASET(entryname)}
  [ERASE|NOERASE]
  [INTOEMPTY]
  [LOCK|UNLOCK]
  [OBJECTS((entryname [FILE(ddname)] [NEWNAME(newname)]
           [VOLUMES(volser...)]))]
  [PURGE|NOPURGE]
  [CATALOG(catname)]
```

### Comportement

1. Recherche entree de meme nom dans catalogue cible
2. Si inexistante : cree nouvelle entree depuis copie portable
3. Si existante et marquee "exportee" : remplace
4. Si existante non marquee : echec

### Exemple

```jcl
//STEPIMP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//BACKUP   DD DSN=FTEST.EXPORT.BACKUP,DISP=SHR
//SYSIN    DD *
  IMPORT -
    INFILE(BACKUP) -
    OUTDATASET(FTEST.KSDS.RESTORED) -
    OBJECTS((FTEST.KSDS -
      NEWNAME(FTEST.KSDS.RESTORED) -
      VOLUMES(ZASYS1)))
/*
```

---

## Resume du Chapitre

| Commande | Usage |
|----------|-------|
| **LISTCAT** | Lister le contenu du catalogue |
| **REPRO** | Copier donnees entre Data Sets |
| **ALTER** | Modifier attributs d'un Data Set |
| **DELETE** | Supprimer un Data Set |
| **VERIFY** | Verifier/reparer fichier mal ferme |
| **PRINT** | Imprimer le contenu |
| **EXPORT** | Sauvegarder avec metadata |
| **IMPORT** | Restaurer depuis EXPORT |

---

## Aide-Memoire

```
LISTCAT niveaux:
- NAME = nom seulement
- HISTORY = + dates, proprietaire
- VOLUME = + volumes
- ALLOCATION = + espace
- ALL = tout

REPRO selections:
- KSDS: FROMKEY/TOKEY
- ESDS: FROMADDRESS/TOADDRESS
- RRDS: FROMNUMBER/TONUMBER
- Tous: SKIP/COUNT

DELETE options:
- PURGE = ignorer retention
- ERASE = ecraser avant suppression
- MASK = nom generique (*.*)
- FORCE = forcer meme si non vide

PRINT formats:
- CHARACTER = texte
- HEX = hexadecimal
- DUMP = hex + char
```

---
*Formation VSAM - M2i Formation*
