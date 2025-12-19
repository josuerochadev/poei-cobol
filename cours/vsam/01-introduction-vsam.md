# Chapitre I - Introduction a VSAM

## I-1. Presentation Generale

### Qu'est-ce que VSAM ?

**VSAM = Virtual Storage Access Method**

VSAM est une methode d'acces au stockage introduite par IBM en 1970 pour les systemes MVS, OS/390 et z/OS. C'est la methode d'acces la plus utilisee pour les donnees applicatives sur mainframe.

```
+------------------------------------------------------------------+
|                    VSAM - Caracteristiques                        |
+------------------------------------------------------------------+
|  - Introduit en 1970 sur les systemes IBM                        |
|  - Methode d'acces au stockage pour MVS, OS/390, z/OS            |
|  - Stocke les donnees dans des fichiers regroupes en Catalogues  |
|  - Format propre non comprehensible par d'autres methodes        |
|  - Valable UNIQUEMENT sur disques DASD (pas sur bandes)          |
|  - Acces sequentiel ou aleatoire (par cle)                       |
|  - Securise les donnees contre acces non autorises               |
+------------------------------------------------------------------+
```

### Points Cles

- Les donnees sont structurees en **enregistrements** stockes dans des **Data Sets**
- Impossible de modifier ou acceder aux donnees directement par un editeur ISPF
- Donnees applicatives uniquement (pas de programmes sources, JCL ou executables)
- Optimisation des performances via CI, CA, Freespace, Shareoptions

---

## I-2. Types d'Organisation VSAM

VSAM propose quatre types d'organisation de fichiers :

| Type | Nom Complet | Description |
|------|-------------|-------------|
| **ESDS** | Entry Sequenced Data Set | Organisation sequentielle |
| **KSDS** | Key Sequenced Data Set | Organisation indexee par cle |
| **RRDS** | Relative Record Data Set | Organisation relative par numero |
| **LDS** | Linear Data Set | Donnees non structurees (multiples de 4K) |

### Resume des Types

```
+-------------+--------------------------------------------------+
| ESDS        | Enregistrements ajoutes sequentiellement         |
|             | Acces par RBA (Relative Byte Address)            |
+-------------+--------------------------------------------------+
| KSDS        | Enregistrements indexes par une cle unique       |
|             | Type le plus utilise                             |
+-------------+--------------------------------------------------+
| RRDS        | Enregistrements identifies par numero (RRN)      |
|             | Slots de taille fixe                             |
+-------------+--------------------------------------------------+
| LDS         | Donnees brutes sans structure d'enregistrement   |
|             | Utilise par DB2, CICS                            |
+-------------+--------------------------------------------------+
```

---

## I-3. Notion de VTOC

### VTOC = Volume Table Of Contents

La VTOC est la table des matieres d'un volume DASD. Elle contient les informations sur l'espace alloue et disponible.

```
+--------------------------------------------------------------+
|  DASD Volume                                                 |
+--------+------------+------------+---------------------------+
| Label  |   Data 1   |   Data 2   |    Espace Libre           |
| VOLSER |   Tracks   |   Tracks   |       Tracks              |
+--------+------------+------------+---------------------------+
|                         VTOC                                 |
|        (Table des matieres - Extents et allocations)         |
+--------------------------------------------------------------+
```

**Caracteristiques :**
- Premier enregistrement du volume contient le **VOLSER** (6 caracteres)
- Creee lors de l'initialisation du DASD par l'utilitaire **ICKDSF**
- Contient la repartition de l'espace occupe/libre

---

## I-4. Capacites des Disques IBM

| Device | Cylindres/Volume | Tracks/Cylindre | Bytes/Track | Capacite Totale |
|--------|------------------|-----------------|-------------|-----------------|
| 3380 ADJ | 885 | 15 | 23552-45056 | 570 Mo |
| 3380 K | 2655 | 15 | 23552-45056 | 1.7 Go |
| 3390-1 | 1113 | 15 | 25088-55296 | 846 Mo |
| 3390-2 | 2226 | 15 | 25088-55296 | 1.7 Go |
| 3390-3 | 3339 | 15 | 25088-55296 | 2.5 Go |
| 3390-3 (extended) | 10017 | 15 | 56664 | 8.5 Go |

**Points de reference :**
- 1 Track 3390 ≈ 56 664 octets (~56 Ko)
- 1 Cylindre 3390 = 15 Tracks ≈ 849 Ko

---

## I-5. Methodes d'Acces aux Data Sets

| Methode | Description | Usage |
|---------|-------------|-------|
| **BSAM** | Basic Sequential Access Method | Blocs physiques sequentiels |
| **QSAM** | Queued Sequential Access Method | Enregistrements logiques, anticipation lecture |
| **BDAM** | Basic Direct Access Method | Acces aleatoire par adresse (obsolete) |
| **BPAM** | Basic Partitioned Access Method | Acces aux PDS |
| **VSAM** | Virtual Storage Access Method | Acces sequentiel, indexe ou direct |

### Comparaison VSAM vs Non-VSAM

| Aspect | VSAM | Non-VSAM (QSAM/BSAM) |
|--------|------|----------------------|
| Structure | CI, CA | Blocks |
| Index | Integre (KSDS) | Separe ou absent |
| Acces | Sequentiel, direct, dynamique | Sequentiel principalement |
| Catalogue | Obligatoire | Optionnel |
| Gestion espace | Automatique | Manuelle |

---

## I-6. Structure Physique et Logique

### Enregistrement Physique vs Logique

```
+-------------------------------------------------------------+
|  Relations possibles:                                        |
|                                                              |
|  1 Enreg. Physique = 1 Enreg. Logique                       |
|  1 Enreg. Physique = N Enreg. Logiques (blocked)            |
|  N Enreg. Physiques = 1 Enreg. Logique (spanned)            |
+-------------------------------------------------------------+
```

### Formats d'Enregistrement

| Format | Description | Relation Block/Record |
|--------|-------------|----------------------|
| **F** | Fixed | 1 record fixe par block |
| **FB** | Fixed Blocked | N records fixes par block |
| **V** | Variable | 1 record variable par block (avec RDW 4 octets) |
| **VB** | Variable Blocked | N records variables par block (avec BDW + RDW) |
| **U** | Undefined | Format gere par l'application |

### Schema des Formats

```
Fixed Block (FB):
+--------+--------+--------+--------+
| Record | Record | Record | Record |  <- Block
+--------+--------+--------+--------+

Variable Block (VB):
+-----+-------------+-----+-------------+-----+-------------+
| BDW | RDW|Record | RDW |   Record    | RDW |   Record    |
+-----+-------------+-----+-------------+-----+-------------+
  4B    4B              4B                 4B

BDW = Block Descriptor Word (longueur du bloc)
RDW = Record Descriptor Word (longueur de l'enregistrement)
```

---

## I-7. PDS - Partitioned Data Set

### Structure d'un PDS

```
+-------------------------------------------------------------+
|                    Structure PDS                             |
+-------------------------------------------------------------+
|  Directory (256 enregistrements max)                         |
|  +---------+---------+---------+---------+---------+        |
|  | Entry A | Entry B | Entry C | Entry M |  ...    |        |
|  +----+----+----+----+----+----+----+----+---------+        |
|       |         |         |         |                        |
|       v         v         v         v                        |
|  +---------+---------+---------+---------+---------+        |
|  |Member A |Member B |Member C |  Free   |Member M |        |
|  +---------+---------+---------+---------+---------+        |
|                     Data Area                                |
+-------------------------------------------------------------+
```

### Caracteristiques du PDS

- Stocke uniquement sur DASD
- Membres identifies par nom unique (1-8 caracteres)
- Enregistrements ecrits sequentiellement dans chaque membre
- Maximum 65535 pistes, ne peut pas s'etendre au-dela d'un volume
- Suppression d'un membre ne libere pas l'espace (necessite COMPRESS)

### PDS vs PDSE

| Caracteristique | PDS | PDSE |
|-----------------|-----|------|
| **Directory** | Taille fixe | Extensible |
| **Fragmentation** | Oui (compression manuelle) | Non (reutilisation auto) |
| **Compression** | IEBCOPY necessaire | Pas necessaire |
| **DSNTYPE** | Non specifie | LIBRARY |
| **Acces concurrent** | Limite | Ameliore |

---

## Resume du Chapitre

| Concept | Description |
|---------|-------------|
| **VSAM** | Virtual Storage Access Method |
| **ESDS** | Entry Sequenced - acces par RBA |
| **KSDS** | Key Sequenced - acces par cle |
| **RRDS** | Relative Record - acces par RRN |
| **LDS** | Linear - donnees brutes 4K |
| **VTOC** | Table des matieres du volume |
| **DASD** | Direct Access Storage Device |
| **PDS/PDSE** | Partitioned Data Set |

---

## Aide-Memoire

```
Disque 3390:
- 1 Track ≈ 56 Ko
- 1 Cylindre = 15 Tracks ≈ 849 Ko

Formats:
- FB = Fixed Blocked (le plus courant)
- VB = Variable Blocked

Types VSAM:
- ESDS = Sequentiel (RBA)
- KSDS = Indexe (Cle) - le plus utilise
- RRDS = Relatif (RRN)
- LDS = Lineaire (4K blocks)
```

---
*Formation VSAM - M2i Formation*
