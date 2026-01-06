# Chapitre I - Introduction a VSAM

## I-1. Présentation Générale

### Qu'est-ce que VSAM ?

**VSAM = Virtual Storage Accèss Method**

VSAM est une méthode d'accès au stockage introduite par IBM en 1970 pour les systèmes MVS, OS/390 et z/OS. C'est la méthode d'accès la plus utilisée pour les données applicatives sur mainframe.

```
+------------------------------------------------------------------+
|                    VSAM - Caractéristiques                        |
+------------------------------------------------------------------+
|  - Introduit en 1970 sur les systèmes IBM                        |
|  - Méthode d'accès au stockage pour MVS, OS/390, z/OS            |
|  - Stocke les données dans des fichiers regroupes en Catalogues  |
|  - Format propre non comprehensible par d'autres méthodes        |
|  - Valable UNIQUEMENT sur disques DASD (pas sur bandes)          |
|  - Accès séquentiel ou aléatoire (par clé)                       |
|  - Sécurise les données contre accès non autorises               |
+------------------------------------------------------------------+
```

### Points Clés

- Les données sont structurées en **enregistrements** stockes dans des **Data Sets**
- Impossible de modifiér ou acceder aux données directement par un editeur ISPF
- Données applicatives uniquement (pas de programmes sources, JCL ou executables)
- Optimisation des performances via CI, CA, Freespace, Shareoptions

---

## I-2. Types d'Organisation VSAM

VSAM propose quatre types d'organisation de fichiers :

| Type | Nom Complet | Description |
|------|-------------|-------------|
| **ESDS** | Entry Sequenced Data Set | Organisation séquentielle |
| **KSDS** | Key Sequenced Data Set | Organisation indexée par clé |
| **RRDS** | Relative Record Data Set | Organisation relative par numéro |
| **LDS** | Linear Data Set | Données non structurées (multiples de 4K) |

### Résumé des Types

```
+-------------+--------------------------------------------------+
| ESDS        | Enregistrements ajoutes séquentiellement         |
|             | Accès par RBA (Relative Byte Address)            |
+-------------+--------------------------------------------------+
| KSDS        | Enregistrements indexes par une clé unique       |
|             | Type le plus utilise                             |
+-------------+--------------------------------------------------+
| RRDS        | Enregistrements identifiés par numéro (RRN)      |
|             | Slots de taille fixe                             |
+-------------+--------------------------------------------------+
| LDS         | Données brutes sans structure d'enregistrement   |
|             | Utilise par DB2, CICS                            |
+-------------+--------------------------------------------------+
```

---

## I-3. Notion de VTOC

### VTOC = Volume Table Of Contents

La VTOC est la table des matieres d'un volume DASD. Elle contient les informations sur l'espace alloué et disponible.

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

**Caractéristiques :**
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

**Points de référence :**
- 1 Track 3390 ≈ 56 664 octets (~56 Ko)
- 1 Cylindre 3390 = 15 Tracks ≈ 849 Ko

---

## I-5. Méthodes d'Accès aux Data Sets

| Méthode | Description | Usage |
|---------|-------------|-------|
| **BSAM** | Basic Sequential Accèss Method | Blocs physiques séquentiels |
| **QSAM** | Queued Sequential Accèss Method | Enregistrements logiques, anticipation lecture |
| **BDAM** | Basic Direct Accèss Method | Accès aléatoire par adresse (obsolete) |
| **BPAM** | Basic Partitioned Accèss Method | Accès aux PDS |
| **VSAM** | Virtual Storage Accèss Method | Accès séquentiel, indexe ou direct |

### Comparaison VSAM vs Non-VSAM

| Aspect | VSAM | Non-VSAM (QSAM/BSAM) |
|--------|------|----------------------|
| Structure | CI, CA | Blocks |
| Index | Integre (KSDS) | Separe ou absent |
| Accès | Séquentiel, direct, dynamique | Séquentiel principalement |
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

### Caractéristiques du PDS

- Stocke uniquement sur DASD
- Membres identifiés par nom unique (1-8 caracteres)
- Enregistrements ecrits séquentiellement dans chaque membre
- Maximum 65535 pistes, ne peut pas s'etendre au-dela d'un volume
- Suppression d'un membre ne libere pas l'espace (nécessite COMPRESS)

### PDS vs PDSE

| Caracteristique | PDS | PDSE |
|-----------------|-----|------|
| **Directory** | Taille fixe | Extensible |
| **Fragmentation** | Oui (compression manuelle) | Non (reutilisation auto) |
| **Compression** | IEBCOPY nécessaire | Pas nécessaire |
| **DSNTYPE** | Non specifie | LIBRARY |
| **Accès concurrent** | Limite | Ameliore |

---

## Résumé du Chapitre

| Concept | Description |
|---------|-------------|
| **VSAM** | Virtual Storage Accèss Method |
| **ESDS** | Entry Sequenced - accès par RBA |
| **KSDS** | Key Sequenced - accès par clé |
| **RRDS** | Relative Record - accès par RRN |
| **LDS** | Linear - données brutes 4K |
| **VTOC** | Table des matieres du volume |
| **DASD** | Direct Accèss Storage Device |
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
- ESDS = Séquentiel (RBA)
- KSDS = Indexe (Clé) - le plus utilise
- RRDS = Relatif (RRN)
- LDS = Lineaire (4K blocks)
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Module VSAM](README.md) | [Chapitre II - Les Modes d'Organisation VSAM](02-modes-organisation.md) |

---
*Formation VSAM - M2i Formation*
