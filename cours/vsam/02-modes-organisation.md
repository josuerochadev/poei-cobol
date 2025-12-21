# Chapitre II - Les Modes d'Organisation VSAM

## II-1. Notion de Catalogue

### Role du Catalogue

Le catalogue VSAM est une structure essentielle qui :
- Gere les Data Sets dans un volume
- Localise les Data Sets
- Identifie les Data Sets par leurs noms

### Structure Hierarchique des Catalogues

```
+-------------------------------------------------------------+
|                  Structure des Catalogues                    |
+-------------------------------------------------------------+
|                                                              |
|                    +---------------+                         |
|                    |   MASTER      |                         |
|                    |  CATALOG      |                         |
|                    +-------+-------+                         |
|              +-------------+-------------+                   |
|              v             v             v                   |
|      +------------+ +------------+ +------------+            |
|      |   USER     | |   USER     | |   USER     |            |
|      | CATALOG 1  | | CATALOG 2  | | CATALOG 3  |            |
|      +------+-----+ +------+-----+ +------+-----+            |
|             |              |              |                  |
|             v              v              v                  |
|      +----------+    +----------+    +----------+            |
|      |Data Space|    |Data Space|    |Data Space|            |
|      |Clusters  |    |Clusters  |    |Clusters  |            |
|      |Non-VSAM  |    |Non-VSAM  |    |Non-VSAM  |            |
|      +----------+    +----------+    +----------+            |
+-------------------------------------------------------------+
```

---

## II-2. Master Catalog

### Caracteristiques

- **Un seul** Master Catalog par systeme z/OS
- Cree lors de la generation du systeme
- Reside sur le volume systeme
- Gere et surveille toutes les operations VSAM

### Fonctions du Master Catalog

1. Acces VSAM pour les Data Sets
2. Autorisation de mot de passe
3. Gestion de l'espace
4. Emplacement des Data Sets
5. Espace disponible

### Regles Importantes

| Regle | Description |
|-------|-------------|
| HLQ uniquement | Stocke uniquement les High Level Qualifiers |
| Pas de Data Sets | Ne pas cataloguer directement les VSAM/Non-VSAM |
| Systeme | Gere par les administrateurs systeme |

> **Note historique :** Le Master Catalog etait appele "VSAM King" dans les annees 1970.

---

## II-3. User Catalog

### Caracteristiques

- Meme structure et concept que le Master Catalog
- Niveau hierarchique inferieur
- Pas obligatoire mais **fortement recommande**
- Ameliore securite et organisation

### Regles des User Catalogs

- Proprietaire du volume sur lequel il reside
- Doit etre le premier objet VSAM stocke sur le volume
- Peut posseder plusieurs volumes
- Cree par les administrateurs systemes

### Exemple de Definition

```jcl
//DEFUCAT  JOB 'DEF USER CATALOG',CLASS=A,MSGCLASS=A
//IDCAMS   EXEC PGM=IDCAMS,REGION=4096K
//DISK01   DD UNIT=3380,VOL=SER=DISK01,DISP=OLD
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE USERCATALOG ( -
    NAME (UCDISK01) -
    VOLUME (DISK01) -
    TRACKS (7000 0) -
    FOR (9999) -
    DATA TRACKS (25 5) -
    INDEX TRACKS (15))
/*
```

### Comparaison Master vs User Catalog

| Aspect | Master Catalog | User Catalog |
|--------|----------------|--------------|
| **Nombre** | 1 par systeme | Multiples |
| **Creation** | Generation systeme | DEFINE USERCATALOG |
| **Contenu** | Alias, User Catalogs | Data Sets, Clusters |
| **Propriete** | Systeme | Volume specifique |
| **Obligatoire** | Oui | Non (recommande) |

---

## II-4. VSAM Data Space

### Definition

Un Data Space est une zone de stockage sur un volume DASD reservee pour les donnees VSAM.

### Caracteristiques

- Ne peut contenir que des donnees VSAM
- Relation etablie entre Data Space et catalogue lors de la definition VSAM
- Le catalogue est proprietaire de l'espace de donnees
- Un catalogue peut avoir plusieurs Data Spaces
- Option **CANDIDATE** : reserve le volume sans allocation reelle

### Syntaxe DEFINE SPACE

```jcl
DEFINE SPACE (
  FILE (ddname)
  VOLUME (volser)
  {CYLINDERS|RECORDS|TRACKS} (primary [secondary])
  [CANDIDATE]
) -
CATALOG (catname)
```

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

## II-5. SHAREOPTIONS - Partage des Data Sets

### Syntaxe

```
SHAREOPTIONS (cross-region cross-system)
```

### Valeurs et Significations

| Valeur | Cross Region | Cross System |
|--------|--------------|--------------|
| **1** | Lecture multiple OU ecriture unique | Non valide |
| **2** | Lecture multiple ET ecriture unique | Non valide |
| **3** | Lecture/ecriture multiple (pas d'integrite) | Meme que region |
| **4** | Comme 3 + rafraichissement buffer a chaque acces | Rafraichissement buffer |

### Valeur par Defaut

```
SHAREOPTIONS (1 3)
```

### Details des Options

#### SHAREOPTIONS(1,x) - Le plus restrictif

```
+-----------------------------------------------------------+
| Cross-Region = 1                                           |
+-----------------------------------------------------------+
| - Un seul programme peut ecrire a la fois                 |
| - Plusieurs programmes peuvent lire simultanement          |
| - Pas de lecture pendant l'ecriture                        |
| - Integrite des donnees garantie                           |
+-----------------------------------------------------------+
```

#### SHAREOPTIONS(2,x) - Lecture et ecriture separees

```
+-----------------------------------------------------------+
| Cross-Region = 2                                           |
+-----------------------------------------------------------+
| - Un seul programme peut ecrire                            |
| - Plusieurs programmes peuvent lire en meme temps          |
| - Lecture possible pendant l'ecriture                      |
| - Risque : donnees non commitees lues                      |
+-----------------------------------------------------------+
```

#### SHAREOPTIONS(3,x) - Aucune restriction

```
+-----------------------------------------------------------+
| Cross-Region = 3                                           |
+-----------------------------------------------------------+
| - Plusieurs programmes peuvent ecrire                      |
| - Plusieurs programmes peuvent lire                        |
| - Pas de controle d'integrite                              |
| - L'application doit gerer la synchronisation              |
+-----------------------------------------------------------+
```

#### SHAREOPTIONS(4,x) - Rafraichissement systematique

```
+-----------------------------------------------------------+
| Cross-Region = 4                                           |
+-----------------------------------------------------------+
| - Comme SHAREOPTIONS(3)                                    |
| - Buffer rafraichi a chaque acces                          |
| - Performance degradee                                      |
| - Meilleure coherence des donnees                          |
+-----------------------------------------------------------+
```

### Recommandations

| Situation | SHAREOPTIONS |
|-----------|--------------|
| Fichier en lecture seule | (1,3) |
| Fichier avec un seul ecrivain | (2,3) |
| Applications CICS | (2,3) |
| Applications batch exclusives | (1,3) |
| Tests/developpement | (3,3) |

---

## II-6. Connexion des Catalogues

### Alias de Catalogue

Un alias permet de lier un HLQ a un User Catalog specifique.

```jcl
//DEFALIAS EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE ALIAS ( -
    NAME (FTEST) -
    RELATE (UCDISK01)) -
  CATALOG (CATALOG.MASTER)
/*
```

**Resultat :** Tous les Data Sets commencant par `FTEST.` seront catalogues dans `UCDISK01`.

### Schema de Resolution

```
1. Application demande : FTEST.DATA.FILE
2. Systeme cherche alias FTEST dans Master Catalog
3. Trouve : FTEST -> UCDISK01
4. Systeme cherche FTEST.DATA.FILE dans UCDISK01
5. Trouve l'entree et localise le Data Set
```

---

## Resume du Chapitre

| Concept | Description |
|---------|-------------|
| **Master Catalog** | Catalogue principal unique par systeme |
| **User Catalog** | Catalogue secondaire pour organisation |
| **Data Space** | Zone de stockage VSAM sur volume |
| **SHAREOPTIONS(1,x)** | Lecture multiple OU ecriture unique |
| **SHAREOPTIONS(2,x)** | Lecture pendant ecriture autorisee |
| **SHAREOPTIONS(3,x)** | Aucune restriction (pas d'integrite) |
| **SHAREOPTIONS(4,x)** | Rafraichissement buffer systematique |
| **ALIAS** | Lien entre HLQ et User Catalog |

---

## Aide-Memoire

```
Catalogues:
- Master Catalog = 1 par systeme (systeme)
- User Catalog = N par systeme (utilisateur)
- ALIAS = lien HLQ -> User Catalog

SHAREOPTIONS par defaut: (1 3)
- 1 = Cross-region (meme systeme)
- 3 = Cross-system (entre systemes)

Valeurs courantes:
- (1,3) = Standard, securise
- (2,3) = CICS, lecture pendant ecriture
- (3,3) = Pas d'integrite, gestion applicative
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Introduction à VSAM](01-introduction-vsam.md) | [Chapitre III - Structure des Data Sets VSAM](03-structure-datasets.md) |

---
*Formation VSAM - M2i Formation*
