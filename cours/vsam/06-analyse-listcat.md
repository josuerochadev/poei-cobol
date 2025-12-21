# Chapitre VI - Analyse de la Sortie LISTCAT

## VI-1. Structure de la Sortie LISTCAT

### Vue d'Ensemble

La sortie LISTCAT ALL fournit des informations detaillees sur les objets VSAM. Elle se decompose en plusieurs sections selon le type d'objet.

### Exemple de Sortie LISTCAT ALL pour un KSDS

```
CLUSTER ------- FTEST.KSDS
     IN-CAT --- CATALOG.MASTER
     HISTORY
         DATASET-OWNER-----(null)
         CREATION--------2025.320
         EXPIRATION------0000.000
     PROTECTION
         RACF------------(null)
     ASSOCIATIONS
         DATA-----------FTEST.KSDS.DATA
         INDEX----------FTEST.KSDS.INDEX

DATA ------- FTEST.KSDS.DATA
     IN-CAT --- CATALOG.MASTER
     ATTRIBUTES
         KEYLEN---------15        AVGLRECL-------100
         MAXLRECL-------100       RKP------------0
         CI/CA----------xx        CISIZE---------4096
         FREESPACE-CI---0         FREESPACE-CA---0
         SHROPT(R,S)----1,3       RECOVERY
         INDEXED        NOERASE   NOSPANNED
         REUSE          NOWRITECHECK
     STATISTICS
         REC-TOTAL------0         SPLITS-CI------0
         REC-DELETED----0         SPLITS-CA------0
         REC-INSERTED---0         FREESPACE-CI---100%
         REC-UPDATED----0         FREESPACE-CA---100%
     ALLOCATION
         SPACE-TYPE-----TRACK     SPACE-PRI------1
         SPACE-SEC------1         HI-ALLOC-RBA---0
         HI-USED-RBA----0
     VOLUME
         VOLSER---------ZASYS1    DEVTYPE--------3390

INDEX ------- FTEST.KSDS.INDEX
     IN-CAT --- CATALOG.MASTER
     (similar structure)
```

---

## VI-2. Sections de la Sortie

### Section CLUSTER

```
+----------------------------------------------------------+
| CLUSTER ------- FTEST.KSDS                                |
+----------------------------------------------------------+
| Informations sur le cluster dans son ensemble             |
| - IN-CAT: Catalogue contenant l'entree                    |
| - HISTORY: Dates et proprietaire                          |
| - PROTECTION: Securite RACF                               |
| - ASSOCIATIONS: Composants DATA et INDEX                  |
+----------------------------------------------------------+
```

### Section DATA

```
+----------------------------------------------------------+
| DATA ------- FTEST.KSDS.DATA                              |
+----------------------------------------------------------+
| Informations sur le composant DATA                        |
| - ATTRIBUTES: Caracteristiques du fichier                 |
| - STATISTICS: Statistiques d'utilisation                  |
| - ALLOCATION: Espace alloue                               |
| - VOLUME: Volumes physiques                               |
+----------------------------------------------------------+
```

### Section INDEX (KSDS uniquement)

```
+----------------------------------------------------------+
| INDEX ------- FTEST.KSDS.INDEX                            |
+----------------------------------------------------------+
| Informations sur le composant INDEX                       |
| - ATTRIBUTES: Caracteristiques de l'index                 |
| - STATISTICS: Statistiques d'utilisation                  |
| - ALLOCATION: Espace alloue                               |
| - VOLUME: Volumes physiques                               |
+----------------------------------------------------------+
```

---

## VI-3. Champs ATTRIBUTES

### Champs de Definition

| Champ | Description | Exemple |
|-------|-------------|---------|
| **KEYLEN** | Longueur de la cle | 15 |
| **RKP** | Relative Key Position (offset) | 0 |
| **AVGLRECL** | Longueur moyenne enregistrement | 100 |
| **MAXLRECL** | Longueur maximale enregistrement | 100 |
| **CISIZE** | Taille du Control Interval | 4096 |
| **CI/CA** | Nombre de CI par CA | 15 |

### Champs d'Options

| Champ | Description | Valeurs |
|-------|-------------|---------|
| **FREESPACE-CI** | % espace libre par CI | 0-100 |
| **FREESPACE-CA** | % espace libre par CA | 0-100 |
| **SHROPT(R,S)** | Share Options (Region, System) | 1-4, 1-4 |

### Attributs Booleens

| Attribut | Description |
|----------|-------------|
| INDEXED / NONINDEXED / NUMBERED | Type de cluster |
| SPANNED / NOSPANNED | Enregistrements fractionnes |
| REUSE / NOREUSE | Reutilisation a l'ouverture |
| ERASE / NOERASE | Effacement a la suppression |
| RECOVERY / SPEED | Mode de chargement |
| WRITECHECK / NOWRITECHECK | Verification ecriture |

---

## VI-4. Champs STATISTICS

### Statistiques d'Enregistrements

| Champ | Description |
|-------|-------------|
| **REC-TOTAL** | Nombre total d'enregistrements |
| **REC-DELETED** | Nombre d'enregistrements supprimes |
| **REC-INSERTED** | Nombre d'enregistrements inseres |
| **REC-UPDATED** | Nombre d'enregistrements mis a jour |
| **REC-RETRIEVED** | Nombre d'enregistrements lus |

### Statistiques de Splits

| Champ | Description | Impact |
|-------|-------------|--------|
| **SPLITS-CI** | Nombre de CI splits | Performance |
| **SPLITS-CA** | Nombre de CA splits | Performance |

> **Note :** Un nombre eleve de splits indique un besoin de reorganisation ou d'augmentation du FREESPACE.

### Statistiques d'Espace Libre

| Champ | Description |
|-------|-------------|
| **FREESPACE-CI** | % d'espace libre dans les CI |
| **FREESPACE-CA** | % d'espace libre dans les CA |

---

## VI-5. Champs ALLOCATION

### Espace Alloue

| Champ | Description |
|-------|-------------|
| **SPACE-TYPE** | Type d'allocation (TRACK/CYLINDER/RECORD) |
| **SPACE-PRI** | Allocation primaire |
| **SPACE-SEC** | Allocation secondaire |

### Utilisation de l'Espace

| Champ | Description |
|-------|-------------|
| **HI-ALLOC-RBA** | Plus haute RBA allouee |
| **HI-USED-RBA** | Plus haute RBA utilisee |

### Calcul du Taux d'Utilisation

```
Taux utilisation (%) = (HI-USED-RBA / HI-ALLOC-RBA) x 100
```

**Exemple :**
- HI-ALLOC-RBA = 1000000
- HI-USED-RBA = 750000
- Taux = 75%

---

## VI-6. Champs VOLUME

| Champ | Description | Exemple |
|-------|-------------|---------|
| **VOLSER** | Numero de serie du volume | ZASYS1 |
| **DEVTYPE** | Type de peripherique | 3390 |
| **EXTENTS** | Nombre d'extensions | 1-123 |

---

## VI-7. Interpretation des Resultats

### Indicateurs de Sante

```
+----------------------------------------------------------+
| BONNE SANTE:                                              |
| - SPLITS-CI = 0 ou tres faible                            |
| - SPLITS-CA = 0                                           |
| - FREESPACE-CI > 10%                                      |
| - FREESPACE-CA > 10%                                      |
| - EXTENTS < 5                                             |
+----------------------------------------------------------+
| PROBLEMES POTENTIELS:                                     |
| - SPLITS-CI eleve (> 100)                                 |
| - SPLITS-CA > 0                                           |
| - FREESPACE proche de 0%                                  |
| - EXTENTS > 10                                            |
| - HI-USED-RBA proche de HI-ALLOC-RBA                      |
+----------------------------------------------------------+
```

### Actions Correctives

| Probleme | Solution |
|----------|----------|
| Beaucoup de CI splits | Augmenter FREESPACE-CI |
| CA splits | Augmenter FREESPACE-CA |
| Trop d'extents | Reorganiser (REPRO) |
| Espace sature | Augmenter allocation secondaire |

---

## VI-8. Exemple d'Analyse Complete

### Sortie LISTCAT

```
DATA ------- PROD.CLIENTS.KSDS.DATA
     ATTRIBUTES
         KEYLEN---------10        AVGLRECL-------200
         MAXLRECL-------200       RKP------------0
         CI/CA----------15        CISIZE---------4096
         FREESPACE-CI---5         FREESPACE-CA---5
         SHROPT(R,S)----2,3       RECOVERY
         INDEXED        NOERASE   NOSPANNED
     STATISTICS
         REC-TOTAL------125000    SPLITS-CI------1523
         REC-DELETED----2340      SPLITS-CA------45
         REC-INSERTED---15600     FREESPACE-CI---2%
         REC-UPDATED----89000     FREESPACE-CA---3%
     ALLOCATION
         SPACE-TYPE-----CYL       SPACE-PRI------10
         SPACE-SEC------5         HI-ALLOC-RBA---8500000
         HI-USED-RBA----8200000   EXTENTS--------12
```

### Analyse

```
+----------------------------------------------------------+
| DIAGNOSTIC:                                               |
+----------------------------------------------------------+
| 1. SPLITS-CI = 1523 (ELEVE)                               |
|    -> Le fichier a subi beaucoup de splits de CI          |
|    -> Indique des insertions frequentes                   |
|                                                           |
| 2. SPLITS-CA = 45 (PROBLEMATIQUE)                         |
|    -> Des Control Areas ont du etre scindees              |
|    -> Impact significatif sur les performances            |
|                                                           |
| 3. FREESPACE-CI = 2% (INSUFFISANT)                        |
|    -> Presque plus d'espace libre dans les CI             |
|    -> Nouvelles insertions vont causer des splits         |
|                                                           |
| 4. EXTENTS = 12 (ELEVE)                                   |
|    -> Le fichier s'est etendu 12 fois                     |
|    -> Fragmentation possible                               |
|                                                           |
| 5. Taux utilisation = 8200000/8500000 = 96%               |
|    -> Espace presque sature                               |
+----------------------------------------------------------+
| RECOMMANDATIONS:                                          |
+----------------------------------------------------------+
| 1. Reorganiser le fichier (REPRO vers nouveau cluster)    |
| 2. Augmenter FREESPACE a (15 15)                          |
| 3. Augmenter allocation primaire                          |
| 4. Planifier reorganisation periodique                    |
+----------------------------------------------------------+
```

---

## VI-9. Commandes d'Analyse Utiles

### Lister uniquement les statistiques

```jcl
//STEPSTAT EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT -
    ENTRIES(PROD.CLIENTS.KSDS) -
    ALL
/*
```

### Script d'analyse automatise

```jcl
//ANALYZE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT LEVEL(PROD) -
    CLUSTER -
    ALL

  /* Verifier les fichiers mal fermes */
  VERIFY DATASET(PROD.CLIENTS.KSDS)
  VERIFY DATASET(PROD.PRODUITS.KSDS)
/*
```

---

## Resume du Chapitre

| Section | Contenu |
|---------|---------|
| **CLUSTER** | Vue d'ensemble, associations |
| **ATTRIBUTES** | Caracteristiques du fichier |
| **STATISTICS** | Compteurs d'operations et splits |
| **ALLOCATION** | Espace alloue et utilise |
| **VOLUME** | Informations physiques |

---

## Aide-Memoire

```
Champs cles a surveiller:
- REC-TOTAL: Nombre d'enregistrements
- SPLITS-CI: Indicateur de fragmentation CI
- SPLITS-CA: Indicateur de fragmentation CA
- FREESPACE-CI/CA: Espace libre restant
- EXTENTS: Nombre d'extensions
- HI-USED-RBA/HI-ALLOC-RBA: Taux utilisation

Seuils d'alerte:
- SPLITS-CI > 100: Reorganisation recommandee
- SPLITS-CA > 0: Reorganisation necessaire
- FREESPACE < 5%: Risque de degradation
- EXTENTS > 10: Fragmentation importante
- Utilisation > 90%: Augmenter l'espace
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Commandes de Manipulation IDCAMS](05-manipulation-idcams.md) | [Chapitre VII - GDG et Codes Retour VSAM](07-gdg-codes-retour.md) |

---
*Formation VSAM - M2i Formation*
