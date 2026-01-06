# Chapitre III - Structure des Data Sets VSAM

## III-1. Structure des Entites VSAM

### Vue d'Ensemble d'un Cluster

Un cluster VSAM est compose d'un ou plusieurs composants :

```
+-------------------------------------------------------------+
|                    CLUSTER VSAM                              |
+-------------------------------------------------------------+
|                                                              |
|  +---------------------+    +---------------------+          |
|  |   DATA COMPONENT    |    |   INDEX COMPONENT   |          |
|  |   (Enregistrements) |    |   (Clés + Pointeurs)|          |
|  +----------+----------+    +----------+----------+          |
|             |                          |                     |
|             v                          v                     |
|  +---------------------------------------------------+      |
|  |              CONTROL AREA (CA)                     |      |
|  |  +--------+ +--------+ +--------+ +--------+      |      |
|  |  |   CI   | |   CI   | |   CI   | |   CI   |      |      |
|  |  +--------+ +--------+ +--------+ +--------+      |      |
|  +---------------------------------------------------+      |
+-------------------------------------------------------------+
```

---

## III-2. Control Interval (CI)

### Definition

Le Control Interval est la plus petite unite echangee entre disque et mémoire. C'est l'equivalent du BLOCK pour les fichiers non-VSAM.

### Structure d'un CI

```
+------------------------------------------------------------+
|  E1  |  E2  |  E3  |  E4  |  FREE SPACE  |RDF|RDF|RDF|CIDF|
+------------------------------------------------------------+
  |      |      |      |         |           |    |    |   |
  +------+------+------+         |           |    |    |   |
  Enregistrements logiques       |           |    |    |   |
                                 |           |    |    |   |
  Espace libre pour extensions --+           |    |    |   |
                                             |    |    |   |
  RDF: Record Definition Field (3 octets) ---+----+----+   |
  - Longueur des enregistrements                            |
  - Nombre d'enregistrements adjacents de meme longueur     |
                                                            |
  CIDF: Control Interval Definition Field (4 octets) -------+
  - Informations sur l'intervalle de contrôle
```

### Caractéristiques du CI

| Caracteristique | Valeur |
|-----------------|--------|
| Taille | 512 octets a 32 Ko |
| Stockage | Zone contigue obligatoire |
| E/S | Lu/ecrit en une seule operation |
| Accès aléatoire | Petits CI (economie mémoire) |
| Accès séquentiel | Grands CI (reduction E/S) |

### Choix de la Taille du CI

| Type d'accès | Taille CI recommandee |
|--------------|----------------------|
| Aleatoire predominant | 512 - 2048 octets |
| Mixte | 4096 octets (defaut) |
| Séquentiel predominant | 8192 - 32768 octets |

---

## III-3. Enregistrements Fractionnes (SPANNED)

Quand un enregistrement est plus grand qu'un CI, il peut s'etendre sur plusieurs CI :

```
+-------------------------------------------------------------+
|  CI 1: | Enreg. Segment 1          | FS |RDF|RDF|CIDF|      |
+------------------------------------------------------------|
|  CI 2: | Enreg. Segment 2          | FS |RDF|RDF|CIDF|      |
+-------------------------------------------------------------+
|  CI 3: | Enreg. Segment 3          | FS |RDF|RDF|CIDF|      |
+-------------------------------------------------------------+
```

### Regles des Enregistrements SPANNED

- Doit commencer au debut d'un CI
- Peut s'etendre sur plusieurs CI dans une seule CA
- Un CI ne peut pas etre partage par deux enregistrements fractionnes
- Taille limite = taille de la Control Area
- Pour KSDS : la clé primaire doit etre dans le premier CI

---

## III-4. Control Area (CA)

### Definition

La Control Area est un ensemble de deux ou plusieurs CI contigus.

```
+-------------------------------------------------------------+
|                      CONTROL AREA                            |
+-------------------------------------------------------------+
|  +----------------------------------------------------------+|
|  | CI 1: E1 E2 E3 E4 E5 E6 E7 | FS |RDF|RDF|CIDF|           ||
|  +----------------------------------------------------------+|
|  | CI 2: E1 E2 E3 E4 E5 E6 E7 | FS |RDF|RDF|CIDF|           ||
|  +----------------------------------------------------------+|
|  | CI 3: E1 E2 E3 E4 E5 E6 E7 | FS |RDF|RDF|CIDF|           ||
|  +----------------------------------------------------------+|
|  | CI 4:            FREE CI                |CIDF|           ||
|  +----------------------------------------------------------+|
|  | CI 5:            FREE CI                |CIDF|           ||
|  +----------------------------------------------------------+|
+-------------------------------------------------------------+
```

### Caractéristiques de la CA

| Caracteristique | Valeur |
|-----------------|--------|
| Taille typique | 1 cylindre (15 tracks pour 3390) |
| Taille minimale | 1 track |
| Taille maximale | 15 tracks |
| Cluster VSAM | Taille = multiple d'un CA |

### Relation CI/CA/Cluster

```
1 Block = 512 bytes
1 Track 3390 = 56664 bytes
1 CA = 1 a 15 tracks
1 Cluster = N x CA
```

---

## III-5. ESDS - Entry Sequenced Data Set

### Caractéristiques

| Caracteristique | Description |
|-----------------|-------------|
| Ordre | Enregistrements stockes dans l'ordre de creation |
| Identification | Par adresse RBA (Relative Byte Address) |
| Modification RBA | Permanente, ne peut pas etre modifiée |
| Insertion | Toujours a la fin |
| Suppression | **Pas de suppression physique possible** |
| Taille | Fixe ou variable |
| Index | **Pas de composant Index** |
| Doublons | Autorises |

### Structure ESDS

```
+-------------------------------------------------------------+
|                    Structure ESDS                            |
+-------------------------------------------------------------+
|  CI 1 (RBA 0):                                               |
|  +------+------+------+------+------+-------+-------+-----+ |
|  |Rec 1 |Rec 2 |Rec 3 |Rec 4 |Rec 5 |RDF|RDF|RDF|CIDF|     | |
|  +------+------+------+------+------+-------+-------+-----+ |
|                                                              |
|  CI 2 (RBA 4096):                                            |
|  +------+------+------+------+------+-------+-------+-----+ |
|  |Rec 7 |Rec 8 |Rec 9 |Rec10 |Rec11 |RDF|RDF|RDF|CIDF|     | |
|  +------+------+------+------+------+-------+-------+-----+ |
|                                                              |
|  CI 3 (RBA 8192):                                            |
|  +------+------+------+-------------+-------+-------+-----+ |
|  |Rec13 |Rec14 |Rec15 |  Non utilise|RDF|RDF|RDF|CIDF|     | |
|  +------+------+------+-------------+-------+-------+-----+ |
+-------------------------------------------------------------+
```

### Calcul RBA

Si taille enregistrement = 200 octets :
- Record 1 : RBA = 0
- Record 2 : RBA = 200
- Record 3 : RBA = 400

### Types d'Accès ESDS

| Type | Description |
|------|-------------|
| Séquentiel | Depuis le debut ou le milieu, pas de saut possible |
| Direct (RBA) | Via la valeur RBA |
| Via AIX | Index alternatif pour accès aléatoire |

### ESDS Etendu

RBA sur 64 bits (XRBA) permet de depasser la limite 4 Go.

---

## III-6. KSDS - Key Sequenced Data Set

### Caractéristiques

| Caracteristique | Description |
|-----------------|-------------|
| Usage | Type le plus frequemment utilise |
| Composants | **DATA** et **INDEX** |
| Identification | Clé a position predéfinie dans l'enregistrement |
| Index | Contient pointeurs vers tous les enregistrements |
| Taille | Fixe ou variable |
| Unicite | Clé unique identifie un seul enregistrement |
| Chargement | Dans l'ordre croissant de la clé |
| Suppression | **Possible** (reorganisation automatique) |

### Regles des Clés

| Regle | Description |
|-------|-------------|
| Longueur | 1 a 255 octets, identique pour tous |
| Position | Identique pour tous les enregistrements |
| Unicite | Obligatoire, pas de doublons |
| Modification | Interdite apres creation |

### Types d'Accès KSDS

| Type | Description |
|------|-------------|
| Séquentiel | Dans l'ordre des clés (le plus rapide pour accès multiples) |
| Direct | Par valeur de clé complete ou generique |
| Dynamique | Positionnement par clé puis traitement séquentiel |

### Structure Index B-Tree

```
+-------------------------------------------------------------+
|                     INDEX COMPONENT                          |
|                                                              |
|                    +---------------+                         |
|                    |  Index Set    | (Niveaux supérieurs)    |
|                    |   (High Key)  |                         |
|                    +-------+-------+                         |
|              +-------------+-------------+                   |
|              v             v             v                   |
|         +--------+    +--------+    +--------+               |
|         |Sequence|    |Sequence|    |Sequence| (Niveau bas)  |
|         |  Set   |    |  Set   |    |  Set   |               |
|         +----+---+    +----+---+    +----+---+               |
|              |             |             |                   |
|              v             v             v                   |
|         +--------+    +--------+    +--------+               |
|         |  CI    |    |  CI    |    |  CI    | DATA          |
|         | Data   |    | Data   |    | Data   |               |
|         +--------+    +--------+    +--------+               |
+-------------------------------------------------------------+
```

---

## III-7. RRDS - Relative Record Data Set

### Caractéristiques

| Caracteristique | Description |
|-----------------|-------------|
| Identification | Par RRN (Relative Record Number) |
| Numerotation | Numero de sequence relatif au premier enregistrement |
| Accès | Aleatoire par numéro d'enregistrement |
| Taille | **Fixe uniquement** (RRDS standard) |
| Suppression | Laisse un emplacement vide (slot) |
| Insertion | Dans emplacements vides |
| Index | **Pas de composant Index** (RRDS fixe) |

### Structure RRDS

```
+-------------------------------------------------------------+
|  CI 1:                                                       |
|  +------+------+------+------+------+------+-------+-----+  |
|  |SLOT 1|SLOT 2|SLOT 3|SLOT 4|SLOT 5|SLOT 6|RDF|RDF|CIDF|  |
|  | RRN1 | RRN2 | RRN3 | RRN4 | RRN5 | RRN6 |       |     |  |
|  +------+------+------+------+------+------+-------+-----+  |
|                                                              |
|  CI 2:                                                       |
|  +------+------+------+------+------+------+-------+-----+  |
|  |SLOT 7|SLOT 8|SLOT 9|SLT10 |SLT11 |SLT12 |RDF|RDF|CIDF|  |
|  | RRN7 |(vide)| RRN9 |RRN10 |(vide)|RRN12 |       |     |  |
|  +------+------+------+------+------+------+-------+-----+  |
+-------------------------------------------------------------+
```

### VRRDS - Variable RRDS

| Caracteristique | RRDS | VRRDS |
|-----------------|------|-------|
| Longueur | Fixe | Variable |
| Index | Non | Oui |
| Comportement | Slots fixes | Comme KSDS avec RRN |
| Suppression | Slot vide | RRN reutilisable |

### Types d'Accès RRDS

| Type | Description |
|------|-------------|
| Séquentiel | Depuis le debut, ordre RRN croissant |
| Direct | Par numéro RRN |
| Dynamique | Positionnement par RRN puis séquentiel |

---

## III-8. LDS - Linear Data Set

### Caractéristiques

| Caracteristique | Description |
|-----------------|-------------|
| RDF/CIDF | **Pas de RDF ni CIDF** dans le CI |
| Taille CI | Fixe **4 Ko** |
| Type | Data Set non-VSAM avec fonctionnalites VSAM |
| Enregistrements | Pas de concept d'enregistrements |
| Contenu | Tous les octets sont des données |
| Composants | Uniquement DATA |
| Usage | Frequemment utilise par **DB2** |

### Structure LDS

```
+-------------------------------------------------------------+
|  Control Area 1:                                             |
|  +----------------------------------------------------------+|
|  | CI 1: Data (4096 bytes)                                  ||
|  +----------------------------------------------------------+|
|  | CI 2: Data (4096 bytes)                                  ||
|  +----------------------------------------------------------+|
|  | CI 3: Data (4096 bytes)                                  ||
|  +----------------------------------------------------------+|
|  | CI 4: Data (4096 bytes)                                  ||
|  +----------------------------------------------------------+|
|                                                              |
|  Control Area 2:                                             |
|  +----------------------------------------------------------+|
|  | CI 5: Data (4096 bytes)                                  ||
|  | ...                                                      ||
|  +----------------------------------------------------------+|
+-------------------------------------------------------------+
```

---

## III-9. Tableau Comparatif des Types VSAM

| Caracteristique | ESDS | KSDS | RRDS | VRRDS | LDS |
|-----------------|:----:|:----:|:----:|:-----:|:---:|
| Composant Data | X | X | X | X | X |
| Composant Index | - | X | - | X | - |
| Identification | RBA | Clé | RRN | RRN | - |
| Longueur fixe | X | X | X | - | - |
| Longueur variable | X | X | - | X | - |
| Suppression | - | X | X | X | - |
| Insertion milieu | - | X | X | X | - |
| Doublons clé | X | - | - | - | - |
| Accès séquentiel | X | X | X | X | - |
| Accès direct | RBA | Clé | RRN | RRN | - |
| AIX possible | X | X | - | - | - |

---

## III-10. Alternate Index (AIX)

### Definition

Un Alternate Index est un index secondaire permettant d'acceder a un cluster VSAM avec une clé differente de la clé primaire.

### Caractéristiques

- Peut etre créé pour KSDS et ESDS
- Clé secondaire peut avoir des doublons (NONUNIQUEKEY)
- AIX est lui-meme un cluster KSDS

### Etapes de Creation

```
+-------------------------------------------------------------+
|  1. DEFINE ALTERNATEINDEX                                    |
|     --> Cree la structure de l'index secondaire             |
|                                                              |
|  2. DEFINE PATH                                              |
|     --> Etablit le chemin entre AIX et cluster de base      |
|                                                              |
|  3. BLDINDEX                                                 |
|     --> Construit les clés de l'index secondaire            |
+-------------------------------------------------------------+
```

### UPGRADE vs NOUPGRADE

| Option | Description |
|--------|-------------|
| **UPGRADE** | AIX mis a jour automatiquement lors de modifications du cluster de base |
| **NOUPGRADE** | AIX non maintenu, nécessite reconstruction manuelle |

---

## Résumé du Chapitre

| Concept | Description |
|---------|-------------|
| **CI** | Control Interval - unite d'echange disque/mémoire |
| **CA** | Control Area - groupe de CI contigus |
| **RDF** | Record Definition Field - 3 octets par enregistrement |
| **CIDF** | Control Interval Definition Field - 4 octets par CI |
| **ESDS** | Séquentiel par RBA, pas de suppression |
| **KSDS** | Indexe par clé, le plus utilise |
| **RRDS** | Par numéro RRN, slots fixes |
| **LDS** | Lineaire 4K, utilise par DB2 |
| **AIX** | Index alternatif pour accès secondaire |

---

## Aide-Memoire

```
Tailles CI:
- Min: 512 octets
- Max: 32 Ko
- Defaut: 4096 octets

Composants par type:
- ESDS: DATA uniquement
- KSDS: DATA + INDEX
- RRDS: DATA uniquement
- VRRDS: DATA + INDEX
- LDS: DATA uniquement (4K fixe)

Suppression possible:
- ESDS: Non
- KSDS: Oui
- RRDS: Oui (slot vide)
- LDS: Non applicable

AIX:
- ESDS: Oui
- KSDS: Oui
- RRDS: Non
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre II - Les Modes d'Organisation VSAM](02-modes-organisation.md) | [Chapitre IV - Langage de Commandes AMS](04-commandes-ams.md) |

---
*Formation VSAM - M2i Formation*
