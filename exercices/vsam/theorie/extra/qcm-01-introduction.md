# QCM 01 - Introduction a VSAM

## Chapitre I - Presentation generale et concepts de base

---

### Question 1
Que signifie VSAM ?

- [ ] a) Virtual System Access Method
- [ ] b) Virtual Storage Access Method
- [ ] c) Volume Storage Access Manager
- [ ] d) Variable Sequential Access Method

<details>
<summary>Reponse</summary>

**b) Virtual Storage Access Method**

VSAM est une methode d'acces au stockage introduite par IBM en 1970 pour les systemes MVS, OS/390 et z/OS.
</details>

---

### Question 2
Sur quel type de peripherique VSAM peut-il stocker des donnees ?

- [ ] a) Bandes magnetiques uniquement
- [ ] b) Disques DASD uniquement
- [ ] c) Bandes et disques
- [ ] d) Memoire virtuelle uniquement

<details>
<summary>Reponse</summary>

**b) Disques DASD uniquement**

VSAM est valable UNIQUEMENT sur disques DASD (Direct Access Storage Device). Les donnees VSAM ne peuvent pas etre stockees sur bandes.
</details>

---

### Question 3
Quels sont les quatre types d'organisation VSAM ?

<details>
<summary>Reponse</summary>

Les quatre types d'organisation VSAM sont :

| Type | Nom Complet | Description |
|------|-------------|-------------|
| **ESDS** | Entry Sequenced Data Set | Organisation sequentielle |
| **KSDS** | Key Sequenced Data Set | Organisation indexee par cle |
| **RRDS** | Relative Record Data Set | Organisation relative par numero |
| **LDS** | Linear Data Set | Donnees non structurees (4K) |
</details>

---

### Question 4
Que signifie VTOC ?

- [ ] a) Virtual Table Of Contents
- [ ] b) Volume Table Of Contents
- [ ] c) VSAM Table Of Clusters
- [ ] d) Variable Table Of Catalogs

<details>
<summary>Reponse</summary>

**b) Volume Table Of Contents**

La VTOC est la table des matieres d'un volume DASD. Elle contient les informations sur l'espace alloue et disponible sur le volume.
</details>

---

### Question 5
Quelle est la taille approximative d'une piste (Track) sur un disque 3390 ?

- [ ] a) 16 Ko
- [ ] b) 32 Ko
- [ ] c) 56 Ko
- [ ] d) 100 Ko

<details>
<summary>Reponse</summary>

**c) 56 Ko**

Sur un disque 3390, une piste (Track) fait environ 56 664 octets (~56 Ko). Un cylindre = 15 pistes = environ 849 Ko.
</details>

---

### Question 6
Combien de pistes (Tracks) composent un cylindre sur un disque 3390 ?

- [ ] a) 10
- [ ] b) 15
- [ ] c) 20
- [ ] d) 30

<details>
<summary>Reponse</summary>

**b) 15**

Un cylindre sur un disque 3390 est compose de 15 pistes (Tracks).
</details>

---

### Question 7
Quel utilitaire permet d'initialiser un DASD et de creer la VTOC ?

- [ ] a) IDCAMS
- [ ] b) IEBGENER
- [ ] c) ICKDSF
- [ ] d) IEFBR14

<details>
<summary>Reponse</summary>

**c) ICKDSF**

L'utilitaire ICKDSF (Device Support Facilities) est utilise pour initialiser les volumes DASD et creer la VTOC.
</details>

---

### Question 8
Quelle est la difference entre QSAM et VSAM ?

<details>
<summary>Reponse</summary>

| Aspect | QSAM | VSAM |
|--------|------|------|
| **Structure** | Blocs physiques | CI, CA |
| **Index** | Separe ou absent | Integre (KSDS) |
| **Acces** | Sequentiel principalement | Sequentiel, direct, dynamique |
| **Catalogue** | Optionnel | Obligatoire |
| **Gestion espace** | Manuelle | Automatique |

VSAM offre plus de flexibilite et une meilleure gestion automatique de l'espace.
</details>

---

### Question 9
Que signifie le format FB pour les enregistrements ?

- [ ] a) Fixed Binary
- [ ] b) Fixed Blocked
- [ ] c) File Backup
- [ ] d) Format Buffer

<details>
<summary>Reponse</summary>

**b) Fixed Blocked**

FB (Fixed Blocked) signifie que les enregistrements ont une longueur fixe et sont regroupes en blocs. C'est le format le plus couramment utilise.
</details>

---

### Question 10
Quelle est la longueur maximale d'un qualificateur dans un nom de Data Set ?

- [ ] a) 4 caracteres
- [ ] b) 8 caracteres
- [ ] c) 12 caracteres
- [ ] d) 44 caracteres

<details>
<summary>Reponse</summary>

**b) 8 caracteres**

Chaque qualificateur d'un nom de Data Set peut avoir de 1 a 8 caracteres. Le nom complet peut avoir jusqu'a 44 caracteres au total.
</details>

---

### Question 11
Quel format d'enregistrement inclut un RDW (Record Descriptor Word) ?

- [ ] a) F (Fixed)
- [ ] b) FB (Fixed Blocked)
- [ ] c) V ou VB (Variable)
- [ ] d) U (Undefined)

<details>
<summary>Reponse</summary>

**c) V ou VB (Variable)**

Les formats V (Variable) et VB (Variable Blocked) incluent un RDW de 4 octets au debut de chaque enregistrement pour indiquer sa longueur.
</details>

---

### Question 12
Qu'est-ce qu'un PDS ?

- [ ] a) Physical Data Set
- [ ] b) Partitioned Data Set
- [ ] c) Primary Data Storage
- [ ] d) Protected Data System

<details>
<summary>Reponse</summary>

**b) Partitioned Data Set**

Un PDS est un Data Set partitionne qui contient des membres identifies par un nom unique (1-8 caracteres). Il est compose d'un directory et d'une zone de donnees.
</details>

---

### Question 13
Quelle est la difference entre un PDS et un PDSE ?

<details>
<summary>Reponse</summary>

| Caracteristique | PDS | PDSE |
|-----------------|-----|------|
| **Directory** | Taille fixe | Extensible |
| **Fragmentation** | Oui (compression manuelle) | Non (reutilisation auto) |
| **Compression** | IEBCOPY necessaire | Pas necessaire |
| **DSNTYPE** | Non specifie | LIBRARY |
| **Acces concurrent** | Limite | Ameliore |

PDSE est l'evolution moderne du PDS avec gestion automatique de l'espace.
</details>

---

### Question 14
Peut-on modifier directement un fichier VSAM avec un editeur ISPF ?

- [ ] a) Oui, comme tout fichier
- [ ] b) Non, jamais
- [ ] c) Oui, mais uniquement en lecture
- [ ] d) Uniquement pour les ESDS

<details>
<summary>Reponse</summary>

**b) Non, jamais**

Les fichiers VSAM ont un format propre non comprehensible par d'autres methodes. On ne peut pas les editer directement avec ISPF. Il faut utiliser IDCAMS ou des programmes applicatifs.
</details>

---

### Question 15
Quel est le VOLSER ?

- [ ] a) Le nom du Data Set
- [ ] b) Le numero de serie du volume (6 caracteres)
- [ ] c) Le type de peripherique
- [ ] d) L'adresse du catalogue

<details>
<summary>Reponse</summary>

**b) Le numero de serie du volume (6 caracteres)**

Le VOLSER (Volume Serial) est un identifiant unique de 6 caracteres qui identifie un volume DASD. Il est stocke dans le premier enregistrement du volume.
</details>

---

### Question 16
Quels types de donnees sont typiquement stockes dans des fichiers VSAM ?

- [ ] a) Programmes sources et JCL
- [ ] b) Modules executables
- [ ] c) Donnees applicatives
- [ ] d) Documentation

<details>
<summary>Reponse</summary>

**c) Donnees applicatives**

VSAM est utilise pour stocker des donnees applicatives (clients, produits, transactions, etc.). Les programmes sources, JCL et modules executables sont generalement stockes dans des PDS.
</details>

---

### Question 17
Quelle methode d'acces est obsolete et remplacee par VSAM ?

- [ ] a) QSAM
- [ ] b) BSAM
- [ ] c) BDAM
- [ ] d) BPAM

<details>
<summary>Reponse</summary>

**c) BDAM**

BDAM (Basic Direct Access Method) est une methode obsolete d'acces aleatoire par adresse physique. VSAM l'a largement remplacee pour l'acces direct aux donnees.
</details>

---

### Question 18
Que signifie "spanned" pour un enregistrement VSAM ?

- [ ] a) L'enregistrement est compresse
- [ ] b) L'enregistrement s'etend sur plusieurs CI
- [ ] c) L'enregistrement est crypte
- [ ] d) L'enregistrement est duplique

<details>
<summary>Reponse</summary>

**b) L'enregistrement s'etend sur plusieurs CI**

Un enregistrement "spanned" (fractionne) est plus grand qu'un CI et s'etend sur plusieurs Control Intervals. Il doit commencer au debut d'un CI.
</details>

---

### Question 19
Combien de pistes maximum peut avoir un PDS ?

- [ ] a) 1000
- [ ] b) 15000
- [ ] c) 65535
- [ ] d) Illimite

<details>
<summary>Reponse</summary>

**c) 65535**

Un PDS peut avoir jusqu'a 65535 pistes maximum. Il ne peut pas s'etendre au-dela d'un seul volume.
</details>

---

### Question 20
Pourquoi doit-on faire un COMPRESS sur un PDS ?

<details>
<summary>Reponse</summary>

**Pour recuperer l'espace des membres supprimes**

Dans un PDS, la suppression d'un membre ne libere pas l'espace physique. L'espace reste alloue mais inutilisable. La compression (IEBCOPY COMPRESS) reorganise les membres et recupere l'espace perdu.

Note : Ce probleme n'existe pas avec les PDSE qui reutilisent automatiquement l'espace.
</details>

---

## Resume

| Concept | Description |
|---------|-------------|
| **VSAM** | Virtual Storage Access Method |
| **DASD** | Direct Access Storage Device |
| **VTOC** | Volume Table Of Contents |
| **ESDS** | Entry Sequenced (sequentiel) |
| **KSDS** | Key Sequenced (indexe) |
| **RRDS** | Relative Record (par numero) |
| **LDS** | Linear Data Set (4K) |
| **Track 3390** | ~56 Ko |
| **Cylindre** | 15 Tracks |
| **PDS** | Partitioned Data Set |
| **PDSE** | PDS Extended |

---
*Formation VSAM - M2i Formation*
