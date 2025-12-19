# QCM 03 - Structure des Data Sets VSAM

## Chapitre III - CI, CA, ESDS, KSDS, RRDS, LDS et AIX

---

### Question 1
Que signifie CI dans le contexte VSAM ?

- [ ] a) Cluster Index
- [ ] b) Control Interval
- [ ] c) Catalog Information
- [ ] d) Common Interface

<details>
<summary>Reponse</summary>

**b) Control Interval**

Le Control Interval est la plus petite unite echangee entre disque et memoire. C'est l'equivalent du BLOCK pour les fichiers non-VSAM.
</details>

---

### Question 2
Quelle est la plage de taille valide pour un CI ?

- [ ] a) 256 octets a 16 Ko
- [ ] b) 512 octets a 32 Ko
- [ ] c) 1 Ko a 64 Ko
- [ ] d) 4 Ko a 128 Ko

<details>
<summary>Reponse</summary>

**b) 512 octets a 32 Ko**

La taille d'un CI peut aller de 512 octets a 32 768 octets (32 Ko). La valeur par defaut est generalement 4096 octets.
</details>

---

### Question 3
Que contient le CIDF ?

- [ ] a) Les donnees de l'enregistrement
- [ ] b) La cle de l'enregistrement
- [ ] c) Les informations sur le Control Interval
- [ ] d) Le numero de version

<details>
<summary>Reponse</summary>

**c) Les informations sur le Control Interval**

Le CIDF (Control Interval Definition Field) fait 4 octets et contient les informations sur l'intervalle de controle (espace libre, etc.).
</details>

---

### Question 4
Que contient le RDF ?

- [ ] a) L'adresse du prochain enregistrement
- [ ] b) La longueur et le nombre d'enregistrements adjacents
- [ ] c) La cle primaire
- [ ] d) Le checksum des donnees

<details>
<summary>Reponse</summary>

**b) La longueur et le nombre d'enregistrements adjacents**

Le RDF (Record Definition Field) fait 3 octets et contient la longueur des enregistrements et le nombre d'enregistrements adjacents de meme longueur.
</details>

---

### Question 5
Qu'est-ce qu'une Control Area (CA) ?

- [ ] a) Une zone de controle d'acces
- [ ] b) Un ensemble de CI contigus
- [ ] c) L'aire de catalogue
- [ ] d) Une zone de compression

<details>
<summary>Reponse</summary>

**b) Un ensemble de CI contigus**

Une Control Area est un ensemble de deux ou plusieurs CI contigus. Sa taille typique est de 1 cylindre (15 tracks pour un 3390).
</details>

---

### Question 6
Quel est le type de cluster VSAM le plus frequemment utilise ?

- [ ] a) ESDS
- [ ] b) KSDS
- [ ] c) RRDS
- [ ] d) LDS

<details>
<summary>Reponse</summary>

**b) KSDS**

Le KSDS (Key Sequenced Data Set) est le type le plus utilise car il permet un acces indexe par cle, avec des possibilites de lecture sequentielle, directe et dynamique.
</details>

---

### Question 7
Comment les enregistrements sont-ils identifies dans un ESDS ?

- [ ] a) Par une cle
- [ ] b) Par un numero de record (RRN)
- [ ] c) Par une adresse RBA
- [ ] d) Par un index

<details>
<summary>Reponse</summary>

**c) Par une adresse RBA**

Dans un ESDS, les enregistrements sont identifies par leur RBA (Relative Byte Address), qui est leur position en octets depuis le debut du fichier.
</details>

---

### Question 8
Peut-on supprimer un enregistrement dans un ESDS ?

- [ ] a) Oui, comme dans tout fichier
- [ ] b) Non, jamais physiquement
- [ ] c) Oui, mais uniquement le dernier
- [ ] d) Oui, avec une commande speciale

<details>
<summary>Reponse</summary>

**b) Non, jamais physiquement**

Dans un ESDS, la suppression physique n'est pas possible. On ne peut qu'ajouter des enregistrements a la fin. Pour marquer un enregistrement comme supprime, il faut utiliser une logique applicative.
</details>

---

### Question 9
Quelle est la longueur maximale d'une cle KSDS ?

- [ ] a) 64 octets
- [ ] b) 128 octets
- [ ] c) 255 octets
- [ ] d) 512 octets

<details>
<summary>Reponse</summary>

**c) 255 octets**

La cle d'un KSDS peut avoir une longueur de 1 a 255 octets. Elle doit etre identique pour tous les enregistrements.
</details>

---

### Question 10
Dans un KSDS, peut-on avoir des cles en double ?

- [ ] a) Oui, toujours
- [ ] b) Non, jamais
- [ ] c) Oui, avec l'option DUPLICATEKEY
- [ ] d) Oui, dans l'AIX uniquement

<details>
<summary>Reponse</summary>

**b) Non, jamais**

Dans un KSDS, la cle primaire doit etre unique. Les doublons ne sont pas autorises. En revanche, un AIX peut avoir des cles en double avec NONUNIQUEKEY.
</details>

---

### Question 11
Comment les enregistrements sont-ils identifies dans un RRDS ?

- [ ] a) Par une cle
- [ ] b) Par un numero RRN (Relative Record Number)
- [ ] c) Par une adresse RBA
- [ ] d) Par un hash

<details>
<summary>Reponse</summary>

**b) Par un numero RRN (Relative Record Number)**

Dans un RRDS, chaque enregistrement est identifie par son numero relatif (RRN), qui est sa position sequentielle depuis le premier enregistrement.
</details>

---

### Question 12
Quelle est la particularite d'un LDS ?

- [ ] a) Il n'a pas de RDF ni CIDF
- [ ] b) Il a une cle obligatoire
- [ ] c) Il ne peut pas etre catalogue
- [ ] d) Il est toujours compresse

<details>
<summary>Reponse</summary>

**a) Il n'a pas de RDF ni CIDF**

Un LDS (Linear Data Set) n'a pas de RDF ni CIDF. Tous les octets du CI sont des donnees. La taille du CI est fixe a 4 Ko.
</details>

---

### Question 13
Quelle application utilise frequemment les LDS ?

- [ ] a) COBOL
- [ ] b) CICS
- [ ] c) DB2
- [ ] d) JCL

<details>
<summary>Reponse</summary>

**c) DB2**

DB2 utilise frequemment les LDS pour stocker ses tablespaces car le format LDS permet un acces direct aux donnees sans overhead de structure VSAM.
</details>

---

### Question 14
Qu'est-ce qu'un AIX ?

- [ ] a) Un fichier auxiliaire
- [ ] b) Un index alternatif
- [ ] c) Un acces indexe automatique
- [ ] d) Un alias d'index

<details>
<summary>Reponse</summary>

**b) Un index alternatif**

Un AIX (Alternate Index) est un index secondaire qui permet d'acceder a un cluster VSAM avec une cle differente de la cle primaire.
</details>

---

### Question 15
Pour quels types de clusters peut-on creer un AIX ?

- [ ] a) KSDS et ESDS uniquement
- [ ] b) KSDS et RRDS uniquement
- [ ] c) Tous les types
- [ ] d) KSDS uniquement

<details>
<summary>Reponse</summary>

**a) KSDS et ESDS uniquement**

Les AIX ne peuvent etre crees que pour les KSDS et ESDS. Les RRDS et LDS ne supportent pas les index alternatifs.
</details>

---

### Question 16
Que signifie UPGRADE pour un AIX ?

- [ ] a) Mettre a jour la version de l'AIX
- [ ] b) Mise a jour automatique de l'AIX lors de modifications du cluster
- [ ] c) Ameliorer les performances
- [ ] d) Augmenter la taille de l'AIX

<details>
<summary>Reponse</summary>

**b) Mise a jour automatique de l'AIX lors de modifications du cluster**

UPGRADE signifie que l'AIX sera automatiquement mis a jour lorsque le cluster de base est modifie (ajout, suppression, modification d'enregistrements).
</details>

---

### Question 17
Quelle est la structure d'index utilisee par VSAM pour les KSDS ?

- [ ] a) Index plat
- [ ] b) B-Tree
- [ ] c) Hash table
- [ ] d) Index bitmap

<details>
<summary>Reponse</summary>

**b) B-Tree**

VSAM utilise une structure d'index B-Tree pour les KSDS, avec un Index Set (niveaux superieurs) et un Sequence Set (niveau bas) pointant vers les CI de donnees.
</details>

---

### Question 18
Qu'est-ce qu'un PATH dans le contexte VSAM ?

- [ ] a) Le chemin vers un fichier
- [ ] b) Le lien entre un AIX et son cluster de base
- [ ] c) Une procedure de navigation
- [ ] d) Un raccourci vers le catalogue

<details>
<summary>Reponse</summary>

**b) Le lien entre un AIX et son cluster de base**

Un PATH etablit la connexion entre un AIX et son cluster de base, permettant d'acceder aux donnees via la cle secondaire.
</details>

---

### Question 19
Quelles sont les trois etapes pour creer un AIX fonctionnel ?

<details>
<summary>Reponse</summary>

Les trois etapes sont :

1. **DEFINE ALTERNATEINDEX** - Cree la structure de l'index secondaire
2. **DEFINE PATH** - Etablit le chemin entre l'AIX et le cluster de base
3. **BLDINDEX** - Construit les cles de l'index secondaire

L'ordre est important : le cluster de base doit contenir des donnees avant BLDINDEX.
</details>

---

### Question 20
Quelle est la difference entre UNIQUEKEY et NONUNIQUEKEY pour un AIX ?

<details>
<summary>Reponse</summary>

| Option | Description |
|--------|-------------|
| **UNIQUEKEY** | La cle secondaire doit etre unique (pas de doublons) |
| **NONUNIQUEKEY** | La cle secondaire peut avoir des doublons |

Exemple : Un AIX sur le nom de famille peut avoir NONUNIQUEKEY car plusieurs personnes peuvent avoir le meme nom.
</details>

---

### Question 21
Qu'est-ce qu'un enregistrement SPANNED ?

- [ ] a) Un enregistrement compresse
- [ ] b) Un enregistrement qui s'etend sur plusieurs CI
- [ ] c) Un enregistrement partage entre clusters
- [ ] d) Un enregistrement duplique

<details>
<summary>Reponse</summary>

**b) Un enregistrement qui s'etend sur plusieurs CI**

Un enregistrement SPANNED est plus grand qu'un CI et s'etend sur plusieurs Control Intervals. Il doit commencer au debut d'un CI et rester dans une seule CA.
</details>

---

### Question 22
Dans un RRDS standard, quelle est la contrainte sur la longueur des enregistrements ?

- [ ] a) Variable obligatoire
- [ ] b) Fixe obligatoire
- [ ] c) Maximum 32 Ko
- [ ] d) Pas de contrainte

<details>
<summary>Reponse</summary>

**b) Fixe obligatoire**

Dans un RRDS standard, les enregistrements doivent avoir une longueur fixe (slots de taille fixe). Le VRRDS permet des enregistrements de longueur variable.
</details>

---

### Question 23
Que se passe-t-il quand on supprime un enregistrement dans un RRDS ?

- [ ] a) L'espace est libere immediatement
- [ ] b) Le slot reste vide et reutilisable
- [ ] c) Le fichier est reorganise
- [ ] d) Erreur, suppression impossible

<details>
<summary>Reponse</summary>

**b) Le slot reste vide et reutilisable**

Dans un RRDS, la suppression laisse un slot vide (emplacement). Ce slot peut etre reutilise pour une nouvelle insertion au meme RRN.
</details>

---

### Question 24
Quel composant est present dans un KSDS mais pas dans un ESDS ?

- [ ] a) DATA
- [ ] b) INDEX
- [ ] c) CIDF
- [ ] d) RDF

<details>
<summary>Reponse</summary>

**b) INDEX**

Un KSDS a deux composants : DATA et INDEX. Un ESDS n'a que le composant DATA car il n'y a pas d'index (acces par RBA).
</details>

---

### Question 25
Quelle taille de CI est recommandee pour un acces principalement aleatoire ?

- [ ] a) Petits CI (512-2048 octets)
- [ ] b) CI moyens (4096 octets)
- [ ] c) Grands CI (8192-32768 octets)
- [ ] d) Taille maximale (32 Ko)

<details>
<summary>Reponse</summary>

**a) Petits CI (512-2048 octets)**

Pour un acces principalement aleatoire, des petits CI sont recommandes pour :
- Economiser la memoire (buffer)
- Lire uniquement les donnees necessaires
- Reduire les temps de transfert

Les grands CI sont preferes pour l'acces sequentiel.
</details>

---

## Resume

| Concept | Description |
|---------|-------------|
| **CI** | Control Interval (512-32 Ko) |
| **CA** | Control Area (groupe de CI) |
| **CIDF** | 4 octets info CI |
| **RDF** | 3 octets info enregistrement |
| **ESDS** | Sequentiel par RBA |
| **KSDS** | Indexe par cle (DATA + INDEX) |
| **RRDS** | Par numero RRN |
| **LDS** | Lineaire 4K (DB2) |
| **AIX** | Index alternatif |
| **PATH** | Lien AIX-Cluster |

---
*Formation VSAM - M2i Formation*
