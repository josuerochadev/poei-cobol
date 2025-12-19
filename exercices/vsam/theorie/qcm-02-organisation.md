# QCM 02 - Modes d'Organisation VSAM

## Chapitre II - Catalogues, Data Space et ShareOptions

---

### Question 1
Combien de Master Catalog peut-il y avoir par systeme z/OS ?

- [ ] a) Autant que necessaire
- [ ] b) Un par volume
- [ ] c) Un seul
- [ ] d) Un par utilisateur

<details>
<summary>Reponse</summary>

**c) Un seul**

Il n'y a qu'un seul Master Catalog par systeme z/OS. Il est cree lors de la generation du systeme et reside sur le volume systeme.
</details>

---

### Question 2
Quel est le role principal du Master Catalog ?

<details>
<summary>Reponse</summary>

Le Master Catalog a pour roles principaux :

1. Gerer et surveiller toutes les operations VSAM
2. Stocker les alias vers les User Catalogs
3. Contrôler l'acces aux Data Sets
4. Gerer l'autorisation de mot de passe
5. Localiser les Data Sets via les User Catalogs

Le Master Catalog ne contient pas directement les Data Sets VSAM mais des pointeurs vers les User Catalogs.
</details>

---

### Question 3
Qu'est-ce qu'un User Catalog ?

- [ ] a) Un catalogue personnel d'utilisateur
- [ ] b) Un catalogue secondaire pour organiser les Data Sets
- [ ] c) Un fichier de configuration utilisateur
- [ ] d) Un journal des connexions

<details>
<summary>Reponse</summary>

**b) Un catalogue secondaire pour organiser les Data Sets**

Un User Catalog est un catalogue de niveau hierarchique inferieur au Master Catalog. Il est fortement recommande pour organiser les Data Sets par application, projet ou environnement.
</details>

---

### Question 4
Un User Catalog est-il obligatoire ?

- [ ] a) Oui, toujours
- [ ] b) Non, mais fortement recommande
- [ ] c) Uniquement pour les fichiers KSDS
- [ ] d) Uniquement en production

<details>
<summary>Reponse</summary>

**b) Non, mais fortement recommande**

Les User Catalogs ne sont pas obligatoires mais sont fortement recommandes pour :
- Ameliorer la securite
- Faciliter l'organisation
- Separer les environnements (DEV, TEST, PROD)
</details>

---

### Question 5
Quelle commande cree un User Catalog ?

- [ ] a) CREATE USERCATALOG
- [ ] b) DEFINE USERCATALOG
- [ ] c) ALLOCATE CATALOG
- [ ] d) BUILD USERCATALOG

<details>
<summary>Reponse</summary>

**b) DEFINE USERCATALOG**

```jcl
DEFINE USERCATALOG (
  NAME (UCDISK01)
  VOLUME (DISK01)
  TRACKS (7000 0)
  FOR (9999))
```
</details>

---

### Question 6
Qu'est-ce qu'un VSAM Data Space ?

<details>
<summary>Reponse</summary>

Un VSAM Data Space est une zone de stockage sur un volume DASD reservee pour les donnees VSAM.

Caracteristiques :
- Ne peut contenir que des donnees VSAM
- Appartient a un User Catalog
- Un catalogue peut avoir plusieurs Data Spaces
- Cree avec DEFINE SPACE
</details>

---

### Question 7
Que signifie SHAREOPTIONS(1,3) ?

- [ ] a) 1 utilisateur max, 3 volumes
- [ ] b) Cross-region=1, Cross-system=3
- [ ] c) Priorite 1, niveau 3
- [ ] d) 1 lecture, 3 ecritures

<details>
<summary>Reponse</summary>

**b) Cross-region=1, Cross-system=3**

SHAREOPTIONS(1,3) signifie :
- **1** = Cross-region : Lecture multiple OU ecriture unique (dans le meme systeme)
- **3** = Cross-system : Lecture/ecriture multiple sans controle d'integrite (entre systemes)
</details>

---

### Question 8
Quelle est la valeur par defaut de SHAREOPTIONS ?

- [ ] a) (1,1)
- [ ] b) (1,3)
- [ ] c) (2,3)
- [ ] d) (3,3)

<details>
<summary>Reponse</summary>

**b) (1,3)**

La valeur par defaut de SHAREOPTIONS est (1,3), ce qui offre une bonne protection pour le partage au sein d'un systeme tout en permettant le partage entre systemes.
</details>

---

### Question 9
Quelle SHAREOPTION convient pour un fichier en lecture seule partage ?

- [ ] a) (1,3)
- [ ] b) (2,3)
- [ ] c) (3,3)
- [ ] d) (4,4)

<details>
<summary>Reponse</summary>

**a) (1,3)**

Pour un fichier en lecture seule :
- SHAREOPTIONS(1,3) est suffisant
- Permet plusieurs lecteurs simultanement
- N'autorise pas l'ecriture pendant la lecture
</details>

---

### Question 10
Quelle est la difference entre SHAREOPTIONS(3) et SHAREOPTIONS(4) ?

<details>
<summary>Reponse</summary>

| SHAREOPTIONS | Description |
|--------------|-------------|
| **(3,x)** | Lecture/ecriture multiple sans controle d'integrite |
| **(4,x)** | Comme (3) + rafraichissement buffer a chaque acces |

SHAREOPTIONS(4) offre une meilleure coherence des donnees mais avec une degradation des performances due au rafraichissement systematique des buffers.
</details>

---

### Question 11
Qu'est-ce qu'un ALIAS de catalogue ?

- [ ] a) Un nom alternatif pour un Data Set
- [ ] b) Un lien entre un HLQ et un User Catalog
- [ ] c) Une copie du catalogue
- [ ] d) Un raccourci ISPF

<details>
<summary>Reponse</summary>

**b) Un lien entre un HLQ et un User Catalog**

Un ALIAS permet de lier un High Level Qualifier (HLQ) a un User Catalog specifique. Tous les Data Sets commencant par ce HLQ seront automatiquement catalogues dans ce User Catalog.

```jcl
DEFINE ALIAS (
  NAME (FTEST)
  RELATE (UCDISK01))
CATALOG (CATALOG.MASTER)
```
</details>

---

### Question 12
Dans quel catalogue un ALIAS est-il defini ?

- [ ] a) Dans le User Catalog cible
- [ ] b) Dans le Master Catalog
- [ ] c) Dans les deux
- [ ] d) Dans un catalogue specifique ALIAS

<details>
<summary>Reponse</summary>

**b) Dans le Master Catalog**

L'ALIAS est defini dans le Master Catalog avec CATALOG(CATALOG.MASTER). Il pointe vers le User Catalog qui contiendra les Data Sets.
</details>

---

### Question 13
Que signifie l'option CANDIDATE dans DEFINE SPACE ?

- [ ] a) Le volume est en attente d'approbation
- [ ] b) Le volume est reserve sans allocation reelle
- [ ] c) Le volume est candidat pour suppression
- [ ] d) Le volume est temporaire

<details>
<summary>Reponse</summary>

**b) Le volume est reserve sans allocation reelle**

CANDIDATE reserve le volume comme destination potentielle pour les extensions de Data Sets sans allocation immediate d'espace.
</details>

---

### Question 14
Quel est le premier objet VSAM qui doit etre stocke sur un volume ?

- [ ] a) N'importe quel cluster
- [ ] b) Un Data Space
- [ ] c) Le User Catalog proprietaire du volume
- [ ] d) Un fichier ESDS

<details>
<summary>Reponse</summary>

**c) Le User Catalog proprietaire du volume**

Le User Catalog doit etre le premier objet VSAM stocke sur le volume dont il est proprietaire.
</details>

---

### Question 15
Combien de User Catalogs peut-on avoir par systeme ?

- [ ] a) 1
- [ ] b) 10
- [ ] c) 100
- [ ] d) Autant que necessaire

<details>
<summary>Reponse</summary>

**d) Autant que necessaire**

Il n'y a pas de limite theorique au nombre de User Catalogs. On peut en creer autant que necessaire pour organiser les Data Sets par application, environnement, etc.
</details>

---

### Question 16
Quelle SHAREOPTION est recommandee pour les applications CICS ?

- [ ] a) (1,3)
- [ ] b) (2,3)
- [ ] c) (3,3)
- [ ] d) (4,4)

<details>
<summary>Reponse</summary>

**b) (2,3)**

SHAREOPTIONS(2,3) est recommandee pour CICS car elle permet :
- Plusieurs lecteurs simultanement
- Un seul ecrivain a la fois
- La lecture pendant l'ecriture (mais risque de lire des donnees non commitees)
</details>

---

### Question 17
Quelle est la syntaxe pour definir un Data Space ?

<details>
<summary>Reponse</summary>

```jcl
DEFINE SPACE (
  FILE (ddname)
  VOLUME (volser)
  {CYLINDERS|RECORDS|TRACKS} (primary [secondary])
  [CANDIDATE]
) -
CATALOG (catname)
```

Exemple :
```jcl
DEFINE SPACE (
  FILE (SPDISK02)
  VOLUME (DISK02)
  TRACKS (13200 0)
) -
CATALOG (UCDISK02)
```
</details>

---

### Question 18
Un User Catalog peut-il posseder plusieurs volumes ?

- [ ] a) Non, un seul volume par catalogue
- [ ] b) Oui, plusieurs volumes
- [ ] c) Oui, mais maximum 10
- [ ] d) Uniquement avec l'option MULTIVOLUME

<details>
<summary>Reponse</summary>

**b) Oui, plusieurs volumes**

Un User Catalog peut posseder plusieurs volumes via l'utilisation de Data Spaces ou l'option ADDVOLUMES.
</details>

---

### Question 19
Que se passe-t-il avec SHAREOPTIONS(3,3) si deux programmes ecrivent simultanement ?

- [ ] a) Erreur automatique
- [ ] b) File d'attente automatique
- [ ] c) Les donnees peuvent etre corrompues
- [ ] d) Le dernier ecrasement gagne

<details>
<summary>Reponse</summary>

**c) Les donnees peuvent etre corrompues**

SHAREOPTIONS(3,3) n'offre aucun controle d'integrite. Si deux programmes ecrivent simultanement :
- Les donnees peuvent etre corrompues
- L'application doit gerer la synchronisation elle-meme
- Cette option est a eviter pour les fichiers partages en ecriture
</details>

---

### Question 20
Comment resoudre l'emplacement d'un Data Set FTEST.DATA.FILE ?

<details>
<summary>Reponse</summary>

Le processus de resolution est :

1. L'application demande : `FTEST.DATA.FILE`
2. Le systeme cherche l'alias `FTEST` dans le Master Catalog
3. Trouve : `FTEST -> UCDISK01` (User Catalog)
4. Le systeme cherche `FTEST.DATA.FILE` dans `UCDISK01`
5. Trouve l'entree avec les informations de volume
6. Acces au Data Set sur le volume physique
</details>

---

## Resume

| Concept | Description |
|---------|-------------|
| **Master Catalog** | Un seul par systeme, contient les alias |
| **User Catalog** | Multiples, contient les Data Sets |
| **Data Space** | Zone reservee pour VSAM sur un volume |
| **ALIAS** | Lien HLQ -> User Catalog |
| **SHAREOPTIONS(1,3)** | Defaut, securise |
| **SHAREOPTIONS(2,3)** | Pour CICS |
| **SHAREOPTIONS(3,3)** | Pas d'integrite |
| **SHAREOPTIONS(4,x)** | Rafraichissement buffer |

---
*Formation VSAM - M2i Formation*
