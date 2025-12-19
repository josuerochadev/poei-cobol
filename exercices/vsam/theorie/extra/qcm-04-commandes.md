# QCM 04 - Commandes IDCAMS

## Chapitres IV-VII - AMS, Manipulation, LISTCAT, GDG et Codes Retour

---

### Question 1
Que signifie IDCAMS ?

- [ ] a) Integrated Data Catalog Management System
- [ ] b) Integrated Data Cluster Access Method Services
- [ ] c) Index Data Cluster Access Manager
- [ ] d) Interactive Data Control And Management Service

<details>
<summary>Reponse</summary>

**b) Integrated Data Cluster Access Method Services**

IDCAMS est l'utilitaire principal pour gerer les objets VSAM sur z/OS.
</details>

---

### Question 2
Quel est le DD obligatoire pour les messages de sortie IDCAMS ?

- [ ] a) SYSOUT
- [ ] b) SYSPRINT
- [ ] c) SYSMSG
- [ ] d) SYSLOG

<details>
<summary>Reponse</summary>

**b) SYSPRINT**

SYSPRINT est le DD obligatoire pour les messages de sortie d'IDCAMS :
```jcl
//SYSPRINT DD SYSOUT=*
```
</details>

---

### Question 3
Quel caractere permet de continuer une commande IDCAMS sur la ligne suivante ?

- [ ] a) +
- [ ] b) &
- [ ] c) -
- [ ] d) \

<details>
<summary>Reponse</summary>

**c) -**

Le tiret haut (-) permet de continuer une commande sur la ligne suivante :
```jcl
DEFINE CLUSTER (NAME(TEST) -
  TRACKS(1 1) -
  VOLUMES(ZASYS1))
```
</details>

---

### Question 4
Que represente LASTCC dans IDCAMS ?

- [ ] a) Le code retour du dernier job
- [ ] b) Le code retour de la derniere commande executee
- [ ] c) Le nombre de commandes executees
- [ ] d) Le dernier catalogue utilise

<details>
<summary>Reponse</summary>

**b) Le code retour de la derniere commande executee**

LASTCC contient le code condition de la derniere commande IDCAMS executee.
</details>

---

### Question 5
Quelle est la difference entre LASTCC et MAXCC ?

<details>
<summary>Reponse</summary>

| Variable | Description |
|----------|-------------|
| **LASTCC** | Code condition de la derniere commande executee |
| **MAXCC** | Code condition le plus eleve de toutes les commandes precedentes |

Exemple : Si trois commandes retournent 0, 4, 0 :
- LASTCC = 0 (derniere commande)
- MAXCC = 4 (maximum global)
</details>

---

### Question 6
Quel code retour IDCAMS indique un avertissement (warning) ?

- [ ] a) 0
- [ ] b) 4
- [ ] c) 8
- [ ] d) 12

<details>
<summary>Reponse</summary>

**b) 4**

| Code | Signification |
|------|---------------|
| 0 | Succes |
| 4 | Avertissement (fonction completee) |
| 8 | Erreur majeure |
| 12 | Erreur logique |
| 16 | Erreur grave |
</details>

---

### Question 7
Quel parametre DEFINE CLUSTER indique un cluster KSDS ?

- [ ] a) KEYED
- [ ] b) INDEXED
- [ ] c) SEQUENTIAL
- [ ] d) KEYSEQUENCED

<details>
<summary>Reponse</summary>

**b) INDEXED**

Les types de cluster sont :
- INDEXED = KSDS
- NONINDEXED = ESDS
- NUMBERED = RRDS
- LINEAR = LDS
</details>

---

### Question 8
Que signifie REUSE dans DEFINE CLUSTER ?

- [ ] a) Le cluster peut etre partage
- [ ] b) Le cluster est vide a chaque ouverture en OUTPUT
- [ ] c) Le cluster peut etre renomme
- [ ] d) Le cluster peut etre deplace

<details>
<summary>Reponse</summary>

**b) Le cluster est vide a chaque ouverture en OUTPUT**

REUSE permet de vider automatiquement le cluster quand il est ouvert en mode OUTPUT, sans avoir a le supprimer et le recreer.
</details>

---

### Question 9
Quelle commande permet de copier des donnees entre Data Sets ?

- [ ] a) COPY
- [ ] b) MOVE
- [ ] c) REPRO
- [ ] d) TRANSFER

<details>
<summary>Reponse</summary>

**c) REPRO**

REPRO permet de copier des donnees entre Data Sets, de charger des clusters VSAM, et de convertir des formats (VSAM <-> sequentiel).
</details>

---

### Question 10
Quelle commande permet de modifier les attributs d'un Data Set existant ?

- [ ] a) MODIFY
- [ ] b) CHANGE
- [ ] c) UPDATE
- [ ] d) ALTER

<details>
<summary>Reponse</summary>

**d) ALTER**

ALTER permet de modifier les attributs d'un Data Set existant (NEWNAME, FREESPACE, SHAREOPTIONS, etc.).
</details>

---

### Question 11
Que fait VERIFY sur un fichier VSAM ?

<details>
<summary>Reponse</summary>

VERIFY :
- Verifie et repare les fichiers VSAM mal fermes apres une erreur
- Identifie la fin correcte du Data Set
- Reinitialise l'entree de catalogue
- Ajoute les enregistrements de fin de donnees corrects

Usage typique en debut de job :
```jcl
VERIFY DATASET(FTEST.KSDS)
```
</details>

---

### Question 12
Quels sont les formats de sortie de la commande PRINT ?

- [ ] a) TEXT, BINARY, RAW
- [ ] b) CHARACTER, HEX, DUMP
- [ ] c) ASCII, EBCDIC, UNICODE
- [ ] d) NORMAL, DEBUG, TRACE

<details>
<summary>Reponse</summary>

**b) CHARACTER, HEX, DUMP**

| Format | Description |
|--------|-------------|
| CHARACTER | Format texte lisible |
| HEX | Format hexadecimal |
| DUMP | Format dump (hex + caracteres) |
</details>

---

### Question 13
Quelle est la difference entre REPRO et EXPORT ?

<details>
<summary>Reponse</summary>

| Aspect | REPRO | EXPORT |
|--------|-------|--------|
| Contenu | Donnees uniquement | Donnees + metadata catalogue |
| Usage | Copie simple | Sauvegarde complete |
| Restauration | REPRO | IMPORT |
| Format | Fichier standard | Format portable VSAM |

EXPORT cree une copie portable complete qui peut etre restauree avec IMPORT sur un autre systeme.
</details>

---

### Question 14
Que fait l'option PURGE dans DELETE ?

- [ ] a) Vide le fichier avant suppression
- [ ] b) Ignore la periode de retention
- [ ] c) Supprime les sauvegardes
- [ ] d) Nettoie le catalogue

<details>
<summary>Reponse</summary>

**b) Ignore la periode de retention**

PURGE permet de supprimer un Data Set meme si sa periode de retention (TO/FOR) n'est pas expiree.
</details>

---

### Question 15
Que fait l'option ERASE dans DELETE ?

- [ ] a) Supprime l'entree du catalogue uniquement
- [ ] b) Ecrase les donnees avec des zeros binaires avant suppression
- [ ] c) Efface les traces de log
- [ ] d) Supprime l'index uniquement

<details>
<summary>Reponse</summary>

**b) Ecrase les donnees avec des zeros binaires avant suppression**

ERASE est utilise pour les donnees sensibles afin qu'elles ne puissent pas etre recuperees apres suppression.
</details>

---

### Question 16
Quel champ LISTCAT indique le nombre de CI splits ?

- [ ] a) CI-COUNT
- [ ] b) SPLITS-CI
- [ ] c) CI-SPLITS
- [ ] d) SPLIT-COUNT

<details>
<summary>Reponse</summary>

**b) SPLITS-CI**

SPLITS-CI indique le nombre de fois qu'un CI a du etre divise pour inserer de nouveaux enregistrements. Un nombre eleve indique un besoin de reorganisation.
</details>

---

### Question 17
Que signifie un nombre eleve de SPLITS-CA dans LISTCAT ?

<details>
<summary>Reponse</summary>

Un nombre eleve de SPLITS-CA (Control Area splits) indique :

- Des insertions frequentes qui ont necesssite l'extension de Control Areas
- Une fragmentation du fichier
- Une degradation des performances
- Un besoin urgent de reorganisation (REPRO vers un nouveau cluster)

**Recommandation** : Augmenter le FREESPACE-CA pour prevenir les splits.
</details>

---

### Question 18
Qu'est-ce qu'un GDG ?

- [ ] a) General Data Group
- [ ] b) Generation Data Group
- [ ] c) Global Data Gateway
- [ ] d) Group Data Generator

<details>
<summary>Reponse</summary>

**b) Generation Data Group**

Un GDG est un groupe de Data Sets lies par un nom commun, utilises pour collecter des donnees periodiques (journalier, hebdomadaire, mensuel).
</details>

---

### Question 19
Que signifie la reference GDG.BASE(+1) ?

- [ ] a) La premiere generation
- [ ] b) La generation actuelle plus une
- [ ] c) Une nouvelle generation a creer
- [ ] d) La generation suivante existante

<details>
<summary>Reponse</summary>

**c) Une nouvelle generation a creer**

Les references relatives GDG :
- (0) = Generation actuelle (la plus recente)
- (+1) = Nouvelle generation a creer
- (-1) = Generation precedente
- (-2) = Avant-derniere generation
</details>

---

### Question 20
Quelle est la difference entre EMPTY et NOEMPTY pour un GDG ?

<details>
<summary>Reponse</summary>

| Option | Comportement quand LIMIT est atteint |
|--------|--------------------------------------|
| **EMPTY** | Supprime TOUTES les generations |
| **NOEMPTY** | Supprime uniquement la plus ancienne generation |

Exemple avec LIMIT(3) :
- NOEMPTY : G0001, G0002, G0003 + G0004 = G0002, G0003, G0004
- EMPTY : G0001, G0002, G0003 + G0004 = G0004 uniquement
</details>

---

### Question 21
Quel code retour VSAM indique "fin de fichier" ?

- [ ] a) 00
- [ ] b) 10
- [ ] c) 20
- [ ] d) 99

<details>
<summary>Reponse</summary>

**b) 10**

Le code 10 indique que la fin de fichier (EOF) a ete atteinte lors d'une lecture sequentielle.
</details>

---

### Question 22
Quel code retour VSAM indique "cle en double" ?

- [ ] a) 20
- [ ] b) 21
- [ ] c) 22
- [ ] d) 23

<details>
<summary>Reponse</summary>

**c) 22**

Le code 22 indique qu'une tentative d'ecriture a ete faite avec une cle primaire qui existe deja dans le fichier.
</details>

---

### Question 23
Quel code retour VSAM indique "enregistrement non trouve" ?

- [ ] a) 20
- [ ] b) 21
- [ ] c) 22
- [ ] d) 23

<details>
<summary>Reponse</summary>

**d) 23**

Le code 23 indique qu'un enregistrement ou fichier n'a pas ete trouve lors d'une operation de lecture ou d'acces.
</details>

---

### Question 24
Quelle commande construit les entrees d'un AIX ?

- [ ] a) BUILDINDEX
- [ ] b) BLDINDEX
- [ ] c) CREATEINDEX
- [ ] d) MAKEINDEX

<details>
<summary>Reponse</summary>

**b) BLDINDEX**

BLDINDEX lit le cluster de base et construit les entrees dans l'AIX :
```jcl
BLDINDEX -
  INDATASET(FTEST.KSDS) -
  OUTDATASET(FTEST.AIX.KSDS) -
  NOSORTCALL
```
</details>

---

### Question 25
Comment reinitialiser MAXCC a 0 dans IDCAMS ?

<details>
<summary>Reponse</summary>

Avec la commande SET :
```jcl
IF LASTCC <= 8 THEN -
  SET MAXCC = 0
```

Ceci est utile pour ignorer des erreurs attendues (comme un DELETE qui echoue parce que le fichier n'existe pas).
</details>

---

### Question 26
Quelle est la syntaxe pour definir un KSDS ?

<details>
<summary>Reponse</summary>

```jcl
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
```

Elements cles :
- INDEXED = type KSDS
- KEYS(longueur offset) = definition de la cle
- DATA et INDEX = composants
</details>

---

### Question 27
Que signifie KEYS(15 0) dans DEFINE CLUSTER ?

- [ ] a) 15 cles, offset 0
- [ ] b) Cle de 15 octets a la position 0
- [ ] c) 15 cles maximum, 0 actuellement
- [ ] d) Version de cle 15.0

<details>
<summary>Reponse</summary>

**b) Cle de 15 octets a la position 0**

KEYS(length offset) :
- length = longueur de la cle en octets (ici 15)
- offset = position du debut de la cle dans l'enregistrement (ici 0, debut)
</details>

---

### Question 28
Que signifie FREESPACE(10 10) ?

- [ ] a) 10 Ko d'espace libre
- [ ] b) 10% espace libre CI, 10% espace libre CA
- [ ] c) 10 CI libres, 10 CA libres
- [ ] d) Minimum 10, maximum 10

<details>
<summary>Reponse</summary>

**b) 10% espace libre CI, 10% espace libre CA**

FREESPACE(CI-percent CA-percent) :
- Premier parametre = % d'espace libre a garder dans chaque CI
- Deuxieme parametre = % de CI libres a garder dans chaque CA

Permet les insertions futures sans splits.
</details>

---

### Question 29
Quelle option LISTCAT affiche toutes les informations ?

- [ ] a) FULL
- [ ] b) COMPLETE
- [ ] c) ALL
- [ ] d) VERBOSE

<details>
<summary>Reponse</summary>

**c) ALL**

Les niveaux de detail LISTCAT :
- NAME = nom uniquement
- HISTORY = + dates, proprietaire
- VOLUME = + volumes
- ALLOCATION = + espace
- ALL = toutes les informations
</details>

---

### Question 30
Comment supprimer un GDG avec toutes ses generations ?

<details>
<summary>Reponse</summary>

Avec l'option FORCE :
```jcl
DELETE (TESTGDG.PAIE.MENSUEL) GDG FORCE
```

Sans FORCE, seul l'index GDG est supprime et les generations restent cataloguees.

Avec FORCE, l'index ET toutes les generations sont supprimes.
</details>

---

## Resume

| Commande | Usage |
|----------|-------|
| **DEFINE CLUSTER** | Creer un cluster VSAM |
| **DEFINE AIX** | Creer un index alternatif |
| **DEFINE PATH** | Lier AIX au cluster |
| **DEFINE GDG** | Creer un groupe de generations |
| **BLDINDEX** | Construire un AIX |
| **LISTCAT** | Lister le catalogue |
| **REPRO** | Copier des donnees |
| **ALTER** | Modifier un Data Set |
| **DELETE** | Supprimer un Data Set |
| **VERIFY** | Verifier/reparer |
| **PRINT** | Imprimer le contenu |
| **EXPORT/IMPORT** | Sauvegarde/restauration |

---
*Formation VSAM - M2i Formation*
