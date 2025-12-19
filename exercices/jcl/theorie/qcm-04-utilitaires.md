# QCM 04 - Utilitaires et SORT

## Chapitre IV - IEFBR14, IEBGENER, IEBCOPY, IEBCOMPR, IDCAMS et SORT

---

## Partie 1 : Utilitaires de base

### Question 1
Que fait le programme IEFBR14 ?

- [ ] a) Copie des fichiers
- [ ] b) Rien (retourne immediatement)
- [ ] c) Trie des donnees
- [ ] d) Compare des fichiers

<details>
<summary>Reponse</summary>

**b) Rien (retourne immediatement)**

IEFBR14 est le programme le plus simple de z/OS. Son code : `BR 14` (Branch to Register 14 = return).

L'interet est que les cartes DD associees sont traitees par JES, permettant de creer ou supprimer des datasets sans programme reel.
</details>

---

### Question 2
Quel utilitaire utiliser pour creer un dataset vide ?

- [ ] a) IEBGENER
- [ ] b) IEBCOPY
- [ ] c) IEFBR14
- [ ] d) IDCAMS

<details>
<summary>Reponse</summary>

**c) IEFBR14**

```jcl
//CREATE  EXEC PGM=IEFBR14
//NEWFILE DD DSN=MY.NEW.FILE,
//           DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(10,5)),
//           DCB=(RECFM=FB,LRECL=80)
```

Le fichier est cree par le traitement de la carte DD, pas par le programme.
</details>

---

### Question 3
Comment supprimer un dataset avec IEFBR14 ?

<details>
<summary>Reponse</summary>

**Avec DISP=(OLD,DELETE,DELETE) :**

```jcl
//DELETE  EXEC PGM=IEFBR14
//DELFILE DD DSN=MY.OLD.FILE,
//           DISP=(OLD,DELETE,DELETE)
```

- OLD : Le fichier doit exister
- DELETE : Supprime a la fin du step (normal et abnormal)

Note : Provoque une erreur si le fichier n'existe pas.
</details>

---

### Question 4
Quel est le role principal de IEBGENER ?

- [ ] a) Generer du code
- [ ] b) Copier des fichiers sequentiels
- [ ] c) Compresser des PDS
- [ ] d) Creer des fichiers VSAM

<details>
<summary>Reponse</summary>

**b) Copier des fichiers sequentiels**

IEBGENER copie des fichiers sequentiels ou des membres de PDS. C'est l'utilitaire de copie le plus utilise pour les fichiers non-VSAM.
</details>

---

### Question 5
Quelles sont les DDs obligatoires pour IEBGENER ?

<details>
<summary>Reponse</summary>

**4 DDs requises :**

| DDNAME | Role |
|--------|------|
| **SYSUT1** | Fichier source (entree) |
| **SYSUT2** | Fichier destination (sortie) |
| **SYSPRINT** | Messages de l'utilitaire |
| **SYSIN** | Cartes de controle (ou DUMMY) |

```jcl
//COPY    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=INPUT.FILE,DISP=SHR
//SYSUT2   DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),...
```
</details>

---

### Question 6
A quoi sert SYSIN DD DUMMY avec IEBGENER ?

- [ ] a) Ignorer le fichier d'entree
- [ ] b) Copie simple sans transformation
- [ ] c) Supprimer le fichier de sortie
- [ ] d) Mode debug

<details>
<summary>Reponse</summary>

**b) Copie simple sans transformation**

Avec SYSIN DD DUMMY, IEBGENER effectue une copie directe sans cartes de controle (pas de reformatage, pas de selection).

Si SYSIN contient des cartes (GENERATE, RECORD), des transformations peuvent etre appliquees.
</details>

---

### Question 7
Quel utilitaire gere les PDS (copie, compression) ?

- [ ] a) IEBGENER
- [ ] b) IEBCOPY
- [ ] c) IEBCOMPR
- [ ] d) IEBPTPCH

<details>
<summary>Reponse</summary>

**b) IEBCOPY**

IEBCOPY est l'utilitaire principal pour les PDS :
- Copier des membres entre PDS
- Compresser un PDS (recuperer l'espace)
- UNLOAD (decharger vers sequentiel)
- LOAD (recharger depuis sequentiel)
</details>

---

### Question 8
Comment copier tous les membres d'un PDS vers un autre ?

<details>
<summary>Reponse</summary>

**Avec IEBCOPY et COPY :**

```jcl
//COPYPDS EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//INPDS   DD DSN=SOURCE.PDS,DISP=SHR
//OUTPDS  DD DSN=TARGET.PDS,DISP=SHR
//SYSIN   DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
/*
```

Sans SELECT ni EXCLUDE, tous les membres sont copies.
</details>

---

### Question 9
Comment compresser un PDS avec IEBCOPY ?

<details>
<summary>Reponse</summary>

**Copie in-place (meme PDS source et cible) :**

```jcl
//COMPRESS EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//MYPDS   DD DSN=MY.PDS.LIBRARY,DISP=OLD
//SYSIN   DD *
  COPY OUTDD=MYPDS,INDD=MYPDS
/*
```

- DISP=OLD obligatoire (acces exclusif)
- Recupere l'espace des membres supprimes
- Les membres sont reorganises en debut de fichier
</details>

---

### Question 10
Comment selectionner certains membres a copier ?

<details>
<summary>Reponse</summary>

**Avec SELECT ou EXCLUDE :**

```jcl
//SYSIN DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  SELECT MEMBER=(MBR1,MBR2,MBR3)
/*
```

ou pour exclure :

```jcl
//SYSIN DD *
  COPY OUTDD=OUTPDS,INDD=INPDS
  EXCLUDE MEMBER=(TESTPGM,OLDPGM)
/*
```

SELECT et EXCLUDE sont mutuellement exclusifs.
</details>

---

### Question 11
Quel utilitaire compare deux fichiers ?

- [ ] a) IEBGENER
- [ ] b) IEBCOPY
- [ ] c) IEBCOMPR
- [ ] d) IDCAMS

<details>
<summary>Reponse</summary>

**c) IEBCOMPR**

IEBCOMPR compare deux fichiers sequentiels ou deux PDS enregistrement par enregistrement.

Code retour :
- RC=0 : Fichiers identiques
- RC=8 : Differences trouvees
</details>

---

### Question 12
Quelle carte de controle specifie le type d'organisation pour IEBCOMPR ?

<details>
<summary>Reponse</summary>

**COMPARE TYPORG= :**

```jcl
//COMPARE EXEC PGM=IEBCOMPR
//SYSPRINT DD SYSOUT=*
//SYSUT1  DD DSN=FILE1,DISP=SHR
//SYSUT2  DD DSN=FILE2,DISP=SHR
//SYSIN   DD *
  COMPARE TYPORG=PS
/*
```

- `TYPORG=PS` : Fichiers sequentiels
- `TYPORG=PO` : Fichiers partitionnes (PDS)
</details>

---

## Partie 2 : IDCAMS

### Question 13
Que signifie IDCAMS ?

- [ ] a) Integrated Data Cluster Access Method Services
- [ ] b) IBM Data Control And Management System
- [ ] c) Index Data Catalog Access Method Services
- [ ] d) Integrated Disk Catalog Management

<details>
<summary>Reponse</summary>

**a) Integrated Data Cluster Access Method Services**

IDCAMS est l'utilitaire le plus puissant pour la gestion des fichiers VSAM et du catalogue z/OS.
</details>

---

### Question 14
Quelles sont les commandes principales d'IDCAMS ?

<details>
<summary>Reponse</summary>

**Commandes IDCAMS principales :**

| Commande | Fonction |
|----------|----------|
| **DEFINE CLUSTER** | Creer un fichier VSAM |
| **DELETE** | Supprimer fichiers/entrees catalogue |
| **REPRO** | Copier fichiers (VSAM et non-VSAM) |
| **PRINT** | Afficher le contenu d'un fichier |
| **LISTCAT** | Lister les entrees du catalogue |
| **ALTER** | Modifier les attributs |
| **VERIFY** | Verifier/reparer un fichier VSAM |
</details>

---

### Question 15
Comment supprimer un fichier avec IDCAMS sans erreur s'il n'existe pas ?

<details>
<summary>Reponse</summary>

**Avec IF LASTCC = 8 THEN SET MAXCC = 0 :**

```jcl
//DELETE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN   DD *
  DELETE MY.FILE.DATA
  IF LASTCC = 8 THEN SET MAXCC = 0
/*
```

- Si le fichier n'existe pas, DELETE retourne RC=8
- SET MAXCC = 0 remet le code retour a 0
- Le job continue sans erreur
</details>

---

### Question 16
Quels sont les trois types principaux de fichiers VSAM ?

<details>
<summary>Reponse</summary>

**Types VSAM :**

| Type | Description | Acces | Usage |
|------|-------------|-------|-------|
| **KSDS** | Key Sequenced | Cle + Sequentiel | Fichiers maitre, index |
| **ESDS** | Entry Sequenced | Sequentiel | Logs, journaux |
| **RRDS** | Relative Record | Numero relatif | Acces direct par position |

Plus LDS (Linear Data Set) pour les donnees binaires (DB2).
</details>

---

### Question 17
Quelle commande IDCAMS cree un fichier VSAM KSDS ?

<details>
<summary>Reponse</summary>

**DEFINE CLUSTER avec INDEXED :**

```jcl
//DEFINE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN   DD *
  DEFINE CLUSTER (                    -
           NAME(MY.VSAM.KSDS)         -
           INDEXED                    -
           KEYS(8 0)                  -
           RECORDSIZE(100 100)        -
           TRACKS(10 5)               -
         )                            -
         DATA (NAME(MY.VSAM.KSDS.DATA)) -
         INDEX (NAME(MY.VSAM.KSDS.INDEX))
/*
```

- INDEXED = KSDS
- NONINDEXED = ESDS
- NUMBERED = RRDS
</details>

---

### Question 18
Que signifie KEYS(8 0) dans DEFINE CLUSTER ?

- [ ] a) 8 cles de 0 octet
- [ ] b) Cle de 8 octets en position 0
- [ ] c) 8 enregistrements avec cle
- [ ] d) Cle optionnelle de 8 octets

<details>
<summary>Reponse</summary>

**b) Cle de 8 octets en position 0**

KEYS(longueur offset) :
- Premier parametre : longueur de la cle (8 octets)
- Deuxieme parametre : position de debut (0 = debut de l'enregistrement)
</details>

---

### Question 19
Quelle commande IDCAMS copie des donnees ?

- [ ] a) COPY
- [ ] b) REPRO
- [ ] c) MOVE
- [ ] d) TRANSFER

<details>
<summary>Reponse</summary>

**b) REPRO**

REPRO copie des fichiers (VSAM ou non-VSAM) :

```jcl
//LOAD    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE  DD DSN=SEQ.INPUT,DISP=SHR
//OUTFILE DD DSN=VSAM.KSDS,DISP=SHR
//SYSIN   DD *
  REPRO INFILE(INFILE) OUTFILE(OUTFILE)
/*
```

Conversions possibles : PS→VSAM, VSAM→PS, VSAM→VSAM
</details>

---

### Question 20
Comment lister le contenu du catalogue ?

<details>
<summary>Reponse</summary>

**Avec LISTCAT :**

```jcl
//LISTCAT EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN   DD *
  LISTCAT ENTRIES(USER01.*) ALL
/*
```

Options :
- `ENTRIES(mask)` : Filtre par pattern
- `ALL` : Toutes les informations
- `NAME` : Noms uniquement
- `VOLUME` : Informations volume
</details>

---

## Partie 3 : SORT

### Question 21
Quelles sont les DDs principales pour SORT ?

<details>
<summary>Reponse</summary>

**DDs SORT :**

| DDNAME | Role |
|--------|------|
| **SORTIN** | Fichier d'entree |
| **SORTOUT** | Fichier de sortie |
| **SYSOUT** | Messages SORT |
| **SYSIN** | Cartes de controle SORT |
| **SORTWKnn** | Fichiers de travail (optionnel) |

```jcl
//SORT    EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN  DD DSN=INPUT.FILE,DISP=SHR
//SORTOUT DD DSN=OUTPUT.FILE,DISP=(NEW,CATLG),...
//SYSIN   DD *
  SORT FIELDS=(1,10,CH,A)
/*
```
</details>

---

### Question 22
Quelle est la syntaxe de SORT FIELDS ?

<details>
<summary>Reponse</summary>

**SORT FIELDS=(position,longueur,format,ordre,...)**

```jcl
SORT FIELDS=(1,10,CH,A)
```

- **position** : Position de debut de la cle (1-based)
- **longueur** : Nombre d'octets
- **format** : Type de donnees (CH, ZD, PD, BI, FI)
- **ordre** : A (Ascendant) ou D (Descendant)

Exemple multi-cles :
```jcl
SORT FIELDS=(1,5,CH,A,10,8,PD,D)
```
</details>

---

### Question 23
Que signifient les formats CH, ZD et PD ?

<details>
<summary>Reponse</summary>

**Formats de donnees SORT :**

| Format | Description | Usage |
|--------|-------------|-------|
| **CH** | Character (EBCDIC) | Texte, codes |
| **ZD** | Zoned Decimal | Numerique zone (DISPLAY) |
| **PD** | Packed Decimal | Numerique compacte (COMP-3) |
| **BI** | Binary | Binaire non signe |
| **FI** | Fixed Integer | Binaire signe |
| **AC** | ASCII Character | Texte ASCII |
</details>

---

### Question 24
Comment copier un fichier sans le trier ?

- [ ] a) SORT FIELDS=NONE
- [ ] b) SORT FIELDS=COPY
- [ ] c) COPY FIELDS=ALL
- [ ] d) SORT FIELDS=(1,1,CH,A)

<details>
<summary>Reponse</summary>

**b) SORT FIELDS=COPY**

```jcl
//SYSIN DD *
  SORT FIELDS=COPY
/*
```

Copie tous les enregistrements sans tri. Utile avec INCLUDE/OMIT pour filtrer ou OUTREC pour reformater.
</details>

---

### Question 25
Quelle est la difference entre INCLUDE et OMIT ?

<details>
<summary>Reponse</summary>

| Instruction | Fonction |
|-------------|----------|
| **INCLUDE** | Ne garde QUE les enregistrements qui correspondent |
| **OMIT** | Exclut les enregistrements qui correspondent |

```jcl
* Garder uniquement Paris (code 75)
INCLUDE COND=(40,2,CH,EQ,C'75')

* Exclure Paris
OMIT COND=(40,2,CH,EQ,C'75')
```

INCLUDE et OMIT sont mutuellement exclusifs.
</details>

---

### Question 26
Quels sont les operateurs de comparaison pour INCLUDE/OMIT ?

<details>
<summary>Reponse</summary>

**Operateurs relationnels :**

| Operateur | Signification |
|-----------|---------------|
| **EQ** | Egal (=) |
| **NE** | Different (≠) |
| **GT** | Superieur (>) |
| **GE** | Superieur ou egal (>=) |
| **LT** | Inferieur (<) |
| **LE** | Inferieur ou egal (<=) |

**Operateurs logiques :**
- **AND** (ou &) : ET logique
- **OR** (ou |) : OU logique

```jcl
INCLUDE COND=((40,2,CH,EQ,C'75',OR,40,2,CH,EQ,C'69'),
              AND,(50,5,PD,GT,+1000))
```
</details>

---

### Question 27
Quelle est la difference entre INREC et OUTREC ?

<details>
<summary>Reponse</summary>

| Instruction | Moment | Usage |
|-------------|--------|-------|
| **INREC** | AVANT le tri | Reduire la taille, meilleure performance |
| **OUTREC** | APRES le tri | Format final de sortie |

```jcl
* INREC : Extraire avant tri (plus rapide)
INREC FIELDS=(1,10,25,15)
SORT FIELDS=(1,10,CH,A)

* OUTREC : Formater apres tri
SORT FIELDS=(1,10,CH,A)
OUTREC FIELDS=(1,10,15,20,C'TRAITE')
```

Utiliser INREC quand possible pour optimiser les performances.
</details>

---

### Question 28
Comment ajouter une constante avec OUTREC ?

<details>
<summary>Reponse</summary>

**Avec C'texte' :**

```jcl
OUTREC FIELDS=(1,50,C'NOUVEAU',60,20)
```

- `1,50` : Positions 1-50 de l'entree
- `C'NOUVEAU'` : Constante caractere
- `60,20` : Positions 60-79 de l'entree

Autres types de constantes :
- `X'0D25'` : Hexadecimal
- `+100` : Numerique
- `Z'0000'` : Zone decimal
</details>

---

### Question 29
Que fait SUM FIELDS=NONE ?

- [ ] a) Additionne tous les champs
- [ ] b) Supprime les doublons (garde le premier)
- [ ] c) Ne fait rien
- [ ] d) Compte les enregistrements

<details>
<summary>Reponse</summary>

**b) Supprime les doublons (garde le premier)**

```jcl
SORT FIELDS=(1,10,CH,A)
SUM FIELDS=NONE
```

Si deux enregistrements ont la meme cle de tri, seul le premier est garde. Utile pour eliminer les doublons.

Avec SUM FIELDS=(pos,len,format), les valeurs sont additionnees au lieu de supprimer.
</details>

---

### Question 30
Comment creer plusieurs fichiers de sortie avec SORT ?

<details>
<summary>Reponse</summary>

**Avec OUTFIL :**

```jcl
//PARIS  DD DSN=DATA.PARIS,...
//LYON   DD DSN=DATA.LYON,...
//AUTRES DD DSN=DATA.AUTRES,...
//SYSIN  DD *
  SORT FIELDS=(1,10,CH,A)
  OUTFIL FNAMES=PARIS,INCLUDE=(40,2,CH,EQ,C'75')
  OUTFIL FNAMES=LYON,INCLUDE=(40,2,CH,EQ,C'69')
  OUTFIL FNAMES=AUTRES,SAVE
/*
```

- Chaque OUTFIL definit un fichier de sortie
- SAVE dirige les enregistrements non selectionnes
- FNAMES reference le DDNAME
</details>

---

### Question 31
Que fait OUTREC FINDREP ?

<details>
<summary>Reponse</summary>

**Recherche et remplacement de chaines :**

```jcl
SORT FIELDS=COPY
OUTREC FINDREP=(IN=C'ANCIEN',OUT=C'NOUVEAU')
```

Options :
- `IN=C'texte'` : Chaine a rechercher
- `OUT=C'texte'` : Chaine de remplacement
- Peut avoir plusieurs paires IN/OUT

```jcl
OUTREC FINDREP=(IN=C'OUI',OUT=C'NON',
               IN=C'YES',OUT=C'NO')
```
</details>

---

### Question 32
Comment fusionner plusieurs fichiers deja tries ?

<details>
<summary>Reponse</summary>

**Avec MERGE :**

```jcl
//SORT    EXEC PGM=SORT
//SYSOUT  DD SYSOUT=*
//SORTIN01 DD DSN=FILE1.SORTED,DISP=SHR
//SORTIN02 DD DSN=FILE2.SORTED,DISP=SHR
//SORTIN03 DD DSN=FILE3.SORTED,DISP=SHR
//SORTOUT DD DSN=MERGED.FILE,...
//SYSIN   DD *
  MERGE FIELDS=(1,10,CH,A)
/*
```

- Les fichiers d'entree DOIVENT etre deja tries
- Plus rapide que SORT (pas de tri, juste fusion)
- DDNAMEs : SORTINnn (01, 02, 03, ...)
</details>

---

## Partie 4 : Codes retour

### Question 33
Quel code retour indique un succes ?

- [ ] a) RC=1
- [ ] b) RC=0
- [ ] c) RC=4
- [ ] d) RC=-1

<details>
<summary>Reponse</summary>

**b) RC=0**

Codes retour standards :
- **0** : Succes
- **4** : Warning
- **8** : Erreur
- **12** : Erreur grave
- **16** : Erreur critique
</details>

---

### Question 34
Que signifie le code ABEND S0C7 ?

<details>
<summary>Reponse</summary>

**Donnees non numeriques dans un champ numerique**

S0C7 (Data Exception) se produit quand :
- Un MOVE vers un champ numerique contient des caracteres invalides
- Une operation arithmetique sur des donnees non numeriques
- Un champ COMP-3 mal initialise

Diagnostic :
- Verifier les donnees d'entree
- Verifier les INITIALIZE
- Consulter CEEDUMP pour la variable en cause
</details>

---

### Question 35
Que signifie le code ABEND S806 ?

<details>
<summary>Reponse</summary>

**Module (programme) introuvable**

S806 se produit quand :
- Le programme PGM= n'existe pas dans les bibliotheques
- JOBLIB/STEPLIB mal configure
- Nom de programme mal orthographie

Solutions :
- Verifier le nom du programme
- Verifier STEPLIB/JOBLIB
- Verifier que le programme est compile et linkedite
</details>

---

## Resume

| Utilitaire | Fonction principale |
|------------|---------------------|
| **IEFBR14** | Creer/supprimer via DD (aucun traitement) |
| **IEBGENER** | Copier fichiers sequentiels |
| **IEBCOPY** | Gerer PDS (copie, compression) |
| **IEBCOMPR** | Comparer fichiers |
| **IDCAMS** | Gestion VSAM et catalogue |
| **SORT** | Trier, filtrer, reformater |

| Commande SORT | Fonction |
|---------------|----------|
| **SORT FIELDS** | Definir les cles de tri |
| **SORT FIELDS=COPY** | Copier sans tri |
| **INCLUDE/OMIT** | Filtrer les enregistrements |
| **INREC/OUTREC** | Reformater |
| **SUM FIELDS=NONE** | Supprimer doublons |
| **OUTFIL** | Multiples sorties |
| **MERGE** | Fusionner fichiers tries |
| **FINDREP** | Rechercher/remplacer |

---
*Formation z/OS - M2i Formation*
