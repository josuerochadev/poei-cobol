# QCM 02 - Fichiers et Parametres Avances

## Chapitre II - Gestion des fichiers, concatenation et references

---

### Question 1
Quels sont les deux types principaux d'organisation de fichiers non-VSAM ?

- [ ] a) ISAM et BDAM
- [ ] b) PS et PO
- [ ] c) KSDS et ESDS
- [ ] d) SEQ et PART

<details>
<summary>Reponse</summary>

**b) PS et PO**

- **PS** (Physical Sequential) : Fichier sequentiel
- **PO** (Partitioned Organization) : PDS/PDSE (bibliotheque avec membres)
</details>

---

### Question 2
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

### Question 3
Combien de qualificateurs maximum peut avoir un nom de dataset ?

- [ ] a) 8
- [ ] b) 22
- [ ] c) 44
- [ ] d) Illimite

<details>
<summary>Reponse</summary>

**b) 22**

Avec 44 caracteres maximum au total et des qualificateurs de 1-8 caracteres separes par des points, on peut avoir au maximum 22 qualificateurs (cas extreme de qualificateurs d'un seul caractere).

En pratique, 3 a 5 qualificateurs sont courants :
`PROD.BATCH.DATA.CLIENT` (4 qualificateurs)
</details>

---

### Question 4
Qu'est-ce que le HLQ (High Level Qualifier) ?

<details>
<summary>Reponse</summary>

**Le HLQ est le premier qualificateur du nom de dataset.**

Exemple : Dans `USER01.COBOL.SOURCE`, le HLQ est `USER01`.

Le HLQ est souvent :
- L'userid du proprietaire
- Un identifiant d'application (PROD, TEST)
- Un identifiant de systeme

Le HLQ determine souvent :
- Les droits RACF
- Le catalogue utilisateur
- Le Storage Group SMS
</details>

---

### Question 5
Que signifie DISP=(,PASS) ?

- [ ] a) Passer le fichier au job suivant
- [ ] b) Passer le fichier au step suivant du meme job
- [ ] c) Ignorer le fichier
- [ ] d) Passer en mode lecture

<details>
<summary>Reponse</summary>

**b) Passer le fichier au step suivant du meme job**

`DISP=(NEW,PASS)` ou `DISP=(,PASS)` :
- Le fichier reste disponible pour les steps suivants
- A la fin du job, il est supprime (sauf si le dernier step le catalogue)
- Utilise principalement pour les fichiers temporaires
</details>

---

### Question 6
Que se passe-t-il si DISP n'est pas specifie pour un nouveau fichier ?

<details>
<summary>Reponse</summary>

**La valeur par defaut est DISP=(NEW,DELETE,DELETE)**

Si vous oubliez DISP lors de la creation :
- Le fichier est cree (NEW)
- Il est supprime a la fin du step (DELETE en normal)
- Il est supprime en cas d'erreur (DELETE en abnormal)

C'est pourquoi il faut toujours specifier DISP explicitement pour les fichiers permanents !
</details>

---

### Question 7
Quelle disposition utiliser pour ajouter des enregistrements a un fichier existant ?

- [ ] a) DISP=OLD
- [ ] b) DISP=SHR
- [ ] c) DISP=MOD
- [ ] d) DISP=NEW

<details>
<summary>Reponse</summary>

**c) DISP=MOD**

MOD (Modify) :
- Si le fichier existe : positionne apres le dernier enregistrement
- Si le fichier n'existe pas : le cree
- Utilise pour les fichiers de log, cumuls, ajouts

```jcl
//LOGFILE DD DSN=MY.LOG.FILE,
//           DISP=(MOD,CATLG,CATLG)
```
</details>

---

### Question 8
Combien d'extensions secondaires maximum un fichier peut-il avoir ?

- [ ] a) 5
- [ ] b) 10
- [ ] c) 15
- [ ] d) 16

<details>
<summary>Reponse</summary>

**c) 15**

Un fichier peut avoir jusqu'a 15 extensions secondaires. Apres 15 extensions, une erreur SB37 (espace insuffisant) se produit meme s'il y a de l'espace sur le volume.

Conseil : Dimensionner correctement l'allocation primaire pour eviter trop d'extensions.
</details>

---

### Question 9
Quelle est la taille approximative d'une piste (TRK) sur un disque 3390 ?

- [ ] a) 4 Ko
- [ ] b) 16 Ko
- [ ] c) 56 Ko
- [ ] d) 100 Ko

<details>
<summary>Reponse</summary>

**c) 56 Ko**

Sur un disque 3390 :
- 1 piste (TRK) ≈ 56 664 octets (~56 Ko)
- 1 cylindre (CYL) = 15 pistes ≈ 849 Ko

Ces valeurs sont utiles pour calculer l'espace necessaire.
</details>

---

### Question 10
Que signifie UNIT=VIO ?

- [ ] a) Virtual Input/Output (fichier en memoire)
- [ ] b) Volume I/O
- [ ] c) Variable I/O
- [ ] d) Verified I/O

<details>
<summary>Reponse</summary>

**a) Virtual Input/Output (fichier en memoire)**

VIO stocke les fichiers temporaires en memoire virtuelle :
- Pas d'I/O disque physique
- Performance maximale
- Uniquement pour fichiers temporaires (&&nom)
- Supprime automatiquement a la fin du job

```jcl
//TEMP DD DSN=&&WORK,UNIT=VIO,SPACE=(TRK,(10,5))
```
</details>

---

### Question 11
Que specifie VOL=SER=PROD01 ?

- [ ] a) Le type de volume
- [ ] b) Le nom de serie du volume (disque specifique)
- [ ] c) Le numero de serie du programme
- [ ] d) La version du volume

<details>
<summary>Reponse</summary>

**b) Le nom de serie du volume (disque specifique)**

VOL=SER designe un volume (disque) specifique par son serial number.

```jcl
//OUTPUT DD DSN=MY.DATA,VOL=SER=PROD01,UNIT=3390,...
```

Note : Rarement necessaire avec SMS (Storage Management Subsystem) qui choisit automatiquement le volume.
</details>

---

### Question 12
Que fait DCB=*.STEP1.INPUT ?

- [ ] a) Copie les donnees du fichier INPUT
- [ ] b) Copie les caracteristiques DCB du fichier INPUT de STEP1
- [ ] c) Reference le contenu du fichier
- [ ] d) Verifie la compatibilite

<details>
<summary>Reponse</summary>

**b) Copie les caracteristiques DCB du fichier INPUT de STEP1**

La reference DCB=*.step.dd copie automatiquement RECFM, LRECL, BLKSIZE du fichier reference.

```jcl
//STEP1  EXEC PGM=PROG1
//INPUT  DD DSN=DATA.SOURCE,DISP=SHR
//STEP2  EXEC PGM=PROG2
//OUTPUT DD DSN=DATA.COPY,DCB=*.STEP1.INPUT,...
```

Evite de repeter les caracteristiques et assure la coherence.
</details>

---

### Question 13
Dans une concatenation, quel fichier doit avoir le plus grand BLKSIZE ?

- [ ] a) Le dernier
- [ ] b) Le premier
- [ ] c) N'importe lequel
- [ ] d) Celui du milieu

<details>
<summary>Reponse</summary>

**b) Le premier**

Le systeme alloue un buffer base sur le premier fichier. Si un fichier suivant a un BLKSIZE plus grand, des erreurs peuvent survenir.

```jcl
//INPUT DD DSN=FILE.BIG,DISP=SHR    <- BLKSIZE=27920
//      DD DSN=FILE.SMALL,DISP=SHR  <- BLKSIZE=8000
```

Mettre le fichier avec le plus grand BLKSIZE en premier.
</details>

---

### Question 14
Combien de fichiers maximum peut-on concatener ?

<details>
<summary>Reponse</summary>

**Limites de concatenation :**

- **255** fichiers sequentiels (PS) maximum
- **16** PDS maximum

Regles :
- Meme LRECL et RECFM
- BLKSIZE : le plus grand en premier
- Pas de melange PS et PDS
- Le DDNAME n'apparait que sur la premiere DD
</details>

---

### Question 15
Comment ecrire un commentaire en JCL ?

- [ ] a) /* commentaire */
- [ ] b) // commentaire
- [ ] c) //* commentaire
- [ ] d) * commentaire

<details>
<summary>Reponse</summary>

**c) //* commentaire**

```jcl
//*----------------------------------------
//* Ceci est un commentaire JCL
//*----------------------------------------
//STEP1 EXEC PGM=MYPROG
```

- `//*` en colonnes 1-3
- Le reste de la ligne est ignore
- Utile pour documenter le JCL
</details>

---

### Question 16
Que signifie le symbole && dans un nom de dataset ?

- [ ] a) Concatenation
- [ ] b) Fichier temporaire
- [ ] c) Variable symbolique
- [ ] d) Reference arriere

<details>
<summary>Reponse</summary>

**b) Fichier temporaire**

`&&` indique un fichier temporaire :
- Cree pendant le job
- Supprime automatiquement a la fin du job
- Peut etre passe entre steps (PASS)

```jcl
//WORK DD DSN=&&TEMPFILE,DISP=(NEW,PASS),...
```

Note : Un seul `&` indique une variable symbolique.
</details>

---

### Question 17
Comment passer un fichier temporaire d'un step a un autre ?

<details>
<summary>Reponse</summary>

**Avec DISP=(NEW,PASS) puis reference arriere :**

```jcl
//STEP1  EXEC PGM=PROG1
//TEMP   DD DSN=&&WORK,
//          DISP=(NEW,PASS),
//          SPACE=(TRK,(10,5))
//*
//STEP2  EXEC PGM=PROG2
//INPUT  DD DSN=*.STEP1.TEMP,
//          DISP=(OLD,DELETE)
```

- PASS garde le fichier pour les steps suivants
- Reference par `*.stepname.ddname`
- DELETE sur le dernier step qui l'utilise
</details>

---

### Question 18
Que fait l'option CONTIG dans SPACE ?

- [ ] a) Compresse les donnees
- [ ] b) Exige un espace disque contigu
- [ ] c) Concatene les fichiers
- [ ] d) Controle l'allocation

<details>
<summary>Reponse</summary>

**b) Exige un espace disque contigu**

`SPACE=(CYL,(10,5),CONTIG)` demande que les 10 cylindres primaires soient contigus sur le disque.

Variantes :
- **CONTIG** : Espace contigu obligatoire
- **MXIG** : Plus grande zone contigue disponible
- **ALX** : Jusqu'a 5 zones separees

Utile pour les fichiers a acces direct ou haute performance.
</details>

---

### Question 19
Quelle DD est utilisee pour les dumps en cas d'ABEND ?

- [ ] a) SYSPRINT
- [ ] b) SYSUDUMP
- [ ] c) SYSOUT
- [ ] d) SYSERR

<details>
<summary>Reponse</summary>

**b) SYSUDUMP**

DDs de diagnostic :
- **SYSUDUMP** : Dump utilisateur (Working Storage, registres)
- **SYSABEND** : Dump complet (inclut zones systeme)
- **SYSMDUMP** : Dump machine (pour IPCS)
- **CEEDUMP** : Dump Language Environment (COBOL)

```jcl
//STEP1    EXEC PGM=MYPROG
//SYSUDUMP DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
```
</details>

---

### Question 20
Comment specifier un delimiteur personnalise pour les donnees in-stream ?

<details>
<summary>Reponse</summary>

**Avec le parametre DLM= :**

```jcl
//SYSIN DD *,DLM='@@'
SELECT * FROM TABLE
WHERE COL = 'VALUE'
@@
```

- DLM='xx' definit un delimiteur de 2 caracteres
- Utile quand les donnees contiennent `/*`
- Le delimiteur doit etre en colonnes 1-2
</details>

---

### Question 21
Que fait OUTLIM=10000 sur une DD SYSOUT ?

- [ ] a) Limite a 10000 octets
- [ ] b) Limite a 10000 lignes
- [ ] c) Limite a 10000 pages
- [ ] d) Limite a 10000 enregistrements

<details>
<summary>Reponse</summary>

**b) Limite a 10000 lignes**

OUTLIM protege contre les impressions infinies :
```jcl
//SYSPRINT DD SYSOUT=*,OUTLIM=10000
```

Si la limite est atteinte, le job fait un ABEND S722.
</details>

---

### Question 22
Que signifie LIKE dans une allocation ?

<details>
<summary>Reponse</summary>

**LIKE copie les attributs d'un dataset existant :**

```jcl
//NEWFILE DD DSN=NEW.DATA.FILE,
//           LIKE=OLD.DATA.FILE,
//           DISP=(NEW,CATLG)
```

Attributs copies :
- RECFM, LRECL, BLKSIZE
- DSORG
- SPACE (peut etre surcharge)

Alternative plus explicite a DCB=*.step.dd
</details>

---

### Question 23
Quelle est la difference entre DD DATA et DD * ?

<details>
<summary>Reponse</summary>

| Aspect | DD * | DD DATA |
|--------|------|---------|
| **// dans donnees** | Interdit | Autorise |
| **Usage** | Donnees simples | JCL comme donnees |

```jcl
//SYSIN DD *
DONNEES NORMALES
/*

//CARTES DD DATA
//JOB1 JOB ...
//STEP EXEC PGM=X
/*
```

DD DATA permet d'inclure du JCL comme donnees (ex: JCL a analyser).
</details>

---

### Question 24
Comment referencer un fichier catalogue sans connaitre son volume ?

- [ ] a) VOL=SER=??????
- [ ] b) Ne pas specifier VOL
- [ ] c) VOL=CATALOG
- [ ] d) VOL=AUTO

<details>
<summary>Reponse</summary>

**b) Ne pas specifier VOL**

Pour un fichier catalogue, il suffit de specifier DSN et DISP :

```jcl
//INPUT DD DSN=PROD.DATA.FILE,DISP=SHR
```

Le systeme trouve automatiquement le volume via le catalogue. C'est le mode standard avec SMS.
</details>

---

### Question 25
Que sont les DDNAMEs reserves du systeme ?

<details>
<summary>Reponse</summary>

**DDNAMEs avec signification speciale :**

| DDNAME | Usage |
|--------|-------|
| **JOBLIB** | Bibliotheque programmes (tout le job) |
| **STEPLIB** | Bibliotheque programmes (un step) |
| **SYSPRINT** | Sortie standard des messages |
| **SYSIN** | Entree standard (cartes controle) |
| **SYSOUT** | Sortie vers spool |
| **SYSUDUMP** | Dump utilisateur |
| **SYSABEND** | Dump complet |
| **SYSLMOD** | Module de sortie (linkage editor) |
| **SYSLIN** | Entree linkage editor |

Ces noms sont attendus par les utilitaires et programmes systeme.
</details>

---

## Resume

| Element | Description |
|---------|-------------|
| **PS** | Physical Sequential (fichier sequentiel) |
| **PO** | Partitioned Organization (PDS) |
| **PDSE** | PDS Extended (DSNTYPE=LIBRARY) |
| **PASS** | Garder fichier pour steps suivants |
| **MOD** | Ajouter a la fin / creer si inexistant |
| **&&nom** | Fichier temporaire |
| **VIO** | Virtual I/O (memoire) |
| **CONTIG** | Espace contigu |
| **LIKE** | Copier attributs d'un fichier existant |
| **OUTLIM** | Limite de lignes SYSOUT |
| **DLM** | Delimiteur personnalise |
| **DCB=*.step.dd** | Reference DCB |
| **Concatenation** | Max 255 PS, 16 PDS |

---
*Formation z/OS - M2i Formation*
