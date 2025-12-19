# QCM 01 - Les Cartes JCL

## Chapitre I - Instructions JOB, EXEC et DD

---

### Question 1
Que signifie JCL ?

- [ ] a) Job Command Language
- [ ] b) Job Control Language
- [ ] c) Job Communication Link
- [ ] d) Java Control Language

<details>
<summary>Reponse</summary>

**b) Job Control Language**

JCL est le langage de controle des travaux sous z/OS. Il permet de definir les ressources necessaires a l'execution d'un programme batch.
</details>

---

### Question 2
Quelles sont les trois instructions principales du JCL ?

- [ ] a) START, RUN, END
- [ ] b) BEGIN, EXECUTE, FINISH
- [ ] c) JOB, EXEC, DD
- [ ] d) INIT, PROC, TERM

<details>
<summary>Reponse</summary>

**c) JOB, EXEC, DD**

- **JOB** : Identifie le travail batch
- **EXEC** : Execute un programme ou une procedure
- **DD** : Definit les fichiers (Data Definition)
</details>

---

### Question 3
Par quoi commencent toutes les instructions JCL ?

- [ ] a) /*
- [ ] b) //
- [ ] c) **
- [ ] d) ##

<details>
<summary>Reponse</summary>

**b) //**

Toutes les instructions JCL commencent par `//` en colonnes 1-2. Les commentaires utilisent `//*` et les delimiteurs JES utilisent `/*`.
</details>

---

### Question 4
Quelle est la longueur maximale du nom d'un JOB ?

- [ ] a) 4 caracteres
- [ ] b) 8 caracteres
- [ ] c) 16 caracteres
- [ ] d) 32 caracteres

<details>
<summary>Reponse</summary>

**b) 8 caracteres**

Le nom du JOB (JOBNAME) peut avoir de 1 a 8 caracteres alphanumeriques. Le premier caractere doit etre alphabetique ou un caractere national (@, #, $).
</details>

---

### Question 5
Que specifie le parametre CLASS dans la carte JOB ?

- [ ] a) La priorite du job
- [ ] b) La classe d'execution (file d'attente)
- [ ] c) La classe de sortie
- [ ] d) Le type de programme

<details>
<summary>Reponse</summary>

**b) La classe d'execution (file d'attente)**

CLASS determine dans quelle file d'attente le job sera place et quel Initiator pourra l'executer. Les classes vont de A a Z et de 0 a 9.
</details>

---

### Question 6
Que specifie le parametre MSGCLASS ?

- [ ] a) La classe d'execution
- [ ] b) Le niveau de messages
- [ ] c) La classe de sortie des messages du job
- [ ] d) La priorite des messages

<details>
<summary>Reponse</summary>

**c) La classe de sortie des messages du job**

MSGCLASS determine ou seront diriges les messages JCL et les sorties du job (JESMSGLG, JESJCL, JESYSMSG).
</details>

---

### Question 7
Que signifie MSGLEVEL=(1,1) ?

<details>
<summary>Reponse</summary>

**MSGLEVEL=(1,1) affiche :**
- Premier parametre (1) : Toutes les instructions JCL (y compris les procedures expandees)
- Deuxieme parametre (1) : Tous les messages d'allocation (toujours)

Autres valeurs :
- (0,0) : JCL minimal, messages si ABEND
- (0,1) : JCL minimal, tous messages
- (1,0) : Tout JCL, messages si ABEND
- (2,0) : JOB seulement, messages si ABEND
- (2,1) : JOB seulement, tous messages
</details>

---

### Question 8
Quel parametre permet de notifier l'utilisateur a la fin du job ?

- [ ] a) ALERT
- [ ] b) NOTIFY
- [ ] c) MESSAGE
- [ ] d) INFORM

<details>
<summary>Reponse</summary>

**b) NOTIFY**

`NOTIFY=&SYSUID` envoie un message a l'utilisateur qui a soumis le job quand celui-ci se termine.
</details>

---

### Question 9
Que fait TYPRUN=SCAN ?

- [ ] a) Execute le job en mode debug
- [ ] b) Verifie la syntaxe JCL sans executer
- [ ] c) Scanne les fichiers d'entree
- [ ] d) Execute le job en priorite haute

<details>
<summary>Reponse</summary>

**b) Verifie la syntaxe JCL sans executer**

TYPRUN=SCAN permet de valider la syntaxe du JCL sans executer le job. Utile pour tester un JCL avant soumission reelle.

Autre valeur : TYPRUN=HOLD suspend le job jusqu'a liberation manuelle.
</details>

---

### Question 10
Dans la carte EXEC, que specifie le parametre PGM= ?

- [ ] a) Le nom de la procedure
- [ ] b) Le nom du programme a executer
- [ ] c) Le nom du parametre
- [ ] d) Le nom du step

<details>
<summary>Reponse</summary>

**b) Le nom du programme a executer**

`EXEC PGM=nom` execute le programme specifie. Alternative : `EXEC PROC=nom` ou `EXEC nom` pour executer une procedure.
</details>

---

### Question 11
A quoi sert le parametre PARM dans la carte EXEC ?

<details>
<summary>Reponse</summary>

**PARM transmet des donnees au programme execute.**

- Maximum 100 caracteres
- Accessible en COBOL via LINKAGE SECTION
- Syntaxe : `PARM='valeur'` ou `PARM=(val1,val2)`

Exemple COBOL pour recuperer PARM :
```cobol
LINKAGE SECTION.
01 PARM-DATA.
   05 PARM-LENGTH    PIC S9(4) COMP.
   05 PARM-VALUE     PIC X(100).
```
</details>

---

### Question 12
Que signifie COND=(4,LT) dans une carte EXEC ?

- [ ] a) Executer si le code retour < 4
- [ ] b) Ne pas executer si le code retour < 4
- [ ] c) Executer si le code retour > 4
- [ ] d) Ne pas executer si le code retour > 4

<details>
<summary>Reponse</summary>

**b) Ne pas executer si le code retour < 4**

La logique COND est "inversee" : si la condition est VRAIE, le step est SAUTE.
- COND=(4,LT) : Skip si RC < 4
- COND=(0,NE) : Skip si RC != 0 (execute seulement si tous RC=0)

Operateurs : GT, GE, EQ, LT, LE, NE
</details>

---

### Question 13
Que signifie COND=EVEN ?

- [ ] a) Executer seulement si tous les steps precedents ont reussi
- [ ] b) Executer meme si un step precedent a fait un ABEND
- [ ] c) Executer un step sur deux
- [ ] d) Executer seulement si le code retour est pair

<details>
<summary>Reponse</summary>

**b) Executer meme si un step precedent a fait un ABEND**

- COND=EVEN : Execute meme apres ABEND
- COND=ONLY : Execute SEULEMENT si ABEND precedent

Utile pour les steps de cleanup ou notification d'erreur.
</details>

---

### Question 14
Quelle est la signification de DD ?

- [ ] a) Data Definition
- [ ] b) Data Description
- [ ] c) Direct Data
- [ ] d) Disk Drive

<details>
<summary>Reponse</summary>

**a) Data Definition**

L'instruction DD definit les fichiers utilises par le programme : nom, emplacement, caracteristiques, disposition.
</details>

---

### Question 15
Quelle est la longueur maximale d'un nom de dataset (DSN) ?

- [ ] a) 8 caracteres
- [ ] b) 22 caracteres
- [ ] c) 44 caracteres
- [ ] d) 80 caracteres

<details>
<summary>Reponse</summary>

**c) 44 caracteres**

Le nom complet (avec les points) peut avoir jusqu'a 44 caracteres. Chaque qualificateur (segment) peut avoir de 1 a 8 caracteres.

Exemple : `PROD.BATCH.DATA.CLIENT.EXTRACT` (30 caracteres)
</details>

---

### Question 16
Que signifie DISP=(NEW,CATLG,DELETE) ?

<details>
<summary>Reponse</summary>

**DISP=(status,normal-end,abnormal-end)**

- **NEW** : Le fichier est a creer
- **CATLG** : Si le step reussit, cataloguer le fichier
- **DELETE** : Si le step echoue (ABEND), supprimer le fichier

Autres valeurs status : OLD, SHR, MOD
Autres dispositions : KEEP, PASS, UNCATLG
</details>

---

### Question 17
Quelle est la difference entre DISP=OLD et DISP=SHR ?

<details>
<summary>Reponse</summary>

| DISP | Acces | Usage |
|------|-------|-------|
| **OLD** | Exclusif | Modification, mise a jour |
| **SHR** | Partage | Lecture seule, acces concurrent |

- OLD verrouille le fichier pour le job
- SHR permet a plusieurs jobs de lire simultanement
</details>

---

### Question 18
Que fait DISP=MOD ?

- [ ] a) Modifie les attributs du fichier
- [ ] b) Cree le fichier s'il n'existe pas, sinon ajoute a la fin
- [ ] c) Mode lecture seule
- [ ] d) Mode modification atomique

<details>
<summary>Reponse</summary>

**b) Cree le fichier s'il n'existe pas, sinon ajoute a la fin**

MOD (Modify) est utile pour :
- Ajouter des enregistrements a un fichier existant
- Creer un nouveau fichier si inexistant
- Logs, fichiers de cumul
</details>

---

### Question 19
Dans DCB=(RECFM=FB,LRECL=80,BLKSIZE=0), que signifie FB ?

- [ ] a) Fixed Binary
- [ ] b) Fixed Blocked
- [ ] c) Free Block
- [ ] d) Full Buffer

<details>
<summary>Reponse</summary>

**b) Fixed Blocked**

Formats d'enregistrement (RECFM) :
- **F** : Fixe (non bloque)
- **FB** : Fixe bloque
- **V** : Variable
- **VB** : Variable bloque
- **U** : Indefini
</details>

---

### Question 20
Pourquoi specifier BLKSIZE=0 ?

<details>
<summary>Reponse</summary>

**BLKSIZE=0 laisse le systeme calculer la taille de bloc optimale.**

Avantages :
- Performance optimisee pour le type de disque
- Pas besoin de calculer manuellement
- Adaptation automatique aux caracteristiques du volume

Le systeme choisit un multiple de LRECL qui optimise l'utilisation des pistes.
</details>

---

### Question 21
Que signifie SPACE=(TRK,(10,5,20)) ?

<details>
<summary>Reponse</summary>

**Allocation d'espace pour un PDS :**

- **TRK** : Unite = pistes (tracks)
- **10** : Allocation primaire (10 pistes)
- **5** : Allocation secondaire (5 pistes par extension, max 15)
- **20** : 20 blocs pour le directory (PDS uniquement)

Pour CYL : meme logique en cylindres (1 CYL = 15 TRK sur 3390)
</details>

---

### Question 22
A quoi sert le parametre RLSE dans SPACE ?

- [ ] a) Reserver l'espace
- [ ] b) Liberer l'espace non utilise a la fermeture
- [ ] c) Reinitialiser l'espace
- [ ] d) Relocaliser le fichier

<details>
<summary>Reponse</summary>

**b) Liberer l'espace non utilise a la fermeture**

`SPACE=(TRK,(100,50),RLSE)` : Si le fichier n'utilise que 60 pistes, les 40 restantes sont liberees.

Bonne pratique pour optimiser l'utilisation du stockage.
</details>

---

### Question 23
Que fait la carte DD SYSOUT=* ?

- [ ] a) Envoie la sortie vers un fichier
- [ ] b) Envoie la sortie vers le spool avec la classe MSGCLASS
- [ ] c) Supprime la sortie
- [ ] d) Envoie la sortie vers l'operateur

<details>
<summary>Reponse</summary>

**b) Envoie la sortie vers le spool avec la classe MSGCLASS**

- `SYSOUT=*` : Utilise la classe MSGCLASS du JOB
- `SYSOUT=A` : Classe A specifique
- `SYSOUT=X` : Classe X (souvent held output)

Les sorties sont consultables dans SDSF.
</details>

---

### Question 24
Que fait la carte DD DUMMY ?

<details>
<summary>Reponse</summary>

**DUMMY simule un fichier vide :**

- En lecture : Retourne fin de fichier immediate
- En ecriture : Les donnees sont ignorees (pas d'I/O)

Usages :
- Tester un programme sans fichiers reels
- Desactiver temporairement une sortie
- Ignorer un fichier optionnel
</details>

---

### Question 25
Comment definir des donnees in-stream dans le JCL ?

<details>
<summary>Reponse</summary>

**Avec DD * ou DD DATA :**

```jcl
//SYSIN DD *
donnees ligne 1
donnees ligne 2
/*

//CARTES DD DATA
//ligne avec slashs
/*
```

- `DD *` : Donnees inline (// interdit dans les donnees)
- `DD DATA` : Permet // dans les donnees
- `/*` : Delimiteur de fin
- `DLM='xx'` : Delimiteur personnalise
</details>

---

### Question 26
Qu'est-ce qu'un fichier temporaire en JCL ?

<details>
<summary>Reponse</summary>

**Un fichier temporaire existe uniquement pendant le job :**

```jcl
//TEMP DD DSN=&&TEMPFILE,
//        DISP=(NEW,PASS),
//        SPACE=(TRK,(5,1))
```

- `&&nom` : Nom temporaire
- Cree au debut du step, supprime a la fin du job
- `DISP=(,PASS)` : Passe au step suivant
- Pas besoin de UNIT si VIO disponible
</details>

---

### Question 27
Qu'est-ce qu'une reference arriere (referback) ?

<details>
<summary>Reponse</summary>

**Reference a un dataset defini dans un step precedent :**

```jcl
//STEP1  EXEC PGM=PROG1
//OUTPUT DD DSN=&&TEMP,DISP=(NEW,PASS),...
//*
//STEP2  EXEC PGM=PROG2
//INPUT  DD DSN=*.STEP1.OUTPUT,DISP=(OLD,DELETE)
```

Syntaxe : `*.stepname.ddname`

Peut aussi referencer DCB ou VOL :
- `DCB=*.STEP1.OUTPUT`
- `VOL=REF=*.STEP1.OUTPUT`
</details>

---

### Question 28
Quelle est la difference entre JOBLIB et STEPLIB ?

<details>
<summary>Reponse</summary>

| Aspect | JOBLIB | STEPLIB |
|--------|--------|---------|
| **Position** | Apres JOB, avant 1er EXEC | Apres EXEC, dans le step |
| **Portee** | Tout le job | Un seul step |
| **Priorite** | Moins prioritaire | Plus prioritaire (ecrase JOBLIB) |

```jcl
//MYJOB  JOB ...
//JOBLIB DD DSN=PROD.LOADLIB,DISP=SHR
//STEP1  EXEC PGM=PROG1     <- utilise JOBLIB
//STEP2  EXEC PGM=PROG2
//STEPLIB DD DSN=TEST.LOADLIB,DISP=SHR  <- utilise STEPLIB
```
</details>

---

### Question 29
A quoi sert JCLLIB ?

- [ ] a) Definir les bibliotheques de load modules
- [ ] b) Definir les bibliotheques de procedures et INCLUDE
- [ ] c) Definir les bibliotheques de donnees
- [ ] d) Definir les bibliotheques de parametres

<details>
<summary>Reponse</summary>

**b) Definir les bibliotheques de procedures et INCLUDE**

```jcl
//MYJOB  JOB ...
//       JCLLIB ORDER=(USER.PROCLIB,PROD.PROCLIB)
//STEP1  EXEC MYPROC
```

- Place apres JOB, avant tout EXEC
- Jusqu'a 15 bibliotheques
- Ordre de recherche respecte
</details>

---

### Question 30
Qu'est-ce que la concatenation de fichiers ?

<details>
<summary>Reponse</summary>

**Plusieurs fichiers lus comme un seul :**

```jcl
//INPUT DD DSN=FILE1.DATA,DISP=SHR
//      DD DSN=FILE2.DATA,DISP=SHR
//      DD DSN=FILE3.DATA,DISP=SHR
```

Regles :
- Meme LRECL et RECFM
- BLKSIZE : le plus grand en premier
- Jusqu'a 255 fichiers sequentiels
- Jusqu'a 16 PDS
- Le DDNAME n'est specifie que sur la premiere DD
</details>

---

## Resume

| Element | Description |
|---------|-------------|
| **JOB** | Identifie le travail batch |
| **EXEC** | Execute un programme (PGM=) ou procedure |
| **DD** | Definit les fichiers |
| **CLASS** | File d'execution |
| **MSGCLASS** | Classe sortie messages |
| **NOTIFY** | Notification fin de job |
| **DISP** | (status,normal,abnormal) |
| **DCB** | RECFM, LRECL, BLKSIZE |
| **SPACE** | (unite,(prim,sec,dir),RLSE) |
| **SYSOUT** | Sortie spool |
| **DUMMY** | Fichier vide simule |
| **&&nom** | Fichier temporaire |
| **JOBLIB/STEPLIB** | Bibliotheques programmes |
| **JCLLIB** | Bibliotheques procedures |

---
*Formation z/OS - M2i Formation*
