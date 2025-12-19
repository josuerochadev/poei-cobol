# QCM 03 - Procedures JCL

## Chapitre III - Procedures in-stream, cataloguees et parametres symboliques

---

### Question 1
Qu'est-ce qu'une procedure JCL ?

- [ ] a) Un programme executable
- [ ] b) Un ensemble reutilisable d'instructions JCL
- [ ] c) Un fichier de donnees
- [ ] d) Un script shell

<details>
<summary>Reponse</summary>

**b) Un ensemble reutilisable d'instructions JCL**

Une procedure est un segment pre-ecrit de code JCL qui peut etre appele plusieurs fois. Elle permet la reutilisation, la standardisation et la maintenance centralisee.
</details>

---

### Question 2
Quels sont les deux types de procedures JCL ?

- [ ] a) Systeme et utilisateur
- [ ] b) In-stream et cataloguee
- [ ] c) Simple et complexe
- [ ] d) Locale et distante

<details>
<summary>Reponse</summary>

**b) In-stream et cataloguee**

- **In-stream** : Definie dans le JCL, entre PROC et PEND
- **Cataloguee** : Stockee dans une bibliotheque PROCLIB
</details>

---

### Question 3
Comment definir une procedure in-stream ?

<details>
<summary>Reponse</summary>

**Entre les instructions PROC et PEND :**

```jcl
//MYJOB   JOB ...
//*
//MYPROC  PROC
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN   DD DUMMY
//SYSUT1  DD DSN=&INFILE,DISP=SHR
//SYSUT2  DD DSN=&OUTFILE,DISP=(NEW,CATLG)
//        PEND
//*
//RUNSTEP EXEC MYPROC,INFILE=MY.INPUT,OUTFILE=MY.OUTPUT
```

La procedure doit etre definie avant le premier EXEC qui l'appelle.
</details>

---

### Question 4
Que signifie PEND ?

- [ ] a) Process End
- [ ] b) Procedure End
- [ ] c) Pending
- [ ] d) Program End

<details>
<summary>Reponse</summary>

**b) Procedure End**

PEND marque la fin d'une procedure in-stream. Tout ce qui est entre PROC et PEND constitue le corps de la procedure.
</details>

---

### Question 5
Combien de procedures in-stream maximum peut contenir un job ?

- [ ] a) 5
- [ ] b) 10
- [ ] c) 15
- [ ] d) Illimite

<details>
<summary>Reponse</summary>

**c) 15**

Un job peut contenir jusqu'a 15 procedures in-stream. Au-dela, utiliser des procedures cataloguees.
</details>

---

### Question 6
Ou sont stockees les procedures cataloguees ?

- [ ] a) Dans le catalogue systeme
- [ ] b) Dans une bibliotheque PROCLIB (PDS)
- [ ] c) Dans la VTOC
- [ ] d) Dans le spool JES

<details>
<summary>Reponse</summary>

**b) Dans une bibliotheque PROCLIB (PDS)**

Les procedures cataloguees sont des membres d'un PDS de type PROCLIB :
- SYS1.PROCLIB (systeme)
- Bibliotheques utilisateur specifiees par JCLLIB

Chaque membre contient une procedure complete.
</details>

---

### Question 7
Comment specifier les bibliotheques de procedures a utiliser ?

<details>
<summary>Reponse</summary>

**Avec l'instruction JCLLIB :**

```jcl
//MYJOB  JOB ...
//       JCLLIB ORDER=(USER.PROCLIB,
//                     PROD.PROCLIB,
//                     SYS1.PROCLIB)
//STEP1  EXEC MYPROC
```

- Place apres JOB, avant tout EXEC
- ORDER= specifie l'ordre de recherche
- Jusqu'a 15 bibliotheques
</details>

---

### Question 8
Quelle est la syntaxe pour appeler une procedure ?

- [ ] a) CALL MYPROC
- [ ] b) EXEC MYPROC ou EXEC PROC=MYPROC
- [ ] c) RUN MYPROC
- [ ] d) INCLUDE MYPROC

<details>
<summary>Reponse</summary>

**b) EXEC MYPROC ou EXEC PROC=MYPROC**

Les deux syntaxes sont equivalentes :
```jcl
//STEP1 EXEC MYPROC
//STEP2 EXEC PROC=MYPROC
```

Les parametres sont passes apres le nom de la procedure.
</details>

---

### Question 9
Qu'est-ce qu'un parametre symbolique ?

- [ ] a) Un parametre numerique
- [ ] b) Un parametre commencant par & remplace a l'execution
- [ ] c) Un parametre systeme
- [ ] d) Un parametre optionnel

<details>
<summary>Reponse</summary>

**b) Un parametre commencant par & remplace a l'execution**

Les parametres symboliques permettent de parametrer les procedures :

```jcl
//MYPROC PROC DSN=DEFAULT.FILE,ENV=TEST
//STEP1  EXEC PGM=MYPROG
//INPUT  DD DSN=&DSN,DISP=SHR
//OUTPUT DD DSN=&ENV..DATA.OUTPUT,DISP=(NEW,CATLG)
```

`&DSN` et `&ENV` sont remplaces par les valeurs a l'appel.
</details>

---

### Question 10
Comment definir une valeur par defaut pour un parametre symbolique ?

<details>
<summary>Reponse</summary>

**Dans l'instruction PROC :**

```jcl
//MYPROC PROC DSN=DEFAULT.FILE,
//            ENV=TEST,
//            REGION=4M
```

- `DSN=DEFAULT.FILE` : valeur par defaut
- Si non fourni a l'appel, la valeur par defaut est utilisee
- `PARAM=` (vide) signifie pas de valeur par defaut (obligatoire a l'appel)
</details>

---

### Question 11
Pourquoi utilise-t-on deux points (..) apres un symbole ?

- [ ] a) Pour terminer le symbole
- [ ] b) Pour separer le symbole du texte suivant
- [ ] c) Pour indiquer une concatenation
- [ ] d) Pour echapper le caractere

<details>
<summary>Reponse</summary>

**b) Pour separer le symbole du texte suivant**

```jcl
//OUTPUT DD DSN=&ENV..DATA.FILE
```

- `&ENV.` : le point termine le symbole
- `..DATA` : le second point fait partie du nom
- Resultat si ENV=PROD : `PROD.DATA.FILE`

Sans le double point, `&ENVDATA` serait interprete comme un seul symbole.
</details>

---

### Question 12
Quels sont les symboles systeme predifinis ?

<details>
<summary>Reponse</summary>

**Principaux symboles systeme :**

| Symbole | Description | Exemple |
|---------|-------------|---------|
| `&SYSUID` | Userid du soumetteur | FTEST |
| `&SYSDATE` | Date (yy.ddd) | 24.340 |
| `&LYYMMDD` | Date (yyyymmdd) | 20241205 |
| `&SYSTIME` | Heure (hh.mm.ss) | 14.30.25 |
| `&SYSJOBNAME` | Nom du job | MYJOB01 |
| `&SYSSTEP` | Nom du step courant | STEP1 |

```jcl
//OUTPUT DD DSN=&SYSUID..RAPPORT.D&LYYMMDD
```
</details>

---

### Question 13
Comment passer une valeur a un parametre symbolique lors de l'appel ?

<details>
<summary>Reponse</summary>

**Sur la carte EXEC :**

```jcl
//STEP1 EXEC MYPROC,DSN=PROD.DATA.INPUT,ENV=PROD
```

ou avec SET :

```jcl
//       SET DSN=PROD.DATA.INPUT
//       SET ENV=PROD
//STEP1  EXEC MYPROC
```

Les valeurs fournies remplacent les valeurs par defaut.
</details>

---

### Question 14
Qu'est-ce que l'instruction SET ?

- [ ] a) Configure le systeme
- [ ] b) Definit des variables symboliques
- [ ] c) Initialise un fichier
- [ ] d) Demarre un processus

<details>
<summary>Reponse</summary>

**b) Definit des variables symboliques**

```jcl
//       SET ENV=PROD
//       SET DATE=20241205
//       SET HLQUAL=PROD.BATCH
//*
//STEP1  EXEC PGM=MYPROG
//INPUT  DD DSN=&HLQUAL..DATA.INPUT,DISP=SHR
```

SET permet de definir des symboles reutilisables dans tout le JCL.
</details>

---

### Question 15
Combien de niveaux d'imbrication de procedures sont autorises ?

- [ ] a) 5
- [ ] b) 10
- [ ] c) 15
- [ ] d) Illimite

<details>
<summary>Reponse</summary>

**c) 15**

Une procedure peut appeler une autre procedure, jusqu'a 15 niveaux d'imbrication.

```
JOB -> PROC1 -> PROC2 -> PROC3 -> ... (max 15 niveaux)
```

Note : Une procedure in-stream ne peut pas etre appelee depuis une procedure cataloguee.
</details>

---

### Question 16
Comment surcharger (override) une carte DD d'une procedure ?

<details>
<summary>Reponse</summary>

**En specifiant procstep.ddname :**

```jcl
//MYPROC  PROC
//STEP1   EXEC PGM=MYPROG
//INPUT   DD DSN=DEFAULT.INPUT,DISP=SHR
//        PEND
//*
//RUN     EXEC MYPROC
//STEP1.INPUT DD DSN=MY.REAL.INPUT,DISP=SHR
```

Syntaxe : `//procstepname.ddname DD ...`

L'override remplace completement la DD de la procedure.
</details>

---

### Question 17
Comment ajouter une DD a un step de procedure ?

<details>
<summary>Reponse</summary>

**En specifiant une nouvelle DD apres le step :**

```jcl
//MYPROC  PROC
//STEP1   EXEC PGM=MYPROG
//INPUT   DD DSN=DATA.INPUT,DISP=SHR
//        PEND
//*
//RUN     EXEC MYPROC
//STEP1.EXTRA DD DSN=ADDITIONAL.FILE,DISP=SHR
```

La nouvelle DD `EXTRA` est ajoutee au step STEP1.
</details>

---

### Question 18
Quelles instructions sont interdites dans une procedure ?

<details>
<summary>Reponse</summary>

**Instructions interdites dans une procedure :**

- **JOB** : Definit un nouveau job
- **JOBLIB** : Doit etre dans le JCL appelant
- **JOBCAT** : Catalogue job
- **Instructions JES2/JES3** : /*ROUTE, /*PRIORITY, etc.
- **DD ***, **DD DATA** : Donnees in-stream (dans proc cataloguee)

Ces instructions doivent etre dans le JCL principal, pas dans les procedures.
</details>

---

### Question 19
Peut-on utiliser des donnees in-stream (DD *) dans une procedure cataloguee ?

- [ ] a) Oui, sans restriction
- [ ] b) Non, jamais
- [ ] c) Oui, mais avec DLM=
- [ ] d) Seulement avec SYSIN

<details>
<summary>Reponse</summary>

**b) Non, jamais**

Les donnees in-stream (DD *, DD DATA) ne peuvent pas etre dans une procedure cataloguee car la procedure est dans un PDS, pas dans le flux JCL.

Solution : Utiliser un fichier externe ou passer les donnees via override dans le JCL appelant.
</details>

---

### Question 20
Comment voir le JCL expanse (avec procedures resolues) ?

<details>
<summary>Reponse</summary>

**Avec MSGLEVEL=(1,1) :**

```jcl
//MYJOB JOB ...,MSGLEVEL=(1,1)
```

Le premier parametre a 1 affiche :
- Le JCL original
- Les procedures expansees (lignes XX et ++)

Dans SDSF, consulter JESJCL pour voir le JCL complet avec les procedures resolues.
</details>

---

### Question 21
Que signifient les prefixes XX et ++ dans le listing JCL ?

<details>
<summary>Reponse</summary>

**Identification des lignes dans le listing :**

| Prefixe | Signification |
|---------|---------------|
| `//` | Instruction JCL originale |
| `XX` | Instruction de procedure (non modifiee) |
| `++` | Instruction de procedure (modifiee/override) |

```
//STEP1  EXEC MYPROC           <- JCL original
XXSTEP1  EXEC PGM=MYPROG       <- Venant de la procedure
++INPUT  DD DSN=MY.FILE        <- Override applique
```
</details>

---

### Question 22
Comment surcharger le parametre PARM d'un step de procedure ?

<details>
<summary>Reponse</summary>

**Sur la carte EXEC avec procstep.PARM= :**

```jcl
//MYPROC  PROC
//STEP1   EXEC PGM=MYPROG,PARM='DEFAULT'
//        PEND
//*
//RUN     EXEC MYPROC,STEP1.PARM='NEWVALUE'
```

Syntaxe : `procstepname.parameter=value`

Fonctionne aussi pour REGION, TIME, COND, etc.
</details>

---

### Question 23
Quelle est la difference entre JCLLIB et JOBLIB ?

<details>
<summary>Reponse</summary>

| Aspect | JCLLIB | JOBLIB |
|--------|--------|--------|
| **Contenu** | Procedures JCL, INCLUDE | Load modules (programmes) |
| **Moment** | Interpretation JCL | Execution |
| **Syntaxe** | `JCLLIB ORDER=` | `//JOBLIB DD` |
| **Position** | Apres JOB, avant EXEC | Apres JOB |
| **Usage** | Trouver PROC= | Trouver PGM= |

Les deux sont complementaires et servent des buts differents.
</details>

---

### Question 24
Comment inclure un fragment de JCL avec INCLUDE ?

<details>
<summary>Reponse</summary>

**Avec l'instruction INCLUDE :**

```jcl
//MYJOB  JOB ...
//       JCLLIB ORDER=MY.JCLLIB
//*
//STEP1  EXEC PGM=MYPROG
//       INCLUDE MEMBER=STDDD
//OUTPUT DD DSN=MY.OUTPUT,DISP=(NEW,CATLG)
```

Le membre STDDD de MY.JCLLIB est insere a cet endroit.

INCLUDE permet de reutiliser des fragments de JCL (DDs standards, parametres communs).
</details>

---

### Question 25
Qu'est-ce qu'une procedure imbriquee ?

<details>
<summary>Reponse</summary>

**Une procedure qui appelle une autre procedure :**

```jcl
//PROC1   PROC
//STEP1   EXEC PGM=PROG1
//STEP2   EXEC PROC2      <- Appel de PROC2
//        PEND
//*
//PROC2   PROC
//STEP1   EXEC PGM=PROG2
//        PEND
```

Regles :
- Maximum 15 niveaux d'imbrication
- Une proc in-stream ne peut pas etre dans une proc cataloguee
- Utile pour la modularite
</details>

---

## Resume

| Element | Description |
|---------|-------------|
| **PROC** | Debut de procedure |
| **PEND** | Fin de procedure in-stream |
| **JCLLIB** | Bibliotheques de procedures |
| **&symbole** | Parametre symbolique |
| **&SYSxxxx** | Symboles systeme |
| **SET** | Definit une variable symbolique |
| **step.DD** | Override d'une DD |
| **step.PARM** | Override de PARM |
| **INCLUDE** | Insere un fragment JCL |
| **XX** | Ligne de procedure non modifiee |
| **++** | Ligne de procedure avec override |
| **Max 15** | Procedures in-stream ou niveaux d'imbrication |

---
*Formation z/OS - M2i Formation*
