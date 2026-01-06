# Chapitre I - Traitement des cartes JOB, EXEC et DD

## Introduction

Ce chapitre traite des trois cartes fondamentales du JCL (Job Control Language) qui permettent à l'utilisateur de définir son traitement batch sur z/OS :

1. **La carte JOB** - Identifie le travail à traiter
2. **La carte EXEC** - Identifie le programme à exécuter
3. **La carte DD** - Identifie et localise les données concernées

Ces trois cartes constituent la **base de tous les travaux batch** sur mainframe.

---

## I-1 Introduction au Job Control Language (JCL)

### I-1-1 Qu'est-ce que le JCL ?

Le **JCL (Job Control Language)** est le langage de contrôle des travaux sur z/OS. Il permet de :

```
┌─────────────────────────────────────────────────────────────────┐
│                    RÔLE DU JCL                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. DÉFINIR UN TRAVAIL (JOB)                                   │
│      • Nom du job                                               │
│      • Classe d'exécution                                       │
│      • Priorité                                                 │
│      • Comptabilité                                             │
│                                                                 │
│   2. SPÉCIFIER LES PROGRAMMES À EXÉCUTER                        │
│      • Nom du programme                                         │
│      • Paramètres d'exécution                                   │
│      • Ressources mémoire                                       │
│                                                                 │
│   3. DÉCRIRE LES FICHIERS UTILISÉS                              │
│      • Nom et localisation des datasets                         │
│      • Caractéristiques (organisation, format, taille)          │
│      • Mode d'accès (lecture, écriture, création)               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-1-2 Structure générale d'un JCL

```
┌─────────────────────────────────────────────────────────────────┐
│                    STRUCTURE JCL                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Colonnes:  1-2     3-10      12-15    16-71      72          │
│              ──────────────────────────────────────────────     │
│              //      NOM       OPER     OPERANDES  SUITE        │
│                                                                 │
│   Exemple:                                                      │
│   //MYJOB    JOB   (ACCT),'DESCRIPTION',CLASS=A                │
│   //STEP1    EXEC  PGM=IEFBR14                                  │
│   //DD1      DD    DSN=MY.DATASET,DISP=SHR                      │
│                                                                 │
│   RÈGLES:                                                       │
│   • Colonnes 1-2  : Toujours //                                │
│   • Colonne 3     : Début du nom (ou espace si pas de nom)     │
│   • Colonnes 3-10 : Nom (1-8 caractères alphanumériques)       │
│   • Après le nom  : Au moins un espace                         │
│   • Opération     : JOB, EXEC, DD, etc.                        │
│   • Opérandes     : Paramètres séparés par des virgules        │
│   • Colonne 72    : Caractère de continuation (non-blanc)      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-1-3 Types d'instructions JCL

| Type | Description | Exemple |
|------|-------------|---------|
| **JOB** | Début de travail | `//MYJOB JOB ...` |
| **EXEC** | Exécution programme | `//STEP1 EXEC PGM=PROG1` |
| **DD** | Définition de données | `//INPUT DD DSN=...` |
| **PROC** | Définition procédure | `//MYPROC PROC ...` |
| **PEND** | Fin de procédure | `// PEND` |
| **SET** | Variable symbolique | `// SET VAR=VALUE` |
| **IF/THEN/ELSE/ENDIF** | Conditionnel | `// IF RC=0 THEN` |
| **JCLLIB** | Bibliothèque JCL | `// JCLLIB ORDER=...` |
| **INCLUDE** | Inclusion membre | `// INCLUDE MEMBER=...` |
| **/\*** | Délimiteur données | `/*` |
| **//\*** | Commentaire | `//* Ceci est un commentaire` |

### I-1-4 Règles de syntaxe

```
┌─────────────────────────────────────────────────────────────────┐
│                    RÈGLES DE SYNTAXE JCL                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. MAJUSCULES                                                 │
│      Le JCL est en MAJUSCULES (sauf contenu entre apostrophes)  │
│                                                                 │
│   2. DÉBUT DE LIGNE                                             │
│      // en colonnes 1-2 (obligatoire)                          │
│                                                                 │
│   3. NOMS                                                       │
│      • 1 à 8 caractères                                        │
│      • Commence par une lettre ou @, #, $                      │
│      • Alphanumérique + @, #, $                                │
│                                                                 │
│   4. CONTINUATION                                               │
│      • Interrompre après une virgule                           │
│      • Caractère non-blanc en colonne 72 (optionnel)           │
│      • Continuer entre colonnes 4 et 16 sur ligne suivante     │
│                                                                 │
│   5. COMMENTAIRES                                               │
│      • //* en colonnes 1-3                                     │
│      • Ou après les opérandes (séparé par espace)              │
│                                                                 │
│   6. FIN DE JOB                                                 │
│      • // seul en colonnes 1-2 (null statement)                │
│      • Ou début d'un nouveau JOB                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-1-5 Exemple de JCL complet

```jcl
//FTESTJB1 JOB (ACCT),'EXEMPLE JCL',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXEMPLE DE JCL - COPIE DE FICHIER
//* ============================================================
//*
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//SYSUT1   DD   DSN=FTEST.INPUT.DATA,
//              DISP=SHR
//SYSUT2   DD   DSN=FTEST.OUTPUT.DATA,
//              DISP=(NEW,CATLG,DELETE),
//              SPACE=(TRK,(10,5),RLSE),
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//*
//STEP020  EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *
  LISTCAT ENTRIES(FTEST.OUTPUT.DATA) ALL
/*
//
```

---

## I-2 Instructions JCL

### I-2-a Structure des champs du JOB statement

La carte **JOB** est la première instruction de tout travail batch. Elle identifie le job auprès du système.

#### Syntaxe générale

```
//jobname JOB accounting,'programmer-name',paramètres...
```

#### I-2-a-i Champ identifiant (Colonnes 1-2)

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAMP IDENTIFIANT                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Colonnes 1-2 : //                                            │
│                                                                 │
│   • Obligatoire pour toutes les cartes JCL                     │
│   • Identifie une instruction JCL pour le système              │
│   • Distingue le JCL des données in-stream                     │
│                                                                 │
│   Exemples:                                                     │
│   //MYJOB    JOB  ...      ◄── Carte JCL                       │
│   DONNEES                   ◄── Données (pas de //)            │
│   /*                        ◄── Fin de donnees                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-a-ii Champ de Nom (Colonnes 3-10)

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAMP DE NOM (JOBNAME)                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Position : Colonnes 3 à 10 (max 8 caractères)                │
│                                                                 │
│   RÈGLES:                                                       │
│   • Commence en colonne 3                                      │
│   • 1 à 8 caractères alphanumériques                           │
│   • Premier caractère : A-Z ou @, #, $                         │
│   • Caractères suivants : A-Z, 0-9, @, #, $                    │
│   • Le nom doit être UNIQUE dans le système                    │
│                                                                 │
│   CONVENTIONS COURANTES:                                        │
│   • Préfixe = Userid (ex: FTEST)                               │
│   • Suffixe = Identifiant unique                               │
│                                                                 │
│   Exemples valides:                                             │
│   //FTESTJB1   ◄── Userid FTEST, job 1                         │
│   //PAIE001    ◄── Application PAIE, job 001                   │
│   //A         ◄── Nom minimal (1 caractere)                    │
│   //#SPECIAL   ◄── Commence par #                              │
│                                                                 │
│   Exemples INVALIDES:                                           │
│   //1JOB       ◄── Commence par un chiffre                     │
│   //MYVERYLONGJOB  ◄── Plus de 8 caractères                    │
│   //MY JOB     ◄── Contient un espace                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-a-iii Champ d'operation (apres le nom)

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAMP D'OPERATION                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Position : Apres le nom, separe par au moins un espace       │
│                                                                 │
│   Pour la carte JOB, l'operation est toujours : JOB            │
│                                                                 │
│   //MYJOB    JOB   ...                                          │
│              ───                                                 │
│              Operation                                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-a-iv Champ d'operande (paramètres)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRES DU JOB                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PARAMETRES POSITIONNELS (dans l'ordre) :                      │
│   ─────────────────────────────────────────                     │
│   1. Accounting information : (compte,sous-compte,...)         │
│   2. Programmer name        : 'Nom du programmeur'             │
│                                                                 │
│   PARAMETRES A MOTS-CLES (ordre libre) :                        │
│   ──────────────────────────────────────                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Paramètres a mots-clés du JOB :**

| Paramètre | Description | Exemple |
|-----------|-------------|---------|
| **CLASS** | Classe d'execution (A-Z, 0-9) | `CLASS=A` |
| **MSGCLASS** | Classe de sortie des messages | `MSGCLASS=X` |
| **MSGLEVEL** | Niveau de messages (statements,allocation) | `MSGLEVEL=(1,1)` |
| **NOTIFY** | Userid a notifier | `NOTIFY=&SYSUID` |
| **REGION** | Mémoire pour le job | `REGION=4M` |
| **TIME** | Temps CPU maximum | `TIME=(1,30)` ou `TIME=NOLIMIT` |
| **TYPRUN** | Type d'execution | `TYPRUN=SCAN` ou `TYPRUN=HOLD` |
| **PRTY** | Priorite (0-15) | `PRTY=8` |
| **COND** | Condition de terminaison | `COND=(4,LT)` |
| **RESTART** | Point de reprise | `RESTART=STEP020` |
| **USER** | Userid d'execution | `USER=FTEST` |
| **PASSWORD** | Mot de passe (obsolète) | - |

**Valeurs de MSGLEVEL :**

```
┌─────────────────────────────────────────────────────────────────┐
│                    MSGLEVEL=(stmt,alloc)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Premier paramètre (stmt) - Affichage des instructions :       │
│   ─────────────────────────────────────────────────────────     │
│   0 = Seulement la carte JOB                                   │
│   1 = Toutes les instructions JCL et JES                       │
│   2 = Seulement les instructions JCL                           │
│                                                                 │
│   Second paramètre (alloc) - Messages d'allocation :            │
│   ─────────────────────────────────────────────────────         │
│   0 = Pas de messages (sauf si job ABEND)                      │
│   1 = Tous les messages d'allocation                           │
│                                                                 │
│   Recommandation : MSGLEVEL=(1,1) pour le debug                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Valeurs de TYPRUN :**

| Valeur | Description |
|--------|-------------|
| `TYPRUN=SCAN` | Vérification syntaxe uniquement (pas d'execution) |
| `TYPRUN=HOLD` | Job en attente (nécessite release) |
| `TYPRUN=JCLHOLD` | Job en attente avec verification JCL |
| `TYPRUN=COPY` | Copie du JCL dans le spool |

#### I-2-a-v Champ de commentaires (facultatif)

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMENTAIRES JCL                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   METHODE 1 : Ligne de commentaire complete                     │
│   ─────────────────────────────────────────                     │
│   //* Ceci est un commentaire sur une ligne entiere            │
│                                                                 │
│   METHODE 2 : Commentaire en fin d'instruction                  │
│   ─────────────────────────────────────────────                 │
│   //STEP1 EXEC PGM=PROG1  EXECUTION DU PROGRAMME               │
│                           ──────────────────────                │
│                           Commentaire (apres 2+ espaces)        │
│                                                                 │
│   Note : Les commentaires ne sont PAS continues                │
│          Chaque ligne de commentaire doit commencer par //*    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Exemples de cartes JOB

```jcl
//* Exemple 1 : JOB minimal
//MYJOB1   JOB

//* Exemple 2 : JOB avec accounting et nom
//MYJOB2   JOB (DEPT01),'JEAN DUPONT'

//* Exemple 3 : JOB complet avec paramètres
//FTESTJB1 JOB (ACCT123,SUBACCT),'FORMATION JCL',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,
//             REGION=0M,
//             TIME=5

//* Exemple 4 : JOB pour verification syntaxe
//TESTJOB  JOB ,'SCAN JCL',TYPRUN=SCAN

//* Exemple 5 : JOB avec conditions
//PRODJOB  JOB (PROD),'BATCH PROD',
//             CLASS=P,
//             COND=(0,NE),
//             RESTART=STEP030
```

---

### I-2-b Structure des champs du EXEC statement

La carte **EXEC** définit le programme ou la procédure a executer dans un step.

#### Syntaxe generale

```
//stepname EXEC PGM=program   ou   //stepname EXEC PROC=procédure
```

#### Paramètres du EXEC

##### I-2-b-i Paramètre PGM

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PGM                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PGM=program-name                                              │
│                                                                 │
│   • Spécifie le programme a executer                           │
│   • Le programme doit etre dans une bibliothèque système       │
│     ou spécifiée par JOBLIB/STEPLIB                            │
│   • Nom de 1 a 8 caractères                                    │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=IEFBR14     ◄── Programme IBM (ne fait rien)│
│   //STEP2 EXEC PGM=IEBGENER    ◄── Utilitaire copie            │
│   //STEP3 EXEC PGM=MYPROG      ◄── Programme utilisateur       │
│   //STEP4 EXEC PGM=IKJEFT01    ◄── TSO batch                   │
│   //STEP5 EXEC PGM=IDCAMS      ◄── Utilitaire VSAM             │
│                                                                 │
│   REFERENCE ARRIERE :                                           │
│   //STEP2 EXEC PGM=*.STEP1.STEPLIB                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-ii Paramètre PROC

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PROC                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PROC=procédure-name  ou simplement  procédure-name            │
│                                                                 │
│   • Spécifie une procédure cataloguée a executer               │
│   • La procédure doit etre dans une bibliothèque PROCLIB       │
│   • Le mot-clé PROC est optionnel                              │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PROC=MYPROC                                      │
│   //STEP1 EXEC MYPROC          ◄── Equivalent                  │
│   //STEP1 EXEC COBOLCL         ◄── Procedure de compilation    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-iii Paramètre REGION

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE REGION                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   REGION=valeur                                                 │
│                                                                 │
│   • Spécifie la mémoire virtuelle pour le step                 │
│   • Peut etre en K (kilobytes) ou M (megabytes)                │
│   • REGION=0M = mémoire illimitee (jusqu'au max système)       │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG,REGION=4M                              │
│   //STEP2 EXEC PGM=PROG,REGION=2048K                           │
│   //STEP3 EXEC PGM=PROG,REGION=0M    ◄── Illimitée             │
│                                                                 │
│   Note : REGION sur EXEC écrase REGION sur JOB pour ce step    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-iv Paramètre PARM

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PARM                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PARM='valeur'  ou  PARM=(val1,val2,...)                       │
│                                                                 │
│   • Passe des paramètres au programme                          │
│   • Maximum 100 caractères                                     │
│   • Utiliser apostrophes si caractères spéciaux                │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=MYPROG,PARM='DEBUG'                         │
│   //STEP2 EXEC PGM=MYPROG,PARM='DATE=2024,MODE=TEST'           │
│   //STEP3 EXEC PGM=SORT,PARM='MSG=AP'                          │
│   //STEP4 EXEC PGM=IGYCRCTL,PARM='LIST,MAP'                    │
│                                                                 │
│   Pour COBOL, le paramètre est reçu dans :                      │
│   01 WS-PARM.                                                   │
│      05 WS-PARM-LENGTH PIC S9(4) COMP.                         │
│      05 WS-PARM-DATA   PIC X(100).                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-v Paramètre ADDRSPC

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE ADDRSPC                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ADDRSPC=VIRT | REAL                                           │
│                                                                 │
│   • VIRT = Mémoire virtuelle (défaut)                          │
│   • REAL = Mémoire réelle (pour programmes spéciaux)           │
│                                                                 │
│   Exemple:                                                      │
│   //STEP1 EXEC PGM=MYPROG,ADDRSPC=REAL                         │
│                                                                 │
│   Note : REAL rarement utilisé, nécessite privileges           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-vi Paramètre MEMLIMIT

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE MEMLIMIT                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   MEMLIMIT=valeur                                               │
│                                                                 │
│   • Limite de mémoire au-dessus de la barre (>2GB)             │
│   • Pour programmes 64-bit                                     │
│   • MEMLIMIT=NOLIMIT pour pas de limite                        │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG64,MEMLIMIT=4G                          │
│   //STEP2 EXEC PGM=PROG64,MEMLIMIT=NOLIMIT                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-vii Paramètre TIME

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE TIME                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   TIME=(minutes,secondes)  ou  TIME=minutes  ou  TIME=NOLIMIT   │
│                                                                 │
│   • Limite le temps CPU pour le step                           │
│   • Protège contre les bouclés infinies                        │
│   • TIME=NOLIMIT ou TIME=1440 = pas de limite                  │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG,TIME=5          ◄── 5 minutes          │
│   //STEP2 EXEC PGM=PROG,TIME=(1,30)     ◄── 1 min 30 sec       │
│   //STEP3 EXEC PGM=PROG,TIME=NOLIMIT    ◄── Illimité           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-viii Paramètre COND

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE COND                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   COND=(code,operateur)  ou  COND=((c1,op1),(c2,op2),...)       │
│                                                                 │
│   Conditionne l'execution du step selon les return codes       │
│   précédents. Le step est BYPASSE si la condition est VRAIE.   │
│                                                                 │
│   Operateurs:                                                   │
│   GT = Greater Than (>)      LT = Less Than (<)                │
│   GE = Greater or Equal (>=) LE = Less or Equal (<=)           │
│   EQ = Equal (=)             NE = Not Equal (<>)               │
│                                                                 │
│   Exemples:                                                     │
│   COND=(4,LT)         Si RC < 4, bypass ce step                │
│   COND=(0,NE)         Si RC <> 0, bypass (executer si RC=0)    │
│   COND=(8,LE,STEP1)   Si STEP1.RC <= 8, bypass                 │
│   COND=EVEN           Exécuter meme si ABEND précédent         │
│   COND=ONLY           Exécuter SEULEMENT si ABEND précédent    │
│                                                                 │
│   Note : Préférer IF/THEN/ELSE pour plus de clarté             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Exemple de cartes EXEC

```jcl
//* Step simple
//STEP010  EXEC PGM=IEFBR14

//* Step avec paramètres
//STEP020  EXEC PGM=MYPROG,
//             REGION=4M,
//             TIME=5,
//             PARM='MODE=TEST'

//* Step conditionnel
//STEP030  EXEC PGM=RAPPORT,COND=(4,LT)

//* Appel de procédure
//STEP040  EXEC PROC=COBOLCL,
//             PARM.COB='LIST,MAP'
```

---

### I-2-c Structure des champs du DD statement

La carte **DD (Data Définition)** définit les fichiers utilisés par le programme. C'est la carte la plus riche en paramètres.

#### Syntaxe generale

```
//ddname   DD   paramètres...
```

#### I-2-c-1 DSN (Data Set Name)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE DSN (ou DSNAME)                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DSN=dataset-name                                              │
│                                                                 │
│   • Nom du dataset (fichier) a utilisér                        │
│   • Format : qualificateur1.qualificateur2...                  │
│   • Chaque qualificateur : 1-8 caractères                      │
│   • Maximum 44 caractères au total                             │
│                                                                 │
│   Types de noms:                                                │
│   ─────────────                                                 │
│   DSN=FTEST.DATA.FILE           ◄── Dataset permanent          │
│   DSN=FTEST.PDS(MEMBRE)         ◄── Membre de PDS              │
│   DSN=&&TEMP                    ◄── Dataset temporaire         │
│   DSN=*.STEP1.DDNAME            ◄── Référence arriere          │
│   DSN=NULLFILE                  ◄── Fichier nul                │
│                                                                 │
│   Exemples:                                                     │
│   //INPUT  DD DSN=PROD.CLIENTS.DATA,DISP=SHR                   │
│   //OUTPUT DD DSN=PROD.RAPPORT.LIST(+1),DISP=(NEW,CATLG)       │
│   //WORK   DD DSN=&&TEMP,DISP=(NEW,PASS)                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-2 DISP (Disposition)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE DISP                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DISP=(statut,fin-normale,fin-anormale)                        │
│                                                                 │
│   STATUT (etat initial) :                                       │
│   ───────────────────────                                       │
│   NEW      Le dataset sera créé par ce step                    │
│   OLD      Dataset existant, acces exclusif                    │
│   SHR      Dataset existant, acces partagé                     │
│   MOD      Ajout en fin de fichier (ou creation si inexistant) │
│                                                                 │
│   FIN NORMALE (si step OK) :                                    │
│   ──────────────────────────                                    │
│   DELETE   Supprimer le dataset                                │
│   KEEP     Conserver (non catalogué)                           │
│   PASS     Passer au step suivant                              │
│   CATLG    Cataloguer le dataset                               │
│   UNCATLG  Décataloguér                                        │
│                                                                 │
│   FIN ANORMALE (si step ABEND) :                                │
│   ──────────────────────────────                                │
│   Memes valeurs que fin normale                                │
│   Defaut = meme valeur que fin normale                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Combinaisons courantes DISP :**

| DISP | Description | Utilisation |
|------|-------------|-------------|
| `DISP=SHR` | Lecture partagée | Fichiers en lecture |
| `DISP=OLD` | Acces exclusif | Mise a jour fichier |
| `DISP=(NEW,CATLG,DELETE)` | Creation, catalogué si OK | Nouveaux fichiers permanents |
| `DISP=(NEW,PASS)` | Creation, passage au step suivant | Fichiers intermediaires |
| `DISP=(NEW,DELETE)` | Creation, suppression | Fichiers de travail |
| `DISP=(MOD,CATLG)` | Ajout/creation, catalogué | Log, cumul |
| `DISP=(OLD,DELETE)` | Lecture exclusive puis suppression | Traitement puis purge |

```
┌─────────────────────────────────────────────────────────────────┐
│                    EXEMPLES DISP                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //INPUT  DD DSN=DATA.INPUT,DISP=SHR                          │
│   //       Lecture partagée d'un fichier existant              │
│                                                                 │
│   //OUTPUT DD DSN=DATA.OUTPUT,                                  │
│   //          DISP=(NEW,CATLG,DELETE)                           │
│   //       Creation, catalogué si OK, supprime si ABEND        │
│                                                                 │
│   //WORK   DD DSN=&&TEMP,                                       │
│   //          DISP=(NEW,PASS,DELETE)                            │
│   //       Temporaire, passe au step suivant si OK             │
│                                                                 │
│   //LOG    DD DSN=PROD.LOG,DISP=MOD                            │
│   //       Ajout en fin de fichier existant                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-3 DCB (Data Control Block)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE DCB                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DCB=(sous-paramètres)                                         │
│                                                                 │
│   Définit les caracteristiques physiques du dataset            │
│                                                                 │
│   RECFM = Record Format (format d'enregistrement)              │
│   ────────────────────────────────────────────────              │
│   F   = Fixe                                                   │
│   FB  = Fixe bloque                                            │
│   V   = Variable                                               │
│   VB  = Variable bloque                                        │
│   U   = Undefined (load modules)                               │
│   FA  = Fixe avec ASA (caractères de controle imprimante)     │
│   FBA = Fixe bloque avec ASA                                   │
│                                                                 │
│   LRECL = Logical Record Length (longueur enregistrement)      │
│   ─────────────────────────────────────────────────────        │
│   Pour RECFM=F/FB : longueur exacte                            │
│   Pour RECFM=V/VB : longueur maximale (inclut 4 octets RDW)    │
│                                                                 │
│   BLKSIZE = Block Size (taille de bloc)                        │
│   ─────────────────────────────────────                        │
│   Pour FB : multiple de LRECL                                  │
│   BLKSIZE=0 : système calcule optimal                          │
│                                                                 │
│   DSORG = Data Set Organization                                 │
│   ─────────────────────────────────                            │
│   PS = Physical Sequential                                     │
│   PO = Partitioned Organization (PDS)                          │
│   DA = Direct Access                                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Exemples DCB :**

```jcl
//* Fichier fixe bloque 80 colonnes (source COBOL)
//SOURCE DD DSN=...,DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)

//* Fichier variable bloque
//VARFILE DD DSN=...,DCB=(RECFM=VB,LRECL=1004,BLKSIZE=0)

//* Fichier impression avec controle ASA
//PRINT DD DSN=...,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=0)

//* Référence DCB d'un autre fichier
//OUTPUT DD DSN=...,DCB=*.INPUT
```

#### I-2-c-4 UNIT

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE UNIT                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   UNIT=type-unite                                               │
│                                                                 │
│   Spécifie le type de périphérique pour le dataset             │
│                                                                 │
│   Valeurs courantes:                                            │
│   ──────────────────                                           │
│   UNIT=SYSDA         Disque système (le plus courant)          │
│   UNIT=SYSALLDA      Tous disques disponibles                  │
│   UNIT=3390          Modele de disque specifique               │
│   UNIT=TAPE          Bande magnetique                          │
│   UNIT=3480          Cartouche                                 │
│   UNIT=VIO           Virtual I/O (mémoire)                     │
│                                                                 │
│   Exemples:                                                     │
│   //OUTPUT DD DSN=...,UNIT=SYSDA                               │
│   //BACKUP DD DSN=...,UNIT=TAPE                                │
│   //TEMP   DD DSN=&&T,UNIT=VIO    ◄── Très rapide              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-5 VOLUME (VOL)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE VOLUME (VOL)                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   VOL=SER=volume-serial                                         │
│                                                                 │
│   Spécifie le volume (disque/bande) a utilisér                 │
│                                                                 │
│   Sous-paramètres:                                              │
│   ─────────────────                                            │
│   SER=volser     Volume serial (nom du disque)                 │
│   REF=dsname     Meme volume qu'un autre dataset               │
│   PRIVATE       Volume privé (non partagé)                     │
│   RETAIN        Conserver montage jusqu'a fin job              │
│                                                                 │
│   Exemples:                                                     │
│   //DD1 DD DSN=...,VOL=SER=PROD01                              │
│   //DD2 DD DSN=...,VOL=SER=(TAPE01,TAPE02)   ◄── Multi-volumes │
│   //DD3 DD DSN=...,VOL=REF=*.STEP1.DD1       ◄── Référence     │
│                                                                 │
│   Note : Rarement nécessaire si dataset catalogué              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-6 SPACE

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE SPACE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SPACE=(unite,(primaire,secondaire,répertoire),RLSE)           │
│                                                                 │
│   Allocation d'espace disque pour nouveaux datasets            │
│                                                                 │
│   UNITE D'ALLOCATION :                                          │
│   ────────────────────                                         │
│   TRK        Pistes (tracks)                                   │
│   CYL        Cylindres                                         │
│   nnnnn      Blocs de n octets                                 │
│   (RECL,n)   n enregistrements de longueur RECL                │
│                                                                 │
│   QUANTITES :                                                   │
│   ───────────                                                  │
│   primaire   Espace initial alloué                             │
│   secondaire Espace additionnel (jusqu'a 15 extensions)        │
│   répertoire Blocs pour répertoire PDS (uniquement PDS)        │
│                                                                 │
│   OPTIONS :                                                     │
│   ──────────                                                   │
│   RLSE       Libérer espace non utilisé a la fermeture        │
│   CONTIG     Espace contigu obligatoire                        │
│   MXIG       Plus grande zone contigue                         │
│   ALX        5 zones max                                       │
│   ROUND      Arrondir au cylindre (si unite=blocs)            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Exemples SPACE :**

```jcl
//* Fichier sequentiel : 10 pistes + 5 extensions
//OUTPUT DD DSN=...,SPACE=(TRK,(10,5),RLSE)

//* Fichier PDS : 5 cylindres, 10 blocs répertoire
//PDS DD DSN=...,SPACE=(CYL,(5,1,10))

//* Allocation en nombre de blocs
//DATA DD DSN=...,SPACE=(800,(1000,100),RLSE)

//* Très gros fichier : 100 cylindres
//BIG DD DSN=...,SPACE=(CYL,(100,50),RLSE,CONTIG)

//* Fichier temporaire petit
//TEMP DD DSN=&&T,SPACE=(TRK,(1,1))
```

**Guide de dimensionnement :**

```
┌─────────────────────────────────────────────────────────────────┐
│               GUIDE DIMENSIONNEMENT SPACE                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Disque 3390 :                                                 │
│   • 1 piste (TRK) ≈ 56 KB                                      │
│   • 1 cylindre (CYL) = 15 pistes ≈ 840 KB                     │
│                                                                 │
│   Pour RECFM=FB, LRECL=80, BLKSIZE=27920 :                     │
│   • ~349 enregistrements par bloc                              │
│   • ~2 blocs par piste                                         │
│   • ~698 enregistrements par piste                             │
│                                                                 │
│   Formule approximative :                                       │
│   Pistes = (Nb_enregistrements × LRECL) / 56000                │
│                                                                 │
│   Recommandation : BLKSIZE=0 laisse z/OS optimiser             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-7 KEYLEN et RECORG (VSAM)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRES VSAM                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   KEYLEN=longueur                                               │
│   • Longueur de la clé pour fichiers VSAM KSDS                 │
│                                                                 │
│   KEYOFF=position                                               │
│   • Position de la clé dans l'enregistrement                   │
│                                                                 │
│   RECORG=organisation                                           │
│   • KS = KSDS (Key Sequenced)                                  │
│   • ES = ESDS (Entry Sequenced)                                │
│   • RR = RRDS (Relative Record)                                │
│   • LS = Linear (fichiers lineaires)                           │
│                                                                 │
│   Note : Ces paramètres sont rarement nécessaires car          │
│   les caracteristiques sont définies a la creation du fichier  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-8 OUTLIM

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE OUTLIM                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   OUTLIM=nombre                                                 │
│                                                                 │
│   • Limite le nombre de lignes pour SYSOUT                     │
│   • Protège contre les impressions infinies                    │
│   • Provoque ABEND si limite atteinte                          │
│                                                                 │
│   Exemple:                                                      │
│   //SYSPRINT DD SYSOUT=*,OUTLIM=10000                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-9 SYSOUT

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE SYSOUT                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SYSOUT=classe                                                 │
│                                                                 │
│   • Dirige la sortie vers le spool (JES2/JES3)                 │
│   • Les sorties sont consultables dans SDSF                    │
│                                                                 │
│   Classes courantes:                                            │
│   ──────────────────                                           │
│   SYSOUT=*     Meme classe que MSGCLASS du JOB                 │
│   SYSOUT=A     Classe A (souvent impression)                   │
│   SYSOUT=X     Classe X (held output)                          │
│   SYSOUT=H     Classe H (held)                                 │
│                                                                 │
│   Options supplementaires:                                      │
│   ────────────────────────                                     │
│   SYSOUT=(classe,,formulaire)                                   │
│   SYSOUT=(classe,programme-ecrivain)                            │
│                                                                 │
│   Exemples:                                                     │
│   //SYSPRINT DD SYSOUT=*                                       │
│   //RAPPORT  DD SYSOUT=A                                       │
│   //DEBUG    DD SYSOUT=X                                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-10 SYSIN (Données in-stream)

```
┌─────────────────────────────────────────────────────────────────┐
│                    DD * ET DD DATA (SYSIN)                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //ddname DD *                                                 │
│   donnees inline                                                │
│   /*                                                            │
│                                                                 │
│   • DD * : Données inline (colonnes 1-80)                      │
│   • DD DATA : Comme DD * mais permet // dans les donnees       │
│   • /* : Délimiteur de fin de donnees                          │
│                                                                 │
│   Options:                                                      │
│   ─────────                                                    │
│   DLM='xx'   Délimiteur personnalisé (2 caractères)            │
│                                                                 │
│   Exemples:                                                     │
│   //SYSIN DD *                                                  │
│     SORT FIELDS=(1,10,CH,A)                                    │
│   /*                                                            │
│                                                                 │
│   //SYSIN DD *,DLM='@@'                                        │
│     SELECT A                                                    │
│     FROM TABLE                                                  │
│   @@                                                            │
│                                                                 │
│   //CARTES DD DATA                                              │
│   //LIGNE1                                                      │
│   //LIGNE2                                                      │
│   /*                                                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-11 DUMMY

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE DUMMY                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //ddname DD DUMMY                                             │
│                                                                 │
│   • Simule un fichier vide                                     │
│   • En lecture : retourne fin de fichier immediate             │
│   • En ecriture : donnees ignorees (pas d'I/O)                 │
│   • Utile pour tests ou désactiver temporairement un fichier   │
│                                                                 │
│   Equivalent:                                                   │
│   //ddname DD DSN=NULLFILE                                      │
│                                                                 │
│   Exemples:                                                     │
│   //SYSIN  DD DUMMY                ◄── Pas de cartes controle  │
│   //OUTPUT DD DUMMY                ◄── Ignorer les sorties     │
│   //PRINT  DD DUMMY,DCB=(...       ◄── DCB peut etre spécifié  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-12 SYSUDUMP et SYSABEND

```
┌─────────────────────────────────────────────────────────────────┐
│                    DD DE DIAGNOSTIC                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //SYSUDUMP DD SYSOUT=*                                        │
│   • Dump utilisateur en cas d'ABEND                            │
│   • Contient : registres, PSW, trace, Working Storage          │
│   • Format lisible                                             │
│                                                                 │
│   //SYSABEND DD SYSOUT=*                                        │
│   • Dump complet en cas d'ABEND                                │
│   • Inclut les zones système                                   │
│   • Plus volumineux que SYSUDUMP                               │
│                                                                 │
│   //SYSMDUMP DD DSN=dump.dataset,...                            │
│   • Dump au format machine (pour IPCS)                         │
│                                                                 │
│   //CEEDUMP DD SYSOUT=*                                         │
│   • Dump LE (Language Environment) pour COBOL                  │
│   • Trace des appels, variables                                │
│                                                                 │
│   Exemple:                                                      │
│   //STEP1   EXEC PGM=MYPROG                                     │
│   //SYSUDUMP DD SYSOUT=*                                        │
│   //CEEDUMP  DD SYSOUT=*                                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-13 Concatenation

```
┌─────────────────────────────────────────────────────────────────┐
│                    CONCATENATION DE FICHIERS                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //ddname DD DSN=fichier1,DISP=SHR                             │
│   //       DD DSN=fichier2,DISP=SHR                             │
│   //       DD DSN=fichier3,DISP=SHR                             │
│                                                                 │
│   • Les fichiers sont lus séquentiellement comme un seul       │
│   • Le DDname n'est spécifié que sur la premiere DD            │
│   • Jusqu'a 255 datasets concatenes                            │
│   • DCB du premier fichier determine les caracteristiques      │
│                                                                 │
│   Exemple - Concatenation de bibliothèques:                     │
│   //STEPLIB DD DSN=USER.LOADLIB,DISP=SHR                       │
│   //        DD DSN=PROD.LOADLIB,DISP=SHR                       │
│   //        DD DSN=SYS1.LINKLIB,DISP=SHR                       │
│                                                                 │
│   Exemple - Concatenation de fichiers de donnees:               │
│   //INPUT  DD DSN=DATA.JANVIER,DISP=SHR                        │
│   //       DD DSN=DATA.FEVRIER,DISP=SHR                        │
│   //       DD DSN=DATA.MARS,DISP=SHR                           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-14 Fichiers TEMPORAIRES

```
┌─────────────────────────────────────────────────────────────────┐
│                    DATASETS TEMPORAIRES                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DSN=&&nom  ou  sans DSN                                       │
│                                                                 │
│   • Existent uniquement pendant le job                         │
│   • Supprimes automatiquement a la fin du job                  │
│   • && indique un nom temporaire                               │
│   • Peuvent etre passes entre steps avec DISP=(,PASS)          │
│                                                                 │
│   Exemples:                                                     │
│   //WORK DD DSN=&&TEMP,                                         │
│   //        DISP=(NEW,PASS),                                    │
│   //        SPACE=(TRK,(10,5))                                  │
│                                                                 │
│   //WORK DD DISP=(NEW,PASS),                                    │
│   //        SPACE=(TRK,(5,1))                                   │
│   (sans DSN = système genere un nom)                           │
│                                                                 │
│   //STEP2 DD DSN=&&TEMP,DISP=(OLD,DELETE)                      │
│   (reprise du fichier temporaire)                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-15 REFERBACK (Référence arriere)

```
┌─────────────────────────────────────────────────────────────────┐
│                    REFERENCE ARRIERE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Syntaxe: *.stepname.ddname  ou  *.procstep.stepname.ddname    │
│                                                                 │
│   Permet de référencer un dataset défini dans un step précédent │
│                                                                 │
│   Types de référence:                                           │
│   ───────────────────                                          │
│   DSN=*.STEP1.OUTPUT        ◄── Référence au DSN               │
│   VOL=REF=*.STEP1.OUTPUT    ◄── Référence au volume            │
│   DCB=*.STEP1.OUTPUT        ◄── Référence au DCB               │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1   EXEC PGM=PROG1                                      │
│   //OUTPUT  DD DSN=&&TEMP,DISP=(NEW,PASS),...                   │
│   //*                                                           │
│   //STEP2   EXEC PGM=PROG2                                      │
│   //INPUT   DD DSN=*.STEP1.OUTPUT,DISP=(OLD,DELETE)            │
│   //OUT2    DD DSN=MY.DATA,                                     │
│   //           DCB=*.STEP1.OUTPUT,  ◄── Memes caracteristiques │
│   //           DISP=(NEW,CATLG)                                 │
│                                                                 │
│   Dans une procédure:                                           │
│   //INPUT   DD DSN=*.COMPILE.SYSLIN,DISP=(OLD,DELETE)          │
│             ─────────────────────                               │
│             procstep.ddname                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

### I-2-d JCL PROCEDURE Statement

#### PROC Définition

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCEDURE JCL                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Une PROCEDURE est un ensemble de JCL réutilisable            │
│                                                                 │
│   Avantages:                                                    │
│   • Réutilisation du code JCL                                  │
│   • Maintenance centralisée                                    │
│   • Standardisation des traitements                            │
│   • Parametrisation flexible                                   │
│                                                                 │
│   Deux types:                                                   │
│   • In-stream : Definie dans le JCL                            │
│   • Cataloguée : Stockée dans une bibliothèque PROCLIB         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-d-1 PROC In-Stream

```jcl
//* PROCEDURE IN-STREAM
//*
//MYPROC   PROC
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&INFILE,DISP=SHR
//SYSUT2   DD DSN=&OUTFILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE)
//         PEND
//*
//* APPEL DE LA PROCEDURE
//STEP01   EXEC MYPROC,INFILE=DATA.INPUT,OUTFILE=DATA.OUTPUT
```

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROC IN-STREAM                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //procname PROC [param=default,...]                           │
│   //...      (corps de la procédure)                            │
│   //         PEND                                               │
│                                                                 │
│   • Definie au debut du JCL (avant le premier EXEC)            │
│   • Se termine par PEND                                        │
│   • Appelee avec EXEC procname ou EXEC PROC=procname           │
│   • Les paramètres sont passes a l'appel                       │
│                                                                 │
│   Note: Peu utilisée - preferer les procédures cataloguées     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-d-2 PROC Cataloguée

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROC CATALOGUEE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Stockée dans une bibliothèque PROCLIB                         │
│   • SYS1.PROCLIB (système)                                     │
│   • Bibliothèque utilisateur spécifiée par JCLLIB              │
│                                                                 │
│   Contenu du membre MYPROC dans la PROCLIB:                     │
│   ──────────────────────────────────────────                   │
│   //MYPROC   PROC INFILE=,OUTFILE=                             │
│   //STEP1    EXEC PGM=IEBGENER                                  │
│   //SYSPRINT DD SYSOUT=*                                        │
│   //SYSIN    DD DUMMY                                           │
│   //SYSUT1   DD DSN=&INFILE,DISP=SHR                           │
│   //SYSUT2   DD DSN=&OUTFILE,                                   │
│   //            DISP=(NEW,CATLG,DELETE),                        │
│   //            SPACE=(TRK,(10,5),RLSE)                         │
│                                                                 │
│   Appel dans le JCL:                                            │
│   ──────────────────                                           │
│   //MYJOB    JOB ...                                            │
│   //         JCLLIB ORDER=USER.PROCLIB                         │
│   //STEP01   EXEC MYPROC,INFILE=DATA.IN,OUTFILE=DATA.OUT       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

### I-2-e Paramètres symboliques

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRES SYMBOLIQUES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Syntaxe: &nom  ou  &nom.suffixe                               │
│                                                                 │
│   • Permettent de paramètrer les procédures                    │
│   • Remplaces a l'execution par les valeurs fournies           │
│   • Le point (.) separe le symbole du texte suivant            │
│                                                                 │
│   Définition avec valeur par défaut:                            │
│   //MYPROC PROC ENV=TEST,DATE=&LYYMMDD                         │
│                                                                 │
│   Symboles système predifinis:                                  │
│   ────────────────────────────                                 │
│   &SYSUID     Userid du soumetteur                             │
│   &SYSDATE    Date au format yy.ddd                            │
│   &LYYMMDD    Date au format yyyymmdd                          │
│   &SYSTIME    Heure au format hh.mm.ss                         │
│   &SYSJOBNAME Nom du job                                       │
│   &SYSSTEP    Nom du step courant                              │
│                                                                 │
│   Exemple:                                                      │
│   //OUTPUT DD DSN=&SYSUID..DATA.&ENV..D&LYYMMDD,               │
│   //          DISP=(NEW,CATLG)                                  │
│   Résultat: FTEST.DATA.TEST.D20241205                          │
│                                                                 │
│   Note: && pour un & litteral dans les donnees                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Instruction SET :**

```jcl
//* Définition de variables avec SET
//         SET ENV=PROD
//         SET DATE=20241205
//         SET HLQUAL=PROD.BATCH
//*
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=&HLQUAL..DATA.INPUT,DISP=SHR
//OUTPUT   DD DSN=&HLQUAL..RAPPORT.&DATE,
//            DISP=(NEW,CATLG,DELETE)
```

---

### I-2-f JCL - Bibliothèques de base

#### I-2-f-1 Declaration JOBLIB

```
┌─────────────────────────────────────────────────────────────────┐
│                    JOBLIB                                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //JOBLIB DD DSN=bibliothèque,DISP=SHR                         │
│                                                                 │
│   • Place immediatement apres la carte JOB                     │
│   • Définit les bibliothèques de programmes pour TOUT le job   │
│   • Le système cherche les programmes dans l'ordre:            │
│     1. JOBLIB (si spécifiée)                                   │
│     2. STEPLIB (si spécifiée dans le step)                     │
│     3. Bibliothèques système (LINKLIB, etc.)                   │
│                                                                 │
│   Exemple:                                                      │
│   //MYJOB   JOB ...                                             │
│   //JOBLIB  DD DSN=USER.LOADLIB,DISP=SHR                       │
│   //        DD DSN=PROD.LOADLIB,DISP=SHR                       │
│   //STEP1   EXEC PGM=MYPROG                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-f-2 Declaration STEPLIB

```
┌─────────────────────────────────────────────────────────────────┐
│                    STEPLIB                                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //STEPLIB DD DSN=bibliothèque,DISP=SHR                        │
│                                                                 │
│   • Définit les bibliothèques de programmes pour UN step       │
│   • Écrase JOBLIB pour ce step uniquement                      │
│   • Plus specifique que JOBLIB                                 │
│                                                                 │
│   Exemple:                                                      │
│   //MYJOB   JOB ...                                             │
│   //JOBLIB  DD DSN=PROD.LOADLIB,DISP=SHR                       │
│   //*                                                           │
│   //STEP1   EXEC PGM=PROG1                                      │
│   //* Utilise JOBLIB                                            │
│   //*                                                           │
│   //STEP2   EXEC PGM=PROG2                                      │
│   //STEPLIB DD DSN=TEST.LOADLIB,DISP=SHR                       │
│   //* Utilise STEPLIB (ignore JOBLIB)                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-f-3 Declaration JCLLIB

```
┌─────────────────────────────────────────────────────────────────┐
│                    JCLLIB                                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   // JCLLIB ORDER=(biblio1,biblio2,...)                         │
│                                                                 │
│   • Définit les bibliothèques de PROCEDURES et INCLUDE         │
│   • Place apres JOB, avant tout EXEC                           │
│   • Jusqu'a 15 bibliothèques                                   │
│   • Recherche dans l'ordre spécifié                            │
│                                                                 │
│   Exemple:                                                      │
│   //MYJOB   JOB ...                                             │
│   //        JCLLIB ORDER=(USER.PROCLIB,                        │
│   //                      PROD.PROCLIB,                         │
│   //                      SYS1.PROCLIB)                         │
│   //STEP1   EXEC MYPROC                                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-f-4 Difference entre JOBLIB et JCLLIB

```
┌─────────────────────────────────────────────────────────────────┐
│                    JOBLIB vs JCLLIB                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│                     JOBLIB              JCLLIB                   │
│   ─────────────────────────────────────────────────────────    │
│   Contenu        Load modules         JCL (PROC, INCLUDE)      │
│                  (programmes)         (procédures)              │
│                                                                 │
│   Position       Apres JOB            Apres JOB                 │
│                                       Avant 1er EXEC            │
│                                                                 │
│   Syntaxe        //JOBLIB DD ...      // JCLLIB ORDER=...      │
│                                                                 │
│   Portée         Tout le job          Tout le job               │
│                  (sauf si STEPLIB)                              │
│                                                                 │
│   Utilisation    Recherche PGM=       Recherche PROC=           │
│                                       et INCLUDE                │
│                                                                 │
│   Equivalent     STEPLIB par step     Pas d'equivalent         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Exemple complet :**

```jcl
//MYJOB    JOB (ACCT),'EXEMPLE COMPLET',
//             CLASS=A,MSGCLASS=X
//*
//* JCLLIB pour les procédures
//         JCLLIB ORDER=(USER.PROCLIB,PROD.PROCLIB)
//*
//* JOBLIB pour les programmes
//JOBLIB   DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=PROD.LOADLIB,DISP=SHR
//*
//* Step 1 : utilisé JOBLIB
//STEP1    EXEC PGM=MYPROG1
//INPUT    DD DSN=DATA.INPUT,DISP=SHR
//*
//* Step 2 : utilisé STEPLIB (écrase JOBLIB)
//STEP2    EXEC PGM=MYPROG2
//STEPLIB  DD DSN=TEST.LOADLIB,DISP=SHR
//INPUT    DD DSN=DATA.INPUT,DISP=SHR
//*
//* Step 3 : appel procédure (trouvée via JCLLIB)
//STEP3    EXEC MYPROC
//
```

---

## I-3 Codes Retour et ABEND

### I-3-1 Codes retour standards

```
┌─────────────────────────────────────────────────────────────────┐
│                    CODES RETOUR (RETURN CODES)                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Le code retour indique le résultat de l'execution d'un step  │
│                                                                 │
│   CODE    SIGNIFICATION                                         │
│   ────    ─────────────                                        │
│    0      Succès complet                                       │
│    4      Warning (avertissement) - traitement OK              │
│    8      Erreur - traitement partiel ou échoué                │
│   12      Erreur grave                                         │
│   16      Erreur critique                                      │
│                                                                 │
│   Utilisation avec COND :                                       │
│   ───────────────────────                                      │
│   COND=(4,LT)     Ne pas executer si RC < 4                    │
│   COND=(8,LE)     Ne pas executer si RC <= 8                   │
│   COND=EVEN       Exécuter meme si ABEND précédent             │
│   COND=ONLY       Exécuter seulement si ABEND précédent        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-3-2 Codes ABEND système

```
┌─────────────────────────────────────────────────────────────────┐
│                    CODES ABEND SYSTEME (Sxxx)                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CODE    DESCRIPTION                              CAUSE TYPIQUE│
│   ────    ───────────                              ─────────────│
│   S001    Erreur I/O                               Fichier      │
│   S013    Erreur ouverture fichier                 DCB/DSN      │
│   S0C1    Operation invalide                       Code machine │
│   S0C4    Violation protection mémoire             Pointeur     │
│   S0C7    Données non numériques                   MOVE/COMPUTE │
│   S0CB    Division par zero                        DIVIDE       │
│   S0D3    Erreur virgule flottante                 Calcul       │
│   S106    Module introuvable (FETCH)               Programme    │
│   S122    Operateur a annulé le job                Operateur    │
│   S137    Fin de volume inattend                   Bande/Disque │
│   S213    Erreur ouverture fichier sortie          DISP/SPACE   │
│   S222    Job annulé par operateur                 Operateur    │
│   S237    Fin de volume (bande)                    Bande        │
│   S306    Module introuvable en mémoire            Link-edit    │
│   S322    Temps CPU dépassé (TIME)                 Bouclé infini│
│   S522    Attente trop longue (WAIT)               Deadlock     │
│   S613    Erreur I/O magnetique                    Hardware     │
│   S706    Module introuvable (LOAD)                STEPLIB      │
│   S713    Erreur ouverture fichier bande           Bande        │
│   S722    Lignes imprimees dépassées               OUTLIM       │
│   S806    Module introuvable (LINK/ATTACH)         JOBLIB       │
│   S837    Fin de volume disque                     Espace plein │
│   S913    Erreur sécurité RACF                     Droits acces │
│   SB14    Erreur QSAM close                        Fichier      │
│   SB37    Espace disque insuffisant                SPACE        │
│   SD37    Espace primaire insuffisant              SPACE        │
│   SE37    Espace secondaire insuffisant            Extensions   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-3-3 Codes ABEND utilisateur

```
┌─────────────────────────────────────────────────────────────────┐
│                    CODES ABEND UTILISATEUR (Unnnn)               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Les codes Unnnn sont définis par les programmes utilisateur   │
│                                                                 │
│   CODE    DESCRIPTION                                           │
│   ────    ───────────                                          │
│   U0001   Erreur applicative generale                          │
│   U0016   Erreur COBOL (STOP RUN avec code)                    │
│   U1000+  Erreurs définies par l'application                   │
│   U4038   Erreur Language Environment (LE)                     │
│   U4093   Fin anormale demandée                                │
│                                                                 │
│   En COBOL, generation d'ABEND :                                │
│   ───────────────────────────────                              │
│   CALL 'CEE3ABD' USING abort-code timing                       │
│   ou STOP RUN avec RETURN-CODE                                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-3-4 Diagnostic et resolution

```
┌─────────────────────────────────────────────────────────────────┐
│                    DIAGNOSTIC DES ABEND                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ETAPES DE DIAGNOSTIC :                                        │
│   ───────────────────────                                      │
│   1. Consulter le JESMSGLG (messages JES)                      │
│   2. Examiner le JESYSMSG (messages système)                   │
│   3. Lire le SYSOUT du step en erreur                          │
│   4. Analyser le dump (SYSUDUMP/CEEDUMP)                       │
│                                                                 │
│   SOLUTIONS COURANTES :                                         │
│   ─────────────────────                                        │
│   S0C7  → Vérifier les donnees en entree                       │
│   S806  → Vérifier STEPLIB/JOBLIB                              │
│   S913  → Vérifier les droits RACF                             │
│   SB37  → Augmenter SPACE ou libérer de l'espace               │
│   S322  → Augmenter TIME ou corriger bouclé                    │
│   S013  → Vérifier DSN et DISP                                 │
│                                                                 │
│   MESSAGES UTILES :                                             │
│   ─────────────────                                            │
│   IEF450I  jobname stepname - ABEND=Sxxx                       │
│   IEF451I  jobname stepname - ABEND=Unnnn                      │
│   IGZ0xxxW Messages COBOL runtime                               │
│   CEE3xxx  Messages Language Environment                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Synthèse

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLES DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CARTE JOB                                                     │
│   ─────────                                                    │
│   • Identifie le travail batch                                 │
│   • Paramètres: CLASS, MSGCLASS, MSGLEVEL, NOTIFY, REGION      │
│   • Une seule carte JOB par travail                            │
│                                                                 │
│   CARTE EXEC                                                    │
│   ──────────                                                   │
│   • Exécute un programme (PGM=) ou procédure (PROC=)           │
│   • Paramètres: REGION, TIME, PARM, COND                       │
│   • Un ou plusieurs steps par job                              │
│                                                                 │
│   CARTE DD                                                      │
│   ────────                                                     │
│   • Définit les fichiers utilisés                              │
│   • DSN : nom du dataset                                       │
│   • DISP : statut et disposition                               │
│   • DCB : caracteristiques (RECFM, LRECL, BLKSIZE)             │
│   • SPACE : allocation disque                                  │
│   • SYSOUT : sortie vers spool                                 │
│                                                                 │
│   BIBLIOTHEQUES                                                 │
│   ─────────────                                                │
│   • JOBLIB/STEPLIB : bibliothèques de programmes               │
│   • JCLLIB : bibliothèques de procédures                       │
│                                                                 │
│   PROCEDURES                                                    │
│   ───────────                                                  │
│   • In-stream ou cataloguées                                   │
│   • Paramètres symboliques (&param)                            │
│   • Symboles système (&SYSUID, &LYYMMDD, etc.)                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-mémoire JCL

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MÉMOIRE JCL                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   STRUCTURE DE BASE                                             │
│   ─────────────────────────────────────────────────────────    │
│   //JOBNAME  JOB (acct),'nom',CLASS=A,MSGCLASS=X               │
│   //STEP1    EXEC PGM=programme                                 │
│   //DDNAME   DD   DSN=dataset,DISP=SHR                         │
│                                                                 │
│   DISP - DISPOSITIONS COURANTES                                 │
│   ─────────────────────────────────────────────────────────    │
│   SHR              Lecture partagée                            │
│   OLD              Acces exclusif                              │
│   (NEW,CATLG,DEL)  Creation, catalogué si OK                   │
│   (NEW,PASS)       Creation, passer au step suivant            │
│   (MOD,CATLG)      Ajout/creation                              │
│                                                                 │
│   DCB - CARACTERISTIQUES                                        │
│   ─────────────────────────────────────────────────────────    │
│   RECFM=FB         Fixe bloque                                 │
│   RECFM=VB         Variable bloque                             │
│   LRECL=80         Longueur enregistrement                     │
│   BLKSIZE=0        Systeme optimise                            │
│                                                                 │
│   SPACE - ALLOCATION                                            │
│   ─────────────────────────────────────────────────────────    │
│   (TRK,(10,5))     10 pistes + 5 extensions                    │
│   (CYL,(5,1,10))   5 cyl + 1 ext + 10 blocs PDS                │
│   ,RLSE            Libérer espace non utilisé                  │
│                                                                 │
│   DD SPECIALES                                                  │
│   ─────────────────────────────────────────────────────────    │
│   SYSOUT=*         Sortie spool                                │
│   DUMMY            Fichier vide/ignore                         │
│   *                Données inline                              │
│   DSN=&&TEMP       Fichier temporaire                          │
│                                                                 │
│   SYMBOLES SYSTEME                                              │
│   ─────────────────────────────────────────────────────────    │
│   &SYSUID          Userid                                      │
│   &LYYMMDD         Date AAAAMMJJ                               │
│   &SYSTIME         Heure HH.MM.SS                              │
│                                                                 │
│   CODES ABEND FREQUENTS                                         │
│   ─────────────────────────────────────────────────────────    │
│   S0C7   Données non numériques                               │
│   S0C4   Violation mémoire                                    │
│   S806   Module introuvable                                   │
│   S913   Erreur RACF (sécurité)                               │
│   SB37   Espace disque insuffisant                            │
│   S322   Temps CPU dépassé                                    │
│   S013   Erreur ouverture fichier                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Fichiers spéciaux et paramètres](02-fichiers-paramètres.md) |
