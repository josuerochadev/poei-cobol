# Chapitre I - Traitement des cartes JOB, EXEC et DD

## Introduction

Ce chapitre traite des trois cartes fondamentales du JCL (Job Control Language) qui permettent a l'utilisateur de definir son traitement batch sur z/OS :

1. **La carte JOB** - Identifie le travail a traiter
2. **La carte EXEC** - Identifie le programme a executer
3. **La carte DD** - Identifie et localise les donnees concernees

Ces trois cartes constituent la **base de tous les travaux batch** sur mainframe.

---

## I-1 Introduction au Job Control Language (JCL)

### I-1-1 Qu'est-ce que le JCL ?

Le **JCL (Job Control Language)** est le langage de controle des travaux sur z/OS. Il permet de :

```
┌─────────────────────────────────────────────────────────────────┐
│                    ROLE DU JCL                                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. DEFINIR UN TRAVAIL (JOB)                                   │
│      • Nom du job                                               │
│      • Classe d'execution                                       │
│      • Priorite                                                 │
│      • Comptabilite                                             │
│                                                                 │
│   2. SPECIFIER LES PROGRAMMES A EXECUTER                        │
│      • Nom du programme                                         │
│      • Parametres d'execution                                   │
│      • Ressources memoire                                       │
│                                                                 │
│   3. DECRIRE LES FICHIERS UTILISES                              │
│      • Nom et localisation des datasets                         │
│      • Caracteristiques (organisation, format, taille)          │
│      • Mode d'acces (lecture, ecriture, creation)               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-1-2 Structure generale d'un JCL

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
│   REGLES:                                                       │
│   • Colonnes 1-2  : Toujours //                                │
│   • Colonne 3     : Debut du nom (ou espace si pas de nom)     │
│   • Colonnes 3-10 : Nom (1-8 caracteres alphanumeriques)       │
│   • Apres le nom  : Au moins un espace                         │
│   • Operation     : JOB, EXEC, DD, etc.                        │
│   • Operandes     : Parametres separes par des virgules        │
│   • Colonne 72    : Caractere de continuation (non-blanc)      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### I-1-3 Types d'instructions JCL

| Type | Description | Exemple |
|------|-------------|---------|
| **JOB** | Debut de travail | `//MYJOB JOB ...` |
| **EXEC** | Execution programme | `//STEP1 EXEC PGM=PROG1` |
| **DD** | Definition de donnees | `//INPUT DD DSN=...` |
| **PROC** | Definition procedure | `//MYPROC PROC ...` |
| **PEND** | Fin de procedure | `// PEND` |
| **SET** | Variable symbolique | `// SET VAR=VALUE` |
| **IF/THEN/ELSE/ENDIF** | Conditionnel | `// IF RC=0 THEN` |
| **JCLLIB** | Bibliotheque JCL | `// JCLLIB ORDER=...` |
| **INCLUDE** | Inclusion membre | `// INCLUDE MEMBER=...` |
| **/\*** | Delimiteur donnees | `/*` |
| **//\*** | Commentaire | `//* Ceci est un commentaire` |

### I-1-4 Regles de syntaxe

```
┌─────────────────────────────────────────────────────────────────┐
│                    REGLES DE SYNTAXE JCL                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   1. MAJUSCULES                                                 │
│      Le JCL est en MAJUSCULES (sauf contenu entre apostrophes)  │
│                                                                 │
│   2. DEBUT DE LIGNE                                             │
│      // en colonnes 1-2 (obligatoire)                          │
│                                                                 │
│   3. NOMS                                                       │
│      • 1 a 8 caracteres                                        │
│      • Commence par une lettre ou @, #, $                      │
│      • Alphanumerique + @, #, $                                │
│                                                                 │
│   4. CONTINUATION                                               │
│      • Interrompre apres une virgule                           │
│      • Caractere non-blanc en colonne 72 (optionnel)           │
│      • Continuer entre colonnes 4 et 16 sur ligne suivante     │
│                                                                 │
│   5. COMMENTAIRES                                               │
│      • //* en colonnes 1-3                                     │
│      • Ou apres les operandes (separe par espace)              │
│                                                                 │
│   6. FIN DE JOB                                                 │
│      • // seul en colonnes 1-2 (null statement)                │
│      • Ou debut d'un nouveau JOB                               │
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

La carte **JOB** est la premiere instruction de tout travail batch. Elle identifie le job aupres du systeme.

#### Syntaxe generale

```
//jobname JOB accounting,'programmer-name',parametres...
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
│   • Identifie une instruction JCL pour le systeme              │
│   • Distingue le JCL des donnees in-stream                     │
│                                                                 │
│   Exemples:                                                     │
│   //MYJOB    JOB  ...      ◄── Carte JCL                       │
│   DONNEES                   ◄── Donnees (pas de //)            │
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
│   Position : Colonnes 3 a 10 (max 8 caracteres)                │
│                                                                 │
│   REGLES:                                                       │
│   • Commence en colonne 3                                      │
│   • 1 a 8 caracteres alphanumeriques                           │
│   • Premier caractere : A-Z ou @, #, $                         │
│   • Caracteres suivants : A-Z, 0-9, @, #, $                    │
│   • Le nom doit etre UNIQUE dans le systeme                    │
│                                                                 │
│   CONVENTIONS COURANTES:                                        │
│   • Prefixe = Userid (ex: FTEST)                               │
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
│   //MYVERYLONGJOB  ◄── Plus de 8 caracteres                    │
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

#### I-2-a-iv Champ d'operande (parametres)

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

**Parametres a mots-cles du JOB :**

| Parametre | Description | Exemple |
|-----------|-------------|---------|
| **CLASS** | Classe d'execution (A-Z, 0-9) | `CLASS=A` |
| **MSGCLASS** | Classe de sortie des messages | `MSGCLASS=X` |
| **MSGLEVEL** | Niveau de messages (statements,allocation) | `MSGLEVEL=(1,1)` |
| **NOTIFY** | Userid a notifier | `NOTIFY=&SYSUID` |
| **REGION** | Memoire pour le job | `REGION=4M` |
| **TIME** | Temps CPU maximum | `TIME=(1,30)` ou `TIME=NOLIMIT` |
| **TYPRUN** | Type d'execution | `TYPRUN=SCAN` ou `TYPRUN=HOLD` |
| **PRTY** | Priorite (0-15) | `PRTY=8` |
| **COND** | Condition de terminaison | `COND=(4,LT)` |
| **RESTART** | Point de reprise | `RESTART=STEP020` |
| **USER** | Userid d'execution | `USER=FTEST` |
| **PASSWORD** | Mot de passe (obsolete) | - |

**Valeurs de MSGLEVEL :**

```
┌─────────────────────────────────────────────────────────────────┐
│                    MSGLEVEL=(stmt,alloc)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Premier parametre (stmt) - Affichage des instructions :       │
│   ─────────────────────────────────────────────────────────     │
│   0 = Seulement la carte JOB                                   │
│   1 = Toutes les instructions JCL et JES                       │
│   2 = Seulement les instructions JCL                           │
│                                                                 │
│   Second parametre (alloc) - Messages d'allocation :            │
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
| `TYPRUN=SCAN` | Verification syntaxe uniquement (pas d'execution) |
| `TYPRUN=HOLD` | Job en attente (necessite release) |
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

//* Exemple 3 : JOB complet avec parametres
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

La carte **EXEC** definit le programme ou la procedure a executer dans un step.

#### Syntaxe generale

```
//stepname EXEC PGM=program   ou   //stepname EXEC PROC=procedure
```

#### Parametres du EXEC

##### I-2-b-i Parametre PGM

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PGM                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PGM=program-name                                              │
│                                                                 │
│   • Specifie le programme a executer                           │
│   • Le programme doit etre dans une bibliotheque systeme       │
│     ou specifiee par JOBLIB/STEPLIB                            │
│   • Nom de 1 a 8 caracteres                                    │
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

##### I-2-b-ii Parametre PROC

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PROC                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PROC=procedure-name  ou simplement  procedure-name            │
│                                                                 │
│   • Specifie une procedure cataloguee a executer               │
│   • La procedure doit etre dans une bibliotheque PROCLIB       │
│   • Le mot-cle PROC est optionnel                              │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PROC=MYPROC                                      │
│   //STEP1 EXEC MYPROC          ◄── Equivalent                  │
│   //STEP1 EXEC COBOLCL         ◄── Procedure de compilation    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-iii Parametre REGION

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE REGION                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   REGION=valeur                                                 │
│                                                                 │
│   • Specifie la memoire virtuelle pour le step                 │
│   • Peut etre en K (kilobytes) ou M (megabytes)                │
│   • REGION=0M = memoire illimitee (jusqu'au max systeme)       │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG,REGION=4M                              │
│   //STEP2 EXEC PGM=PROG,REGION=2048K                           │
│   //STEP3 EXEC PGM=PROG,REGION=0M    ◄── Illimitee             │
│                                                                 │
│   Note : REGION sur EXEC ecrase REGION sur JOB pour ce step    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-iv Parametre PARM

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE PARM                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   PARM='valeur'  ou  PARM=(val1,val2,...)                       │
│                                                                 │
│   • Passe des parametres au programme                          │
│   • Maximum 100 caracteres                                     │
│   • Utiliser apostrophes si caracteres speciaux                │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=MYPROG,PARM='DEBUG'                         │
│   //STEP2 EXEC PGM=MYPROG,PARM='DATE=2024,MODE=TEST'           │
│   //STEP3 EXEC PGM=SORT,PARM='MSG=AP'                          │
│   //STEP4 EXEC PGM=IGYCRCTL,PARM='LIST,MAP'                    │
│                                                                 │
│   Pour COBOL, le parametre est recu dans :                      │
│   01 WS-PARM.                                                   │
│      05 WS-PARM-LENGTH PIC S9(4) COMP.                         │
│      05 WS-PARM-DATA   PIC X(100).                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-v Parametre ADDRSPC

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE ADDRSPC                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   ADDRSPC=VIRT | REAL                                           │
│                                                                 │
│   • VIRT = Memoire virtuelle (defaut)                          │
│   • REAL = Memoire reelle (pour programmes speciaux)           │
│                                                                 │
│   Exemple:                                                      │
│   //STEP1 EXEC PGM=MYPROG,ADDRSPC=REAL                         │
│                                                                 │
│   Note : REAL rarement utilise, necessite privileges           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-vi Parametre MEMLIMIT

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE MEMLIMIT                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   MEMLIMIT=valeur                                               │
│                                                                 │
│   • Limite de memoire au-dessus de la barre (>2GB)             │
│   • Pour programmes 64-bit                                     │
│   • MEMLIMIT=NOLIMIT pour pas de limite                        │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG64,MEMLIMIT=4G                          │
│   //STEP2 EXEC PGM=PROG64,MEMLIMIT=NOLIMIT                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-vii Parametre TIME

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE TIME                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   TIME=(minutes,secondes)  ou  TIME=minutes  ou  TIME=NOLIMIT   │
│                                                                 │
│   • Limite le temps CPU pour le step                           │
│   • Protege contre les boucles infinies                        │
│   • TIME=NOLIMIT ou TIME=1440 = pas de limite                  │
│                                                                 │
│   Exemples:                                                     │
│   //STEP1 EXEC PGM=PROG,TIME=5          ◄── 5 minutes          │
│   //STEP2 EXEC PGM=PROG,TIME=(1,30)     ◄── 1 min 30 sec       │
│   //STEP3 EXEC PGM=PROG,TIME=NOLIMIT    ◄── Illimite           │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

##### I-2-b-viii Parametre COND

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE COND                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   COND=(code,operateur)  ou  COND=((c1,op1),(c2,op2),...)       │
│                                                                 │
│   Conditionne l'execution du step selon les return codes       │
│   precedents. Le step est BYPASSE si la condition est VRAIE.   │
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
│   COND=EVEN           Executer meme si ABEND precedent         │
│   COND=ONLY           Executer SEULEMENT si ABEND precedent    │
│                                                                 │
│   Note : Preferer IF/THEN/ELSE pour plus de clarte             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### Exemple de cartes EXEC

```jcl
//* Step simple
//STEP010  EXEC PGM=IEFBR14

//* Step avec parametres
//STEP020  EXEC PGM=MYPROG,
//             REGION=4M,
//             TIME=5,
//             PARM='MODE=TEST'

//* Step conditionnel
//STEP030  EXEC PGM=RAPPORT,COND=(4,LT)

//* Appel de procedure
//STEP040  EXEC PROC=COBOLCL,
//             PARM.COB='LIST,MAP'
```

---

### I-2-c Structure des champs du DD statement

La carte **DD (Data Definition)** definit les fichiers utilises par le programme. C'est la carte la plus riche en parametres.

#### Syntaxe generale

```
//ddname   DD   parametres...
```

#### I-2-c-1 DSN (Data Set Name)

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE DSN (ou DSNAME)                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   DSN=dataset-name                                              │
│                                                                 │
│   • Nom du dataset (fichier) a utiliser                        │
│   • Format : qualificateur1.qualificateur2...                  │
│   • Chaque qualificateur : 1-8 caracteres                      │
│   • Maximum 44 caracteres au total                             │
│                                                                 │
│   Types de noms:                                                │
│   ─────────────                                                 │
│   DSN=FTEST.DATA.FILE           ◄── Dataset permanent          │
│   DSN=FTEST.PDS(MEMBRE)         ◄── Membre de PDS              │
│   DSN=&&TEMP                    ◄── Dataset temporaire         │
│   DSN=*.STEP1.DDNAME            ◄── Reference arriere          │
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
│   NEW      Le dataset sera cree par ce step                    │
│   OLD      Dataset existant, acces exclusif                    │
│   SHR      Dataset existant, acces partage                     │
│   MOD      Ajout en fin de fichier (ou creation si inexistant) │
│                                                                 │
│   FIN NORMALE (si step OK) :                                    │
│   ──────────────────────────                                    │
│   DELETE   Supprimer le dataset                                │
│   KEEP     Conserver (non catalogue)                           │
│   PASS     Passer au step suivant                              │
│   CATLG    Cataloguer le dataset                               │
│   UNCATLG  Decataloguer                                        │
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
| `DISP=SHR` | Lecture partagee | Fichiers en lecture |
| `DISP=OLD` | Acces exclusif | Mise a jour fichier |
| `DISP=(NEW,CATLG,DELETE)` | Creation, catalogue si OK | Nouveaux fichiers permanents |
| `DISP=(NEW,PASS)` | Creation, passage au step suivant | Fichiers intermediaires |
| `DISP=(NEW,DELETE)` | Creation, suppression | Fichiers de travail |
| `DISP=(MOD,CATLG)` | Ajout/creation, catalogue | Log, cumul |
| `DISP=(OLD,DELETE)` | Lecture exclusive puis suppression | Traitement puis purge |

```
┌─────────────────────────────────────────────────────────────────┐
│                    EXEMPLES DISP                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //INPUT  DD DSN=DATA.INPUT,DISP=SHR                          │
│   //       Lecture partagee d'un fichier existant              │
│                                                                 │
│   //OUTPUT DD DSN=DATA.OUTPUT,                                  │
│   //          DISP=(NEW,CATLG,DELETE)                           │
│   //       Creation, catalogue si OK, supprime si ABEND        │
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
│   DCB=(sous-parametres)                                         │
│                                                                 │
│   Definit les caracteristiques physiques du dataset            │
│                                                                 │
│   RECFM = Record Format (format d'enregistrement)              │
│   ────────────────────────────────────────────────              │
│   F   = Fixe                                                   │
│   FB  = Fixe bloque                                            │
│   V   = Variable                                               │
│   VB  = Variable bloque                                        │
│   U   = Undefined (load modules)                               │
│   FA  = Fixe avec ASA (caracteres de controle imprimante)     │
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
│   BLKSIZE=0 : systeme calcule optimal                          │
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

//* Reference DCB d'un autre fichier
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
│   Specifie le type de peripherique pour le dataset             │
│                                                                 │
│   Valeurs courantes:                                            │
│   ──────────────────                                           │
│   UNIT=SYSDA         Disque systeme (le plus courant)          │
│   UNIT=SYSALLDA      Tous disques disponibles                  │
│   UNIT=3390          Modele de disque specifique               │
│   UNIT=TAPE          Bande magnetique                          │
│   UNIT=3480          Cartouche                                 │
│   UNIT=VIO           Virtual I/O (memoire)                     │
│                                                                 │
│   Exemples:                                                     │
│   //OUTPUT DD DSN=...,UNIT=SYSDA                               │
│   //BACKUP DD DSN=...,UNIT=TAPE                                │
│   //TEMP   DD DSN=&&T,UNIT=VIO    ◄── Tres rapide              │
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
│   Specifie le volume (disque/bande) a utiliser                 │
│                                                                 │
│   Sous-parametres:                                              │
│   ─────────────────                                            │
│   SER=volser     Volume serial (nom du disque)                 │
│   REF=dsname     Meme volume qu'un autre dataset               │
│   PRIVATE       Volume prive (non partage)                     │
│   RETAIN        Conserver montage jusqu'a fin job              │
│                                                                 │
│   Exemples:                                                     │
│   //DD1 DD DSN=...,VOL=SER=PROD01                              │
│   //DD2 DD DSN=...,VOL=SER=(TAPE01,TAPE02)   ◄── Multi-volumes │
│   //DD3 DD DSN=...,VOL=REF=*.STEP1.DD1       ◄── Reference     │
│                                                                 │
│   Note : Rarement necessaire si dataset catalogue              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-6 SPACE

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRE SPACE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   SPACE=(unite,(primaire,secondaire,repertoire),RLSE)           │
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
│   primaire   Espace initial alloue                             │
│   secondaire Espace additionnel (jusqu'a 15 extensions)        │
│   repertoire Blocs pour repertoire PDS (uniquement PDS)        │
│                                                                 │
│   OPTIONS :                                                     │
│   ──────────                                                   │
│   RLSE       Liberer espace non utilise a la fermeture        │
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

//* Fichier PDS : 5 cylindres, 10 blocs repertoire
//PDS DD DSN=...,SPACE=(CYL,(5,1,10))

//* Allocation en nombre de blocs
//DATA DD DSN=...,SPACE=(800,(1000,100),RLSE)

//* Tres gros fichier : 100 cylindres
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
│   • Longueur de la cle pour fichiers VSAM KSDS                 │
│                                                                 │
│   KEYOFF=position                                               │
│   • Position de la cle dans l'enregistrement                   │
│                                                                 │
│   RECORG=organisation                                           │
│   • KS = KSDS (Key Sequenced)                                  │
│   • ES = ESDS (Entry Sequenced)                                │
│   • RR = RRDS (Relative Record)                                │
│   • LS = Linear (fichiers lineaires)                           │
│                                                                 │
│   Note : Ces parametres sont rarement necessaires car          │
│   les caracteristiques sont definies a la creation du fichier  │
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
│   • Protege contre les impressions infinies                    │
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

#### I-2-c-10 SYSIN (Donnees in-stream)

```
┌─────────────────────────────────────────────────────────────────┐
│                    DD * ET DD DATA (SYSIN)                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //ddname DD *                                                 │
│   donnees inline                                                │
│   /*                                                            │
│                                                                 │
│   • DD * : Donnees inline (colonnes 1-80)                      │
│   • DD DATA : Comme DD * mais permet // dans les donnees       │
│   • /* : Delimiteur de fin de donnees                          │
│                                                                 │
│   Options:                                                      │
│   ─────────                                                    │
│   DLM='xx'   Delimiteur personnalise (2 caracteres)            │
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
│   • Utile pour tests ou desactiver temporairement un fichier   │
│                                                                 │
│   Equivalent:                                                   │
│   //ddname DD DSN=NULLFILE                                      │
│                                                                 │
│   Exemples:                                                     │
│   //SYSIN  DD DUMMY                ◄── Pas de cartes controle  │
│   //OUTPUT DD DUMMY                ◄── Ignorer les sorties     │
│   //PRINT  DD DUMMY,DCB=(...       ◄── DCB peut etre specifie  │
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
│   • Inclut les zones systeme                                   │
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
│   • Les fichiers sont lus sequentiellement comme un seul       │
│   • Le DDname n'est specifie que sur la premiere DD            │
│   • Jusqu'a 255 datasets concatenes                            │
│   • DCB du premier fichier determine les caracteristiques      │
│                                                                 │
│   Exemple - Concatenation de bibliotheques:                     │
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
│   (sans DSN = systeme genere un nom)                           │
│                                                                 │
│   //STEP2 DD DSN=&&TEMP,DISP=(OLD,DELETE)                      │
│   (reprise du fichier temporaire)                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-c-15 REFERBACK (Reference arriere)

```
┌─────────────────────────────────────────────────────────────────┐
│                    REFERENCE ARRIERE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Syntaxe: *.stepname.ddname  ou  *.procstep.stepname.ddname    │
│                                                                 │
│   Permet de referencer un dataset defini dans un step precedent │
│                                                                 │
│   Types de reference:                                           │
│   ───────────────────                                          │
│   DSN=*.STEP1.OUTPUT        ◄── Reference au DSN               │
│   VOL=REF=*.STEP1.OUTPUT    ◄── Reference au volume            │
│   DCB=*.STEP1.OUTPUT        ◄── Reference au DCB               │
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
│   Dans une procedure:                                           │
│   //INPUT   DD DSN=*.COMPILE.SYSLIN,DISP=(OLD,DELETE)          │
│             ─────────────────────                               │
│             procstep.ddname                                     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

### I-2-d JCL PROCEDURE Statement

#### PROC Definition

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROCEDURE JCL                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Une PROCEDURE est un ensemble de JCL reutilisable            │
│                                                                 │
│   Avantages:                                                    │
│   • Reutilisation du code JCL                                  │
│   • Maintenance centralisee                                    │
│   • Standardisation des traitements                            │
│   • Parametrisation flexible                                   │
│                                                                 │
│   Deux types:                                                   │
│   • In-stream : Definie dans le JCL                            │
│   • Cataloguee : Stockee dans une bibliotheque PROCLIB         │
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
│   //...      (corps de la procedure)                            │
│   //         PEND                                               │
│                                                                 │
│   • Definie au debut du JCL (avant le premier EXEC)            │
│   • Se termine par PEND                                        │
│   • Appelee avec EXEC procname ou EXEC PROC=procname           │
│   • Les parametres sont passes a l'appel                       │
│                                                                 │
│   Note: Peu utilisee - preferer les procedures cataloguees     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

#### I-2-d-2 PROC Cataloguee

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROC CATALOGUEE                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Stockee dans une bibliotheque PROCLIB                         │
│   • SYS1.PROCLIB (systeme)                                     │
│   • Bibliotheque utilisateur specifiee par JCLLIB              │
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

### I-2-e Parametres symboliques

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARAMETRES SYMBOLIQUES                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   Syntaxe: &nom  ou  &nom.suffixe                               │
│                                                                 │
│   • Permettent de parametrer les procedures                    │
│   • Remplaces a l'execution par les valeurs fournies           │
│   • Le point (.) separe le symbole du texte suivant            │
│                                                                 │
│   Definition avec valeur par defaut:                            │
│   //MYPROC PROC ENV=TEST,DATE=&LYYMMDD                         │
│                                                                 │
│   Symboles systeme predifinis:                                  │
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
│   Resultat: FTEST.DATA.TEST.D20241205                          │
│                                                                 │
│   Note: && pour un & litteral dans les donnees                 │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Instruction SET :**

```jcl
//* Definition de variables avec SET
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

### I-2-f JCL - Bibliotheques de base

#### I-2-f-1 Declaration JOBLIB

```
┌─────────────────────────────────────────────────────────────────┐
│                    JOBLIB                                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   //JOBLIB DD DSN=bibliotheque,DISP=SHR                         │
│                                                                 │
│   • Place immediatement apres la carte JOB                     │
│   • Definit les bibliotheques de programmes pour TOUT le job   │
│   • Le systeme cherche les programmes dans l'ordre:            │
│     1. JOBLIB (si specifiee)                                   │
│     2. STEPLIB (si specifiee dans le step)                     │
│     3. Bibliotheques systeme (LINKLIB, etc.)                   │
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
│   //STEPLIB DD DSN=bibliotheque,DISP=SHR                        │
│                                                                 │
│   • Definit les bibliotheques de programmes pour UN step       │
│   • Ecrase JOBLIB pour ce step uniquement                      │
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
│   • Definit les bibliotheques de PROCEDURES et INCLUDE         │
│   • Place apres JOB, avant tout EXEC                           │
│   • Jusqu'a 15 bibliotheques                                   │
│   • Recherche dans l'ordre specifie                            │
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
│                  (programmes)         (procedures)              │
│                                                                 │
│   Position       Apres JOB            Apres JOB                 │
│                                       Avant 1er EXEC            │
│                                                                 │
│   Syntaxe        //JOBLIB DD ...      // JCLLIB ORDER=...      │
│                                                                 │
│   Portee         Tout le job          Tout le job               │
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
//* JCLLIB pour les procedures
//         JCLLIB ORDER=(USER.PROCLIB,PROD.PROCLIB)
//*
//* JOBLIB pour les programmes
//JOBLIB   DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=PROD.LOADLIB,DISP=SHR
//*
//* Step 1 : utilise JOBLIB
//STEP1    EXEC PGM=MYPROG1
//INPUT    DD DSN=DATA.INPUT,DISP=SHR
//*
//* Step 2 : utilise STEPLIB (ecrase JOBLIB)
//STEP2    EXEC PGM=MYPROG2
//STEPLIB  DD DSN=TEST.LOADLIB,DISP=SHR
//INPUT    DD DSN=DATA.INPUT,DISP=SHR
//*
//* Step 3 : appel procedure (trouvee via JCLLIB)
//STEP3    EXEC MYPROC
//
```

---

## Synthese

```
┌─────────────────────────────────────────────────────────────────┐
│                    POINTS CLES DU CHAPITRE                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   CARTE JOB                                                     │
│   ─────────                                                    │
│   • Identifie le travail batch                                 │
│   • Parametres: CLASS, MSGCLASS, MSGLEVEL, NOTIFY, REGION      │
│   • Une seule carte JOB par travail                            │
│                                                                 │
│   CARTE EXEC                                                    │
│   ──────────                                                   │
│   • Execute un programme (PGM=) ou procedure (PROC=)           │
│   • Parametres: REGION, TIME, PARM, COND                       │
│   • Un ou plusieurs steps par job                              │
│                                                                 │
│   CARTE DD                                                      │
│   ────────                                                     │
│   • Definit les fichiers utilises                              │
│   • DSN : nom du dataset                                       │
│   • DISP : statut et disposition                               │
│   • DCB : caracteristiques (RECFM, LRECL, BLKSIZE)             │
│   • SPACE : allocation disque                                  │
│   • SYSOUT : sortie vers spool                                 │
│                                                                 │
│   BIBLIOTHEQUES                                                 │
│   ─────────────                                                │
│   • JOBLIB/STEPLIB : bibliotheques de programmes               │
│   • JCLLIB : bibliotheques de procedures                       │
│                                                                 │
│   PROCEDURES                                                    │
│   ───────────                                                  │
│   • In-stream ou cataloguees                                   │
│   • Parametres symboliques (&param)                            │
│   • Symboles systeme (&SYSUID, &LYYMMDD, etc.)                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Aide-memoire JCL

```
┌─────────────────────────────────────────────────────────────────┐
│                    AIDE-MEMOIRE JCL                              │
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
│   SHR              Lecture partagee                            │
│   OLD              Acces exclusif                              │
│   (NEW,CATLG,DEL)  Creation, catalogue si OK                   │
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
│   ,RLSE            Liberer espace non utilise                  │
│                                                                 │
│   DD SPECIALES                                                  │
│   ─────────────────────────────────────────────────────────    │
│   SYSOUT=*         Sortie spool                                │
│   DUMMY            Fichier vide/ignore                         │
│   *                Donnees inline                              │
│   DSN=&&TEMP       Fichier temporaire                          │
│                                                                 │
│   SYMBOLES SYSTEME                                              │
│   ─────────────────────────────────────────────────────────    │
│   &SYSUID          Userid                                      │
│   &LYYMMDD         Date AAAAMMJJ                               │
│   &SYSTIME         Heure HH.MM.SS                              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| - | [Chapitre II - Fichiers speciaux et parametres](02-fichiers-parametres.md) |
