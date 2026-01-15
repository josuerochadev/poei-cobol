# Partie 1 : Creation du Data Set et Affichage

[< Partie 0 : Preparation](01-partie-0-preparation.md) | [Retour au sommaire](00-introduction.md) | [Partie 2a : Ajout >](03-partie-2a-ajout.md)

---

## Exercice 1 : Definition du Data Set CLIENT dans CICS

### Enonce

Definir le Data Set CLIENT dans la procedure de demarrage de CICS et comme ressource VSAM a utiliser par les programmes. Les operations de lecture, ecriture et suppression seront autorisees sur ce Data Set.

### Mon travail

Cet exercice comporte deux etapes principales :

1. **Creation du fichier VSAM** : J'ai utilise IDCAMS pour definir un cluster KSDS avec une cle de 6 octets (numero de compte) en position 0.

2. **Integration dans CICS** : J'ai declare le fichier dans CICS via CEDA pour permettre les operations READ, WRITE, REWRITE, DELETE et BROWSE.

**Choix des parametres VSAM :**
- `KEYS(6 0)` : Cle de 6 caracteres en debut d'enregistrement (numero compte)
- `RECORDSIZE(80 80)` : Enregistrements de taille fixe (80 octets) - compatible avec LRECL=80 par defaut du JCL
- `FREESPACE(20 10)` : Reserve de l'espace pour les insertions futures
- `SHAREOPTIONS(2 3)` : Permet le partage entre regions CICS

> **Note technique** : Les enregistrements font 80 octets (64 donnees + 16 filler) pour etre compatibles avec le LRECL=80 par defaut des DD * en JCL. Les programmes COBOL utiliseront un FILLER de 16 caracteres en fin d'enregistrement.

### Resolution

**Etape 1 : JCL de definition VSAM (IDCAMS)**

```jcl
//ROCHA01 JOB (ACCT),'DEF VSAM CLIENT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* DEFINITION DU DATA SET CLIENT (VSAM KSDS)
//*****************************************************************
//*
//* ETAPE 1 : SUPPRESSION DU CLUSTER EXISTANT (SI EXISTE)
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE ROCHA.CICS.CLIENT CLUSTER
  SET MAXCC = 0
/*
//*
//* ETAPE 2 : DEFINITION DU CLUSTER VSAM KSDS
//*
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER (                                    -
         NAME(ROCHA.CICS.CLIENT)                   -
         INDEXED                                      -
         VOLUMES(FDDBAS)                              -
         KEYS(6 0)                                    -
         RECORDSIZE(80 80)                            -
         TRACKS(5 5)                                  -
         FREESPACE(20 10)                             -
         SHAREOPTIONS(2 3)                            -
         )                                            -
         DATA (NAME(ROCHA.CICS.CLIENT.DATA))       -
         INDEX (NAME(ROCHA.CICS.CLIENT.INDEX))
/*
//*
//* ETAPE 3 : VERIFICATION DE LA CREATION
//*
//STEP3    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  LISTCAT ENTRIES(ROCHA.CICS.CLIENT) ALL
/*
```

**Etape 2 : Integration dans CICS via CEDA**

```
CEDA DEFINE FILE(FCLIENT) GROUP(CLIGROUP)
     DSNAME(ROCHA.CICS.CLIENT)
     ADD(YES)
     BROWSE(YES)
     DELETE(YES)
     READ(YES)
     UPDATE(YES)
     RECORDFORMAT(F)
     RECORDSIZE(80)
     KEYLENGTH(6)
     STATUS(ENABLED)
     OPENTIME(FIRSTREF)

CEDA INSTALL FILE(FCLIENT) GROUP(CLIGROUP)
```

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| FILE | FCLIENT | Nom logique dans CICS (8 car max) |
| DSNAME | ROCHA.CICS.CLIENT | Nom physique du dataset |
| ADD(YES) | - | Autoriser WRITE (ajout) |
| BROWSE(YES) | - | Autoriser STARTBR/READNEXT |
| DELETE(YES) | - | Autoriser DELETE |
| READ(YES) | - | Autoriser READ |
| UPDATE(YES) | - | Autoriser REWRITE |

**Methode alternative : Via macro assembleur (batch)**

Pour les environnements de production ou l'automatisation, on peut utiliser la macro DFHFCT au lieu de CEDA :

```
DFHFCT TYPE=FILE,
       FILE=FCLIENT,
       DATASET=ROCHA.CICS.CLIENT,
       ACCMETH=VSAM,
       SERVREQ=(READ,UPDATE,DELETE,ADD,BROWSE),
       RECFORM=FIXED,
       LRECL=80,
       KEYLEN=6,
       FILSTAT=(ENABLED,OPENED)
```

> **Note** : Cette methode necessite une compilation assembleur et un arret/redemarrage de CICS. Pour le developpement, CEDA est plus pratique car les modifications sont immediates.

**Etape 3 : Verification avec CEMT**

```
CEMT INQUIRE FILE(FCLIENT)
```

Resultat attendu :
```
FILE(FCLIENT)   Dsn(ROCHA.CICS.CLIENT)
                Ena Ope Rea Upd Add Bro Del
                Vsam Ksds
```

**Commandes CEMT utiles pour la gestion du fichier :**

| Commande | Usage |
|----------|-------|
| `CEMT SET FILE(FCLIENT) OPEN` | Ouvrir le fichier |
| `CEMT SET FILE(FCLIENT) ENABLED` | Activer le fichier |
| `CEMT SET FILE(FCLIENT) CLOSED` | Fermer le fichier (pour maintenance VSAM) |
| `CEMT INQ FILE(FCLIENT)` | Verifier l'etat du fichier |

**Etape 4 : Chargement des donnees initiales**

Le chargement utilise directement IDCAMS REPRO avec des enregistrements de 80 octets (64 donnees + 16 espaces en filler). Le DD * lit par defaut en LRECL=80, ce qui est maintenant compatible avec notre definition VSAM.

```jcl
//ROCHA02 JOB (ACCT),'LOAD VSAM CLIENT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* CHARGEMENT DES DONNEES INITIALES DANS LE FICHIER CLIENT
//*
//* Structure enregistrement (80 octets) :
//*   Pos 01-06 : Numero compte (cle)
//*   Pos 07-08 : Code region
//*   Pos 09-10 : Nature compte
//*   Pos 11-20 : Nom client (10 car)
//*   Pos 21-30 : Prenom client (10 car)
//*   Pos 31-38 : Date naissance (AAAAMMJJ)
//*   Pos 39    : Sexe (M/F)
//*   Pos 40-41 : Activite professionnelle
//*   Pos 42    : Situation sociale (C/M/D/V)
//*   Pos 43-52 : Adresse (10 car)
//*   Pos 53-62 : Solde (10 car)
//*   Pos 63-64 : Position (DB/CR)
//*   Pos 65-80 : Filler (16 espaces)
//*****************************************************************
//*
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
0000010120DUPONT    JEAN      19850315M10CPARIS     0000150000CR
0000020125MARTIN    MARIE     19900622F15MPARIS     0000080000DB
0000030220BERNARD   PIERRE    19780410M20CMARSEILLE 0000250000CR
0000040225PETIT     SOPHIE    19880912F05MMARSEILLE 0000045000DB
0000050330ROBERT    ALAIN     19750520M10CLYON      0000320000CR
0000060335RICHARD   CLAIRE    19920805F25VLYON      0000012000DB
0000070420DURAND    PAUL      19820718M30DLILLE     0000180000CR
0000080425MOREAU    ANNE      19950303F15CLILLE     0000095000DB
0000090130LAURENT   MARC      19800125M20MPARIS     0000420000CR
0000100235SIMON     JULIE     19870930F10CMARSEILLE 0000067000DB
2220010120LEROY     MICHEL    19830214M05CPARIS     0000145000CR
2220020125ROUX      NATHALIE  19910607F15MMARSEILLE 0000032000DB
2220030230DAVID     FRANCOIS  19760819M20CLYON      0000278000CR
2220040335BERTRAND  ISABELLE  19890423F25VLILLE     0000089000DB
2220050420MOREL     PHILIPPE  19840111M30DPARIS     0000156000CR
/*
//SYSIN    DD *
 REPRO INFILE(INFILE) -
       OUTDATASET(ROCHA.CICS.CLIENT)
/*
//*
//STEP2    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 PRINT INDATASET(ROCHA.CICS.CLIENT) -
       CHARACTER
/*
//*
//STEP3    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 LISTCAT ENTRIES(ROCHA.CICS.CLIENT) ALL
/*
```

> **Note technique** : Les donnees font 64 caracteres et sont automatiquement completees a 80 caracteres (padding avec des espaces) par le JCL. Le VSAM etant defini avec RECORDSIZE(80 80), les enregistrements sont compatibles.

**Donnees chargees :**

| Type | Numeros | Quantite | Usage |
|------|---------|----------|-------|
| Clients de base | 000001-000010 | 10 | Tests CRUD (Ex 3-15) |
| Clients 222xxx | 222001-222005 | 5 | Test READNEXT (Ex 18) |

**Repartition par region (pour Ex 19 - Statistiques) :**

| Region | Debiteurs | Crediteurs | Total |
|--------|-----------|------------|-------|
| 01 Paris | 1 | 4 | 5 |
| 02 Marseille | 2 | 2 | 4 |
| 03 Lyon | 1 | 2 | 3 |
| 04 Lille | 2 | 1 | 3 |

> **Note** : Les clients 111xxx, 444xxx et 777xxx seront crees manuellement via la transaction AJOU dans l'exercice 16.

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt1ex01-1 : Soumission JCL DEFVSAM - ISPF EDIT avec SUB
2. pt1ex01-2 : SDSF - Job output avec RC=0000 pour IDCAMS DEFINE
3. pt1ex01-3 : CEDA DEFINE FILE(FCLIENT) - ecran de definition
4. pt1ex01-4 : CEDA INSTALL FILE(FCLIENT) - message INSTALL SUCCESSFUL
5. pt1ex01-5 : CEMT INQ FILE(FCLIENT) - verification statut Ena Ope
6. pt1ex01-6 : Soumission JCL LOADVSAM - chargement des donnees
7. pt1ex01-7 : SDSF - Output PRINT montrant les 15 enregistrements charges
-->

---

## Exercice 2 : Creation de la MAP BMS pour affichage

### Enonce

Creer la MAP conformement a la structure du Data Set CLIENT permettant l'affichage des nouvelles donnees. Prevoir dans ce cadre le controle des donnees redondantes et une zone de message de 40 caracteres pour afficher les informations necessaires en cas d'erreur ou de saisie correcte.

### Mon travail

J'ai cree une MAP BMS avec tous les champs du fichier CLIENT. La MAP comprend :
- Un titre en haut de l'ecran
- Une zone de saisie pour le numero de compte (cle de recherche)
- Les 12 champs d'affichage avec leurs libelles
- Des zones libelles pour afficher les descriptions (region, sexe, situation, position)
- Une zone de message de 60 caracteres en bas
- Les touches fonction en bas de l'ecran

**Choix de conception :**

- `CTRL=(FREEKB,FRSET)` : Clavier debloque et MDT remis a zero
- `TIOAPFX=YES` : Reserve 12 octets pour le prefixe TIOA (requis pour CICS)
- Seul le champ NUMCPT est saisissable (UNPROT), les autres sont en affichage (ASKIP)

**Comprendre les concepts BMS :**

| Option | Signification | Role |
|--------|---------------|------|
| **FREEKB** | Free Keyboard | Debloque le clavier apres l'envoi de la MAP, permettant a l'utilisateur de saisir |
| **FRSET** | Flag Reset | Remet le MDT a zero pour tous les champs |
| **MDT** | Modified Data Tag | Bit qui indique si un champ a ete modifie par l'utilisateur |
| **TIOAPFX** | TIOA Prefix | Reserve 12 octets au debut de la zone MAP pour le prefixe CICS |
| **UNPROT** | Unprotected | Champ saisissable par l'utilisateur |
| **ASKIP** | Auto-skip | Champ en affichage seul, le curseur le saute |

> **Le MDT (Modified Data Tag)** : Chaque champ de l'ecran possede un bit MDT. Quand l'utilisateur modifie un champ, le MDT passe a 1. Lors du RECEIVE MAP, seuls les champs avec MDT=1 sont transmis au programme. L'option FRSET remet tous les MDT a 0 au SEND MAP, permettant de detecter les nouvelles modifications.

### Resolution

**MAP BMS : CLIAFF.bms**

```
***********************************************************************
*  MAPSET : CLIAFF - Affichage Client
*  Transaction : AFFI
***********************************************************************
CLIAFF   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAFF   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AFFICHAGE CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE MESSAGE ET TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**Zones de la MAP :**

| Zone | Longueur | Attribut | Description |
|------|----------|----------|-------------|
| NUMCPT | 6 | UNPROT,NUM,IC | Numero compte (saisie) |
| CODREG | 2 | ASKIP,BRT | Code region |
| LIBREG | 15 | ASKIP,BRT | Libelle region |
| NATCPT | 2 | ASKIP,BRT | Nature compte |
| NOM | 10 | ASKIP,BRT | Nom client |
| PRENOM | 10 | ASKIP,BRT | Prenom client |
| DATNA | 10 | ASKIP,BRT | Date naissance |
| SEXE | 1 | ASKIP,BRT | Sexe |
| ACTPRO | 2 | ASKIP,BRT | Activite professionnelle |
| SITSO | 1 | ASKIP,BRT | Situation sociale |
| ADRESSE | 10 | ASKIP,BRT | Adresse |
| SOLDE | 12 | ASKIP,BRT | Solde |
| POSIT | 2 | ASKIP,BRT | Position (DB/CR) |
| MSG | 60 | ASKIP,BRT | Zone message |

**JCL d'assemblage : ASMCLAF.jcl**

```jcl
//ROCHA03 JOB (ACCT),'ASSEMBL BMS CLIAFF',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* ASSEMBLAGE DE LA MAP BMS CLIAFF (AFFICHAGE CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIAFF',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIAFF),DISP=SHR
/*
```

> **Note** : La procedure DFHMAPS genere automatiquement le module physique (MAP) et le copybook COBOL (DSECT). Le copybook sera stocke dans ROCHA.CICS.LINK avec le nom du mapset. Attention a ne pas utiliser la meme library pour le source et le DSECT, sinon le source sera ecrase!

**Structure du copybook genere (DSECT) :**

Pour chaque champ nomme dans la MAP BMS (ex: NUMCPT), le copybook genere contient plusieurs variables avec des suffixes :

| Suffixe | Type | Description | Exemple |
|---------|------|-------------|---------|
| **L** | S9(04) COMP | Longueur des donnees recues | NUMCPTL |
| **F** | X(01) | Flag (usage interne) | NUMCPTF |
| **A** | X(01) | Attribut dynamique | NUMCPTA |
| **I** | X(nn) | Zone input (donnees recues) | NUMCPTI |
| **O** | X(nn) | Zone output (donnees a envoyer) | NUMCPTO |

> **Important** : Avec `STORAGE=AUTO`, les zones I et O partagent la meme memoire. Apres un `RECEIVE MAP`, sauvegarder les valeurs importantes avant de faire `MOVE LOW-VALUES`.

**Apercu de l'ecran MAPAFF :**

```
+------------------------------------------------------------------------------+
|                         *** AFFICHAGE CLIENT ***                             |
|------------------------------------------------------------------------------|
|                                                                              |
|  NUMERO COMPTE : ______                                                      |
|                                                                              |
|  CODE REGION   : __                            _______________               |
|  NATURE COMPTE : __                            _______________               |
|  NOM           : __________                                                  |
|  PRENOM        : __________                                                  |
|  DATE NAISSANCE: __________                                                  |
|  SEXE          : _               ________                                    |
|  ACTIVITE PRO  : __                                                          |
|  SITUATION SOC : _               ____________                                |
|  ADRESSE       : __________                                                  |
|  SOLDE         : ____________                                                |
|  POSITION      : __              __________                                  |
|                                                                              |
|                                                                              |
|  MESSAGE : ____________________________________________________________      |
|                                                                              |
|                                                                              |
|  ENTER=Rechercher  PF3=Quitter  CLEAR=Effacer                                |
+------------------------------------------------------------------------------+
```

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt1ex02-1 : Source BMS dans ISPF EDIT - ROCHA.CICS.SOURCE(CLIAFF)
2. pt1ex02-2 : Soumission JCL ASMCLAF - assemblage de la MAP
3. pt1ex02-3 : SDSF - Job output avec RC=0000 pour assemblage
4. pt1ex02-4 : Verification ROCHA.CICS.LOAD - membre CLIAFF present
5. pt1ex02-5 : Verification ROCHA.CICS.LINK - copybook CLIAFF genere
-->

---

## Exercice 3 : Programme COBOL-CICS d'affichage

### Enonce

Creer le PROGRAMME necessaire pour l'affichage des donnees pour un code CLIENT saisi. Il doit permettre une saisie multiple de code CLIENT jusqu'a fin de saisie d'affichage de la part de l'utilisateur. De meme, il faut accompagner chaque anomalie ou action par un message d'information ou d'avertissement.

### Mon travail

J'ai developpe un programme COBOL-CICS en mode **pseudo-conversationnel**.

#### Comprendre le mode pseudo-conversationnel

En CICS, un programme ne reste pas en memoire pendant que l'utilisateur reflechit. Au lieu de cela :

1. **Le programme s'execute** : traite les donnees, affiche un ecran
2. **Le programme se TERMINE** : libere la memoire et les ressources
3. **L'utilisateur saisit** : pendant ce temps, le programme n'existe plus
4. **CICS relance le programme** : quand l'utilisateur appuie sur une touche

C'est le mode **pseudo-conversationnel** : l'utilisateur a l'impression d'une conversation continue, mais en realite le programme est relance a chaque interaction.

```
┌─────────────────────────────────────────────────────────────────┐
│ LANCEMENT TRANSACTION "AFFI"                                    │
└─────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│ PREMIER PASSAGE (EIBCALEN = 0)                                  │
│ ──────────────────────────────────────────────────────────────  │
│ → CICS lance le programme pour la premiere fois                 │
│ → EIBCALEN = 0 (pas de COMMAREA, c'est un nouveau contexte)     │
│ → Le programme affiche l'ecran vide (SEND MAP)                  │
│ → Le programme se TERMINE (RETURN TRANSID)                      │
│ → Memoire liberee, ressources liberees                          │
└─────────────────────────────────────────────────────────────────┘
                            │
        L'utilisateur saisit un numero et appuie sur ENTREE
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│ PASSAGES SUIVANTS (EIBCALEN > 0)                                │
│ ──────────────────────────────────────────────────────────────  │
│ → CICS relance le programme (nouveau processus)                 │
│ → EIBCALEN > 0 (la COMMAREA indique un contexte existant)       │
│ → Le programme recoit la saisie (RECEIVE MAP)                   │
│ → Le programme lit le fichier (READ FILE)                       │
│ → Le programme affiche le resultat (SEND MAP)                   │
│ → Le programme se TERMINE a nouveau (RETURN TRANSID)            │
└─────────────────────────────────────────────────────────────────┘
```

#### Variables cles du EIB (Exec Interface Block)

CICS fournit un bloc de donnees appele EIB contenant des informations sur le contexte :

| Variable | Description | Valeurs typiques |
|----------|-------------|------------------|
| **EIBCALEN** | Longueur de la COMMAREA | 0 = premier passage, >0 = passage suivant |
| **EIBAID** | Touche appuyee | DFHENTER, DFHPF3, DFHCLEAR |
| **EIBTRNID** | Code transaction | 'AFFI' |
| **EIBRESP** | Code reponse derniere commande | 0=OK, 13=NOTFND |

#### Logique du programme

Le programme utilise un `EVALUATE` pour aiguiller selon le contexte :

| Condition | Action | Paragraphe |
|-----------|--------|------------|
| EIBCALEN = 0 | Premier passage, afficher ecran vide | 1000-PREMIER-PASSAGE |
| EIBAID = DFHPF3 | Touche PF3, quitter | 9000-FIN-PROGRAMME |
| EIBAID = DFHCLEAR | Touche CLEAR, reinitialiser | 1000-PREMIER-PASSAGE |
| Autre (ENTER) | Traiter la saisie | 2000-TRAITEMENT |

#### Points techniques importants

- **DFHAID** : Copybook contenant les constantes des touches (DFHPF3, DFHCLEAR, DFHENTER)
- **SEND TEXT** : Necessite une variable, pas une constante litterale (`FROM(WS-MSG)` pas `FROM('texte')`)
- **Copybook BMS** : Genere par l'assemblage, contient MAPAFFI (input) et MAPAFFO (output)

### Resolution

**Programme : PRGCLIA.cbl**

Le code source est stocke dans `ROCHA.CICS.SOURCE(PRGCLIA)`. Voici les extraits principaux :

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCLIA.
      ******************************************************************
      * PROGRAMME : PRGCLIA - Affichage client
      * TRANSACTION : AFFI
      * MODE : Pseudo-conversationnel
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMMAREA.
           05 WS-FLAG-INIT         PIC X(01) VALUE 'N'.

      * Copybooks CICS
       COPY DFHAID.
       COPY CLIAFF.

       01  ENR-CLIENT.
           05 CLI-NUMCPT           PIC X(06).
           05 CLI-CODREG           PIC X(02).
           05 CLI-NATCPT           PIC X(02).
           05 CLI-NOM              PIC X(10).
           05 CLI-PRENOM           PIC X(10).
           05 CLI-DATNAISS         PIC X(08).
           05 CLI-SEXE             PIC X(01).
           05 CLI-ACTPRO           PIC X(02).
           05 CLI-SITSO            PIC X(01).
           05 CLI-ADRESSE          PIC X(10).
           05 CLI-SOLDE            PIC X(10).
           05 CLI-POSITION         PIC X(02).
           05 FILLER               PIC X(16).

       01  WS-RESP                 PIC S9(08) COMP VALUE 0.
       01  WS-NUMCPT               PIC X(06) VALUE SPACES.
       01  WS-MSG-FIN              PIC X(40)
           VALUE 'TRANSACTION AFFI TERMINEE - AU REVOIR'.

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN EIBAID = DFHPF3
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN OTHER
                   PERFORM 2000-TRAITEMENT
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('AFFI')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-PREMIER-PASSAGE.
           MOVE LOW-VALUES TO MAPAFFO
           MOVE 'SAISIR LE NUMERO DE COMPTE ET APPUYER SUR ENTREE'
               TO MSGO
           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
               ERASE
           END-EXEC.

       2000-TRAITEMENT.
           EXEC CICS RECEIVE MAP('MAPAFF')
               MAPSET('CLIAFF')
           END-EXEC

           IF NUMCPTL = 0 OR NUMCPTI = SPACES
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               GO TO 2000-FIN
           END-IF

           MOVE NUMCPTI TO WS-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(WS-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 3000-AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO' TO MSGO
               WHEN OTHER
                   MOVE 'ERREUR LECTURE FICHIER' TO MSGO
           END-EVALUATE

           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
           END-EXEC.

       2000-FIN.
           EXIT.

       3000-AFFICHER-CLIENT.
           MOVE LOW-VALUES TO MAPAFFO
           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITO
      * ... (conversion des libelles region, sexe, situation, position)
           MOVE 'CLIENT TROUVE - PF3=QUITTER' TO MSGO.

       9000-FIN-PROGRAMME.
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC
           EXEC CICS RETURN END-EXEC.
```

**JCL de compilation : CMPCLAF.jcl**

```jcl
//ROCHA04 JOB (ACCT),'COMPILE PRGCLIA',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* COMPILATION DU PROGRAMME COBOL-CICS PRGCLIA
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGCLIA),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGCLIA(R)
/*
```

**Commandes CICS utilisees :**

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'ecran |
| RECEIVE MAP | Recevoir la saisie |
| READ FILE | Lire VSAM par cle |
| RETURN TRANSID | Retour pseudo-conversationnel |
| SEND TEXT | Message de fin |

**Structure du programme :**

| Paragraphe | Fonction |
|------------|----------|
| 0000-PRINCIPAL | Point d'entree, aiguillage selon EIBCALEN et EIBAID |
| 1000-PREMIER-PASSAGE | Affichage de l'ecran vide |
| 2000-TRAITEMENT | Reception saisie, lecture VSAM, affichage resultat |
| 3000-AFFICHER-CLIENT | Transfert donnees vers MAP avec conversion libelles |
| 9000-FIN-PROGRAMME | Message de fin et RETURN sans TRANSID |

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt1ex03-1 : Source COBOL dans ISPF EDIT - ROCHA.CICS.SOURCE(PRGCLIA)
2. pt1ex03-2 : Soumission JCL CMPCLAF - compilation du programme
3. pt1ex03-3 : SDSF - Job output avec RC=0000 pour compilation
4. pt1ex03-4 : Verification ROCHA.CICS.LOAD - membre PRGCLIA present
5. pt1ex03-5 : Ecran MAPAFF vide - premier passage (message "SAISIR LE NUMERO...")
6. pt1ex03-6 : Ecran avec client affiche - apres saisie numero valide
7. pt1ex03-7 : Ecran avec message erreur - client inexistant
-->

---

## Exercice 4 : Creation de la transaction via CEDA

### Enonce

Creer la transaction correspondante a l'operation d'affichage des donnees de CLIENT avec l'interface CICS en utilisant la commande CEDA. Mettre eventuellement le GROUP et la LIST a jour en cas de besoin.

### Mon travail

Pour qu'une transaction CICS fonctionne, plusieurs ressources doivent etre definies et liees :

1. **FILE** : Le fichier VSAM (deja defini dans l'exercice 1)
2. **MAPSET** : Le module BMS compile (ecran physique)
3. **PROGRAM** : Le programme COBOL-CICS compile
4. **TRANSACTION** : Le code de 4 caracteres qui lance le programme

Ces ressources sont regroupees dans un GROUP (ici CLIGROUP) qui permet de les gerer ensemble. L'ordre de definition est important car la transaction reference le programme.

### Resolution

**Etape 1 : Definition des nouvelles ressources**

Le fichier FCLIENT etant deja defini et installe (exercice 1), je definis uniquement les nouvelles ressources :

```
CEDA DEFINE MAPSET(CLIAFF) GROUP(CLIGROUP)

CEDA DEFINE PROGRAM(PRGCLIA) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA DEFINE TRANSACTION(AFFI) GROUP(CLIGROUP)
     PROGRAM(PRGCLIA)
```

**Etape 2 : Installation des ressources**

*Option A : Installation individuelle (recommandee)*

Cette methode evite les erreurs si certaines ressources sont deja installees :

```
CEDA INSTALL MAPSET(CLIAFF) GROUP(CLIGROUP)
CEDA INSTALL PROGRAM(PRGCLIA) GROUP(CLIGROUP)
CEDA INSTALL TRANSACTION(AFFI) GROUP(CLIGROUP)
```

*Option B : Installation du groupe complet*

```
CEDA INSTALL GROUP(CLIGROUP)
```

> **Note** : Si FCLIENT est deja installe (exercice 1), cette commande affichera une erreur "ALREADY INSTALLED" pour le fichier. C'est normal et les autres ressources seront quand meme installees.

**Tableau recapitulatif des ressources du groupe CLIGROUP :**

| Ressource | Nom | Defini dans | Description |
|-----------|-----|-------------|-------------|
| FILE | FCLIENT | Exercice 1 | Fichier VSAM CLIENT |
| MAPSET | CLIAFF | Exercice 4 | Ecran BMS d'affichage |
| PROGRAM | PRGCLIA | Exercice 4 | Programme COBOL-CICS |
| TRANSACTION | AFFI | Exercice 4 | Code transaction (4 car) |

**Etape 3 : Verification avec CEMT**

```
CEMT INQ FILE(FCLIENT)
```
Resultat attendu : `Fil(FCLIENT) Dsn(ROCHA.CICS.CLIENT) Ena Ope Rea Upd Add Bro Del Vsam Ksds`

```
CEDA VIEW MAPSET(CLIAFF) GROUP(CLIGROUP)
```
Resultat attendu : Affichage de la definition du mapset (DEFINITION SIGNATURE, RESIDENT, etc.)

```
CEMT INQ PROG(PRGCLIA)
```
Resultat attendu : `Pro(PRGCLIA) Len(...) Cob Ena Pri`

```
CEMT INQ TRAN(AFFI)
```
Resultat attendu : `Tra(AFFI) Pro(PRGCLIA) Ena`

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt1ex04-1 : Ecran CEDA DEFINE MAPSET(CLIAFF) - definition du mapset
2. pt1ex04-2 : Ecran CEDA DEFINE PROGRAM(PRGCLIA) - definition du programme
3. pt1ex04-3 : Ecran CEDA DEFINE TRANSACTION(AFFI) - definition de la transaction
4. pt1ex04-4 : Ecran CEDA INSTALL avec message de succes (ou erreur ALREADY INSTALLED)
5. pt1ex04-5 : Ecran CEMT INQ TRAN(AFFI) - verification que la transaction est active
6. pt1ex04-6 : Test de la transaction AFFI - ecran d'affichage vide
-->

---

## Exercice 5 : Test avec debugger CEDF

### Enonce

Activer la transaction en mode debugger avec la commande CEDF et par suite sans debugger.

### Mon travail

J'ai teste la transaction AFFI en mode debug avec CEDF pour verifier le bon enchainement des commandes CICS et observer les valeurs des variables EIB (voir Exercice 3 pour les explications sur le mode pseudo-conversationnel et les variables EIB).

### Resolution

**Etape 1 : Activation du debugger et lancement de la transaction**

```
CEDF
```

L'ecran se vide et le curseur se positionne en haut. Le mode EDF est active mais aucun message ne s'affiche. Il faut maintenant lancer la transaction a deboguer :

```
AFFI
```

CEDF intercepte alors la transaction et affiche le premier point d'arret.

> **Note** : Sur TK4-, CEDF n'affiche pas de message de confirmation. Le debugger est actif des que la commande est saisie.

**Etape 2 : Navigation dans CEDF**

| Touche | Action |
|--------|--------|
| ENTER | Passer a l'etape suivante |
| PF5 | Afficher la WORKING-STORAGE |
| PF4 | Afficher l'EIB (Exec Interface Block) |
| PF3 | Terminer le debug et continuer l'execution |

**Etape 4 : Points d'arret observes**

| Etape | Commande CICS | RESP attendu |
|-------|---------------|--------------|
| 1 | SEND MAP | NORMAL |
| 2 | RETURN TRANSID | - |
| 3 | TASK TERMINATION | - |
| 4 | RECEIVE MAP | NORMAL |
| 5 | READ FILE | NORMAL ou NOTFND |
| 6 | SEND MAP | NORMAL |
| 7 | RETURN TRANSID | - |

**Etape 5 : Test sans debugger**

Pour tester la transaction sans le debugger CEDF, il suffit de lancer directement la transaction depuis un ecran CICS vierge (sans avoir active CEDF au prealable) :

```
AFFI
```

La transaction s'execute normalement sans interruption, affichant directement l'ecran de saisie.

> **Desactiver CEDF** : Pour sortir du mode debug, appuyer sur PF3 pendant un point d'arret, ou simplement lancer une nouvelle transaction sans avoir tape CEDF.

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt1ex05-1 : Premier arret CEDF - SEND MAP (avant execution)
2. pt1ex05-2 : SEND MAP (apres execution) - RESPONSE: NORMAL
3. pt1ex05-3 : RETURN TRANSID - affichage de la COMMAREA
4. pt1ex05-4 : TASK TERMINATION - fin du premier passage
5. pt1ex05-5 : Ecran d'affichage vide (MAP envoyee) - saisie numero
6. pt1ex05-6 : RECEIVE MAP - reception des donnees saisies
7. pt1ex05-7 : READ FILE - lecture VSAM avec RESP visible
8. pt1ex05-8 : Affichage PF5 - WORKING-STORAGE avec donnees client
9. pt1ex05-9 : SEND MAP final - affichage du client trouve
10. pt1ex05-10 : Test sans debugger - transaction AFFI directe (ecran fonctionnel)
-->

---

[< Partie 0 : Preparation](01-partie-0-preparation.md) | [Retour au sommaire](00-introduction.md) | [Partie 2a : Ajout >](03-partie-2a-ajout.md)
