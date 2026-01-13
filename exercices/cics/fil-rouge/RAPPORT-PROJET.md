# Rapport de Projet - Mini-Projet CICS-VSAM

**Theme** : Developpement d'un mini-projet COBOL-CICS sous z/OS pour l'alimentation du Data Set CLIENT d'une institution financiere.

**Candidat** : Josue ROCHA
**Date** : 19 Decembre 2025 - 22 Janvier 2026
**Formation** : POEI Developpeur Mainframe COBOL - M2i Formation, Strasbourg

---

## Introduction

Ce projet a ete realise dans le cadre de la formation POEI Developpeur Mainframe COBOL. L'objectif est de mettre en pratique les competences acquises en programmation COBOL-CICS et en gestion de fichiers VSAM en mode transactionnel.

### Environnement de travail

- **Systeme** : z/OS sous emulateur Hercules (TK4-)
- **Interface** : TSO/ISPF, CICS
- **Fichiers** : VSAM KSDS
- **Libraries utilisees** :
  - `ROCHA.CICS.SOURCE` : Programmes COBOL et JCL
  - `ROCHA.CICS.LINK` : Programmes objets
  - `ROCHA.CICS.LOAD` : Programmes executables

### Demarche suivie

1. **Definition VSAM** : Creation du Data Set CLIENT et integration dans CICS (FCT)
2. **Developpement MAPs BMS** : Ecrans de saisie et affichage
3. **Programmes COBOL-CICS** : Logique metier avec commandes CICS
4. **Creation transactions** : Definition via CEDA, tests avec CEDF
5. **Operations avancees** : STARTBR, READNEXT pour acces generique

### Difficultes rencontrees et solutions

| Probleme | Solution |
|----------|----------|
| Erreur VSAM 108 au chargement (longueur incorrecte) | Le DD * du JCL lit en LRECL=80 par defaut. Solution : definir RECORDSIZE(80 80) et utiliser un FILLER de 16 octets dans les programmes |
| Volume non specifie (TK4-) | Ajouter VOLUMES(FDDBAS) dans la definition du cluster VSAM |
| Fichier VSAM vide apres REPRO avec RC=00 | Probleme de LRECL incompatible. Resolu en passant a 80 octets |

### Competences mises en oeuvre

- Definition de fichiers VSAM KSDS (IDCAMS)
- Integration fichiers dans CICS (FCT - File Control Table)
- Conception d'ecrans BMS (Basic Mapping Support)
- Commandes CICS : SEND MAP, RECEIVE MAP, READ, WRITE, REWRITE, DELETE
- Navigation VSAM : STARTBR, READNEXT, ENDBR
- Definition de transactions (CEDA)
- Debogage avec CEDF
- Validation et controle des donnees saisies

---

## Sommaire

0. [Partie 0 : Preparation de l'environnement](#partie-0--preparation-de-lenvironnement)
1. [Partie 1 : Creation du Data Set et Affichage](#partie-1--creation-du-data-set-et-affichage)
2. [Partie 2 : Operations CRUD](#partie-2--operations-crud)
3. [Partie 3 : Operations avancees](#partie-3--operations-avancees)

---

# Partie 0 : Preparation de l'environnement

## Exercice 0 : Creation des Libraries

### Enonce

Ce travail necessite la creation de trois Library pour stocker les membres a creer au cours de sa realisation. Les Library a definir doivent porter le nom sous la forme suivante :
- **ROCHA.CICS.SOURCE** : Programmes COBOL et JCL
- **ROCHA.CICS.LINK** : Programmes objets (apres compilation)
- **ROCHA.CICS.LOAD** : Programmes executables (apres link-edit)

### Mon travail

Avant de commencer le developpement des programmes CICS, j'ai cree les trois libraries necessaires via ISPF option 3.2 (Data Set Utility). Ces libraries sont des PDS (Partitioned Data Sets) qui contiendront tous les membres du projet.

**Choix des caracteristiques :**
- **Organisation** : PO (Partitioned Organization) pour stocker plusieurs membres
- **Format d'enregistrement** : FB (Fixed Block) avec LRECL=80 pour les sources
- **Taille** : 10 tracks primaires, 5 secondaires (suffisant pour le projet)
- **Directory blocks** : 10 blocs pour l'index des membres

### Resolution

**Methode 1 : Via ISPF 3.2 (Data Set Utility)**

```
Option ===> 3.2

DATA SET UTILITY

A - Allocate new data set

Data Set Name: ROCHA.CICS.SOURCE

Allocation Parameters:
  Management class  . .
  Storage class . . . .
  Volume serial . . . .
  Device type . . . . .
  Data class  . . . . .
  Space units . . . . . TRACK
  Primary quantity  . . 10
  Secondary quantity  . 5
  Directory blocks  . . 10
  Record format . . . . FB
  Record length . . . . 80
  Block size  . . . . . 27920
  Data set name type  . PDS
```

Repeter l'operation pour `ROCHA.CICS.LINK` et `ROCHA.CICS.LOAD`.

**Methode 2 : Via JCL (IEFBR14)**

```jcl
//CREATLIB JOB (ACCT),'CREATE LIBRARIES',CLASS=A,MSGCLASS=X
//*****************************************************************
//* CREATION DES LIBRARIES POUR LE PROJET CICS
//*****************************************************************
//*
//STEP1    EXEC PGM=IEFBR14
//*
//* LIBRARY SOURCE (PROGRAMMES COBOL, BMS, JCL)
//SOURCE   DD DSN=ROCHA.CICS.SOURCE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920),
//            UNIT=SYSDA
//*
//* LIBRARY LINK (PROGRAMMES OBJETS)
//LINK     DD DSN=ROCHA.CICS.LINK,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5,10)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920),
//            UNIT=SYSDA
//*
//* LIBRARY LOAD (PROGRAMMES EXECUTABLES)
//LOAD     DD DSN=ROCHA.CICS.LOAD,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(20,10,10)),
//            DCB=(RECFM=U,BLKSIZE=27998),
//            UNIT=SYSDA
```

> **Note** : La library LOAD utilise RECFM=U (Undefined) car elle contient des modules executables (load modules) et non du texte source.

**Verification des libraries creees :**

```
Option ===> 3.4

DSLIST - Data Sets Matching ROCHA.CICS

Command - Enter "/" to select action
-------------------------------------------------------------------------------
         ROCHA.CICS.LINK
         ROCHA.CICS.LOAD
         ROCHA.CICS.SOURCE
```

### Structure des libraries

| Library | Contenu | RECFM | LRECL |
|---------|---------|-------|-------|
| ROCHA.CICS.SOURCE | Programmes COBOL (.cbl), MAPs BMS (.bms), JCL, Copybooks | FB | 80 |
| ROCHA.CICS.LINK | Modules objets apres compilation | FB | 80 |
| ROCHA.CICS.LOAD | Modules executables (load modules) | U | - |

### Membres a creer dans SOURCE

Au cours du projet, les membres suivants seront crees dans `ROCHA.CICS.SOURCE` :

**Programmes COBOL :**
- PRGCLIA : Affichage client
- PRGAJT : Ajout client
- PRGMAJ : Mise a jour client
- PRGSUP : Suppression client
- PRGSUL : Suppression avec lecture
- PRGSDEL : Suppression generique
- PRGLGEN : Liste generique
- PRGSTAT : Statistiques region

**MAPs BMS :**
- MAPAFF : Ecran affichage
- MAPAJT : Ecran ajout
- MAPMAJ : Ecran mise a jour
- MAPSUP : Ecran suppression
- MAPSTAT : Ecran statistiques

**JCL :**
- DEFVSAM : Definition fichier VSAM
- COMPBMS : Compilation MAP BMS
- COMPCOB : Compilation programme COBOL-CICS

> **Note sur les copybooks** : Les copybooks pour les MAPs BMS sont generes automatiquement lors de l'assemblage avec l'option TYPE=DSECT. Ils contiennent les structures DFHBMSCA et les zones de la MAP (suffixes I pour input, O pour output, L pour longueur, etc.).

### Captures d'ecran

<!-- ![pt0ex00-1](images-pt1/pt0ex00-1.png) -->
<!-- ![pt0ex00-2](images-pt1/pt0ex00-2.png) -->
<!-- ![pt0ex00-3](images-pt1/pt0ex00-3.png) -->

---

# Partie 1 : Creation du Data Set et Affichage

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

J'ai developpe un programme COBOL-CICS qui :
1. Envoie la MAP vide au premier appel (EIBCALEN = 0)
2. Gere les touches PF3 (quitter) et CLEAR (reinitialiser)
3. Recoit le numero de compte saisi par l'utilisateur
4. Lit l'enregistrement VSAM avec la commande READ
5. Affiche les donnees avec les libelles (region, sexe, situation, position)
6. Affiche un message d'erreur si client non trouve

**Gestion pseudo-conversationnelle** : Le programme utilise RETURN TRANSID pour revenir au debut apres chaque interaction, avec une COMMAREA pour conserver l'etat.

**Points techniques importants** :
- Le copybook `DFHAID` est requis pour les constantes de touches (DFHPF3, DFHCLEAR, DFHENTER, etc.)
- La commande `SEND TEXT FROM(...)` necessite une reference de donnee (variable), pas une constante litterale
- Le copybook BMS (CLIAFF) est genere par l'assemblage de la MAP et contient les structures MAPAFFI/MAPAFFO

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
CEMT INQ MAPSET(CLIAFF)
```
Resultat attendu : `Map(CLIAFF) Ins Ena`

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

J'ai teste la transaction AFFI en mode debug avec CEDF pour :
1. Verifier le bon enchainement des commandes CICS
2. Comprendre le fonctionnement pseudo-conversationnel
3. Observer les valeurs des variables (EIBCALEN, EIBAID, RESP)
4. Valider le fonctionnement sans debugger

### Comprendre le mode pseudo-conversationnel

Le programme PRGCLIA fonctionne en mode **pseudo-conversationnel**. Cela signifie que le programme se termine reellement entre chaque interaction utilisateur, puis est relance par CICS.

**Deroulement observe dans CEDF :**

```
┌─────────────────────────────────────────────────────────────────┐
│  PREMIER PASSAGE (EIBCALEN = 0)                                 │
├─────────────────────────────────────────────────────────────────┤
│  1. SEND MAP('MAPAFF') MAPSET('CLIAFF') ERASE                   │
│     → L'ecran vide s'affiche                                    │
│  2. RETURN TRANSID('AFFI') COMMAREA(WS-COMMAREA)                │
│     → Le programme se termine                                   │
│  3. TASK TERMINATION (normal)                                   │
│     → CEDF demande YES/NO pour continuer                        │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ L'utilisateur saisit un numero et appuie ENTER
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│  PASSAGES SUIVANTS (EIBCALEN > 0)                               │
├─────────────────────────────────────────────────────────────────┤
│  1. RECEIVE MAP('MAPAFF') MAPSET('CLIAFF')                      │
│     → Reception du numero de compte saisi                       │
│  2. READ FILE('FCLIENT') INTO(ENR-CLIENT) RIDFLD(WS-NUMCPT)     │
│     → Lecture du fichier VSAM                                   │
│  3. SEND MAP('MAPAFF') MAPSET('CLIAFF')                         │
│     → Affichage des donnees client                              │
│  4. RETURN TRANSID('AFFI') COMMAREA(...)                        │
│     → Le programme se termine a nouveau                         │
└─────────────────────────────────────────────────────────────────┘
```

> **Note importante** : Le "TASK TERMINATION" affiche dans CEDF est le comportement normal du mode pseudo-conversationnel. Le programme se termine pour liberer les ressources pendant que l'utilisateur reflechit, puis CICS le relance quand l'utilisateur appuie sur une touche.

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

> **Note TK4-** : La commande `CEDF OFF` n'est pas toujours disponible sur l'emulateur TK4-. Pour desactiver le mode debug, il suffit de se deconnecter du terminal CICS (CSSF LOGOFF) puis de se reconnecter, ou simplement d'ouvrir un nouveau terminal.

### Variables cles a observer dans CEDF

| Variable | Premier passage | Passages suivants |
|----------|-----------------|-------------------|
| EIBCALEN | 0 | 1 (longueur COMMAREA) |
| EIBAID | X'00' | DFHENTER (X'7D') ou DFHPF3 (X'F3') |
| EIBTRNID | 'AFFI' | 'AFFI' |
| EIBRESP | 0 (NORMAL) | 0 ou 13 (NOTFND) |

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

# Partie 2 : Operations CRUD

## Exercice 6 : MAP pour ajout de client

### Enonce

Creer ou adapter la MAP precedente pour une operation d'ajout de CLIENT dans le Data Set CLIENT.

### Mon travail

J'ai adapte la MAP d'affichage (CLIAFF) pour creer une nouvelle MAP de saisie (CLIAJT). La principale difference est que tous les champs sont maintenant saisissables (UNPROT) au lieu d'etre en affichage seul (ASKIP).

**Differences entre CLIAFF et CLIAJT :**

| Aspect | CLIAFF (Affichage) | CLIAJT (Ajout) |
|--------|-------------------|----------------|
| NUMCPT | UNPROT (saisie cle) | UNPROT (saisie) |
| Autres champs | ASKIP (affichage) | UNPROT (saisie) |
| Libelles (region, sexe...) | Affiches | Non affiches |
| Titre | "AFFICHAGE CLIENT" | "AJOUT CLIENT" |
| Touches | ENTER=Rechercher | ENTER=Valider |

### Resolution

**MAP BMS : CLIAJT.bms**

Le code source est stocke dans `ROCHA.CICS.SOURCE(CLIAJT)`. Voici le code complet :

```
***********************************************************************
*  MAPSET : CLIAJT - Ajout Client
*  Transaction : AJOU
*  Fil Rouge CICS - Exercice 6
***********************************************************************
CLIAJT   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAJT   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,28),LENGTH=24,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AJOUT CLIENT ***'
*----------------------------------------------------------------------
* ZONES DE SAISIE - TOUS LES CHAMPS EN UNPROT
*----------------------------------------------------------------------
         DFHMDF POS=(3,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(3,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(3,26),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(4,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(4,22),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(01=Paris,02=Mars...)'
*
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,INITIAL='NOM           :'
NOM      DFHMDF POS=(6,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(8,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(8,28),LENGTH=10,ATTRB=ASKIP,INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SEXE          :'
SEXE     DFHMDF POS=(9,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(9,21),LENGTH=8,ATTRB=ASKIP,INITIAL='(M ou F)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(10,19),LENGTH=2,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(11,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(11,21),LENGTH=12,ATTRB=ASKIP,INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(12,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(13,19),LENGTH=10,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,INITIAL='POSITION      :'
POSIT    DFHMDF POS=(14,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(14,22),LENGTH=10,ATTRB=ASKIP,INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE ET TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(18,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**Apercu de l'ecran MAPAJT :**

```
+------------------------------------------------------------------------------+
|                         *** AJOUT CLIENT ***                                 |
|                                                                              |
|  NUMERO COMPTE : ______                                                      |
|  CODE REGION   : __     (01=Paris,02=Mars...)                                |
|  NATURE COMPTE : __                                                          |
|  NOM           : __________                                                  |
|  PRENOM        : __________                                                  |
|  DATE NAISSANCE: ________  (AAAAMMJJ)                                        |
|  SEXE          : _  (M ou F)                                                 |
|  ACTIVITE PRO  : __                                                          |
|  SITUATION SOC : _  (C/M/D/V)                                                |
|  ADRESSE       : __________                                                  |
|  SOLDE         : __________                                                  |
|  POSITION      : __  (DB ou CR)                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|  MESSAGE : ____________________________________________________________      |
|                                                                              |
|                                                                              |
|                                                                              |
|  ENTER=Valider  PF3=Quitter  CLEAR=Effacer                                   |
+------------------------------------------------------------------------------+
```

**Zones de saisie :**

| Zone | Longueur | Attribut | Aide affichee |
|------|----------|----------|---------------|
| NUMCPT | 6 | UNPROT,NUM,IC | - |
| CODREG | 2 | UNPROT,NUM | (01=Paris,02=Mars...) |
| NATCPT | 2 | UNPROT,NUM | - |
| NOM | 10 | UNPROT | - |
| PRENOM | 10 | UNPROT | - |
| DATNA | 8 | UNPROT,NUM | (AAAAMMJJ) |
| SEXE | 1 | UNPROT | (M ou F) |
| ACTPRO | 2 | UNPROT,NUM | - |
| SITSO | 1 | UNPROT | (C/M/D/V) |
| ADRESSE | 10 | UNPROT | - |
| SOLDE | 10 | UNPROT,NUM | - |
| POSIT | 2 | UNPROT | (DB ou CR) |

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex06-1 : Source BMS dans ISPF EDIT - ROCHA.CICS.SOURCE(CLIAJT)
2. pt2ex06-2 : Soumission JCL assemblage BMS
3. pt2ex06-3 : SDSF - Job output avec RC=0000
4. pt2ex06-4 : Ecran MAPAJT vide - pret pour saisie
-->

---

## Exercice 7 : Programme d'ajout (WRITE)

### Enonce

Creer le PROGRAMME pour une operation d'ajout d'un nouveau CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et de doublure doit etre effectue.

### Mon travail

J'ai developpe le programme PRGAJT qui gere l'ajout de nouveaux clients avec les fonctionnalites suivantes :

1. **Mode pseudo-conversationnel** : Premier passage affiche ecran vide, passages suivants traitent la saisie
2. **Gestion MAPFAIL** : Detection si l'utilisateur n'a saisi aucune donnee
3. **Controles de conformite** avant ecriture :
   - Numero de compte obligatoire et numerique (6 chiffres)
   - Code region valide (01, 02, 03 ou 04)
   - Nom obligatoire
   - Sexe valide (M ou F)
   - Situation sociale valide (C, M, D ou V)
   - Position valide (DB ou CR)
4. **Verification de doublure** : READ pour verifier que le client n'existe pas deja
5. **Ecriture VSAM** : WRITE avec gestion des erreurs (DUPREC, etc.)

**Point technique** : La commande `EXIT PARAGRAPH` n'etant pas supportee sur la version COBOL de TK4-, j'ai utilise le pattern `GO TO paragraphe-FIN` pour sortir des validations en cas d'erreur.

### Resolution

**Programme : PRGAJT.cbl**

Le code source est stocke dans `ROCHA.CICS.SOURCE(PRGAJT)`. Voici les extraits principaux :

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGAJT.
      ******************************************************************
      * PROGRAMME : PRGAJT - Ajout client
      * TRANSACTION : AJOU
      * MODE : Pseudo-conversationnel
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMMAREA.
           05 WS-FLAG-INIT         PIC X(01) VALUE 'N'.

      * Copybooks CICS
       COPY DFHAID.
       COPY CLIAJT.

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
       01  WS-ERREUR               PIC X(01) VALUE 'N'.

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
               TRANSID('AJOU')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       2000-TRAITEMENT.
           MOVE 'N' TO WS-ERREUR

           EXEC CICS RECEIVE MAP('MAPAJT')
               MAPSET('CLIAJT')
               RESP(WS-RESP)
           END-EXEC

      * Gestion MAPFAIL (aucune donnee transmise)
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPAJTO
               MOVE 'AUCUNE DONNEE SAISIE - VEUILLEZ REMPLIR' TO MSGO
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Validation des donnees
           PERFORM 2100-VALIDER-DONNEES
           IF WS-ERREUR = 'O'
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verification doublure (client existe deja ?)
           PERFORM 2200-VERIFIER-DOUBLURE
           IF WS-ERREUR = 'O'
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Preparation et ecriture de l'enregistrement
           PERFORM 2300-PREPARER-ENREGISTREMENT
           PERFORM 2400-ECRIRE-CLIENT

           EXEC CICS SEND MAP('MAPAJT')
               MAPSET('CLIAJT')
           END-EXEC.

       2000-FIN.
           EXIT.

       2100-VALIDER-DONNEES.
           MOVE LOW-VALUES TO MAPAJTO

      * Controle numero de compte (obligatoire et numerique)
           IF NUMCPTL = 0 OR NUMCPTI = SPACES
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF NUMCPTI NOT NUMERIC
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle code region (01, 02, 03 ou 04)
           IF CODREGI NOT = '01' AND CODREGI NOT = '02'
              AND CODREGI NOT = '03' AND CODREGI NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle sexe (M ou F)
           IF SEXEI NOT = 'M' AND SEXEI NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle situation sociale (C, M, D ou V)
           IF SITSOI NOT = 'C' AND SITSOI NOT = 'M'
              AND SITSOI NOT = 'D' AND SITSOI NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle position (DB ou CR)
           IF POSITI NOT = 'DB' AND POSITI NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF.

       2100-FIN.
           EXIT.

       2200-VERIFIER-DOUBLURE.
           MOVE NUMCPTI TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT EN DOUBLE - CE CLIENT EXISTE DEJA'
                   TO MSGO
               MOVE 'O' TO WS-ERREUR
           END-IF.

       2400-ECRIRE-CLIENT.
           EXEC CICS WRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPAJTO
                   MOVE 'CLIENT AJOUTE AVEC SUCCES - NOUVEAU OU PF3'
                       TO MSGO
               WHEN DFHRESP(DUPREC)
                   MOVE 'ENREGISTREMENT EN DOUBLE' TO MSGO
                   MOVE 'O' TO WS-ERREUR
               WHEN OTHER
                   MOVE 'ERREUR ECRITURE FICHIER - CONTACTEZ SUPPORT'
                       TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.
```

**JCL de compilation : CMPAJT.jcl (ROCHA06)**

```jcl
//ROCHA06 JOB (ACCT),'COMPILE PRGAJT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* COMPILATION DU PROGRAMME COBOL-CICS PRGAJT (AJOUT CLIENT)
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGAJT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGAJT(R)
/*
```

**Structure du programme :**

| Paragraphe | Fonction |
|------------|----------|
| 0000-PRINCIPAL | Point d'entree, aiguillage selon EIBCALEN et EIBAID |
| 1000-PREMIER-PASSAGE | Affichage de l'ecran vide pour saisie |
| 2000-TRAITEMENT | Reception saisie, validations, ecriture |
| 2100-VALIDER-DONNEES | Controles de conformite des champs |
| 2200-VERIFIER-DOUBLURE | Verification que le client n'existe pas |
| 2300-PREPARER-ENREGISTREMENT | Transfert MAP vers enregistrement |
| 2400-ECRIRE-CLIENT | WRITE VSAM avec gestion erreurs |
| 9000-FIN-PROGRAMME | Message de fin et RETURN sans TRANSID |

**Commandes CICS utilisees :**

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'ecran (avec ERASE au premier passage) |
| RECEIVE MAP | Recevoir la saisie avec RESP pour MAPFAIL |
| READ FILE | Verifier si client existe (doublure) |
| WRITE FILE | Ecrire le nouveau client |
| RETURN TRANSID | Retour pseudo-conversationnel |

**Messages d'erreur geres :**

| Message | Contexte |
|---------|----------|
| AUCUNE DONNEE SAISIE | MAPFAIL - utilisateur a appuye ENTER sans rien saisir |
| NUMERO DE COMPTE OBLIGATOIRE | Champ NUMCPT vide |
| NUMERO DE COMPTE DOIT ETRE NUMERIQUE | Champ NUMCPT non numerique |
| CODE REGION INVALIDE | Code region different de 01/02/03/04 |
| SEXE INVALIDE | Sexe different de M ou F |
| SITUATION INVALIDE | Situation different de C/M/D/V |
| POSITION INVALIDE | Position differente de DB ou CR |
| ENREGISTREMENT EN DOUBLE | Client avec ce numero existe deja |
| CLIENT AJOUTE AVEC SUCCES | Ecriture reussie |
| ERREUR ECRITURE FICHIER | Erreur VSAM autre |

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex07-1 : Source COBOL dans ISPF EDIT - ROCHA.CICS.SOURCE(PRGAJT)
2. pt2ex07-2 : Soumission JCL CMPAJT - compilation du programme
3. pt2ex07-3 : SDSF - Job output avec RC=0000 pour compilation
4. pt2ex07-4 : Verification ROCHA.CICS.LOAD - membre PRGAJT present
5. pt2ex07-5 : Ecran MAPAJT vide - premier passage (message "SAISIR LES DONNEES...")
6. pt2ex07-6 : Test erreur de validation - message "SEXE INVALIDE"
7. pt2ex07-7 : Test doublon - message "ENREGISTREMENT EN DOUBLE"
8. pt2ex07-8 : Ajout reussi - message "CLIENT AJOUTE AVEC SUCCES"
-->

---

## Exercice 8 : Transaction d'ajout

### Enonce

Suivre cette operation par l'ajout d'une nouvelle Transaction dans le GROUP et activer la transaction en mode debugger CEDF et sans debugger.

### Resolution

```
CEDA DEFINE TRANSACTION(AJOU) GROUP(CLIGROUP)
     PROGRAM(PRGAJT)

CEDA DEFINE PROGRAM(PRGAJT) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'ecran

<!-- ![pt2ex08-1](images-pt2/pt2ex08-1.png) -->

---

## Exercice 9 : MAP pour mise a jour

### Enonce

Creer ou adapter la MAP precedente pour une operation de mise a jour de CLIENT dans le Data Set CLIENT.

### Mon travail

La MAP de mise a jour permet de :
1. Saisir le numero de compte (pour recherche)
2. Afficher les donnees existantes
3. Modifier les champs souhaites

Le numero de compte reste en lecture seule apres l'affichage (cle non modifiable).

### Resolution

**MAP BMS : CLIMAJ.bms**

Similaire a la MAP d'ajout, avec gestion dynamique des attributs pour passer le numero de compte en ASKIP apres affichage.

### Captures d'ecran

<!-- ![pt2ex09-1](images-pt2/pt2ex09-1.png) -->

---

## Exercice 10 : Programme de mise a jour (REWRITE)

### Enonce

Creer le PROGRAMME pour une operation de mise a jour d'un CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Mon travail

Le programme effectue :
1. Lecture du client avec READ UPDATE (verrouillage)
2. Affichage des donnees actuelles
3. Reception des modifications
4. Validation des donnees modifiees
5. REWRITE pour sauvegarder

### Resolution

**Programme : PRGMAJ.cbl**

```cobol
       2000-LIRE-CLIENT.
           MOVE NUMCPTI TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CLIENT INEXISTANT' TO MSGO
           ELSE IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 3000-AFFICHER-CLIENT
               MOVE 'MODIFIER ET VALIDER' TO MSGO
           END-IF.

       4000-MISE-A-JOUR.
           PERFORM 2100-VALIDER-DONNEES
           IF WS-ERREUR = 'O'
               EXIT PARAGRAPH
           END-IF

           PERFORM 2200-PREPARER-ENREG

           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'MISE A JOUR EFFECTUEE' TO MSGO
           ELSE
               MOVE 'ERREUR MISE A JOUR' TO MSGO
           END-IF.
```

### Captures d'ecran

<!-- ![pt2ex10-1](images-pt2/pt2ex10-1.png) -->

---

## Exercice 11 : Transaction de mise a jour

### Resolution

```
CEDA DEFINE TRANSACTION(MAJO) GROUP(CLIGROUP)
     PROGRAM(PRGMAJ)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'ecran

<!-- ![pt2ex11-1](images-pt2/pt2ex11-1.png) -->

---

## Exercice 12 : MAP pour suppression

### Enonce

Creer ou adapter la MAP precedente pour une operation de suppression de CLIENT dans le Data Set CLIENT.

### Mon travail

La MAP de suppression affiche les donnees du client a supprimer et demande confirmation avant la suppression.

### Captures d'ecran

<!-- ![pt2ex12-1](images-pt2/pt2ex12-1.png) -->

---

## Exercice 13 : Programme de suppression (DELETE)

### Enonce

Creer le PROGRAMME pour une operation de suppression d'un CLIENT dans le Data Set CLIENT en precisant le code CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Resolution

**Programme : PRGSUP.cbl**

```cobol
       2000-SUPPRIMER-CLIENT.
           MOVE NUMCPTI TO CLI-NUMCPT

      * Verification existence
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CLIENT INEXISTANT' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Suppression
           EXEC CICS DELETE
               FILE('FCLIENT')
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'ERREUR SUPPRESSION' TO MSGO
           END-IF.
```

### Captures d'ecran

<!-- ![pt2ex13-1](images-pt2/pt2ex13-1.png) -->

---

## Exercice 14 : Transaction de suppression

### Resolution

```
CEDA DEFINE TRANSACTION(SUPP) GROUP(CLIGROUP)
     PROGRAM(PRGSUP)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'ecran

<!-- ![pt2ex14-1](images-pt2/pt2ex14-1.png) -->

---

## Exercice 15 : Suppression avec lecture prealable

### Enonce

Reprendre cette operation de suppression en la precedant par une operation de lecture. Definir une transaction independante de la precedente.

### Mon travail

Cette version affiche d'abord les donnees du client avant de demander confirmation pour la suppression. Cela permet a l'utilisateur de verifier qu'il supprime le bon client.

### Resolution

**Programme : PRGSUL.cbl** (Suppression avec Lecture)

```cobol
       01 WS-PHASE             PIC X(01).
          88 PHASE-SAISIE      VALUE '1'.
          88 PHASE-CONFIRM     VALUE '2'.

       2000-TRAITEMENT.
           EVALUATE TRUE
               WHEN PHASE-SAISIE
                   PERFORM 3000-RECHERCHER-CLIENT
               WHEN PHASE-CONFIRM
                   PERFORM 4000-CONFIRMER-SUPPRESSION
           END-EVALUATE.

       3000-RECHERCHER-CLIENT.
      * Lecture et affichage du client
           EXEC CICS READ FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM AFFICHER-DONNEES
               MOVE 'CONFIRMER SUPPRESSION (O/N) ?' TO MSGO
               MOVE '2' TO WS-PHASE
           END-IF.

       4000-CONFIRMER-SUPPRESSION.
      * Si confirmation, suppression
           IF CONFIRMI = 'O'
               EXEC CICS DELETE FILE('FCLIENT')
                   RIDFLD(CLI-NUMCPT)
                   RESP(WS-RESP)
               END-EXEC
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'SUPPRESSION ANNULEE' TO MSGO
           END-IF
           MOVE '1' TO WS-PHASE.
```

**Transaction independante :**

```
CEDA DEFINE TRANSACTION(SULE) GROUP(CLIGROUP)
     PROGRAM(PRGSUL)
```

### Captures d'ecran

<!-- ![pt2ex15-1](images-pt2/pt2ex15-1.png) -->

---

# Partie 3 : Operations avancees

## Exercice 16 : Creation de clients generiques

### Enonce

Sachant que le CODE CLIENT est sur six caracteres, creer cinq CLIENT avec une partie de leur code generique commencant par '111...', de meme '444...' et '777...'.

### Mon travail

J'ai cree 15 clients de test avec des codes generiques :
- 111001, 111002, 111003, 111004, 111005
- 444001, 444002, 444003, 444004, 444005
- 777001, 777002, 777003, 777004, 777005

Ces clients serviront aux tests des commandes STARTBR et READNEXT.

### Resolution

Utilisation de la transaction AJOU pour creer les 15 clients.

### Captures d'ecran

<!-- ![pt3ex16-1](images-pt3/pt3ex16-1.png) -->

---

## Exercice 17 : Suppression par code generique (STARTBR)

### Enonce

En utilisant les commandes adequates, supprimer les CLIENT dont le code generique est '111...'.

### Mon travail

J'ai utilise STARTBR pour positionner le curseur sur le premier client '111', puis READNEXT en boucle pour lire et supprimer chaque client commencant par '111'.

### Resolution

**Programme : PRGSDEL.cbl** (Suppression Generique)

```cobol
       2000-SUPPRIMER-GENERIQUE.
           MOVE '111000' TO WS-CLE-DEBUT

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'AUCUN CLIENT 111XXX TROUVE' TO MSGO
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE IF WS-CLE-COURANTE(1:3) NOT = '111'
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE
                   EXEC CICS DELETE
                       FILE('FCLIENT')
                       RIDFLD(WS-CLE-COURANTE)
                   END-EXEC
                   ADD 1 TO WS-COMPTEUR-SUP
               END-IF
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.
```

### Captures d'ecran

<!-- ![pt3ex17-1](images-pt3/pt3ex17-1.png) -->

---

## Exercice 18 : Lecture successive (READNEXT, ENDBR)

### Enonce

Faire une lecture successive des CLIENT dont le code generique est '222...' en utilisant la commande READNEXT et ENDBR.

### Mon travail

Ce programme illustre le parcours sequentiel d'un fichier VSAM avec positionnement generique :
1. STARTBR avec GTEQ pour se positionner sur le premier '222xxx'
2. READNEXT en boucle pour lire les suivants
3. Arret quand le code ne commence plus par '222'
4. ENDBR pour terminer le browse

### Resolution

**Programme : PRGLGEN.cbl** (Liste Generique)

```cobol
       2000-LISTER-GENERIQUE.
           MOVE '222000' TO WS-CLE-DEBUT
           MOVE 0 TO WS-COMPTEUR

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:3) NOT = '222'
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       PERFORM 3000-AFFICHER-LIGNE
                       ADD 1 TO WS-COMPTEUR
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

           DISPLAY 'TOTAL CLIENTS 222XXX : ' WS-COMPTEUR.
```

### Captures d'ecran

<!-- ![pt3ex18-1](images-pt3/pt3ex18-1.png) -->

---

## Exercice 19 : Statistiques par region

### Enonce

Elaborer une transaction permettant de calculer pour une REGION le nombre de CLIENT, la somme des montants des CLIENT Debiteurs et leur nombre et la somme des montants des CLIENT Crediteurs et leur nombre. Cette transaction aura en entree le code REGION et affichera les quatre informations specifiees ci-dessus.

### Mon travail

Cette transaction effectue un parcours complet du fichier pour calculer les statistiques d'une region donnee :
- Nombre total de clients de la region
- Nombre et somme des clients debiteurs (DB)
- Nombre et somme des clients crediteurs (CR)

J'utilise STARTBR/READNEXT pour parcourir tout le fichier et je filtre sur le code region.

### Resolution

**MAP BMS : CLISTAT.bms**

```
* Zone de saisie code region
         DFHMDF POS=(3,2),LENGTH=15,                                   X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(3,18),LENGTH=2,                                   X
               ATTRB=(UNPROT,NUM,IC)

* Zones d'affichage des statistiques
         DFHMDF POS=(6,2),LENGTH=25,                                   X
               INITIAL='NOMBRE TOTAL CLIENTS    :'
NBTOT    DFHMDF POS=(6,28),LENGTH=5,ATTRB=(ASKIP)

         DFHMDF POS=(8,2),LENGTH=25,                                   X
               INITIAL='CLIENTS DEBITEURS       :'
NBDB     DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP)
MTDB     DFHMDF POS=(8,35),LENGTH=12,ATTRB=(ASKIP)

         DFHMDF POS=(10,2),LENGTH=25,                                  X
               INITIAL='CLIENTS CREDITEURS      :'
NBCR     DFHMDF POS=(10,28),LENGTH=5,ATTRB=(ASKIP)
MTCR     DFHMDF POS=(10,35),LENGTH=12,ATTRB=(ASKIP)
```

**Programme : PRGSTAT.cbl**

```cobol
       WORKING-STORAGE SECTION.
       01 WS-STATS.
          05 WS-NB-TOTAL         PIC 9(05) VALUE 0.
          05 WS-NB-DEBITEURS     PIC 9(05) VALUE 0.
          05 WS-MT-DEBITEURS     PIC 9(10) VALUE 0.
          05 WS-NB-CREDITEURS    PIC 9(05) VALUE 0.
          05 WS-MT-CREDITEURS    PIC 9(10) VALUE 0.

       2000-CALCULER-STATS.
           INITIALIZE WS-STATS
           MOVE CODREGI TO WS-CODE-REGION

      * Verification region existante
           IF WS-CODE-REGION NOT = '01' AND '02' AND '03' AND '04'
               MOVE 'REGION INEXISTANTE, SAISIR CODE REGION' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Parcours du fichier
           MOVE LOW-VALUES TO WS-CLE-DEBUT
           EXEC CICS STARTBR FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE
                   IF CLI-CODREG = WS-CODE-REGION
                       ADD 1 TO WS-NB-TOTAL
                       IF CLI-DEBITEUR
                           ADD 1 TO WS-NB-DEBITEURS
                           ADD CLI-SOLDE TO WS-MT-DEBITEURS
                       ELSE
                           ADD 1 TO WS-NB-CREDITEURS
                           ADD CLI-SOLDE TO WS-MT-CREDITEURS
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC
           PERFORM 3000-AFFICHER-RESULTATS.

       3000-AFFICHER-RESULTATS.
           MOVE WS-NB-TOTAL      TO NBTOTO
           MOVE WS-NB-DEBITEURS  TO NBDBO
           MOVE WS-MT-DEBITEURS  TO MTDBO
           MOVE WS-NB-CREDITEURS TO NBCRO
           MOVE WS-MT-CREDITEURS TO MTCRO
           MOVE 'STATISTIQUES CALCULEES' TO MSGO.
```

**Transaction :**

```
CEDA DEFINE TRANSACTION(STAT) GROUP(CLIGROUP)
     PROGRAM(PRGSTAT)
```

### Captures d'ecran

<!-- ![pt3ex19-1](images-pt3/pt3ex19-1.png) -->

---

# Annexes

## Liste des programmes COBOL-CICS

| Programme | Transaction | Description |
|-----------|-------------|-------------|
| PRGCLIA | AFFI | Affichage d'un client |
| PRGAJT | AJOU | Ajout d'un nouveau client |
| PRGMAJ | MAJO | Mise a jour d'un client |
| PRGSUP | SUPP | Suppression d'un client |
| PRGSUL | SULE | Suppression avec lecture prealable |
| PRGSDEL | SDEL | Suppression par code generique |
| PRGLGEN | LGEN | Liste par code generique (READNEXT) |
| PRGSTAT | STAT | Statistiques par region |

## Liste des MAPs BMS

| Mapset | Map | Description |
|--------|-----|-------------|
| CLIAFF | MAPAFF | Ecran d'affichage client |
| CLIAJT | MAPAJT | Ecran d'ajout client |
| CLIMAJ | MAPMAJ | Ecran de mise a jour |
| CLISUP | MAPSUP | Ecran de suppression |
| CLISTAT | MAPSTAT | Ecran de statistiques |

## Liste des transactions CICS

| Code | Description |
|------|-------------|
| AFFI | Affichage client |
| AJOU | Ajout client |
| MAJO | Mise a jour client |
| SUPP | Suppression client |
| SULE | Suppression avec lecture |
| SDEL | Suppression generique |
| LGEN | Liste generique |
| STAT | Statistiques region |

## Commandes CICS utilisees

| Commande | Utilisation |
|----------|-------------|
| SEND MAP | Envoi d'ecran |
| RECEIVE MAP | Reception des donnees saisies |
| READ | Lecture directe par cle |
| WRITE | Ecriture nouvel enregistrement |
| REWRITE | Mise a jour enregistrement |
| DELETE | Suppression enregistrement |
| STARTBR | Debut de parcours (browse) |
| READNEXT | Lecture suivante |
| ENDBR | Fin de parcours |
| RETURN TRANSID | Retour pseudo-conversationnel |

## Messages d'erreur

| Message | Contexte |
|---------|----------|
| ENREGISTREMENT EN DOUBLE | Ajout d'un client existant |
| ZONE NUMERIQUE, RESAISIR CE CHAMP | Champ numerique invalide |
| SAISIE CORRECTE, CONTINUER (O/N) ? | Confirmation apres succes |
| REGION INEXISTANTE, SAISIR CODE REGION | Code region invalide |
| CLIENT INEXISTANT | Recherche sans resultat |
| SUPPRESSION EFFECTUEE | Confirmation suppression |
| MISE A JOUR EFFECTUEE | Confirmation mise a jour |

---

# Conclusion

Ce projet m'a permis de mettre en pratique l'ensemble des competences acquises durant la formation POEI Mainframe COBOL pour le volet CICS. A travers les trois parties du projet, j'ai pu :

- **Maitriser VSAM sous CICS** : Definition de fichiers KSDS, integration dans la FCT (File Control Table), et gestion des operations de lecture, ecriture, mise a jour et suppression.

- **Developper des ecrans BMS** : Conception de MAPs avec gestion des attributs (couleurs, protection), zones de saisie et d'affichage, messages d'erreur.

- **Programmer en COBOL-CICS** : Utilisation des commandes CICS (SEND/RECEIVE MAP, READ, WRITE, REWRITE, DELETE), gestion pseudo-conversationnelle avec RETURN TRANSID, et navigation VSAM avec STARTBR/READNEXT/ENDBR.

- **Administrer les transactions** : Definition via CEDA, installation de groupes, tests avec CEDF.

Le projet couvre un cas concret de gestion clientele dans le secteur financier, avec 8 programmes COBOL-CICS, 5 MAPs BMS et 8 transactions. Les principales difficultes rencontrees (gestion des attributs BMS, validation des donnees, navigation VSAM) m'ont permis de developper une approche methodique de resolution de problemes.

Cette experience constitue une base solide pour aborder des projets mainframe transactionnels en entreprise.

---

*Rapport realise par Josue ROCHA - Formation POEI Mainframe COBOL - M2i Formation, Strasbourg - Janvier 2026*
