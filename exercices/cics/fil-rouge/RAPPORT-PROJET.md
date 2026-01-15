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

### Membres crees dans SOURCE

Au cours du projet, les membres suivants ont ete crees dans `ROCHA.CICS.SOURCE` :

**Programmes COBOL :**

| Membre | Transaction | Description |
|--------|-------------|-------------|
| PRGCLIA | AFFI | Affichage client (READ) |
| PRGAJT | AJOU | Ajout client (WRITE) |
| PRGMAJ | MAJO | Mise a jour client (REWRITE) |
| PRGSUP | SUPP | Suppression client (DELETE) |
| PRGLGEN | LGEN | Liste generique (STARTBR/READNEXT) |
| PRGSTAT | STAT | Statistiques par region |

**MAPs BMS :**

| Membre | Programme | Description |
|--------|-----------|-------------|
| CLIAFF | PRGCLIA | Ecran affichage client |
| CLIAJT | PRGAJT | Ecran ajout client |
| CLIMAJ | PRGMAJ | Ecran mise a jour client |
| CLISUP | PRGSUP | Ecran suppression client |
| CLISTAT | PRGSTAT | Ecran statistiques |

**JCL :**

| Membre | Usage | Exercice |
|--------|-------|----------|
| DEFVSAM | Definition cluster VSAM | Ex 1 |
| LOADVSAM | Chargement donnees initiales | Ex 1 |
| ASMCLAF | Assemblage MAP CLIAFF | Ex 2 |
| CMPCLAF | Compilation PRGCLIA | Ex 3 |
| ASMAJT | Assemblage MAP CLIAJT | Ex 6 |
| CMPAJT | Compilation PRGAJT | Ex 7 |

> **Note sur les copybooks** : Les copybooks pour les MAPs BMS sont generes automatiquement lors de l'assemblage avec l'option `TYPE=DSECT`. Ils contiennent les structures de donnees avec les suffixes :
> - `I` : Zone input (donnees recues de l'ecran)
> - `O` : Zone output (donnees a envoyer)
> - `L` : Longueur du champ saisi
> - `A` : Attribut du champ (couleur, intensite, etc.)

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
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
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

      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
      * Important : sauvegarder aussi les champs longueur (L) pour les validations
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE NUMCPTL   TO WS-NUMCPTL
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL
           ...

      * Validation des donnees
           PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verification doublure (client existe deja ?)
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
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

      * Controle numero de compte (utilise variables WS- sauvegardees)
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle code region (utilise WS-CODREG sauvegardee)
           IF WS-CODREGL = 0 OR WS-CODREG = SPACES
               MOVE 'CODE REGION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle nom (obligatoire)
           IF WS-NOML = 0 OR WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle sexe (utilise WS-SEXE sauvegardee)
           IF WS-SEXEL = 0 OR WS-SEXE = SPACES
               MOVE 'SEXE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle situation sociale (utilise WS-SITSO sauvegardee)
           IF WS-SITSOL = 0 OR WS-SITSO = SPACES
               MOVE 'SITUATION SOCIALE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle position (utilise WS-POSITION sauvegardee)
           IF WS-POSITL = 0 OR WS-POSITION = SPACES
               MOVE 'POSITION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF.

       2100-FIN.
           EXIT.

       2200-VERIFIER-DOUBLURE.
           MOVE WS-NUMCPT TO CLI-NUMCPT

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
| NUMERO DE COMPTE OBLIGATOIRE | Champ NUMCPT vide (longueur = 0) |
| NUMERO DE COMPTE DOIT ETRE NUMERIQUE | Champ NUMCPT contient des caracteres non numeriques |
| CODE REGION OBLIGATOIRE | Champ CODREG vide |
| CODE REGION INVALIDE | Code region different de 01/02/03/04 |
| NOM OBLIGATOIRE | Champ NOM vide |
| SEXE OBLIGATOIRE | Champ SEXE vide |
| SEXE INVALIDE | Sexe different de M ou F |
| SITUATION SOCIALE OBLIGATOIRE | Champ SITSO vide |
| SITUATION INVALIDE | Situation differente de C/M/D/V |
| POSITION OBLIGATOIRE | Champ POSIT vide |
| POSITION INVALIDE | Position differente de DB ou CR |
| ENREGISTREMENT EN DOUBLE | Client avec ce numero existe deja (READ a trouve un enregistrement) |
| CLIENT AJOUTE AVEC SUCCES | WRITE VSAM reussi |
| ERREUR ECRITURE FICHIER | Erreur VSAM inattendue (ni NORMAL ni DUPREC) |

### Difficultes rencontrees et solutions

#### Probleme 1 : Ecrasement des donnees saisies par LOW-VALUES

**Symptome** : Apres le `RECEIVE MAP`, les donnees saisies etaient perdues lors du `MOVE LOW-VALUES TO MAPAJTO` dans le paragraphe de validation.

**Cause** : Avec `MODE=INOUT` et `STORAGE=AUTO` dans la definition BMS, les zones input (suffixe I) et output (suffixe O) partagent la meme zone memoire. Le `MOVE LOW-VALUES TO MAPAJTO` ecrasait donc les donnees recues.

**Solution** : Sauvegarder les donnees saisies dans des variables Working-Storage (prefixe WS-) immediatement apres le `RECEIVE MAP`, avant tout `MOVE LOW-VALUES`.

```cobol
      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
      * Sauvegarder aussi les champs longueur (suffixe L) pour les validations
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE NUMCPTL   TO WS-NUMCPTL
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL
```

#### Probleme 2 : Validations ignorees - le client etait ajoute malgre les erreurs

**Symptome** : Meme avec des donnees invalides (sexe = 'X'), le client etait ajoute dans le fichier. Les messages d'erreur s'affichaient dans CEDF mais le programme continuait jusqu'au WRITE.

**Cause** : Le `GO TO paragraphe-FIN` dans les validations sortait de la plage du `PERFORM`, ce qui faisait continuer le programme sequentiellement vers les paragraphes suivants (2200, 2300, 2400...) au lieu de retourner a l'appelant.

En COBOL, quand on fait :
```cobol
       PERFORM 2100-VALIDER-DONNEES
```

Et dans 2100-VALIDER-DONNEES on fait :
```cobol
       GO TO 2100-FIN
```

Le `GO TO` sort du PERFORM car `2100-FIN` est un paragraphe separe. Le programme continue alors sequentiellement apres 2100-FIN.

**Solution** : Utiliser la clause `THRU` pour inclure le paragraphe FIN dans la plage du PERFORM :

```cobol
       PERFORM 2000-TRAITEMENT THRU 2000-FIN
       ...
       PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
       ...
       PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
```

Avec `THRU`, le `GO TO 2100-FIN` reste dans la plage du PERFORM, et apres le `EXIT` de 2100-FIN, le controle retourne correctement a l'appelant.

#### Probleme 3 : Message d'erreur non visible sans CEDF

**Symptome** : Le message d'erreur de validation s'affichait dans CEDF mais pas sur l'ecran normal.

**Cause** : Le `SEND MAP` apres detection d'erreur n'avait pas l'option `ERASE`, donc l'ecran precedent restait visible.

**Solution** : Ajouter `ERASE` au `SEND MAP` d'erreur :

```cobol
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF
```

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

### Mon travail

Pour que la transaction AJOU fonctionne, je dois definir et installer trois ressources CICS :

1. **MAPSET CLIAJT** : L'ecran BMS compile (exercice 6)
2. **PROGRAM PRGAJT** : Le programme COBOL-CICS compile (exercice 7)
3. **TRANSACTION AJOU** : Le code de 4 caracteres qui lance le programme

L'ordre de definition est important : le programme doit etre defini avant la transaction (car TRANSACTION reference PROGRAM).

### Resolution

**Etape 1 : Definition des ressources**

```
CEDA DEFINE MAPSET(CLIAJT) GROUP(CLIGROUP)

CEDA DEFINE PROGRAM(PRGAJT) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA DEFINE TRANSACTION(AJOU) GROUP(CLIGROUP)
     PROGRAM(PRGAJT)
```

**Etape 2 : Installation des ressources**

*Option A : Installation individuelle (recommandee)*

```
CEDA INSTALL MAPSET(CLIAJT) GROUP(CLIGROUP)
CEDA INSTALL PROGRAM(PRGAJT) GROUP(CLIGROUP)
CEDA INSTALL TRANSACTION(AJOU) GROUP(CLIGROUP)
```

*Option B : Installation du groupe complet*

```
CEDA INSTALL GROUP(CLIGROUP)
```

> **Note** : Si certaines ressources sont deja installees (FCLIENT, CLIAFF, PRGCLIA, AFFI), des erreurs "ALREADY INSTALLED" apparaitront. C'est normal et les nouvelles ressources seront quand meme installees.

**Etape 3 : Verification avec CEMT et CEDA**

```
CEDA VIEW MAPSET(CLIAJT) GROUP(CLIGROUP)
```
Resultat attendu : Affichage de la definition du mapset

```
CEMT INQ PROG(PRGAJT)
```
Resultat attendu : `Pro(PRGAJT) Len(...) Cob Ena Pri`

```
CEMT INQ TRAN(AJOU)
```
Resultat attendu : `Tra(AJOU) Pro(PRGAJT) Ena`

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour verifier un mapset, utiliser `CEDA VIEW MAPSET(nom) GROUP(groupe)`.

**Tableau recapitulatif du groupe CLIGROUP apres exercice 8 :**

| Ressource | Nom | Defini dans | Description |
|-----------|-----|-------------|-------------|
| FILE | FCLIENT | Exercice 1 | Fichier VSAM CLIENT |
| MAPSET | CLIAFF | Exercice 4 | Ecran d'affichage |
| PROGRAM | PRGCLIA | Exercice 4 | Programme d'affichage |
| TRANSACTION | AFFI | Exercice 4 | Transaction d'affichage |
| MAPSET | CLIAJT | Exercice 8 | Ecran d'ajout |
| PROGRAM | PRGAJT | Exercice 8 | Programme d'ajout |
| TRANSACTION | AJOU | Exercice 8 | Transaction d'ajout |

**Etape 4 : Test avec CEDF**

```
CEDF
AJOU
```

Observer les points d'arret :
1. SEND MAP (ecran vide)
2. RETURN TRANSID (fin premier passage)
3. RECEIVE MAP (reception saisie)
4. READ FILE (verification doublure)
5. WRITE FILE (ecriture client)
6. SEND MAP (message succes)
7. RETURN TRANSID (fin traitement)

> **Note importante sur NOTFND** : Lors du point d'arret 4 (READ FILE), CEDF affiche souvent une reponse `NOTFND`. C'est le comportement **attendu et normal** ! Ce READ sert a verifier que le client n'existe pas deja (controle de doublure). Si NOTFND est retourne, cela signifie que le numero de compte est disponible et que le programme peut proceder au WRITE. Ce n'est pas une erreur mais une verification reussie.

**Etape 5 : Test sans debugger**

Depuis un ecran CICS vierge (sans CEDF actif) :

```
AJOU
```

Tester les scenarios suivants :
- Saisir un nouveau client complet et valider → message "CLIENT AJOUTE AVEC SUCCES"
- Ressaisir le meme numero → message "ENREGISTREMENT EN DOUBLE"
- Saisir un sexe invalide → message "SEXE INVALIDE"
- Appuyer ENTER sans rien saisir → message "AUCUNE DONNEE SAISIE"
- Appuyer PF3 → fin de la transaction

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex08-1 : CEDA DEFINE MAPSET(CLIAJT) - definition du mapset
2. pt2ex08-2 : CEDA DEFINE PROGRAM(PRGAJT) - definition du programme
3. pt2ex08-3 : CEDA DEFINE TRANSACTION(AJOU) - definition de la transaction
4. pt2ex08-4 : CEDA INSTALL avec message de succes
5. pt2ex08-5 : CEMT INQ TRAN(AJOU) - verification transaction active
6. pt2ex08-6 : Test CEDF - point d'arret sur WRITE FILE
7. pt2ex08-7 : Ecran MAPAJT - saisie d'un nouveau client
8. pt2ex08-8 : Message "CLIENT AJOUTE AVEC SUCCES" apres ajout
9. pt2ex08-9 : Verification avec AFFI - le nouveau client existe
-->

---

## Exercice 9 : MAP pour mise a jour

### Enonce

Creer ou adapter la MAP precedente pour une operation de mise a jour de CLIENT dans le Data Set CLIENT.

### Mon travail

La MAP de mise a jour differe de celle d'ajout par la **gestion dynamique des attributs** :

1. **Phase 1 (Recherche)** : Le numero de compte est saisissable (UNPROT) pour permettre la recherche du client
2. **Phase 2 (Affichage)** : Apres lecture du client, le numero de compte passe en lecture seule (ASKIP) car la cle d'un enregistrement VSAM ne peut pas etre modifiee
3. **Phase 3 (Modification)** : L'utilisateur modifie les autres champs et valide

Cette gestion dynamique se fait dans le programme COBOL via le **suffixe 'A'** (Attribut) du copybook genere :

```cobol
* Proteger le numero de compte apres affichage
MOVE DFHBMASK TO NUMCPTA
```

### Resolution

**MAP BMS : CLIMAJ.bms**

```
***********************************************************************
*  MAPSET : CLIMAJ - Mise a jour Client
*  Transaction : MAJO
*  Fil Rouge CICS - Exercice 9
*
*  PARTICULARITE MISE A JOUR :
*  ---------------------------
*  Le numero de compte est d'abord saisissable (recherche),
*  puis passe en lecture seule apres affichage des donnees.
*  Cette gestion dynamique des attributs se fait dans le programme
*  COBOL via le suffixe 'A' (ex: NUMCPTA pour modifier l'attribut).
***********************************************************************
CLIMAJ   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPMAJ   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** MISE A JOUR CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* NUMERO DE COMPTE - CHAMP CLE
* Commence en UNPROT pour la saisie initiale (recherche)
* Le programme passera l'attribut a ASKIP apres affichage
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(Cle - non modifiable)'
*----------------------------------------------------------------------
* ZONES DE SAISIE/MODIFICATION (meme structure que CLIAJT)
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**JCL d'assemblage : ASMMAJ.jcl**

```jcl
//ROCHA09 JOB (ACCT),'ASSEMBL BMS CLIMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 9
//* ASSEMBLAGE DE LA MAP BMS CLIMAJ (MISE A JOUR CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Le copybook genere contiendra pour chaque champ :
//*   - NOMCPTx  ou x = I (input), O (output), L (longueur), A (attr)
//*   - Le suffixe 'A' permet de modifier l'attribut dynamiquement
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//* ASSEMBLAGE DE LA MAP BMS
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIMAJ',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIMAJ),DISP=SHR
/*
//
```

### Concept cle : Attributs dynamiques BMS

Le copybook genere par l'assemblage BMS contient pour chaque champ un suffixe `A` permettant de modifier l'attribut a l'execution :

| Constante CICS | Valeur | Description |
|----------------|--------|-------------|
| DFHBMASK | X'20' | ASKIP - Protege, intensite normale |
| DFHBMPRF | X'28' | ASKIP - Protege, brillant |
| DFHBMUNN | X'4C' | UNPROT + NUM - Saisie numerique |
| DFHBMUNP | X'40' | UNPROT - Saisie alphanumerique |

### Definition CICS

```
CEDA DEFINE MAPSET(CLIMAJ) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

### Verification

```
CEDA VIEW MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour verifier un mapset, utiliser `CEDA VIEW`.

### Captures d'ecran

<!-- ![pt2ex09-1](images-pt2/pt2ex09-1.png) -->

---

## Exercice 10 : Programme de mise a jour (REWRITE)

### Enonce

Creer le PROGRAMME pour une operation de mise a jour d'un CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Mon travail

Le programme PRGMAJ implemente un mode **pseudo-conversationnel a 3 phases** :

1. **Phase RECHERCHE** : Saisie du numero de compte, verification existence
2. **Phase AFFICHAGE** : Affichage des donnees actuelles, NUMCPT protege (ASKIP)
3. **Phase VALIDATION** : Reception modifications, validation, READ UPDATE + REWRITE

**Points techniques importants :**

| Aspect | Explication |
|--------|-------------|
| COPY DFHBMSCA | Copybook pour les constantes d'attribut (DFHBMASK, etc.) |
| COMMAREA etendue | Sauvegarde la phase ET le numero de compte |
| READ UPDATE atomique | Le READ UPDATE et REWRITE doivent etre dans la meme UOW |
| NUMCPT sauvegarde | Necessaire car un champ ASKIP n'est pas transmis par le terminal |

### Resolution

**Programme : PRGMAJ.cbl**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMAJ.
      ******************************************************************
      * PROGRAMME : PRGMAJ
      * FONCTION  : Mise a jour d'un client existant
      * TRANSACTION : MAJO
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPMAJ (MAPSET CLIMAJ)
      *
      * MODE PSEUDO-CONVERSATIONNEL A 3 PHASES :
      * ----------------------------------------
      * Phase 1 (RECHERCHE) : Saisie numero, verification existence
      * Phase 2 (AFFICHAGE) : Affichage donnees, NUMCPT en ASKIP
      * Phase 3 (VALIDATION) : Modifications, READ UPDATE + REWRITE
      *
      * FIL ROUGE CICS - EXERCICE 10
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase et le numero de compte entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-RECHERCHE  VALUE '1'.
              88 PHASE-AFFICHAGE  VALUE '2'.
              88 PHASE-VALIDATION VALUE '3'.
           05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      *-----------------------------------------------------------------
       COPY CLIMAJ.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT          PIC X(06).
           05 CLI-CODREG          PIC X(02).
           05 CLI-NATCPT          PIC X(02).
           05 CLI-NOM             PIC X(10).
           05 CLI-PRENOM          PIC X(10).
           05 CLI-DATNAISS        PIC X(08).
           05 CLI-SEXE            PIC X(01).
           05 CLI-ACTPRO          PIC X(02).
           05 CLI-SITSO           PIC X(01).
           05 CLI-ADRESSE         PIC X(10).
           05 CLI-SOLDE           PIC X(10).
           05 CLI-POSITION        PIC X(02).
           05 FILLER              PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP                PIC S9(08) COMP VALUE 0.
       01  WS-ERREUR              PIC X(01) VALUE 'N'.
       01  WS-MSG-FIN             PIC X(40)
           VALUE 'TRANSACTION MAJO TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-NUMCPT           PIC X(06).
           05 WS-NUMCPTL          PIC S9(04) COMP.
           05 WS-CODREG           PIC X(02).
           05 WS-CODREGL          PIC S9(04) COMP.
           05 WS-NATCPT           PIC X(02).
           05 WS-NOM              PIC X(10).
           05 WS-NOML             PIC S9(04) COMP.
           05 WS-PRENOM           PIC X(10).
           05 WS-DATNAISS         PIC X(08).
           05 WS-SEXE             PIC X(01).
           05 WS-SEXEL            PIC S9(04) COMP.
           05 WS-ACTPRO           PIC X(02).
           05 WS-SITSO            PIC X(01).
           05 WS-SITSOL           PIC S9(04) COMP.
           05 WS-ADRESSE          PIC X(10).
           05 WS-SOLDE            PIC X(10).
           05 WS-POSITION         PIC X(02).
           05 WS-POSITL           PIC S9(04) COMP.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 1000-INIT-RECHERCHE
               WHEN EIBAID = DFHPF3
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
                   PERFORM 1000-INIT-RECHERCHE
               WHEN OTHER
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('MAJO')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT-RECHERCHE.
      *-----------------------------------------------------------------
      * Affichage ecran initial - NUMCPT en UNPROT
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE 'SAISIR LE NUMERO DE COMPTE A MODIFIER' TO MSGO
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-NUMCPT-SAVED

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
           MOVE 'N' TO WS-ERREUR

           EVALUATE TRUE
               WHEN PHASE-RECHERCHE
                   PERFORM 3000-RECHERCHER-CLIENT THRU 3000-FIN
               WHEN PHASE-AFFICHAGE
               WHEN PHASE-VALIDATION
                   PERFORM 4000-VALIDER-MODIFICATION THRU 4000-FIN
           END-EVALUATE.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-RECHERCHER-CLIENT.
      *-----------------------------------------------------------------
      * Phase 1 -> 2 : Recherche du client
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           MOVE NUMCPTI TO WS-NUMCPT

      *    Lecture du client
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Affichage des donnees
           PERFORM 3100-AFFICHER-CLIENT
           MOVE '2' TO WS-PHASE
           MOVE WS-NUMCPT TO WS-NUMCPT-SAVED.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Affiche les donnees - NUMCPT passe en ASKIP
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO

           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NATCPT   TO NATCPTO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-ACTPRO   TO ACTPROO
           MOVE CLI-SITSO    TO SITSOO
           MOVE CLI-ADRESSE  TO ADRESSEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITO

      *    IMPORTANT : Proteger le numero de compte
           MOVE DFHBMASK TO NUMCPTA

           MOVE 'CLIENT TROUVE - MODIFIER ET VALIDER AVEC ENTER' TO MSGO

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       4000-VALIDER-MODIFICATION.
      *-----------------------------------------------------------------
      * Phase 2/3 : Validation et REWRITE
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

      *    Sauvegarde des donnees modifiees
           MOVE WS-NUMCPT-SAVED TO WS-NUMCPT
           MOVE CODREGI   TO WS-CODREG
           MOVE NOMI      TO WS-NOM
      *    ... (autres champs)

      *    Validation
           PERFORM 4100-VALIDER-DONNEES THRU 4100-FIN

           IF WS-ERREUR = 'O'
               MOVE DFHBMASK TO NUMCPTA
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Mise a jour
           PERFORM 4300-ECRIRE-MODIFICATION THRU 4300-FIN

           MOVE DFHBMASK TO NUMCPTA
           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4300-ECRIRE-MODIFICATION.
      *-----------------------------------------------------------------
      * READ UPDATE + REWRITE atomique
      *-----------------------------------------------------------------
      *    Relecture avec UPDATE (verrouillage)
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

      *    Application des modifications
           MOVE WS-CODREG TO CLI-CODREG
           MOVE WS-NOM    TO CLI-NOM
      *    ... (autres champs)

      *    REWRITE - Mise a jour effective
           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE WS-NUMCPT TO NUMCPTO
               MOVE 'MISE A JOUR EFFECTUEE - NOUVEAU OU PF3' TO MSGO
               MOVE '1' TO WS-PHASE
               MOVE SPACES TO WS-NUMCPT-SAVED
           ELSE
               MOVE 'ERREUR MISE A JOUR - CONTACTEZ SUPPORT' TO MSGO
               MOVE 'O' TO WS-ERREUR
           END-IF.

       4300-FIN.
           EXIT.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
```

> **Note** : Le code ci-dessus est une version abregee. Le programme complet inclut tous les paragraphes de validation (4100-VALIDER-DONNEES) et la sauvegarde de tous les champs.

**JCL de compilation : CMPMAJ.jcl**

```jcl
//ROCHA10 JOB (ACCT),'COMPILE PRGMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 10
//* COMPILATION DU PROGRAMME COBOL-CICS PRGMAJ (MISE A JOUR CLIENT)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLIMAJ   : Structure MAP generee
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGMAJ),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGMAJ(R)
/*
//
```

### Concept cle : READ UPDATE + REWRITE

En mode pseudo-conversationnel, chaque interaction utilisateur termine la tache CICS. Or, REWRITE necessite un READ UPDATE prealable dans la **meme unite de travail (UOW)**.

**Solution :** Faire le READ UPDATE et le REWRITE dans le meme paragraphe, juste avant la mise a jour effective :

```
Passage 1 (RECHERCHE) : READ simple -> Affichage
                        (pas de verrouillage car fin de tache)

Passage 2 (VALIDATION) : READ UPDATE -> Modifications -> REWRITE
                         (atomique, meme UOW)
```

### Definition CICS

```
CEDA DEFINE PROGRAM(PRGMAJ) GROUP(CLIGROUP) LANGUAGE(COBOL)
CEDA INSTALL PROGRAM(PRGMAJ) GROUP(CLIGROUP)
```

### Verification

```
CEMT INQ PROGRAM(PRGMAJ)
```

Resultat attendu : `Prog(PRGMAJ) Cob Ena`

### Captures d'ecran

<!-- ![pt2ex10-1](images-pt2/pt2ex10-1.png) -->

---

## Exercice 11 : Transaction de mise a jour

### Enonce

Definir une transaction independante de la precedente pour appeler le programme de mise a jour.

### Mon travail

La transaction MAJO est le point d'entree utilisateur pour la mise a jour. Elle associe :
- Le code transaction `MAJO` (saisi par l'utilisateur)
- Le programme `PRGMAJ` (execute automatiquement)

### Resolution

**Definition de la transaction :**

```
CEDA DEFINE TRANSACTION(MAJO) GROUP(CLIGROUP) PROGRAM(PRGMAJ)
```

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| TRANSACTION | MAJO | Code transaction (4 caracteres max) |
| GROUP | CLIGROUP | Groupe de ressources du projet |
| PROGRAM | PRGMAJ | Programme COBOL a executer |

**Installation du groupe :**

```
CEDA INSTALL GROUP(CLIGROUP)
```

### Verification

```
CEDA VIEW TRANSACTION(MAJO) GROUP(CLIGROUP)
CEMT INQ PROGRAM(PRGMAJ)
```

### Test

```
MAJO
```

Comportement attendu :
1. Ecran de saisie du numero de compte
2. Saisir un numero existant (ex: 100001)
3. Affichage des donnees du client (NUMCPT protege)
4. Modifier les champs souhaites
5. ENTER pour valider -> Message "MISE A JOUR EFFECTUEE"

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
