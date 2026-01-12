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
| ... | ... |

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
- CLIAFF : Affichage client
- CLIAJT : Ajout client
- CLIMAJ : Mise a jour client
- CLISUP : Suppression client
- CLISUL : Suppression avec lecture
- CLISDEL : Suppression generique
- CLILGEN : Liste generique
- CLISTAT : Statistiques region

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
- `RECORDSIZE(64 64)` : Enregistrements de taille fixe (64 octets)
- `FREESPACE(20 10)` : Reserve de l'espace pour les insertions futures
- `SHAREOPTIONS(2 3)` : Permet le partage entre regions CICS

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
         RECORDSIZE(64 64)                            -
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
     RECORDSIZE(64)
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

```jcl
//ROCHA02 JOB (ACCT),'LOAD VSAM CLIENT',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
0000010120DUPONT    JEAN      19850315M10CPARIS     0000150000CR
0000020125MARTIN    MARIE     19900622F15MPARIS     0000080000DB
0000030220BERNARD   PIERRE    19780410M20CMARSEILLE0000250000CR
0000040225PETIT     SOPHIE    19880912F05MMARSEILLE0000045000DB
0000050330ROBERT    ALAIN     19750520M10CLYON      0000320000CR
0000060335RICHARD   CLAIRE    19920805F25VLYON      0000012000DB
0000070420DURAND    PAUL      19820718M30DLILLE     0000180000CR
0000080425MOREAU    ANNE      19950303F15CLILLE     0000095000DB
0000090130LAURENT   MARC      19800125M20MPARIS     0000420000CR
0000100235SIMON     JULIE     19870930F10CMARSEILLE0000067000DB
2220010120LEROY     MICHEL    19830214M05CPARIS     0000145000CR
2220020125ROUX      NATHALIE  19910607F15MMARSEILLE0000032000DB
2220030230DAVID     FRANCOIS  19760819M20CLYON      0000278000CR
2220040335BERTRAND  ISABELLE  19890423F25VLILLE     0000089000DB
2220050420MOREL     PHILIPPE  19840111M30DPARIS     0000156000CR
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) OUTDATASET(ROCHA.CICS.CLIENT)
/*
```

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

<!-- ![pt1ex01-1](images-pt1/pt1ex01-1.png) -->
<!-- ![pt1ex01-2](images-pt1/pt1ex01-2.png) -->
<!-- ![pt1ex01-3](images-pt1/pt1ex01-3.png) -->
<!-- ![pt1ex01-4](images-pt1/pt1ex01-4.png) -->
<!-- ![pt1ex01-5](images-pt1/pt1ex01-5.png) -->

---

## Exercice 2 : Creation de la MAP BMS pour affichage

### Enonce

Creer la MAP conformement a la structure du Data Set CLIENT permettant l'affichage des nouvelles donnees. Prevoir dans ce cadre le controle des donnees redondantes et une zone de message de 40 caracteres pour afficher les informations necessaires en cas d'erreur ou de saisie correcte.

### Mon travail

J'ai cree une MAP BMS avec tous les champs du fichier CLIENT. La MAP comprend :
- Un titre en haut de l'ecran
- Les 12 champs de saisie/affichage avec leurs libelles
- Une zone de message de 40 caracteres en bas
- Les attributs DSATTS et MAPATTS pour gerer les couleurs et attributs dynamiquement

### Resolution

**MAP BMS : CLIAFF.bms**

```
CLIAFF   DFHMSD TYPE=&SYSPARM,                                         X
               MODE=INOUT,                                             X
               LANG=COBOL,                                             X
               STORAGE=AUTO,                                           X
               CTRL=FREEKB,                                            X
               TIOAPFX=YES

MAPAFFI  DFHMDI SIZE=(24,80),                                          X
               LINE=1,                                                 X
               COLUMN=1

         DFHMDF POS=(1,25),LENGTH=30,                                  X
               ATTRB=(ASKIP,BRT),                                      X
               INITIAL='*** AFFICHAGE CLIENT ***'

* Numero de compte (cle)
         DFHMDF POS=(3,2),LENGTH=15,                                   X
               ATTRB=(ASKIP),                                          X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(3,18),LENGTH=6,                                   X
               ATTRB=(UNPROT,NUM,IC),                                  X
               DSATTS=(COLOR,HILIGHT),                                 X
               COLOR=GREEN

* Code region
         DFHMDF POS=(4,2),LENGTH=15,                                   X
               ATTRB=(ASKIP),                                          X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(4,18),LENGTH=2,                                   X
               ATTRB=(ASKIP),                                          X
               DSATTS=(COLOR),                                         X
               COLOR=TURQUOISE

* ... autres champs ...

* Zone message
         DFHMDF POS=(22,2),LENGTH=8,                                   X
               ATTRB=(ASKIP),                                          X
               INITIAL='MESSAGE:'
MSG      DFHMDF POS=(22,11),LENGTH=40,                                 X
               ATTRB=(ASKIP,BRT),                                      X
               DSATTS=(COLOR),                                         X
               COLOR=YELLOW

         DFHMSD TYPE=FINAL
         END
```

### Captures d'ecran

<!-- ![pt1ex02-1](images-pt1/pt1ex02-1.png) -->

---

## Exercice 3 : Programme COBOL-CICS d'affichage

### Enonce

Creer le PROGRAMME necessaire pour l'affichage des donnees pour un code CLIENT saisi. Il doit permettre une saisie multiple de code CLIENT jusqu'a fin de saisie d'affichage de la part de l'utilisateur. De meme, il faut accompagner chaque anomalie ou action par un message d'information ou d'avertissement.

### Mon travail

J'ai developpe un programme COBOL-CICS qui :
1. Envoie la MAP vide au premier appel
2. Recoit le numero de compte saisi par l'utilisateur
3. Lit l'enregistrement VSAM avec la commande READ
4. Affiche les donnees ou un message d'erreur
5. Permet de continuer ou quitter (O/N)

**Gestion pseudo-conversationnelle** : Le programme utilise RETURN TRANSID pour revenir au debut apres chaque interaction.

### Resolution

**Programme : CLIAFF.cbl**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIAFF.
      ******************************************************************
      * Programme d'affichage d'un client
      * Transaction : AFFI
      ******************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-COMMAREA.
          05 WS-FLAG-PREMIER     PIC X(01) VALUE 'O'.

      * Copybook genere par assemblage BMS (TYPE=DSECT)
       COPY CLIAFF.

      * Structure enregistrement CLIENT (64 octets)
       01 ENR-CLIENT.
          05 CLI-NUMCPT           PIC 9(06).
          05 CLI-CODREG           PIC 9(02).
          05 CLI-NATCPT           PIC 9(02).
          05 CLI-NOM              PIC X(10).
          05 CLI-PRENOM           PIC X(10).
          05 CLI-DATNAISS         PIC 9(08).
          05 CLI-SEXE             PIC X(01).
          05 CLI-ACTPRO           PIC 9(02).
          05 CLI-SITSO            PIC X(01).
          05 CLI-ADRESSE          PIC X(10).
          05 CLI-SOLDE            PIC 9(10).
          05 CLI-POSITION         PIC X(02).

       01 WS-RESP                PIC S9(08) COMP.
       01 WS-MESSAGE             PIC X(40).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           IF EIBCALEN = 0
               PERFORM 1000-PREMIER-PASSAGE
           ELSE
               PERFORM 2000-TRAITEMENT
           END-IF
           EXEC CICS RETURN
               TRANSID('AFFI')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-PREMIER-PASSAGE.
           MOVE LOW-VALUES TO MAPAFFII
           MOVE 'SAISIR LE NUMERO DE COMPTE' TO MSGO
           EXEC CICS SEND MAP('MAPAFFI')
               MAPSET('CLIAFF')
               ERASE
           END-EXEC.

       2000-TRAITEMENT.
           EXEC CICS RECEIVE MAP('MAPAFFI')
               MAPSET('CLIAFF')
           END-EXEC

           MOVE NUMCPTI TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 3000-AFFICHER-CLIENT
           ELSE IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CLIENT INEXISTANT' TO MSGO
           ELSE
               MOVE 'ERREUR LECTURE FICHIER' TO MSGO
           END-IF

           EXEC CICS SEND MAP('MAPAFFI')
               MAPSET('CLIAFF')
           END-EXEC.

       3000-AFFICHER-CLIENT.
           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NATCPT   TO NATCPTO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAISO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-ACTPRO   TO ACTPROO
           MOVE CLI-SITSO    TO SITSOO
           MOVE CLI-ADRESSE  TO ADRESSEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITIONO
           MOVE 'CLIENT TROUVE - CONTINUER (O/N) ?' TO MSGO.
```

### Captures d'ecran

<!-- ![pt1ex03-1](images-pt1/pt1ex03-1.png) -->

---

## Exercice 4 : Creation de la transaction via CEDA

### Enonce

Creer la transaction correspondante a l'operation d'affichage des donnees de CLIENT avec l'interface CICS en utilisant la commande CEDA. Mettre eventuellement le GROUP et la LIST a jour en cas de besoin.

### Mon travail

J'ai utilise CEDA pour definir la transaction AFFI qui appelle le programme CLIAFF.

### Resolution

**Commandes CEDA :**

```
CEDA DEFINE TRANSACTION(AFFI) GROUP(CLIGROUP)
     PROGRAM(CLIAFF)

CEDA DEFINE PROGRAM(CLIAFF) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA INSTALL GROUP(CLIGROUP)
```

**Verification :**

```
CEMT INQUIRE TRANSACTION(AFFI)
CEMT INQUIRE PROGRAM(CLIAFF)
```

### Captures d'ecran

<!-- ![pt1ex04-1](images-pt1/pt1ex04-1.png) -->

---

## Exercice 5 : Test avec debugger CEDF

### Enonce

Activer la transaction en mode debugger avec la commande CEDF et par suite sans debugger.

### Mon travail

J'ai teste la transaction en mode debug pour verifier le bon fonctionnement :
1. Activation CEDF depuis le terminal
2. Execution de la transaction AFFI
3. Verification des commandes CICS etape par etape
4. Test sans debugger pour valider le fonctionnement normal

### Resolution

**Commandes de test :**

```
CEDF              (activation du debugger)
AFFI              (execution de la transaction)
CEDF OFF          (desactivation du debugger)
```

### Captures d'ecran

<!-- ![pt1ex05-1](images-pt1/pt1ex05-1.png) -->

---

# Partie 2 : Operations CRUD

## Exercice 6 : MAP pour ajout de client

### Enonce

Creer ou adapter la MAP precedente pour une operation d'ajout de CLIENT dans le Data Set CLIENT.

### Mon travail

J'ai adapte la MAP d'affichage pour permettre la saisie de tous les champs. Les champs qui etaient en ASKIP (affichage seul) sont maintenant en UNPROT (saisissables).

### Resolution

**MAP BMS : CLIAJT.bms**

```
* Tous les champs sont maintenant saisissables (UNPROT)
* Le numero de compte reste le premier champ avec IC (Initial Cursor)

NUMCPT   DFHMDF POS=(3,18),LENGTH=6,                                   X
               ATTRB=(UNPROT,NUM,IC),                                  X
               DSATTS=(COLOR,HILIGHT),                                 X
               COLOR=GREEN

CODREG   DFHMDF POS=(4,18),LENGTH=2,                                   X
               ATTRB=(UNPROT,NUM),                                     X
               DSATTS=(COLOR),                                         X
               COLOR=GREEN

* ... tous les autres champs en UNPROT ...
```

### Captures d'ecran

<!-- ![pt2ex06-1](images-pt2/pt2ex06-1.png) -->

---

## Exercice 7 : Programme d'ajout (WRITE)

### Enonce

Creer le PROGRAMME pour une operation d'ajout d'un nouveau CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et de doublure doit etre effectue.

### Mon travail

Le programme d'ajout effectue les controles suivants avant l'ecriture :
1. Verification que le numero de compte est numerique (6 chiffres)
2. Verification que le client n'existe pas deja (pas de doublure)
3. Validation du sexe (M ou F)
4. Validation de la situation sociale (C, M, D, V)
5. Validation de la position (DB ou CR)

En cas d'erreur, un message explicite est affiche.

### Resolution

**Programme : CLIAJT.cbl**

```cobol
       2000-TRAITEMENT.
           EXEC CICS RECEIVE MAP('MAPAJT')
               MAPSET('CLIAJT')
           END-EXEC

      * Controles de conformite
           PERFORM 2100-VALIDER-DONNEES
           IF WS-ERREUR = 'O'
               GO TO 2000-FIN
           END-IF

      * Verification doublure
           MOVE NUMCPTI TO CLI-NUMCPT
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT EN DOUBLE' TO MSGO
               GO TO 2000-FIN
           END-IF

      * Ecriture du nouveau client
           PERFORM 2200-PREPARER-ENREG
           EXEC CICS WRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'SAISIE CORRECTE, CONTINUER (O/N) ?' TO MSGO
           ELSE
               MOVE 'ERREUR ECRITURE FICHIER' TO MSGO
           END-IF.

       2000-FIN.
           EXIT.

       2100-VALIDER-DONNEES.
           MOVE 'N' TO WS-ERREUR

      * Controle numero de compte numerique
           IF NUMCPTI NOT NUMERIC
               MOVE 'ZONE NUMERIQUE, RESAISIR CE CHAMP' TO MSGO
               MOVE 'O' TO WS-ERREUR
               EXIT PARAGRAPH
           END-IF

      * Controle sexe
           IF SEXEI NOT = 'M' AND SEXEI NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               EXIT PARAGRAPH
           END-IF

      * Controle situation sociale
           IF SITSOI NOT = 'C' AND SITSOI NOT = 'M'
              AND SITSOI NOT = 'D' AND SITSOI NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
           END-IF.
```

### Captures d'ecran

<!-- ![pt2ex07-1](images-pt2/pt2ex07-1.png) -->

---

## Exercice 8 : Transaction d'ajout

### Enonce

Suivre cette operation par l'ajout d'une nouvelle Transaction dans le GROUP et activer la transaction en mode debugger CEDF et sans debugger.

### Resolution

```
CEDA DEFINE TRANSACTION(AJOU) GROUP(CLIGROUP)
     PROGRAM(CLIAJT)

CEDA DEFINE PROGRAM(CLIAJT) GROUP(CLIGROUP)
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

**Programme : CLIMAJ.cbl**

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
     PROGRAM(CLIMAJ)

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

**Programme : CLISUP.cbl**

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
     PROGRAM(CLISUP)

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

**Programme : CLISUL.cbl** (Suppression avec Lecture)

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
     PROGRAM(CLISUL)
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

**Programme : CLISDEL.cbl** (Suppression Generique)

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

**Programme : CLILGEN.cbl** (Liste Generique)

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

**Programme : CLISTAT.cbl**

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
     PROGRAM(CLISTAT)
```

### Captures d'ecran

<!-- ![pt3ex19-1](images-pt3/pt3ex19-1.png) -->

---

# Annexes

## Liste des programmes COBOL-CICS

| Programme | Transaction | Description |
|-----------|-------------|-------------|
| CLIAFF | AFFI | Affichage d'un client |
| CLIAJT | AJOU | Ajout d'un nouveau client |
| CLIMAJ | MAJO | Mise a jour d'un client |
| CLISUP | SUPP | Suppression d'un client |
| CLISUL | SULE | Suppression avec lecture prealable |
| CLISDEL | SDEL | Suppression par code generique |
| CLILGEN | LGEN | Liste par code generique (READNEXT) |
| CLISTAT | STAT | Statistiques par region |

## Liste des MAPs BMS

| Mapset | Map | Description |
|--------|-----|-------------|
| CLIAFF | MAPAFFI | Ecran d'affichage client |
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
