# Exercices CICS - Chapitre V

## Thème : Couche de Présentation (BMS, CEDF, CEDA)

Ce TP met en pratique la création d'écrans BMS et l'utilisation des outils de débogage et d'installation CICS.

## Objectifs

- Créer un MAPSET BMS avec différents types de champs
- Utiliser CEDF pour déboguer un programme CICS
- Installer une application avec les commandes CEDA
- Comprendre le cycle SEND MAP / RECEIVE MAP

## Fichiers fournis

```
chapitre-05/
├── bms/
│   └── MAPTEST.bms       # Exemple de Mapset simple
├── jcl/
│   ├── ASSBLMAP.jcl      # Assemblage MAP BMS
│   └── COMPPGR.jcl       # Compilation programme CICS
└── README.md
```

**Fichiers à créer (exercices)** :
- `bms/WELCOMES.bms` : Mapset d'accueil complet (Exercice 1)
- `cobol/PROGWELC.cbl` : Programme COBOL avec SEND/RECEIVE MAP (Exercice 2)

---

## Exercice 1 : Création d'une MAP d'accueil

### Objectif

Créer un écran BMS simple affichant un message de bienvenue.

### Spécifications

```
┌─────────────────────────────────────────────────────────────────────────┐
│  Ligne 1  : ═══════════════════════════════════════════════════════════ │
│  Ligne 2  :               BIENVENUE DANS L'APPLICATION CICS             │
│  Ligne 3  : ═══════════════════════════════════════════════════════════ │
│                                                                          │
│  Ligne 10 : Code Utilisateur : [________]                               │
│                                                                          │
│  Ligne 12 : ENTER = Valider    PF3 = Quitter                            │
│                                                                          │
│  Ligne 24 : Message : [                                                ]│
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Instructions

1. Créer le fichier `bms/WELCOMES.bms` avec :
   - MAPSET nommé `WELCOMES`
   - MAP nommée `WELCMAP`
   - Attributs : `MODE=INOUT`, `LANG=COBOL`, `TIOAPFX=YES`

2. Définir les champs :
   - Titre (ASKIP, BRT) en ligne 2
   - Zone de saisie `USERID` (UNPROT, IC) en ligne 10
   - Zone message `MSGOUT` (ASKIP) en ligne 24

### Solution BMS

```asm
***********************************************************************
*  MAPSET : WELCOMES - Ecran d'accueil
***********************************************************************
WELCOMES DFHMSD TYPE=&SYSPARM,                                         X
               LANG=COBOL,                                             X
               MODE=INOUT,                                             X
               TIOAPFX=YES,                                            X
               STORAGE=AUTO
*
WELCMAP  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
*─────────────────────── EN-TETE ─────────────────────────────────────
         DFHMDF POS=(1,1),LENGTH=79,ATTRB=(ASKIP,BRT),                 X
               INITIAL='═══════════════════════════════════════════════X
═══════════════════════════════════'
*
         DFHMDF POS=(2,15),LENGTH=50,ATTRB=(ASKIP,BRT),                X
               INITIAL='BIENVENUE DANS L''APPLICATION CICS'
*
         DFHMDF POS=(3,1),LENGTH=79,ATTRB=(ASKIP,BRT),                 X
               INITIAL='═══════════════════════════════════════════════X
═══════════════════════════════════'
*
*─────────────────────── SAISIE ──────────────────────────────────────
         DFHMDF POS=(10,10),LENGTH=18,ATTRB=ASKIP,                     X
               INITIAL='Code Utilisateur :'
*
USERID   DFHMDF POS=(10,30),LENGTH=8,ATTRB=(UNPROT,BRT,IC)
*
         DFHMDF POS=(10,39),LENGTH=1,ATTRB=ASKIP
*
*─────────────────────── AIDE ────────────────────────────────────────
         DFHMDF POS=(12,10),LENGTH=40,ATTRB=ASKIP,                     X
               INITIAL='ENTER = Valider    PF3 = Quitter'
*
*─────────────────────── MESSAGE ─────────────────────────────────────
         DFHMDF POS=(24,1),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='Message : '
*
MSGOUT   DFHMDF POS=(24,12),LENGTH=60,ATTRB=(ASKIP,BRT)
*
WELCOMES DFHMSD TYPE=FINAL
         END
```

---

## Exercice 2 : Programme COBOL avec SEND/RECEIVE MAP

### Objectif

Créer un programme COBOL qui :
1. Affiche la MAP d'accueil
2. Reçoit la saisie de l'utilisateur
3. Affiche un message personnalisé

### Instructions

1. Créer le fichier `cobol/PROGWELC.cbl`
2. Utiliser `COPY WELCOMES` pour la zone symbolique
3. Implémenter :
   - `EXEC CICS SEND MAP` pour affichage initial
   - `EXEC CICS RECEIVE MAP` pour récupérer la saisie
   - `EXEC CICS SEND MAP DATAONLY` pour message

### Solution COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGWELC.
      *
      * Programme : PROGWELC - Ecran d'accueil
      * Transaction : WELC
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.

       COPY WELCOMES.

       01  WS-MSG                  PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.

      *    Affichage initial de la MAP
           EXEC CICS SEND MAP('WELCMAP')
               MAPSET('WELCOMES')
               MAPONLY
               ERASE
           END-EXEC.

      *    Reception de la saisie
           EXEC CICS RECEIVE MAP('WELCMAP')
               MAPSET('WELCOMES')
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               IF USERIDI NOT = SPACES
                   STRING 'Bienvenue ' DELIMITED SIZE
                          USERIDI DELIMITED SPACE
                          ' !' DELIMITED SIZE
                       INTO MSGOUTO
                   END-STRING
               ELSE
                   MOVE 'Veuillez saisir un code utilisateur'
                       TO MSGOUTO
               END-IF
           ELSE
               MOVE 'Erreur de saisie' TO MSGOUTO
           END-IF.

      *    Affichage du message
           EXEC CICS SEND MAP('WELCMAP')
               MAPSET('WELCOMES')
               DATAONLY
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.
```

---

## Exercice 3 : Utilisation de CEDF

### Objectif

Apprendre à utiliser CEDF pour déboguer un programme CICS.

### Instructions

1. **Activer le mode debug** :
   ```
   CEDF
   ```
   Message : "THIS TERMINAL: EDF MODE ON"

2. **Lancer la transaction** :
   ```
   WELC
   ```

3. **Observer les points d'arrêt** :
   - `PROGRAM INITIATION` : démarrage du programme
   - `ABOUT TO EXECUTE COMMAND` : avant SEND MAP
   - `COMMAND EXECUTION COMPLETE` : après SEND MAP

4. **Utiliser les touches PF** :
   - `PF4` : Afficher l'EIB
   - `PF5` : Voir la WORKING-STORAGE
   - `PF9` : Définir des conditions d'arrêt

5. **Terminer le debug** :
   ```
   CEDF OFF
   ```

### Questions à répondre

1. Quelle est la valeur de `EIBTRNID` au démarrage ?
2. Quelle est la valeur de `EIBRESP` après le SEND MAP ?
3. Quelle touche a été pressée (voir `EIBAID`) ?

---

## Exercice 4 : Installation avec CEDA

### Objectif

Installer l'application dans CICS avec les commandes CEDA.

### Instructions

1. **Définir le MAPSET** :
   ```
   CEDA DEF MAPSET(WELCOMES) GROUP(TESTGRP)
   ```

2. **Installer le MAPSET** :
   ```
   CEDA INS MAPSET(WELCOMES) GROUP(TESTGRP)
   ```

3. **Définir le PROGRAMME** :
   ```
   CEDA DEF PROGRAM(PROGWELC) GROUP(TESTGRP) LANGUAGE(COBOL)
   ```

4. **Installer le PROGRAMME** :
   ```
   CEDA INS PROGRAM(PROGWELC) GROUP(TESTGRP)
   ```

5. **Définir la TRANSACTION** :
   ```
   CEDA DEF TRANSACTION(WELC) GROUP(TESTGRP) PROGRAM(PROGWELC)
   ```

6. **Installer la TRANSACTION** :
   ```
   CEDA INS TRANSACTION(WELC) GROUP(TESTGRP)
   ```

7. **Vérifier le groupe** :
   ```
   CEDA DISPLAY GROUP(TESTGRP)
   ```

### Résultat attendu

```
STATUS:  RESULTS - OVERTYPE TO MODIFY
 GRoup(TESTGRP)

 NAME       TYPE
 WELCOMES   MAPSet
 PROGWELC   PROGram
 WELC       TRAnsaction
```

---

## Exercice 5 (Avancé) : Gestion des touches PF

### Objectif

Modifier le programme pour gérer PF3 (quitter).

### Instructions

1. Après le RECEIVE MAP, tester la touche appuyée :
   ```cobol
           IF EIBAID = DFHPF3
               EXEC CICS SEND TEXT
                   FROM('Fin de session')
                   ERASE
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.
   ```

2. Ajouter le COPY `DFHAID` pour les constantes de touches

### Code à ajouter

```cobol
       WORKING-STORAGE SECTION.
       COPY DFHAID.
       ...

       PROCEDURE DIVISION.
       ...
      *    Test touche PF3
           IF EIBAID = DFHPF3
               EXEC CICS SEND TEXT
                   FROM('Au revoir!')
                   LENGTH(10)
                   ERASE
               END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF.
```

---

## Codes retour à connaître

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Opération réussie |
| 26 | DFHRESP(MAPFAIL) | Aucune donnée saisie |
| 36 | DFHRESP(INVMPSZ) | Taille MAP invalide |

## Constantes EIBAID (touches)

| Constante | Touche |
|-----------|--------|
| DFHENTER | ENTER |
| DFHCLEAR | CLEAR |
| DFHPF1 à DFHPF24 | PF1 à PF24 |
| DFHPA1 à DFHPA3 | PA1 à PA3 |

---

## JCL de compilation CICS

### Assemblage d'une MAP BMS (ASSBLMAP.jcl)

```jcl
//ASSBLMAP JOB 'ASSEMBL',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='FTEST.CICS.LOAD',
//          DSCTLIB='FTEST.CICS.LKED',
//          MAPNAME='MAPTEST',RMODE=24
//SYSPRINT DD SYSOUT=A
//SYSUT1   DD DSN=FTEST.CICS.SOURCE(MAPTEST),DISP=SHR
```

**Paramètres clés** :
- `DFHMAPS` : Procédure d'assemblage des MAPs
- `MAPLIB` : Bibliothèque de sortie pour le load module
- `DSCTLIB` : Bibliothèque pour le DSECT (copybook)
- `MAPNAME` : Nom du MAPSET à assembler

### Compilation programme CICS COBOL (COMPPGR.jcl)

```jcl
//COMPPGR  JOB 'COMPPGR',
//          CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//          NOTIFY=&SYSUID
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,FTEST.CICS.SOURCE,
//          FTEST.CICS.LKED,FTEST.CICS.LOAD)
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='FTEST.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='FTEST.CICS.LKED',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(PROGWRIT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PROGWRIT(R)
/*
```

**Paramètres clés** :
- `DFHYITVL` : Procédure intégrée (Translate + Compile + Link)
- `PROGLIB` : Bibliothèque de sortie pour le programme
- `AD370HLQ` : High-Level Qualifier du compilateur COBOL
- `DFHELII` : Module d'interface CICS-COBOL

### Exemple de MAP simple (MAPTEST.bms)

```asm
MAPTEST  DFHMSD TYPE=MAP,                                              X
               MODE=INOUT,                                             X
               LANG=COBOL,                                             X
               STORAGE=AUTO,                                           X
               TIOAPFX=YES
MAP1     DFHMDI SIZE=(24,80),                                          X
               LINE=01,                                                X
               COLUMN=01,                                              X
               CTRL=(PRINT,FREEKB)
         DFHMDF POS=(9,23),                                            X
               ATTRB=(ASKIP,NORM),                                     X
               LENGTH=38,                                              X
               INITIAL='WELCOME TO THE MAGICAL WORLD IN CICS'
         DFHMDF POS=(12,27),                                           X
               ATTRB=(ASKIP,NORM),                                     X
               LENGTH=26,                                              X
               INITIAL='MAY THE FORCE BE WITH YOU'
MAPTEST  DFHMSD TYPE=FINAL
         END
```

**Résultat à l'écran** :
```
┌────────────────────────────────────────────────────────────────────────────────┐
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                       WELCOME TO THE MAGICAL WORLD IN CICS                     │
│                                                                                │
│                                                                                │
│                           MAY THE FORCE BE WITH YOU                            │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
│                                                                                │
└────────────────────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [← Retour exercices CICS](../README.md) | [Chapitre VI - Couche Traitement](../chapitre-06/) |

---
*Formation COBOL - Module CICS - TP Chapitre V*
