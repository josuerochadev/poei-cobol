# Chapitre X - Traitement des Fichiers COBOL

Ce chapitre consolide les concepts de gestion de fichiers et approfondit les aspects mainframe (VSAM, JCL, clauses avancées).

---

## X-1 Types de Data Sets (rappel)

### Organisations VSAM ↔ COBOL

| Type VSAM | Organisation COBOL | Caractéristique |
|-----------|-------------------|-----------------|
| **ESDS** (Entry-Sequenced) | SEQUENTIAL | Accès séquentiel uniquement |
| **KSDS** (Key-Sequenced) | INDEXED | Accès par clé primaire/secondaire |
| **RRDS** (Relative Record) | RELATIVE | Accès par numéro de position |

### Comparatif des organisations

| Critère | SEQUENTIAL | INDEXED | RELATIVE |
|---------|------------|---------|----------|
| Clé | Aucune | Primaire + secondaires | Position (RRN) |
| Accès direct | ❌ | ✅ | ✅ |
| Modification | ❌ (sauf I-O même taille) | ✅ REWRITE | ✅ REWRITE |
| Suppression | ❌ | ✅ DELETE | ✅ DELETE |
| Ajout | EXTEND (fin) | Partout | À position spécifique |
| Usage typique | Logs, batch, fichiers plats | Fichiers maîtres | Tables de référence |

---

## X-2 FILE-CONTROL : Clauses de déclaration

### Syntaxe complète

```cobol
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT [OPTIONAL] nom-logique
               ASSIGN TO nom-externe
               [ORGANIZATION IS {SEQUENTIAL | INDEXED | RELATIVE}]
               [ACCESS MODE IS {SEQUENTIAL | RANDOM | DYNAMIC}]
               [RECORD KEY IS nom-clé]
               [ALTERNATE RECORD KEY IS nom-clé-alt [WITH DUPLICATES]]
               [RELATIVE KEY IS nom-rrn]
               [FILE STATUS IS variable-status]
               [RESERVE entier AREA[S]]
               [PASSWORD IS variable-password].
```

### Clause SELECT

Associe un nom logique interne au programme.

```cobol
       SELECT FICHIER-CLIENTS
       SELECT F-PRODUITS
       SELECT PERSONNEL
```

---

### Clause OPTIONAL

Indique qu'un fichier peut ne pas exister à l'ouverture.

```cobol
       SELECT OPTIONAL FICHIER-HISTORIQUE
           ASSIGN TO 'HISTO.DAT'
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
```

**Comportement :**

| Mode OPEN | Fichier existe | Fichier n'existe pas |
|-----------|----------------|----------------------|
| INPUT | Lecture normale | FILE STATUS = 05, fichier vide |
| I-O | Lecture/écriture | FILE STATUS = 05, fichier créé vide |
| OUTPUT | Écrase ou crée | Crée le fichier |
| EXTEND | Ajoute à la fin | FILE STATUS = 05, fichier créé |

**Sans OPTIONAL :** FILE STATUS = 35 (fichier non trouvé) si le fichier n'existe pas en INPUT ou I-O.

**Usage typique :** Fichiers de paramétrage optionnels, logs qui peuvent ne pas exister.

---

### Clause ASSIGN TO

#### Format GnuCOBOL (PC)

```cobol
       ASSIGN TO 'CLIENTS.DAT'
       ASSIGN TO WS-NOM-FICHIER          *> Variable
       ASSIGN TO '/chemin/complet/fichier.dat'
```

#### Format Mainframe (z/OS)

Sur mainframe, ASSIGN TO fait référence au **DDNAME** dans le JCL.

```cobol
      * Référence au DDNAME dans le JCL
       ASSIGN TO DDCLIENTS
       ASSIGN TO DDPROD
```

**Correspondance JCL :**

```jcl
//MONPROG  EXEC PGM=MONCOBOL
//DDCLIENTS DD DSN=PROD.FICHIER.CLIENTS,DISP=SHR
//DDPROD    DD DSN=PROD.FICHIER.PRODUITS,DISP=SHR
```

#### Préfixes d'assignation (mainframe historique)

| Préfixe | Type de périphérique | Exemple |
|---------|---------------------|---------|
| `UT-S-` | Bande magnétique (séquentiel) | `ASSIGN TO UT-S-TAPES` |
| `UT-AS-` | Disque (accès séquentiel) | `ASSIGN TO UT-AS-DISKS` |
| `DA-S-` | Disque accès direct | `ASSIGN TO DA-S-MASTER` |

**Note :** Ces préfixes sont obsolètes sur z/OS moderne mais peuvent apparaître dans du code legacy.

---

### Clause ORGANIZATION

```cobol
       ORGANIZATION IS SEQUENTIAL    *> Par défaut si omis
       ORGANIZATION IS INDEXED
       ORGANIZATION IS RELATIVE
```

---

### Clause ACCESS MODE

| Mode | Description | Fichiers compatibles |
|------|-------------|---------------------|
| `SEQUENTIAL` | Lecture/écriture dans l'ordre | Tous |
| `RANDOM` | Accès direct par clé/position | INDEXED, RELATIVE |
| `DYNAMIC` | Séquentiel + direct | INDEXED, RELATIVE |

```cobol
       ACCESS MODE IS SEQUENTIAL     *> Par défaut si omis
       ACCESS MODE IS RANDOM
       ACCESS MODE IS DYNAMIC
```

---

### Clause RECORD KEY (INDEXED)

Définit la clé primaire pour un fichier indexé.

```cobol
       RECORD KEY IS CLI-CODE
```

**Règles :**
- La clé doit être un champ de l'enregistrement (01 du FD)
- Doit être unique pour chaque enregistrement
- Ne peut pas être modifiée avec REWRITE

---

### Clause ALTERNATE RECORD KEY (INDEXED)

Définit une ou plusieurs clés secondaires.

```cobol
       ALTERNATE RECORD KEY IS CLI-NOM WITH DUPLICATES
       ALTERNATE RECORD KEY IS CLI-VILLE WITH DUPLICATES
       ALTERNATE RECORD KEY IS CLI-SIRET          *> Sans doublons
```

**WITH DUPLICATES :** Autorise plusieurs enregistrements avec la même valeur de clé secondaire.

---

### Clause RELATIVE KEY (RELATIVE)

Variable contenant le numéro d'enregistrement relatif (RRN).

```cobol
       RELATIVE KEY IS WS-RRN

       WORKING-STORAGE SECTION.
       01 WS-RRN   PIC 9(6).
```

---

### Clause FILE STATUS

Variable recevant le code retour après chaque opération E/S.

```cobol
       FILE STATUS IS WS-STATUS

       WORKING-STORAGE SECTION.
       01 WS-STATUS   PIC XX.
          88 WS-OK    VALUE '00'.
          88 WS-EOF   VALUE '10'.
```

---

### Clause RESERVE (Buffers E/S)

Spécifie le nombre de buffers mémoire pour les opérations E/S.

```cobol
       RESERVE 2 AREAS              *> 2 buffers
       RESERVE 5 AREAS              *> 5 buffers pour gros fichiers
```

**Impact :**
- Plus de buffers = meilleures performances en séquentiel
- Consomme plus de mémoire
- Optimisation pour fichiers volumineux

**Valeur par défaut :** Dépend du système (généralement 2).

---

### Clause PASSWORD (Sécurité fichier)

Protection par mot de passe (mainframe).

```cobol
       PASSWORD IS WS-MDP

       WORKING-STORAGE SECTION.
       01 WS-MDP   PIC X(8) VALUE 'SECRETPW'.
```

**Note :** Rarement utilisé aujourd'hui, remplacé par la sécurité système (RACF, ACF2).

---

## X-3 FD (File Description) : Clauses avancées

### Syntaxe complète FD

```cobol
       FILE SECTION.
       FD  nom-fichier
           [RECORDING MODE IS {F | V | U}]
           [RECORD CONTAINS entier CHARACTERS]
           [RECORD IS VARYING IN SIZE FROM min TO max CHARACTERS
               [DEPENDING ON variable]]
           [BLOCK CONTAINS entier {RECORDS | CHARACTERS}]
           [LABEL RECORD IS {STANDARD | OMITTED}]
           [DATA RECORD IS nom-enregistrement].
       01  nom-enregistrement.
           ...
```

---

### RECORDING MODE

Définit le format des enregistrements.

| Mode | Description | Usage |
|------|-------------|-------|
| `F` | Fixed - taille fixe | Fichiers standards |
| `V` | Variable - taille variable | Fichiers avec enr. de tailles différentes |
| `U` | Undefined - indéfini | Fichiers système, load modules |

```cobol
       FD  FICHIER-CLIENTS
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.

       FD  FICHIER-TEXTE
           RECORDING MODE IS V
           RECORD IS VARYING IN SIZE FROM 20 TO 200 CHARACTERS.
```

---

### RECORD CONTAINS / RECORD IS VARYING

#### Enregistrements de taille fixe

```cobol
       FD  F-CLIENTS
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS.
       01  ENR-CLIENT   PIC X(100).
```

#### Enregistrements de taille variable

```cobol
       FD  F-MESSAGES
           RECORDING MODE IS V
           RECORD IS VARYING IN SIZE FROM 50 TO 500 CHARACTERS
               DEPENDING ON WS-TAILLE-ENR.
       01  ENR-MESSAGE.
           05  MSG-TYPE     PIC X(2).
           05  MSG-CONTENU  PIC X(498).

       WORKING-STORAGE SECTION.
       01  WS-TAILLE-ENR   PIC 9(3).
```

---

### BLOCK CONTAINS (Mainframe)

Définit le facteur de blocage (nombre d'enregistrements par bloc physique).

```cobol
      * Blocage par nombre d'enregistrements
       FD  FICHIER-MASTER
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 50 RECORDS.         *> 50 enr/bloc = 5000 octets

      * Blocage par taille de bloc
       FD  FICHIER-MASTER
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 5000 CHARACTERS.    *> Bloc de 5000 octets
```

**Optimisation mainframe :**
- BLKSIZE = LRECL × nombre d'enregistrements par bloc
- Blocs plus grands = moins d'E/S = meilleures performances
- Typiquement : BLKSIZE proche de 27998 (demi-track)

---

### LABEL RECORD (Mainframe)

Indique la présence d'étiquettes standard.

```cobol
       LABEL RECORD IS STANDARD      *> Fichier disque/bande avec labels
       LABEL RECORDS ARE STANDARD    *> Forme plurielle

       LABEL RECORD IS OMITTED       *> Fichier sans labels (ex: impression)
```

**Note :** Sur z/OS moderne, cette clause est généralement ignorée (toujours STANDARD pour disque).

---

### DATA RECORD (Documentation)

Nomme l'enregistrement associé au fichier (clause documentaire).

```cobol
       FD  F-CLIENTS
           DATA RECORD IS ENR-CLIENT.
       01  ENR-CLIENT.
           05  CLI-CODE   PIC 9(4).
           05  CLI-NOM    PIC X(30).
```

---

## X-4 Accès par clé secondaire (AIX/PATH)

### Concept : Index Alternatif (AIX)

Sur mainframe VSAM, un **AIX** (Alternate Index) est un fichier séparé qui maintient un index sur une clé secondaire.

Un **PATH** combine le fichier de base et l'AIX pour permettre l'accès par la clé secondaire.

```
┌─────────────┐         ┌─────────────┐
│  KSDS Base  │◄───────►│     AIX     │
│  (CLI-CODE) │         │  (CLI-NOM)  │
└─────────────┘         └─────────────┘
       ▲                       ▲
       │                       │
       └───────┬───────────────┘
               │
        ┌──────▼──────┐
        │    PATH     │
        │ (Vue unifiée)│
        └─────────────┘
```

### Déclaration COBOL avec clé secondaire

```cobol
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-CLIENTS
               ASSIGN TO DDCLIENTS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CODE
               ALTERNATE RECORD KEY IS CLI-NOM WITH DUPLICATES
               FILE STATUS IS WS-STATUS.
```

### Lecture par clé secondaire

```cobol
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      * Lecture directe par clé secondaire
      *----------------------------------------------------------------*
           MOVE 'DUPONT' TO CLI-NOM
           READ FICHIER-CLIENTS KEY IS CLI-NOM
               INVALID KEY
                   DISPLAY 'Nom non trouve'
               NOT INVALID KEY
                   DISPLAY 'Trouve : ' CLI-CODE ' - ' CLI-NOM
           END-READ.

      *----------------------------------------------------------------*
      * Parcours séquentiel par clé secondaire
      *----------------------------------------------------------------*
           MOVE 'D' TO CLI-NOM
           START FICHIER-CLIENTS KEY >= CLI-NOM
               INVALID KEY DISPLAY 'Aucun nom >= D'
           END-START

           PERFORM UNTIL FIN-FICHIER
               READ FICHIER-CLIENTS NEXT
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END
                       DISPLAY CLI-NOM ' - ' CLI-CODE
               END-READ
           END-PERFORM.
```

### JCL pour AIX/PATH (mainframe)

```jcl
//DEFAIX   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE AIX (NAME(PROD.CLIENTS.AIX.NOM) -
              RELATE(PROD.CLIENTS.KSDS) -
              KEYS(30 4) -
              NONUNIQUEKEY -
              UPGRADE)
  DEFINE PATH (NAME(PROD.CLIENTS.PATH.NOM) -
               PATHENTRY(PROD.CLIENTS.AIX.NOM))
  BLDINDEX INDATASET(PROD.CLIENTS.KSDS) -
           OUTDATASET(PROD.CLIENTS.AIX.NOM)
/*
```

---

## X-5 Modes d'ouverture et opérations

### Récapitulatif des modes OPEN

| Mode | Lecture | Écriture | Usage |
|------|---------|----------|-------|
| `INPUT` | ✅ | ❌ | Consultation |
| `OUTPUT` | ❌ | ✅ (crée/écrase) | Création |
| `I-O` | ✅ | ✅ | Mise à jour |
| `EXTEND` | ❌ | ✅ (fin) | Ajout |

### Compatibilité Mode × Organisation × Opération

| Organisation | OPEN | READ | WRITE | REWRITE | DELETE | START |
|--------------|------|------|-------|---------|--------|-------|
| SEQUENTIAL | INPUT | ✅ | ❌ | ❌ | ❌ | ❌ |
| SEQUENTIAL | OUTPUT | ❌ | ✅ | ❌ | ❌ | ❌ |
| SEQUENTIAL | I-O | ✅ | ❌ | ✅* | ❌ | ❌ |
| SEQUENTIAL | EXTEND | ❌ | ✅ | ❌ | ❌ | ❌ |
| INDEXED | INPUT | ✅ | ❌ | ❌ | ❌ | ✅ |
| INDEXED | OUTPUT | ❌ | ✅ | ❌ | ❌ | ❌ |
| INDEXED | I-O | ✅ | ✅ | ✅ | ✅ | ✅ |
| RELATIVE | INPUT | ✅ | ❌ | ❌ | ❌ | ✅ |
| RELATIVE | OUTPUT | ❌ | ✅ | ❌ | ❌ | ❌ |
| RELATIVE | I-O | ✅ | ✅ | ✅ | ✅ | ✅ |

*\* SEQUENTIAL en I-O : REWRITE possible mais taille enregistrement identique requise*

---

## X-6 Opérations E/S (Synthèse)

### OPEN

```cobol
       OPEN INPUT FICHIER-ENTREE
       OPEN OUTPUT FICHIER-SORTIE
       OPEN I-O FICHIER-MAJ
       OPEN EXTEND FICHIER-LOG

      * Ouverture multiple
       OPEN INPUT  FICHIER-1
            OUTPUT FICHIER-2
            I-O    FICHIER-3
```

### READ

```cobol
      * Séquentiel
       READ FICHIER AT END SET FIN TO TRUE END-READ

      * Direct par clé
       MOVE '0001' TO CLI-CODE
       READ FICHIER KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
       END-READ

      * Séquentiel suivant (DYNAMIC)
       READ FICHIER NEXT AT END SET FIN TO TRUE END-READ

      * Avec INTO
       READ FICHIER INTO WS-BUFFER
           AT END SET FIN TO TRUE
       END-READ
```

### WRITE

```cobol
      * Écriture simple
       WRITE ENR-CLIENT
           INVALID KEY DISPLAY 'Erreur'
       END-WRITE

      * Avec FROM
       WRITE ENR-CLIENT FROM WS-CLIENT
           INVALID KEY DISPLAY 'Erreur'
       END-WRITE
```

### REWRITE

```cobol
      * TOUJOURS précédé d'un READ
       READ FICHIER KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
           NOT INVALID KEY
               MOVE 'NOUVEAU' TO CLI-NOM
               REWRITE ENR-CLIENT
                   INVALID KEY DISPLAY 'Erreur modification'
               END-REWRITE
       END-READ
```

### DELETE

```cobol
      * Mode RANDOM : pas de READ préalable
       MOVE '0003' TO CLI-CODE
       DELETE FICHIER
           INVALID KEY DISPLAY 'Non trouve'
       END-DELETE

      * Mode SEQUENTIAL : après READ
       READ FICHIER NEXT
           NOT AT END
               DELETE FICHIER END-DELETE
       END-READ
```

### START

```cobol
      * Positionnement pour lecture séquentielle
       MOVE '0100' TO CLI-CODE
       START FICHIER KEY >= CLI-CODE
           INVALID KEY DISPLAY 'Aucun enregistrement'
       END-START

      * Variantes : =, >, >=, <, <=
       START FICHIER KEY = CLI-CODE
       START FICHIER KEY > CLI-CODE
       START FICHIER KEY <= CLI-CODE
```

### CLOSE

```cobol
       CLOSE FICHIER-CLIENTS
       CLOSE FICHIER-CLIENTS WITH LOCK   *> Empêche réouverture
```

---

## X-7 FILE STATUS - Codes retour

### Codes de succès (0x)

| Code | Signification |
|------|---------------|
| `00` | Opération réussie |
| `02` | Clé secondaire en double (succès) |
| `04` | Longueur enregistrement incorrecte (succès) |
| `05` | Fichier OPTIONAL non présent (créé vide) |

### Codes de fin (1x)

| Code | Signification |
|------|---------------|
| `10` | Fin de fichier (AT END) |

### Codes d'erreur clé (2x)

| Code | Signification |
|------|---------------|
| `21` | Erreur de séquence de clé |
| `22` | Clé en double |
| `23` | Enregistrement non trouvé |
| `24` | Dépassement limite fichier |

### Codes d'erreur fichier (3x)

| Code | Signification |
|------|---------------|
| `35` | Fichier non trouvé |
| `37` | Mode ouverture incompatible |
| `38` | Fichier fermé avec LOCK |
| `39` | Conflit d'attributs fichier |

### Codes d'erreur logique (4x)

| Code | Signification |
|------|---------------|
| `41` | Fichier déjà ouvert |
| `42` | Fichier non ouvert |
| `43` | Pas de READ préalable (REWRITE/DELETE) |
| `44` | Taille enregistrement différente |
| `46` | READ/START sans positionnement |
| `48` | WRITE sans OPEN OUTPUT/EXTEND/I-O |
| `49` | REWRITE/DELETE sans OPEN I-O |

### Pattern de gestion FILE STATUS

```cobol
       WORKING-STORAGE SECTION.
       01 WS-STATUS   PIC XX.
          88 WS-OK       VALUE '00'.
          88 WS-EOF      VALUE '10'.
          88 WS-NOTFOUND VALUE '23'.
          88 WS-NOFILE   VALUE '35'.

       PROCEDURE DIVISION.
           OPEN INPUT F-CLIENTS
           EVALUATE TRUE
               WHEN WS-OK
                   CONTINUE
               WHEN WS-NOFILE
                   DISPLAY 'Fichier introuvable'
                   STOP RUN
               WHEN OTHER
                   DISPLAY 'Erreur OPEN : ' WS-STATUS
                   STOP RUN
           END-EVALUATE.
```

---

## X-8 Exemple complet mainframe

### Programme COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRAITEMENT-FICHIERS.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * Programme de traitement fichier VSAM KSDS
      * - Lecture séquentielle
      * - Accès direct par clé
      * - Mise à jour
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * Fichier KSDS Clients
      *----------------------------------------------------------------*
           SELECT OPTIONAL F-CLIENTS
               ASSIGN TO DDCLIENTS
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CODE
               ALTERNATE RECORD KEY IS CLI-NOM WITH DUPLICATES
               FILE STATUS IS WS-STATUS-CLI
               RESERVE 3 AREAS.

      *----------------------------------------------------------------*
      * Fichier séquentiel d'édition
      *----------------------------------------------------------------*
           SELECT F-EDITION
               ASSIGN TO DDEDITION
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-EDT.

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      * Structure fichier Clients (KSDS)
      *----------------------------------------------------------------*
       FD  F-CLIENTS
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           DATA RECORD IS ENR-CLIENT.
       01  ENR-CLIENT.
           05  CLI-CODE       PIC 9(6).
           05  CLI-NOM        PIC X(30).
           05  CLI-ADRESSE    PIC X(40).
           05  CLI-SOLDE      PIC S9(9)V99 COMP-3.
           05  CLI-DATE-MAJ   PIC 9(8).
           05  FILLER         PIC X(16).

      *----------------------------------------------------------------*
      * Structure fichier Édition
      *----------------------------------------------------------------*
       FD  F-EDITION
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS OMITTED.
       01  LIGNE-EDITION      PIC X(132).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FILE STATUS
      *----------------------------------------------------------------*
       01  WS-STATUS-CLI      PIC XX.
           88  CLI-OK         VALUE '00'.
           88  CLI-EOF        VALUE '10'.
           88  CLI-NOTFOUND   VALUE '23'.
           88  CLI-NOFILE     VALUE '05' '35'.

       01  WS-STATUS-EDT      PIC XX.

      *----------------------------------------------------------------*
      * Indicateurs
      *----------------------------------------------------------------*
       01  WS-FIN-FICHIER     PIC 9 VALUE 0.
           88  FIN-FICHIER    VALUE 1.

       01  WS-COMPTEUR        PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           PERFORM 1000-INIT
           PERFORM 2000-TRAITEMENT
              UNTIL FIN-FICHIER
           PERFORM 3000-FIN
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INIT.
           OPEN I-O F-CLIENTS
           IF NOT CLI-OK AND NOT CLI-NOFILE
               DISPLAY 'Erreur OPEN CLIENTS : ' WS-STATUS-CLI
               STOP RUN
           END-IF

           OPEN OUTPUT F-EDITION
           IF WS-STATUS-EDT NOT = '00'
               DISPLAY 'Erreur OPEN EDITION : ' WS-STATUS-EDT
               CLOSE F-CLIENTS
               STOP RUN
           END-IF

           PERFORM 2100-LIRE-SUIVANT.

      *----------------------------------------------------------------*
       2000-TRAITEMENT.
           ADD 1 TO WS-COMPTEUR
           PERFORM 2200-EDITER-CLIENT
           PERFORM 2100-LIRE-SUIVANT.

      *----------------------------------------------------------------*
       2100-LIRE-SUIVANT.
           READ F-CLIENTS NEXT
               AT END SET FIN-FICHIER TO TRUE
           END-READ.

      *----------------------------------------------------------------*
       2200-EDITER-CLIENT.
           MOVE SPACES TO LIGNE-EDITION
           STRING CLI-CODE DELIMITED SIZE
                  ' - ' DELIMITED SIZE
                  CLI-NOM DELIMITED SPACE
                  INTO LIGNE-EDITION
           WRITE LIGNE-EDITION.

      *----------------------------------------------------------------*
       3000-FIN.
           CLOSE F-CLIENTS
           CLOSE F-EDITION
           DISPLAY 'Enregistrements traites : ' WS-COMPTEUR.
```

### JCL d'exécution

```jcl
//TRAIFICH JOB (COMPTA),'TRAIT FICHIERS',CLASS=A,MSGCLASS=X
//*
//*-------------------------------------------------------------*
//* TRAITEMENT DES FICHIERS CLIENTS
//*-------------------------------------------------------------*
//STEP01   EXEC PGM=TRAITFIC
//STEPLIB  DD DSN=PROD.LOAD.LIBRARY,DISP=SHR
//*
//* FICHIER KSDS CLIENTS (VSAM)
//DDCLIENTS DD DSN=PROD.VSAM.CLIENTS,DISP=SHR
//*
//* FICHIER D'EDITION (SPOOL)
//DDEDITION DD SYSOUT=*,DCB=(RECFM=FB,LRECL=132,BLKSIZE=1320)
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
```

---

## Résumé

| Concept | Clause/Instruction | Usage |
|---------|-------------------|-------|
| Fichier optionnel | `SELECT OPTIONAL` | Fichier peut ne pas exister |
| Assignation | `ASSIGN TO ddname` | Liaison avec JCL |
| Organisation | `ORGANIZATION IS` | SEQUENTIAL, INDEXED, RELATIVE |
| Mode d'accès | `ACCESS MODE IS` | SEQUENTIAL, RANDOM, DYNAMIC |
| Clé primaire | `RECORD KEY IS` | Fichiers INDEXED |
| Clé secondaire | `ALTERNATE RECORD KEY` | Index alternatif |
| Position relative | `RELATIVE KEY IS` | Fichiers RELATIVE |
| Code retour | `FILE STATUS IS` | Contrôle des erreurs |
| Buffers | `RESERVE n AREAS` | Optimisation E/S |
| Format enr. | `RECORDING MODE IS` | F (fixe), V (variable) |
| Taille enr. | `RECORD CONTAINS` | Taille fixe ou variable |
| Blocage | `BLOCK CONTAINS` | Optimisation mainframe |
| Labels | `LABEL RECORD IS` | STANDARD ou OMITTED |

---

## Références

- Chapitre VII - Gestion des Fichiers (organisations et modes)
- Chapitre VIII - Opérations E/S (OPEN, READ, WRITE, REWRITE, DELETE, START, CLOSE)
- IBM Enterprise COBOL for z/OS - File Processing
- IBM VSAM - Virtual Storage Access Method
