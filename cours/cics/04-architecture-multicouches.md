# Chapitre IV - Architecture Multicouches

## Table des matières

1. [IV-1 Présentation générale](#iv-1-présentation-générale)
2. [IV-2 Structure de l'architecture multicouches](#iv-2-structure-de-larchitecture-multicouches)
3. [IV-3 Les trois couches en détail](#iv-3-les-trois-couches-en-détail)
4. [IV-4 TSI - Transaction Scripting Interface](#iv-4-tsi---transaction-scripting-interface)
5. [IV-5 Exemple de programme COBOL avec TSI](#iv-5-exemple-de-programme-cobol-avec-tsi)
6. [IV-6 Avantages de l'architecture multicouches](#iv-6-avantages-de-larchitecture-multicouches)
7. [IV-7 Exemple pratique : Application CICS-VSAM](#iv-7-exemple-pratique--application-cics-vsam)

---

## IV-1 Présentation générale

### Préambule

L'**Architecture Multicouche** est une approche essentielle dans le développement d'applications. Elle permet de **séparer les différentes préoccupations du système** entre les couches qui composent une application.

Par cette structure séparée, apparaît la notion des **transactions TSI** (Transaction Scripting Interface) qui permettent l'échange des données entre les couches Frontale et Back-end.

> **Transactions TSI** : permettent de garantir l'intégrité des données lors des échanges entre les différentes couches de l'application. Elles assurent que toutes les opérations nécessaires sont exécutées de manière **atomique, cohérente, isolée et durable (ACID)**.

### Définition

Une **Architecture Multicouche** (ou **n-tiers**) sépare les différentes fonctions d'une application en plusieurs couches. Chaque couche est responsable d'une partie spécifique du traitement.

Dans un environnement mainframe utilisant **CICS** (Customer Information Control System) et **VSAM** (Virtual Storage Access Method), une architecture multicouche inclut :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE 3 TIERS CICS                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────────┐                                               │
│   │    PRÉSENTATION     │  ◄── Front-end (Interface utilisateur)       │
│   │     (Front-End)     │                                               │
│   └──────────┬──────────┘                                               │
│              │                                                          │
│              ▼                                                          │
│   ┌─────────────────────┐                                               │
│   │     TRAITEMENT      │  ◄── CICS / TSI (Logique métier)             │
│   │     (CICS/TSI)      │                                               │
│   └──────────┬──────────┘                                               │
│              │                                                          │
│              ▼                                                          │
│   ┌─────────────────────┐                                               │
│   │      DONNÉES        │  ◄── VSAM / DB2 (Stockage)                   │
│   │       (VSAM)        │                                               │
│   └─────────────────────┘                                               │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

**TSI** peut être utilisé pour orchestrer les transactions entre ces différentes couches, en automatisant la gestion des flux de données et des transactions.

> **L'Architecture 3 tiers** est idéale pour les applications nécessitant une séparation claire entre l'interface utilisateur, la logique métier et les données. Elle est largement utilisée dans les applications web, les systèmes d'entreprise et les projets nécessitant une évolutivité.

---

## IV-2 Structure de l'architecture multicouches

### Schéma général avec TSI

```
┌────────────────────────────────────────────────────────────────────────┐
│                        ARCHITECTURE MULTICOUCHE                         │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    COUCHE DE PRÉSENTATION                         │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌──────────────────────────┐  │  │
│  │  │ Terminal    │  │ Application │  │ Applications distribuées │  │  │
│  │  │ 3270        │  │ Web (HTTP)  │  │ (API REST/SOAP)          │  │  │
│  │  └──────┬──────┘  └──────┬──────┘  └────────────┬─────────────┘  │  │
│  └─────────┼────────────────┼─────────────────────┼─────────────────┘  │
│            │                │                     │                     │
│            └────────────────┼─────────────────────┘                     │
│                             │                                           │
│                             ▼                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    COUCHE DE TRAITEMENT                           │  │
│  │                                                                    │  │
│  │    ┌─────────────────────────────────────────────────────────┐    │  │
│  │    │                         CICS                             │    │  │
│  │    │  • Gestion des transactions                              │    │  │
│  │    │  • Gestion des erreurs                                   │    │  │
│  │    │  • Environnement hautement fiable                        │    │  │
│  │    └─────────────────────────────────────────────────────────┘    │  │
│  │                              │                                     │  │
│  │    ┌─────────────────────────┴───────────────────────────────┐    │  │
│  │    │                         TSI                              │    │  │
│  │    │  • Transaction Scripting Interface                       │    │  │
│  │    │  • Tests automatisés                                     │    │  │
│  │    │  • Simulation de charges                                 │    │  │
│  │    │  • Pilotage programmatique de sessions 3270              │    │  │
│  │    └─────────────────────────────────────────────────────────┘    │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                             │                                           │
│                             ▼                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    COUCHE DE DONNÉES                              │  │
│  │                                                                    │  │
│  │    ┌─────────────────┐           ┌─────────────────┐              │  │
│  │    │     VSAM        │           │      DB2        │              │  │
│  │    │  • KSDS         │           │  • Tables       │              │  │
│  │    │  • ESDS         │           │  • SQL          │              │  │
│  │    │  • RRDS         │           │                 │              │  │
│  │    └─────────────────┘           └─────────────────┘              │  │
│  │                                                                    │  │
│  │    Commandes CICS: READ, WRITE, REWRITE, DELETE                   │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                         │
└────────────────────────────────────────────────────────────────────────┘
```

---

## IV-3 Les trois couches en détail

### Couche de Présentation (Front-End)

C'est l'interface utilisateur avec laquelle les utilisateurs interagissent.

| Caractéristique | Description |
|-----------------|-------------|
| **Rôle** | Point d'entrée pour les demandes de traitement de données |
| **Fonction** | Interaction avec l'utilisateur final et gestion de l'affichage |
| **Technologies** | Terminal 3270, Applications Web, Applications distribuées via API |

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE PRÉSENTATION                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  RESPONSABILITÉS :                                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Affichage des écrans (SEND MAP)                              │   │
│  │  • Réception des saisies (RECEIVE MAP)                          │   │
│  │  • Validation de format (numérique, date, longueur)             │   │
│  │  • Gestion des touches fonction (PF1, PF3, ENTER...)            │   │
│  │  • Navigation entre écrans                                       │   │
│  │  • Messages d'erreur utilisateur                                 │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
│  MODES DE COMMUNICATION AVEC CICS :                                    │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Appels API                                                    │   │
│  │  • Web services SOAP/REST                                        │   │
│  │  • Queues de messages MQ                                         │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Couche de Traitement (CICS et TSI)

Cette couche regroupe la **logique métier** et les **règles de traitement**.

#### CICS

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           CICS                                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  • Reçoit les demandes du Front-End                                    │
│  • Gère l'exécution des transactions                                   │
│  • Prend en charge les appels transactionnels                          │
│  • Gestion des erreurs dans un environnement hautement fiable          │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

#### TSI (Transaction Scripting Interface)

Interface IBM utilisée pour automatiser, piloter ou tester des transactions CICS dans un environnement z/OS.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    FONCTIONNALITÉS TSI                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ✅ Tests automatisés de transactions CICS                             │
│  ✅ Simulation de charges                                               │
│  ✅ Pilotage programmatique de sessions 3270                           │
│  ✅ Tests d'intégration batch ↔ CICS                                   │
│                                                                         │
│  MOTEURS D'EXÉCUTION TSI :                                             │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • IBM TEST CONTROL FACILITY (TCF)                               │   │
│  │  • IBM Application Test Facility (ATF)                           │   │
│  │  • IBM CICS Transaction Gateway (CTG) Scripting                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Couche de Données (VSAM ou Base de Données)

Responsable du **stockage total des informations**.

| Technologie | Utilisation |
|-------------|-------------|
| **VSAM** | Fichiers et enregistrements dans un environnement mainframe |
| **DB2** | Base de données relationnelle |

**Commandes CICS pour interagir avec VSAM :**

| Commande | Fonction |
|----------|----------|
| `READ` | Lecture d'enregistrements |
| `WRITE` | Écriture d'enregistrements |
| `REWRITE` | Mise à jour d'enregistrements |
| `DELETE` | Suppression d'enregistrements |

---

## IV-4 TSI - Transaction Scripting Interface

### Modules TSI

| Module | Fonction |
|--------|----------|
| `TSIINIT` | Allocation Terminal + connexion |
| `TSIFINI` | Déconnexion + Libération |
| `TSISEND` | Envoi du Texte / touches |
| `TSIRECEIVE` | Lecture écran |
| `TSI VERIFY` | Assertions de Test |

### Commandes TSI

| Étape | Fonction |
|-------|----------|
| `ALLOCATE` | Réservation de Terminal |
| `CONNECT` | Connexion à CICS |
| `LOGON` | Simulation du logon |
| `SEND/RECEIVE` | Pilotage écran 3270 |
| `VERIFY` | Assertions de test |
| `PFKEY` | Navigation |
| `LOGOFF` | Fin transaction |
| `FREE` | Libération terminal |

### Résumé des commandes EXEC TSI

| Commande | Rôle |
|----------|------|
| `ALLOC` | Réserver un terminal TSI |
| `CONNECT` | Se connecter à CICS |
| `SEND` | Envoyer texte, PFKEY ou champ |
| `RECEIVE` | Lire l'écran |
| `VERIFY` | Vérifier des textes / champs |
| `DISCONNECT` | Se déconnecter |
| `FREE` | Libérer les ressources TSI |

### Flux de communication TSI

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    FLUX TSI - SESSION 3270                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Programme COBOL                          CICS Region                  │
│   (avec EXEC TSI)                                                       │
│        │                                                                │
│        │  1. EXEC TSI ALLOC                                            │
│        │     ──────────────────►  Réserve un terminal virtuel          │
│        │                                                                │
│        │  2. EXEC TSI CONNECT                                          │
│        │     ──────────────────►  Connexion à CICS                     │
│        │                                                                │
│        │  3. EXEC TSI SEND (LOGON)                                     │
│        │     ──────────────────►  Simulation LOGON                     │
│        │                                                                │
│        │  4. EXEC TSI RECEIVE                                          │
│        │     ◄──────────────────  Lecture écran                        │
│        │                                                                │
│        │  5. EXEC TSI VERIFY                                           │
│        │     Vérifie le contenu de l'écran                             │
│        │                                                                │
│        │  6. EXEC TSI SEND (TRANSACTION)                               │
│        │     ──────────────────►  Lance une transaction                │
│        │                                                                │
│        │  7. EXEC TSI RECEIVE                                          │
│        │     ◄──────────────────  Récupère les résultats               │
│        │                                                                │
│        │  8. EXEC TSI DISCONNECT                                       │
│        │     ──────────────────►  Déconnexion                          │
│        │                                                                │
│        │  9. EXEC TSI FREE                                             │
│        │     ──────────────────►  Libère les ressources                │
│        ▼                                                                │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## IV-5 Exemple de programme COBOL avec TSI

### Structure du programme

Le programme effectue les étapes suivantes :
1. Initialise TSI (ALLOC + CONNECT)
2. Effectue un LOGON
3. Appelle la transaction INQY
4. Vérifie les résultats
5. Fait un LOGOFF
6. Déconnecte et libère le terminal

### Code COBOL complet

#### Déclaration TSI

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSI-TEST-INQY.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TERMID        PIC X(4) VALUE SPACES.
       01 WS-APPLID        PIC X(8) VALUE 'CICSTEST'.
       01 WS-USER          PIC X(8) VALUE 'TESTUSR'.
       01 WS-PASS          PIC X(8) VALUE 'PASS123'.
       01 WS-RETURN-CODE   PIC S9(4) COMP.
       01 WS-TEXT          PIC X(80).
       COPY DFH0TSI.       *> COPY les macros EXEC TSI
```

#### Initialisation TSI

```cobol
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY '--- INITIALISATION TSI ---'.

           EXEC TSI ALLOC
               APPLID  (WS-APPLID)
               USERID  (WS-USER)
               PASSWORD(WS-PASS)
               TERMID  (WS-TERMID)
           END-EXEC.

           MOVE RETURN-CODE TO WS-RETURN-CODE.
           IF WS-RETURN-CODE NOT = 0
               DISPLAY 'ERREUR ALLOC TSI RC=' WS-RETURN-CODE
               STOP RUN
           END-IF.

           DISPLAY 'TSI ALLOC OK - TERMID = ' WS-TERMID.

           EXEC TSI CONNECT
               TERMID(WS-TERMID)
           END-EXEC.

           MOVE RETURN-CODE TO WS-RETURN-CODE.
           IF WS-RETURN-CODE NOT = 0
               DISPLAY 'ERREUR CONNECT TSI RC=' WS-RETURN-CODE
               STOP RUN
           END-IF.

           DISPLAY 'TSI CONNECT OK'.
```

#### LOGON CICS

```cobol
           MOVE 'LOGON TESTUSR PASS123' TO WS-TEXT.

           EXEC TSI SEND
               TEXT (WS-TEXT)
               ENTER
               TERMID (WS-TERMID)
           END-EXEC.

           EXEC TSI RECEIVE
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI VERIFY
               TEXT CONTAINS 'LOGON COMPLETED'
               TERMID(WS-TERMID)
           END-EXEC.

           IF RETURN-CODE NOT = 0
               DISPLAY 'ECHEC LOGON'
               STOP RUN
           END-IF.

           DISPLAY 'LOGON OK'.
```

#### Appel Transaction INQY

```cobol
           MOVE 'INQY' TO WS-TEXT.

           EXEC TSI SEND
               TEXT(WS-TEXT)
               ENTER
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI RECEIVE
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI VERIFY
               TEXT CONTAINS 'INQUIRY MENU'
               TERMID(WS-TERMID)
           END-EXEC.

           IF RETURN-CODE NOT = 0
               DISPLAY 'MENU INQY INCORRECT'
               STOP RUN
           END-IF.

           DISPLAY 'MENU INQY OK'.
```

#### Envoi du numéro de compte et vérification

```cobol
           EXEC TSI SEND
               FIELD('ACCOUNT-NO')
               VALUE '000123456789'
               ENTER
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI RECEIVE
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI VERIFY
               FIELD('ACCOUNT-NAME')
               VALUE 'DUPONT JEAN'
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI VERIFY
               FIELD('BALANCE')
               VALUE '00001234.56'
               TERMID(WS-TERMID)
           END-EXEC.

           IF RETURN-CODE = 0
               DISPLAY 'COMPTE VERIFIE AVEC SUCCES'
           ELSE
               DISPLAY 'ECHEC VERIFICATION COMPTE'
               STOP RUN
           END-IF.
```

#### LOGOFF

```cobol
           MOVE 'LOGOFF' TO WS-TEXT.

           EXEC TSI SEND
               TEXT(WS-TEXT)
               ENTER
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI RECEIVE
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI VERIFY
               TEXT CONTAINS 'LOGOFF SUCCESSFUL'
               TERMID(WS-TERMID)
           END-EXEC.

           DISPLAY 'LOGOFF OK'.
```

#### Finalisation

```cobol
           EXEC TSI DISCONNECT
               TERMID(WS-TERMID)
           END-EXEC.

           EXEC TSI FREE
               TERMID(WS-TERMID)
           END-EXEC.

           DISPLAY 'TSI TERMINAL LIBERE'.
           GOBACK.
```

### JCL de Pré-compilation

```jcl
//TSITESTC JOB (ACCT),'COMPIL TSI',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//* ------------------------------------------------------------------
//* PRECOMPIL TSI + COMPILATION COBOL
//* ------------------------------------------------------------------
//PRECOMP  EXEC DFHECP1C,
//         INFILE=YOUR.SOURCE(TSI-TEST-INQY),
//         OUTFILE=&&COBOL,
//         LIB=YOUR.COPYLIB
//* ------------------------------------------------------------------
//COBOL    EXEC IGYWCL,
//         INFILE=&&COBOL,
//         OUTFILE=&&OBJ
//* ------------------------------------------------------------------
//LKED     EXEC HEWL,PARM='LIST,MAP,XREF'
//SYSLIN   DD DISP=SHR,DSN=&&OBJ
//         DD DISP=SHR,DSN=CEE.SCEELKED       /* RUNTIME COBOL  */
//         DD DISP=SHR,DSN=CICSTS56.CICS.SDFHLOAD /* Interface TSI */
//         DD DISP=SHR,DSN=SYS1.CSSLIB        /* TSO/TSI libraries */
//SYSLMOD  DD DISP=SHR,DSN=YOUR.LOAD(TSITEST)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
```

**Notes importantes :**
- `DFHECP1C` = précompilateur CICS
- `CICSTS56.CICS.SDFHLOAD` = contient les commandes EXEC TSI
- Le load final est placé dans `YOUR.LOAD(TSITEST)`

### JCL d'Exécution

```jcl
//TSITESTX JOB (ACCT),'EXEC TSI',CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//* ------------------------------------------------------------------
//* EXECUTION DU PROGRAMME COBOL UTILISANT TSI
//* ------------------------------------------------------------------
//RUN      EXEC PGM=TSITEST,REGION=0M
//STEPLIB  DD DISP=SHR,DSN=YOUR.LOAD
//         DD DISP=SHR,DSN=CICSTS56.CICS.SDFHLOAD  /* TSI   */
//         DD DISP=SHR,DSN=CEE.SCEERUN             /* COBOL */
//         DD DISP=SHR,DSN=SYS1.CSSLIB             /* TSI/ATF */
//*
//TSILOG   DD SYSOUT=*     /* Journal TSI          */
//TSITRACE DD SYSOUT=*     /* Trace écrans 3270    */
//TSIDUMP  DD SYSOUT=*     /* Dump TSI si RC ≠ 0   */
//TSISCRN  DD SYSOUT=*     /* Écrans capturés      */
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD DUMMY
```

**DD Statements TSI :**

| DD | Rôle |
|----|------|
| `TSILOG` | Log des commandes EXEC TSI |
| `TSITRACE` | Trace de communication terminal |
| `TSIDUMP` | Dump complet en cas d'erreur |
| `TSISCRN` | Dump des écrans 3270 |
| `STEPLIB` | Charge les modules TSI, CICS et COBOL |

---

## IV-6 Avantages de l'architecture multicouches

### Dissociation des besoins

Chaque couche dans une architecture multicouche a une fonctionnalité spécifique et bien définie.

### Modularité et réutilisabilité

- Les différentes couches peuvent être développées indépendamment
- Permet de réutiliser des composants ou des services
- La couche de logique métier peut être utilisée par plusieurs interfaces Front-End

### Scalabilité horizontale et verticale

- Chaque couche peut être mise à l'échelle indépendamment
- Les couches métier et données peuvent être déployées sur plusieurs serveurs
- Support du Load Balancing et de la redondance

### Facilité de maintenance

**Indépendance des couches :**
- Déploiement de mises à jour sans perturber les autres parties du système

**Testabilité :**
- Les différentes couches peuvent être testées indépendamment
- Facilite les tests unitaires et les tests d'intégration

### Flexibilité et indépendance technologique

**Changement technologique :**
Chaque couche peut utiliser des technologies différentes :
- Couche de Présentation : Application web
- Couche Métier : Services CICS ou Java
- Couche de Données : VSAM ou DB2

**Interopérabilité :**
- Facilité d'intégration avec des systèmes externes (ERP, etc.)
- Communication via web services ou API

### Sécurité améliorée

**Contrôle des accès et sécurisation des couches :**
- Mécanismes de sécurité spécifiques pour chaque couche
- Authentification différente selon les couches

**Isolation des données sensibles :**
- Données protégées au niveau de la couche de Données
- Accès restreints et audit trails

### Facilité d'intégration et de gestion des services

**Web services et API :**
- Exposition de la logique métier via web services ou APIs RESTful
- Intégration facilitée avec systèmes internes ou externes

**Gestion des transactions :**
- Mécanismes transactionnels robustes dans CICS
- Cohérence des données dans les scénarios distribués

### Développement parallèle et efficacité accrue

- Les équipes peuvent travailler sur différentes couches en parallèle
- Accélération du développement global de l'application

### Support pour des architectures distribuées

**Déploiement sur plusieurs serveurs :**
- Couche de présentation sur serveur Web
- Couches métier et données sur serveurs principaux ou cloud hybride

**Communication asynchrone :**
- Messages MQ ou queues
- Meilleure résilience et gestion des erreurs

### Résumé des avantages

```
┌─────────────────────────────────────────────────────────────────────────┐
│           AVANTAGES DE L'ARCHITECTURE MULTICOUCHE                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ✅ Simplifier le développement et améliorer la maintenance             │
│                                                                         │
│  ✅ Optimiser la scalabilité (capacité d'évolution)                     │
│                                                                         │
│  ✅ Faciliter l'intégration avec d'autres systèmes                      │
│     tout en assurant la sécurité et la modularité                       │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## IV-7 Exemple pratique : Application CICS-VSAM

### Contexte

Application de gestion de la situation financière des employés (vérification des crédits avant mise à jour du salaire).

### Flux de traitement

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     FLUX DE TRAITEMENT                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  1. Transaction pour récupérer les données existantes                   │
│     dans VSAM (Situation Crédit EMPL)                                   │
│                          │                                              │
│                          ▼                                              │
│  2. Valider des règles métier                                           │
│     (Calculer et déduire la somme des Crédits)                          │
│                          │                                              │
│                          ▼                                              │
│  3. Mettre à jour VSAM                                                  │
│     (Data Set EMPLOYE et CREDIT-EMPLOYE)                                │
│                          │                                              │
│                          ▼                                              │
│  4. Retour des résultats au Front-End                                   │
│     (Afficher le résultat selon l'interface User)                       │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Structures de données VSAM

#### Data Set EMPLOYE

| Champ | Type | Description |
|-------|------|-------------|
| `ID-EMPL` | X(4) numérique | Identifiant employé (clé) |
| `NAME-EMPL` | X(15) | Nom de l'employé |
| `DEPT-EMPL` | X(15) | Département |
| `SALAIRE-EMPL` | 9(5)V9(2) | Salaire |
| `ETAT-CRED-EMPL` | X | État crédit ('O' ou 'N') |

#### Data Set CRE-EMP (CREDIT-EMPLOYE)

| Champ | Type | Description |
|-------|------|-------------|
| `ID-EMPL` | X(4) numérique | Identifiant employé (clé) |
| `LIB-CREDIT-EMPL` | X(20) | Libellé du crédit |
| `MONTANT-CREDIT` | 9(5)V9(2) | Montant total du crédit |
| `TOTAL MONTANT-ECHEANCE` | 9(5)V9(2) | Montant de l'échéance |
| `RESTE-CREDIT` | 9(5)V9(2) | Reste à payer |

### Schéma de l'application

```
┌───────────────────────────────────────────────────────────────────────┐
│                    APPLICATION CICS - VSAM                            │
├───────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  ┌──────────────────┐                                                  │
│  │   COUCHE DE      │  • Terminal 3270                                 │
│  │   PRÉSENTATION   │  • Application Web via API RESTful               │
│  │   (Front-End)    │                                                  │
│  └────────┬─────────┘                                                  │
│           │                                                            │
│           │ API                                                        │
│           ▼                                                            │
│  ┌──────────────────┐                                                  │
│  │   COUCHE MÉTIER  │  • CICS : Gestion des transactions RH            │
│  │   (Logique       │  • TSI : Orchestration des actions complexes     │
│  │    métier)       │    ou séquences de transactions                  │
│  └────────┬─────────┘                                                  │
│           │                                                            │
│           │ CICS Commands                                              │
│           ▼                                                            │
│  ┌──────────────────┐                                                  │
│  │   COUCHE DE      │                                                  │
│  │   DONNÉES        │                                                  │
│  │   (Backend)      │                                                  │
│  │                  │                                                  │
│  │  ┌────────────┐  ┌────────────┐                                     │
│  │  │  EMPLOYE   │  │  CRE-EMP   │                                     │
│  │  │   (VSAM)   │  │   (VSAM)   │                                     │
│  │  │            │  │            │                                     │
│  │  │ ID-EMPL    │  │ ID-EMPL    │                                     │
│  │  │ NAME-EMPL  │  │ LIB-CREDIT │                                     │
│  │  │ DEPT-EMPL  │  │ MONTANT    │                                     │
│  │  │ SALAIRE    │  │ ECHEANCE   │                                     │
│  │  │ ETAT-CRED  │  │ RESTE      │                                     │
│  │  └────────────┘  └────────────┘                                     │
│  │                                                                     │
│  │  Commandes: READ, WRITE, REWRITE, DELETE                            │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                        │
└───────────────────────────────────────────────────────────────────────┘
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE IV - RÉSUMÉ                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  IV-1 PRÉSENTATION GÉNÉRALE                                            │
│       • Architecture n-tiers : séparation des préoccupations           │
│       • Architecture 3 tiers : Présentation / Traitement / Données     │
│       • Transactions TSI pour orchestration                            │
│                                                                         │
│  IV-2 STRUCTURE DE L'ARCHITECTURE                                      │
│       • Couche Présentation : Terminal 3270, Web, API                  │
│       • Couche Traitement : CICS + TSI                                 │
│       • Couche Données : VSAM, DB2                                     │
│                                                                         │
│  IV-3 LES TROIS COUCHES                                                │
│       • Présentation : interface utilisateur                           │
│       • Traitement : logique métier, CICS, TSI                         │
│       • Données : READ, WRITE, REWRITE, DELETE                         │
│                                                                         │
│  IV-4 TSI - TRANSACTION SCRIPTING INTERFACE                            │
│       • Modules : TSIINIT, TSIFINI, TSISEND, TSIRECEIVE, VERIFY        │
│       • Commandes EXEC TSI : ALLOC, CONNECT, SEND, RECEIVE,            │
│         VERIFY, DISCONNECT, FREE                                        │
│       • Tests automatisés et simulation de charges                     │
│                                                                         │
│  IV-5 EXEMPLE PROGRAMME TSI                                            │
│       • Initialisation (ALLOC, CONNECT)                                │
│       • LOGON, exécution transaction, VERIFY                           │
│       • Finalisation (DISCONNECT, FREE)                                │
│       • JCL précompilation et exécution                                │
│                                                                         │
│  IV-6 AVANTAGES                                                        │
│       • Modularité et réutilisabilité                                  │
│       • Scalabilité horizontale et verticale                           │
│       • Facilité de maintenance et testabilité                         │
│       • Flexibilité technologique                                      │
│       • Sécurité améliorée                                             │
│       • Support architectures distribuées                              │
│                                                                         │
│  IV-7 EXEMPLE PRATIQUE                                                 │
│       • Application gestion employés avec VSAM                         │
│       • Data Sets EMPLOYE et CRE-EMP                                   │
│       • Flux de traitement complet                                     │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Glossaire

| Terme | Définition |
|-------|------------|
| **CICS** | Customer Information Control System - Système de gestion de transactions IBM |
| **VSAM** | Virtual Storage Access Method - Méthode d'accès aux fichiers IBM |
| **TSI** | Transaction Scripting Interface - Interface de script pour transactions CICS |
| **ACID** | Atomicité, Cohérence, Isolation, Durabilité - Propriétés des transactions |
| **n-tiers** | Architecture à plusieurs couches |
| **3270** | Terminal IBM pour interaction avec mainframe |
| **DB2** | Système de gestion de base de données relationnelle IBM |
| **JCL** | Job Control Language - Langage de contrôle des jobs sur mainframe |
| **COBOL** | Common Business-Oriented Language - Langage de programmation mainframe |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - SGBD IMS](03-sgbd-ims.md) | [Chapitre V - Couche de Présentation](05-couche-presentation.md) |

---
*Formation COBOL - Module CICS*
