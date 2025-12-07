# Chapitre IV - Architecture Multicouches et Transactions TSI

## IV-1 Présentation générale

### Préambule

Avec l'évolution des systèmes d'information, les applications mainframe ont dû s'adapter aux exigences modernes :

- **Séparation des responsabilités** : chaque composant a un rôle bien défini
- **Maintenabilité** : modifications localisées sans impact global
- **Réutilisabilité** : composants utilisables par plusieurs applications
- **Scalabilité** : capacité à absorber la charge

L'**architecture multicouches** répond à ces besoins en organisant l'application en couches distinctes et indépendantes.

### Contexte historique

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ÉVOLUTION DES ARCHITECTURES                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1960-1980 : MONOLITHIQUE                                               │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  Terminal ──► Programme unique (IHM + Logique + Accès données) │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  1980-2000 : CLIENT-SERVEUR (2 tiers)                                   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  Client (IHM + Logique) ◄──────────► Serveur (Base de données) │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2000+ : MULTICOUCHES (3 tiers et plus)                                 │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  Présentation ◄──► Traitement (Logique) ◄──► Données           │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Définition

L'**architecture multicouches** (ou architecture N-tiers) est un modèle de conception logicielle qui sépare l'application en **couches distinctes**, chacune ayant une responsabilité spécifique.

Dans le contexte CICS, on parle généralement d'une architecture **3 tiers** :

| Couche | Nom | Responsabilité |
|--------|-----|----------------|
| **1** | Présentation | Interface utilisateur (écrans BMS) |
| **2** | Traitement | Logique métier (règles de gestion) |
| **3** | Données | Accès aux données (VSAM, DB2, IMS) |

```
┌─────────────────────────────────────────────────────────────────────────┐
│                   ARCHITECTURE 3 TIERS CICS                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│       ┌───────────────────┐                                             │
│       │   UTILISATEUR     │                                             │
│       │   Terminal 3270   │                                             │
│       └─────────┬─────────┘                                             │
│                 │                                                        │
│                 ▼                                                        │
│  ┌─────────────────────────────────┐                                    │
│  │      COUCHE PRÉSENTATION        │  Programme COBOL "Front-End"      │
│  │         (Programme P)           │  - Affichage écrans BMS           │
│  │                                 │  - Saisie utilisateur             │
│  │  SEND MAP / RECEIVE MAP         │  - Validation de format           │
│  └───────────────┬─────────────────┘                                    │
│                  │ COMMAREA / TSI                                       │
│                  ▼                                                       │
│  ┌─────────────────────────────────┐                                    │
│  │      COUCHE TRAITEMENT          │  Programme COBOL "Back-End"       │
│  │         (Programme T)           │  - Règles de gestion              │
│  │                                 │  - Calculs métier                 │
│  │  Logique métier pure            │  - Validation fonctionnelle       │
│  └───────────────┬─────────────────┘                                    │
│                  │ EXEC CICS READ/WRITE                                 │
│                  ▼                                                       │
│  ┌─────────────────────────────────┐                                    │
│  │       COUCHE DONNÉES            │  Programme COBOL "Data Access"    │
│  │         (Programme D)           │  - Lecture/Écriture VSAM          │
│  │                                 │  - Requêtes SQL DB2               │
│  │  VSAM / DB2 / IMS               │  - Accès IMS                      │
│  └─────────────────────────────────┘                                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## IV-2 Structure de l'architecture multicouches

### Les trois couches en détail

#### Couche de Présentation (Front-End)

La couche de présentation gère l'**interface homme-machine** :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE PRÉSENTATION                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  RESPONSABILITÉS :                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Affichage des écrans (SEND MAP)                              │   │
│  │  • Réception des saisies (RECEIVE MAP)                          │   │
│  │  • Validation de format (numérique, date, longueur)             │   │
│  │  • Gestion des touches fonction (PF1, PF3, ENTER...)            │   │
│  │  • Navigation entre écrans                                       │   │
│  │  • Messages d'erreur utilisateur                                 │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  NE DOIT PAS CONTENIR :                                                 │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  ✗ Règles de gestion métier                                     │   │
│  │  ✗ Accès direct aux fichiers/bases de données                   │   │
│  │  ✗ Calculs complexes                                            │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

**Exemple de programme de présentation :**

```cobol
      ******************************************************************
      * Programme : CLNTP00 - Couche Présentation Gestion Clients
      * Rôle      : Interface utilisateur uniquement
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTP00.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY DFHAID.
       01 WS-COMMAREA.
          05 CA-ACTION            PIC X(1).
          05 CA-NUM-CLIENT        PIC 9(8).
          05 CA-DONNEES-CLIENT    PIC X(200).
          05 CA-CODE-RETOUR       PIC 9(2).
          05 CA-MESSAGE           PIC X(78).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
      * Gestion des touches - PRESENTATION UNIQUEMENT
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 1000-APPELER-TRAITEMENT
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN OTHER
                   MOVE 'TOUCHE NON AUTORISEE' TO CA-MESSAGE
           END-EVALUATE
           PERFORM 2000-AFFICHER-ECRAN
           PERFORM 9999-RETOUR-CICS.

       1000-APPELER-TRAITEMENT.
      * Appel de la couche de traitement via LINK
           EXEC CICS
               LINK PROGRAM('CLNTT00')
                    COMMAREA(WS-COMMAREA)
           END-EXEC.

       2000-AFFICHER-ECRAN.
      * Affichage BMS - PRESENTATION UNIQUEMENT
           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    ERASE
           END-EXEC.
```

#### Couche de Traitement (Back-End / Business Logic)

La couche de traitement contient la **logique métier** :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE TRAITEMENT                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  RESPONSABILITÉS :                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Règles de gestion métier                                     │   │
│  │  • Validation fonctionnelle (client existe, solde suffisant)   │   │
│  │  • Calculs métier (intérêts, taxes, remises)                   │   │
│  │  • Orchestration des appels à la couche données                 │   │
│  │  • Gestion des transactions (SYNCPOINT, ROLLBACK)               │   │
│  │  • Journalisation fonctionnelle                                 │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  NE DOIT PAS CONTENIR :                                                 │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  ✗ Commandes BMS (SEND MAP, RECEIVE MAP)                        │   │
│  │  ✗ Gestion des écrans et touches                                │   │
│  │  ✗ Requêtes SQL ou accès fichiers directs                       │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

**Exemple de programme de traitement :**

```cobol
      ******************************************************************
      * Programme : CLNTT00 - Couche Traitement Gestion Clients
      * Rôle      : Logique métier uniquement (pas d'écrans)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTT00.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CLIENT-DATA.
          05 WS-NUM-CLIENT       PIC 9(8).
          05 WS-NOM              PIC X(30).
          05 WS-SOLDE            PIC S9(9)V99 COMP-3.

       LINKAGE SECTION.
       01 DFHCOMMAREA            PIC X(300).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MOVE DFHCOMMAREA TO WS-COMMAREA

      * LOGIQUE METIER - pas d'écran ici !
           EVALUATE CA-ACTION
               WHEN 'C'
                   PERFORM 1000-CONSULTER-CLIENT
               WHEN 'M'
                   PERFORM 2000-MODIFIER-CLIENT
               WHEN 'S'
                   PERFORM 3000-CALCULER-SOLDE
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

       1000-CONSULTER-CLIENT.
      * Appel de la couche données
           EXEC CICS
               LINK PROGRAM('CLNTD00')
                    COMMAREA(WS-COMMAREA)
           END-EXEC
      * Vérification des règles métier
           IF CA-CODE-RETOUR = 0
               IF WS-SOLDE < 0
                   MOVE 'ATTENTION: COMPTE DEBITEUR' TO CA-MESSAGE
               END-IF
           END-IF.

       3000-CALCULER-SOLDE.
      * Calcul métier : intérêts
           COMPUTE WS-SOLDE = WS-SOLDE * 1.05.
```

#### Couche de Données (Data Access Layer)

La couche de données gère l'**accès aux ressources** :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE DONNÉES                                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  RESPONSABILITÉS :                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Lecture/écriture fichiers VSAM                               │   │
│  │  • Requêtes SQL DB2                                             │   │
│  │  • Accès bases IMS                                              │   │
│  │  • Gestion des erreurs techniques (NOTFND, DUPREC...)          │   │
│  │  • Transformation données fichier ↔ structure programme        │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  NE DOIT PAS CONTENIR :                                                 │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  ✗ Règles de gestion métier                                     │   │
│  │  ✗ Affichage ou saisie                                          │   │
│  │  ✗ Décisions fonctionnelles                                     │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  SOURCES DE DONNÉES :                                                   │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │   VSAM   │  │   DB2    │  │   IMS    │  │    MQ    │              │
│  │   KSDS   │  │   SQL    │  │   DL/I   │  │  Series  │              │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

**Exemple de programme d'accès aux données :**

```cobol
      ******************************************************************
      * Programme : CLNTD00 - Couche Données Gestion Clients
      * Rôle      : Accès VSAM uniquement (pas de logique métier)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTD00.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CLIENT-REC.
          05 CLI-NUM             PIC 9(8).
          05 CLI-NOM             PIC X(30).
          05 CLI-ADRESSE         PIC X(50).
          05 CLI-SOLDE           PIC S9(9)V99 COMP-3.

       01 WS-RESP                PIC S9(8) COMP.

       LINKAGE SECTION.
       01 DFHCOMMAREA            PIC X(300).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MOVE DFHCOMMAREA TO WS-COMMAREA

      * ACCES DONNEES - uniquement lecture/écriture
           EVALUATE CA-ACTION
               WHEN 'R'
                   PERFORM 1000-LIRE-CLIENT
               WHEN 'W'
                   PERFORM 2000-ECRIRE-CLIENT
               WHEN 'U'
                   PERFORM 3000-MAJ-CLIENT
               WHEN 'D'
                   PERFORM 4000-SUPPRIMER-CLIENT
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

       1000-LIRE-CLIENT.
      * Accès VSAM pur - pas de règle métier
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CA-NUM-CLIENT)
                    RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO CA-CODE-RETOUR
                   MOVE WS-CLIENT-REC TO CA-DONNEES-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO CA-CODE-RETOUR
               WHEN OTHER
                   MOVE 99 TO CA-CODE-RETOUR
           END-EVALUATE.
```

### Flux de communication entre couches

```
┌─────────────────────────────────────────────────────────────────────────┐
│              FLUX DE COMMUNICATION MULTICOUCHES                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│   UTILISATEUR                                                           │
│       │                                                                  │
│       │ (1) Saisie numéro client + ENTER                                │
│       ▼                                                                  │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  PRÉSENTATION (CLNTP00)                                          │   │
│  │  ─────────────────────────                                       │   │
│  │  (2) RECEIVE MAP → Récupère saisie                              │   │
│  │  (3) Validation format (numérique ?)                            │   │
│  │  (4) Prépare COMMAREA avec action='C' + num_client              │   │
│  │  (5) LINK PROGRAM('CLNTT00') ─────────────────────┐             │   │
│  │                                                    │             │   │
│  │  (12) Reçoit COMMAREA avec données ou erreur      │             │   │
│  │  (13) SEND MAP → Affiche résultat                 │             │   │
│  └───────────────────────────────────────────────────│─────────────┘   │
│                                                       │                  │
│                                                       ▼                  │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  TRAITEMENT (CLNTT00)                                            │   │
│  │  ─────────────────────                                           │   │
│  │  (6) Reçoit COMMAREA                                            │   │
│  │  (7) Valide règles métier (client VIP ? actif ?)                │   │
│  │  (8) LINK PROGRAM('CLNTD00') ─────────────────────┐             │   │
│  │                                                    │             │   │
│  │  (10) Reçoit données                              │             │   │
│  │  (11) Applique logique (calculs, enrichissement)  │             │   │
│  │       RETURN vers CLNTP00                         │             │   │
│  └───────────────────────────────────────────────────│─────────────┘   │
│                                                       │                  │
│                                                       ▼                  │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  DONNÉES (CLNTD00)                                               │   │
│  │  ─────────────────                                               │   │
│  │  (9) READ FILE('CLIENTS') → Lecture VSAM                        │   │
│  │      RETURN vers CLNTT00                                        │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## IV-3 Transactions TSI (Terminal Services Interface)

### Présentation des transactions TSI

Les **transactions TSI** (Terminal Services Interface) sont des transactions CICS spécialisées pour gérer les **échanges entre les couches** dans une architecture multicouches.

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    TRANSACTIONS TSI                                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  PRINCIPE :                                                              │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  Transaction TSI = Interface standardisée entre couches         │   │
│  │                                                                  │   │
│  │  ┌──────────┐    TSI    ┌──────────┐    TSI    ┌──────────┐   │   │
│  │  │  Front   │◄─────────►│  Middle  │◄─────────►│  Back    │   │   │
│  │  │  (Écran) │           │ (Logique)│           │ (Données)│   │   │
│  │  └──────────┘           └──────────┘           └──────────┘   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  CARACTÉRISTIQUES :                                                      │
│  • Interface normalisée et documentée                                   │
│  • Échanges via COMMAREA ou Channels/Containers                         │
│  • Découplage entre les couches                                         │
│  • Traçabilité des échanges                                             │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Structure d'une COMMAREA TSI

Pour les échanges TSI, on définit une **COMMAREA normalisée** :

```cobol
      ******************************************************************
      * Copybook : TSICOPY - Structure COMMAREA pour échanges TSI
      ******************************************************************
       01 TSI-COMMAREA.
      *─── EN-TÊTE TSI (zone technique) ────────────────────────────────
          05 TSI-HEADER.
             10 TSI-VERSION        PIC X(2).
                88 TSI-V01         VALUE '01'.
             10 TSI-SERVICE        PIC X(8).
             10 TSI-ACTION         PIC X(4).
                88 TSI-READ        VALUE 'READ'.
                88 TSI-WRIT        VALUE 'WRIT'.
                88 TSI-UPDT        VALUE 'UPDT'.
                88 TSI-DELT        VALUE 'DELT'.
                88 TSI-LIST        VALUE 'LIST'.
             10 TSI-CODE-RETOUR    PIC 9(4).
                88 TSI-OK          VALUE 0.
                88 TSI-NOT-FOUND   VALUE 0013.
                88 TSI-DUPLICATE   VALUE 0014.
                88 TSI-ERROR       VALUE 9999.
             10 TSI-MESSAGE        PIC X(78).
             10 TSI-TIMESTAMP      PIC X(26).
             10 TSI-USER-ID        PIC X(8).

      *─── ZONE DONNÉES (contenu métier) ───────────────────────────────
          05 TSI-DATA.
             10 TSI-CLE            PIC X(20).
             10 TSI-DONNEES        PIC X(500).
             10 TSI-NB-ENREG       PIC 9(5).
```

### Mécanismes d'appel TSI

#### Via LINK (synchrone)

```cobol
      * Appel synchrone - le programme attend la réponse
           MOVE 'CLNTSRV' TO TSI-SERVICE
           SET TSI-READ TO TRUE
           MOVE WS-NUM-CLIENT TO TSI-CLE

           EXEC CICS
               LINK PROGRAM('TSIHANDL')
                    COMMAREA(TSI-COMMAREA)
                    LENGTH(LENGTH OF TSI-COMMAREA)
           END-EXEC

      * Analyse du retour
           IF TSI-OK
               MOVE TSI-DONNEES TO WS-CLIENT-DATA
           ELSE
               MOVE TSI-MESSAGE TO WS-ERREUR
           END-IF.
```

#### Via START (asynchrone)

```cobol
      * Démarrage asynchrone d'une transaction TSI
           MOVE 'CLNTSRV' TO TSI-SERVICE
           SET TSI-LIST TO TRUE

           EXEC CICS
               START TRANSID('TSIB')
                     FROM(TSI-COMMAREA)
                     LENGTH(LENGTH OF TSI-COMMAREA)
           END-EXEC

      * Le programme continue sans attendre
      * Les résultats seront récupérés plus tard via TS Queue
```

#### Via Channels et Containers (CICS TS 3.1+)

Les **Channels/Containers** offrent une alternative moderne à la COMMAREA :

```cobol
      * Création d'un channel pour l'échange
           EXEC CICS
               PUT CONTAINER('CLIENT-REQ')
                   CHANNEL('CLNT-CHANNEL')
                   FROM(WS-REQUEST-DATA)
                   FLENGTH(LENGTH OF WS-REQUEST-DATA)
           END-EXEC

      * Appel du service avec le channel
           EXEC CICS
               LINK PROGRAM('CLNTSRV')
                    CHANNEL('CLNT-CHANNEL')
           END-EXEC

      * Récupération de la réponse
           EXEC CICS
               GET CONTAINER('CLIENT-RESP')
                   CHANNEL('CLNT-CHANNEL')
                   INTO(WS-RESPONSE-DATA)
                   FLENGTH(WS-RESP-LEN)
           END-EXEC
```

**Avantages des Channels/Containers :**

| Caractéristique | COMMAREA | Channels/Containers |
|-----------------|----------|---------------------|
| Taille max | 32 KB | 500 MB+ |
| Structure | Fixe | Flexible (plusieurs containers) |
| Types | Caractères uniquement | BIT, CHAR, etc. |
| Nommage | Non applicable | Containers nommés |

### Exemple complet d'architecture TSI

```
┌─────────────────────────────────────────────────────────────────────────┐
│              ARCHITECTURE TSI COMPLÈTE                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  TRANSACTION : CLNT (Gestion des clients)                               │
│                                                                          │
│  ┌─────────────┐                                                        │
│  │  Terminal   │                                                        │
│  │    3270     │                                                        │
│  └──────┬──────┘                                                        │
│         │                                                                │
│         ▼                                                                │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  TRANS: CLNT                                                     │   │
│  │  PROG:  CLNTPRES                                                 │   │
│  │  RÔLE:  Présentation                                             │   │
│  │                                                                  │   │
│  │  • RECEIVE MAP                                                   │   │
│  │  • Validation format                                             │   │
│  │  • LINK → CLNTTSI (via TSI)                                     │   │
│  │  • SEND MAP                                                      │   │
│  └──────────────────────────┬──────────────────────────────────────┘   │
│                              │                                          │
│                    TSI-COMMAREA                                         │
│                              │                                          │
│                              ▼                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  PROG:  CLNTTSI                                                  │   │
│  │  RÔLE:  Router TSI                                               │   │
│  │                                                                  │   │
│  │  • Analyse TSI-SERVICE                                          │   │
│  │  • Dispatch vers le bon service                                  │   │
│  │  • Journalisation des appels                                     │   │
│  └───────────┬─────────────────────────────────┬───────────────────┘   │
│              │                                  │                        │
│              ▼                                  ▼                        │
│  ┌─────────────────────────┐      ┌─────────────────────────┐          │
│  │  PROG: CLNTTRT          │      │  PROG: CPTTRT           │          │
│  │  Service: CLNTSRV       │      │  Service: CPTSRV        │          │
│  │                         │      │                         │          │
│  │  Traitement Clients     │      │  Traitement Comptes     │          │
│  │  • Règles métier        │      │  • Règles métier        │          │
│  │  • LINK → CLNTDAT       │      │  • LINK → CPTDAT        │          │
│  └───────────┬─────────────┘      └───────────┬─────────────┘          │
│              │                                 │                         │
│              ▼                                 ▼                         │
│  ┌─────────────────────────┐      ┌─────────────────────────┐          │
│  │  PROG: CLNTDAT          │      │  PROG: CPTDAT           │          │
│  │                         │      │                         │          │
│  │  Accès VSAM CLIENTS     │      │  Accès DB2 COMPTES      │          │
│  │  • READ/WRITE/DELETE    │      │  • SELECT/INSERT/UPDATE │          │
│  └─────────────────────────┘      └─────────────────────────┘          │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## IV-4 Avantages de l'architecture multicouches

### Tableau comparatif

| Critère | Architecture Monolithique | Architecture Multicouches |
|---------|---------------------------|---------------------------|
| **Maintenabilité** | Difficile - modifications impactent tout | Facile - modifications localisées |
| **Tests** | Tests unitaires complexes | Tests par couche isolée |
| **Réutilisation** | Faible | Élevée (services partagés) |
| **Équipe** | Tout le monde sur tout | Spécialisation possible |
| **Performance** | Parfois meilleure (moins d'appels) | Overhead des appels inter-couches |
| **Évolutivité** | Limitée | Haute (remplacement de couche) |
| **Documentation** | Souvent absente | Interfaces documentées |

### Avantages détaillés

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    AVANTAGES DE L'ARCHITECTURE MULTICOUCHES              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. SÉPARATION DES PRÉOCCUPATIONS                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Chaque développeur se concentre sur sa couche                │   │
│  │  • Moins de risque d'effets de bord                             │   │
│  │  • Code plus lisible et maintenable                              │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. RÉUTILISABILITÉ                                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • La couche données peut servir plusieurs transactions         │   │
│  │  • Nouveaux écrans sans toucher à la logique                    │   │
│  │  • Services partagés entre applications                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  3. TESTABILITÉ                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Tests unitaires de chaque couche                              │   │
│  │  • Mocking des couches dépendantes                               │   │
│  │  • Tests de non-régression facilités                             │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  4. ÉVOLUTIVITÉ TECHNOLOGIQUE                                           │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Remplacement de VSAM par DB2 : seule couche données change   │   │
│  │  • Nouvel écran web : nouvelle couche présentation              │   │
│  │  • Migration progressive possible                                │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  5. SCALABILITÉ                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Répartition de charge par couche                              │   │
│  │  • Instances multiples d'un service                              │   │
│  │  • Optimisation ciblée                                           │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Exemple concret : évolution d'interface

```
┌─────────────────────────────────────────────────────────────────────────┐
│         EXEMPLE : AJOUT D'UNE INTERFACE WEB                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  AVANT (Terminal 3270 uniquement)                                       │
│  ─────────────────────────────────                                      │
│                                                                          │
│  ┌──────────┐      ┌──────────┐      ┌──────────┐                      │
│  │ Terminal │ ───► │ Trait.   │ ───► │ Données  │                      │
│  │   3270   │      │ (COBOL)  │      │ (VSAM)   │                      │
│  └──────────┘      └──────────┘      └──────────┘                      │
│                                                                          │
│  APRÈS (Terminal 3270 + Interface Web)                                  │
│  ──────────────────────────────────────                                 │
│                                                                          │
│  ┌──────────┐                                                           │
│  │ Terminal │──────────┐                                                │
│  │   3270   │          │                                                │
│  └──────────┘          │                                                │
│                        ▼                                                │
│                   ┌──────────┐      ┌──────────┐                       │
│                   │ Trait.   │ ───► │ Données  │                       │
│                   │ (COBOL)  │      │ (VSAM)   │                       │
│                   └──────────┘      └──────────┘                       │
│                        ▲                                                │
│  ┌──────────┐          │                                                │
│  │  Web     │──────────┘                                                │
│  │ (CICS WS)│  NOUVELLE couche présentation                            │
│  └──────────┘  Traitement et Données INCHANGÉS !                       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Inconvénients à considérer

| Inconvénient | Impact | Mitigation |
|--------------|--------|------------|
| **Complexité initiale** | Plus de programmes à créer | Templates et générateurs |
| **Overhead d'appels** | Latence supplémentaire | Regrouper les appels |
| **Courbe d'apprentissage** | Formation nécessaire | Documentation claire |
| **Debugging** | Trace sur plusieurs programmes | Journalisation centralisée |

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE IV - RÉSUMÉ                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  IV-1 PRÉSENTATION GÉNÉRALE                                             │
│       • Évolution : Monolithique → Client-Serveur → Multicouches       │
│       • Architecture 3 tiers : Présentation / Traitement / Données     │
│       • Séparation des responsabilités                                  │
│                                                                          │
│  IV-2 STRUCTURE DE L'ARCHITECTURE                                       │
│       • Couche Présentation :                                           │
│         - Écrans BMS, SEND/RECEIVE MAP                                 │
│         - Validation de format, gestion des touches                    │
│       • Couche Traitement :                                             │
│         - Logique métier, règles de gestion                            │
│         - Calculs, validations fonctionnelles                          │
│       • Couche Données :                                                │
│         - Accès VSAM, DB2, IMS                                         │
│         - CRUD (Create, Read, Update, Delete)                          │
│                                                                          │
│  IV-3 TRANSACTIONS TSI                                                   │
│       • Interface standardisée entre couches                            │
│       • COMMAREA normalisée avec en-tête et données                    │
│       • Mécanismes : LINK (sync), START (async), Channels              │
│       • Routage vers les services appropriés                           │
│                                                                          │
│  IV-4 AVANTAGES                                                          │
│       • Maintenabilité : modifications localisées                       │
│       • Réutilisabilité : services partagés                            │
│       • Testabilité : tests unitaires par couche                       │
│       • Évolutivité : remplacement de couche facilité                  │
│       • Scalabilité : optimisation ciblée                              │
│                                                                          │
│  BONNES PRATIQUES                                                        │
│       • Définir des interfaces claires (COMMAREA/Channels)             │
│       • Documenter les services disponibles                             │
│       • Pas de logique métier dans la présentation                     │
│       • Pas d'accès données dans le traitement                         │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [← Précédent : SGBD IMS](03-sgbd-ims.md)
- [Suivant : Couche de Présentation →](05-couche-presentation.md)
- [Retour au sommaire](README.md)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre III - SGBD IMS](03-sgbd-ims.md) | [Chapitre V - Couche de Présentation](05-couche-presentation.md) |

---
*Formation COBOL - Module CICS*
