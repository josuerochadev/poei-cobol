# Chapitre V - Couche de Présentation

## V-1 Introduction à la couche de présentation

### Rôle de la couche de présentation

La **couche de présentation** est l'interface entre l'utilisateur et l'application. Dans un environnement CICS, elle gère :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE PRÉSENTATION                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────┐                                                        │
│  │ Utilisateur │                                                        │
│  │  Terminal   │                                                        │
│  │    3270     │                                                        │
│  └──────┬──────┘                                                        │
│         │                                                                │
│         ▼                                                                │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    RESPONSABILITÉS                               │   │
│  │                                                                  │   │
│  │  1. AFFICHAGE                                                    │   │
│  │     • Envoi des écrans formatés (BMS)                           │   │
│  │     • Mise en forme des données                                  │   │
│  │     • Gestion des attributs visuels (couleurs, protection)      │   │
│  │                                                                  │   │
│  │  2. SAISIE                                                       │   │
│  │     • Réception des données utilisateur                         │   │
│  │     • Détection des touches fonction (PF1-PF12, ENTER, CLEAR)   │   │
│  │     • Position du curseur                                        │   │
│  │                                                                  │   │
│  │  3. VALIDATION DE FORMAT                                         │   │
│  │     • Contrôle du type de données (numérique, alphabétique)     │   │
│  │     • Vérification de la longueur                               │   │
│  │     • Champs obligatoires                                        │   │
│  │                                                                  │   │
│  │  4. NAVIGATION                                                   │   │
│  │     • Enchaînement des écrans                                   │   │
│  │     • Gestion du mode pseudo-conversationnel                    │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Le terminal 3270

Le **terminal 3270** est l'interface traditionnelle des mainframes IBM :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         TERMINAL 3270                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  ▌ GESTION DES CLIENTS                              ▐ CLNT     │   │
│  │─────────────────────────────────────────────────────────────────│   │
│  │                                                                  │   │
│  │  Numéro client : [________]                                     │   │
│  │                                                                  │   │
│  │  Nom          : [____________________________]                  │   │
│  │  Prénom       : [____________________________]                  │   │
│  │  Adresse      : [________________________________________]      │   │
│  │  Code postal  : [_____]                                         │   │
│  │  Ville        : [____________________________]                  │   │
│  │                                                                  │   │
│  │  Solde        : [__________,__]                                 │   │
│  │                                                                  │   │
│  │─────────────────────────────────────────────────────────────────│   │
│  │  MESSAGE: _                                                      │   │
│  │─────────────────────────────────────────────────────────────────│   │
│  │  PF1=Aide  PF3=Quitter  PF7=Préc.  PF8=Suiv.  ENTER=Valider    │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  Caractéristiques :                                                      │
│  • Écran 24 lignes × 80 colonnes (standard)                            │
│  • Mode bloc (envoi de tout l'écran à la fois)                         │
│  • Touches AID (Attention Identifier) : ENTER, PF1-PF24, PA1-PA3       │
│  • Attributs de champs : protégé, intensifié, caché, numérique         │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### BMS - Basic Mapping Support

**BMS** (Basic Mapping Support) est le système CICS qui permet de :

| Fonction | Description |
|----------|-------------|
| **Définir les écrans** | Structure des champs, positions, attributs |
| **Séparer forme et fond** | MAP (définition) vs données (programme) |
| **Gérer les attributs** | Couleurs, protection, intensité |
| **Simplifier le code** | Abstraction du protocole 3270 |

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE BMS                                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  DÉVELOPPEMENT                           EXÉCUTION                       │
│  ─────────────                           ─────────                       │
│                                                                          │
│  ┌─────────────┐     Assemblage     ┌─────────────┐                    │
│  │  Source     │ ──────────────────►│   MAPSET    │                    │
│  │  MAPSET     │                    │  (Load Mod) │                    │
│  │  (Macro     │                    └──────┬──────┘                    │
│  │  Assembleur)│                           │                            │
│  └──────┬──────┘                           │                            │
│         │                                  ▼                            │
│         │                          ┌─────────────┐                     │
│         │                          │    CICS     │                     │
│         │                          │   Région    │                     │
│         │                          └──────┬──────┘                     │
│         │                                 │                            │
│         │     Génération            ┌─────▼─────┐                      │
│         └──────────────────────────►│  COPYBOOK │                      │
│                                     │  (DSECT)  │                      │
│                                     └─────┬─────┘                      │
│                                           │                            │
│                                           ▼                            │
│                                     ┌───────────┐                      │
│                                     │ Programme │                      │
│                                     │  COBOL    │                      │
│                                     └───────────┘                      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## V-2 Définition d'un MAPSET

### Structure d'un MAPSET

Un **MAPSET** est un ensemble de **MAPs** (écrans) regroupés dans un même module :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         MAPSET                                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  MAPSET : CLNTSET (nom du module)                                       │
│  ├── MAP : CLNTM01  (écran de recherche)                               │
│  ├── MAP : CLNTM02  (écran de détail)                                  │
│  └── MAP : CLNTM03  (écran de liste)                                   │
│                                                                          │
│  Chaque MAP contient :                                                  │
│  ├── Champs littéraux (texte fixe)                                     │
│  ├── Champs de saisie (INPUT)                                          │
│  └── Champs d'affichage (OUTPUT)                                       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Exemple de définition BMS (Macro Assembleur)

```asm
***********************************************************************
*  MAPSET : CLNTSET - Écrans de gestion des clients                  *
***********************************************************************
CLNTSET  DFHMSD TYPE=&SYSPARM,                                         X
               LANG=COBOL,                                             X
               MODE=INOUT,                                             X
               TIOAPFX=YES,                                            X
               CTRL=FREEKB,                                            X
               STORAGE=AUTO,                                           X
               TERM=3270-2
*
***********************************************************************
*  MAP : CLNTMAP - Écran principal de gestion client                 *
***********************************************************************
CLNTMAP  DFHMDI SIZE=(24,80),                                          X
               LINE=1,                                                 X
               COLUMN=1
*
*─────────────────────── LIGNE DE TITRE ────────────────────────────
         DFHMDF POS=(1,1),                                             X
               LENGTH=30,                                              X
               ATTRB=(ASKIP,BRT),                                      X
               INITIAL='GESTION DES CLIENTS'
*
         DFHMDF POS=(1,70),                                            X
               LENGTH=4,                                               X
               ATTRB=(ASKIP,BRT),                                      X
               INITIAL='CLNT'
*
*─────────────────────── ZONE DE SAISIE ────────────────────────────
         DFHMDF POS=(4,2),                                             X
               LENGTH=15,                                              X
               ATTRB=ASKIP,                                            X
               INITIAL='Numéro client :'
*
NUMCLI   DFHMDF POS=(4,18),                                            X
               LENGTH=8,                                               X
               ATTRB=(UNPROT,NUM,IC),                                  X
               INITIAL='________'
         DFHMDF POS=(4,27),                                            X
               LENGTH=1,                                               X
               ATTRB=ASKIP
*
         DFHMDF POS=(6,2),                                             X
               LENGTH=15,                                              X
               ATTRB=ASKIP,                                            X
               INITIAL='Nom           :'
*
NOMCLI   DFHMDF POS=(6,18),                                            X
               LENGTH=30,                                              X
               ATTRB=UNPROT
         DFHMDF POS=(6,49),                                            X
               LENGTH=1,                                               X
               ATTRB=ASKIP
*
         DFHMDF POS=(8,2),                                             X
               LENGTH=15,                                              X
               ATTRB=ASKIP,                                            X
               INITIAL='Solde         :'
*
SOLDCLI  DFHMDF POS=(8,18),                                            X
               LENGTH=12,                                              X
               ATTRB=(ASKIP,BRT),                                      X
               PICOUT='ZZZ,ZZZ,ZZ9.99'
         DFHMDF POS=(8,31),                                            X
               LENGTH=1,                                               X
               ATTRB=ASKIP
*
*─────────────────────── ZONE MESSAGE ──────────────────────────────
MSGZONE  DFHMDF POS=(22,2),                                            X
               LENGTH=78,                                              X
               ATTRB=(ASKIP,BRT)
*
*─────────────────────── LIGNE DE COMMANDES ────────────────────────
         DFHMDF POS=(24,2),                                            X
               LENGTH=60,                                              X
               ATTRB=ASKIP,                                            X
               INITIAL='PF1=Aide  PF3=Quitter  ENTER=Rechercher'
*
         DFHMSD TYPE=FINAL
         END
```

### Attributs de champs BMS

| Attribut | Code | Description |
|----------|------|-------------|
| **ASKIP** | Autoskip | Champ protégé, curseur saute |
| **PROT** | Protégé | Champ non modifiable |
| **UNPROT** | Non protégé | Champ saisissable |
| **NUM** | Numérique | Saisie numérique uniquement |
| **BRT** | Intensifié | Affichage brillant |
| **NORM** | Normal | Affichage standard |
| **DRK** | Sombre | Champ invisible (mots de passe) |
| **IC** | Initial Cursor | Position initiale du curseur |
| **FSET** | Field Set | Champ marqué comme modifié |

### COPYBOOK généré (DSECT)

La compilation du MAPSET génère un **copybook COBOL** :

```cobol
      ******************************************************************
      * COPYBOOK GENERE : CLNTSET - Structure des données              *
      ******************************************************************
       01  CLNTMAPI.
           02  FILLER                    PIC X(12).
      *─── Champ NUMCLI ───────────────────────────────────────────────
           02  NUMCLIL                   PIC S9(4) COMP.    *> Longueur
           02  NUMCLIF                   PIC X.             *> Flag
           02  FILLER REDEFINES NUMCLIF.
               03 NUMCLIA                PIC X.             *> Attribut
           02  NUMCLII                   PIC X(8).          *> Input
      *─── Champ NOMCLI ───────────────────────────────────────────────
           02  NOMCLIL                   PIC S9(4) COMP.
           02  NOMCLIF                   PIC X.
           02  FILLER REDEFINES NOMCLIF.
               03 NOMCLIA                PIC X.
           02  NOMCLII                   PIC X(30).
      *─── Champ SOLDCLI ──────────────────────────────────────────────
           02  SOLDCLIL                  PIC S9(4) COMP.
           02  SOLDCLIF                  PIC X.
           02  FILLER REDEFINES SOLDCLIF.
               03 SOLDCLIA               PIC X.
           02  SOLDCLII                  PIC X(12).
      *─── Champ MSGZONE ──────────────────────────────────────────────
           02  MSGZONEL                  PIC S9(4) COMP.
           02  MSGZONEF                  PIC X.
           02  FILLER REDEFINES MSGZONEF.
               03 MSGZONEA               PIC X.
           02  MSGZONEI                  PIC X(78).

       01  CLNTMAPO REDEFINES CLNTMAPI.
           02  FILLER                    PIC X(12).
           02  FILLER                    PIC X(3).
           02  NUMCLIO                   PIC X(8).          *> Output
           02  FILLER                    PIC X(3).
           02  NOMCLIO                   PIC X(30).
           02  FILLER                    PIC X(3).
           02  SOLDCLIO                  PIC X(12).
           02  FILLER                    PIC X(3).
           02  MSGZONEO                  PIC X(78).
```

**Convention de nommage des champs :**

| Suffixe | Signification | Type |
|---------|---------------|------|
| **L** | Length | Longueur des données reçues |
| **F** | Flag | Indicateur de modification |
| **A** | Attribute | Attribut dynamique |
| **I** | Input | Données en entrée (saisie) |
| **O** | Output | Données en sortie (affichage) |

## V-3 Application dans un environnement CICS-VSAM

### Architecture de l'exemple

```
┌─────────────────────────────────────────────────────────────────────────┐
│           APPLICATION GESTION CLIENTS CICS-VSAM                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────┐         ┌─────────────────────────────────────────┐   │
│  │  Terminal   │         │           FICHIER VSAM                   │   │
│  │    3270     │         │           CLIENTS                        │   │
│  └──────┬──────┘         │                                          │   │
│         │                │  CLÉ: NUM-CLIENT (8 car.)                │   │
│         │                │  ├── NOM-CLIENT     (30 car.)            │   │
│         │                │  ├── PRENOM-CLIENT  (20 car.)            │   │
│         │                │  ├── ADRESSE        (50 car.)            │   │
│         │                │  ├── CODE-POSTAL    (5 car.)             │   │
│         │                │  ├── VILLE          (30 car.)            │   │
│         │                │  └── SOLDE          (S9(9)V99)           │   │
│         │                │                                          │   │
│         ▼                └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                    TRANSACTION CLNT                              │   │
│  │                                                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │  PROGRAMME : CLNTPGM                                     │   │   │
│  │  │                                                          │   │   │
│  │  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │   │   │
│  │  │  │ Présentation │  │  Traitement  │  │   Données    │  │   │   │
│  │  │  │              │  │              │  │              │  │   │   │
│  │  │  │ SEND MAP     │  │ Validation   │  │ READ FILE    │  │   │   │
│  │  │  │ RECEIVE MAP  │  │ Logique      │  │ WRITE FILE   │  │   │   │
│  │  │  └──────────────┘  └──────────────┘  └──────────────┘  │   │   │
│  │  │                                                          │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                                                                  │   │
│  │  MAPSET : CLNTSET                                               │   │
│  │  MAP    : CLNTMAP                                               │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Programme de présentation complet

```cobol
      ******************************************************************
      * Programme : CLNTPGM - Gestion des clients (Présentation)
      * Transaction : CLNT
      * MAPSET : CLNTSET / MAP : CLNTMAP
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTPGM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *─── Copybooks CICS ─────────────────────────────────────────────
           COPY DFHAID.
           COPY DFHBMSCA.

      *─── Copybook BMS généré ────────────────────────────────────────
           COPY CLNTSET.

      *─── Variables de travail ───────────────────────────────────────
       01  WS-VARIABLES.
           05  WS-RESP             PIC S9(8) COMP VALUE 0.
           05  WS-RESP2            PIC S9(8) COMP VALUE 0.
           05  WS-MESSAGE          PIC X(78) VALUE SPACES.
           05  WS-NUM-EDIT         PIC 9(8)  VALUE 0.

      *─── Structure enregistrement client ────────────────────────────
       01  WS-CLIENT-REC.
           05  CLI-NUM-CLIENT      PIC X(8).
           05  CLI-NOM             PIC X(30).
           05  CLI-PRENOM          PIC X(20).
           05  CLI-ADRESSE         PIC X(50).
           05  CLI-CODE-POSTAL     PIC X(5).
           05  CLI-VILLE           PIC X(30).
           05  CLI-SOLDE           PIC S9(9)V99 COMP-3.

      *─── COMMAREA pour mode pseudo-conversationnel ──────────────────
       01  WS-COMMAREA.
           05  CA-ETAT             PIC X(1).
               88  CA-PREMIER-PASSAGE    VALUE 'I'.
               88  CA-SAISIE-EN-COURS    VALUE 'S'.
               88  CA-AFFICHAGE-CLIENT   VALUE 'A'.
           05  CA-NUM-CLIENT       PIC X(8).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(9).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entrée du programme
      ******************************************************************
       0000-PRINCIPAL.

      *─── Premier passage : afficher écran vide ──────────────────────
           IF EIBCALEN = 0
               SET CA-PREMIER-PASSAGE TO TRUE
               PERFORM 1000-AFFICHER-ECRAN-VIDE
               PERFORM 9000-RETOUR-CICS
           END-IF

      *─── Passages suivants : traiter la saisie ──────────────────────
           MOVE DFHCOMMAREA TO WS-COMMAREA
           PERFORM 2000-TRAITER-SAISIE
           PERFORM 9000-RETOUR-CICS.

      ******************************************************************
      * 1000-AFFICHER-ECRAN-VIDE : Premier affichage
      ******************************************************************
       1000-AFFICHER-ECRAN-VIDE.

           INITIALIZE CLNTMAPO
           MOVE 'Entrez un numéro client et appuyez ENTER'
               TO MSGZONEO

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    ERASE
           END-EXEC.

      ******************************************************************
      * 2000-TRAITER-SAISIE : Gestion des actions utilisateur
      ******************************************************************
       2000-TRAITER-SAISIE.

      *─── Réception des données saisies ──────────────────────────────
           EXEC CICS
               RECEIVE MAP('CLNTMAP')
                       MAPSET('CLNTSET')
                       INTO(CLNTMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               IF WS-RESP = DFHRESP(MAPFAIL)
                   MOVE 'Aucune donnée saisie' TO WS-MESSAGE
                   PERFORM 3000-AFFICHER-MESSAGE
               ELSE
                   MOVE 'Erreur de réception écran' TO WS-MESSAGE
                   PERFORM 3000-AFFICHER-MESSAGE
               END-IF
               GO TO 2000-EXIT
           END-IF

      *─── Analyse de la touche appuyée ───────────────────────────────
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-TRAITER-ENTER
               WHEN DFHPF1
                   PERFORM 2200-AFFICHER-AIDE
               WHEN DFHPF3
                   PERFORM 9100-QUITTER
               WHEN DFHCLEAR
                   PERFORM 1000-AFFICHER-ECRAN-VIDE
               WHEN OTHER
                   MOVE 'Touche non autorisée' TO WS-MESSAGE
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 2100-TRAITER-ENTER : Recherche d'un client
      ******************************************************************
       2100-TRAITER-ENTER.

      *─── Validation du numéro client ────────────────────────────────
           IF NUMCLIL = 0 OR NUMCLII = SPACES
               MOVE 'Le numéro client est obligatoire' TO WS-MESSAGE
               MOVE DFHBMDAR TO NUMCLIA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Vérification format numérique ──────────────────────────────
           MOVE NUMCLII TO WS-NUM-EDIT
           IF WS-NUM-EDIT NOT NUMERIC
               MOVE 'Le numéro client doit être numérique'
                   TO WS-MESSAGE
               MOVE DFHBMDAR TO NUMCLIA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Recherche dans le fichier VSAM ─────────────────────────────
           MOVE NUMCLII TO CLI-NUM-CLIENT

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CLI-NUM-CLIENT)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2110-AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE 'Client non trouvé' TO WS-MESSAGE
                   MOVE DFHBMDAR TO NUMCLIA
                   PERFORM 3000-AFFICHER-MESSAGE
               WHEN DFHRESP(DISABLED)
                   MOVE 'Fichier CLIENTS indisponible' TO WS-MESSAGE
                   PERFORM 3000-AFFICHER-MESSAGE
               WHEN OTHER
                   STRING 'Erreur lecture RESP=' WS-RESP
                          ' RESP2=' WS-RESP2
                          DELIMITED SIZE INTO WS-MESSAGE
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2100-EXIT.
           EXIT.

      ******************************************************************
      * 2110-AFFICHER-CLIENT : Affichage des données client
      ******************************************************************
       2110-AFFICHER-CLIENT.

           INITIALIZE CLNTMAPO

      *─── Transfert des données vers la MAP ──────────────────────────
           MOVE CLI-NUM-CLIENT TO NUMCLIO
           MOVE CLI-NOM        TO NOMCLIO
           MOVE CLI-SOLDE      TO SOLDCLIO

           MOVE 'Client trouvé' TO MSGZONEO
           SET CA-AFFICHAGE-CLIENT TO TRUE
           MOVE CLI-NUM-CLIENT TO CA-NUM-CLIENT

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    ERASE
           END-EXEC.

      ******************************************************************
      * 2200-AFFICHER-AIDE : Écran d'aide
      ******************************************************************
       2200-AFFICHER-AIDE.

           MOVE 'AIDE: Entrez un numéro client (8 chiffres)'
               TO WS-MESSAGE
           PERFORM 3000-AFFICHER-MESSAGE.

      ******************************************************************
      * 3000-AFFICHER-MESSAGE : Affichage d'un message
      ******************************************************************
       3000-AFFICHER-MESSAGE.

           MOVE WS-MESSAGE TO MSGZONEO

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    DATAONLY
           END-EXEC.

      ******************************************************************
      * 9000-RETOUR-CICS : Retour pseudo-conversationnel
      ******************************************************************
       9000-RETOUR-CICS.

           EXEC CICS
               RETURN TRANSID('CLNT')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      ******************************************************************
      * 9100-QUITTER : Fin de la transaction
      ******************************************************************
       9100-QUITTER.

           EXEC CICS
               SEND TEXT FROM('Transaction terminée. Au revoir.')
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
```

## V-4 Mise en place d'une Transaction (TRNM)

### Définition de la transaction dans CICS

Pour qu'une transaction fonctionne, elle doit être définie dans les **tables CICS** :

```
┌─────────────────────────────────────────────────────────────────────────┐
│              DÉFINITION D'UNE TRANSACTION CICS                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. TABLE PCT (Program Control Table)                                   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  DEFINE TRANSACTION(CLNT)                                        │   │
│  │         GROUP(APPGROUP)                                          │   │
│  │         PROGRAM(CLNTPGM)                                         │   │
│  │         TWASIZE(0)                                               │   │
│  │         PROFILE(DFHCICST)                                        │   │
│  │         STATUS(ENABLED)                                          │   │
│  │         TASKDATALOC(ANY)                                         │   │
│  │         TASKDATAKEY(USER)                                        │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. TABLE PPT (Processing Program Table)                                │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  DEFINE PROGRAM(CLNTPGM)                                         │   │
│  │         GROUP(APPGROUP)                                          │   │
│  │         LANGUAGE(COBOL)                                          │   │
│  │         DATALOCATION(ANY)                                        │   │
│  │         EXECKEY(USER)                                            │   │
│  │         STATUS(ENABLED)                                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  3. MAPSET (dans PPT également)                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  DEFINE MAPSET(CLNTSET)                                          │   │
│  │         GROUP(APPGROUP)                                          │   │
│  │         STATUS(ENABLED)                                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  4. TABLE FCT (File Control Table)                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  DEFINE FILE(CLIENTS)                                            │   │
│  │         GROUP(APPGROUP)                                          │   │
│  │         DSNAME(PROD.VSAM.CLIENTS)                                │   │
│  │         STATUS(ENABLED)                                          │   │
│  │         OPENTIME(FIRSTREF)                                       │   │
│  │         RECORDFORMAT(F)                                          │   │
│  │         ADD(YES) BROWSE(YES) DELETE(YES)                         │   │
│  │         READ(YES) UPDATE(YES)                                    │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Exemple pratique : Transaction TRNM (Menu principal)

#### Définition du MAPSET TRNMSET

```asm
***********************************************************************
*  MAPSET : TRNMSET - Menu principal de l'application                *
***********************************************************************
TRNMSET  DFHMSD TYPE=&SYSPARM,                                         X
               LANG=COBOL,                                             X
               MODE=INOUT,                                             X
               TIOAPFX=YES,                                            X
               STORAGE=AUTO,                                           X
               TERM=3270-2
*
TRNMMAP  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
*─────────────────────── EN-TÊTE ───────────────────────────────────
         DFHMDF POS=(1,1),LENGTH=80,ATTRB=(ASKIP,BRT),                 X
               INITIAL='═══════════════════════════════════════════════X
               ══════════════════════════════════'
*
         DFHMDF POS=(2,25),LENGTH=30,ATTRB=(ASKIP,BRT),                X
               INITIAL='MENU PRINCIPAL APPLICATION'
*
         DFHMDF POS=(3,1),LENGTH=80,ATTRB=(ASKIP,BRT),                 X
               INITIAL='═══════════════════════════════════════════════X
               ══════════════════════════════════'
*
*─────────────────────── OPTIONS DU MENU ───────────────────────────
         DFHMDF POS=(6,20),LENGTH=40,ATTRB=ASKIP,                      X
               INITIAL='1. Gestion des clients'
*
         DFHMDF POS=(8,20),LENGTH=40,ATTRB=ASKIP,                      X
               INITIAL='2. Gestion des comptes'
*
         DFHMDF POS=(10,20),LENGTH=40,ATTRB=ASKIP,                     X
               INITIAL='3. Opérations bancaires'
*
         DFHMDF POS=(12,20),LENGTH=40,ATTRB=ASKIP,                     X
               INITIAL='4. Consultation historique'
*
         DFHMDF POS=(14,20),LENGTH=40,ATTRB=ASKIP,                     X
               INITIAL='5. Administration'
*
*─────────────────────── ZONE DE SAISIE ────────────────────────────
         DFHMDF POS=(17,20),LENGTH=15,ATTRB=ASKIP,                     X
               INITIAL='Votre choix : '
*
CHOIX    DFHMDF POS=(17,36),LENGTH=1,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(17,38),LENGTH=1,ATTRB=ASKIP
*
*─────────────────────── ZONE MESSAGE ──────────────────────────────
MSGZONE  DFHMDF POS=(20,2),LENGTH=78,ATTRB=(ASKIP,BRT)
*
*─────────────────────── LIGNE DE COMMANDES ────────────────────────
         DFHMDF POS=(24,2),LENGTH=40,ATTRB=ASKIP,                      X
               INITIAL='PF3=Quitter'
*
         DFHMSD TYPE=FINAL
         END
```

#### Programme TRNMPGM

```cobol
      ******************************************************************
      * Programme : TRNMPGM - Menu principal
      * Transaction : TRNM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRNMPGM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY DFHAID.
           COPY TRNMSET.

       01  WS-VARIABLES.
           05  WS-RESP             PIC S9(8) COMP.
           05  WS-CHOIX            PIC 9(1).
           05  WS-TRANS-CIBLE      PIC X(4).

       01  WS-COMMAREA             PIC X(1).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(1).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           IF EIBCALEN = 0
               PERFORM 1000-AFFICHER-MENU
           ELSE
               PERFORM 2000-TRAITER-CHOIX
           END-IF

           EXEC CICS
               RETURN TRANSID('TRNM')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(1)
           END-EXEC.

       1000-AFFICHER-MENU.

           INITIALIZE TRNMMAPO
           MOVE 'Sélectionnez une option (1-5)' TO MSGZONEO

           EXEC CICS
               SEND MAP('TRNMMAP')
                    MAPSET('TRNMSET')
                    FROM(TRNMMAPO)
                    ERASE
           END-EXEC.

       2000-TRAITER-CHOIX.

           EXEC CICS
               RECEIVE MAP('TRNMMAP')
                       MAPSET('TRNMSET')
                       INTO(TRNMMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-MENU
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN DFHENTER
                   PERFORM 2100-VALIDER-CHOIX
               WHEN OTHER
                   MOVE 'Touche non autorisée' TO MSGZONEO
                   EXEC CICS
                       SEND MAP('TRNMMAP')
                            MAPSET('TRNMSET')
                            FROM(TRNMMAPO)
                            DATAONLY
                   END-EXEC
           END-EVALUATE.

       2000-EXIT.
           EXIT.

       2100-VALIDER-CHOIX.

           IF CHOIXL = 0 OR CHOIXI = SPACES
               MOVE 'Veuillez entrer un choix' TO MSGZONEO
               EXEC CICS
                   SEND MAP('TRNMMAP')
                        MAPSET('TRNMSET')
                        FROM(TRNMMAPO)
                        DATAONLY
               END-EXEC
               GO TO 2100-EXIT
           END-IF

           MOVE CHOIXI TO WS-CHOIX

           EVALUATE WS-CHOIX
               WHEN 1
                   MOVE 'CLNT' TO WS-TRANS-CIBLE
               WHEN 2
                   MOVE 'CPTE' TO WS-TRANS-CIBLE
               WHEN 3
                   MOVE 'OPER' TO WS-TRANS-CIBLE
               WHEN 4
                   MOVE 'HIST' TO WS-TRANS-CIBLE
               WHEN 5
                   MOVE 'ADMN' TO WS-TRANS-CIBLE
               WHEN OTHER
                   MOVE 'Choix invalide (1-5)' TO MSGZONEO
                   EXEC CICS
                       SEND MAP('TRNMMAP')
                            MAPSET('TRNMSET')
                            FROM(TRNMMAPO)
                            DATAONLY
                   END-EXEC
                   GO TO 2100-EXIT
           END-EVALUATE

      *─── Lancement de la transaction sélectionnée ───────────────────
           EXEC CICS
               RETURN TRANSID(WS-TRANS-CIBLE)
           END-EXEC.

       2100-EXIT.
           EXIT.

       9000-QUITTER.

           EXEC CICS
               SEND TEXT FROM('Merci. A bientôt.')
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
```

## V-5 Commandes BMS essentielles

### SEND MAP - Envoi d'un écran

```cobol
      *─── Envoi complet avec effacement ──────────────────────────────
           EXEC CICS
               SEND MAP('nommap')
                    MAPSET('nommapset')
                    FROM(zone-données-O)
                    ERASE
           END-EXEC

      *─── Envoi des données uniquement (sans le squelette) ───────────
           EXEC CICS
               SEND MAP('nommap')
                    MAPSET('nommapset')
                    FROM(zone-données-O)
                    DATAONLY
           END-EXEC

      *─── Envoi du squelette uniquement (sans les données) ───────────
           EXEC CICS
               SEND MAP('nommap')
                    MAPSET('nommapset')
                    MAPONLY
                    ERASE
           END-EXEC

      *─── Options courantes ──────────────────────────────────────────
      * ERASE      : Efface l'écran avant affichage
      * ERASEAUP   : Efface les champs non protégés
      * CURSOR(n)  : Positionne le curseur
      * ALARM      : Émet un bip sonore
      * FREEKB     : Déverrouille le clavier
```

### RECEIVE MAP - Réception des données

```cobol
      *─── Réception standard ─────────────────────────────────────────
           EXEC CICS
               RECEIVE MAP('nommap')
                       MAPSET('nommapset')
                       INTO(zone-données-I)
                       RESP(WS-RESP)
           END-EXEC

      *─── Gestion des erreurs de réception ───────────────────────────
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(MAPFAIL)
                   MOVE 'Aucune donnée saisie' TO WS-MSG
               WHEN DFHRESP(INVMPSZ)
                   MOVE 'Taille MAP incorrecte' TO WS-MSG
               WHEN OTHER
                   MOVE 'Erreur réception' TO WS-MSG
           END-EVALUATE
```

### Gestion des attributs dynamiques

```cobol
      *─── Copybook DFHBMSCA - Constantes d'attributs ─────────────────
       01  DFHBMSCA.
           05  DFHBMPEM           PIC X VALUE X'00'.  *> Défaut
           05  DFHBMPRO           PIC X VALUE X'F0'.  *> Protégé
           05  DFHBMUNP           PIC X VALUE X'C0'.  *> Non protégé
           05  DFHBMUNN           PIC X VALUE X'D0'.  *> Num non prot.
           05  DFHBMBRY           PIC X VALUE X'F8'.  *> Intensifié
           05  DFHBMDAR           PIC X VALUE X'F4'.  *> Sombre (erreur)
           05  DFHBMASK           PIC X VALUE X'F1'.  *> Autoskip
           05  DFHBMFSE           PIC X VALUE X'C1'.  *> Non prot+FSET

      *─── Exemple : mise en évidence d'un champ en erreur ────────────
           IF WS-ERREUR-NUMCLI
               MOVE DFHBMDAR TO NUMCLIA        *> Champ sombre = erreur
               MOVE -1 TO NUMCLIL              *> Curseur sur ce champ
           ELSE
               MOVE DFHBMUNP TO NUMCLIA        *> Retour normal
           END-IF

      *─── Envoi avec positionnement curseur ──────────────────────────
           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    CURSOR
                    DATAONLY
           END-EXEC
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE V - RÉSUMÉ                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  V-1 INTRODUCTION                                                        │
│      • Rôle : interface utilisateur (affichage, saisie, validation)    │
│      • Terminal 3270 : 24×80, mode bloc, touches AID                   │
│      • BMS : Basic Mapping Support (définition des écrans)             │
│                                                                          │
│  V-2 DÉFINITION D'UN MAPSET                                             │
│      • MAPSET = ensemble de MAPs (écrans)                              │
│      • Macro assembleur : DFHMSD, DFHMDI, DFHMDF                       │
│      • Attributs : ASKIP, PROT, UNPROT, NUM, BRT, DRK, IC              │
│      • COPYBOOK généré : suffixes L, F, A, I, O                        │
│                                                                          │
│  V-3 APPLICATION CICS-VSAM                                              │
│      • Structure : Transaction → Programme → MAP → Fichier             │
│      • Mode pseudo-conversationnel avec COMMAREA                       │
│      • Validation de format dans la couche présentation                │
│                                                                          │
│  V-4 MISE EN PLACE D'UNE TRANSACTION                                    │
│      • PCT : Association transaction ↔ programme                       │
│      • PPT : Définition des programmes et MAPSETs                      │
│      • FCT : Définition des fichiers                                   │
│      • Navigation entre transactions (RETURN TRANSID)                  │
│                                                                          │
│  V-5 COMMANDES BMS                                                       │
│      • SEND MAP : envoi écran (ERASE, DATAONLY, MAPONLY)               │
│      • RECEIVE MAP : réception saisie                                  │
│      • Attributs dynamiques : DFHBMSCA (DFHBMDAR, DFHBMUNP...)        │
│      • Positionnement curseur : CURSOR, longueur = -1                  │
│                                                                          │
│  BONNES PRATIQUES                                                        │
│      • Pas de logique métier dans la présentation                      │
│      • Validation de format uniquement (pas de règles métier)          │
│      • Messages clairs et explicites                                   │
│      • Gestion de toutes les touches AID                               │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [← Précédent : Architecture Multicouches](04-architecture-multicouches.md)
- [Suivant : Couche de Traitement →](06-couche-traitement.md)
- [Retour au sommaire](README.md)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IV - Architecture Multicouches](04-architecture-multicouches.md) | [Chapitre VI - Couche de Traitement](06-couche-traitement.md) |

---
*Formation COBOL - Module CICS*
