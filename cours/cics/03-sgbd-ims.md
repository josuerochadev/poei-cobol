# Chapitre III - SGBD IMS

## 1. Présentation d'IMS

### 1.1 Qu'est-ce qu'IMS ?

**IMS** (Information Management System) est un système de gestion de bases de données et de transactions développé par IBM depuis 1968. Il se compose de deux parties principales :

- **IMS DB** (Database Manager) : Gestionnaire de bases de données hiérarchiques
- **IMS TM** (Transaction Manager) : Moniteur transactionnel (alternatif à CICS)

```
┌─────────────────────────────────────────────────────────────────────┐
│                        SYSTÈME IMS                                   │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│     ┌─────────────────────┐       ┌─────────────────────┐           │
│     │       IMS TM        │       │       IMS DB        │           │
│     │  (Transaction       │       │  (Database          │           │
│     │   Manager)          │       │   Manager)          │           │
│     │                     │       │                     │           │
│     │  Similaire à CICS   │       │  Base hiérarchique  │           │
│     └─────────────────────┘       └─────────────────────┘           │
│                                                                      │
│     Note: Ce chapitre se concentre sur IMS DB                       │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 IMS DB vs Bases relationnelles

| Caractéristique | IMS DB (Hiérarchique) | DB2 (Relationnel) |
|-----------------|----------------------|-------------------|
| **Structure** | Arborescente (parent-enfant) | Tables avec relations |
| **Accès** | Navigation dans l'arbre | SQL |
| **Langage** | DL/I | SQL |
| **Flexibilité** | Structure fixe | Très flexible |
| **Performance** | Très rapide pour accès prédéfinis | Optimiseur de requêtes |
| **Cas d'usage** | Transactions à fort volume | Requêtes ad-hoc |

## 2. Caractéristiques du SGBD IMS

### 2.1 Le modèle hiérarchique

IMS DB utilise un modèle de données **hiérarchique** (arborescent) :

```
                    ┌─────────────┐
                    │   CLIENT    │ ◄── Segment racine
                    │  (Root)     │
                    └──────┬──────┘
                           │
           ┌───────────────┼───────────────┐
           │               │               │
    ┌──────┴──────┐ ┌──────┴──────┐ ┌──────┴──────┐
    │   COMPTE    │ │   ADRESSE   │ │  TELEPHONE  │
    │  (Enfant)   │ │  (Enfant)   │ │  (Enfant)   │
    └──────┬──────┘ └─────────────┘ └─────────────┘
           │
    ┌──────┴──────┐
    │ TRANSACTION │
    │(Petit-enfant)│
    └─────────────┘
```

### 2.2 Concepts fondamentaux

#### Segment
Un **segment** est l'unité de base des données IMS (équivalent d'un enregistrement).

```cobol
       01 CLIENT-SEGMENT.
          05 CLI-NUM          PIC 9(8).
          05 CLI-NOM          PIC X(30).
          05 CLI-PRENOM       PIC X(20).
          05 CLI-DATE-NAISS   PIC 9(8).
```

#### Database (DBD)
La **Database Description** définit la structure physique de la base :
- Hiérarchie des segments
- Champs clés
- Types d'accès

#### Program Specification Block (PSB)
Le **PSB** définit la vue logique de la base pour un programme :
- Segments accessibles
- Opérations autorisées (lecture, écriture, etc.)

#### Program Communication Block (PCB)
Le **PCB** est la zone de communication entre le programme et IMS DB :
- Statut de l'opération
- Informations sur le segment courant

### 2.3 Types d'organisation

IMS DB supporte plusieurs méthodes d'accès :

| Type | Nom complet | Description |
|------|-------------|-------------|
| **HSAM** | Hierarchical Sequential Access Method | Accès séquentiel uniquement |
| **HISAM** | Hierarchical Indexed Sequential | Séquentiel indexé |
| **HDAM** | Hierarchical Direct Access Method | Accès direct par hashing |
| **HIDAM** | Hierarchical Indexed Direct | Accès direct indexé |

## 3. Langage DL/I

### 3.1 Présentation

**DL/I** (Data Language/I) est le langage utilisé pour accéder aux données IMS. Dans un programme COBOL, les appels DL/I se font via :

```cobol
       CALL 'CBLTDLI' USING fonction
                            pcb
                            zone-io
                            ssa...
```

### 3.2 Fonctions principales

| Fonction | Code | Description |
|----------|------|-------------|
| **GU** | Get Unique | Lecture directe d'un segment |
| **GN** | Get Next | Lecture séquentielle suivante |
| **GNP** | Get Next within Parent | Suivant sous le même parent |
| **GHU** | Get Hold Unique | Lecture pour mise à jour |
| **GHN** | Get Hold Next | Séquentiel pour mise à jour |
| **ISRT** | Insert | Insertion d'un segment |
| **REPL** | Replace | Mise à jour d'un segment |
| **DLET** | Delete | Suppression d'un segment |

### 3.3 Structure d'un appel DL/I

```cobol
       WORKING-STORAGE SECTION.

      * Fonction DL/I
       01 WS-FONCTION          PIC X(4).

      * Zone I/O pour le segment
       01 WS-CLIENT-SEG.
          05 CLI-NUM           PIC 9(8).
          05 CLI-NOM           PIC X(30).
          05 CLI-PRENOM        PIC X(20).

      * SSA (Segment Search Argument)
       01 WS-SSA-CLIENT.
          05 FILLER            PIC X(8) VALUE 'CLIENT  '.
          05 FILLER            PIC X(1) VALUE '('.
          05 FILLER            PIC X(8) VALUE 'CLI-NUM '.
          05 FILLER            PIC X(2) VALUE ' ='.
          05 WS-SSA-NUM        PIC 9(8).
          05 FILLER            PIC X(1) VALUE ')'.

       LINKAGE SECTION.
       01 PCB-CLIENT.
          05 PCB-DBD-NAME      PIC X(8).
          05 PCB-SEG-LEVEL     PIC XX.
          05 PCB-STATUS        PIC XX.
          05 PCB-PROC-OPT      PIC X(4).
          05 FILLER            PIC S9(5) COMP.
          05 PCB-SEG-NAME      PIC X(8).
          05 PCB-KEY-LENGTH    PIC S9(5) COMP.
          05 PCB-SENS-SEGS     PIC S9(5) COMP.
          05 PCB-KEY-FB        PIC X(50).
```

### 3.4 Exemples d'appels DL/I

#### Lecture directe (GU)
```cobol
           MOVE 'GU  ' TO WS-FONCTION
           MOVE 12345678 TO WS-SSA-NUM

           CALL 'CBLTDLI' USING WS-FONCTION
                                PCB-CLIENT
                                WS-CLIENT-SEG
                                WS-SSA-CLIENT

           IF PCB-STATUS = SPACES
               DISPLAY 'Client trouvé : ' CLI-NOM
           ELSE IF PCB-STATUS = 'GE'
               DISPLAY 'Client non trouvé'
           END-IF
```

#### Lecture séquentielle (GN)
```cobol
           MOVE 'GN  ' TO WS-FONCTION

           PERFORM UNTIL PCB-STATUS = 'GB'
               CALL 'CBLTDLI' USING WS-FONCTION
                                    PCB-CLIENT
                                    WS-CLIENT-SEG
               IF PCB-STATUS = SPACES
                   DISPLAY 'Client : ' CLI-NOM
               END-IF
           END-PERFORM
```

#### Insertion (ISRT)
```cobol
           MOVE 'ISRT' TO WS-FONCTION
           MOVE 99999999 TO CLI-NUM
           MOVE 'DUPONT' TO CLI-NOM
           MOVE 'JEAN' TO CLI-PRENOM

           CALL 'CBLTDLI' USING WS-FONCTION
                                PCB-CLIENT
                                WS-CLIENT-SEG
                                WS-SSA-PARENT

           IF PCB-STATUS = SPACES
               DISPLAY 'Insertion réussie'
           ELSE IF PCB-STATUS = 'II'
               DISPLAY 'Segment déjà existant'
           END-IF
```

#### Mise à jour (REPL)
```cobol
      * D'abord lecture avec HOLD
           MOVE 'GHU ' TO WS-FONCTION
           CALL 'CBLTDLI' USING WS-FONCTION
                                PCB-CLIENT
                                WS-CLIENT-SEG
                                WS-SSA-CLIENT

           IF PCB-STATUS = SPACES
      * Modification
               MOVE 'NOUVEAU NOM' TO CLI-NOM
               MOVE 'REPL' TO WS-FONCTION
               CALL 'CBLTDLI' USING WS-FONCTION
                                    PCB-CLIENT
                                    WS-CLIENT-SEG
           END-IF
```

### 3.5 Codes statut PCB

| Code | Signification |
|------|---------------|
| `  ` (blancs) | Opération réussie |
| `GE` | Segment non trouvé |
| `GB` | Fin de base de données |
| `GK` | Segment d'un type différent trouvé |
| `II` | Tentative d'insertion d'un doublon |
| `DA` | Erreur lors de la suppression |

## 4. IMS avec CICS

### 4.1 Intégration CICS-IMS DB

CICS peut accéder aux bases IMS DB via le composant **DBCTL** (Database Control) :

```
┌─────────────────────────────────────────────────────────────────────┐
│                                                                      │
│   ┌─────────────┐         ┌─────────────┐         ┌─────────────┐  │
│   │   CICS      │ ◄─────► │   DBCTL     │ ◄─────► │   IMS DB    │  │
│   │(Transaction)│         │  (Interface)│         │  (Database) │  │
│   └─────────────┘         └─────────────┘         └─────────────┘  │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 4.2 Programme COBOL-CICS-IMS

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSIMS1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FONCTION         PIC X(4).
       01 WS-CLIENT-SEG.
          05 CLI-NUM          PIC 9(8).
          05 CLI-NOM          PIC X(30).
       01 WS-SSA-CLIENT       PIC X(20).
       01 WS-MESSAGE          PIC X(50).

       LINKAGE SECTION.
       01 PCB-CLIENT.
          05 PCB-STATUS       PIC XX.

       PROCEDURE DIVISION.

      * Appel DL/I depuis CICS
           MOVE 'GU  ' TO WS-FONCTION

           EXEC CICS
               DLI GU USING PCB(1)
                            SEGMENT(WS-CLIENT-SEG)
                            SEGLENGTH(38)
                            SSA(WS-SSA-CLIENT)
           END-EXEC

           IF PCB-STATUS = SPACES
               MOVE 'CLIENT TROUVE' TO WS-MESSAGE
           ELSE
               MOVE 'CLIENT NON TROUVE' TO WS-MESSAGE
           END-IF

           EXEC CICS
               SEND TEXT FROM(WS-MESSAGE)
                    LENGTH(50)
                    ERASE
           END-EXEC

           EXEC CICS RETURN END-EXEC.
```

## 5. Comparaison IMS DB / VSAM / DB2

```
┌─────────────────────────────────────────────────────────────────────┐
│              COMPARAISON DES SYSTÈMES DE DONNÉES                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  VSAM                                                                │
│  ├── Structure : Fichiers (KSDS, ESDS, RRDS)                        │
│  ├── Accès    : READ, WRITE, REWRITE, DELETE                        │
│  ├── Langage  : Commandes COBOL natives                             │
│  └── Usage    : Fichiers simples, accès direct                      │
│                                                                      │
│  IMS DB                                                              │
│  ├── Structure : Hiérarchique (segments)                            │
│  ├── Accès    : GU, GN, ISRT, REPL, DLET                            │
│  ├── Langage  : DL/I                                                │
│  └── Usage    : Relations parent-enfant fixes                       │
│                                                                      │
│  DB2                                                                 │
│  ├── Structure : Relationnel (tables)                               │
│  ├── Accès    : SELECT, INSERT, UPDATE, DELETE                      │
│  ├── Langage  : SQL                                                 │
│  └── Usage    : Requêtes flexibles, jointures                       │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

## 6. Résumé

```
┌─────────────────────────────────────────────────────────────────────┐
│                     IMS DB - RÉSUMÉ                                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  MODÈLE HIÉRARCHIQUE                                                │
│  • Segments organisés en arbre                                       │
│  • Relations parent-enfant                                          │
│  • Navigation dans la hiérarchie                                    │
│                                                                      │
│  COMPOSANTS                                                          │
│  • DBD : Description de la base                                     │
│  • PSB : Vue programme                                              │
│  • PCB : Zone de communication                                      │
│                                                                      │
│  LANGAGE DL/I                                                        │
│  • GU/GN/GNP : Lectures                                             │
│  • GHU/GHN : Lectures pour mise à jour                              │
│  • ISRT : Insertion                                                 │
│  • REPL : Mise à jour                                               │
│  • DLET : Suppression                                               │
│                                                                      │
│  INTÉGRATION CICS                                                   │
│  • Via DBCTL                                                        │
│  • EXEC CICS DLI ...                                                │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [← Précédent : Organisation du système](02-organisation-systeme.md)
- [Retour au sommaire](README.md)
