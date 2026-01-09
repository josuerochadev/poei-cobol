# Fil Rouge CICS - Application Bancaire

## Description

Application CICS complète de gestion bancaire en **architecture 3 tiers**, intégrée avec le projet fil-rouge COBOL/JCL existant. Cette application fournit une interface transactionnelle temps réel pour consulter et gérer les données clients et comptes.

## Contexte fonctionnel

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    APPLICATION BANCAIRE CICS                            │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  FONCTIONNALITÉS :                                                      │
│  ─────────────────                                                      │
│  • Menu principal avec navigation                                       │
│  • Consultation des informations client                                 │
│  • Consultation et gestion des comptes                                  │
│  • Recherche par numéro client ou numéro de compte                      │
│  • Affichage des soldes et dernières opérations                         │
│                                                                         │
│  TRANSACTIONS :                                                         │
│  ──────────────                                                         │
│  • MENU : Menu principal de l'application                               │
│  • CLNT : Consultation client                                           │
│  • CPTE : Gestion des comptes                                           │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE 3 TIERS                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  TRANSACTION MENU                                                       │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │  MENUPRES.cbl - Écran menu principal                              │  │
│  │  → Choix 1 : Consultation Client (CLNT)                           │  │
│  │  → Choix 2 : Gestion Comptes (CPTE)                               │  │
│  │  → PF3 : Quitter                                                  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                │                                        │
│                 ┌──────────────┴──────────────┐                         │
│                 ▼                              ▼                         │
│  TRANSACTION CLNT                  TRANSACTION CPTE                     │
│  ┌─────────────────────────┐      ┌─────────────────────────┐          │
│  │ CLNTPRES (Présentation) │      │ CPTEPRES (Présentation) │          │
│  │ CLNTTRT  (Traitement)   │      │ CPTETRT  (Traitement)   │          │
│  │ CLNTDAO  (Données)      │      │ CPTEDAO  (Données)      │          │
│  └───────────┬─────────────┘      └───────────┬─────────────┘          │
│              │                                │                         │
│              ▼                                ▼                         │
│  ┌────────────────────┐           ┌────────────────────┐               │
│  │  VSAM CLIENT       │           │  VSAM COMPTE       │               │
│  │  (KSDS)            │           │  (KSDS)            │               │
│  └────────────────────┘           └────────────────────┘               │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Structure des fichiers

### Fichier CLIENT (KSDS)

| Champ | Type | Description |
|-------|------|-------------|
| CLI-NUM | X(6) | Numéro client (clé primaire) |
| CLI-NOM | X(25) | Nom du client |
| CLI-PRENOM | X(20) | Prénom |
| CLI-ADRESSE | X(30) | Adresse |
| CLI-VILLE | X(20) | Ville |
| CLI-CODEPOST | X(5) | Code postal |
| CLI-TEL | X(10) | Téléphone |
| CLI-DATEOUV | X(8) | Date ouverture (AAAAMMJJ) |
| CLI-REGION | X(2) | Code région |

### Fichier COMPTE (KSDS)

| Champ | Type | Description |
|-------|------|-------------|
| CPT-NUM | X(11) | Numéro de compte (clé primaire) |
| CPT-CLIENT | X(6) | Numéro client (FK) |
| CPT-TYPE | X(1) | Type (C=Courant, E=Épargne, T=Titre) |
| CPT-LIBELLE | X(20) | Libellé du compte |
| CPT-SOLDE | S9(9)V99 COMP-3 | Solde actuel |
| CPT-DATEOUV | X(8) | Date ouverture |
| CPT-DATEDER | X(8) | Date dernière opération |

## Contenu du projet

```
fil-rouge/
├── README.md               # Ce fichier
├── copybooks/
│   ├── CLIENT.cpy          # Structure client
│   └── COMPTE.cpy          # Structure compte
├── cobol/
│   ├── MENUPRES.cbl        # Menu - Présentation
│   ├── CLNTPRES.cbl        # Client - Présentation
│   ├── CLNTTRT.cbl         # Client - Traitement
│   ├── CLNTDAO.cbl         # Client - Données
│   ├── CPTEPRES.cbl        # Compte - Présentation
│   ├── CPTETRT.cbl         # Compte - Traitement
│   └── CPTEDAO.cbl         # Compte - Données
├── bms/
│   ├── MENUSET.bms         # Écran menu
│   ├── CLNTSET.bms         # Écran client
│   └── CPTESET.bms         # Écran compte
├── jcl/
│   ├── DEFVSAM.jcl         # Définition fichiers VSAM
│   └── LOADDATA.jcl        # Chargement données test
└── data/
    ├── CLIENT.dat          # Données test clients
    └── COMPTE.dat          # Données test comptes
```

## Transactions

### MENU - Menu Principal

| Élément | Description |
|---------|-------------|
| Transaction | MENU |
| Programme | MENUPRES |
| Écran | MENUSET/MENUMAP |
| Fonction | Navigation vers les autres transactions |

**Touches :**
- 1 + ENTER : Consultation Client
- 2 + ENTER : Gestion Comptes
- PF3 : Quitter l'application

### CLNT - Consultation Client

| Élément | Description |
|---------|-------------|
| Transaction | CLNT |
| Programmes | CLNTPRES → CLNTTRT → CLNTDAO |
| Écran | CLNTSET/CLNTMAP |
| Fichier | CLIENT (KSDS) |

**Fonctions :**
- Recherche par numéro client
- Affichage des informations complètes
- Liste des comptes associés

**Touches :**
- ENTER : Rechercher
- PF3 : Retour menu
- PF5 : Afficher comptes du client

### CPTE - Gestion Comptes

| Élément | Description |
|---------|-------------|
| Transaction | CPTE |
| Programmes | CPTEPRES → CPTETRT → CPTEDAO |
| Écran | CPTESET/CPTEMAP |
| Fichier | COMPTE (KSDS) |

**Fonctions :**
- Recherche par numéro de compte
- Affichage solde et détails
- Consultation historique simplifié

**Touches :**
- ENTER : Rechercher
- PF3 : Retour menu
- PF7 : Client précédent (browse)
- PF8 : Client suivant (browse)

## Données de test

### Clients (10 enregistrements)

| N° Client | Nom | Ville | Région |
|-----------|-----|-------|--------|
| CLI001 | MARTIN | PARIS | 75 |
| CLI002 | DUPONT | LYON | 69 |
| CLI003 | DURAND | MARSEILLE | 13 |
| CLI004 | LEROY | TOULOUSE | 31 |
| CLI005 | MOREAU | NICE | 06 |
| CLI006 | SIMON | NANTES | 44 |
| CLI007 | LAURENT | STRASBOURG | 67 |
| CLI008 | LEFEBVRE | BORDEAUX | 33 |
| CLI009 | MICHEL | LILLE | 59 |
| CLI010 | GARCIA | MONTPELLIER | 34 |

### Comptes (15 enregistrements)

| N° Compte | Client | Type | Solde |
|-----------|--------|------|-------|
| 00001CLI001 | CLI001 | C | 2 500,00 € |
| 00002CLI001 | CLI001 | E | 15 000,00 € |
| 00003CLI002 | CLI002 | C | 1 200,00 € |
| ... | ... | ... | ... |

## Installation

### Étape 1 : Définition des fichiers VSAM

```
SUBMIT jcl/DEFVSAM.jcl
```

### Étape 2 : Chargement des données

```
SUBMIT jcl/LOADDATA.jcl
```

### Étape 3 : Compilation

```
// EXEC DFHMAPS pour MENUSET, CLNTSET, CPTESET
// EXEC DFHYITVL pour tous les programmes COBOL
```

### Étape 4 : Définition ressources CICS

```
DEFINE TRANSACTION(MENU) PROGRAM(MENUPRES)
DEFINE TRANSACTION(CLNT) PROGRAM(CLNTPRES)
DEFINE TRANSACTION(CPTE) PROGRAM(CPTEPRES)
DEFINE PROGRAM(MENUPRES,CLNTPRES,CLNTTRT,CLNTDAO,CPTEPRES,CPTETRT,CPTEDAO)
DEFINE FILE(CLIENT,COMPTE)
DEFINE MAPSET(MENUSET,CLNTSET,CPTESET)
```

## Concepts CICS illustrés

| Concept | Utilisation |
|---------|-------------|
| Navigation multi-transactions | XCTL entre MENU et transactions |
| Architecture 3 tiers | Présentation/Traitement/Données |
| Mode pseudo-conversationnel | RETURN TRANSID avec COMMAREA |
| BMS multi-écrans | 3 mapsets distincts |
| Accès VSAM | READ, STARTBR, READNEXT, ENDBR |
| Gestion erreurs | RESP, HANDLE CONDITION |
| Communication inter-programmes | LINK avec COMMAREA |

## Relation avec le Fil Rouge COBOL/JCL

Ce projet CICS s'intègre avec le fil-rouge principal (`exercices/cobol/fil-rouge/`) :

| Fil Rouge COBOL/JCL | Fil Rouge CICS |
|---------------------|----------------|
| Batch - création fichiers | Temps réel - consultation |
| JCL + COBOL batch | CICS transactionnel |
| IDCAMS, SORT, MERGE | SEND MAP, READ, STARTBR |
| Traitements de masse | Interactions utilisateur |

## Exercices suggérés

### Exercice 1 : Navigation
1. Lancez MENU
2. Naviguez vers CLNT, puis retournez au menu
3. Naviguez vers CPTE, puis retournez au menu

### Exercice 2 : Consultation Client
1. Recherchez le client CLI001
2. Affichez ses comptes (PF5)
3. Analysez le flux CLNTPRES → CLNTTRT → CLNTDAO

### Exercice 3 : Browse Comptes
1. Recherchez un compte
2. Utilisez PF7/PF8 pour naviguer
3. Comprenez STARTBR/READNEXT/ENDBR

### Exercice 4 : Extension
Ajoutez une nouvelle fonctionnalité :
- Création d'un nouveau client
- Modification du solde d'un compte
- Recherche par nom de client

## Navigation

| Retour |
|--------|
| [Exercices CICS](../README.md) |

---
*Formation CICS - M2i Formation*
