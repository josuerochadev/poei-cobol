# TP Gestion des Crédits Employés

## Objectif

Ce travail pratique met en œuvre une application CICS complète en **architecture 3 tiers** pour gérer les crédits des employés d'une entreprise.

## Contexte fonctionnel

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    APPLICATION GESTION DES CRÉDITS                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  FONCTIONNALITÉS :                                                      │
│  ─────────────────                                                      │
│  • Consulter les informations d'un employé                              │
│  • Vérifier s'il a contracté un crédit                                  │
│  • Afficher les détails du crédit                                       │
│  • Payer une échéance (mettre à jour le reste à payer)                  │
│                                                                         │
│  RÈGLES MÉTIER :                                                        │
│  ───────────────                                                        │
│  1. Un employé peut avoir un crédit (ETAT-CRED = 'Y') ou non ('N')      │
│  2. Si crédit soldé (RESTE = 0), passer ETAT-CRED à 'N'                 │
│  3. Le montant de l'échéance est fixe pour chaque crédit                │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE 3 TIERS                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │  COUCHE PRÉSENTATION - Programme CREDPRES                         │  │
│  │  Transaction : CRED                                               │  │
│  │  ─────────────────────────────────────────────────────────────────│  │
│  │  • Affichage écran BMS (CREDSET/CREDMAP)                          │  │
│  │  • Réception saisie utilisateur                                   │  │
│  │  • Validation format ID employé                                   │  │
│  │  • Gestion touches fonction (ENTER, PF3, PF5)                     │  │
│  └─────────────────────────────┬─────────────────────────────────────┘  │
│                                │ COMMAREA                               │
│                                ▼                                        │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │  COUCHE TRAITEMENT - Programme CREDTRT                            │  │
│  │  ─────────────────────────────────────────────────────────────────│  │
│  │  • Lecture employé (appel CREDDAO)                                │  │
│  │  • Vérification état crédit (ETAT-CRED = 'Y' ?)                   │  │
│  │  • Calcul nouveau reste après paiement                            │  │
│  │  • Mise à jour état crédit si soldé                               │  │
│  └─────────────────────────────┬─────────────────────────────────────┘  │
│                                │ COMMAREA                               │
│                                ▼                                        │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │  COUCHE DONNÉES - Programme CREDDAO                               │  │
│  │  ─────────────────────────────────────────────────────────────────│  │
│  │  • READ/REWRITE fichier EMPLOYE                                   │  │
│  │  • READ/REWRITE fichier CRE-EMP                                   │  │
│  │  • Gestion des erreurs VSAM                                       │  │
│  └─────────────────────────────┬─────────────────────────────────────┘  │
│                                │                                        │
│                                ▼                                        │
│  ┌────────────────────┐  ┌────────────────────┐                         │
│  │  VSAM EMPLOYE      │  │  VSAM CREDEMP      │                         │
│  │  (KSDS)            │  │  (KSDS)            │                         │
│  └────────────────────┘  └────────────────────┘                         │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Structure des fichiers

### Fichier EMPLOYE (KSDS, LRECL=80)

| Champ | Type | Positions | Description |
|-------|------|-----------|-------------|
| EMP-ID | X(6) | 1-6 | Cle primaire (EMP001-EMP999) |
| EMP-NAME | X(30) | 7-36 | Nom complet |
| EMP-DEPT | X(10) | 37-46 | Departement |
| EMP-SALAIRE | 9(7)V99 | 47-55 | Salaire mensuel |
| EMP-ETAT-CRED | X(1) | 56 | Y=Credit actif, N=Pas de credit |
| FILLER | X(24) | 57-80 | Reserve |

### Fichier CREDEMP (KSDS, LRECL=80)

| Champ | Type | Positions | Description |
|-------|------|-----------|-------------|
| CRD-ID-EMPL | X(6) | 1-6 | Cle primaire (FK vers EMPLOYE) |
| CRD-LIBELLE | X(20) | 7-26 | Type de credit |
| CRD-MONTANT-TOTAL | 9(9)V99 | 27-37 | Montant initial |
| CRD-MONTANT-ECH | 9(7)V99 | 38-46 | Echeance mensuelle |
| CRD-RESTE | 9(9)V99 | 47-57 | Reste a payer |
| FILLER | X(23) | 58-80 | Reserve |

## Contenu du TP

```
tp-gestion-credits/
├── README.md               # Ce fichier
├── cobol/
│   ├── CREDPRES.cbl        # Couche Présentation
│   ├── CREDTRT.cbl         # Couche Traitement (structures intégrées)
│   └── CREDDAO.cbl         # Couche Données
├── bms/
│   └── CREDSET.bms         # Définition écran BMS
└── jcl/
    ├── DEFVSAM.jcl         # Définition fichiers VSAM
    └── LOADDATA.jcl        # Chargement données test
```

> **Note** : Les structures EMPLOYE et CREDIT sont intégrées directement dans CREDTRT.cbl (pas de copybooks externes).

## Instructions d'installation

### Étape 1 : Définition des fichiers VSAM

```
SUBMIT jcl/DEFVSAM.jcl
```

Ce JCL crée deux fichiers VSAM KSDS :
- `FTEST.CICS.EMPLOYE`
- `FTEST.CICS.CREDEMP`

### Étape 2 : Chargement des données de test

```
SUBMIT jcl/LOADDATA.jcl
```

### Étape 3 : Compilation des programmes

1. Assembler le mapset BMS :
```
// EXEC DFHMAPS pour CREDSET.bms
```

2. Compiler les programmes COBOL avec le précompilateur CICS :
```
// EXEC DFHYITVL pour CREDPRES.cbl
// EXEC DFHYITVL pour CREDTRT.cbl
// EXEC DFHYITVL pour CREDDAO.cbl
```

### Étape 4 : Définition des ressources CICS

Via CEDA ou RDO, définir :

```
DEFINE TRANSACTION(CRED) GROUP(CREDGRP)
       PROGRAM(CREDPRES)

DEFINE PROGRAM(CREDPRES) GROUP(CREDGRP)
       LANGUAGE(COBOL)
       
DEFINE PROGRAM(CREDTRT) GROUP(CREDGRP)
       LANGUAGE(COBOL)
       
DEFINE PROGRAM(CREDDAO) GROUP(CREDGRP)
       LANGUAGE(COBOL)

DEFINE FILE(EMPLOYE) GROUP(CREDGRP)
       DSNAME(FTEST.CICS.EMPLOYE)
       RECORDFORMAT(V) KEYLENGTH(6)

DEFINE FILE(CREDEMP) GROUP(CREDGRP)
       DSNAME(FTEST.CICS.CREDEMP)
       RECORDFORMAT(V) KEYLENGTH(6)

DEFINE MAPSET(CREDSET) GROUP(CREDGRP)
```

## Utilisation

### Lancer la transaction

```
CRED
```

### Touches disponibles

| Touche | Action |
|--------|--------|
| ENTER | Rechercher un employé par son ID |
| PF5 | Payer une échéance du crédit |
| PF3 | Quitter la transaction |

### Données de test

| ID | Nom | Département | Salaire | Crédit | Reste |
|----|-----|-------------|---------|--------|-------|
| EMP001 | DUPONT JEAN | FINANCE | 35 000 € | CREDIT IMMOBILIER | 12 000 € |
| EMP002 | MARTIN MARIE | RH | 42 000 € | - | - |
| EMP003 | DURAND PIERRE | IT | 55 000 € | CREDIT AUTO | 1 500 € |
| EMP004 | BERNARD SOPHIE | FINANCE | 38 000 € | CREDIT PERSO | 250 € |
| EMP005 | PETIT ALAIN | IT | 62 000 € | - | - |
| EMP006 | MOREAU CLAIRE | RH | 48 000 € | CREDIT TRAVAUX | 900 € |

**Note**: EMP004 a un reste de 250 € pour faciliter le test du soldage de crédit.

> **Important** : Les fichiers VSAM (FTEST.CICS.EMPLOYE et FTEST.CICS.CREDEMP) sont partagés avec le dossier `pratique/`. Les données sont identiques dans les deux JCL LOADDATA.jcl.

## Exercices suggérés

### Exercice 1 : Comprendre le flux
1. Lancez la transaction CRED
2. Recherchez l'employé EMP001
3. Identifiez le flux d'appels entre les 3 couches
4. Utilisez CEDF pour tracer les commandes CICS

### Exercice 2 : Tester le paiement
1. Recherchez l'employé EMP004 (reste = 250 €)
2. Payez une échéance (PF5)
3. Vérifiez que le crédit est soldé et que ETAT-CRED passe à 'N'

### Exercice 3 : Ajouter une fonctionnalité
Modifiez l'application pour :
- Afficher l'historique des paiements
- Permettre la création d'un nouveau crédit
- Calculer automatiquement le nombre d'échéances restantes

### Exercice 4 : Gestion des erreurs
1. Recherchez un employé inexistant (EMP999)
2. Essayez de payer une échéance pour un employé sans crédit
3. Analysez les codes retour dans CREDDAO

## Concepts CICS illustrés

| Concept | Utilisation |
|---------|-------------|
| Mode pseudo-conversationnel | RETURN TRANSID avec COMMAREA |
| Architecture 3 tiers | CREDPRES → CREDTRT → CREDDAO |
| BMS | SEND MAP, RECEIVE MAP |
| Fichiers VSAM | READ, READ UPDATE, REWRITE |
| Communication inter-programmes | LINK avec COMMAREA |
| Gestion des touches | EIBAID, DFHENTER, DFHPF3, DFHPF5 |
| Codes retour | RESP, DFHRESP(NORMAL), DFHRESP(NOTFND) |

## Navigation

| Retour |
|--------|
| [Exercices CICS](../README.md) |

---
*Formation CICS - M2i Formation*
