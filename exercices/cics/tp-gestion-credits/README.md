# TP - Gestion des Crédits Employés

## Objectif

Mettre en place une application CICS avec un traitement transactionnel et des données stockées dans des fichiers VSAM.

**Version simplifiée** : Un seul programme COBOL utilisant `SEND TEXT` (sans BMS)

## Prérequis

- Avoir lu les chapitres 04 à 07 du module CICS
- Maîtriser les commandes CICS (READ, REWRITE)
- Comprendre la séquence READ UPDATE → REWRITE

## Description fonctionnelle

### Contexte

Une entreprise propose des crédits à ses employés. L'application permet de :

1. **Consulter** les informations d'un employé
2. **Afficher** les détails de son crédit (si existant)
3. **Enregistrer** le paiement d'une échéance
4. **Solder** automatiquement le crédit quand le reste atteint zéro

### Règles métier

| Règle | Description |
|-------|-------------|
| R1 | Un employé peut avoir un crédit actif (`ETAT-CRED = 'Y'`) ou non (`'N'`) |
| R2 | Le paiement d'une échéance diminue le reste du crédit |
| R3 | Si le reste atteint 0, le crédit est soldé (`ETAT-CRED` passe à `'N'`) |
| R4 | Le montant de l'échéance est fixe pour chaque crédit |

## Structures de données

### Fichier EMPLOYE (VSAM KSDS)

| Champ | PIC | Description |
|-------|-----|-------------|
| EMP-ID | X(6) | Clé - Identifiant employé |
| EMP-NAME | X(30) | Nom de l'employé |
| EMP-DEPT | X(10) | Département |
| EMP-SALAIRE | 9(7)V99 COMP-3 | Salaire mensuel |
| EMP-ETAT-CRED | X(1) | 'Y' = crédit actif, 'N' = pas de crédit |

### Fichier CRE-EMP (VSAM KSDS)

| Champ | PIC | Description |
|-------|-----|-------------|
| CRD-ID-EMPL | X(6) | Clé - Identifiant employé |
| CRD-LIBELLE | X(20) | Libellé du crédit |
| CRD-MONTANT-TOTAL | 9(7)V99 COMP-3 | Montant initial du crédit |
| CRD-MONTANT-ECH | 9(5)V99 COMP-3 | Montant de l'échéance |
| CRD-RESTE | 9(7)V99 COMP-3 | Reste à payer |

## Données de test

### Employés (6 enregistrements)

```
EMP001 MARTIN JEAN                  COMPTA     0003500.00 Y
EMP002 DUPONT MARIE                 INFO       0004200.00 N
EMP003 DURAND PIERRE                RH         0003800.00 Y
EMP004 LEROY SOPHIE                 COMPTA     0003200.00 Y
EMP005 MOREAU PAUL                  INFO       0004500.00 N
EMP006 SIMON ANNE                   RH         0003600.00 Y
```

### Crédits (4 enregistrements)

```
EMP001 PRET AUTO            0015000.00 00450.00 0012600.00
EMP003 PRET IMMO            0050000.00 00800.00 0048400.00
EMP004 PRET PERSO           0005000.00 00250.00 0000250.00
EMP006 PRET ETUDES          0008000.00 00200.00 0006800.00
```

## Architecture (version simplifiée)

```
┌──────────────────────────────────────────────────────────────┐
│                      TRANSACTION CRED                         │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  PROGCRED.cbl                                           │ │
│  │  ─────────────                                          │ │
│  │  • Affichage via SEND TEXT (pas de BMS)                │ │
│  │  • Lecture/Mise à jour VSAM directe                    │ │
│  │  • Logique métier intégrée                             │ │
│  └─────────────────────┬───────────────────────────────────┘ │
│                        │                                      │
│              ┌─────────┴─────────┐                           │
│              ▼                   ▼                            │
│        ┌──────────┐        ┌──────────┐                      │
│        │ EMPLOYE  │        │ CRE-EMP  │                      │
│        │  (VSAM)  │        │  (VSAM)  │                      │
│        └──────────┘        └──────────┘                      │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

**Commandes CICS utilisées** :
- `READ FILE()` : Lecture simple
- `READ FILE() UPDATE` : Lecture avec verrouillage
- `REWRITE FILE()` : Mise à jour après READ UPDATE
- `SEND TEXT` : Affichage de messages

## Fichiers fournis

```
tp-gestion-credits/
├── cobol/
│   └── PROGCRED.cbl      # Programme principal (version simple)
├── copybooks/
│   ├── EMPLOYE.cpy       # Structure employé (52 octets)
│   └── CREDEMP.cpy       # Structure crédit (40 octets)
├── data/
│   ├── EMPLOYE.dat       # 6 enregistrements test
│   └── CREDEMP.dat       # 4 enregistrements test
├── jcl/
│   ├── DEFVSAM.jcl       # Définition fichiers VSAM
│   ├── LOADDATA.jcl      # Chargement données test
│   └── COMPCRED.jcl      # Compilation programme PROGCRED
└── README.md

(Version avancée avec BMS - voir section "Exercices complémentaires")
├── bms/CREDSET.bms
├── cobol/CREDPRES.cbl, CREDTRT.cbl, CREDDAO.cbl
```

| Dossier | Fichier | Description |
|---------|---------|-------------|
| `cobol/` | **PROGCRED.cbl** | **Programme principal (version simple)** |
| `copybooks/` | EMPLOYE.cpy | Structure enregistrement employé |
| `copybooks/` | CREDEMP.cpy | Structure enregistrement crédit |
| `jcl/` | DEFVSAM.jcl | Définition des fichiers VSAM |
| `jcl/` | LOADDATA.jcl | Chargement des données de test |
| `jcl/` | COMPCRED.jcl | Compilation du programme CICS |
| `data/` | EMPLOYE.dat | Données employés (6 enreg.) |
| `data/` | CREDEMP.dat | Données crédits (4 enreg.) |

---

## JCL de compilation

### Compilation du programme PROGCRED (COMPCRED.jcl)

```jcl
//COMPCRED JOB 'COMPILE PROGCRED',
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
//TRN.SYSIN DD DSN=FTEST.CICS.SOURCE(PROGCRED),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PROGCRED(R)
/*
```

**Paramètres clés** :
- `DFHYITVL` : Procédure intégrée (Translate + Compile + Link)
- `PROGLIB` : Bibliothèque de sortie pour le programme
- `AD370HLQ` : High-Level Qualifier du compilateur COBOL
- `DFHELII` : Module d'interface CICS-COBOL

---

## Étapes de réalisation

### Étape 1 : Préparation environnement VSAM

1. Exécuter `DEFVSAM.jcl` pour créer les fichiers VSAM
2. Exécuter `LOADDATA.jcl` pour charger les données de test
3. Vérifier avec `LISTCAT` que les fichiers sont créés

### Étape 2 : Compilation du programme

1. Exécuter `COMPCRED.jcl` pour compiler `PROGCRED.cbl`
2. Vérifier le code retour (RC=0)
3. Le programme est dans `FTEST.CICS.LOAD`

### Étape 3 : Définition CICS (CEDA)

```
CEDA DEF FILE(EMPLOYE) GROUP(CREDGRP) DSNAME(USER.CICS.EMPLOYE) ...
CEDA DEF FILE(CREDEMP) GROUP(CREDGRP) DSNAME(USER.CICS.CREDEMP) ...
CEDA DEF PROGRAM(PROGCRED) GROUP(CREDGRP) LANGUAGE(COBOL)
CEDA DEF TRANSACTION(CRED) GROUP(CREDGRP) PROGRAM(PROGCRED)
```

### Étape 4 : Installation et exécution

```
CEDA INSTALL GROUP(CREDGRP)
CRED
```

### Étape 5 : Tests

| Scénario | Action | Résultat attendu |
|----------|--------|------------------|
| 1 | Saisir `EMP001` + ENTER | Affichage employé + crédit auto |
| 2 | Saisir `EMP002` + ENTER | Affichage employé sans crédit |
| 3 | Saisir `EMP999` + ENTER | Message "Employé non trouvé" |
| 4 | Sur EMP001, appuyer PF5 | Reste diminue de 450.00 |
| 5 | Sur EMP004, appuyer PF5 | Crédit soldé, état passe à 'N' |

---

## Déroulement de l'application

```
┌─────────────────────────────────────────────────────────────────────────┐
│  FLUX DE TRAITEMENT - APPLICATION GESTION DES CREDITS                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ÉTAPE 1 : Lecture de l'ID-EMPL                                         │
│  ─────────────────────────────────                                      │
│  L'utilisateur saisit un ID employé (EMP001-EMP006)                     │
│  → Programme CREDPRES reçoit la saisie via RECEIVE MAP                  │
│                                                                          │
│  ÉTAPE 2 : Vérification état crédit                                     │
│  ────────────────────────────────────                                   │
│  → Programme CREDTRT vérifie EMP-ETAT-CRED                              │
│  → Si 'Y' : l'employé a un crédit actif                                │
│  → Si 'N' : pas de crédit en cours                                     │
│                                                                          │
│  ÉTAPE 3 : Lecture informations crédit                                  │
│  ──────────────────────────────────────                                 │
│  Si EMP-ETAT-CRED = 'Y' :                                               │
│  → Programme CREDDAO lit le fichier CRE-EMP                            │
│  → Retourne : libellé, montant total, échéance, reste                   │
│                                                                          │
│  ÉTAPE 4 : Paiement échéance (PF5)                                      │
│  ─────────────────────────────────                                      │
│  1. READ UPDATE sur CRE-EMP (verrouillage)                              │
│  2. RESTE-CREDIT = RESTE-CREDIT - MONTANT-ECHEANCE                     │
│  3. REWRITE CRE-EMP                                                     │
│  4. Si RESTE-CREDIT = 0 :                                               │
│     - READ UPDATE sur EMPLOYE                                           │
│     - ETAT-CRED-EMPL = 'N'                                             │
│     - REWRITE EMPLOYE                                                   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Séquence d'appels entre programmes

```
                    ┌─────────────┐
                    │   Terminal  │
                    └──────┬──────┘
                           │ Transaction CRED
                           ▼
                    ┌─────────────┐
                    │  CREDPRES   │  Couche Présentation
                    │ (BMS/Ecran) │
                    └──────┬──────┘
                           │ LINK
                           ▼
                    ┌─────────────┐
                    │  CREDTRT    │  Couche Traitement
                    │ (Métier)    │
                    └──────┬──────┘
                           │ LINK
                           ▼
                    ┌─────────────┐
                    │  CREDDAO    │  Couche Données
                    │ (VSAM)      │
                    └──────┬──────┘
                           │
              ┌────────────┴────────────┐
              ▼                         ▼
        ┌──────────┐              ┌──────────┐
        │ EMPLOYE  │              │ CRE-EMP  │
        │  (VSAM)  │              │  (VSAM)  │
        └──────────┘              └──────────┘
```

---

## Exercices complémentaires

### Niveau 1 - Modifications simples

1. Ajouter l'affichage du nombre d'échéances restantes
2. Afficher un message différent si moins de 3 échéances restantes
3. Ajouter la touche PF1 pour afficher une aide

### Niveau 2 - Nouvelles fonctionnalités

1. Ajouter la création d'un nouveau crédit (PF4)
2. Permettre le paiement de plusieurs échéances (saisie nombre)
3. Historiser les paiements dans un fichier TD

### Niveau 3 - Architecture avancée

1. Remplacer l'accès VSAM par DB2
2. Ajouter une validation de mot de passe
3. Implémenter un journal d'audit

## Résultats attendus

Après réalisation complète du TP, vous saurez :

- Concevoir une application CICS multicouches
- Définir et utiliser des fichiers VSAM
- Créer des écrans BMS
- Implémenter les commandes READ, REWRITE
- Gérer le mode pseudo-conversationnel
- Appliquer les bonnes pratiques de séparation des responsabilités

---

**Navigation**
- [← Retour aux exercices CICS](../README.md)
- [Cours : Travaux Pratiques](../../../cours/cics/08-travaux-pratiques.md)
