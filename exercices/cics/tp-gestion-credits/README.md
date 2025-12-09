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

## Architecture

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
│   └── PROGCRED.cbl      # Programme principal
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
```

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

---

## Déroulement du programme

```
┌─────────────────────────────────────────────────────────────────────────┐
│  FLUX DE TRAITEMENT - PROGCRED                                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  0000-PRINCIPAL                                                          │
│  │                                                                       │
│  ├─► 1000-LIRE-EMPLOYE                                                  │
│  │   • READ FILE('EMPLOYE') INTO(WS-EMPLOYE)                            │
│  │   • Gestion NOTFND si employé inexistant                             │
│  │                                                                       │
│  ├─► 2000-AFFICHER-EMPLOYE                                              │
│  │   • SEND TEXT : ID, Nom, Dept, Salaire, État crédit                  │
│  │                                                                       │
│  ├─► 3000-LIRE-CREDIT (si EMP-ETAT-CRED = 'Y')                         │
│  │   • READ FILE('CREDEMP') INTO(WS-CREDIT)                             │
│  │                                                                       │
│  ├─► 4000-AFFICHER-CREDIT                                               │
│  │   • SEND TEXT : Libellé, Montant, Échéance, Reste                    │
│  │                                                                       │
│  ├─► 5000-PAYER-ECHEANCE                                                │
│  │   • READ FILE('CREDEMP') UPDATE                                      │
│  │   • SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE                          │
│  │   • REWRITE FILE('CREDEMP')                                          │
│  │   │                                                                   │
│  │   └─► 6000-SOLDER-CREDIT (si CRD-RESTE = 0)                         │
│  │       • READ FILE('EMPLOYE') UPDATE                                  │
│  │       • SET EMP-SANS-CREDIT TO TRUE                                  │
│  │       • REWRITE FILE('EMPLOYE')                                      │
│  │                                                                       │
│  └─► 9000-FIN-PROGRAMME                                                 │
│      • EXEC CICS RETURN                                                 │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Exercices complémentaires

### Niveau 1 - Modifications simples

1. Modifier `WS-ID-EMPL` pour tester différents employés (EMP001 à EMP006)
2. Ajouter l'affichage du nombre d'échéances restantes
3. Afficher un message différent si moins de 3 échéances restantes

### Niveau 2 - Nouvelles fonctionnalités

1. Ajouter un `RECEIVE` pour saisir l'ID employé au clavier
2. Permettre le paiement de plusieurs échéances (saisie nombre)
3. Historiser les paiements dans un fichier TD

### Niveau 3 - Architecture avancée

1. Remplacer l'accès VSAM par DB2
2. Séparer en architecture 3-tiers (Présentation/Traitement/Données)
3. Ajouter des écrans BMS pour une meilleure interface

---

## Résultats attendus

Après réalisation complète du TP, vous saurez :

- Utiliser les commandes READ et REWRITE avec verrouillage (UPDATE)
- Gérer les codes retour CICS (RESP/RESP2)
- Implémenter une logique métier avec mise à jour de plusieurs fichiers
- Utiliser SEND TEXT pour l'affichage simple

---

**Navigation**
- [← Retour aux exercices CICS](../README.md)
- [Cours : Couche Traitement](../../../cours/cics/06-couche-traitement.md)
