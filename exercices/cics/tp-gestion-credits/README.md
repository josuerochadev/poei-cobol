# TP - Gestion des Crédits Employés

## Objectif

Mettre en place une application CICS complète utilisant l'architecture multicouches (3 tiers) avec des données stockées dans des fichiers VSAM.

## Prérequis

- Avoir lu les chapitres 04 à 08 du module CICS
- Comprendre l'architecture multicouches
- Maîtriser les commandes CICS (READ, WRITE, REWRITE)
- Connaître les bases de BMS

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
│  │  CREDPRES (Présentation)                                │ │
│  │  • Écran BMS CREDSET/CREDMAP                           │ │
│  │  • Validation format                                    │ │
│  │  • Gestion touches (ENTER, PF3, PF5)                   │ │
│  └─────────────────────┬───────────────────────────────────┘ │
│                        │ LINK                                 │
│  ┌─────────────────────▼───────────────────────────────────┐ │
│  │  CREDTRT (Traitement)                                   │ │
│  │  • Logique métier                                       │ │
│  │  • Calcul reste après paiement                         │ │
│  │  • Mise à jour état crédit                             │ │
│  └─────────────────────┬───────────────────────────────────┘ │
│                        │ LINK                                 │
│  ┌─────────────────────▼───────────────────────────────────┐ │
│  │  CREDDAO (Données)                                      │ │
│  │  • READ/REWRITE EMPLOYE                                │ │
│  │  • READ/REWRITE CRE-EMP                                │ │
│  └─────────────────────────────────────────────────────────┘ │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

## Fichiers fournis

| Dossier | Fichier | Description |
|---------|---------|-------------|
| `copybooks/` | EMPLOYE.cpy | Structure enregistrement employé |
| `copybooks/` | CREDEMP.cpy | Structure enregistrement crédit |
| `jcl/` | DEFVSAM.jcl | Définition des fichiers VSAM |
| `jcl/` | LOADDATA.jcl | Chargement des données de test |
| `data/` | EMPLOYE.dat | Données employés (6 enreg.) |
| `data/` | CREDEMP.dat | Données crédits (4 enreg.) |
| `bms/` | CREDSET.bms | Définition écran BMS |
| `cobol/` | CREDPRES.cbl | Programme présentation |
| `cobol/` | CREDTRT.cbl | Programme traitement |
| `cobol/` | CREDDAO.cbl | Programme accès données |

## Étapes de réalisation

### Étape 1 : Préparation environnement

1. Compiler le MAPSET `CREDSET.bms`
2. Exécuter `DEFVSAM.jcl` pour créer les fichiers VSAM
3. Exécuter `LOADDATA.jcl` pour charger les données de test

### Étape 2 : Compilation des programmes

1. Compiler `CREDDAO.cbl` (couche données)
2. Compiler `CREDTRT.cbl` (couche traitement)
3. Compiler `CREDPRES.cbl` (couche présentation)

### Étape 3 : Définition CICS

Définir dans les tables CICS :
- Transaction `CRED` → Programme `CREDPRES`
- Programmes `CREDPRES`, `CREDTRT`, `CREDDAO`
- MAPSET `CREDSET`
- Fichiers `EMPLOYE`, `CREDEMP`

### Étape 4 : Tests

| Scénario | Action | Résultat attendu |
|----------|--------|------------------|
| 1 | Saisir `EMP001` + ENTER | Affichage employé + crédit auto |
| 2 | Saisir `EMP002` + ENTER | Affichage employé sans crédit |
| 3 | Saisir `EMP999` + ENTER | Message "Employé non trouvé" |
| 4 | Sur EMP001, appuyer PF5 | Reste diminue de 450.00 |
| 5 | Sur EMP004, appuyer PF5 | Crédit soldé, état passe à 'N' |

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
