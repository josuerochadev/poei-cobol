# Rapport de Projet - Mini-Projet CICS-VSAM

---

## Identification

| | |
|---|---|
| **Candidat** | Josué ROCHA |
| **Formation** | POEI Développeur Mainframe COBOL |
| **Organisme** | M2i Formation, Strasbourg |
| **Période** | 19 Décembre 2025 - 22 Janvier 2026 |

---

## Thème

Développement d'un mini-projet COBOL-CICS sous z/OS pour l'alimentation du Data Set CLIENT d'une institution financière.

---

## Introduction

Ce projet a été réalisé dans le cadre de la formation POEI Développeur Mainframe COBOL. L'objectif est de mettre en pratique les compétences acquises en programmation COBOL-CICS et en gestion de fichiers VSAM en mode transactionnel.

Le projet consiste à développer un système de gestion de clientèle pour une institution financière, permettant :
- La consultation des informations client
- L'ajout de nouveaux clients
- La modification des données existantes
- La suppression de clients
- La navigation et les statistiques par région

---

## Objectifs pédagogiques

### Compétences visées

| Domaine | Objectif |
|---------|----------|
| **VSAM** | Définir et manipuler des fichiers VSAM (KSDS, ESDS, RRDS), créer des index alternatifs (AIX/PATH) |
| **CICS** | Programmer des transactions avec gestion des commandes (SEND, RECEIVE, READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT) |
| **BMS** | Concevoir des écrans (MAPs) avec attributs dynamiques, validation de saisie et messages d'erreur |
| **JCL** | Maîtriser les jobs de définition VSAM (IDCAMS), chargement de données, assemblage BMS et compilation COBOL-CICS |
| **Administration CICS** | Définir et gérer des ressources via CEDA (DEFINE, INSTALL), CEMT (SET, INQ) et déboguer avec CEDF |

### Progression pédagogique

```
┌────────────────────────────────────────────────────────────────────────┐
│  Partie 0       Partie 1        Partie 2a-2c         Partie 3         │
│  ─────────      ─────────       ─────────────        ────────         │
│  Préparation    READ            WRITE, REWRITE       STARTBR/READNEXT │
│  (Libraries)    (Lecture)       DELETE (CRUD)        (Navigation,AIX) │
│                                                                       │
│  Environnement → Fondamentaux → Opérations CRUD → Techniques avancées │
└────────────────────────────────────────────────────────────────────────┘
```

---

## Livrables du projet

| Livrable | Quantité | Description |
|----------|----------|-------------|
| Programmes COBOL-CICS | 7 | Gestion complète du cycle de vie client |
| MAPs BMS | 7 | Écrans de saisie et d'affichage |
| Transactions CICS | 7 | AFFI, AJOU, MAJO, SUPP, DELG, LGEN, STAT |
| Fichiers VSAM | 2 | FCLIENT (KSDS), PCLIENT (PATH via AIX) |
| JCL | 17 | Définition, assemblage, compilation |
| Captures d'écran | 160+ | Documentation de chaque étape |

---

## Environnement de travail

### Système et interface

| Élément | Description |
|---------|-------------|
| **Système d'exploitation** | z/OS sous émulateur Hercules (TK4-) |
| **Interface utilisateur** | TSO/ISPF pour le développement, CICS pour l'exécution |
| **Gestionnaire transactionnel** | CICS Transaction Server |
| **Type de fichier** | VSAM KSDS (Key-Sequenced Data Set) |

### Langages et technologies

| Langage/Technologie | Usage |
|---------------------|-------|
| **COBOL** | Programmation des traitements métier |
| **CICS (commandes)** | Gestion transactionnelle (SEND, RECEIVE, READ, WRITE, etc.) |
| **BMS (Assembleur)** | Définition des écrans (MAPs) |
| **JCL** | Compilation des programmes et assemblage des MAPs |
| **VSAM** | Stockage et accès aux données |

### Libraries utilisées

| Library | Contenu |
|---------|---------|
| `ROCHA.CICS.SOURCE` | Sources COBOL, MAPs BMS, JCL de compilation |
| `ROCHA.CICS.LINK` | Copybooks générés (DSECT des MAPs) |
| `ROCHA.CICS.LOAD` | Modules exécutables (programmes et MAPs) |

---

## Organisation du rapport

> **Note** : Le document fourni présente les exercices de manière linéaire (exercices 1 à 19). J'ai choisi d'organiser ce rapport en parties thématiques pour une meilleure lisibilité et compréhension de la progression pédagogique.

Le projet est organisé en **4 parties** regroupant **19 exercices** :

```
Partie 0 : Préparation
    └── Exercice 0 : Création des libraries CICS

Partie 1 : Affichage (READ)
    ├── Exercice 1 : Définition VSAM et intégration CICS
    ├── Exercice 2 : MAP BMS d'affichage
    ├── Exercice 3 : Programme COBOL-CICS (READ)
    ├── Exercice 4 : Définition de la transaction
    └── Exercice 5 : Tests et débogage (CEDF)

Partie 2 : Opérations CRUD
    ├── 2a - Ajout (WRITE)
    │   ├── Exercice 6 : MAP BMS d'ajout
    │   ├── Exercice 7 : Programme COBOL-CICS (WRITE)
    │   └── Exercice 8 : Transaction d'ajout
    │
    ├── 2b - Mise à jour (REWRITE)
    │   ├── Exercice 9 : MAP BMS de mise à jour
    │   ├── Exercice 10 : Programme COBOL-CICS (REWRITE)
    │   └── Exercice 11 : Transaction de mise à jour
    │
    └── 2c - Suppression (DELETE)
        ├── Exercice 12 : MAP BMS de suppression
        ├── Exercice 13 : Programme COBOL-CICS (DELETE)
        ├── Exercice 14 : Transaction de suppression
        └── Exercice 15 : Variante avec lecture préalable

Partie 3 : Opérations avancées
    ├── Exercice 16 : Création de clients génériques
    ├── Exercice 17 : Suppression par code générique (STARTBR, READNEXT, DELETE)
    ├── Exercice 18 : Liste générique paginée (10 clients/page)
    └── Exercice 19 : Statistiques par région
```

**Justification de ce découpage :**
- **Partie 0** : Prérequis techniques avant de commencer
- **Partie 1** : Introduction aux concepts CICS avec une opération simple (lecture)
- **Partie 2** : Complète le CRUD avec les opérations d'écriture
- **Partie 3** : Techniques avancées de navigation et agrégation

---

## Sommaire

| # | Fichier | Contenu |
|---|---------|---------|
| 00 | [Introduction](00-introduction.md) | Présentation du projet |
| 01 | [Partie 0 - Préparation](01-partie-0-preparation.md) | Exercice 0 : Création des Libraries |
| 02 | [Partie 1 - Affichage](02-partie-1-affichage.md) | Exercices 1-5 : VSAM, MAP, READ |
| 03 | [Partie 2a - Ajout](03-partie-2a-ajout.md) | Exercices 6-8 : WRITE |
| 04 | [Partie 2b - Mise à jour](04-partie-2b-maj.md) | Exercices 9-11 : REWRITE |
| 05 | [Partie 2c - Suppression](05-partie-2c-suppression.md) | Exercices 12-15 : DELETE |
| 06 | [Partie 3 - Avancées](06-partie-3-avancees.md) | Exercices 16-19 : STARTBR, READNEXT |
| 07 | [Conclusion](07-conclusion.md) | Bilan, annexes, références |

---

[Partie 0 : Préparation >](01-partie-0-preparation.md)
