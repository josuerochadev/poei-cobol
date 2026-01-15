# Rapport de Projet - Mini-Projet CICS-VSAM

**Theme** : Developpement d'un mini-projet COBOL-CICS sous z/OS pour l'alimentation du Data Set CLIENT d'une institution financiere.

**Candidat** : Josue ROCHA
**Date** : 19 Decembre 2025 - 22 Janvier 2026
**Formation** : POEI Developpeur Mainframe COBOL - M2i Formation, Strasbourg

---

## Introduction

Ce projet a ete realise dans le cadre de la formation POEI Developpeur Mainframe COBOL. L'objectif est de mettre en pratique les competences acquises en programmation COBOL-CICS et en gestion de fichiers VSAM en mode transactionnel.

Le projet consiste a developper un systeme de gestion de clientele pour une institution financiere, permettant :
- La consultation des informations client
- L'ajout de nouveaux clients
- La modification des donnees existantes
- La suppression de clients
- La navigation et les statistiques par region

---

## Environnement de travail

| Element | Description |
|---------|-------------|
| **Systeme** | z/OS sous emulateur Hercules (TK4-) |
| **Interface** | TSO/ISPF, CICS |
| **Fichiers** | VSAM KSDS |
| **Langage** | COBOL avec commandes CICS |

**Libraries utilisees** :

| Library | Contenu |
|---------|---------|
| `ROCHA.CICS.SOURCE` | Programmes COBOL, MAPs BMS, JCL |
| `ROCHA.CICS.LINK` | Programmes objets (copybooks) |
| `ROCHA.CICS.LOAD` | Programmes executables |

---

## Demarche suivie

Le projet est organise en 3 parties et 19 exercices :

```
Partie 0 : Preparation
    └── Creation des libraries

Partie 1 : Affichage (READ)
    ├── Definition VSAM et integration CICS
    ├── MAP BMS d'affichage
    ├── Programme COBOL-CICS
    ├── Transaction CEDA
    └── Tests CEDF

Partie 2 : Operations CRUD
    ├── 2a : Ajout (WRITE)
    ├── 2b : Mise a jour (REWRITE)
    └── 2c : Suppression (DELETE)

Partie 3 : Operations avancees
    ├── Navigation VSAM (STARTBR, READNEXT)
    └── Statistiques par region
```

---

## Sommaire

| # | Fichier | Contenu |
|---|---------|---------|
| 00 | [Introduction](00-introduction.md) | Presentation du projet |
| 01 | [Partie 0 - Preparation](01-partie-0-preparation.md) | Exercice 0 : Creation des Libraries |
| 02 | [Partie 1 - Affichage](02-partie-1-affichage.md) | Exercices 1-5 : VSAM, MAP, READ |
| 03 | [Partie 2a - Ajout](03-partie-2a-ajout.md) | Exercices 6-8 : WRITE |
| 04 | [Partie 2b - Mise a jour](04-partie-2b-maj.md) | Exercices 9-11 : REWRITE |
| 05 | [Partie 2c - Suppression](05-partie-2c-suppression.md) | Exercices 12-15 : DELETE |
| 06 | [Partie 3 - Avancees](06-partie-3-avancees.md) | Exercices 16-19 : STARTBR, READNEXT |
| 07 | [Conclusion](07-conclusion.md) | Bilan, annexes, references |

---

[Partie 0 : Preparation >](01-partie-0-preparation.md)
