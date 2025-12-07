# Module CICS - Customer Information Control System

## Introduction

CICS (Customer Information Control System) est un moniteur transactionnel développé par IBM. Il permet de gérer des transactions en temps réel sur les systèmes mainframe, offrant une interface entre les utilisateurs et les applications COBOL.

## Prérequis

- Maîtrise du langage COBOL (module cours/cobol/)
- Connaissance de l'environnement mainframe z/OS
- Familiarité avec ISPF et JCL

## Table des matières

### Partie 1 : Fondamentaux CICS

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 01 | [Présentation générale](01-presentation-generale.md) | Introduction à CICS, historique et concepts fondamentaux |
| 02 | [Organisation du système](02-organisation-systeme.md) | Composants, transactions, bloc EIB, commandes CICS |
| 03 | [SGBD IMS](03-sgbd-ims.md) | Caractéristiques et utilisation du SGBD IMS avec CICS |

### Partie 2 : Architecture Multicouches et Transactions TSI

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 04 | [Architecture Multicouches](04-architecture-multicouches.md) | Présentation et structure de l'architecture 3 tiers |
| 05 | [Couche de Présentation](05-couche-presentation.md) | Interface utilisateur, BMS, écrans 3270 |
| 06 | [Couche de Traitement](06-couche-traitement.md) | Logique métier et règles de gestion |
| 07 | [Couche des Données](07-couche-donnees.md) | Accès VSAM, DB2, IMS |
| 08 | [Travaux Pratiques](08-travaux-pratiques.md) | Application complète gestion des crédits |

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

### Fondamentaux CICS
- Comprendre l'architecture et le fonctionnement de CICS
- Identifier les composants principaux du système
- Maîtriser le déroulement d'une transaction CICS
- Utiliser le bloc EIB (Execute Interface Block)
- Écrire des commandes CICS dans un programme COBOL
- Interagir avec le SGBD IMS

### Architecture Multicouches
- Concevoir une application CICS en architecture 3 tiers
- Séparer les responsabilités entre couches (Présentation/Traitement/Données)
- Utiliser les transactions TSI pour les échanges front/back
- Créer des écrans BMS dans la couche de présentation
- Implémenter la logique métier dans la couche de traitement
- Accéder aux données VSAM et DB2 dans la couche des données

## Liens avec le module COBOL

Les programmes CICS sont écrits en COBOL avec des extensions spécifiques :

```cobol
       EXEC CICS
           commande CICS
       END-EXEC.
```

Les connaissances acquises dans le module COBOL (structures de données, fichiers, sous-programmes) sont directement applicables dans le contexte CICS.

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [COBOL](../cobol/README.md) | - |

---
*Formation COBOL - M2i Formation*
