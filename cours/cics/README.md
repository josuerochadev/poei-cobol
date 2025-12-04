# Module CICS - Customer Information Control System

## Introduction

CICS (Customer Information Control System) est un moniteur transactionnel développé par IBM. Il permet de gérer des transactions en temps réel sur les systèmes mainframe, offrant une interface entre les utilisateurs et les applications COBOL.

## Prérequis

- Maîtrise du langage COBOL (module cours/cobol/)
- Connaissance de l'environnement mainframe z/OS
- Familiarité avec ISPF et JCL

## Table des matières

| Chapitre | Titre | Description |
|----------|-------|-------------|
| 01 | [Présentation générale](01-presentation-generale.md) | Introduction à CICS, historique et concepts fondamentaux |
| 02 | [Organisation du système](02-organisation-systeme.md) | Composants, transactions, bloc EIB, commandes CICS |
| 03 | [SGBD IMS](03-sgbd-ims.md) | Caractéristiques et utilisation du SGBD IMS avec CICS |

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

- Comprendre l'architecture et le fonctionnement de CICS
- Identifier les composants principaux du système
- Maîtriser le déroulement d'une transaction CICS
- Utiliser le bloc EIB (Execute Interface Block)
- Écrire des commandes CICS dans un programme COBOL
- Interagir avec le SGBD IMS

## Liens avec le module COBOL

Les programmes CICS sont écrits en COBOL avec des extensions spécifiques :

```cobol
       EXEC CICS
           commande CICS
       END-EXEC.
```

Les connaissances acquises dans le module COBOL (structures de données, fichiers, sous-programmes) sont directement applicables dans le contexte CICS.
