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

## Aller plus loin

### Documentation officielle IBM

| Ressource | Description |
|-----------|-------------|
| [CICS Transaction Server for z/OS](https://www.ibm.com/docs/en/cics-ts/6.1) | Documentation complète CICS TS |
| [CICS Application Programming Guide](https://www.ibm.com/docs/en/cics-ts/6.1?topic=programming-application-guide) | Guide de programmation CICS |
| [CICS Application Programming Reference](https://www.ibm.com/docs/en/cics-ts/6.1?topic=programming-application-reference) | Référence des commandes CICS |
| [CICS BMS Macro Reference](https://www.ibm.com/docs/en/cics-ts/6.1?topic=reference-bms-macros) | Référence BMS pour écrans |
| [CICS Resource Definition Guide](https://www.ibm.com/docs/en/cics-ts/6.1?topic=configuring-resource-definition-guide) | Définition des ressources (PCT, PPT, FCT) |

### Cours en ligne

| Plateforme | Cours | Niveau |
|------------|-------|--------|
| [IBM Skills](https://www.ibm.com/training/collection/cics) | CICS Fundamentals | Débutant |
| [Coursera - IBM](https://www.coursera.org/learn/cics-programming) | Introduction to CICS | Débutant |
| [Udemy](https://www.udemy.com/course/cics-tutorial-for-beginners/) | CICS Tutorial for Beginners | Débutant |
| [Udemy](https://www.udemy.com/course/mainframe-cics-complete-course/) | Mainframe CICS Complete Course | Tous niveaux |
| [Interskill](https://interskill.com/) | CICS Application Programming | Intermédiaire |

### Tutoriels gratuits

- [TutorialsPoint - CICS](https://www.tutorialspoint.com/cics/index.htm) - Tutoriel complet avec exemples
- [Mainframe Gurukul - CICS](https://www.mainframegurukul.com/tutorials/programming/cics/cics-tutorial.html) - Cours détaillé
- [IBMMainframer - CICS](https://www.ibmmainframer.com/cics-tutorial/) - Tutoriel orienté mainframe
- [Guru99 - CICS](https://www.guru99.com/cics-tutorial.html) - Introduction CICS pour débutants

### Livres recommandés

| Titre | Auteur | Description |
|-------|--------|-------------|
| *Murach's CICS for the COBOL Programmer* | Raul Menendez | Référence pédagogique complète |
| *CICS: A Practical Guide to System Fine Tuning* | Ernie Guerrera | Optimisation et tuning CICS |
| *CICS Application Design and Programming* | Phillipe Warin | Conception d'applications |

### Outils et extensions

| Outil | Description |
|-------|-------------|
| [IBM CICS Explorer](https://www.ibm.com/docs/en/cics-explorer) | IDE Eclipse pour développement CICS |
| [IBM Developer for z/OS](https://www.ibm.com/products/developer-for-zos) | Environnement de développement intégré |
| [VS Code Zowe Explorer](https://marketplace.visualstudio.com/items?itemName=Zowe.vscode-extension-for-zowe) | Extension VS Code pour mainframe |

### Commandes CICS - Références rapides

| Catégorie | Commandes |
|-----------|-----------|
| **Fichiers** | READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, ENDBR |
| **Écrans** | SEND MAP, RECEIVE MAP, SEND TEXT |
| **Programmes** | LINK, XCTL, RETURN |
| **Transactions** | SYNCPOINT, ROLLBACK |
| **Système** | ASKTIME, FORMATTIME, ASSIGN |

### Communautés

- [IBM Z Community - CICS](https://community.ibm.com/community/user/ibmz-and-linuxone/communities/community-home?communitykey=cics) - Forum officiel IBM
- [Stack Overflow - CICS](https://stackoverflow.com/questions/tagged/cics) - Questions/réponses
- [Planet Mainframe](https://planetmainframe.com/) - Blog et actualités mainframe

### Environnements de pratique

- [IBM Z Trial](https://www.ibm.com/z/trials) - Accès gratuit à un environnement z/OS avec CICS
- [IBM Z Xplore](https://www.ibm.com/it-infrastructure/z/education/zxplore) - Programme d'apprentissage gratuit IBM

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [COBOL](../cobol/README.md) | - |

---
*Formation COBOL - M2i Formation*
