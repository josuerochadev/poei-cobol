# Module Z/OS et TSO/ISPF

## Présentation

Ce module présente le système d'exploitation z/OS d'IBM et son environnement de développement TSO/ISPF. Il constitue le **prérequis fondamental** pour la compréhension de l'environnement mainframe.

## Chapitres disponibles

| Fichier | Chapitre | Contenu |
|---------|----------|---------|
| `01-presentation-zos.md` | I | Historique, architecture, terminologie IBM |
| `02-fonctionnement-zos.md` | II | Mémoire virtuelle, DAT, tâches, travaux, données |
| `03-tso.md` | III | TSO, LOGON/LOGOFF, commandes Data Management et Jobs |
| `04-ispf.md` | IV | ISPF/PDF, panels, View/Edit, Utilities, SDSF, éditeur |
| `05-architecture-zos.md` | V | Architecture Z/OS : SYSPLEX, SMS, RACF |

> **Note** : Le JCL et VSAM font maintenant l'objet de modules dédiés séparés.

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

- Comprendre l'architecture et les composants de z/OS
- Naviguer dans l'environnement TSO/ISPF
- Créer et gérer des datasets (fichiers)
- Comprendre les concepts de SYSPLEX, SMS et RACF

## Progression

- [x] Chapitre I - Présentation générale de Z/OS
- [x] Chapitre II - Fonctionnement de Z/OS
- [x] Chapitre III - TSO
- [x] Chapitre IV - ISPF/PDF
- [x] Chapitre V - Architecture Z/OS (SYSPLEX, SMS, RACF)

**Module terminé !** Passez au module JCL.

## Prérequis

- Aucun prérequis technique
- Curiosité pour les systèmes d'information d'entreprise

## Lien avec les autres modules

```
┌─────────────────────────────────────────────────────────┐
│                PARCOURS DE FORMATION                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│   ┌─────────────┐                                       │
│   │   Z/OS      │  ◄── MODULE TERMINÉ                  │
│   │  TSO/ISPF   │                                       │
│   └──────┬──────┘                                       │
│          │                                              │
│          ▼                                              │
│   ┌─────────────┐                                       │
│   │    JCL      │  Job Control Language                │
│   │             │                                       │
│   └──────┬──────┘                                       │
│          │                                              │
│          ▼                                              │
│   ┌─────────────┐                                       │
│   │   COBOL     │  Langage de programmation            │
│   │             │                                       │
│   └──────┬──────┘                                       │
│          │                                              │
│          ▼                                              │
│   ┌─────────────┐                                       │
│   │   CICS      │  Transactionnel temps réel           │
│   │             │                                       │
│   └─────────────┘                                       │
│                                                         │
└─────────────────────────────────────────────────────────┘
```
