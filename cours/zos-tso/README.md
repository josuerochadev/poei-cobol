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
| `05-jcl-base.md` | V | JCL, JOB, EXEC, DD, utilitaires *(à venir)* |
| `06-vsam.md` | VI | VSAM, KSDS, ESDS, RRDS, IDCAMS *(à venir)* |

## Objectifs pédagogiques

À l'issue de ce module, vous serez capable de :

- Comprendre l'architecture et les composants de z/OS
- Naviguer dans l'environnement TSO/ISPF
- Créer et gérer des datasets (fichiers)
- Écrire des JCL de base
- Comprendre les organisations de fichiers VSAM

## Progression

- [x] Chapitre I - Présentation générale de Z/OS
- [x] Chapitre II - Fonctionnement de Z/OS
- [x] Chapitre III - TSO
- [x] Chapitre IV - ISPF/PDF
- [ ] Chapitre V - JCL de base
- [ ] Chapitre VI - VSAM

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
│   │   Z/OS      │  ◄── VOUS ÊTES ICI                   │
│   │  TSO/ISPF   │                                       │
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
