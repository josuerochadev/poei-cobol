# Exercices CICS

## Présentation

Ce dossier contient les exercices pratiques pour le module CICS, organisés par chapitre.

## Structure

```
cics/
├── chapitre-05/           # TP Couche Présentation (BMS, CEDF, CEDA)
│   ├── README.md
│   └── ...
│
├── chapitre-06/           # TP Couche Traitement (READ, WRITE, REWRITE, DELETE)
│   ├── bms/               # TESTSET.bms
│   ├── cobol/             # PROGREAD, PROGWRIT, PROGREWT, PROGDELT
│   ├── copybooks/         # MAPTEST.cpy
│   └── README.md
│
├── chapitre-07/           # TP Couche Données (Browse, Transactions)
│   ├── README.md
│   └── ...
│
└── tp-gestion-credits/    # TP complet : Architecture multicouches
    ├── copybooks/         # Structures de données
    ├── jcl/               # Définition et chargement VSAM
    ├── data/              # Données de test
    ├── bms/               # Écran BMS
    └── cobol/             # Programmes (3 couches)
```

## Travaux Pratiques disponibles

| TP | Titre | Description | Chapitres cours |
|----|-------|-------------|-----------------|
| [chapitre-05](chapitre-05/) | Couche Présentation | BMS, CEDF, installation CEDA | 05 |
| [chapitre-06](chapitre-06/) | Couche Traitement | READ, WRITE, REWRITE, DELETE | 06 |
| [chapitre-07](chapitre-07/) | Couche Données | Browse VSAM (STARTBR, READNEXT), SYNCPOINT | 07 |
| [tp-gestion-credits](tp-gestion-credits/) | Gestion des Crédits | Application CICS multicouches complète | 05-07 |

---

## Exercices de compréhension (Chapitres 1-4)

### Chapitre I - Présentation générale

1. **Question** : Quelle est la différence entre le traitement BATCH et ONLINE ?
2. **Question** : Qu'est-ce qu'une LUW (Logical Unit of Work) ?
3. **Question** : Expliquez le concept de pseudo-conversation.
4. **Question** : Combien de caractères contient un écran 3270 standard (24x80) ?

### Chapitre II - Organisation du système

1. **Question** : Citez les 5 Control Programs principaux de CICS et leur rôle.
2. **Question** : Quelle table CICS contient la définition des transactions ?
3. **Question** : Qu'est-ce que la TIOA et quel est son rôle ?
4. **Question** : Décrivez les 5 méthodes de lancement d'une transaction.
5. **Question** : Quels champs de l'EIB permettent d'identifier la transaction et le terminal ?

### Chapitre III - SGBD IMS

1. **Question** : Quelle est la différence entre un SGBD hiérarchique et relationnel ?
2. **Question** : Dans IMS, qu'est-ce qu'un segment ?
3. **Question** : Citez 2 avantages et 2 inconvénients du modèle hiérarchique.
4. **Question** : Qu'est-ce que l'IMS Transaction Manager (IMS TM) ?

### Chapitre IV - Programmation CICS

1. **Question** : Quelle est la syntaxe générale d'une commande CICS ?
2. **Question** : À quoi servent les options RESP et RESP2 ?
3. **Question** : Que se passe-t-il si on ne gère pas les codes retour ?
4. **Question** : Quelle est la constante COBOL pour un code retour normal ?

---

## TP Chapitre 05 - Couche Présentation

Ce TP met en pratique la création d'écrans BMS et l'utilisation des outils CEDF et CEDA.

### Objectifs

- Créer un MAPSET BMS simple
- Utiliser CEDF pour déboguer un programme
- Installer une application avec CEDA

### Exercices

1. **Exercice 1** : Créer une MAP d'accueil avec message "BIENVENUE"
2. **Exercice 2** : Utiliser CEDF pour tracer l'exécution d'une transaction
3. **Exercice 3** : Installer un programme et une transaction avec CEDA

→ [Voir les détails](chapitre-05/)

---

## TP Chapitre 06 - Couche Traitement

Ce TP met en pratique les 4 commandes CICS de manipulation de fichiers VSAM.

### Objectifs

- Maîtriser READ, WRITE, REWRITE, DELETE
- Gérer les codes retour (RESP/RESP2)
- Intégrer présentation (BMS) et accès données (VSAM)

### Exercices

| Programme | Transaction | Commande | Description |
|-----------|-------------|----------|-------------|
| PROGREAD | READ | READ | Lecture par clé |
| PROGWRIT | WRIT | WRITE | Création d'enregistrement |
| PROGREWT | REWT | REWRITE | Mise à jour |
| PROGDELT | DELT | DELETE | Suppression |

→ [Voir les détails](chapitre-06/)

---

## TP Chapitre 07 - Couche Données

Ce TP met en pratique le parcours de fichiers VSAM et la gestion des transactions.

### Objectifs

- Maîtriser STARTBR, READNEXT, READPREV, ENDBR
- Comprendre SYNCPOINT et ROLLBACK
- Implémenter un parcours séquentiel avec affichage paginé

### Exercices

1. **Exercice 1** : Parcours complet d'un fichier VSAM
2. **Exercice 2** : Affichage paginé (10 enregistrements par page)
3. **Exercice 3** : Transaction avec SYNCPOINT (virement entre comptes)

→ [Voir les détails](chapitre-07/)

---

## TP Complet - Gestion des Crédits

Application CICS complète utilisant l'architecture multicouches (3 tiers).

### Fonctionnalités

- Consultation d'un employé
- Affichage des détails de son crédit
- Paiement d'une échéance
- Solde automatique du crédit

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE 3-TIERS                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │  CREDPRES.cbl (Présentation)                            │   │
│  │  • Écran BMS CREDSET/CREDMAP                           │   │
│  │  • Gestion touches PF                                   │   │
│  └─────────────────────────────┬───────────────────────────┘   │
│                                │ LINK                           │
│  ┌─────────────────────────────▼───────────────────────────┐   │
│  │  CREDTRT.cbl (Traitement)                               │   │
│  │  • Logique métier                                       │   │
│  │  • Calculs et validations                              │   │
│  └─────────────────────────────┬───────────────────────────┘   │
│                                │ LINK                           │
│  ┌─────────────────────────────▼───────────────────────────┐   │
│  │  CREDDAO.cbl (Données)                                  │   │
│  │  • READ/REWRITE VSAM                                   │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

→ [Voir les détails](tp-gestion-credits/)

---

## Prérequis

- Avoir complété les exercices du module COBOL
- Avoir lu les chapitres 01 à 07 du cours CICS
- Comprendre l'architecture multicouches

## Navigation

- [Cours CICS](../../cours/cics/)
- [Chapitre VIII - Travaux Pratiques](../../cours/cics/08-travaux-pratiques.md)

---
*Formation COBOL - Module CICS*
