# Exercices CICS

## Présentation

Ce dossier contient les exercices pratiques pour le module CICS.

## Structure

```
cics/
├── chapitre-06/           # TP Commande READ avec MAP
│   ├── bms/               # TESTSET.bms
│   ├── cobol/             # PROGTEST.cbl
│   └── copybooks/         # MAPTEST.cpy
│
└── tp-gestion-credits/    # TP complet : Architecture multicouches
    ├── copybooks/         # Structures de données
    ├── jcl/               # Définition et chargement VSAM
    ├── data/              # Données de test
    ├── bms/               # Écran BMS
    └── cobol/             # Programmes (3 couches)
```

## Travaux Pratiques disponibles

| TP | Titre | Description | Chapitres |
|----|-------|-------------|-----------|
| [chapitre-06](chapitre-06/) | Commande READ avec MAP | Lecture VSAM et affichage BMS | 06 |
| [tp-gestion-credits](tp-gestion-credits/) | Gestion des Crédits Employés | Application CICS multicouches avec VSAM | 04-08 |

## Prérequis

- Avoir complété les exercices du module COBOL
- Avoir lu les chapitres 01 à 08 du cours CICS
- Comprendre l'architecture multicouches

---

## TP Chapitre 06 - Commande READ

Ce TP met en pratique la **commande READ** vue au chapitre VI (Couche Traitement).

### Objectifs
- Utiliser la commande CICS READ pour lire un fichier KSDS
- Intégrer la lecture avec un écran BMS (MAP)
- Gérer les codes retour (RESP/RESP2)

### Fichiers fournis

| Type | Fichiers |
|------|----------|
| BMS | TESTSET.bms (contient MAPTEST) |
| COBOL | PROGTEST.cbl |
| Copybook | MAPTEST.cpy |

### Les 3 parties à intégrer
1. **WS-REC-DATA** : Structure de l'enregistrement EMPLOYE
2. **COPY MAPTEST** : Zone symbolique de la MAP
3. **EXEC CICS READ** : Lecture par clé avec gestion RESP

---

## TP Gestion des Crédits

Ce TP met en œuvre :

- **Architecture 3 tiers** : Présentation / Traitement / Données
- **Fichiers VSAM** : EMPLOYE et CRE-EMP (KSDS)
- **Écran BMS** : Consultation et paiement d'échéances
- **Commandes CICS** : READ, REWRITE, LINK, SEND MAP, RECEIVE MAP
- **Mode pseudo-conversationnel** : COMMAREA entre transactions

### Fichiers fournis

| Type | Fichiers |
|------|----------|
| Copybooks | EMPLOYE.cpy, CREDEMP.cpy |
| JCL | DEFVSAM.jcl, LOADDATA.jcl |
| Données | EMPLOYE.dat, CREDEMP.dat |
| BMS | CREDSET.bms |
| COBOL | CREDPRES.cbl, CREDTRT.cbl, CREDDAO.cbl |

---

**Navigation**
- [Cours CICS](../../cours/cics/README.md)
- [Chapitre VIII - Travaux Pratiques](../../cours/cics/08-travaux-pratiques.md)
