# Exercices CICS

## Présentation

Ce dossier contient les exercices pratiques pour le module CICS.

## Structure

```
cics/
├── tp-gestion-credits/    # TP complet : Architecture multicouches
│   ├── copybooks/         # Structures de données
│   ├── jcl/               # Définition et chargement VSAM
│   ├── data/              # Données de test
│   ├── bms/               # Écran BMS
│   └── cobol/             # Programmes (3 couches)
```

## Travaux Pratiques disponibles

| TP | Titre | Description | Chapitres |
|----|-------|-------------|-----------|
| [tp-gestion-credits](tp-gestion-credits/) | Gestion des Crédits Employés | Application CICS multicouches avec VSAM | 04-08 |

## Prérequis

- Avoir complété les exercices du module COBOL
- Avoir lu les chapitres 01 à 08 du cours CICS
- Comprendre l'architecture multicouches

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
