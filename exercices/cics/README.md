# Exercices CICS

Ce module contient les exercices et travaux pratiques pour CICS.

## Structure

```
cics/
├── README.md                  # Ce fichier
│
├── theorie/                   # QCM et questions conceptuelles
│   ├── qcm-01-presentation.md
│   ├── qcm-02-organisation.md
│   ├── qcm-03-architecture.md
│   ├── qcm-04-presentation.md
│   ├── qcm-05-traitement.md
│   └── qcm-06-donnees.md
│
├── pratique/                  # Exercices pratiques
│   ├── bms/                   # Ecrans BMS
│   ├── cobol/                 # Programmes COBOL-CICS
│   ├── copybooks/             # Copybooks
│   └── jcl/                   # Jobs JCL
│
├── tp-gestion-credits/        # TP complet (Transaction CRED)
│   ├── cobol/
│   ├── copybooks/
│   ├── data/
│   └── jcl/
│
└── fil-rouge/                 # Projet fil rouge bancaire complet
    ├── cobol/                 # 7 programmes (MENU, CLNT, CPTE)
    ├── bms/                   # 3 mapsets
    ├── copybooks/             # CLIENT, COMPTE
    ├── jcl/                   # DEFVSAM, LOADDATA
    └── data/                  # 10 clients, 15 comptes
```

## Contenu detaille

### Theorie (127 questions)

| QCM | Chapitre | Questions |
|-----|----------|-----------|
| [QCM 01](theorie/qcm-01-presentation.md) | Presentation generale | 10 |
| [QCM 02](theorie/qcm-02-organisation.md) | Organisation systeme | 12 |
| [QCM 03](theorie/qcm-03-architecture.md) | Architecture multicouches | 15 |
| [QCM 04](theorie/qcm-04-presentation.md) | Couche Presentation (BMS) | 15 |
| [QCM 05](theorie/qcm-05-traitement.md) | Couche Traitement | 15 |
| [QCM 06](theorie/qcm-06-donnees.md) | Couche Donnees | 15 |
| [QCM 08](theorie/qcm-08-travaux-pratiques.md) | Travaux Pratiques | 20 |
| [QCM 09](theorie/qcm-09-architecture-tsi.md) | Architecture et Transactions TSI | 25 |

### Pratique (10 exercices)

| Theme | Exercices | Description |
|-------|-----------|-------------|
| BMS | 2 | MAPTEST, TESTSET (ecrans) |
| COBOL | 4 | PROGREAD, PROGWRIT, PROGREWT, PROGDELT |
| JCL | 4 | Assemblage MAP, Compilation, VSAM |

Voir [pratique/README.md](pratique/README.md) pour les details.

### TP Gestion Credits (1 TP complet)

Application CICS complete en **architecture 3 tiers** pour la gestion des credits employes.

| Composant | Description |
|-----------|-------------|
| Transaction | CRED |
| Programmes | CREDPRES (Presentation), CREDTRT (Traitement), CREDDAO (Donnees) |
| Fichiers VSAM | EMPLOYE (6 enr.), CREDEMP (4 enr.) |
| Ecran BMS | CREDSET/CREDMAP |

**Fonctionnalites :**
- Consulter un employe et son credit
- Payer une echeance (PF5)
- Solde automatique du credit (RESTE = 0 → ETAT = 'N')

Voir [tp-gestion-credits/README.md](tp-gestion-credits/README.md) pour les details.

### Fil Rouge Bancaire (1 application complete)

Application bancaire CICS multi-transactions en **architecture 3 tiers**.

| Transaction | Programmes | Fonction |
|-------------|------------|----------|
| MENU | MENUPRES | Menu principal, navigation |
| CLNT | CLNTPRES, CLNTTRT, CLNTDAO | Consultation client |
| CPTE | CPTEPRES, CPTETRT, CPTEDAO | Gestion comptes avec browse |

**Donnees :**
- 10 clients (CLI001-CLI010)
- 15 comptes (Courant, Epargne, Titres)

**Concepts avances :**
- Navigation multi-transactions (XCTL)
- Browse VSAM (STARTBR, READNEXT, READPREV, ENDBR)
- 3 mapsets BMS distincts

Voir [fil-rouge/README.md](fil-rouge/README.md) pour les details.

## Progression recommandee

```
1. THEORIE (Partie 1)
   QCM 01 → QCM 02 → QCM 03 → QCM 04 → QCM 05 → QCM 06
   (Valider la comprehension des concepts fondamentaux)

2. PRATIQUE BMS
   MAPTEST → Comprendre les ecrans CICS
   (Maitriser SEND MAP / RECEIVE MAP)

3. PRATIQUE VSAM
   PROGREAD → PROGWRIT → PROGREWT → PROGDELT
   (Maitriser les commandes fichier)

4. TP GESTION CREDITS + QCM 08
   Application 3-tiers complete
   (Integration de tous les concepts)

5. THEORIE (Partie 2)
   QCM 09 → Architecture et Transactions TSI
   (Concepts avances : accesseurs, TSI, web, administration)

6. FIL ROUGE BANCAIRE
   Application multi-transactions (MENU, CLNT, CPTE)
   (Navigation XCTL, Browse VSAM, 3 mapsets)
```

## Outils CICS utiles

| Transaction | Description |
|-------------|-------------|
| CEDF | Debugger interactif |
| CEDA | Definition de ressources |
| CEMT | Administration |
| CEBR | Browse des TS Queues |

## Prerequis

- Avoir complete les exercices du module COBOL
- Avoir lu les chapitres 01 a 07 du cours CICS
- Comprendre les commandes CICS de base

## Navigation

- [Cours CICS](../../cours/cics/)
- [Chapitre VIII - Travaux Pratiques](../../cours/cics/08-travaux-pratiques.md)

## Total

- **127 questions** theoriques (8 QCM)
- **10 exercices** pratiques
- **1 TP** complet architecture 3 tiers (CRED)
- **1 Fil Rouge** bancaire multi-transactions (MENU, CLNT, CPTE)
- **7 programmes COBOL** + **3 mapsets BMS** dans le fil rouge

---
*Formation CICS - M2i Formation*
