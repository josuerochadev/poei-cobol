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
└── fil-rouge/                 # Projet fil rouge (a venir)
```

## Contenu detaille

### Theorie (82 questions)

| QCM | Chapitre | Questions |
|-----|----------|-----------|
| [QCM 01](theorie/qcm-01-presentation.md) | Presentation generale | 10 |
| [QCM 02](theorie/qcm-02-organisation.md) | Organisation systeme | 12 |
| [QCM 03](theorie/qcm-03-architecture.md) | Architecture multicouches | 15 |
| [QCM 04](theorie/qcm-04-presentation.md) | Couche Presentation (BMS) | 15 |
| [QCM 05](theorie/qcm-05-traitement.md) | Couche Traitement | 15 |
| [QCM 06](theorie/qcm-06-donnees.md) | Couche Donnees | 15 |

### Pratique (10 exercices)

| Theme | Exercices | Description |
|-------|-----------|-------------|
| BMS | 2 | MAPTEST, TESTSET (ecrans) |
| COBOL | 4 | PROGREAD, PROGWRIT, PROGREWT, PROGDELT |
| JCL | 4 | Assemblage MAP, Compilation, VSAM |

Voir [pratique/README.md](pratique/README.md) pour les details.

### TP Gestion Credits (1 TP)

Application CICS simplifiee demonstrant l'utilisation de READ/REWRITE avec SEND TEXT.

| Composant | Description |
|-----------|-------------|
| Transaction | CRED |
| Programme | PROGCRED.cbl |
| Fichiers VSAM | EMPLOYE, CRE-EMP |

Voir [tp-gestion-credits/README.md](tp-gestion-credits/README.md) pour les details.

## Progression recommandee

```
1. THEORIE
   QCM 01 → QCM 02 → QCM 03 → QCM 04 → QCM 05 → QCM 06
   (Valider la comprehension des concepts)

2. PRATIQUE BMS
   MAPTEST → Comprendre les ecrans CICS
   (Maitriser SEND MAP / RECEIVE MAP)

3. PRATIQUE VSAM
   PROGREAD → PROGWRIT → PROGREWT → PROGDELT
   (Maitriser les commandes fichier)

4. TP GESTION CREDITS
   Application complete avec SEND TEXT
   (Integration de tous les concepts)

5. FIL ROUGE
   Application complete (a venir)
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

- **82 questions** theoriques (QCM)
- **10 exercices** pratiques
- **1 TP** complet (CRED)
- **93 exercices** au total

---
*Formation CICS - M2i Formation*
