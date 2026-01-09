# Exercices DB2/SQL

Ce module contient les exercices et travaux pratiques pour DB2/SQL.

## Structure

```
db2/
├── README.md              # Ce fichier
│
├── theorie/               # QCM et questions conceptuelles
│   ├── qcm-01-fondamentaux.md
│   ├── qcm-02-architecture.md
│   ├── qcm-03-modelisation.md
│   ├── qcm-04-modele-relationnel.md
│   └── qcm-05-sql.md
│
├── tp/                    # Travaux Pratiques (formatrice)
│   ├── 00-schema/         # Scripts de création des tables
│   ├── activite-01-select/
│   ├── activite-02-selection-tri/
│   ├── activite-04-jointures/
│   ├── activite-05-operateurs-ensemblistes/
│   ├── activite-06-fonctions-groupe/
│   └── activite-07-sous-requetes/
│
├── pratique/              # Exercices complémentaires
│   ├── exercices-ddl.sql  # CREATE, ALTER, DROP
│   └── exercices-dml.sql  # INSERT, UPDATE, DELETE
│
└── fil-rouge/             # Mini-projet COBOL-DB2 (terminé)
    ├── RAPPORT-PROJET.md  # Rapport complet
    ├── RAPPORT-PROJET.pdf # Version PDF
    └── images-pt*/        # Captures d'écran
```

## Contenu détaillé

### Théorie (71 questions)

| QCM | Chapitre | Questions |
|-----|----------|-----------|
| [QCM 01](theorie/qcm-01-fondamentaux.md) | Fondamentaux BD | 15 |
| [QCM 02](theorie/qcm-02-architecture.md) | Architecture DB2 | 12 |
| [QCM 03](theorie/qcm-03-modelisation.md) | Modélisation | 12 |
| [QCM 04](theorie/qcm-04-modele-relationnel.md) | Modèle relationnel | 12 |
| [QCM 05](theorie/qcm-05-sql.md) | Langage SQL | 20 |

### Travaux Pratiques (56 exercices SQL)

| Activité | Thème | Questions |
|----------|-------|-----------|
| [Activité 1](tp/activite-01-select/) | SELECT élémentaire | 6 |
| [Activité 2](tp/activite-02-selection-tri/) | WHERE, ORDER BY | 12 |
| [Activité 4](tp/activite-04-jointures/) | Jointures | 11 |
| [Activité 5](tp/activite-05-operateurs-ensemblistes/) | UNION, INTERSECT, EXCEPT | 5 |
| [Activité 6](tp/activite-06-fonctions-groupe/) | GROUP BY, HAVING | 7 |
| [Activité 7](tp/activite-07-sous-requetes/) | Sous-requêtes | 15 |

### Pratique (17 exercices complémentaires)

| Fichier | Thème | Exercices |
|---------|-------|-----------|
| [exercices-ddl.sql](pratique/exercices-ddl.sql) | DDL (CREATE, ALTER, DROP) | 7 |
| [exercices-dml.sql](pratique/exercices-dml.sql) | DML + Transactions | 10 |

## Progression recommandée

```
1. THÉORIE
   QCM 01 → QCM 02 → QCM 03 → QCM 04 → QCM 05
   (Valider la compréhension des concepts)

2. PRATIQUE DDL/DML
   exercices-ddl.sql → exercices-dml.sql
   (Pratiquer CREATE, INSERT, UPDATE, DELETE)

3. TRAVAUX PRATIQUES
   Activité 1 → 2 → 4 → 5 → 6 → 7
   (Maîtriser SELECT et ses fonctionnalités)

4. FIL ROUGE ✅
   Mini-projet COBOL-DB2 complet (25 exercices)
   → Voir [fil-rouge/RAPPORT-PROJET.pdf](fil-rouge/RAPPORT-PROJET.pdf)
```

## Base de données de test

Les TP utilisent les tables EMPLOYEE, DEPT et SAL_GRILLE.
Scripts disponibles dans [tp/00-schema/](tp/00-schema/).

### Tables

| Table | Description | Lignes |
|-------|-------------|--------|
| DEPT | Départements | 4 |
| EMPLOYEE | Employés | 14 |
| SAL_GRILLE | Grille des salaires | 5 |

## Total

- **71 questions** théoriques (QCM)
- **56 exercices** SQL (TP)
- **17 exercices** complémentaires (DDL/DML)
- **25 exercices** fil-rouge COBOL-DB2
- **169 exercices** au total

---
*Formation DB2/SQL - M2i Formation, Strasbourg*
