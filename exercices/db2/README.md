# Exercices DB2/SQL - Interrogation d'une base de donnees

Ce dossier contient les travaux pratiques SQL pour interroger une base de donnees relationnelle.

## Contexte

L'entreprise **Humania Services** souhaite mettre en place une base de donnees pour gerer :
- Les employes
- Les departements
- La grille des salaires selon le grade

## Structure de la base de donnees

```
┌─────────────────────────────────────────────────────────────────┐
│                    SCHEMA DE LA BASE                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  DEPT                    EMPLOYEE                SAL_GRILLE     │
│  ┌──────────────┐        ┌──────────────┐        ┌───────────┐  │
│  │ DEPT_NUM (PK)│◄───────│ DEPT_NUM (FK)│        │ GRADE (PK)│  │
│  │ DEPT_NOM     │        │ EMP_NUM (PK) │        │ MIN_SAL   │  │
│  │ LOC          │        │ EMP_NOM      │        │ MAX_SAL   │  │
│  └──────────────┘        │ POSTE        │        └───────────┘  │
│                          │ DIR (FK auto)│                       │
│                          │ DATE_EMB     │                       │
│                          │ SAL          │                       │
│                          │ COMM         │                       │
│                          └──────────────┘                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Fichiers disponibles

| Fichier | Description | Exercices |
|---------|-------------|-----------|
| [00-creation-tables.sql](00-creation-tables.sql) | Scripts DDL et INSERT | - |
| [Activite 1](activite-01-select-elementaire.md) | SELECT elementaire | 6 questions |
| [Activite 2](activite-02-selection-tri.md) | Selection et tri (WHERE, ORDER BY) | 12 questions |
| [Activite 4](activite-04-jointures.md) | Jointures (JOIN, LEFT JOIN, Self-join) | 11 questions |
| [Activite 5](activite-05-operateurs-ensemblistes.md) | Operateurs ensemblistes (UNION, INTERSECT, EXCEPT) | 5 questions |
| [Activite 6](activite-06-fonctions-groupe.md) | Fonctions de groupe (COUNT, SUM, AVG, GROUP BY, HAVING) | 7 questions |

**Total : 41 exercices corriges**

## Donnees de test

### Table DEPT (4 lignes)

| DEPT_NUM | DEPT_NOM | LOC |
|----------|----------|-----|
| 10 | COMPTABILITE | MARSEILLE |
| 20 | RECHERCHE | STRASBOURG |
| 30 | VENTES | LYON |
| 40 | EXPLOITATION | PARIS |

### Table EMPLOYEE (14 lignes)

| EMP_NUM | EMP_NOM | POSTE | DIR | SAL | DEPT_NUM |
|---------|---------|-------|-----|-----|----------|
| 7369 | ARTHUR | AGENT | 7902 | 800.00 | 20 |
| 7499 | PAUL | VENDEUR | 7698 | 1600.00 | 30 |
| 7521 | JEAN | VENDEUR | 7698 | 1250.00 | 30 |
| ... | ... | ... | ... | ... | ... |
| 7839 | HENRI | PDG | NULL | 5000.00 | 10 |

### Table SAL_GRILLE (5 lignes)

| GRADE | MIN_SAL | MAX_SAL |
|-------|---------|---------|
| 1 | 700.00 | 1200.00 |
| 2 | 1201.00 | 1400.00 |
| 3 | 1401.00 | 2000.00 |
| 4 | 2001.00 | 3000.00 |
| 5 | 3001.00 | 9999.00 |

## Utilisation sur z/OS

### Configuration SPUFI

1. Creer les datasets :
   - `userid.DB2.ISPUFI` (input) : FB, LRECL=80, BLKSIZE=3120
   - `userid.DB2.OSPUFI` (output) : VB, LRECL=4092, BLKSIZE=4096

2. Acceder a SPUFI : `=M.DB2` puis option 1

3. Configurer :
   ```
   1 DATA SET NAME ... ===> 'userid.DB2.ISPUFI'
   4 DATA SET NAME ... ===> 'userid.DB2.OSPUFI'
   6 EDIT INPUT  ... ===> YES
   7 EXECUTE     ... ===> YES
   8 AUTOCOMMIT  ... ===> YES
   9 BROWSE OUTPUT .. ===> YES
   ```

## Competences couvertes

- SELECT, FROM, WHERE
- Operateurs : =, <>, <, >, BETWEEN, IN, LIKE, IS NULL
- Alias de colonnes et de tables
- Concatenation (||)
- ORDER BY (ASC, DESC)
- DISTINCT
- INNER JOIN, LEFT OUTER JOIN, RIGHT OUTER JOIN
- Auto-jointure (self-join)
- UNION, INTERSECT, EXCEPT
- COUNT, SUM, AVG, MIN, MAX
- GROUP BY, HAVING
- Sous-requetes

---
*Formation DB2/SQL - M2i Formation*
