# Fil Rouge - DB2/SQL

Mini-Projet COBOL-DB2 : Suivi Clientele Secteur Financier

## Structure

```
fil-rouge/
├── p1-ex01-creation-tables/
├── p1-ex02-alimentation/
├── p1-ex03-verification/
├── p2-ex01-extraction-profession/
└── ...
```

## Schema

| Table    | Cle primaire  | Description         |
|----------|---------------|---------------------|
| REGION   | CODE_REGION   | 4 regions           |
| NATCOMPT | CODE_NATCPT   | 5 natures de compte |
| PROFESSI | CODE_PROF     | 6 professions       |
| CLIENT   | NUM_COMPTE    | 20 clients (FK)     |

> Note: `POS` remplace `POSITION` (mot reserve COBOL)

## Partie 1 : Creation et chargement

| Exo | Description              | Statut |
|-----|--------------------------|--------|
| 1   | Creation des tables      | OK     |
| 2   | Alimentation (INSERT)    | OK     |
| 3   | Verification (SELECT)    | OK     |

## Partie 2 : Exploitation SQL

| Exo | Description                          | Statut |
|-----|--------------------------------------|--------|
| 1   | Extraction par profession            | OK     |
| 2   | Repartition DB/CR                    | OK     |
| 3   | Repartition par region               | OK     |
| 4   | Index sur CODE_REGION                | OK     |
| 5   | Index sur CODE_PROF                  | OK     |
| 6   | Tri region/profession/num_compte     | OK     |
| 7   | Fusion COMPTABLES + FONCTIONNAIRES   | OK     |
| 8   | Vue CLIENT reduit                    | OK     |
| 9   | Analyse multi-criteres               | OK     |
| 10  | Clients anormalement debiteurs       | OK     |

## Partie 3 : Programmation COBOL-DB2

| Exo | Description                          | Statut |
|-----|--------------------------------------|--------|
| 1   | Afficher region Marseille            | TODO   |
| 2   | Inserer nouveau client               | TODO   |
| 3   | Afficher clients Marseille           | TODO   |
| 4   | Mise a jour client                   | TODO   |
| 5   | Liste tries avec ruptures            | TODO   |
| 6   | Statistiques DB/CR                   | TODO   |
| 7   | Totaux par region (niveau 88)        | TODO   |
| 8   | Creation table MOUVEMENT             | TODO   |
| 9   | Total mouvements client              | TODO   |
| 10  | Releve de compte                     | TODO   |
| 11  | Mouvements 2024                      | TODO   |
| 12  | Releve compte specifique             | TODO   |
