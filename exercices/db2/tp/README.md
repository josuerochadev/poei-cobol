# Travaux Pratiques DB2/SQL

Ce dossier contient les travaux pratiques SQL bases sur les activites de la formatrice.

## Prerequis

Avant de commencer, executez le script de creation des tables :
```sql
-- Executer dans SPUFI ou votre client SQL
@00-schema/creation-tables.sql
```

## Activites

| Activite | Theme | Questions |
|----------|-------|-----------|
| [00-schema](00-schema/) | Creation des tables | DDL + INSERT |
| [activite-01-select](activite-01-select/) | SELECT elementaire | 6 |
| [activite-02-selection-tri](activite-02-selection-tri/) | Selection et tri (WHERE, ORDER BY) | 12 |
| [activite-04-jointures](activite-04-jointures/) | Jointures (INNER, LEFT, Self-join) | 11 |
| [activite-05-operateurs-ensemblistes](activite-05-operateurs-ensemblistes/) | UNION, INTERSECT, EXCEPT | 5 |
| [activite-06-fonctions-groupe](activite-06-fonctions-groupe/) | COUNT, SUM, AVG, GROUP BY, HAVING | 7 |
| [activite-07-sous-requetes](activite-07-sous-requetes/) | Sous-requetes, IN, ANY, ALL, EXISTS | 15 |

**Total : 56 exercices**

## Organisation des dossiers

Chaque activite contient :
```
activite-XX/
├── README.md       # Enonces des questions
└── solutions.sql   # Solutions SQL commentees
```

## Methode de travail recommandee

1. Lire l'enonce dans README.md
2. Ecrire votre requete
3. Tester dans SPUFI ou votre client SQL
4. Comparer avec la solution dans solutions.sql

## Ordre de progression

```
Activite 1 → Activite 2 → Activite 4 → Activite 5 → Activite 6 → Activite 7
   SELECT      WHERE        JOIN        UNION        GROUP BY    Sous-requetes
              ORDER BY                  EXCEPT       HAVING      EXISTS
```

Note : L'activite 3 (exercice sur les fonctions scalaires) n'est pas incluse dans ce TP.

---
*Formation DB2/SQL - M2i Formation*
