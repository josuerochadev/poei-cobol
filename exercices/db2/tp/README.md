# Travaux Pratiques DB2/SQL

Ce dossier contient les travaux pratiques SQL basés sur les activités de la formatrice.

## Prérequis

Avant de commencer, exécutez le script de création des tables :
```sql
-- Exécuter dans SPUFI ou votre client SQL
@00-schema/création-tables.sql
```

## Activités

| Activité | Thème | Questions |
|----------|-------|-----------|
| [00-schema](00-schema/) | Création des tables | DDL + INSERT |
| [activite-01-select](activite-01-select/) | SELECT élémentaire | 6 |
| [activite-02-selection-tri](activite-02-selection-tri/) | Sélection et tri (WHERE, ORDER BY) | 12 |
| [activite-04-jointures](activite-04-jointures/) | Jointures (INNER, LEFT, Self-join) | 11 |
| [activite-05-operateurs-ensemblistes](activite-05-operateurs-ensemblistes/) | UNION, INTERSECT, EXCEPT | 5 |
| [activite-06-fonctions-groupe](activite-06-fonctions-groupe/) | COUNT, SUM, AVG, GROUP BY, HAVING | 7 |
| [activite-07-sous-requetes](activite-07-sous-requetes/) | Sous-requêtes, IN, ANY, ALL, EXISTS | 15 |

**Total : 56 exercices**

## Organisation des dossiers

Chaque activité contient :
```
activite-XX/
├── README.md       # Énoncés des questions
└── solutions.sql   # Solutions SQL commentées
```

## Méthode de travail recommandée

1. Lire l'énoncé dans README.md
2. Ecrire votre requête
3. Tester dans SPUFI ou votre client SQL
4. Comparer avec la solution dans solutions.sql

## Ordre de progression

```
Activité 1 → Activité 2 → Activité 4 → Activité 5 → Activité 6 → Activité 7
   SELECT      WHERE        JOIN        UNION        GROUP BY    Sous-requêtes
              ORDER BY                  EXCEPT       HAVING      EXISTS
```

Note : L'activité 3 (exercice sur les fonctions scalaires) n'est pas incluse dans ce TP.

---
*Formation DB2/SQL - M2i Formation*
