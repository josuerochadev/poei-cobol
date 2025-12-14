# Activité 1 - L'ordre SELECT élémentaire

## Objectifs

- Maîtriser la syntaxe de base du SELECT
- Utiliser les alias de colonnes
- Concaténer des colonnes
- Éliminer les doublons avec DISTINCT

---

## Question 1

Affichez toutes les données de la table EMPLOYEE.

<details>
<summary>Correction</summary>

```sql
SELECT * FROM EMPLOYEE;
```

**Résultat** : 14 lignes affichées

</details>

---

## Question 2

Créez une requête pour afficher le nom (EMP_NOM), le poste (POSTE), la date d'embauche (DATE_EMB) et le matricule (EMP_NUM) de chaque employé, **en plaçant le matricule en premier**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NUM, EMP_NOM, POSTE, DATE_EMB
FROM EMPLOYEE;
```

**Résultat** :
```
EMP_NUM  EMP_NOM    POSTE       DATE_EMB
-------  ---------  ---------   ----------
7369     ARTHUR     AGENT       2020-12-17
7499     PAUL       VENDEUR     2021-02-20
7521     JEAN       VENDEUR     2021-02-22
...
```

</details>

---

## Question 3

Créez une requête pour afficher les **différents types de poste** existant dans la table EMPLOYEE.

<details>
<summary>Correction</summary>

```sql
SELECT DISTINCT POSTE
FROM EMPLOYEE;
```

**Résultat** : 5 postes distincts
```
POSTE
---------
AGENT
ANALYSTE
DIRECTEUR
PDG
VENDEUR
```

</details>

---

## Question 4

Utilisez la requête (2) et donnez respectivement les noms suivants aux en-têtes de colonne :
- N° Emp.
- Employés
- Poste
- Date embauche

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NUM "N° EMP.",
       EMP_NOM "EMPLOYÉS",
       POSTE POSTE,
       DATE_EMB "DATE EMBAUCHE"
FROM EMPLOYEE;
```

**Note** : Les guillemets doubles permettent d'utiliser des espaces et caractères spéciaux dans les alias.

</details>

---

## Question 5

Affichez le nom concaténé avec le poste en les séparant par une virgule suivie d'un espace, puis donnez comme titre à la colonne **'Employés et Postes'**.

<details>
<summary>Correction</summary>

```sql
SELECT EMP_NOM || ', ' || POSTE "EMPLOYÉS ET POSTES"
FROM EMPLOYEE;
```

**Résultat** :
```
EMPLOYÉS ET POSTES
------------------
ARTHUR, AGENT
PAUL, VENDEUR
JEAN, VENDEUR
CHARLES, DIRECTEUR
...
```

</details>

---

## Question 6

Créez une requête pour afficher **toutes les données** de la table EMPLOYEE dans **une seule colonne** d'affichage. Séparez chaque colonne par une virgule. Nommez la colonne d'affichage **'Liste des Employés'**.

<details>
<summary>Correction</summary>

**Version simple** (problème avec les NULL) :
```sql
SELECT CHAR(EMP_NUM) || ' ' || EMP_NOM || ' ' ||
       POSTE || ' ' || CHAR(DIR) || ' ' ||
       CHAR(DATE_EMB) || ' ' || CHAR(SAL) || ' ' ||
       CHAR(COMM) || ' ' ||
       CHAR(DEPT_NUM) AS "LISTE DES EMPLOYÉS"
FROM EMPLOYEE;
```

**Problème** : Si une valeur est NULL (comme COMM ou DIR), toute la ligne devient NULL dans la concaténation.

**Version avec COALESCE** (gère les NULL) :
```sql
SELECT CHAR(EMP_NUM) || ' ' || EMP_NOM || ' ' ||
       POSTE || ' ' || CHAR(DIR) || ' ' ||
       CHAR(DATE_EMB) || ' ' || CHAR(SAL) || ' ' ||
       COALESCE(CHAR(COMM), '0') || ' ' ||
       CHAR(DEPT_NUM) AS "LISTE DES EMPLOYÉS"
FROM EMPLOYEE;
```

**Note importante** : La fonction `COALESCE(valeur, valeur_par_defaut)` retourne la première valeur non NULL.

</details>

---

## Points clés à retenir

```
┌─────────────────────────────────────────────────────────────────┐
│                   ACTIVITÉ 1 - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SELECT * : toutes les colonnes                                 │
│  SELECT col1, col2 : colonnes spécifiques                       │
│                                                                  │
│  DISTINCT : élimine les doublons                                │
│                                                                  │
│  ALIAS :                                                         │
│    • col AS alias                                                │
│    • col "Alias avec espaces"                                   │
│                                                                  │
│  CONCATÉNATION :                                                 │
│    • col1 || col2                                                │
│    • col1 || ' ' || col2 (avec séparateur)                      │
│                                                                  │
│  GESTION DES NULL :                                              │
│    • COALESCE(col, valeur_defaut)                               │
│    • NULL dans concaténation = résultat NULL                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Scripts de création](00-creation-tables.sql) | [Activité 2 - Sélection et tri](activite-02-selection-tri.md) |

---
*Formation DB2/SQL - M2i Formation*
