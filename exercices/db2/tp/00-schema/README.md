# Schema de la Base de Donnees

Ce dossier contient le script de creation des tables pour les travaux pratiques.

## Fichiers

| Fichier | Description |
|---------|-------------|
| [creation-tables.sql](creation-tables.sql) | Script DDL + INSERT |

## Tables creees

### DEPT (4 lignes)
```
DEPT_NUM | DEPT_NOM      | LOC
---------|---------------|------------
10       | COMPTABILITE  | MARSEILLE
20       | RECHERCHE     | STRASBOURG
30       | VENTES        | LYON
40       | EXPLOITATION  | PARIS
```

### EMPLOYEE (14 lignes)
```
EMP_NUM | EMP_NOM  | POSTE     | DIR  | DATE_EMB   | SAL     | COMM | DEPT_NUM
--------|----------|-----------|------|------------|---------|------|----------
7369    | ARTHUR   | AGENT     | 7902 | 2020-12-17 | 800.00  | NULL | 20
7499    | PAUL     | VENDEUR   | 7698 | 2021-02-20 | 1600.00 | 300  | 30
7521    | JEAN     | VENDEUR   | 7698 | 2021-02-22 | 1250.00 | 500  | 30
...
```

### SAL_GRILLE (5 lignes)
```
GRADE | MIN_SAL  | MAX_SAL
------|----------|--------
1     | 700.00   | 1200.00
2     | 1201.00  | 1400.00
3     | 1401.00  | 2000.00
4     | 2001.00  | 3000.00
5     | 3001.00  | 9999.00
```

## Execution

### Dans SPUFI (z/OS)
1. Copier le contenu dans votre dataset d'entree SPUFI
2. Executer via DB2I > SPUFI

### Dans un client SQL (DBeaver, etc.)
```bash
# Copier/coller le contenu ou executer :
source creation-tables.sql
```

---
*Formation DB2/SQL - M2i Formation*
