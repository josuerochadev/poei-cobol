# Chapitre VII - SQL DML (Data Manipulation Language)

## VII-1 : Introduction au DML

### Rôle du DML

Le **DML** (Data Manipulation Language) permet de manipuler les données stockées dans les tables.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDES DML                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  INSERT  ──► Ajouter de nouvelles lignes                        │
│  UPDATE  ──► Modifier des lignes existantes                     │
│  DELETE  ──► Supprimer des lignes                               │
│                                                                  │
│  + Contrôle transactionnel :                                     │
│  COMMIT    ──► Valider les modifications                        │
│  ROLLBACK  ──► Annuler les modifications                        │
│  SAVEPOINT ──► Point de reprise intermédiaire                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## VII-2 : INSERT

### Syntaxe avec liste de colonnes (recommandé)

```sql
INSERT INTO table (col1, col2, col3, ...)
VALUES (val1, val2, val3, ...);
```

### Exemples

```sql
-- Insert complet avec liste de colonnes
INSERT INTO DEPT (DEPT_NUM, DEPT_NOM, LOC)
VALUES (50, 'FORMATION', 'STRASBOURG');

-- Insert partiel (colonnes nullable omises)
INSERT INTO EMPLOYEE (EMP_NUM, EMP_NOM, DEPT_NUM, SAL)
VALUES (8000, 'NOUVEAU', 20, 2500.00);

-- Les colonnes omises reçoivent :
-- • Leur valeur DEFAULT si définie
-- • NULL sinon (si autorisé)
```

### Syntaxe sans liste de colonnes

```sql
-- Toutes les colonnes dans l'ordre de la table
INSERT INTO DEPT
VALUES (60, 'MARKETING', 'PARIS');

-- ⚠ RISQUÉ : dépend de l'ordre des colonnes dans la table
-- Préférer toujours la syntaxe avec liste
```

### INSERT avec SELECT

```sql
-- Copier des données d'une autre table
INSERT INTO EMP_BACKUP (EMP_NUM, EMP_NOM, SAL)
SELECT EMP_NUM, EMP_NOM, SAL
FROM EMPLOYEE
WHERE DEPT_NUM = 30;

-- Créer une table et l'alimenter
INSERT INTO EMP_HISTORIQUE
SELECT E.*, CURRENT_DATE AS DATE_ARCHIVAGE
FROM EMPLOYEE E
WHERE DATE_EMB < '2020-01-01';
```

### INSERT multiple (DB2 10+)

```sql
INSERT INTO DEPT (DEPT_NUM, DEPT_NOM, LOC)
VALUES (70, 'JURIDIQUE', 'PARIS'),
       (80, 'COMMUNICATION', 'LYON'),
       (90, 'QUALITE', 'MARSEILLE');
```

### Gestion des erreurs INSERT

| SQLCODE | Signification |
|---------|---------------|
| 0 | Succès |
| -803 | Violation clé primaire (doublon) |
| -530 | Violation clé étrangère (FK inexistante) |
| -545 | Violation contrainte CHECK |
| -407 | Valeur NULL non autorisée |

---

## VII-3 : UPDATE

### Syntaxe de base

```sql
UPDATE table
SET colonne1 = valeur1,
    colonne2 = valeur2,
    ...
WHERE condition;
```

### Exemples

```sql
-- Update simple
UPDATE EMPLOYEE
SET SAL = 3000
WHERE EMP_NUM = 7369;

-- Update multiple colonnes
UPDATE EMPLOYEE
SET SAL = SAL * 1.10,
    COMM = 500
WHERE DEPT_NUM = 30;

-- Update avec calcul
UPDATE EMPLOYEE
SET SAL = SAL + (SAL * 0.05)
WHERE POSTE = 'VENDEUR';

-- Update avec sous-requête
UPDATE EMPLOYEE
SET SAL = (SELECT AVG(SAL) FROM EMPLOYEE)
WHERE EMP_NUM = 7900;

-- Update basé sur une autre table
UPDATE EMPLOYEE E
SET SAL = SAL * 1.15
WHERE EXISTS (
    SELECT 1 FROM DEPT D
    WHERE D.DEPT_NUM = E.DEPT_NUM
    AND D.LOC = 'PARIS'
);
```

### ATTENTION : UPDATE sans WHERE

```sql
-- ⚠⚠⚠ DANGER : Met à jour TOUTES les lignes ⚠⚠⚠
UPDATE EMPLOYEE
SET SAL = 0;

-- TOUJOURS vérifier la clause WHERE avant d'exécuter !
```

### Gestion des erreurs UPDATE

| SQLCODE | Signification |
|---------|---------------|
| 0 | Succès |
| 100 | Aucune ligne mise à jour (WHERE ne correspond à rien) |
| -530 | Violation FK (nouvelle valeur inexistante) |
| -545 | Violation CHECK |
| -532 | Impossible de modifier PK référencée |

---

## VII-4 : DELETE

### Syntaxe de base

```sql
DELETE FROM table
WHERE condition;
```

### Exemples

```sql
-- Supprimer une ligne spécifique
DELETE FROM EMPLOYEE
WHERE EMP_NUM = 7900;

-- Supprimer plusieurs lignes
DELETE FROM EMPLOYEE
WHERE DEPT_NUM = 40;

-- Supprimer avec sous-requête
DELETE FROM EMPLOYEE
WHERE DEPT_NUM IN (
    SELECT DEPT_NUM FROM DEPT WHERE LOC = 'PARIS'
);

-- Supprimer les doublons (garder le plus ancien)
DELETE FROM EMPLOYEE E1
WHERE EXISTS (
    SELECT 1 FROM EMPLOYEE E2
    WHERE E1.EMP_NOM = E2.EMP_NOM
    AND E1.EMP_NUM > E2.EMP_NUM
);
```

### ATTENTION : DELETE sans WHERE

```sql
-- ⚠⚠⚠ DANGER : Supprime TOUTES les lignes ⚠⚠⚠
DELETE FROM EMPLOYEE;

-- La table existe toujours mais est VIDE
-- Différent de DROP TABLE qui supprime la structure
```

### Gestion des erreurs DELETE

| SQLCODE | Signification |
|---------|---------------|
| 0 | Succès |
| 100 | Aucune ligne supprimée |
| -532 | Violation FK (lignes enfant existent) |

### TRUNCATE (vidage rapide)

```sql
-- Plus rapide que DELETE sans WHERE
-- Mais pas de ROLLBACK possible
TRUNCATE TABLE EMP_TEMP IMMEDIATE;
```

---

## VII-5 : Transactions

### Concept de transaction

Une **transaction** est un ensemble d'opérations qui doivent être traitées comme une unité indivisible (tout ou rien).

```
┌─────────────────────────────────────────────────────────────────┐
│                      TRANSACTION                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Début implicite (première instruction DML)                      │
│        │                                                         │
│        ▼                                                         │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  INSERT INTO ...                                        │    │
│  │  UPDATE ...                                             │    │
│  │  DELETE ...                                             │    │
│  │  ...                                                    │    │
│  └─────────────────────────────────────────────────────────┘    │
│        │                                                         │
│        ▼                                                         │
│  ┌──────────────┐          ┌──────────────┐                     │
│  │   COMMIT     │    ou    │   ROLLBACK   │                     │
│  │  (valider)   │          │  (annuler)   │                     │
│  └──────────────┘          └──────────────┘                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### COMMIT

```sql
-- Valider toutes les modifications depuis le dernier COMMIT
UPDATE EMPLOYEE SET SAL = SAL * 1.10 WHERE DEPT_NUM = 30;
INSERT INTO AUDIT_LOG VALUES ('Augmentation DEPT 30', CURRENT_TIMESTAMP);
COMMIT;
-- Les modifications sont maintenant permanentes
```

### ROLLBACK

```sql
-- Annuler toutes les modifications depuis le dernier COMMIT
DELETE FROM EMPLOYEE WHERE DEPT_NUM = 40;
-- Oups, erreur !
ROLLBACK;
-- Les employés du DEPT 40 sont restaurés
```

### SAVEPOINT

```sql
-- Points de reprise intermédiaires
UPDATE EMPLOYEE SET SAL = SAL * 1.05;
SAVEPOINT SP1;

UPDATE EMPLOYEE SET COMM = COMM * 1.10 WHERE COMM IS NOT NULL;
SAVEPOINT SP2;

DELETE FROM EMPLOYEE WHERE SAL > 5000;
-- Erreur ! Annuler uniquement le DELETE
ROLLBACK TO SAVEPOINT SP2;

-- Les UPDATE sont conservés
COMMIT;
```

### Exemple complet : virement bancaire

```sql
-- Transaction de virement
-- Début implicite

-- 1. Débiter le compte source
UPDATE COMPTE
SET SOLDE = SOLDE - 500
WHERE NUM_COMPTE = 'A001';

-- Vérifier que le débit a fonctionné
-- (le solde ne doit pas devenir négatif)

-- 2. Créditer le compte destination
UPDATE COMPTE
SET SOLDE = SOLDE + 500
WHERE NUM_COMPTE = 'B002';

-- 3. Enregistrer l'opération
INSERT INTO OPERATIONS (DATE_OP, COMPTE_SRC, COMPTE_DST, MONTANT)
VALUES (CURRENT_TIMESTAMP, 'A001', 'B002', 500);

-- Si tout OK
COMMIT;

-- Si erreur à n'importe quelle étape
-- ROLLBACK;
```

---

## VII-6 : Bonnes pratiques DML

### Règles de sécurité

```
┌─────────────────────────────────────────────────────────────────┐
│                 BONNES PRATIQUES DML                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. TOUJOURS tester avec SELECT avant UPDATE/DELETE             │
│     ┌─────────────────────────────────────────────────────┐    │
│     │  -- D'abord : vérifier ce qui sera affecté         │    │
│     │  SELECT * FROM EMPLOYEE WHERE DEPT_NUM = 40;       │    │
│     │  -- 3 rows selected                                │    │
│     │                                                     │    │
│     │  -- Ensuite : exécuter le DELETE                   │    │
│     │  DELETE FROM EMPLOYEE WHERE DEPT_NUM = 40;         │    │
│     └─────────────────────────────────────────────────────┘    │
│                                                                  │
│  2. TOUJOURS avoir une clause WHERE pour UPDATE/DELETE          │
│     (sauf si c'est vraiment intentionnel)                       │
│                                                                  │
│  3. Utiliser les transactions explicites                        │
│     • Ne pas faire AUTOCOMMIT en production                     │
│     • Grouper les opérations logiques                           │
│                                                                  │
│  4. Vérifier le SQLCODE après chaque opération                 │
│     • 0 = succès                                                │
│     • 100 = pas de données (attention pour UPDATE/DELETE)       │
│     • < 0 = erreur                                              │
│                                                                  │
│  5. Utiliser INSERT avec liste de colonnes explicite           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                   CHAPITRE VII - RÉSUMÉ                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  VII-2 INSERT                                                    │
│        INSERT INTO table (cols) VALUES (vals)                   │
│        INSERT INTO table SELECT ... FROM ...                    │
│        Toujours utiliser la liste de colonnes                   │
│                                                                  │
│  VII-3 UPDATE                                                    │
│        UPDATE table SET col=val WHERE condition                 │
│        ⚠ TOUJOURS mettre WHERE (sauf volontairement)            │
│        Tester avec SELECT avant                                 │
│                                                                  │
│  VII-4 DELETE                                                    │
│        DELETE FROM table WHERE condition                        │
│        ⚠ Sans WHERE = supprime tout                             │
│        TRUNCATE pour vidage rapide                              │
│                                                                  │
│  VII-5 TRANSACTIONS                                              │
│        COMMIT   : valide les modifications                      │
│        ROLLBACK : annule les modifications                      │
│        SAVEPOINT: point de reprise intermédiaire                │
│        Propriétés ACID garanties                                │
│                                                                  │
│  VII-6 BONNES PRATIQUES                                         │
│        • SELECT avant UPDATE/DELETE                             │
│        • WHERE obligatoire                                      │
│        • Transactions explicites                                │
│        • Vérifier SQLCODE                                       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Exercice 1 : INSERT

1. Insérez un nouveau département (50, 'FORMATION', 'STRASBOURG')
2. Insérez un employé dans ce département
3. Insérez 3 produits en une seule requête

### Exercice 2 : UPDATE

1. Augmentez le salaire de 10% pour tous les vendeurs
2. Mettez à jour la commission à 0 pour ceux qui n'en ont pas (NULL)
3. Testez d'abord avec SELECT combien de lignes seront affectées

### Exercice 3 : DELETE et Transactions

Écrivez un script qui :
1. Crée un SAVEPOINT
2. Supprime tous les employés d'un département
3. Vérifie qu'il reste des employés (SELECT COUNT)
4. Si oui, COMMIT ; sinon ROLLBACK au SAVEPOINT

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI - SQL DDL](06-sql-ddl.md) | [Chapitre VIII - SQL SELECT](08-sql-select.md) |

---
*Formation DB2/SQL - M2i Formation*
