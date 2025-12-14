# Chapitre X - Embedded SQL (COBOL + DB2)

## X-1 : Introduction à l'Embedded SQL

### Concept

L'**Embedded SQL** permet d'intégrer des instructions SQL directement dans un programme COBOL. Le programme peut ainsi lire, modifier et gérer des données DB2.

```
┌─────────────────────────────────────────────────────────────────┐
│                    EMBEDDED SQL                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Programme COBOL + SQL                                           │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  WORKING-STORAGE SECTION.                               │    │
│  │  01 WS-EMP-NOM PIC X(30).                               │    │
│  │                                                          │    │
│  │  PROCEDURE DIVISION.                                     │    │
│  │      EXEC SQL                                           │    │
│  │          SELECT EMP_NOM INTO :WS-EMP-NOM                │    │
│  │          FROM EMPLOYEE                                  │    │
│  │          WHERE EMP_NUM = 7369                           │    │
│  │      END-EXEC.                                          │    │
│  │      DISPLAY WS-EMP-NOM.                                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Caractéristiques :                                              │
│  • SQL entre EXEC SQL et END-EXEC                               │
│  • Variables COBOL préfixées par ":"                            │
│  • Résultat dans variable hôte (:WS-EMP-NOM)                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## X-2 : Processus de préparation

### Étapes de compilation

```
┌─────────────────────────────────────────────────────────────────┐
│              PROCESSUS DE PRÉPARATION DB2                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Source COBOL + SQL (.cbl)                                      │
│           │                                                      │
│           ▼                                                      │
│  ┌─────────────────────┐                                        │
│  │  1. PRECOMPILE      │  ──► Remplace EXEC SQL par CALL       │
│  │     (DSNHPC)        │      Génère le DBRM                    │
│  └─────────────────────┘                                        │
│           │                                                      │
│      ┌────┴────┐                                                │
│      ▼         ▼                                                │
│  Source     DBRM                                                │
│  modifié   (Database                                            │
│  (.cob)    Request                                              │
│      │     Module)                                              │
│      │         │                                                │
│      ▼         │                                                │
│  ┌─────────────────────┐                                        │
│  │  2. COMPILE         │  ──► Compile le COBOL modifié         │
│  │     (IGYCRCTL)      │                                        │
│  └─────────────────────┘                                        │
│           │                                                      │
│           ▼                                                      │
│      Object file                                                │
│      (.obj)                                                     │
│           │                                                      │
│           ▼                                                      │
│  ┌─────────────────────┐                                        │
│  │  3. LINK-EDIT       │  ──► Crée le module exécutable        │
│  │     (IEWL)          │                                        │
│  └─────────────────────┘                                        │
│           │                                                      │
│           ▼                                                      │
│      Load module                                                │
│      (.load)         DBRM                                       │
│           │              │                                       │
│           │              ▼                                       │
│           │     ┌─────────────────────┐                         │
│           │     │  4. BIND            │  ──► Crée le plan/package│
│           │     │     (DSNHPC)        │      d'accès DB2        │
│           │     └─────────────────────┘                         │
│           │              │                                       │
│           ▼              ▼                                       │
│      Programme      Package/Plan                                │
│      exécutable     DB2                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Description des étapes

| Étape | Outil | Entrée | Sortie | Rôle |
|-------|-------|--------|--------|------|
| **Precompile** | DSNHPC | Source COBOL+SQL | Source modifié + DBRM | Transforme EXEC SQL en CALL |
| **Compile** | IGYCRCTL | Source modifié | Object file | Compile le COBOL |
| **Link-Edit** | IEWL | Object file | Load module | Crée l'exécutable |
| **Bind** | BIND | DBRM | Package/Plan | Crée le plan d'accès DB2 |

---

## X-3 : Variables hôtes

### Définition

Les **variables hôtes** sont des variables COBOL utilisées pour échanger des données avec DB2.

```
┌─────────────────────────────────────────────────────────────────┐
│                   VARIABLES HÔTES                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Déclaration dans WORKING-STORAGE :                             │
│                                                                  │
│       EXEC SQL BEGIN DECLARE SECTION END-EXEC.                  │
│       01 WS-EMP-NUM        PIC S9(9) COMP.                      │
│       01 WS-EMP-NOM        PIC X(30).                           │
│       01 WS-SAL            PIC S9(5)V99 COMP-3.                 │
│       EXEC SQL END DECLARE SECTION END-EXEC.                    │
│                                                                  │
│  Utilisation dans SQL (préfixe ":") :                           │
│                                                                  │
│       EXEC SQL                                                   │
│           SELECT EMP_NOM, SAL                                   │
│           INTO :WS-EMP-NOM, :WS-SAL                             │
│           FROM EMPLOYEE                                          │
│           WHERE EMP_NUM = :WS-EMP-NUM                           │
│       END-EXEC.                                                  │
│                                                                  │
│  ⚠ Le préfixe ":" est OBLIGATOIRE dans les requêtes SQL        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Correspondance types DB2/COBOL

| Type DB2 | Type COBOL |
|----------|------------|
| SMALLINT | PIC S9(4) COMP |
| INTEGER | PIC S9(9) COMP |
| DECIMAL(p,s) | PIC S9(p-s)V9(s) COMP-3 |
| CHAR(n) | PIC X(n) |
| VARCHAR(n) | Structure 49 (voir ci-dessous) |
| DATE | PIC X(10) |
| TIME | PIC X(8) |
| TIMESTAMP | PIC X(26) |

### VARCHAR en COBOL

```cobol
      * Structure pour VARCHAR(30)
       01  WS-EMP-NOM.
           49 WS-EMP-NOM-LEN  PIC S9(4) COMP.
           49 WS-EMP-NOM-TEXT PIC X(30).

      * Initialisation
           MOVE 5 TO WS-EMP-NOM-LEN.
           MOVE 'PAUL' TO WS-EMP-NOM-TEXT.
```

---

## X-4 : DCLGEN

### Utilisation de DCLGEN

**DCLGEN** génère automatiquement les variables hôtes correspondant à une table DB2.

```cobol
      ******************************************************************
      * DCLGEN pour table EMPLOYEE                                     *
      * Généré via DB2I option 2                                       *
      ******************************************************************

      * Déclaration SQL de la table
           EXEC SQL DECLARE EMPLOYEE TABLE
           ( EMP_NUM                        INTEGER NOT NULL,
             EMP_NOM                        VARCHAR(30) NOT NULL,
             POSTE                          VARCHAR(20),
             DIR                            INTEGER,
             DATE_EMB                       DATE,
             SAL                            DECIMAL(7, 2),
             COMM                           DECIMAL(7, 2),
             DEPT_NUM                       SMALLINT NOT NULL
           ) END-EXEC.

      * Variables hôtes COBOL
       01  DCLEMPLOYEE.
           10 EMP-NUM              PIC S9(9) COMP.
           10 EMP-NOM.
              49 EMP-NOM-LEN       PIC S9(4) COMP.
              49 EMP-NOM-TEXT      PIC X(30).
           10 POSTE.
              49 POSTE-LEN         PIC S9(4) COMP.
              49 POSTE-TEXT        PIC X(20).
           10 DIR                  PIC S9(9) COMP.
           10 DATE-EMB             PIC X(10).
           10 SAL                  PIC S9(5)V9(2) COMP-3.
           10 COMM                 PIC S9(5)V9(2) COMP-3.
           10 DEPT-NUM             PIC S9(4) COMP.

      * Variables indicateur (pour détecter les NULL)
       01  IEMPLOYEE.
           10 IEMP-NUM             PIC S9(4) COMP.
           10 IEMP-NOM             PIC S9(4) COMP.
           10 IPOSTE               PIC S9(4) COMP.
           10 IDIR                 PIC S9(4) COMP.
           10 IDATE-EMB            PIC S9(4) COMP.
           10 ISAL                 PIC S9(4) COMP.
           10 ICOMM                PIC S9(4) COMP.
           10 IDEPT-NUM            PIC S9(4) COMP.
```

### Inclusion dans le programme

```cobol
       WORKING-STORAGE SECTION.

      * Inclusion du DCLGEN
           EXEC SQL
               INCLUDE EMPLOYEE
           END-EXEC.

      * Inclusion de SQLCA
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
```

---

## X-5 : SQLCA et gestion des erreurs

### Structure SQLCA

**SQLCA** (SQL Communication Area) contient les informations sur l'exécution de chaque requête SQL.

```cobol
      * Déclaration automatique via INCLUDE
           EXEC SQL INCLUDE SQLCA END-EXEC.

      * Structure SQLCA (simplifiée)
       01  SQLCA.
           05 SQLCAID        PIC X(8).
           05 SQLCABC        PIC S9(9) COMP.
           05 SQLCODE        PIC S9(9) COMP.      ← Code retour
           05 SQLERRM.
              49 SQLERRML    PIC S9(4) COMP.
              49 SQLERRMC    PIC X(70).           ← Message erreur
           05 SQLERRP        PIC X(8).
           05 SQLERRD        PIC S9(9) COMP OCCURS 6.
           05 SQLWARN.
              10 SQLWARN0    PIC X.
              10 SQLWARN1    PIC X.               ← Warnings
              ...
           05 SQLSTATE       PIC X(5).            ← Code état
```

### SQLCODE - Codes retour

```
┌─────────────────────────────────────────────────────────────────┐
│                       SQLCODE                                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  SQLCODE = 0      ──► Succès                                    │
│                                                                  │
│  SQLCODE > 0      ──► Warning (avertissement)                   │
│  • +100 : NOT FOUND (aucune ligne trouvée/plus de données)     │
│                                                                  │
│  SQLCODE < 0      ──► Erreur                                    │
│  • -204 : Object undefined (table n'existe pas)                │
│  • -206 : Column not found                                      │
│  • -530 : Foreign key violation                                 │
│  • -803 : Duplicate key                                         │
│  • -811 : Multiple rows returned (INTO attend 1 ligne)         │
│  • -904 : Resource unavailable                                  │
│  • -911 : Deadlock or timeout                                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Gestion des erreurs en COBOL

```cobol
       PROCEDURE DIVISION.

       1000-LIRE-EMPLOYE.
           MOVE 7369 TO EMP-NUM.

           EXEC SQL
               SELECT EMP_NOM, SAL
               INTO :EMP-NOM, :SAL
               FROM EMPLOYEE
               WHERE EMP_NUM = :EMP-NUM
           END-EXEC.

           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY "Employé trouvé : " EMP-NOM-TEXT
               WHEN 100
                   DISPLAY "Employé non trouvé"
               WHEN -811
                   DISPLAY "Erreur : plusieurs lignes retournées"
               WHEN OTHER
                   DISPLAY "Erreur SQL : " SQLCODE
                   DISPLAY "Message : " SQLERRMC
           END-EVALUATE.
```

### Variables indicateur (NULL)

```cobol
      * Détection des valeurs NULL
       WORKING-STORAGE SECTION.
       01  WS-COMM          PIC S9(5)V99 COMP-3.
       01  WS-IND-COMM      PIC S9(4) COMP.

       PROCEDURE DIVISION.
           EXEC SQL
               SELECT COMM
               INTO :WS-COMM :WS-IND-COMM
               FROM EMPLOYEE
               WHERE EMP_NUM = :EMP-NUM
           END-EXEC.

           IF WS-IND-COMM < 0
               DISPLAY "Commission est NULL"
           ELSE
               DISPLAY "Commission : " WS-COMM
           END-IF.

      * Valeurs de l'indicateur :
      * < 0  : la valeur est NULL
      * = 0  : valeur normale
      * > 0  : troncature (pour les chaînes)
```

---

## X-6 : Exemples complets

### SELECT simple

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELSIMP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-EMP-NUM        PIC S9(9) COMP.
       01  WS-EMP-NOM        PIC X(30).
       01  WS-SAL            PIC S9(5)V99 COMP-3.
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MOVE 7369 TO WS-EMP-NUM.
           PERFORM 1000-LIRE-EMPLOYE.
           STOP RUN.

       1000-LIRE-EMPLOYE.
           EXEC SQL
               SELECT EMP_NOM, SAL
               INTO :WS-EMP-NOM, :WS-SAL
               FROM EMPLOYEE
               WHERE EMP_NUM = :WS-EMP-NUM
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "Nom : " WS-EMP-NOM
               DISPLAY "Salaire : " WS-SAL
           ELSE IF SQLCODE = 100
               DISPLAY "Employé non trouvé"
           ELSE
               DISPLAY "Erreur SQL : " SQLCODE
           END-IF.
```

### INSERT

```cobol
       1000-INSERER-EMPLOYE.
           MOVE 9000 TO WS-EMP-NUM.
           MOVE 'NOUVEAU' TO WS-EMP-NOM.
           MOVE 2000.00 TO WS-SAL.
           MOVE 20 TO WS-DEPT-NUM.

           EXEC SQL
               INSERT INTO EMPLOYEE (EMP_NUM, EMP_NOM, SAL, DEPT_NUM)
               VALUES (:WS-EMP-NUM, :WS-EMP-NOM, :WS-SAL, :WS-DEPT-NUM)
           END-EXEC.

           IF SQLCODE = 0
               EXEC SQL COMMIT END-EXEC
               DISPLAY "Insertion réussie"
           ELSE IF SQLCODE = -803
               DISPLAY "Erreur : employé déjà existant"
           ELSE
               EXEC SQL ROLLBACK END-EXEC
               DISPLAY "Erreur insertion : " SQLCODE
           END-IF.
```

### UPDATE

```cobol
       1000-AUGMENTER-SALAIRE.
           MOVE 7369 TO WS-EMP-NUM.
           MOVE 100.00 TO WS-AUGMENTATION.

           EXEC SQL
               UPDATE EMPLOYEE
               SET SAL = SAL + :WS-AUGMENTATION
               WHERE EMP_NUM = :WS-EMP-NUM
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "Lignes modifiées : " SQLERRD(3)
               EXEC SQL COMMIT END-EXEC
           ELSE
               EXEC SQL ROLLBACK END-EXEC
           END-IF.
```

### DELETE

```cobol
       1000-SUPPRIMER-EMPLOYE.
           MOVE 9000 TO WS-EMP-NUM.

           EXEC SQL
               DELETE FROM EMPLOYEE
               WHERE EMP_NUM = :WS-EMP-NUM
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "Employé supprimé"
               EXEC SQL COMMIT END-EXEC
           ELSE IF SQLCODE = 100
               DISPLAY "Employé non trouvé"
           ELSE
               EXEC SQL ROLLBACK END-EXEC
           END-IF.
```

---

## X-7 : Curseurs

### Concept

Un **curseur** permet de parcourir un ensemble de lignes résultat ligne par ligne.

```
┌─────────────────────────────────────────────────────────────────┐
│                      CURSEUR                                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. DECLARE : définir le curseur (requête)                      │
│  2. OPEN    : exécuter la requête, positionner au début         │
│  3. FETCH   : lire la ligne courante, avancer                   │
│  4. CLOSE   : fermer le curseur                                 │
│                                                                  │
│  Résultat                                                        │
│  ┌─────────────────────────┐                                    │
│  │ Ligne 1                 │ ◄── FETCH 1                        │
│  │ Ligne 2                 │ ◄── FETCH 2                        │
│  │ Ligne 3                 │ ◄── FETCH 3                        │
│  │ ...                     │                                     │
│  │ (fin)                   │ ◄── SQLCODE = 100                  │
│  └─────────────────────────┘                                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Exemple complet

```cobol
       WORKING-STORAGE SECTION.
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-EMP-NUM        PIC S9(9) COMP.
       01  WS-EMP-NOM        PIC X(30).
       01  WS-SAL            PIC S9(5)V99 COMP-3.
       01  WS-DEPT-NUM       PIC S9(4) COMP.
           EXEC SQL END DECLARE SECTION END-EXEC.

           EXEC SQL INCLUDE SQLCA END-EXEC.

      * Déclaration du curseur
           EXEC SQL
               DECLARE C_EMPLOYES CURSOR FOR
               SELECT EMP_NUM, EMP_NOM, SAL
               FROM EMPLOYEE
               WHERE DEPT_NUM = :WS-DEPT-NUM
               ORDER BY SAL DESC
           END-EXEC.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MOVE 30 TO WS-DEPT-NUM.
           PERFORM 1000-LISTER-EMPLOYES.
           STOP RUN.

       1000-LISTER-EMPLOYES.
      * Ouvrir le curseur
           EXEC SQL OPEN C_EMPLOYES END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY "Erreur OPEN : " SQLCODE
               GO TO 1000-FIN
           END-IF.

      * Boucle de lecture
           PERFORM 2000-LIRE-EMPLOYE UNTIL SQLCODE = 100.

      * Fermer le curseur
           EXEC SQL CLOSE C_EMPLOYES END-EXEC.

       1000-FIN.
           EXIT.

       2000-LIRE-EMPLOYE.
           EXEC SQL
               FETCH C_EMPLOYES
               INTO :WS-EMP-NUM, :WS-EMP-NOM, :WS-SAL
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY WS-EMP-NUM " " WS-EMP-NOM " " WS-SAL
           END-IF.
```

### Curseur FOR UPDATE

```cobol
      * Curseur pour mise à jour
           EXEC SQL
               DECLARE C_EMP_UPDATE CURSOR FOR
               SELECT EMP_NUM, SAL
               FROM EMPLOYEE
               WHERE DEPT_NUM = :WS-DEPT-NUM
               FOR UPDATE OF SAL
           END-EXEC.

      * Mise à jour via curseur
           EXEC SQL
               UPDATE EMPLOYEE
               SET SAL = SAL * 1.10
               WHERE CURRENT OF C_EMP_UPDATE
           END-EXEC.
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAPITRE X - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  X-1 EMBEDDED SQL                                                │
│      • SQL entre EXEC SQL et END-EXEC                           │
│      • Variables préfixées par ":"                              │
│                                                                  │
│  X-2 PRÉPARATION                                                │
│      PRECOMPILE → COMPILE → LINK-EDIT → BIND                   │
│      Génère DBRM + Load module + Package/Plan                   │
│                                                                  │
│  X-3 VARIABLES HÔTES                                            │
│      • DECLARE SECTION                                          │
│      • Correspondance types DB2/COBOL                           │
│      • VARCHAR = structure niveau 49                            │
│                                                                  │
│  X-4 DCLGEN                                                      │
│      • Génère variables hôtes depuis table                      │
│      • INCLUDE pour réutiliser                                  │
│                                                                  │
│  X-5 SQLCA ET ERREURS                                           │
│      • SQLCODE : 0=OK, 100=NOT FOUND, <0=erreur                │
│      • Variables indicateur pour NULL                           │
│                                                                  │
│  X-6 OPÉRATIONS                                                 │
│      • SELECT INTO pour une ligne                               │
│      • INSERT, UPDATE, DELETE                                   │
│      • COMMIT / ROLLBACK                                        │
│                                                                  │
│  X-7 CURSEURS                                                    │
│      • DECLARE, OPEN, FETCH, CLOSE                              │
│      • FOR UPDATE pour modifications                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Exercice 1 : Variables hôtes

Écrivez les déclarations COBOL pour les colonnes suivantes :
- CLIENT_ID INTEGER
- CLIENT_NOM VARCHAR(50)
- SOLDE DECIMAL(10,2)
- DATE_CREATION DATE

### Exercice 2 : SELECT et gestion d'erreurs

Écrivez un programme qui :
1. Lit un employé par son numéro
2. Affiche ses informations si trouvé
3. Gère les cas NOT FOUND et erreur

### Exercice 3 : Curseur

Écrivez un programme qui :
1. Déclare un curseur pour les employés d'un département
2. Parcourt tous les employés
3. Affiche le total des salaires

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre IX - Agrégations](09-sql-avance.md) | - |

---
*Formation DB2/SQL - M2i Formation*
