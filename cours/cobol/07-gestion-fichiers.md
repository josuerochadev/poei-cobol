# Chapitre VII - Gestion des Fichiers

## VII-1 Notion de fichiers sous COBOL

Un **fichier** est un ensemble d'enregistrements stockés sur un support externe.

### Table vs Fichier

| Aspect | Table (OCCURS) | Fichier |
|--------|----------------|---------|
| Stockage | Mémoire (RAM) | Disque/Bande |
| Persistance | Durée du programme | Permanent |
| Taille | Limitée | Quasi illimitée |
| Déclaration | WORKING-STORAGE | FILE SECTION |

### Déclaration d'un fichier

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-CLIENTS
               ASSIGN TO 'CLIENTS.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-CLIENTS
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  ENR-CLIENT.
           05  CLI-CODE    PIC 9(4).
           05  CLI-NOM     PIC X(30).
           05  CLI-SOLDE   PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  WS-STATUS       PIC XX.
```

### FILE STATUS - Codes retour

| Code | Signification |
|------|---------------|
| `00` | Opération réussie |
| `10` | Fin de fichier (EOF) |
| `22` | Clé en double |
| `23` | Enregistrement non trouvé |
| `35` | Fichier non trouvé |
| `41` | Fichier déjà ouvert |
| `42` | Fichier non ouvert |

---

## VII-2 Organisation de fichiers

### 1. SEQUENTIAL (ESDS)

Enregistrements stockés dans l'ordre d'écriture.

```cobol
       SELECT FICHIER-SEQ
           ASSIGN TO 'DATA.SEQ'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS.
```

| Aspect | Valeur |
|--------|--------|
| Accès | Séquentiel uniquement |
| Ajout | En fin (EXTEND) |
| Modification | Impossible |
| Usage | Logs, historiques, batch |

### 2. INDEXED (KSDS/VSAM)

Accès via une clé unique + clés secondaires optionnelles.

```cobol
       SELECT FICHIER-IDX
           ASSIGN TO 'DATA.IDX'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLI-CODE
           ALTERNATE RECORD KEY IS CLI-NOM WITH DUPLICATES
           FILE STATUS IS WS-STATUS.
```

| Aspect | Valeur |
|--------|--------|
| Accès | Séquentiel, Direct, Dynamique |
| Clé primaire | Obligatoire, unique |
| Modification | REWRITE |
| Suppression | DELETE |
| Usage | Fichiers maîtres |

### 3. RELATIVE (RRDS)

Accès par numéro de position (RRN).

```cobol
       SELECT FICHIER-REL
           ASSIGN TO 'DATA.REL'
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS WS-RRN
           FILE STATUS IS WS-STATUS.
```

### Comparatif

| Critère | SEQUENTIAL | INDEXED | RELATIVE |
|---------|------------|---------|----------|
| Clé | Aucune | Obligatoire | Position |
| Accès direct | ❌ | ✅ | ✅ |
| Modification | ❌ | ✅ | ✅ |
| Suppression | ❌ | ✅ | ✅ |
| Mainframe | ESDS | KSDS | RRDS |

### RECORDING MODE

| Mode | Description |
|------|-------------|
| `F` | Fixed - taille fixe |
| `V` | Variable - taille variable |
| `U` | Undefined - indéfini |

---

## VII-3 Mode d'accès aux fichiers

### 1. ACCESS MODE IS SEQUENTIAL

Lecture/écriture dans l'ordre.

```cobol
       PERFORM UNTIL FIN-FICHIER
           READ FICHIER-SEQ
               AT END SET FIN-FICHIER TO TRUE
           END-READ
       END-PERFORM
```

### 2. ACCESS MODE IS RANDOM

Accès direct par clé ou position.

```cobol
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
       END-READ
```

### 3. ACCESS MODE IS DYNAMIC

Combine séquentiel ET direct.

```cobol
      * Accès direct
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE

      * Positionnement + séquentiel
       START FICHIER-IDX KEY >= CLI-CODE
       READ FICHIER-IDX NEXT
```

### Compatibilité Organisation × Mode

| Organisation | SEQUENTIAL | RANDOM | DYNAMIC |
|--------------|------------|--------|---------|
| SEQUENTIAL | ✅ | ❌ | ❌ |
| INDEXED | ✅ | ✅ | ✅ |
| RELATIVE | ✅ | ✅ | ✅ |

---

## Modes d'ouverture (OPEN)

| Mode | Lecture | Écriture | Usage |
|------|---------|----------|-------|
| `INPUT` | ✅ | ❌ | Lecture seule |
| `OUTPUT` | ❌ | ✅ | Création/écrasement |
| `I-O` | ✅ | ✅ | Mise à jour |
| `EXTEND` | ❌ | ✅ fin | Ajout en fin |

---

## Instructions E/S

### READ - Lecture

```cobol
      * Séquentiel
       READ FICHIER AT END SET FIN TO TRUE END-READ

      * Direct par clé
       READ FICHIER KEY IS CLE INVALID KEY ... END-READ

      * Suivant (DYNAMIC)
       READ FICHIER NEXT AT END ... END-READ
```

### WRITE - Écriture

```cobol
       WRITE ENR-CLIENT
           INVALID KEY DISPLAY 'Cle en double'
       END-WRITE
```

### REWRITE - Modification

```cobol
      * Doit être précédé d'un READ réussi
       READ FICHIER KEY IS CLI-CODE
       MOVE 'NOUVEAU' TO CLI-NOM
       REWRITE ENR-CLIENT
           INVALID KEY DISPLAY 'Erreur'
       END-REWRITE
```

### DELETE - Suppression

```cobol
      * Mode RANDOM
       MOVE '0003' TO CLI-CODE
       DELETE FICHIER INVALID KEY ... END-DELETE

      * Mode SEQUENTIAL (après READ)
       READ FICHIER NEXT
       DELETE FICHIER END-DELETE
```

### START - Positionnement

```cobol
       MOVE '0050' TO CLI-CODE
       START FICHIER KEY >= CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
       END-START

      * Variantes : =, >, >=, <, <=
```

---

## Exemple complet

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-FICHIER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENTS
               ASSIGN TO 'CLIENTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-CODE
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-CLIENTS.
       01  ENR-CLIENT.
           05  CLI-CODE    PIC 9(4).
           05  CLI-NOM     PIC X(30).

       WORKING-STORAGE SECTION.
       01  WS-STATUS       PIC XX.
       01  WS-FIN          PIC 9 VALUE 0.
           88 FIN-FICHIER  VALUE 1.

       PROCEDURE DIVISION.
           OPEN I-O F-CLIENTS

           IF WS-STATUS NOT = '00'
               DISPLAY 'Erreur : ' WS-STATUS
               STOP RUN
           END-IF

      * Lecture directe
           MOVE '0003' TO CLI-CODE
           READ F-CLIENTS KEY IS CLI-CODE
               INVALID KEY DISPLAY 'Non trouve'
           END-READ

      * Parcours séquentiel
           MOVE '0001' TO CLI-CODE
           START F-CLIENTS KEY >= CLI-CODE
           PERFORM UNTIL FIN-FICHIER
               READ F-CLIENTS NEXT
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END DISPLAY CLI-CODE ' ' CLI-NOM
               END-READ
           END-PERFORM

           CLOSE F-CLIENTS
           STOP RUN.
```

---

## Résumé

| Concept | Syntaxe clé |
|---------|-------------|
| Fichier séquentiel | `ORGANIZATION IS SEQUENTIAL` |
| Fichier indexé | `ORGANIZATION IS INDEXED` + `RECORD KEY` |
| Fichier relatif | `ORGANIZATION IS RELATIVE` + `RELATIVE KEY` |
| Accès séquentiel | `ACCESS MODE IS SEQUENTIAL` |
| Accès direct | `ACCESS MODE IS RANDOM` |
| Accès mixte | `ACCESS MODE IS DYNAMIC` |
| Code retour | `FILE STATUS IS variable` |
| Lecture | `READ ... AT END / INVALID KEY` |
| Écriture | `WRITE ... INVALID KEY` |
| Modification | `REWRITE ... INVALID KEY` |
| Suppression | `DELETE ... INVALID KEY` |
| Positionnement | `START ... KEY >= ...` |
