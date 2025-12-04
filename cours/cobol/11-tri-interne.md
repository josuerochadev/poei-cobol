# Chapitre XI - Tri Interne (SORT / MERGE)

Ce chapitre traite du tri et de la fusion de fichiers en COBOL à l'aide des instructions SORT et MERGE.

---

## XI-1 Tri Interne (SORT)

### Concept

Le **tri interne** COBOL permet de trier un fichier selon un ou plusieurs critères (clés de tri) sans utiliser d'utilitaire externe.

```
┌─────────────────┐      ┌──────────┐      ┌─────────────────┐
│ Fichier Entrée  │ ───► │   SORT   │ ───► │ Fichier Sortie  │
│   (non trié)    │      │          │      │    (trié)       │
└─────────────────┘      └──────────┘      └─────────────────┘
```

### Déclaration du fichier de travail (SD)

Le tri nécessite un **fichier de travail** déclaré avec **SD** (Sort Description) au lieu de FD.

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Fichier d'entrée
           SELECT F-ENTREE ASSIGN TO 'ENTREE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-E.

      *    Fichier de travail pour le tri (SORT)
           SELECT F-TRI ASSIGN TO 'SORT.TMP'.

      *    Fichier de sortie
           SELECT F-SORTIE ASSIGN TO 'SORTIE.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-S.

       DATA DIVISION.
       FILE SECTION.
      *    Fichier d'entrée
       FD  F-ENTREE.
       01  ENR-ENTREE          PIC X(80).

      *    Fichier de travail SORT (SD au lieu de FD)
       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-CLE1        PIC X(10).
           05  TRI-CLE2        PIC 9(6).
           05  TRI-RESTE       PIC X(64).

      *    Fichier de sortie
       FD  F-SORTIE.
       01  ENR-SORTIE          PIC X(80).
```

**Points importants SD :**
- Pas de RECORDING MODE, BLOCK CONTAINS, etc.
- Structure identique aux fichiers d'entrée/sortie
- Les clés de tri doivent être définies dans la structure

---

### Instruction SORT - Syntaxe de base

```cobol
       SORT nom-fichier-tri
           ON {ASCENDING | DESCENDING} KEY nom-clé-1 [nom-clé-2 ...]
           [ON {ASCENDING | DESCENDING} KEY nom-clé-3 ...]
           [WITH DUPLICATES IN ORDER]
           {USING nom-fichier-entrée | INPUT PROCEDURE nom-paragraphe}
           {GIVING nom-fichier-sortie | OUTPUT PROCEDURE nom-paragraphe}
```

### Tri simple avec USING / GIVING

La forme la plus simple : tri direct d'un fichier.

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
      *    Tri du fichier ENTREE vers SORTIE
      *    Clé croissante : TRI-CLE1
           SORT F-TRI
               ON ASCENDING KEY TRI-CLE1
               USING F-ENTREE
               GIVING F-SORTIE

           DISPLAY 'Tri termine'
           STOP RUN.
```

**USING :** Fichier d'entrée (ouvert/fermé automatiquement)
**GIVING :** Fichier de sortie (ouvert/fermé automatiquement)

### Tri multi-clés

```cobol
      * Tri sur plusieurs clés
      * 1. DEPARTEMENT croissant
      * 2. SALAIRE décroissant (dans chaque département)
       SORT F-TRI
           ON ASCENDING KEY TRI-DEPT
           ON DESCENDING KEY TRI-SALAIRE
           USING F-EMPLOYES
           GIVING F-EMPLOYES-TRIES
```

### WITH DUPLICATES IN ORDER

Préserve l'ordre relatif des enregistrements ayant la même clé.

```cobol
       SORT F-TRI
           ON ASCENDING KEY TRI-NOM
           WITH DUPLICATES IN ORDER
           USING F-ENTREE
           GIVING F-SORTIE
```

---

## XI-2 INPUT PROCEDURE

L'**INPUT PROCEDURE** permet de traiter les enregistrements **avant** le tri (filtrage, transformation, etc.).

### Syntaxe

```cobol
       SORT F-TRI
           ON ASCENDING KEY TRI-CLE
           INPUT PROCEDURE IS 1000-PREPARATION
           GIVING F-SORTIE
```

### Instruction RELEASE

Dans l'INPUT PROCEDURE, on utilise **RELEASE** (au lieu de WRITE) pour envoyer un enregistrement au tri.

```cobol
       RELEASE nom-enregistrement-SD [FROM zone-source]
```

### Exemple : Filtrage avant tri

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           SORT F-TRI
               ON ASCENDING KEY TRI-NOM
               INPUT PROCEDURE IS 1000-FILTRER
               GIVING F-SORTIE
           DISPLAY 'Tri avec filtrage termine'
           STOP RUN.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Filtrer les enregistrements avant tri
      * Ne garder que les clients actifs (CLI-STATUT = 'A')
      *----------------------------------------------------------------*
       1000-FILTRER.
           OPEN INPUT F-ENTREE
           PERFORM UNTIL FIN-FICHIER
               READ F-ENTREE INTO WS-CLIENT
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END
                       IF CLI-STATUT = 'A'
                           MOVE WS-CLIENT TO ENR-TRI
                           RELEASE ENR-TRI
                       END-IF
               END-READ
           END-PERFORM
           CLOSE F-ENTREE.
```

### Exemple : Transformation avant tri

```cobol
       1000-TRANSFORMER.
           OPEN INPUT F-ENTREE
           PERFORM UNTIL FIN-FICHIER
               READ F-ENTREE INTO WS-EMPLOYE
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END
      *                Convertir le nom en majuscules
                       INSPECT WS-NOM
                           CONVERTING 'abcdefghijklmnopqrstuvwxyz'
                           TO         'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      *                Calculer un champ dérivé
                       COMPUTE WS-SALAIRE-ANNUEL = WS-SALAIRE * 12
      *                Envoyer au tri
                       MOVE WS-EMPLOYE TO ENR-TRI
                       RELEASE ENR-TRI
               END-READ
           END-PERFORM
           CLOSE F-ENTREE.
```

---

## XI-3 OUTPUT PROCEDURE

L'**OUTPUT PROCEDURE** permet de traiter les enregistrements **après** le tri (agrégation, édition, etc.).

### Syntaxe

```cobol
       SORT F-TRI
           ON ASCENDING KEY TRI-CLE
           USING F-ENTREE
           OUTPUT PROCEDURE IS 2000-EDITION
```

### Instruction RETURN

Dans l'OUTPUT PROCEDURE, on utilise **RETURN** (au lieu de READ) pour récupérer les enregistrements triés.

```cobol
       RETURN nom-fichier-SD [INTO zone-destination]
           AT END instruction
           [NOT AT END instruction]
       END-RETURN
```

### Exemple : Édition après tri

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           SORT F-TRI
               ON ASCENDING KEY TRI-DEPT
               ON DESCENDING KEY TRI-SALAIRE
               USING F-EMPLOYES
               OUTPUT PROCEDURE IS 2000-EDITER
           STOP RUN.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Éditer les résultats triés
      *----------------------------------------------------------------*
       2000-EDITER.
           OPEN OUTPUT F-ETAT
           WRITE LIGNE-ETAT FROM WS-TITRE
           MOVE 0 TO WS-TOTAL

           PERFORM UNTIL FIN-TRI
               RETURN F-TRI INTO WS-EMPLOYE
                   AT END SET FIN-TRI TO TRUE
                   NOT AT END
                       ADD TRI-SALAIRE TO WS-TOTAL
                       PERFORM 2100-ECRIRE-LIGNE
               END-RETURN
           END-PERFORM

           WRITE LIGNE-ETAT FROM WS-LIGNE-TOTAL
           CLOSE F-ETAT.

       2100-ECRIRE-LIGNE.
           MOVE TRI-NOM TO WS-DET-NOM
           MOVE TRI-SALAIRE TO WS-DET-SALAIRE
           WRITE LIGNE-ETAT FROM WS-LIGNE-DETAIL.
```

### Exemple : Rupture de contrôle

```cobol
       2000-RUPTURE-DEPT.
           MOVE SPACES TO WS-DEPT-PRECEDENT
           MOVE 0 TO WS-SOUS-TOTAL

           PERFORM UNTIL FIN-TRI
               RETURN F-TRI INTO WS-EMPLOYE
                   AT END
                       SET FIN-TRI TO TRUE
                       IF WS-DEPT-PRECEDENT NOT = SPACES
                           PERFORM 2200-AFFICHER-SOUS-TOTAL
                       END-IF
                   NOT AT END
      *                Détecter changement de département
                       IF TRI-DEPT NOT = WS-DEPT-PRECEDENT
                           IF WS-DEPT-PRECEDENT NOT = SPACES
                               PERFORM 2200-AFFICHER-SOUS-TOTAL
                           END-IF
                           MOVE TRI-DEPT TO WS-DEPT-PRECEDENT
                           MOVE 0 TO WS-SOUS-TOTAL
                           PERFORM 2100-AFFICHER-EN-TETE-DEPT
                       END-IF
                       ADD TRI-SALAIRE TO WS-SOUS-TOTAL
                       PERFORM 2300-AFFICHER-EMPLOYE
               END-RETURN
           END-PERFORM.
```

---

## XI-4 INPUT et OUTPUT PROCEDURE combinées

On peut utiliser les deux procédures ensemble.

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           SORT F-TRI
               ON ASCENDING KEY TRI-NOM
               INPUT PROCEDURE IS 1000-FILTRER-ACTIFS
               OUTPUT PROCEDURE IS 2000-GENERER-RAPPORT
           STOP RUN.

       1000-FILTRER-ACTIFS.
           OPEN INPUT F-CLIENTS
           PERFORM UNTIL FIN-LECTURE
               READ F-CLIENTS INTO WS-CLIENT
                   AT END SET FIN-LECTURE TO TRUE
                   NOT AT END
                       IF CLI-ACTIF = 'O'
                           MOVE WS-CLIENT TO ENR-TRI
                           RELEASE ENR-TRI
                       END-IF
               END-READ
           END-PERFORM
           CLOSE F-CLIENTS.

       2000-GENERER-RAPPORT.
           OPEN OUTPUT F-RAPPORT
           PERFORM UNTIL FIN-TRI
               RETURN F-TRI INTO WS-CLIENT
                   AT END SET FIN-TRI TO TRUE
                   NOT AT END
                       PERFORM 2100-FORMATER-LIGNE
                       WRITE LIGNE-RAPPORT FROM WS-LIGNE
               END-RETURN
           END-PERFORM
           CLOSE F-RAPPORT.
```

---

## XI-5 MERGE (Fusion)

L'instruction **MERGE** fusionne plusieurs fichiers **déjà triés** en un seul fichier trié.

```
┌─────────────────┐
│ Fichier 1 trié  │──┐
└─────────────────┘  │     ┌──────────┐      ┌─────────────────┐
                     ├───► │  MERGE   │ ───► │ Fichier Sortie  │
┌─────────────────┐  │     │          │      │    (trié)       │
│ Fichier 2 trié  │──┘     └──────────┘      └─────────────────┘
└─────────────────┘
```

### Syntaxe MERGE

```cobol
       MERGE nom-fichier-tri
           ON {ASCENDING | DESCENDING} KEY nom-clé [...]
           [WITH DUPLICATES IN ORDER]
           USING nom-fichier-1 nom-fichier-2 [nom-fichier-3 ...]
           {GIVING nom-fichier-sortie | OUTPUT PROCEDURE nom-paragraphe}
```

**Note :** MERGE n'a pas d'INPUT PROCEDURE (les fichiers doivent être pré-triés).

### Exemple MERGE

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PARIS ASSIGN TO 'CLIENTS-PARIS.DAT'.
           SELECT F-LYON ASSIGN TO 'CLIENTS-LYON.DAT'.
           SELECT F-MERGE ASSIGN TO 'MERGE.TMP'.
           SELECT F-TOUS ASSIGN TO 'CLIENTS-TOUS.DAT'.

       DATA DIVISION.
       FILE SECTION.
       FD  F-PARIS.
       01  ENR-PARIS           PIC X(100).

       FD  F-LYON.
       01  ENR-LYON            PIC X(100).

       SD  F-MERGE.
       01  ENR-MERGE.
           05  MRG-CODE        PIC X(10).
           05  MRG-NOM         PIC X(30).
           05  MRG-RESTE       PIC X(60).

       FD  F-TOUS.
       01  ENR-TOUS            PIC X(100).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
      *    Fusionner les fichiers Paris et Lyon
      *    Les deux fichiers doivent être pré-triés sur MRG-CODE
           MERGE F-MERGE
               ON ASCENDING KEY MRG-CODE
               USING F-PARIS F-LYON
               GIVING F-TOUS

           DISPLAY 'Fusion terminee'
           STOP RUN.
```

### MERGE avec OUTPUT PROCEDURE

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           MERGE F-MERGE
               ON ASCENDING KEY MRG-CODE
               USING F-PARIS F-LYON F-MARSEILLE
               OUTPUT PROCEDURE IS 2000-ELIMINER-DOUBLONS
           STOP RUN.

      *----------------------------------------------------------------*
      * Éliminer les doublons lors de la fusion
      *----------------------------------------------------------------*
       2000-ELIMINER-DOUBLONS.
           OPEN OUTPUT F-RESULTAT
           MOVE SPACES TO WS-CODE-PRECEDENT

           PERFORM UNTIL FIN-MERGE
               RETURN F-MERGE INTO WS-CLIENT
                   AT END SET FIN-MERGE TO TRUE
                   NOT AT END
      *                Ne garder que si code différent du précédent
                       IF MRG-CODE NOT = WS-CODE-PRECEDENT
                           WRITE ENR-RESULTAT FROM WS-CLIENT
                           MOVE MRG-CODE TO WS-CODE-PRECEDENT
                       END-IF
               END-RETURN
           END-PERFORM

           CLOSE F-RESULTAT.
```

---

## XI-6 Récapitulatif

### Instructions de tri

| Instruction | Usage | Contexte |
|-------------|-------|----------|
| `SORT` | Tri d'un fichier | PROCEDURE DIVISION |
| `MERGE` | Fusion de fichiers pré-triés | PROCEDURE DIVISION |
| `RELEASE` | Envoyer un enregistrement au tri | INPUT PROCEDURE |
| `RETURN` | Récupérer un enregistrement trié | OUTPUT PROCEDURE |

### Déclaration fichier de travail

| Type | Usage | Clauses |
|------|-------|---------|
| `FD` | Fichier standard (entrée/sortie) | RECORDING MODE, BLOCK, etc. |
| `SD` | Fichier de travail SORT/MERGE | Aucune clause supplémentaire |

### Ordre des clés

| Clause | Ordre |
|--------|-------|
| `ASCENDING KEY` | Croissant (A→Z, 0→9) |
| `DESCENDING KEY` | Décroissant (Z→A, 9→0) |

### Comparaison USING/GIVING vs PROCEDURE

| Méthode | Avantage | Inconvénient |
|---------|----------|--------------|
| USING/GIVING | Simple, automatique | Pas de traitement |
| INPUT PROCEDURE | Filtrage, transformation | Plus complexe |
| OUTPUT PROCEDURE | Agrégation, édition | Plus complexe |

---

## Exemple complet

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRI-EMPLOYES.
      ******************************************************************
      * Tri des employés par département puis salaire décroissant
      * - Filtrage : employés actifs uniquement
      * - Édition : rapport avec sous-totaux par département
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLOYES ASSIGN TO 'EMPLOYES.DAT'
               FILE STATUS IS WS-STATUS.
           SELECT F-TRI ASSIGN TO 'SORT.TMP'.
           SELECT F-RAPPORT ASSIGN TO 'RAPPORT.TXT'
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-EMPLOYES.
       01  ENR-EMPLOYE         PIC X(80).

       SD  F-TRI.
       01  ENR-TRI.
           05  TRI-MATRICULE   PIC 9(6).
           05  TRI-NOM         PIC X(20).
           05  TRI-DEPT        PIC X(10).
           05  TRI-SALAIRE     PIC 9(6).
           05  TRI-STATUT      PIC X.
           05  FILLER          PIC X(37).

       FD  F-RAPPORT.
       01  LIGNE-RAPPORT       PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-STATUS           PIC XX.
       01  WS-FIN-LECTURE      PIC X VALUE 'N'.
           88 FIN-LECTURE      VALUE 'O'.
       01  WS-FIN-TRI          PIC X VALUE 'N'.
           88 FIN-TRI          VALUE 'O'.
       01  WS-DEPT-PREC        PIC X(10) VALUE SPACES.
       01  WS-SOUS-TOTAL       PIC 9(8) VALUE 0.
       01  WS-TOTAL-GENERAL    PIC 9(10) VALUE 0.
       01  WS-CPT-DEPT         PIC 9(4) VALUE 0.

       01  WS-EMPLOYE.
           05  WS-MATRICULE    PIC 9(6).
           05  WS-NOM          PIC X(20).
           05  WS-DEPT         PIC X(10).
           05  WS-SALAIRE      PIC 9(6).
           05  WS-STATUT       PIC X.
           05  FILLER          PIC X(37).

       01  WS-LIGNE-TITRE.
           05  FILLER          PIC X(30) VALUE
               '*** RAPPORT EMPLOYES ACTIFS **'.
           05  FILLER          PIC X(50) VALUE SPACES.

       01  WS-LIGNE-DEPT.
           05  FILLER          PIC X(15) VALUE 'DEPARTEMENT : '.
           05  WS-LD-DEPT      PIC X(10).
           05  FILLER          PIC X(55) VALUE SPACES.

       01  WS-LIGNE-DETAIL.
           05  FILLER          PIC X(3) VALUE SPACES.
           05  WS-LD-MAT       PIC 9(6).
           05  FILLER          PIC X(2) VALUE SPACES.
           05  WS-LD-NOM       PIC X(20).
           05  FILLER          PIC X(2) VALUE SPACES.
           05  WS-LD-SAL       PIC ZZZ.ZZ9.
           05  FILLER          PIC X(40) VALUE SPACES.

       01  WS-LIGNE-STOTAL.
           05  FILLER          PIC X(20) VALUE
               '   Sous-total : '.
           05  WS-LS-TOTAL     PIC ZZZ.ZZZ.ZZ9.
           05  FILLER          PIC X(5) VALUE ' EUR'.
           05  FILLER          PIC X(42) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           SORT F-TRI
               ON ASCENDING KEY TRI-DEPT
               ON DESCENDING KEY TRI-SALAIRE
               INPUT PROCEDURE IS 1000-FILTRER
               OUTPUT PROCEDURE IS 2000-EDITER

           DISPLAY 'Rapport genere avec succes'
           STOP RUN.

      *----------------------------------------------------------------*
      * INPUT PROCEDURE : Filtrer les employés actifs
      *----------------------------------------------------------------*
       1000-FILTRER.
           OPEN INPUT F-EMPLOYES
           PERFORM UNTIL FIN-LECTURE
               READ F-EMPLOYES INTO WS-EMPLOYE
                   AT END SET FIN-LECTURE TO TRUE
                   NOT AT END
                       IF WS-STATUT = 'A'
                           MOVE WS-EMPLOYE TO ENR-TRI
                           RELEASE ENR-TRI
                       END-IF
               END-READ
           END-PERFORM
           CLOSE F-EMPLOYES.

      *----------------------------------------------------------------*
      * OUTPUT PROCEDURE : Générer le rapport avec ruptures
      *----------------------------------------------------------------*
       2000-EDITER.
           OPEN OUTPUT F-RAPPORT
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-TITRE
           MOVE SPACES TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           PERFORM UNTIL FIN-TRI
               RETURN F-TRI INTO WS-EMPLOYE
                   AT END
                       SET FIN-TRI TO TRUE
                       IF WS-DEPT-PREC NOT = SPACES
                           PERFORM 2200-SOUS-TOTAL
                       END-IF
                   NOT AT END
                       IF WS-DEPT NOT = WS-DEPT-PREC
                           IF WS-DEPT-PREC NOT = SPACES
                               PERFORM 2200-SOUS-TOTAL
                           END-IF
                           PERFORM 2100-EN-TETE-DEPT
                       END-IF
                       PERFORM 2300-LIGNE-EMPLOYE
               END-RETURN
           END-PERFORM

           CLOSE F-RAPPORT.

       2100-EN-TETE-DEPT.
           MOVE WS-DEPT TO WS-DEPT-PREC
           MOVE WS-DEPT TO WS-LD-DEPT
           MOVE 0 TO WS-SOUS-TOTAL
           MOVE 0 TO WS-CPT-DEPT
           MOVE SPACES TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-DEPT.

       2200-SOUS-TOTAL.
           MOVE WS-SOUS-TOTAL TO WS-LS-TOTAL
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-STOTAL
           ADD WS-SOUS-TOTAL TO WS-TOTAL-GENERAL.

       2300-LIGNE-EMPLOYE.
           MOVE WS-MATRICULE TO WS-LD-MAT
           MOVE WS-NOM TO WS-LD-NOM
           MOVE WS-SALAIRE TO WS-LD-SAL
           WRITE LIGNE-RAPPORT FROM WS-LIGNE-DETAIL
           ADD WS-SALAIRE TO WS-SOUS-TOTAL
           ADD 1 TO WS-CPT-DEPT.
```

---

## Résumé

| Concept | Instruction | Description |
|---------|-------------|-------------|
| Fichier de travail | `SD` | Déclaration du fichier temporaire de tri |
| Tri | `SORT ... USING ... GIVING` | Tri direct sans traitement |
| Tri avec filtre | `SORT ... INPUT PROCEDURE` | Traitement avant tri |
| Tri avec édition | `SORT ... OUTPUT PROCEDURE` | Traitement après tri |
| Envoi au tri | `RELEASE` | Dans INPUT PROCEDURE |
| Réception du tri | `RETURN` | Dans OUTPUT PROCEDURE |
| Fusion | `MERGE ... USING ... GIVING` | Fusion de fichiers pré-triés |
| Ordre croissant | `ASCENDING KEY` | A→Z, 0→9 |
| Ordre décroissant | `DESCENDING KEY` | Z→A, 9→0 |
