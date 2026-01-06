# Exercices Pratiques CICS

Ce dossier contient les exercices pratiques CICS alignes avec le support de formation (Chapitre IX - Architecture Multicouches).

## Structure

```
pratique/
├── bms/            # Ecrans BMS (MAPs)
│   ├── MAPTEST.bms    # Ecran de bienvenue (Couche Presentation)
│   └── TESTSET.bms    # Mapset pour les exercices VSAM
│
├── cobol/          # Programmes COBOL-CICS
│   ├── PROGTEST.cbl   # Affichage MAPTEST (Couche Presentation)
│   ├── PROGREAD.cbl   # Programme READ (lecture)
│   ├── PROGWRIT.cbl   # Programme WRITE (creation)
│   ├── PROGREWT.cbl   # Programme REWRITE (mise a jour)
│   └── PROGDELT.cbl   # Programme DELETE (suppression)
│
├── copybooks/      # Copybooks COBOL
│   └── MAPTEST.cpy    # DSECT genere par BMS
│
└── jcl/            # Jobs JCL
    ├── ASSBLMAP.jcl   # Assemblage des MAPs BMS
    ├── COMPPGR.jcl    # Compilation programme COBOL-CICS
    ├── DEFVSAM.jcl    # Definition fichier VSAM
    └── LOADDATA.jcl   # Chargement donnees initiales
```

## Correspondance avec le support de formation

| Support | Fichier | Description |
|---------|---------|-------------|
| MAPTEST | `bms/MAPTEST.bms` | MAP ecran de bienvenue |
| PROGTEST | `cobol/PROGTEST.cbl` | Programme affichage MAP |
| ASSBLMAP | `jcl/ASSBLMAP.jcl` | JCL assemblage MAP |
| COMPPGR | `jcl/COMPPGR.jcl` | JCL compilation programme |

## Exercices par theme

### 1. Couche Presentation (II-1 du support)

| Composant | Transaction | Description | Fichiers |
|-----------|-------------|-------------|----------|
| MAPTEST | - | Ecran de bienvenue CICS | `bms/MAPTEST.bms` |
| PROGTEST | TR01 | Programme affichant MAPTEST | `cobol/PROGTEST.cbl` |

### 2. Commandes VSAM (Chapitre VI)

| Programme | Transaction | Description |
|-----------|-------------|-------------|
| PROGREAD | READ | Lecture d'un enregistrement par cle |
| PROGWRIT | WRIT | Creation d'un nouvel enregistrement |
| PROGREWT | REWT | Mise a jour (READ UPDATE + REWRITE) |
| PROGDELT | DELT | Suppression d'un enregistrement |

### 3. Browse et Transactions (Chapitre VII)

| Exercice | Description | Status |
|----------|-------------|--------|
| Parcours | STARTBR/READNEXT/ENDBR | Theorique |
| Pagination | Navigation page suivante/precedente | Theorique |
| Virement | SYNCPOINT/ROLLBACK | Theorique |

## Compilation

### Assembler une MAP BMS

```jcl
//ASMMAP   EXEC PGM=DFHEAP1$
//...
```

Voir `jcl/ASSBLMAP.jcl` pour le JCL complet.

### Compiler un programme COBOL-CICS

```jcl
//COBCICS  EXEC PROC=DFHYITVL
//...
```

Voir `jcl/COMPPGR.jcl` pour le JCL complet.

## Fichier VSAM de test

Le fichier EMPLOYE est utilise pour les exercices :

| Champ | Type | Longueur | Description |
|-------|------|----------|-------------|
| EMP-ID | X | 6 | Matricule (cle) |
| EMP-NOM | X | 20 | Nom |
| EMP-PRENOM | X | 15 | Prenom |
| EMP-DEPT | X | 4 | Code departement |
| EMP-SALAIRE | 9V99 | 7 | Salaire |

Voir `jcl/DEFVSAM.jcl` pour la definition et `jcl/LOADDATA.jcl` pour le chargement.

## Progression recommandee

```
1. COUCHE PRESENTATION - Afficher un ecran
   a. Assembler MAPTEST.bms avec ASSBLMAP.jcl
   b. Compiler PROGTEST.cbl avec COMPPGR.jcl
   c. Definir transaction TR01 dans CICS
   d. Tester : entrer TR01 dans le terminal CICS

2. READ - Lire un enregistrement
   PROGREAD.cbl → Compiler → Transaction READ

3. WRITE - Creer un enregistrement
   PROGWRIT.cbl → Compiler → Transaction WRIT

4. REWRITE - Modifier un enregistrement
   PROGREWT.cbl → Compiler → Transaction REWT

5. DELETE - Supprimer un enregistrement
   PROGDELT.cbl → Compiler → Transaction DELT
```

## Installation CICS

### Definir la transaction TR01
```
CEDA DEF TRANS(TR01) GROUP(GRPTEST) PROG(PROGTEST)
CEDA DEF PROG(PROGTEST) GROUP(GRPTEST) LANG(COBOL)
CEDA DEF MAPSET(MAPTEST) GROUP(GRPTEST)
CEDA INS GROUP(GRPTEST)
```

---
*Formation CICS - M2i Formation*
