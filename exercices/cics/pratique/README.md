# Exercices Pratiques CICS

Ce dossier contient les exercices pratiques CICS alignes avec le support de formation (Chapitre IX - Architecture Multicouches).

## Structure

```
pratique/
├── bms/            # Ecrans BMS (MAPs)
│   ├── MAPTEST.bms    # Ecran de bienvenue (Couche Presentation)
│   ├── MAPREAD.bms    # MAP pour BROWSE basique (PGSTART)
│   ├── MAPPATH.bms    # MAP pour BROWSE avec AIX (PGPATHF)
│   └── TESTSET.bms    # Mapset pour les exercices VSAM
│
├── cobol/          # Programmes COBOL-CICS
│   ├── PROGTEST.cbl   # Affichage MAPTEST (Couche Presentation)
│   ├── PROGREAD.cbl   # Programme READ (lecture)
│   ├── PROGWRIT.cbl   # Programme WRITE (creation)
│   ├── PROGREWT.cbl   # Programme REWRITE (mise a jour)
│   ├── PROGDELT.cbl   # Programme DELETE (suppression)
│   ├── PGSTART.cbl    # BROWSE basique (STARTBR/READNEXT)
│   └── PGPATHF.cbl    # BROWSE avec AIX (Alternate Index)
│
├── copybooks/      # Copybooks COBOL
│   └── MAPTEST.cpy    # DSECT genere par BMS
│
└── jcl/            # Jobs JCL
    ├── ASSBLMAP.jcl   # Assemblage des MAPs BMS
    ├── COMPPGR.jcl    # Compilation programme COBOL-CICS
    ├── DEFVSAM.jcl    # Definition fichier VSAM
    ├── DEFPATH.jcl    # Definition AIX et PATH pour PGPATHF
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

### 3. Browse VSAM (Chapitre VIII - Exercices 13 et 14)

| Programme | Transaction | Description | Fichiers |
|-----------|-------------|-------------|----------|
| PGSTART | TSRT | BROWSE avec STARTBR/READNEXT/ENDBR (cle primaire) | `cobol/PGSTART.cbl`, `bms/MAPREAD.bms` |
| PGPATHF | TPTH | BROWSE avec ALTERNATE INDEX (cle NOMCPT) | `cobol/PGPATHF.cbl`, `bms/MAPPATH.bms` |

#### Exercice 13 - BROWSE basique (PGSTART)
- Parcours sequentiel avec cle generique
- Commandes : STARTBR, READNEXT, ENDBR
- Lit 3 enregistrements a partir d'une cle de depart

#### Exercice 14 - BROWSE avec AIX (PGPATHF)
- Parcours via ALTERNATE INDEX sur le champ NOMCPT
- Pre-requis : AIX et PATH definis (`jcl/DEFPATH.jcl`)
- Recherche par nom de client

### 4. Transactions avancees (Chapitre VII)

| Exercice | Description | Status |
|----------|-------------|--------|
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

### Definir les transactions BROWSE (TSRT et TPTH)
```
* Exercice 13 - BROWSE basique
CEDA DEF TRANS(TSRT) GROUP(GRPTEST) PROG(PGSTART)
CEDA DEF PROG(PGSTART) GROUP(GRPTEST) LANG(COBOL)

* Exercice 14 - BROWSE avec AIX
CEDA DEF TRANS(TPTH) GROUP(GRPTEST) PROG(PGPATHF)
CEDA DEF PROG(PGPATHF) GROUP(GRPTEST) LANG(COBOL)
CEDA DEF MAPSET(MAPPATH) GROUP(GRPTEST)

* Definition du fichier PATH pour AIX
CEDA DEF FILE(PCLIENT) GROUP(GRPTEST) -
     DSN(FTEST.CICS.FCLIENT.PATH) -
     RLSACCESS(NO) RECOVERY(NONE) -
     STATUS(ENABLED) OPENTIME(FIRSTREF)

CEDA INS GROUP(GRPTEST)
```

---
*Formation CICS - M2i Formation*
