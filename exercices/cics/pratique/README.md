# Exercices Pratiques CICS

Ce dossier contient les exercices pratiques CICS alignés avec le support de formation (Chapitre IX - Architecture Multicouches).

## Structure

```
pratique/
├── bms/            # Écrans BMS (MAPs)
│   ├── MAPTEST.bms    # Écran de bienvenue (Couche Présentation)
│   ├── MAPREAD.bms    # MAP pour BROWSE basique (PGSTART)
│   ├── MAPPATH.bms    # MAP pour BROWSE avec AIX (PGPATHF)
│   ├── MAPWRIT.bms    # MAP pour écriture
│   ├── MAPIO.bms      # MAP entrées/sorties
│   ├── MAPIO1.bms     # MAP entrées/sorties (variante)
│   ├── MAPNAME.bms    # MAP recherche par nom
│   └── MDELG.bms      # MAP suppression générique
│
├── cobol/          # Programmes COBOL-CICS
│   ├── PROGTEST.cbl   # Affichage MAPTEST (Couche Présentation)
│   ├── PROGREAD.cbl   # Programme READ (lecture)
│   ├── PRGWRIT.cbl    # Programme WRITE (création)
│   ├── PRGREWR.cbl    # Programme REWRITE (mise à jour)
│   ├── PRGRDEL.cbl    # Programme DELETE (suppression)
│   ├── PGSTART.cbl    # BROWSE basique (STARTBR/READNEXT)
│   ├── PGPATHF.cbl    # BROWSE avec AIX (Alternate Index)
│   ├── PROGIO.cbl     # Programme E/S générique
│   ├── PROGIO1.cbl    # Programme E/S (variante)
│   ├── PROGNAME.cbl   # Programme recherche par nom
│   ├── PRGRGEN.cbl    # Programme lecture générique
│   ├── PDELGEN.cbl    # Programme suppression générique
│   └── PRWRSPL.cbl    # Programme écriture spéciale
│
├── copybooks/      # Copybooks COBOL
│   └── CLIENT.cpy     # Structure enregistrement client
│
└── jcl/            # Jobs JCL
    ├── ASSBLMAP.jcl   # Assemblage des MAPs BMS
    ├── COMPPGR.jcl    # Compilation programme COBOL-CICS
    ├── DEFVSAM.jcl    # Définition fichier VSAM
    ├── DEFFCLI.jcl    # Définition fichier CLIENT
    ├── DEFPATH.jcl    # Définition AIX et PATH pour PGPATHF
    └── LOADDATA.jcl   # Chargement données initiales
```

## Correspondance avec le support de formation

| Support | Fichier | Description |
|---------|---------|-------------|
| MAPTEST | `bms/MAPTEST.bms` | MAP écran de bienvenue |
| PROGTEST | `cobol/PROGTEST.cbl` | Programme affichage MAP |
| ASSBLMAP | `jcl/ASSBLMAP.jcl` | JCL assemblage MAP |
| COMPPGR | `jcl/COMPPGR.jcl` | JCL compilation programme |

## Exercices par thème

### 1. Couche Présentation (II-1 du support)

| Composant | Transaction | Description | Fichiers |
|-----------|-------------|-------------|----------|
| MAPTEST | - | Écran de bienvenue CICS | `bms/MAPTEST.bms` |
| PROGTEST | TR01 | Programme affichant MAPTEST | `cobol/PROGTEST.cbl` |

### 2. Commandes VSAM (Chapitre VI)

| Programme | Transaction | Description |
|-----------|-------------|-------------|
| PROGREAD | READ | Lecture d'un enregistrement par clé |
| PRGWRIT | WRIT | Création d'un nouvel enregistrement |
| PRGREWR | REWT | Mise à jour (READ UPDATE + REWRITE) |
| PRGRDEL | DELT | Suppression d'un enregistrement |

### 3. Browse VSAM (Chapitre VIII - Exercices 13 et 14)

| Programme | Transaction | Description | Fichiers |
|-----------|-------------|-------------|----------|
| PGSTART | TSRT | BROWSE avec STARTBR/READNEXT/ENDBR (clé primaire) | `cobol/PGSTART.cbl`, `bms/MAPREAD.bms` |
| PGPATHF | TPTH | BROWSE avec ALTERNATE INDEX (clé NOMCPT) | `cobol/PGPATHF.cbl`, `bms/MAPPATH.bms` |

#### Exercice 13 - BROWSE basique (PGSTART)
- Parcours séquentiel avec clé générique
- Commandes : STARTBR, READNEXT, ENDBR
- Lit 3 enregistrements à partir d'une clé de départ

#### Exercice 14 - BROWSE avec AIX (PGPATHF)
- Parcours via ALTERNATE INDEX sur le champ NOMCPT
- Prérequis : AIX et PATH définis (`jcl/DEFPATH.jcl`)
- Recherche par nom de client

### 4. Transactions avancées (Chapitre VII)

| Exercice | Description | Status |
|----------|-------------|--------|
| Pagination | Navigation page suivante/précédente | Théorique |
| Virement | SYNCPOINT/ROLLBACK | Théorique |

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

Le fichier EMPLOYE est utilisé pour les exercices :

| Champ | Type | Longueur | Description |
|-------|------|----------|-------------|
| EMP-ID | X | 6 | Matricule (clé) |
| EMP-NOM | X | 20 | Nom |
| EMP-PRENOM | X | 15 | Prénom |
| EMP-DEPT | X | 4 | Code département |
| EMP-SALAIRE | 9V99 | 7 | Salaire |

Voir `jcl/DEFVSAM.jcl` pour la définition et `jcl/LOADDATA.jcl` pour le chargement.

## Progression recommandée

```
1. COUCHE PRÉSENTATION - Afficher un écran
   a. Assembler MAPTEST.bms avec ASSBLMAP.jcl
   b. Compiler PROGTEST.cbl avec COMPPGR.jcl
   c. Définir transaction TR01 dans CICS
   d. Tester : entrer TR01 dans le terminal CICS

2. READ - Lire un enregistrement
   PROGREAD.cbl → Compiler → Transaction READ

3. WRITE - Créer un enregistrement
   PRGWRIT.cbl → Compiler → Transaction WRIT

4. REWRITE - Modifier un enregistrement
   PRGREWR.cbl → Compiler → Transaction REWT

5. DELETE - Supprimer un enregistrement
   PRGRDEL.cbl → Compiler → Transaction DELT
```

## Installation CICS

### Définir la transaction TR01
```
CEDA DEF TRANS(TR01) GROUP(GRPTEST) PROG(PROGTEST)
CEDA DEF PROG(PROGTEST) GROUP(GRPTEST) LANG(COBOL)
CEDA DEF MAPSET(MAPTEST) GROUP(GRPTEST)
CEDA INS GROUP(GRPTEST)
```

### Définir les transactions BROWSE (TSRT et TPTH)
```
* Exercice 13 - BROWSE basique
CEDA DEF TRANS(TSRT) GROUP(GRPTEST) PROG(PGSTART)
CEDA DEF PROG(PGSTART) GROUP(GRPTEST) LANG(COBOL)

* Exercice 14 - BROWSE avec AIX
CEDA DEF TRANS(TPTH) GROUP(GRPTEST) PROG(PGPATHF)
CEDA DEF PROG(PGPATHF) GROUP(GRPTEST) LANG(COBOL)
CEDA DEF MAPSET(MAPPATH) GROUP(GRPTEST)

* Définition du fichier PATH pour AIX
CEDA DEF FILE(PCLIENT) GROUP(GRPTEST) -
     DSN(FTEST.CICS.FCLIENT.PATH) -
     RLSACCESS(NO) RECOVERY(NONE) -
     STATUS(ENABLED) OPENTIME(FIRSTREF)

CEDA INS GROUP(GRPTEST)
```

---
*Formation CICS - M2i Formation*
