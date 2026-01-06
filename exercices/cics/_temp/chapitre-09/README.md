# Chapitre IX - Exercices Pratiques CICS

Ce dossier contient les exercices pratiques du Chapitre IX : Architecture Multicouches et Transactions TSI.

## Structure des fichiers

```
chapitre-09/
├── bms/                    # Ecrans BMS
│   ├── MAPIO.bms             # MAP echange de donnees I/O
│   ├── MAPREAD.bms           # MAP lecture employe
│   ├── MAPWRIT.bms           # MAP creation employe
│   └── MAPBRWS.bms           # MAP liste (Browse)
│
├── cobol/                  # Programmes COBOL-CICS
│   ├── PROGIO.cbl            # Echange donnees via MAP
│   ├── PRGREAD.cbl           # Lecture VSAM avec MAP
│   ├── PRGRGEN.cbl           # Lecture generique (EQUAL/GTEQ/GENERIC)
│   ├── PRGWRIT.cbl           # Ecriture VSAM avec MAP
│   ├── PGSTART.cbl           # Browse (STARTBR/READNEXT/ENDBR)
│   └── PGPATHF.cbl           # Alternate Index (PATH)
│
├── copybooks/              # Copybooks COBOL
│   └── EMPLOYE.cpy           # Structure enregistrement EMPLOYE
│
└── jcl/                    # Jobs JCL
    ├── DEFVSAM.jcl           # Definition fichier VSAM + AIX
    └── COMPALL.jcl           # Compilation de tous les programmes
```

## Correspondance avec le support du formateur

| Support Formateur | Fichier | Description |
|-------------------|---------|-------------|
| MAPIO / MAPIO1 | `bms/MAPIO.bms` | MAP echange de donnees I/O |
| PROGIO / PROGIO1 | `cobol/PROGIO.cbl` | Programme echange via SEND/RECEIVE MAP |
| MAPREAD | `bms/MAPREAD.bms` | MAP pour la lecture |
| PRGREAD | `cobol/PRGREAD.cbl` | Programme READ avec interface MAP |
| PRGRGEN | `cobol/PRGRGEN.cbl` | Lecture generique (modes EQUAL/GTEQ/GENERIC) |
| MAPWRIT | `bms/MAPWRIT.bms` | MAP pour l'ecriture |
| PRGWRIT | `cobol/PRGWRIT.cbl` | Programme WRITE avec interface MAP |
| PGSTART | `cobol/PGSTART.cbl` | Browse avec STARTBR/READNEXT/READPREV/ENDBR |
| PGPATHF | `cobol/PGPATHF.cbl` | Acces via Alternate Index (PATH) |

## Transactions

| Transaction | Programme | Description |
|-------------|-----------|-------------|
| TIO | PROGIO | Echange de donnees I/O |
| TREA | PRGREAD | Lecture employe |
| TGEN | PRGRGEN | Lecture generique |
| TWRT | PRGWRIT | Creation employe |
| TBRW | PGSTART | Liste employes (Browse) |
| TALT | PGPATHF | Recherche par nom (AIX) |

## Concepts illustres

### 1. Echange de donnees via MAP (PROGIO)
- SEND MAP avec ERASE et FREEKB
- RECEIVE MAP avec gestion MAPFAIL
- Mode pseudo-conversationnel avec COMMAREA
- Gestion des touches (DFHAID)

### 2. Operations VSAM avec interface ecran
- READ avec affichage dans une MAP
- WRITE avec validation des champs
- Gestion des erreurs (NOTFND, DUPREC, NOTOPEN)

### 3. Lecture generique (PRGRGEN)
- Mode EQUAL : cle exacte
- Mode GTEQ : cle >= (premier enregistrement)
- Mode GENERIC : cle partielle (prefixe)

### 4. Navigation Browse (PGSTART)
- STARTBR : demarrer le parcours
- READNEXT : lire suivant
- READPREV : lire precedent
- RESETBR : repositionner
- ENDBR : terminer le parcours
- Pagination (PF7/PF8)

### 5. Alternate Index (PGPATHF)
- Acces via PATH (index alternatif)
- Recherche par nom au lieu de code
- Gestion des doublons (DUPKEY)

## Installation CICS

### 1. Definir les MAPs
```
CEDA DEF MAPSET(MAPIO) GROUP(GRPCH09)
CEDA DEF MAPSET(MAPREAD) GROUP(GRPCH09)
CEDA DEF MAPSET(MAPWRIT) GROUP(GRPCH09)
CEDA DEF MAPSET(MAPBRWS) GROUP(GRPCH09)
```

### 2. Definir les programmes
```
CEDA DEF PROG(PROGIO) GROUP(GRPCH09) LANG(COBOL)
CEDA DEF PROG(PRGREAD) GROUP(GRPCH09) LANG(COBOL)
CEDA DEF PROG(PRGRGEN) GROUP(GRPCH09) LANG(COBOL)
CEDA DEF PROG(PRGWRIT) GROUP(GRPCH09) LANG(COBOL)
CEDA DEF PROG(PGSTART) GROUP(GRPCH09) LANG(COBOL)
CEDA DEF PROG(PGPATHF) GROUP(GRPCH09) LANG(COBOL)
```

### 3. Definir les transactions
```
CEDA DEF TRANS(TIO) GROUP(GRPCH09) PROG(PROGIO)
CEDA DEF TRANS(TREA) GROUP(GRPCH09) PROG(PRGREAD)
CEDA DEF TRANS(TGEN) GROUP(GRPCH09) PROG(PRGRGEN)
CEDA DEF TRANS(TWRT) GROUP(GRPCH09) PROG(PRGWRIT)
CEDA DEF TRANS(TBRW) GROUP(GRPCH09) PROG(PGSTART)
CEDA DEF TRANS(TALT) GROUP(GRPCH09) PROG(PGPATHF)
```

### 4. Definir les fichiers
```
CEDA DEF FILE(EMPLOYE) GROUP(GRPCH09) DSNAME(hlq.EMPLOYE.KSDS)
CEDA DEF FILE(EMPNOM) GROUP(GRPCH09) DSNAME(hlq.EMPLOYE.PATH.NOM)
```

### 5. Installer le groupe
```
CEDA INS GROUP(GRPCH09)
```

## Tests avec CEDF

Pour deboguer une transaction :
```
CEDF termid
```

Puis lancer la transaction (ex: TIO)

## Tests avec CECI

Tester l'envoi d'une MAP :
```
CECI SEND MAP('MAP1') MAPSET('MAPIO') ERASE
```

Tester la lecture VSAM :
```
CECI READ FILE('EMPLOYE') RIDFLD('EMP001') INTO(&REC)
```

---
*Formation CICS - Chapitre IX - M2i Formation*
