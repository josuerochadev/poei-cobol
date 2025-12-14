# Exercices Pratiques CICS

Ce dossier contient les exercices pratiques CICS organises par type de ressource.

## Structure

```
pratique/
├── bms/            # Ecrans BMS
│   ├── MAPTEST.bms    # Ecran de test simple (message)
│   └── TESTSET.bms    # Mapset pour les exercices
│
├── cobol/          # Programmes COBOL-CICS
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

## Exercices par theme

### 1. BMS - Basic Mapping Support (Chapitre V)

| Exercice | Description | Fichiers |
|----------|-------------|----------|
| MAPTEST | Afficher un message simple | `bms/MAPTEST.bms` |
| MAP NOM | Saisir un nom et le reafficher | A creer |
| WELCOMES | Ecran d'accueil avec date/heure | A creer |

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
1. BMS - Creer un ecran simple
   MAPTEST.bms → Compiler → Tester avec CEDF

2. READ - Lire un enregistrement
   PROGREAD.cbl → Compiler → Transaction READ

3. WRITE - Creer un enregistrement
   PROGWRIT.cbl → Compiler → Transaction WRIT

4. REWRITE - Modifier un enregistrement
   PROGREWT.cbl → Compiler → Transaction REWT

5. DELETE - Supprimer un enregistrement
   PROGDELT.cbl → Compiler → Transaction DELT
```

---
*Formation CICS - M2i Formation*
