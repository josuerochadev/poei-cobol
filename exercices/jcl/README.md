# Exercices JCL

## Organisation

Les exercices sont organises en deux parties :
- **Theorie** : QCM pour valider les connaissances theoriques
- **Pratique** : Exercices hands-on avec fichiers `.jcl` executables

---

## Partie Theorique (QCM)

Les QCM sont organises par chapitre du cours et permettent de valider la comprehension des concepts.

| Fichier | Chapitre | Nombre de questions |
|---------|----------|---------------------|
| `theorie/qcm-01-cartes-jcl.md` | I - Cartes JOB, EXEC, DD | 30 questions |
| `theorie/qcm-02-fichiers-parametres.md` | II - Fichiers et parametres | 25 questions |
| `theorie/qcm-03-procedures.md` | III - Procedures JCL | 25 questions |
| `theorie/qcm-04-utilitaires.md` | IV - Utilitaires et SORT | 35 questions |

**Total : 115 questions**

### Themes couverts par QCM

#### QCM-01 : Cartes JCL
- Instructions JOB, EXEC, DD
- Parametres CLASS, MSGCLASS, MSGLEVEL, NOTIFY
- DISP, DCB, SPACE
- COND et codes retour
- JOBLIB, STEPLIB, JCLLIB

#### QCM-02 : Fichiers et parametres
- Types de fichiers (PS, PO, PDSE)
- Concatenation et references arriere
- Fichiers temporaires (&&)
- DUMMY, SYSOUT, donnees in-stream
- Parametres avances (VIO, LIKE, OUTLIM)

#### QCM-03 : Procedures
- Procedures in-stream et cataloguees
- Parametres symboliques (&)
- Symboles systeme (&SYSUID, &LYYMMDD)
- Override (stepname.ddname)
- INCLUDE et imbrication

#### QCM-04 : Utilitaires et SORT
- IEFBR14, IEBGENER, IEBCOPY, IEBCOMPR
- IDCAMS (DEFINE, DELETE, REPRO, LISTCAT)
- SORT (FIELDS, INCLUDE/OMIT, INREC/OUTREC)
- OUTFIL, MERGE, FINDREP
- Codes ABEND (S0C7, S806, SB37...)

---

## Partie Pratique

Les exercices pratiques sont organises par chapitre du cours JCL.
Chaque chapitre contient des fichiers `.jcl` directement executables.

## Structure

```
exercices/jcl/
├── README.md
├── theorie/                        # QCM theoriques
│   ├── qcm-01-cartes-jcl.md
│   ├── qcm-02-fichiers-parametres.md
│   ├── qcm-03-procedures.md
│   └── qcm-04-utilitaires.md
├── pratique/                       # Exercices pratiques par chapitre
│   ├── chapitre-02/
│   │   ├── README.md
│   │   ├── ex01-creation-esds.jcl
│   │   ├── ex02-copie-iebgener.jcl
│   │   ├── ex03-fichier-temporaire.jcl
│   │   ├── ex04-concatenation.jcl
│   │   ├── ex-bonus-complet.jcl
│   │   └── cleanup.jcl
│   ├── chapitre-03/
│   │   ├── README.md
│   │   ├── ex01-proc-instream.jcl
│   │   ├── ex02-deux-procs.jcl
│   │   ├── ex03-proc-imbriquees.jcl
│   │   ├── ex04-param-symboliques.jcl
│   │   └── cleanup.jcl
│   └── chapitre-04/
│       ├── README.md
│       ├── ex01-iefbr14-iebgener.jcl
│       ├── ex02-iebcopy-pds.jcl
│       ├── ex03-sort-filtrage.jcl
│       ├── ex04-idcams-vsam.jcl
│       ├── ex-bonus-workflow.jcl
│       └── cleanup.jcl
└── tp/                             # Travaux pratiques de synthese
    ├── README.md
    ├── tp01-dataset-ps.jcl
    ├── tp02-dataset-po.jcl
    ├── tp03-concatenation.jcl
    ├── tp04-utilitaires.jcl
    ├── tp05-sort.jcl
    ├── tp06-analyse-erreurs.jcl
    └── cleanup.jcl
```

## Exercices Pratiques (pratique/)

### Chapitre 02 - Fichiers speciaux et parametres

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-02/ex01-creation-esds.jcl` | Creation dataset avec donnees in-stream |
| `pratique/chapitre-02/ex02-copie-iebgener.jcl` | Copie dataset avec IEBGENER et DCB=* |
| `pratique/chapitre-02/ex03-fichier-temporaire.jcl` | Fichier temporaire entre steps |
| `pratique/chapitre-02/ex04-concatenation.jcl` | Concatenation de datasets |
| `pratique/chapitre-02/ex-bonus-complet.jcl` | Tous les exercices en un seul job |
| `pratique/chapitre-02/cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- IEBGENER et DD *
- DCB=*.ddname (reference arriere)
- DSN=&&TEMP, DISP=(NEW,PASS)
- Concatenation de fichiers

---

### Chapitre 03 - Procedures

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-03/ex01-proc-instream.jcl` | Procedure in-stream avec override |
| `pratique/chapitre-03/ex02-deux-procs.jcl` | Deux procedures successives |
| `pratique/chapitre-03/ex03-proc-imbriquees.jcl` | Procedures imbriquees |
| `pratique/chapitre-03/ex04-param-symboliques.jcl` | Parametrage multiple |
| `pratique/chapitre-03/cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- PROC...PEND
- Parametres symboliques &param
- Override stepname.ddname
- Procedures imbriquees (procstep.step.ddname)

---

### Chapitre 04 - Utilitaires

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-04/ex01-iefbr14-iebgener.jcl` | IEFBR14 et IEBGENER : creation et chargement |
| `pratique/chapitre-04/ex02-iebcopy-pds.jcl` | IEBCOPY : gestion des PDS |
| `pratique/chapitre-04/ex03-sort-filtrage.jcl` | SORT : tri et filtrage |
| `pratique/chapitre-04/ex04-idcams-vsam.jcl` | IDCAMS : gestion VSAM |
| `pratique/chapitre-04/ex-bonus-workflow.jcl` | Workflow complet multi-utilitaires |
| `pratique/chapitre-04/cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- IEFBR14 pour creation/suppression
- IEBGENER pour copie sequentielle
- IEBCOPY pour gestion PDS (COPY, SELECT, compression)
- IDCAMS (DELETE, DEFINE, REPRO, PRINT, LISTCAT)
- SORT (FIELDS, INCLUDE/OMIT, OUTREC, OUTFIL)
- Gestion fichiers VSAM KSDS

---

## Travaux Pratiques de Synthese (tp/)

| Fichier | Description |
|---------|-------------|
| `tp/tp01-dataset-ps.jcl` | Creation d'un dataset sequentiel (PS) |
| `tp/tp02-dataset-po.jcl` | Creation d'un dataset partitionne (PDS) |
| `tp/tp03-concatenation.jcl` | Concatenation de datasets |
| `tp/tp04-utilitaires.jcl` | Utilisation des utilitaires IBM |
| `tp/tp05-sort.jcl` | Manipulation SORT complete |
| `tp/tp06-analyse-erreurs.jcl` | Analyse des erreurs et codes retour |
| `tp/cleanup.jcl` | Nettoyage des datasets |

**Contenu :**
- 6 travaux pratiques de synthese
- Manipulation complete avec ISPF et TSO
- Exercices couvrant tous les chapitres

## Prerequis

- Acces a un environnement z/OS (ou emulateur Hercules/TK4-)
- Userid TSO valide
- Droits de creation de datasets
- Connaissance des chapitres du cours JCL

## Utilisation

### Sur mainframe z/OS

1. Uploader les fichiers .jcl dans un PDS (ex: FTEST.JCL.SOURCE)
2. Editer pour remplacer `FTEST` par votre userid
3. Soumettre avec `SUB` ou `=S` dans ISPF

### Sur Hercules/TK4-

1. Remplacer `FTEST` par votre userid (ex: `HERC01`)
2. Ajouter `VOL=SER=PUB001,UNIT=3390` sur les DD de creation
3. Utiliser `CLASS=A,MSGCLASS=A`
4. Soumettre via TSO ou ISPF

### Ordre d'execution

**Pratique - Chapitre 02 :**
```
pratique/chapitre-02/ex01 -> ex02 -> ex03 -> ex04
                 ou
pratique/chapitre-02/ex-bonus-complet (tout en un)
```

**Pratique - Chapitre 03 :**
```
Chaque exercice est independant
```

**Pratique - Chapitre 04 :**
```
pratique/chapitre-04/ex01 (creation donnees) -> ex03 (utilise ex01)
                                             -> ex04 (utilise ex01)
ex02 est independant
ex-bonus-workflow (tout en un)
```

**TP - Synthese :**
```
tp/tp01 -> tp02 -> tp03 -> tp04 -> tp05 -> tp06
(Ordre recommande mais chaque TP peut etre fait independamment)
```

### Nettoyage

Apres les exercices, executer `cleanup.jcl` pour supprimer les datasets crees.

## Progression recommandee

1. **Theorie d'abord** : Faire les QCM pour valider la comprehension
2. **Pratique ensuite** : Realiser les exercices hands-on
3. **Revision** : Refaire les QCM apres la pratique pour consolider

## Notes

- Chaque dossier `chapitre-XX/` contient un `README.md` avec la documentation detaillee et les enonces
- Les fichiers `.jcl` sont les solutions executables
- Adapter les noms de datasets selon votre environnement

---
*Formation JCL - M2i Formation*
