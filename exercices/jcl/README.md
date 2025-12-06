# Exercices JCL

## Organisation

Les exercices sont organises par chapitre du cours JCL.
Chaque chapitre contient des fichiers `.jcl` directement executables.

## Structure

```
exercices/jcl/
├── README.md
├── chapitre-02/
│   ├── README.md                   # Documentation et enonces
│   ├── ex01-creation-esds.jcl
│   ├── ex02-copie-iebgener.jcl
│   ├── ex03-fichier-temporaire.jcl
│   ├── ex04-concatenation.jcl
│   ├── ex-bonus-complet.jcl
│   └── cleanup.jcl
├── chapitre-03/
│   ├── README.md                   # Documentation et enonces
│   ├── ex01-proc-instream.jcl
│   ├── ex02-deux-procs.jcl
│   ├── ex03-proc-imbriquees.jcl
│   ├── ex04-param-symboliques.jcl
│   └── cleanup.jcl
├── chapitre-04/
│   ├── README.md                   # Documentation et enonces
│   ├── ex01-iefbr14-iebgener.jcl
│   ├── ex02-iebcopy-pds.jcl
│   ├── ex03-sort-filtrage.jcl
│   ├── ex04-idcams-vsam.jcl
│   ├── ex-bonus-workflow.jcl
│   └── cleanup.jcl
└── chapitre-05/
    ├── README.md                   # QCM + enonces TP
    ├── tp01-dataset-ps.jcl
    ├── tp02-dataset-po.jcl
    ├── tp03-concatenation.jcl
    ├── tp04-utilitaires.jcl
    ├── tp05-sort.jcl
    ├── tp06-analyse-erreurs.jcl
    └── cleanup.jcl
```

## Chapitres

### Chapitre 02 - Fichiers speciaux et parametres

| Fichier | Description |
|---------|-------------|
| `ex01-creation-esds.jcl` | Creation dataset avec donnees in-stream |
| `ex02-copie-iebgener.jcl` | Copie dataset avec IEBGENER et DCB=* |
| `ex03-fichier-temporaire.jcl` | Fichier temporaire entre steps |
| `ex04-concatenation.jcl` | Concatenation de datasets |
| `ex-bonus-complet.jcl` | Tous les exercices en un seul job |
| `cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- IEBGENER et DD *
- DCB=*.ddname (reference arriere)
- DSN=&&TEMP, DISP=(NEW,PASS)
- Concatenation de fichiers

---

### Chapitre 03 - Procedures

| Fichier | Description |
|---------|-------------|
| `ex01-proc-instream.jcl` | Procedure in-stream avec override |
| `ex02-deux-procs.jcl` | Deux procedures successives |
| `ex03-proc-imbriquees.jcl` | Procedures imbriquees |
| `ex04-param-symboliques.jcl` | Parametrage multiple |
| `cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- PROC...PEND
- Parametres symboliques &param
- Override stepname.ddname
- Procedures imbriquees (procstep.step.ddname)

---

### Chapitre 04 - Utilitaires

| Fichier | Description |
|---------|-------------|
| `ex01-iefbr14-iebgener.jcl` | IEFBR14 et IEBGENER : creation et chargement |
| `ex02-iebcopy-pds.jcl` | IEBCOPY : gestion des PDS |
| `ex03-sort-filtrage.jcl` | SORT : tri et filtrage |
| `ex04-idcams-vsam.jcl` | IDCAMS : gestion VSAM |
| `ex-bonus-workflow.jcl` | Workflow complet multi-utilitaires |
| `cleanup.jcl` | Nettoyage des datasets |

**Concepts couverts :**
- IEFBR14 pour creation/suppression
- IEBGENER pour copie sequentielle
- IEBCOPY pour gestion PDS (COPY, SELECT, compression)
- IDCAMS (DELETE, DEFINE, REPRO, PRINT, LISTCAT)
- SORT (FIELDS, INCLUDE/OMIT, OUTREC, OUTFIL)
- Gestion fichiers VSAM KSDS

---

### Chapitre 05 - Travaux Pratiques

| Fichier | Description |
|---------|-------------|
| `tp01-dataset-ps.jcl` | Creation d'un dataset sequentiel (PS) |
| `tp02-dataset-po.jcl` | Creation d'un dataset partitionne (PDS) |
| `tp03-concatenation.jcl` | Concatenation de datasets |
| `tp04-utilitaires.jcl` | Utilisation des utilitaires IBM |
| `tp05-sort.jcl` | Manipulation SORT complete |
| `tp06-analyse-erreurs.jcl` | Analyse des erreurs et codes retour |
| `cleanup.jcl` | Nettoyage des datasets |

**Contenu :**
- QCM de comprehension (10 questions)
- 6 travaux pratiques de synthese
- Manipulation complete avec ISPF et TSO

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

**Chapitre 02 :**
```
ex01 -> ex02 -> ex03 -> ex04
         ou
ex-bonus-complet (tout en un)
```

**Chapitre 03 :**
```
Chaque exercice est independant
```

**Chapitre 04 :**
```
ex01 (creation donnees) -> ex03 (utilise ex01)
                        -> ex04 (utilise ex01)
ex02 est independant
ex-bonus-workflow (tout en un)
```

**Chapitre 05 :**
```
tp01 -> tp02 -> tp03 -> tp04 -> tp05 -> tp06
(Ordre recommande mais chaque TP peut etre fait independamment)
```

### Nettoyage

Apres les exercices, executer `cleanup.jcl` pour supprimer les datasets crees.

## Notes

- Chaque dossier `chapitre-XX/` contient un `README.md` avec la documentation detaillee et les enonces
- Les fichiers `.jcl` sont les solutions executables
- Adapter les noms de datasets selon votre environnement
