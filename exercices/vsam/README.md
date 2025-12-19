# Exercices VSAM

## Organisation

Les exercices sont organises en trois parties :
- **Theorie** : QCM pour valider les connaissances theoriques
- **Pratique** : Exercices hands-on avec fichiers `.jcl` executables
- **TP** : Travaux pratiques de synthese

---

## Partie Theorique (QCM)

Les QCM sont organises par chapitre du cours et permettent de valider la comprehension des concepts.

| Fichier | Chapitre | Nombre de questions |
|---------|----------|---------------------|
| `theorie/qcm-01-introduction.md` | I - Introduction a VSAM | 20 questions |
| `theorie/qcm-02-organisation.md` | II - Catalogues et Data Space | 20 questions |
| `theorie/qcm-03-structures.md` | III - CI, CA, ESDS, KSDS, RRDS, LDS | 25 questions |
| `theorie/qcm-04-commandes.md` | IV-VII - Commandes IDCAMS | 30 questions |

**Total : 95 questions**

### Themes couverts par QCM

#### QCM-01 : Introduction
- Presentation VSAM, DASD, VTOC
- Types ESDS, KSDS, RRDS, LDS
- Formats FB, VB, structures PDS/PDSE

#### QCM-02 : Organisation
- Master Catalog et User Catalog
- SHAREOPTIONS et partage
- Data Space et ALIAS

#### QCM-03 : Structures
- Control Interval (CI) et Control Area (CA)
- RDF, CIDF, SPANNED
- Alternate Index (AIX)

#### QCM-04 : Commandes
- DEFINE CLUSTER, AIX, PATH
- REPRO, ALTER, DELETE, VERIFY, PRINT
- LISTCAT et analyse
- GDG et codes retour

---

## Partie Pratique

Les exercices pratiques sont organises par chapitre du cours VSAM.
Chaque chapitre contient des fichiers `.jcl` directement executables.

## Structure

```
exercices/vsam/
├── README.md
├── theorie/                        # QCM theoriques
│   ├── qcm-01-introduction.md
│   ├── qcm-02-organisation.md
│   ├── qcm-03-structures.md
│   └── qcm-04-commandes.md
├── pratique/                       # Exercices pratiques par chapitre
│   ├── chapitre-01/               # Definition de clusters
│   │   ├── README.md
│   │   ├── ex01-esds.jcl
│   │   ├── ex02-ksds.jcl
│   │   ├── ex03-rrds.jcl
│   │   ├── ex04-lds.jcl
│   │   ├── ex-bonus-all-types.jcl
│   │   └── cleanup.jcl
│   ├── chapitre-02/               # Index alternatifs (AIX)
│   │   ├── README.md
│   │   ├── ex01-aix-ksds.jcl
│   │   ├── ex02-aix-nonunique.jcl
│   │   ├── ex03-aix-esds.jcl
│   │   └── cleanup.jcl
│   └── chapitre-03/               # Manipulation IDCAMS
│       ├── README.md
│       ├── ex01-repro.jcl
│       ├── ex02-alter.jcl
│       ├── ex03-delete-verify.jcl
│       ├── ex04-print.jcl
│       └── cleanup.jcl
└── tp/                             # Travaux pratiques de synthese
    ├── README.md
    ├── tp01-clients.jcl
    ├── tp02-gdg.jcl
    ├── tp03-workflow.jcl
    └── cleanup.jcl
```

---

## Exercices Pratiques (pratique/)

### Chapitre 01 - Definition de Clusters

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-01/ex01-esds.jcl` | Creation d'un cluster ESDS |
| `pratique/chapitre-01/ex02-ksds.jcl` | Creation d'un cluster KSDS |
| `pratique/chapitre-01/ex03-rrds.jcl` | Creation d'un cluster RRDS |
| `pratique/chapitre-01/ex04-lds.jcl` | Creation d'un cluster LDS |
| `pratique/chapitre-01/ex-bonus-all-types.jcl` | Les 4 types en un seul job |
| `pratique/chapitre-01/cleanup.jcl` | Nettoyage des clusters |

**Concepts couverts :**
- INDEXED, NONINDEXED, NUMBERED, LINEAR
- KEYS, RECORDSIZE, FREESPACE
- Composants DATA et INDEX

---

### Chapitre 02 - Index Alternatifs (AIX)

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-02/ex01-aix-ksds.jcl` | AIX sur un cluster KSDS |
| `pratique/chapitre-02/ex02-aix-nonunique.jcl` | AIX avec cles en double |
| `pratique/chapitre-02/ex03-aix-esds.jcl` | AIX sur un cluster ESDS |
| `pratique/chapitre-02/cleanup.jcl` | Nettoyage des AIX |

**Concepts couverts :**
- DEFINE ALTERNATEINDEX
- DEFINE PATH
- BLDINDEX
- UPGRADE, NONUNIQUEKEY

---

### Chapitre 03 - Manipulation IDCAMS

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-03/ex01-repro.jcl` | Copie et chargement avec REPRO |
| `pratique/chapitre-03/ex02-alter.jcl` | Modification d'attributs |
| `pratique/chapitre-03/ex03-delete-verify.jcl` | Suppression et verification |
| `pratique/chapitre-03/ex04-print.jcl` | Affichage du contenu |
| `pratique/chapitre-03/cleanup.jcl` | Nettoyage des fichiers |

**Concepts couverts :**
- REPRO (FROMKEY, TOKEY, SKIP, COUNT)
- ALTER (NEWNAME, FREESPACE, INHIBIT)
- DELETE (PURGE, ERASE)
- VERIFY
- PRINT (CHARACTER, HEX, DUMP)

---

## Travaux Pratiques de Synthese (tp/)

| Fichier | Description |
|---------|-------------|
| `tp/tp01-clients.jcl` | Gestion complete fichier clients avec AIX |
| `tp/tp02-gdg.jcl` | Gestion des Generation Data Groups |
| `tp/tp03-workflow.jcl` | Workflow batch complet |
| `tp/cleanup.jcl` | Nettoyage de tous les objets |

**Contenu :**
- 10 questions QCM de revision
- 3 travaux pratiques complets
- Simulation d'un environnement reel

---

## Prerequis

- Acces a un environnement z/OS (ou emulateur Hercules/TK4-)
- Userid TSO valide
- Droits de creation de Data Sets
- Connaissance des chapitres du cours VSAM

## Utilisation

### Sur mainframe z/OS

1. Uploader les fichiers .jcl dans un PDS (ex: FTEST.JCL.SOURCE)
2. Editer pour remplacer `FTEST` par votre userid
3. Remplacer `ZASYS1` par votre volume
4. Soumettre avec `SUB` ou `=S` dans ISPF

### Sur Hercules/TK4-

1. Remplacer `FTEST` par votre userid (ex: `HERC01`)
2. Remplacer `ZASYS1` par `PUB001` ou votre volume
3. Utiliser `CLASS=A,MSGCLASS=A`
4. Soumettre via TSO ou ISPF

### Ordre d'execution

**Pratique - Chapitre 01 :**
```
ex01 -> ex02 -> ex03 -> ex04
     ou
ex-bonus-all-types (tout en un)
```

**Pratique - Chapitre 02 :**
```
ex01 -> ex02 -> ex03
(Chaque exercice est independant)
```

**Pratique - Chapitre 03 :**
```
Chaque exercice est independant
```

**TP - Synthese :**
```
tp01 -> tp02 -> tp03
(Ordre recommande mais chaque TP peut etre fait independamment)
```

### Nettoyage

Apres les exercices, executer `cleanup.jcl` pour supprimer les Data Sets crees.

---

## Progression Recommandee

1. **Theorie d'abord** : Faire les QCM pour valider la comprehension
2. **Pratique ensuite** : Realiser les exercices hands-on
3. **TP de synthese** : Valider les competences acquises
4. **Revision** : Refaire les QCM apres la pratique pour consolider

---

## Notes

- Chaque dossier contient un `README.md` avec la documentation detaillee
- Les fichiers `.jcl` sont les solutions executables
- Adapter les noms de Data Sets et volumes selon votre environnement
- Les codes retour attendus sont 0 sauf indication contraire

---
*Formation VSAM - M2i Formation*
