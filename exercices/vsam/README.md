# Exercices VSAM

## Organisation

Les exercices sont organises en trois parties :
- **Theorie** : QCM pour valider les connaissances theoriques
- **Pratique** : Exercices hands-on avec fichiers `.jcl` executables
- **TP** : Travaux pratiques de synthese

---

## Structure

```
exercices/vsam/
├── README.md
├── theorie/                        # QCM theoriques (formateur)
│   ├── qcm-01-introduction.md      # 9 questions avec erreurs a detecter
│   ├── qcm-02-organisation.md      # 8 questions
│   ├── qcm-03-structures.md        # 10 questions
│   ├── qcm-04-commandes.md         # 10 questions + exercice pratique
│   └── extra/                      # QCM supplementaires (95 questions)
├── pratique/                       # Exercices pratiques (TESTGDG.*)
│   ├── chapitre-01/               # Definition de clusters
│   ├── chapitre-02/               # Index alternatifs (AIX)
│   ├── chapitre-03/               # Manipulation IDCAMS
│   └── extra/                     # Exercices supplementaires (FTEST.*)
└── tp/                            # Travaux pratiques de synthese
    ├── README.md
    ├── tp-gdg-formateur.jcl       # TP GDG du formateur (TESTGDG.COMPTE.MENSUEL)
    ├── tp01-clients.jcl           # Gestion fichier clients avec AIX
    ├── tp03-workflow.jcl          # Workflow batch complet
    ├── cleanup.jcl
    └── extra/                     # TP supplementaires (FTEST.*)
```

---

## Partie Theorique (QCM)

Les QCM du formateur contiennent des **erreurs intentionnelles** a detecter pour valider la comprehension.

| Fichier | Chapitre | Questions |
|---------|----------|-----------|
| `theorie/qcm-01-introduction.md` | I - Introduction a VSAM | 9 questions |
| `theorie/qcm-02-organisation.md` | II - Catalogues et Data Space | 8 questions |
| `theorie/qcm-03-structures.md` | III - CI, CA, Types VSAM | 10 questions |
| `theorie/qcm-04-commandes.md` | IV - Commandes IDCAMS | 10 questions |

**Total : 37 questions** (+ 95 questions supplementaires dans `extra/`)

---

## Partie Pratique

### Nomenclature

Les exercices utilisent la nomenclature `TESTGDG.*` du formateur.
Les exercices supplementaires dans `extra/` utilisent `FTEST.*`.

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
| `tp/tp-gdg-formateur.jcl` | **TP GDG du formateur** : TESTGDG.COMPTE.MENSUEL, LIMIT(4), rotation |
| `tp/tp01-clients.jcl` | Gestion complete fichier clients avec AIX |
| `tp/tp03-workflow.jcl` | Workflow batch complet |
| `tp/cleanup.jcl` | Nettoyage de tous les objets |

### TP GDG Formateur (tp-gdg-formateur.jcl)

Ce TP correspond exactement a l'exercice de fin de module :
1. Definition GDG TESTGDG.COMPTE.MENSUEL avec LIMIT(4)
2. Creation ESDS et chargement donnees clients
3. Conversion ESDS vers KSDS avec REPRO
4. Creation de 4 generations mensuelles
5. Creation d'une 5eme generation pour observer la rotation
6. Concatenation des generations
7. LISTCAT et PRINT pour verification

---

## Prerequis

- Acces a un environnement z/OS (ou emulateur Hercules/TK4-)
- Userid TSO valide
- Droits de creation de Data Sets
- Connaissance des chapitres du cours VSAM

## Utilisation

### Sur mainframe z/OS

1. Uploader les fichiers .jcl dans un PDS (ex: TESTGDG.JCL.SOURCE)
2. Remplacer `ZASYS1` par votre volume
3. Soumettre avec `SUB` ou `=S` dans ISPF

### Sur Hercules/TK4-

1. Remplacer `ZASYS1` par `PUB001` ou votre volume
2. Utiliser `CLASS=A,MSGCLASS=A`
3. Soumettre via TSO ou ISPF

---

## Progression Recommandee

1. **Theorie d'abord** : Faire les QCM pour valider la comprehension
2. **Pratique ensuite** : Realiser les exercices hands-on
3. **TP de synthese** : Valider les competences avec le TP GDG formateur
4. **Approfondissement** : Explorer les exercices supplementaires dans `extra/`

---

## Notes

- Chaque dossier contient un `README.md` avec la documentation detaillee
- Les fichiers `.jcl` sont les solutions executables
- Adapter les noms de Data Sets et volumes selon votre environnement
- Les codes retour attendus sont 0 sauf indication contraire
- Les QCM contiennent des erreurs intentionnelles a detecter

---
*Formation VSAM - M2i Formation*
