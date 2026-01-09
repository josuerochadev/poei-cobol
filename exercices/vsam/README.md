# Exercices VSAM

## Organisation

Les exercices sont organisés en trois parties :
- **Théorie** : QCM pour valider les connaissances théoriques
- **Pratique** : Exercices hands-on avec fichiers `.jcl` exécutables
- **TP** : Travaux pratiques de synthèse

---

## Structure

```
exercices/vsam/
├── README.md
├── theorie/                        # QCM théoriques (formateur)
│   ├── qcm-01-introduction.md      # 9 questions avec erreurs à détecter
│   ├── qcm-02-organisation.md      # 8 questions
│   ├── qcm-03-structures.md        # 10 questions
│   ├── qcm-04-commandes.md         # 10 questions + exercice pratique
│   └── extra/                      # QCM supplémentaires (95 questions)
├── pratique/                       # Exercices pratiques (TESTGDG.*)
│   ├── chapitre-01/               # Définition de clusters
│   ├── chapitre-02/               # Index alternatifs (AIX)
│   ├── chapitre-03/               # Manipulation IDCAMS
│   └── extra/                     # Exercices supplémentaires (FTEST.*)
└── tp/                            # Travaux pratiques de synthèse
    ├── README.md
    ├── tp-gdg-formateur.jcl       # TP GDG du formateur (TESTGDG.COMPTE.MENSUEL)
    ├── tp01-clients.jcl           # Gestion fichier clients avec AIX
    ├── tp03-workflow.jcl          # Workflow batch complet
    ├── cleanup.jcl
    └── extra/                     # TP supplémentaires (FTEST.*)
```

---

## Partie Théorique (QCM)

Les QCM du formateur contiennent des **erreurs intentionnelles** à détecter pour valider la compréhension.

| Fichier | Chapitre | Questions |
|---------|----------|-----------|
| `theorie/qcm-01-introduction.md` | I - Introduction à VSAM | 9 questions |
| `theorie/qcm-02-organisation.md` | II - Catalogues et Data Space | 8 questions |
| `theorie/qcm-03-structures.md` | III - CI, CA, Types VSAM | 10 questions |
| `theorie/qcm-04-commandes.md` | IV - Commandes IDCAMS | 10 questions |

**Total : 37 questions** (+ 95 questions supplémentaires dans `extra/`)

---

## Partie Pratique

### Nomenclature

Les exercices utilisent la nomenclature `TESTGDG.*` du formateur.
Les exercices supplémentaires dans `extra/` utilisent `FTEST.*`.

### Chapitre 01 - Définition de Clusters

| Fichier | Description |
|---------|-------------|
| `pratique/chapitre-01/ex01-esds.jcl` | Création d'un cluster ESDS |
| `pratique/chapitre-01/ex02-ksds.jcl` | Création d'un cluster KSDS |
| `pratique/chapitre-01/ex03-rrds.jcl` | Création d'un cluster RRDS |
| `pratique/chapitre-01/ex04-lds.jcl` | Création d'un cluster LDS |
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
| `pratique/chapitre-02/ex02-aix-nonunique.jcl` | AIX avec clés en double |
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
| `pratique/chapitre-03/ex03-delete-verify.jcl` | Suppression et vérification |
| `pratique/chapitre-03/ex04-print.jcl` | Affichage du contenu |
| `pratique/chapitre-03/cleanup.jcl` | Nettoyage des fichiers |

**Concepts couverts :**
- REPRO (FROMKEY, TOKEY, SKIP, COUNT)
- ALTER (NEWNAME, FREESPACE, INHIBIT)
- DELETE (PURGE, ERASE)
- VERIFY
- PRINT (CHARACTER, HEX, DUMP)

---

## Travaux Pratiques de Synthèse (tp/)

| Fichier | Description |
|---------|-------------|
| `tp/tp-gdg-formateur.jcl` | **TP GDG du formateur** : TESTGDG.COMPTE.MENSUEL, LIMIT(4), rotation |
| `tp/tp01-clients.jcl` | Gestion complète fichier clients avec AIX |
| `tp/tp03-workflow.jcl` | Workflow batch complet |
| `tp/cleanup.jcl` | Nettoyage de tous les objets |

### TP GDG Formateur (tp-gdg-formateur.jcl)

Ce TP correspond exactement à l'exercice de fin de module :
1. Définition GDG TESTGDG.COMPTE.MENSUEL avec LIMIT(4)
2. Création ESDS et chargement données clients
3. Conversion ESDS vers KSDS avec REPRO
4. Création de 4 générations mensuelles
5. Création d'une 5ème génération pour observer la rotation
6. Concaténation des générations
7. LISTCAT et PRINT pour vérification

---

## Prérequis

- Accès à un environnement z/OS (ou émulateur Hercules/TK4-)
- Userid TSO valide
- Droits de création de Data Sets
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

## Progression Recommandée

1. **Théorie d'abord** : Faire les QCM pour valider la compréhension
2. **Pratique ensuite** : Réaliser les exercices hands-on
3. **TP de synthèse** : Valider les compétences avec le TP GDG formateur
4. **Approfondissement** : Explorer les exercices supplémentaires dans `extra/`

---

## Notes

- Chaque dossier contient un `README.md` avec la documentation détaillée
- Les fichiers `.jcl` sont les solutions exécutables
- Adapter les noms de Data Sets et volumes selon votre environnement
- Les codes retour attendus sont 0 sauf indication contraire
- Les QCM contiennent des erreurs intentionnelles à détecter

---
*Formation VSAM - M2i Formation*
