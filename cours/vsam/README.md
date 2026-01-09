# Module VSAM - Virtual Storage Accèss Method

## Présentation

Ce module couvre VSAM (Virtual Storage Accèss Method), la méthode d'accès aux données la plus utilisée sur les systèmes z/OS. VSAM permet de stocker et d'acceder aux données de maniere séquentielle, indexée ou directe.

## Objectifs

- Comprendre l'architecture et les concepts VSAM
- Maitriser les differents types d'organisation (ESDS, KSDS, RRDS, LDS)
- Savoir définir et gerer des clusters VSAM avec IDCAMS
- Manipuler les données VSAM (REPRO, PRINT, EXPORT/IMPORT)
- Analyser les sorties LISTCAT
- Gerer les GDG (Generation Data Groups)

## Contenu

| Fichier | Chapitre | Description |
|---------|----------|-------------|
| `01-introduction-vsam.md` | I | Introduction a VSAM |
| `02-modes-organisation.md` | II | Catalogues et Data Space |
| `03-structure-datasets.md` | III | CI, CA, ESDS, KSDS, RRDS, LDS |
| `04-commandes-ams.md` | IV | DEFINE CLUSTER, AIX, PATH |
| `05-manipulation-idcams.md` | V | REPRO, ALTER, DELETE, VERIFY |
| `06-analyse-listcat.md` | VI | Analyse de la sortie LISTCAT |
| `07-gdg-codes-retour.md` | VII | GDG et codes retour VSAM |

## Chapitres Detailles

### Chapitre I - Introduction a VSAM
- Présentation générale et historique
- Types d'organisation VSAM
- Notion de VTOC
- Méthodes d'accès aux Data Sets
- Structure physique et logique

### Chapitre II - Modes d'Organisation
- Catalogue Master et User
- VSAM Data Space
- SHAREOPTIONS et partage des fichiers

### Chapitre III - Structure des Data Sets
- Control Interval (CI) et Control Area (CA)
- ESDS - Entry Sequenced Data Set
- KSDS - Key Sequenced Data Set
- RRDS - Relative Record Data Set
- LDS - Linear Data Set
- Alternate Index (AIX)

### Chapitre IV - Commandes AMS (DEFINE)
- Introduction a IDCAMS
- Regles de codage
- Commandes modales (IF-THEN-ELSE)
- DEFINE USERCATALOG
- DEFINE SPACE
- DEFINE CLUSTER (ESDS, KSDS, RRDS, LDS)
- DEFINE ALTERNATEINDEX
- DEFINE PATH
- BLDINDEX

### Chapitre V - Manipulation IDCAMS
- LISTCAT - Lister le catalogue
- REPRO - Copier les données
- ALTER - Modifier les attributs
- DELETE - Supprimer
- VERIFY - Verifier/reparer
- PRINT - Imprimer le contenu
- EXPORT/IMPORT - Sauvegarde/restauration

### Chapitre VI - Analyse LISTCAT
- Interpretation des champs
- Statistiques et allocations
- Diagnostic des problemes

### Chapitre VII - GDG et Codes Retour
- Generation Data Groups
- DEFINE/ALTER/DELETE GDG
- Référencement relatif
- Codes retour VSAM

## Prerequis

- Connaissance de z/OS et TSO/ISPF
- Maitrise du JCL
- Notions de gestion de fichiers

## Ressources

- Exercices pratiques : `exercices/vsam/`
- Projet fil-rouge : `exercices/cobol/fil-rouge/`

---

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [JCL](../jcl/README.md) | [Algorithmique](../algorithmique/README.md) |

---
*Formation VSAM - M2i Formation*
