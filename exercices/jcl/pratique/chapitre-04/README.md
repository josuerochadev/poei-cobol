# Exercices JCL - Chapitre IV : Les Utilitaires

## Objectifs

- Maitriser les utilitaires IBM (IEFBR14, IEBGENER, IEBCOPY, IDCAMS, SORT)
- Savoir creer, copier et supprimer des datasets
- Comprendre le tri et le filtrage de donnees
- Manipuler les fichiers VSAM avec IDCAMS

---

## Exercice 1 : IEFBR14 et IEBGENER

### Objectif
Utiliser IEFBR14 pour creer des datasets vides et IEBGENER pour les charger.

### Enonce

Creer un JCL qui effectue les operations suivantes :

1. **Step DELETE** : Supprimer le dataset `FTEST.UTIL.DATA` s'il existe (IDCAMS)
2. **Step CREATE** : Creer le dataset `FTEST.UTIL.DATA` avec IEBGENER et les donnees suivantes :

```
001DUPONT    JEAN      PARIS     75001
002MARTIN    MARIE     LYON      69001
003DURAND    PIERRE    MARSEILLE 13001
004PETIT     SOPHIE    BORDEAUX  33001
005BERNARD   PAUL      PARIS     75008
006THOMAS    CLAIRE    LYON      69002
007ROBERT    LUC       NANTES    44000
008RICHARD   ANNE      PARIS     75016
```

3. **Step PRINT** : Afficher le contenu du fichier cree (IEBGENER vers SYSOUT)

### Format du fichier
- LRECL=50
- RECFM=FB
- Structure : Code(3), Nom(10), Prenom(10), Ville(10), CP(5), Filler(12)

### Solution
Voir `chapitre-04/ex01-iefbr14-iebgener.jcl`

---

## Exercice 2 : IEBCOPY - Gestion PDS

### Objectif
Maitriser la gestion des PDS avec IEBCOPY.

### Enonce

Creer un JCL qui effectue les operations suivantes :

1. **Step DELPDS** : Supprimer les PDS cibles s'ils existent (IDCAMS)

2. **Step CREPDS** : Creer deux PDS vides avec IEFBR14 :
   - `FTEST.SOURCE.PDS` (directory=10)
   - `FTEST.BACKUP.PDS` (directory=10)

3. **Step LOADMEM** : Charger 3 membres dans `FTEST.SOURCE.PDS` avec IEBGENER :
   - MEMBRE1 avec quelques lignes de donnees
   - MEMBRE2 avec quelques lignes de donnees
   - MEMBRE3 avec quelques lignes de donnees

4. **Step COPYPDS** : Copier tout le PDS vers `FTEST.BACKUP.PDS` (IEBCOPY)

5. **Step COPYSEL** : Copier uniquement MEMBRE1 et MEMBRE3 vers un nouveau PDS (IEBCOPY SELECT)

### Solution
Voir `chapitre-04/ex02-iebcopy-pds.jcl`

---

## Exercice 3 : SORT - Tri et Filtrage

### Objectif
Maitriser les operations de tri, filtrage et reformatage avec SORT.

### Enonce

A partir du fichier `FTEST.UTIL.DATA` cree a l'exercice 1, creer un JCL qui :

1. **Step TRISIMPL** : Trier le fichier par nom (positions 4-13) en ordre croissant
   - Sortie : `FTEST.UTIL.TRIENOM`

2. **Step TRICP** : Trier le fichier par code postal (positions 41-45) decroissant, puis nom croissant
   - Sortie : `FTEST.UTIL.TRIECP`

3. **Step FILTPARIS** : Extraire uniquement les enregistrements de Paris (CP commence par 75)
   - Utiliser INCLUDE
   - Sortie : `FTEST.UTIL.PARIS`

4. **Step EXTRACT** : Reformater pour n'avoir que Code, Nom, Ville (30 caracteres)
   - Utiliser OUTREC
   - Sortie : `FTEST.UTIL.EXTRAIT`

### Solution
Voir `chapitre-04/ex03-sort-filtrage.jcl`

---

## Exercice 4 : IDCAMS - Gestion VSAM

### Objectif
Creer et manipuler un fichier VSAM KSDS avec IDCAMS.

### Enonce

1. **Step DELETE** : Supprimer le cluster VSAM s'il existe

2. **Step DEFINE** : Definir un KSDS avec les caracteristiques suivantes :
   - Nom : `FTEST.VSAM.CLIENT`
   - Cle : 3 caracteres, position 0
   - Taille enregistrement : 50 (fixe)
   - Espace : 5 tracks primaire, 2 tracks secondaire
   - SHAREOPTIONS(2 3)

3. **Step LOAD** : Charger les donnees depuis `FTEST.UTIL.DATA` (REPRO)

4. **Step PRINT** : Afficher les 5 premiers enregistrements (PRINT)

5. **Step LISTCAT** : Lister les informations du catalogue (LISTCAT ALL)

### Solution
Voir `chapitre-04/ex04-idcams-vsam.jcl`

---

## Exercice Bonus : Traitement Complet

### Objectif
Combiner plusieurs utilitaires dans un workflow complet.

### Enonce

Creer un JCL de traitement batch complet qui :

1. Supprime les anciens fichiers (IDCAMS)
2. Cree un fichier de donnees clients (IEBGENER)
3. Trie les donnees par ville puis par nom (SORT)
4. Separe les clients parisiens des autres (SORT OUTFIL)
5. Cree un VSAM et charge les donnees triees (IDCAMS)
6. Compare le fichier original avec le fichier trie (IEBCOMPR)
7. Genere un rapport final

### Solution
Voir `chapitre-04/ex-bonus-workflow.jcl`

---

## Cleanup

Apres les exercices, utiliser `cleanup.jcl` pour supprimer tous les datasets crees.

---

## Points Cles a Retenir

### IEFBR14
- Programme vide, utilise uniquement pour ses cartes DD
- Ideal pour creer/supprimer des datasets

### IEBGENER
- DD requises : SYSPRINT, SYSIN (ou DUMMY), SYSUT1, SYSUT2
- DCB=*.SYSUT1 copie les caracteristiques

### IEBCOPY
- Commande COPY OUTDD=...,INDD=...
- SELECT/EXCLUDE pour choisir les membres
- Compression : meme PDS en INDD et OUTDD

### IDCAMS
- SET MAXCC=0 apres DELETE pour ignorer "fichier inexistant"
- REPRO pour charger/decharger VSAM
- DEFINE CLUSTER avec DATA et INDEX

### SORT
- SORT FIELDS=(pos,len,format,ordre)
- INCLUDE/OMIT COND=(...) pour filtrer
- OUTREC FIELDS=(...) pour reformater
- OUTFIL pour sorties multiples

---

## Structure des Fichiers

```
exercices/jcl/chapitre-04/
├── ex01-iefbr14-iebgener.jcl    # IEFBR14 et IEBGENER
├── ex02-iebcopy-pds.jcl         # Gestion PDS
├── ex03-sort-filtrage.jcl       # Tri et filtrage
├── ex04-idcams-vsam.jcl         # VSAM avec IDCAMS
├── ex-bonus-workflow.jcl        # Workflow complet
└── cleanup.jcl                  # Nettoyage
```

---
*Formation COBOL - Exercices JCL Chapitre IV*
