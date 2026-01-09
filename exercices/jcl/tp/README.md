# Exercices JCL - Chapitre V : Travaux Pratiques

## Objectifs

Ce chapitre regroupe les travaux pratiques de synthese du module JCL :

- Manipulation des cartes EXEC, DD et JOB
- Creation de Data Sets PS (Sequentiel) et PO (Partitionne)
- Utilisation des utilitaires IEFBR14, IEBCOMPR, IEBCOPY, IEBGENER et IDCAMS
- Manipulation de l'instruction SORT
- Analyse des resultats et lecture des erreurs

**Environnement :** Ces travaux pratiques sont realises avec ISPF et TSO.

---

## Partie 1 : QCM de Comprehension

Avant de commencer les exercices pratiques, repondez a ce QCM pour verifier vos connaissances.

### Question 1
Pour identifier une Procedure dans un JCL, quel mot cle utiliser pour indiquer la presence d'une Procedure ?

- A. PRG
- B. EXEC
- C. //PROC
- D. PROC

<details>
<summary>Reponse</summary>

**D. PROC**

Une procedure in-stream est definie avec `//nom PROC` et terminee par `PEND`.
Pour appeler une procedure cataloguee, on utilise `EXEC procname`.
</details>

---

### Question 2
Quelle est la fonction de l'instruction JOBLIB ?

- A. Identifie l'emplacement des fichiers DD
- B. Pour declarer les procedures
- C. Identifie l'emplacement des programmes a executer dans un JCL
- D. Identifie la bibliotheque des modules systeme

<details>
<summary>Reponse</summary>

**C. Identifie l'emplacement des programmes a executer dans un JCL**

JOBLIB est une carte DD placee apres la carte JOB qui indique ou chercher les programmes
(load modules) pour tous les steps du job.
</details>

---

### Question 3
Quel est l'ordre de recherche des librairies dans un JCL ?

- A. Dans la librairie systeme par defaut
- B. Dans la librairie des programmes sources
- C. Dans la commande STEPLIB puis JOBLIB puis les bibliotheques systemes
- D. La bibliotheque doit etre specifiee au niveau de chaque Step

<details>
<summary>Reponse</summary>

**C. Dans la commande STEPLIB puis JOBLIB puis les bibliotheques systemes**

L'ordre de recherche est :
1. STEPLIB (si present dans le step)
2. JOBLIB (si present au niveau job)
3. Bibliotheques systeme (LINKLIST)
</details>

---

### Question 4
Comment designer un commentaire dans un JCL ?

<details>
<summary>Reponse</summary>

Un commentaire JCL commence par `//*` en colonnes 1-3.

```jcl
//* Ceci est un commentaire
//* Il peut y avoir plusieurs lignes
```
</details>

---

### Question 5
Quelles sont les instructions de base dans un JOB ?

<details>
<summary>Reponse</summary>

Les trois instructions de base sont :
- **JOB** : Identifie le travail (obligatoire, premiere carte)
- **EXEC** : Execute un programme ou une procedure
- **DD** : Definit les fichiers (Data Definition)

Autres instructions utiles : PROC, PEND, IF/THEN/ELSE/ENDIF, JCLLIB, etc.
</details>

---

### Question 6
Pourquoi utilise-t-on ces symboles dans un JOB JCL ?

- A. `/*`
- B. `//`
- C. `//*`

<details>
<summary>Reponse</summary>

- **A. `/*`** : Delimiteur de fin de donnees in-stream (fin de DD *)
- **B. `//`** : Debut de toute instruction JCL (colonnes 1-2)
- **C. `//*`** : Ligne de commentaire
</details>

---

### Question 7
Que signifie allocation primaire et allocation secondaire pour un Data Set ?

<details>
<summary>Reponse</summary>

Dans le parametre SPACE :
```jcl
SPACE=(unite,(primaire,secondaire,directory))
```

- **Allocation primaire** : Espace alloue a la creation du dataset
- **Allocation secondaire** : Espace supplementaire alloue automatiquement si le primaire est plein (jusqu'a 15 extensions)

Exemple : `SPACE=(TRK,(10,5))` = 10 tracks primaires, extensions de 5 tracks
</details>

---

### Question 8
Quelle est la difference entre un JOBLIB et un STEPLIB ?

<details>
<summary>Reponse</summary>

| JOBLIB | STEPLIB |
|--------|---------|
| Place apres la carte JOB | Place dans un step specifique |
| S'applique a tous les steps | S'applique uniquement au step |
| Un seul JOBLIB par job | Un STEPLIB par step possible |
| Priorite plus basse | Priorite plus haute |

Si STEPLIB est present, il est utilise a la place de JOBLIB pour ce step.
</details>

---

### Question 9
Quelle est la signification du mot-cle TIME et a quelle instruction JCL est-il associe ?

<details>
<summary>Reponse</summary>

**TIME** limite le temps CPU d'execution.

- Sur la carte **JOB** : `TIME=(minutes,secondes)` - limite pour tout le job
- Sur la carte **EXEC** : `TIME=(minutes,secondes)` - limite pour le step

```jcl
//MYJOB    JOB ...,TIME=(5,30)      <= Max 5 min 30 sec pour le job
//STEP1    EXEC PGM=PROG,TIME=(1,0) <= Max 1 min pour ce step
```

`TIME=NOLIMIT` ou `TIME=1440` = pas de limite.
</details>

---

### Question 10
Que font les instructions TYPRUN=SCAN et TYPRUN=HOLD dans une instruction JCL ?

<details>
<summary>Reponse</summary>

- **TYPRUN=SCAN** : Verifie la syntaxe du JCL sans l'executer. Utile pour valider un JCL avant soumission reelle.

- **TYPRUN=HOLD** : Soumet le job mais le met en attente (HOLD). Il faut le liberer manuellement pour qu'il s'execute.

```jcl
//MYJOB    JOB ...,TYPRUN=SCAN   <= Verification syntaxe uniquement
//MYJOB    JOB ...,TYPRUN=HOLD   <= Job en attente
```
</details>

---

## Partie 2 : Travaux Pratiques

### TP 1 : Creation d'un Data Set Sequentiel (PS)

**Objectif :** Creer un dataset sequentiel et y charger des donnees.

**Etapes :**
1. Utiliser ISPF (option 3.2) pour verifier qu'aucun dataset FTEST.* n'existe
2. Creer le dataset `FTEST.DATA.SEQFILE` avec les caracteristiques :
   - Type : PS (Physical Sequential)
   - LRECL : 80
   - RECFM : FB
   - Espace : 5 tracks primaires, 2 secondaires
3. Editer le dataset et ajouter des donnees
4. Verifier avec ISPF 3.4

**Solution :** Voir `tp01-dataset-ps.jcl`

---

### TP 2 : Creation d'un Data Set Partitionne (PO)

**Objectif :** Creer un PDS et y ajouter des membres.

**Etapes :**
1. Creer le dataset `FTEST.TSOJCL.LIBTEST` avec les caracteristiques :
   - Type : PO (Partitioned Organization)
   - LRECL : 80
   - RECFM : FB
   - Espace : 10 tracks primaires, 5 secondaires, 10 blocs directory
2. Utiliser l'editeur ISPF pour creer un membre nomme `JIEBGENE`
3. Activer la coloration syntaxique : commande `HI JCL`
4. Afficher la regle de colonnes : commande `COLS`
5. Saisir un JCL de test et le soumettre

**Solution :** Voir `tp02-dataset-po.jcl`

---

### TP 3 : Concatenation de Data Sets

**Objectif :** Creer plusieurs datasets et les concatener.

**Etapes :**
1. Creer le dataset `FTEST.ESDS.AAAA` avec des donnees
2. Creer le dataset `FTEST.ESDS.BBBB` avec des donnees differentes
3. Concatener ces deux datasets pour creer `FTEST.ESDS.CCCC`
4. Verifier le contenu du fichier concatene

**Solution :** Voir `tp03-concatenation.jcl`

---

### TP 4 : Utilisation des Utilitaires

**Objectif :** Pratiquer les utilitaires IBM.

**Etapes :**
1. **IEFBR14** : Creer un dataset vide
2. **IEBGENER** : Copier un dataset avec transformation
3. **IEBCOPY** : Copier des membres entre PDS
4. **IEBCOMPR** : Comparer deux datasets
5. **IDCAMS** : Creer et manipuler un fichier VSAM

**Solution :** Voir `tp04-utilitaires.jcl`

---

### TP 5 : Manipulation SORT

**Objectif :** Maitriser les operations de tri et filtrage.

**Etapes :**
1. Trier un fichier sur une cle ascendante
2. Trier sur plusieurs cles (ascendant et descendant)
3. Filtrer avec INCLUDE/OMIT
4. Reformater la sortie avec OUTREC
5. Generer plusieurs fichiers de sortie avec OUTFIL

**Solution :** Voir `tp05-sort.jcl`

---

### TP 6 : Analyse des Resultats et Erreurs

**Objectif :** Savoir lire et interpreter les resultats d'execution.

**Etapes :**
1. Soumettre un JCL avec une erreur de syntaxe volontaire
2. Analyser le message d'erreur dans JESMSGLG
3. Corriger et resoumettre
4. Analyser les codes retour dans JESYSMSG
5. Comprendre les differents RC (0, 4, 8, 12, 16)

**Solution :** Voir `tp06-analyse-erreurs.jcl`

---

## Commandes ISPF Utiles

| Commande | Description |
|----------|-------------|
| `=3.2` | Gestion des datasets |
| `=3.4` | Liste des datasets (DSLIST) |
| `=S` ou `SUB` | Soumettre un JCL |
| `HI JCL` | Coloration syntaxique JCL |
| `COLS` | Afficher la regle de colonnes |
| `RES` | Reinitialiser l'ecran |
| `=SD` | Afficher les jobs soumis (SDSF) |

---

## Codes Retour Courants

| RC | Signification |
|----|---------------|
| 0 | Succes |
| 4 | Warning (avertissement) |
| 8 | Erreur |
| 12 | Erreur grave |
| 16 | Erreur fatale |

---

## Structure des Fichiers

```
exercices/jcl/tp/
├── README.md                # Ce fichier (QCM + énoncés)
├── tp01-dataset-ps.jcl      # Création dataset séquentiel
├── tp02-dataset-po.jcl      # Création dataset partitionné
├── tp03-concatenation.jcl   # Concaténation de datasets
├── tp04-utilitaires.jcl     # Utilisation des utilitaires
├── tp05-sort.jcl            # Manipulation SORT
├── tp06-analyse-erreurs.jcl # Analyse erreurs (avec erreurs volontaires)
└── cleanup.jcl              # Nettoyage
```

---
*Formation COBOL - Travaux Pratiques JCL Chapitre V*
