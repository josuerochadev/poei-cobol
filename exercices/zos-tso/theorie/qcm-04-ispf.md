# QCM 04 - ISPF/PDF

## Chapitre IV - Interface ISPF, Utilitaires et Editeur

---

### Question 1
Que signifient les acronymes ISPF et PDF ?

<details>
<summary>Reponse</summary>

- **ISPF** : Interactive System Productivity Facility
- **PDF** : Program Development Facility

ISPF fournit l'interface utilisateur (panels, menus, dialogues) tandis que PDF offre les outils de developpement (editeur, utilitaires, navigation).
</details>

---

### Question 2
Quelle est la relation entre TSO et ISPF ?

- [ ] a) ISPF remplace TSO
- [ ] b) ISPF s'execute au-dessus de TSO
- [ ] c) TSO s'execute au-dessus de ISPF
- [ ] d) Ils sont independants

<details>
<summary>Reponse</summary>

**b) ISPF s'execute au-dessus de TSO**

ISPF est une couche d'interface qui s'execute sous TSO. TSO reste le sous-systeme de base, ISPF fournit une interface plein ecran plus conviviale que le mode ligne de TSO.
</details>

---

### Question 3
Quelle touche de fonction permet d'obtenir l'aide dans ISPF ?

- [ ] a) PF1
- [ ] b) PF3
- [ ] c) PF7
- [ ] d) PF12

<details>
<summary>Reponse</summary>

**a) PF1**

Les touches de fonction ISPF standard :
- **PF1** = Help (aide)
- **PF3** = Exit/End (sortie)
- **PF7/PF8** = Scroll Up/Down
- **PF10/PF11** = Scroll Left/Right
- **PF12** = Retrieve (rappeler commande)
</details>

---

### Question 4
Quelle touche permet de sortir d'un panel ISPF ou de sauvegarder et quitter l'editeur ?

- [ ] a) PF1
- [ ] b) PF3
- [ ] c) PF7
- [ ] d) ESC

<details>
<summary>Reponse</summary>

**b) PF3**

PF3 (Exit/End) est la touche universelle pour :
- Sortir d'un panel et revenir au precedent
- Sauvegarder et quitter l'editeur
- Valider une action et retourner
</details>

---

### Question 5
Quel raccourci ISPF permet d'acceder directement a l'utilitaire Dslist ?

- [ ] a) =1
- [ ] b) =2
- [ ] c) =3.4
- [ ] d) =S

<details>
<summary>Reponse</summary>

**c) =3.4**

Les raccourcis ISPF courants :
- **=0** : Settings
- **=1** : View
- **=2** : Edit
- **=3** : Utilities
- **=3.4** : Dslist (le plus utilise)
- **=S** : SDSF
- **=X** : Exit ISPF
</details>

---

### Question 6
Quelle option ISPF permet d'editer un fichier ?

- [ ] a) Option 1
- [ ] b) Option 2
- [ ] c) Option 3
- [ ] d) Option 4

<details>
<summary>Reponse</summary>

**b) Option 2**

- **Option 1** = View (lecture seule avec commandes editeur)
- **Option 2** = Edit (modification)
- **Option 3** = Utilities
- **Option 4** = Foreground (compilation interactive)
</details>

---

### Question 7
Quelle est la difference entre View (Option 1) et Browse ?

<details>
<summary>Reponse</summary>

| Aspect | View | Browse |
|--------|------|--------|
| **Modification** | Non | Non |
| **Commandes editeur** | Oui (FIND, COLS...) | Limitees |
| **Coloration syntaxique** | Oui | Non |
| **Profile** | Oui | Non |
| **Usage** | Consultation avancee | Consultation rapide |

View permet d'utiliser les commandes de l'editeur (recherche, exclusion) sans modifier le fichier.
</details>

---

### Question 8
Que signifie SDSF ?

- [ ] a) System Display and Search Facility
- [ ] b) Spool Display and Search Facility
- [ ] c) Storage Display and Search Facility
- [ ] d) System Data Search Function

<details>
<summary>Reponse</summary>

**b) Spool Display and Search Facility**

SDSF permet de gerer les jobs et le spool JES : visualiser les jobs en execution, consulter les sorties, purger, annuler, liberer des jobs.
</details>

---

### Question 9
Dans SDSF, quelle commande affiche le statut de tous les jobs ?

- [ ] a) DA
- [ ] b) ST
- [ ] c) O
- [ ] d) H

<details>
<summary>Reponse</summary>

**b) ST**

Commandes SDSF principales :
- **ST** = Status (tous les jobs)
- **DA** = Display Active (jobs en execution)
- **O** = Output queue (sorties disponibles)
- **H** = Held queue (sorties en attente)
- **I** = Input queue (jobs en attente d'execution)
</details>

---

### Question 10
Dans SDSF (ecran ST), quelle commande ligne permet de voir les sorties d'un job ?

- [ ] a) V
- [ ] b) S
- [ ] c) ?
- [ ] d) O

<details>
<summary>Reponse</summary>

**c) ?**

Commandes ligne SDSF :
- **?** = Afficher les sorties (SYSOUT)
- **S** = Selectionner/afficher le job
- **P** = Purger (supprimer)
- **C** = Annuler
- **A** = Liberer (release)
- **H** = Mettre en hold
</details>

---

### Question 11
Dans Dslist (3.4), quelle commande ligne permet d'editer un dataset ?

- [ ] a) V
- [ ] b) E
- [ ] c) B
- [ ] d) O

<details>
<summary>Reponse</summary>

**b) E**

Commandes ligne Dslist :
- **e** = Edit
- **v** = View
- **b** = Browse
- **d** = Delete
- **r** = Rename
- **i** = Info
- **m** = Members (liste membres PDS)
- **z** = Compress (PDS)
</details>

---

### Question 12
Quelle commande primaire de l'editeur ISPF permet de rechercher une chaine de caracteres ?

- [ ] a) SEARCH
- [ ] b) FIND
- [ ] c) LOCATE
- [ ] d) LOOK

<details>
<summary>Reponse</summary>

**b) FIND**

Syntaxe : `FIND 'texte' [ALL] [FIRST|LAST|NEXT|PREV]`

Exemples :
- `FIND 'MOVE'` - Trouve la premiere occurrence
- `FIND 'MOVE' ALL` - Trouve toutes les occurrences
- `F 'MOVE' ALL` - Forme abregee
</details>

---

### Question 13
Quelle commande primaire remplace une chaine par une autre ?

- [ ] a) REPLACE
- [ ] b) SUBSTITUTE
- [ ] c) CHANGE
- [ ] d) MODIFY

<details>
<summary>Reponse</summary>

**c) CHANGE**

Syntaxe : `CHANGE 'ancien' 'nouveau' [ALL]`

Exemples :
- `CHANGE 'WS-OLD' 'WS-NEW'` - Remplace la premiere occurrence
- `CHANGE 'WS-OLD' 'WS-NEW' ALL` - Remplace toutes les occurrences
- `C 'OLD' 'NEW' ALL` - Forme abregee
</details>

---

### Question 14
Quelle touche permet de repeter la derniere recherche (FIND) ?

- [ ] a) PF1
- [ ] b) PF5
- [ ] c) PF6
- [ ] d) PF12

<details>
<summary>Reponse</summary>

**b) PF5**

- **PF5** = RFIND (Repeat Find) - repete la recherche
- **PF6** = RCHANGE (Repeat Change) - repete le remplacement
</details>

---

### Question 15
Quelle commande ligne insere une nouvelle ligne apres la ligne courante ?

- [ ] a) A
- [ ] b) I
- [ ] c) N
- [ ] d) +

<details>
<summary>Reponse</summary>

**b) I**

- **I** = Insert (insere 1 ligne)
- **I5** = Insere 5 lignes
- **A** = After (destination pour copie/deplacement)
- **B** = Before (destination avant)
</details>

---

### Question 16
Comment supprimer un bloc de lignes dans l'editeur ISPF ?

<details>
<summary>Reponse</summary>

**Suppression d'un bloc avec DD :**

1. Taper **DD** sur la premiere ligne du bloc
2. Taper **DD** sur la derniere ligne du bloc
3. Appuyer sur Entree

Les commandes bloc utilisent des lettres doublees :
- **DD** = Delete bloc
- **CC** = Copy bloc
- **MM** = Move bloc
- **RR** = Repeat bloc
- **XX** = Exclude bloc
</details>

---

### Question 17
Comment copier un bloc de lignes apres une autre ligne ?

<details>
<summary>Reponse</summary>

**Copie d'un bloc avec CC et A :**

1. Taper **CC** sur la premiere ligne du bloc
2. Taper **CC** sur la derniere ligne du bloc
3. Taper **A** sur la ligne de destination (apres laquelle copier)
4. Appuyer sur Entree

Pour copier avant une ligne, utiliser **B** au lieu de **A**.
</details>

---

### Question 18
Quelle commande ligne decale une ligne vers la droite ?

- [ ] a) R
- [ ] b) >
- [ ] c) +
- [ ] d) >>

<details>
<summary>Reponse</summary>

**b) >**

Commandes de decalage :
- **>** = Decale vers la droite (2 positions par defaut)
- **<** = Decale vers la gauche
- **>5** = Decale de 5 positions vers la droite
- **>>** = Decale un bloc vers la droite
</details>

---

### Question 19
Quelle commande primaire affiche la regle des colonnes ?

- [ ] a) RULER
- [ ] b) COLS
- [ ] c) NUMBERS
- [ ] d) GRID

<details>
<summary>Reponse</summary>

**b) COLS**

La commande COLS affiche une ligne de reference avec les numeros de colonnes :
```
=COLS> ----+----1----+----2----+----3----+----4----+----5----+----6----+----7--
```

Tres utile pour verifier le positionnement en COBOL (colonnes 7-72).
</details>

---

### Question 20
Quelle commande primaire sauvegarde le fichier sans quitter l'editeur ?

- [ ] a) WRITE
- [ ] b) SAVE
- [ ] c) STORE
- [ ] d) COMMIT

<details>
<summary>Reponse</summary>

**b) SAVE**

- **SAVE** = Sauvegarde et reste dans l'editeur
- **END** (PF3) = Sauvegarde et quitte
- **CANCEL** = Quitte sans sauvegarder (annule les modifications)
</details>

---

### Question 21
Quelle commande ligne masque (exclut) une ligne de l'affichage ?

- [ ] a) H
- [ ] b) X
- [ ] c) E
- [ ] d) M

<details>
<summary>Reponse</summary>

**b) X**

Commandes d'exclusion :
- **X** = Exclure 1 ligne
- **X5** = Exclure 5 lignes
- **XX** = Exclure un bloc
- **RESET** = Reafficher toutes les lignes exclues

Commandes pour reveler des lignes exclues :
- **F** = First (premiere ligne du bloc exclu)
- **L** = Last (derniere ligne)
- **S** = Show (lignes significatives)
</details>

---

### Question 22
Quelle commande primaire masque toutes les lignes contenant un texte ?

- [ ] a) HIDE
- [ ] b) MASK
- [ ] c) EXCLUDE
- [ ] d) FILTER

<details>
<summary>Reponse</summary>

**c) EXCLUDE**

Syntaxe : `EXCLUDE 'texte' ALL`

Exemples :
- `EXCLUDE '*' ALL` - Masque tous les commentaires COBOL
- `EXCLUDE ALL` - Masque toutes les lignes
- `FIND 'PERFORM' ALL` + `FLIP` - Affiche uniquement les PERFORM

La commande FLIP inverse les lignes visibles/masquees.
</details>

---

### Question 23
Quelle option ISPF permet de gerer les membres d'une bibliotheque (PDS) ?

- [ ] a) Option 3.1
- [ ] b) Option 3.2
- [ ] c) Option 3.3
- [ ] d) Option 3.4

<details>
<summary>Reponse</summary>

**a) Option 3.1**

Options Utilities (3.x) :
- **3.1** = Library (gestion membres PDS)
- **3.2** = Data Set (operations sur datasets)
- **3.3** = Move/Copy
- **3.4** = Dslist (liste datasets)
- **3.V** = VSAM utilities
</details>

---

### Question 24
Dans l'option 3.2 (Data Set Utility), quelle lettre permet d'allouer un nouveau dataset ?

- [ ] a) N
- [ ] b) C
- [ ] c) A
- [ ] d) D

<details>
<summary>Reponse</summary>

**c) A**

Options de l'utilitaire Data Set (3.2) :
- **A** = Allocate (creer nouveau dataset)
- **R** = Rename
- **D** = Delete
- **C** = Catalog
- **U** = Uncatalog
- **I** = Information (detailed)
</details>

---

### Question 25
Associez chaque commande editeur a sa fonction :

| Commande | Fonction |
|----------|----------|
| 1. UC | a) Supprimer |
| 2. LC | b) Convertir en majuscules |
| 3. R | c) Repeter (dupliquer) |
| 4. D | d) Convertir en minuscules |

<details>
<summary>Reponse</summary>

| Commande | Fonction |
|----------|----------|
| 1. UC | **b) Convertir en majuscules** |
| 2. LC | **d) Convertir en minuscules** |
| 3. R | **c) Repeter (dupliquer)** |
| 4. D | **a) Supprimer** |

- **UC** = Uppercase
- **LC** = Lowercase
- **R** / **RR** = Repeat
- **D** / **DD** = Delete
</details>

---

## Resume

| Element | Description |
|---------|-------------|
| **PF1** | Help |
| **PF3** | Exit/End |
| **PF5** | RFIND |
| **PF6** | RCHANGE |
| **PF7/8** | Scroll Up/Down |
| **=3.4** | Dslist |
| **=S** | SDSF |
| **FIND** | Rechercher |
| **CHANGE** | Remplacer |
| **COLS** | Afficher colonnes |
| **I, D, C, M, R** | Insert, Delete, Copy, Move, Repeat |
| **DD, CC, MM** | Commandes bloc |
| **X, XX** | Exclure lignes |
| **>, <** | Decaler |

---
*Formation z/OS - M2i Formation*
