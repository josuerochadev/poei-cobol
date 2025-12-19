# Travaux Pratiques VSAM - Synthese

## Objectifs

Ce TP de synthese permet de valider la maitrise des concepts VSAM :
- Definition de clusters de differents types
- Creation et gestion des AIX
- Manipulation avec IDCAMS
- Gestion des GDG

## Fichiers

| Fichier | Description |
|---------|-------------|
| `tp-gdg-formateur.jcl` | **TP GDG du formateur** (TESTGDG.COMPTE.MENSUEL, LIMIT(4)) |
| `tp01-clients.jcl` | Gestion complete fichier clients avec AIX |
| `tp03-workflow.jcl` | Workflow batch complet |
| `cleanup.jcl` | Nettoyage de tous les objets |

---

## Partie 1 : QCM de Comprehension

### Question 1
Quelle est la difference entre un CI et une CA ?

- [ ] a) Un CI contient plusieurs CA
- [ ] b) Une CA contient plusieurs CI
- [ ] c) Ils sont identiques
- [ ] d) Un CI est pour KSDS, une CA pour ESDS

<details>
<summary>Reponse</summary>

**b) Une CA contient plusieurs CI**

- CI (Control Interval) : Plus petite unite d'echange disque/memoire (512-32 Ko)
- CA (Control Area) : Ensemble de CI contigus (typiquement 1 cylindre = 15 tracks)
</details>

---

### Question 2
Quel type de cluster VSAM a un composant INDEX ?

- [ ] a) ESDS et RRDS
- [ ] b) KSDS et VRRDS
- [ ] c) LDS et ESDS
- [ ] d) Tous les types

<details>
<summary>Reponse</summary>

**b) KSDS et VRRDS**

- KSDS : Composant DATA + INDEX
- VRRDS : Composant DATA + INDEX
- ESDS, RRDS, LDS : Composant DATA uniquement
</details>

---

### Question 3
Que fait BLDINDEX ?

- [ ] a) Definit un index
- [ ] b) Construit les entrees d'un AIX
- [ ] c) Supprime un index
- [ ] d) Verifie un index

<details>
<summary>Reponse</summary>

**b) Construit les entrees d'un AIX**

BLDINDEX lit le cluster de base et construit les entrees dans l'AIX. Le cluster de base doit contenir des donnees.
</details>

---

### Question 4
Que signifie UPGRADE pour un AIX ?

- [ ] a) Ameliorer les performances
- [ ] b) Mise a jour automatique de l'AIX
- [ ] c) Augmenter la taille de l'AIX
- [ ] d) Compresser l'AIX

<details>
<summary>Reponse</summary>

**b) Mise a jour automatique de l'AIX**

UPGRADE signifie que l'AIX sera automatiquement mis a jour lorsque le cluster de base est modifie (ajout, suppression, modification).
</details>

---

### Question 5
Quel code retour VSAM indique une cle en double ?

- [ ] a) 10
- [ ] b) 21
- [ ] c) 22
- [ ] d) 23

<details>
<summary>Reponse</summary>

**c) 22**

- 10 = Fin de fichier
- 21 = Erreur de sequence
- 22 = Cle primaire en double
- 23 = Enregistrement non trouve
</details>

---

### Question 6
Que fait SHAREOPTIONS(2,3) ?

- [ ] a) 2 lecteurs, 3 ecrivains max
- [ ] b) Lecture pendant ecriture autorisee, pas de controle cross-system
- [ ] c) 2 systemes, 3 regions
- [ ] d) Priorite 2 region, priorite 3 systeme

<details>
<summary>Reponse</summary>

**b) Lecture pendant ecriture autorisee, pas de controle cross-system**

- 2 = Cross-region : Lecture multiple ET ecriture unique simultanee
- 3 = Cross-system : Pas de controle d'integrite
</details>

---

### Question 7
Quelle reference GDG cree une nouvelle generation ?

- [ ] a) (0)
- [ ] b) (+1)
- [ ] c) (-1)
- [ ] d) (NEW)

<details>
<summary>Reponse</summary>

**b) (+1)**

- (0) = Generation actuelle
- (+1) = Nouvelle generation a creer
- (-1) = Generation precedente
</details>

---

### Question 8
Que fait VERIFY ?

- [ ] a) Verifie la syntaxe du JCL
- [ ] b) Repare un fichier VSAM mal ferme
- [ ] c) Verifie les permissions
- [ ] d) Valide les donnees

<details>
<summary>Reponse</summary>

**b) Repare un fichier VSAM mal ferme**

VERIFY reinitialise les indicateurs de fin de fichier dans le catalogue apres un ABEND ou une fermeture anormale.
</details>

---

### Question 9
Quelle option DELETE efface les donnees avec des zeros ?

- [ ] a) PURGE
- [ ] b) FORCE
- [ ] c) ERASE
- [ ] d) SCRATCH

<details>
<summary>Reponse</summary>

**c) ERASE**

ERASE ecrase les donnees avec des zeros binaires avant la suppression. Utilise pour les donnees sensibles.
</details>

---

### Question 10
Combien d'octets fait un RDF ?

- [ ] a) 2 octets
- [ ] b) 3 octets
- [ ] c) 4 octets
- [ ] d) 8 octets

<details>
<summary>Reponse</summary>

**b) 3 octets**

- RDF (Record Definition Field) = 3 octets
- CIDF (Control Interval Definition Field) = 4 octets
</details>

---

## Partie 2 : Travaux Pratiques

### TP 01 : Gestion Complete d'un Fichier Clients

**Fichier :** `tp01-clients.jcl`

**Objectif :** Creer et gerer un fichier clients complet

**Instructions :**
1. Creer un KSDS clients (numero, nom, ville, solde)
2. Charger des donnees de test
3. Creer un AIX sur la ville (NONUNIQUEKEY)
4. Creer un AIX sur le solde (NONUNIQUEKEY)
5. Afficher les statistiques LISTCAT
6. Exporter vers fichier sequentiel

---

### TP 02 : Gestion des GDG (Formateur)

**Fichier :** `tp-gdg-formateur.jcl`

**Objectif :** Maitriser les Generation Data Groups (exercice exact du formateur)

**Instructions :**
1. Definir un GDG TESTGDG.COMPTE.MENSUEL avec LIMIT(4)
2. Creer ESDS et charger les donnees clients
3. Convertir ESDS vers KSDS avec REPRO
4. Creer 4 generations mensuelles
5. Creer une 5eme generation (observer la rotation)
6. Concatener toutes les generations
7. LISTCAT et PRINT pour verification

---

### TP 03 : Workflow Batch Complet

**Fichier :** `tp03-workflow.jcl`

**Objectif :** Simuler un workflow batch reel

**Instructions :**
1. VERIFY des fichiers de travail
2. Charger les donnees du jour
3. Mettre a jour les totaux
4. Creer une sauvegarde (EXPORT)
5. Generer un rapport (PRINT)
6. Archiver dans un GDG

---

## Fichier de Nettoyage

**Fichier :** `cleanup.jcl`

Supprime tous les objets crees pendant les TP.

---

## Criteres d'Evaluation

| Critere | Points |
|---------|--------|
| QCM (10 questions) | 20 |
| TP01 - Clients et AIX | 30 |
| TP02 - GDG Formateur | 25 |
| TP03 - Workflow | 25 |
| **Total** | **100** |

---

## TP GDG Formateur (tp-gdg-formateur.jcl)

Ce TP correspond exactement a l'exercice de fin de module du formateur :

1. Definition GDG TESTGDG.COMPTE.MENSUEL avec LIMIT(4)
2. Creation ESDS et chargement donnees clients
3. Conversion ESDS vers KSDS avec REPRO
4. Creation de 4 generations mensuelles
5. Creation d'une 5eme generation pour observer la rotation
6. Concatenation des generations
7. LISTCAT et PRINT pour verification

### Structure des donnees

```
Pos 01-06 : Code Client (cle)
Pos 07-26 : Nom Client
Pos 27-27 : Etat operation (D=Debit, C=Credit)
Pos 28-37 : Solde (10 chiffres)
Pos 38-45 : Date operation (AAAAMMJJ)
Pos 46-80 : Reserve
```

---

## Dossier extra/

Le dossier `extra/` contient des TP supplementaires avec la nomenclature `FTEST.*` pour approfondir les connaissances.

---
*Travaux Pratiques VSAM - M2i Formation*
