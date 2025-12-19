# QCM Chapitre II - Organisation VSAM

*Questions du formateur - Certaines contiennent des erreurs intentionnelles a detecter*

---

## Question 1
Le Master Catalog :

- a) Contient tous les Data Sets du systeme
- b) Pointe vers les User Catalogs
- c) Est optionnel sur z/OS
- d) Ne contient que les fichiers VSAM

<details><summary>Reponse</summary>b) Pointe vers les User Catalogs</details>

---

## Question 2
Un User Catalog permet de :

- a) Remplacer le Master Catalog
- b) Organiser les Data Sets par domaine/application
- c) Stocker uniquement les fichiers temporaires
- d) Gerer les utilisateurs TSO

<details><summary>Reponse</summary>b) Organiser les Data Sets par domaine/application</details>

---

## Question 3
L'ALIAS dans un catalogue permet de :

- a) Creer une copie d'un fichier
- b) Associer un prefixe HLQ a un User Catalog
- c) Renommer un Data Set
- d) Supprimer un fichier

<details><summary>Reponse</summary>b) Associer un prefixe HLQ (High Level Qualifier) a un User Catalog</details>

---

## Question 4
**ERREUR A DETECTER** : SHAREOPTIONS(2,3) signifie : lecture multiple autorisee, une seule ecriture a la fois au niveau systeme, et acces libre au niveau inter-systeme.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - SHAREOPTIONS(2,3) : le premier chiffre (2) concerne le cross-region (meme systeme) et le second (3) le cross-system. (2) = lectures multiples, 1 ecriture. (3) = acces complet sans controle d'integrite</details>

---

## Question 5
La valeur SHAREOPTIONS(1,3) indique :

- a) Acces exclusif en lecture et ecriture
- b) Un seul utilisateur a la fois pour le fichier
- c) Lectures multiples sans ecriture simultanee
- d) Aucun controle de partage

<details><summary>Reponse</summary>b) Un seul utilisateur a la fois pour le fichier (exclusif)</details>

---

## Question 6
Le Data Space est :

- a) L'espace disque alloue a un fichier
- b) La zone memoire pour les buffers
- c) L'ensemble des Data Sets d'un catalogue
- d) L'espace de swap

<details><summary>Reponse</summary>c) L'ensemble des Data Sets geres par un catalogue</details>

---

## Question 7
Pour retrouver un Data Set, le systeme consulte :

- a) Uniquement la VTOC
- b) Le catalogue puis la VTOC
- c) Uniquement le Master Catalog
- d) Le JCL directement

<details><summary>Reponse</summary>b) Le catalogue (pour localiser le volume) puis la VTOC (pour les extents)</details>

---

## Question 8
**ERREUR A DETECTER** : Un fichier VSAM peut exister sans etre catalogue.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Contrairement aux fichiers non-VSAM, les fichiers VSAM DOIVENT etre catalogues pour exister</details>

---

*Total : 8 questions - Formation VSAM M2i*
