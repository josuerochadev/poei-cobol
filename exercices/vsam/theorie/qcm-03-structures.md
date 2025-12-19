# QCM Chapitre III - Structures des Data Sets

*Questions du formateur - Certaines contiennent des erreurs intentionnelles a detecter*

---

## Question 1
Le Control Interval (CI) est :

- a) L'unite de transfert entre disque et memoire
- b) Le temps entre deux acces disque
- c) La taille maximale d'un enregistrement
- d) L'intervalle de controle des erreurs

<details><summary>Reponse</summary>a) L'unite de transfert entre disque et memoire</details>

---

## Question 2
Quelle est la taille typique d'un CI ?

- a) 512 octets
- b) 4096 octets (4 KB)
- c) 1 MB
- d) 1 piste

<details><summary>Reponse</summary>b) 4096 octets (4 KB) - valeurs courantes : 2KB, 4KB, 8KB, jusqu'a 32KB</details>

---

## Question 3
Le Control Area (CA) contient :

- a) Un seul CI
- b) Plusieurs CI
- c) Toutes les donnees du fichier
- d) Uniquement l'index

<details><summary>Reponse</summary>b) Plusieurs CI - un CA regroupe un ensemble de CI</details>

---

## Question 4
Le CIDF (Control Interval Definition Field) contient :

- a) Les donnees utilisateur
- b) L'espace libre et le nombre de RDF
- c) La cle de l'enregistrement
- d) L'adresse du prochain CI

<details><summary>Reponse</summary>b) L'espace libre disponible et le nombre de RDF dans le CI</details>

---

## Question 5
Le RDF (Record Definition Field) indique :

- a) La taille et la position d'un enregistrement
- b) L'adresse du fichier
- c) Le type de VSAM
- d) Le nom du Data Set

<details><summary>Reponse</summary>a) La longueur et le type de chaque enregistrement dans le CI</details>

---

## Question 6
**ERREUR A DETECTER** : Un enregistrement SPANNED peut s'etendre sur plusieurs Control Areas.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Un enregistrement SPANNED peut s'etendre sur plusieurs CI, mais pas sur plusieurs CA</details>

---

## Question 7
L'index d'un KSDS est organise en :

- a) Un seul niveau
- b) Deux niveaux : sequence set et index set
- c) Trois niveaux minimum
- d) Il n'y a pas d'index

<details><summary>Reponse</summary>b) Deux niveaux : sequence set (niveau bas, pointe vers les CI de donnees) et index set (niveaux superieurs)</details>

---

## Question 8
Un Alternate Index (AIX) permet :

- a) De dupliquer les donnees
- b) D'acceder aux donnees par une cle alternative
- c) De compresser le fichier
- d) De sauvegarder le fichier

<details><summary>Reponse</summary>b) D'acceder aux donnees par une cle secondaire/alternative</details>

---

## Question 9
**ERREUR A DETECTER** : Un AIX sur un ESDS utilise le RBA comme pointeur, tandis qu'un AIX sur un KSDS utilise la cle primaire.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>a) Vrai - C'est exact : AIX/ESDS pointe par RBA, AIX/KSDS pointe par cle primaire</details>

---

## Question 10
Le parametre UPGRADE sur un AIX signifie :

- a) L'AIX sera mis a jour automatiquement lors des modifications du cluster base
- b) L'AIX doit etre reconstruit manuellement
- c) L'AIX est en lecture seule
- d) L'AIX utilise une version superieure du format

<details><summary>Reponse</summary>a) L'AIX sera automatiquement mis a jour quand le cluster de base est modifie</details>

---

*Total : 10 questions - Formation VSAM M2i*
