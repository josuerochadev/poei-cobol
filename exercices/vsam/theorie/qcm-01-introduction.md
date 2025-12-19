# QCM Chapitre I - Introduction a VSAM

*Questions du formateur - Certaines contiennent des erreurs intentionnelles a detecter*

---

## Question 1
Quelle est l'expression complete de l'acronyme VSAM ?

- a) Virtual Sequential Access Mode
- b) Virtual Storage Access Method
- c) Volume Storage Access Mode
- d) Virtual System Access Method

<details><summary>Reponse</summary>b) Virtual Storage Access Method</details>

---

## Question 2
Quel est le type de disque le plus utilise sur mainframe IBM ?

- a) SCSI
- b) SATA
- c) 3390
- d) SSD

<details><summary>Reponse</summary>c) 3390</details>

---

## Question 3
La VTOC (Volume Table Of Contents) contient :

- a) Les donnees des fichiers
- b) Les informations sur les Data Sets du volume
- c) Le catalogue systeme
- d) Les programmes executables

<details><summary>Reponse</summary>b) Les informations sur les Data Sets du volume</details>

---

## Question 4
Quel type VSAM permet l'acces par cle primaire ?

- a) ESDS
- b) KSDS
- c) RRDS
- d) LDS

<details><summary>Reponse</summary>b) KSDS (Key Sequenced Data Set)</details>

---

## Question 5
Un fichier ESDS permet :

- a) L'acces direct par numero de record
- b) L'acces sequentiel uniquement avec ajout en fin
- c) L'acces par cle unique
- d) Le stockage de pages memoire

<details><summary>Reponse</summary>b) L'acces sequentiel uniquement avec ajout en fin</details>

---

## Question 6
**ERREUR A DETECTER** : Un fichier RRDS stocke les enregistrements avec une cle alphanumerique de longueur variable.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Un RRDS utilise un numero de slot NUMERIQUE (RRN - Relative Record Number), pas une cle alphanumerique</details>

---

## Question 7
Le format FB (Fixed Blocked) signifie :

- a) Enregistrements de longueur fixe regroupes en blocs
- b) Enregistrements de longueur variable
- c) Fichier binaire fixe
- d) Format de base fixe

<details><summary>Reponse</summary>a) Enregistrements de longueur fixe regroupes en blocs</details>

---

## Question 8
Un PDS (Partitioned Data Set) :

- a) Ne peut contenir qu'un seul membre
- b) Contient un repertoire et plusieurs membres
- c) Est un type de fichier VSAM
- d) Ne supporte pas les programmes source

<details><summary>Reponse</summary>b) Contient un repertoire et plusieurs membres</details>

---

## Question 9
**ERREUR A DETECTER** : Un fichier LDS (Linear Data Set) est principalement utilise pour stocker des enregistrements structures avec des cles.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Un LDS stocke des donnees non structurees (stream de bytes), utilise pour DB2 tablespaces ou memory-mapped files</details>

---

*Total : 9 questions - Formation VSAM M2i*
