# QCM 02 - Organisation du Systeme CICS

## Questions (Chapitre II)

### Question 1
Comment s'appelle la table qui definit les programmes CICS ?

- [ ] a) FCT (File Control Table)
- [ ] b) PPT (Processing Program Table)
- [ ] c) PCT (Program Control Table)
- [ ] d) TCT (Terminal Control Table)

<details>
<summary>Reponse</summary>

**b) PPT (Processing Program Table)**

La PPT contient la liste des programmes executables sous CICS avec leurs caracteristiques (langage, resident/non-resident, etc.).
</details>

---

### Question 2
Quelle table fait le lien entre un code transaction et un programme ?

- [ ] a) FCT (File Control Table)
- [ ] b) PPT (Processing Program Table)
- [ ] c) PCT (Program Control Table)
- [ ] d) TCT (Terminal Control Table)

<details>
<summary>Reponse</summary>

**c) PCT (Program Control Table)**

La PCT associe chaque code transaction (4 car.) au programme qui doit etre execute.
</details>

---

### Question 3
Quelle table contient la definition des fichiers VSAM ?

- [ ] a) FCT (File Control Table)
- [ ] b) PPT (Processing Program Table)
- [ ] c) PCT (Program Control Table)
- [ ] d) DCT (Destination Control Table)

<details>
<summary>Reponse</summary>

**a) FCT (File Control Table)**

La FCT definit les fichiers accessibles sous CICS (nom logique, type, mode d'acces, etc.).
</details>

---

### Question 4
Associez chaque table CICS a sa fonction :

| Table | Fonction |
|-------|----------|
| PPT | ? |
| PCT | ? |
| FCT | ? |
| TCT | ? |

<details>
<summary>Reponse</summary>

| Table | Fonction |
|-------|----------|
| **PPT** | Processing Program Table - Definition des programmes |
| **PCT** | Program Control Table - Association Transaction/Programme |
| **FCT** | File Control Table - Definition des fichiers |
| **TCT** | Terminal Control Table - Definition des terminaux |
</details>

---

### Question 5
Quelle transaction permet d'administrer les ressources CICS ?

- [ ] a) CEDA
- [ ] b) CEDF
- [ ] c) CEMT
- [ ] d) CEOT

<details>
<summary>Reponse</summary>

**c) CEMT**

CEMT (CICS Execute Master Terminal) permet d'administrer les ressources CICS (fichiers, programmes, transactions, etc.) en temps reel.
</details>

---

### Question 6
Quelle transaction permet de definir de nouvelles ressources CICS ?

- [ ] a) CEDA
- [ ] b) CEDF
- [ ] c) CEMT
- [ ] d) CESN

<details>
<summary>Reponse</summary>

**a) CEDA**

CEDA (CICS Execution Diagnostic Aid for resource definition) permet de definir, modifier et installer des ressources CICS dans le CSD (CICS System Definition).
</details>

---

### Question 7
Quelle transaction permet de debugger un programme CICS ?

- [ ] a) CEDA
- [ ] b) CEDF
- [ ] c) CEMT
- [ ] d) CEBR

<details>
<summary>Reponse</summary>

**b) CEDF**

CEDF (CICS Execution Diagnostic Facility) est le debugger interactif de CICS. Il permet de tracer l'execution d'une transaction pas a pas.
</details>

---

### Question 8
Qu'est-ce que le CSD ?

- [ ] a) CICS System Data
- [ ] b) CICS System Definition
- [ ] c) CICS Storage Directory
- [ ] d) CICS Service Definition

<details>
<summary>Reponse</summary>

**b) CICS System Definition**

Le CSD est le fichier VSAM qui contient les definitions de toutes les ressources CICS (programmes, transactions, fichiers, etc.).
</details>

---

### Question 9
Quelle commande CEMT permet d'activer un fichier ?

- [ ] a) `CEMT SET FILE(xxx) OPEN`
- [ ] b) `CEMT SET FILE(xxx) ENABLED`
- [ ] c) `CEMT OPEN FILE(xxx)`
- [ ] d) `CEMT FILE(xxx) START`

<details>
<summary>Reponse</summary>

**b) `CEMT SET FILE(xxx) ENABLED`**

Pour activer un fichier, on utilise `CEMT SET FILE(nom) ENABLED`. Pour le desactiver : `CEMT SET FILE(nom) DISABLED`.
</details>

---

### Question 10
Quel est le role de la DCT (Destination Control Table) ?

- [ ] a) Definir les terminaux
- [ ] b) Definir les files d'attente temporaires et de sortie
- [ ] c) Definir les programmes
- [ ] d) Definir les transactions

<details>
<summary>Reponse</summary>

**b) Definir les files d'attente temporaires et de sortie**

La DCT definit les Transient Data Queues (TDQ) utilisees pour les echanges de donnees asynchrones et les impressions.
</details>

---

### Question 11
Comment s'appellent les zones memoire partagees en CICS ?

- [ ] a) COMMAREA
- [ ] b) TS Queue
- [ ] c) TD Queue
- [ ] d) Working Storage

<details>
<summary>Reponse</summary>

**b) TS Queue (Temporary Storage Queue)**

Les TS Queues sont des zones memoire temporaires partagees. La COMMAREA est une zone de communication entre programmes mais pas partagee globalement.
</details>

---

### Question 12
Quelle transaction permet de visualiser le contenu d'une TS Queue ?

- [ ] a) CEDA
- [ ] b) CEDF
- [ ] c) CEBR
- [ ] d) CEMT

<details>
<summary>Reponse</summary>

**c) CEBR**

CEBR (CICS Execute BRowse) permet de visualiser et modifier le contenu des Temporary Storage Queues.
</details>

---

## Resume des tables CICS

| Table | Nom complet | Contenu |
|-------|-------------|---------|
| PPT | Processing Program Table | Programmes executables |
| PCT | Program Control Table | Association Transaction/Programme |
| FCT | File Control Table | Fichiers VSAM |
| TCT | Terminal Control Table | Terminaux |
| DCT | Destination Control Table | Files d'attente (TDQ) |
| TST | Temporary Storage Table | Queues temporaires (TS) |

## Transactions utilitaires CICS

| Transaction | Role |
|-------------|------|
| CEDA | Definition de ressources |
| CEDF | Debugger interactif |
| CEMT | Administration des ressources |
| CEBR | Browse des TS Queues |
| CESN | Sign On (connexion) |
| CESF | Sign Off (deconnexion) |

---
*Formation CICS - M2i Formation*
