# QCM 02 - Fonctionnement de z/OS

## Chapitre II - Memoire, DAT, Taches, Travaux et Donnees

---

### Question 1
Quelle est la difference principale entre la memoire CPC et la memoire auxiliaire ?

<details>
<summary>Reponse</summary>

| Caracteristique | Memoire CPC | Memoire auxiliaire |
|-----------------|-------------|-------------------|
| **Type physique** | RAM | Disques DASD |
| **Vitesse** | Nanosecondes | Millisecondes |
| **Persistance** | Volatile | Permanente |
| **Cout** | Eleve | Modere |
| **Usage** | Execution programmes | Pagination, swap |

La memoire CPC (Central Processing Complex) est la memoire vive rapide, tandis que la memoire auxiliaire est le stockage sur disque pour la pagination.
</details>

---

### Question 2
Quelle est la taille standard d'une page en memoire virtuelle z/OS ?

- [ ] a) 1 Ko
- [ ] b) 4 Ko
- [ ] c) 8 Ko
- [ ] d) 16 Ko

<details>
<summary>Reponse</summary>

**b) 4 Ko**

La taille standard d'une page est de 4 Ko (4096 octets). z/OS supporte egalement des "large pages" de 1 Mo et des pages de 2 Go pour les applications haute performance.
</details>

---

### Question 3
Comment appelle-t-on l'unite de memoire physique (CPC) qui correspond a une page virtuelle ?

- [ ] a) Bloc
- [ ] b) Segment
- [ ] c) Frame
- [ ] d) Slot

<details>
<summary>Reponse</summary>

**c) Frame**

Une **frame** est l'unite de memoire physique (CPC) de 4 Ko. Une **page** virtuelle est mappee sur une frame reelle. Un **slot** est l'emplacement d'une page sur le stockage auxiliaire (disque).
</details>

---

### Question 4
Que signifie DAT ?

- [ ] a) Data Access Table
- [ ] b) Dynamic Address Translation
- [ ] c) Direct Address Transfer
- [ ] d) Disk Allocation Table

<details>
<summary>Reponse</summary>

**b) Dynamic Address Translation**

DAT est le mecanisme materiel qui traduit les adresses virtuelles en adresses reelles (physiques). Cette traduction est effectuee par le processeur a l'aide de tables de translation.
</details>

---

### Question 5
Qu'est-ce qu'un "Page Fault" ?

- [ ] a) Une erreur de programmation
- [ ] b) Une tentative d'acces a une page non residente en memoire CPC
- [ ] c) Une page corrompue
- [ ] d) Un debordement de memoire

<details>
<summary>Reponse</summary>

**b) Une tentative d'acces a une page non residente en memoire CPC**

Un Page Fault se produit quand un programme accede a une page virtuelle qui n'est pas actuellement chargee en memoire physique (CPC). Le systeme doit alors effectuer un "Page-In" pour charger la page depuis le stockage auxiliaire.
</details>

---

### Question 6
Comment s'appelle l'operation de chargement d'une page du disque vers la memoire CPC ?

- [ ] a) Page-Out
- [ ] b) Page-In
- [ ] c) Page-Swap
- [ ] d) Page-Load

<details>
<summary>Reponse</summary>

**b) Page-In**

- **Page-In** : Chargement d'une page du disque (auxiliaire) vers la memoire CPC
- **Page-Out** : Ecriture d'une page de la memoire CPC vers le disque
- **Swapping** : Deplacement d'un espace d'adressage entier
</details>

---

### Question 7
Quel algorithme z/OS utilise-t-il pour choisir les pages a evincer (page stealing) ?

- [ ] a) FIFO (First In First Out)
- [ ] b) LRU (Least Recently Used)
- [ ] c) Random
- [ ] d) Priority-based

<details>
<summary>Reponse</summary>

**b) LRU (Least Recently Used)**

z/OS utilise l'algorithme LRU qui evince les pages les moins recemment utilisees. Un bit de reference est utilise pour suivre les acces aux pages.
</details>

---

### Question 8
A quelle adresse se trouve "The Line" qui separe l'espace 24 bits de l'espace 31 bits ?

- [ ] a) 1 Mo
- [ ] b) 16 Mo
- [ ] c) 256 Mo
- [ ] d) 2 Go

<details>
<summary>Reponse</summary>

**b) 16 Mo**

"The Line" se trouve a 16 Mo (16 megaoctets). C'est la limite de l'adressage 24 bits (2^24 = 16 777 216 octets). Les programmes legacy 24 bits ne peuvent pas adresser au-dela.
</details>

---

### Question 9
A quelle adresse se trouve "The Bar" qui separe l'espace 31 bits de l'espace 64 bits ?

- [ ] a) 16 Mo
- [ ] b) 256 Mo
- [ ] c) 2 Go
- [ ] d) 4 Go

<details>
<summary>Reponse</summary>

**c) 2 Go**

"The Bar" se trouve a 2 Go (2^31 = 2 147 483 648 octets). C'est la limite de l'adressage 31 bits. Au-dela se trouve l'espace "High Virtual Storage" pour les programmes 64 bits.
</details>

---

### Question 10
Quelle zone de l'espace d'adressage contient le noyau z/OS ?

- [ ] a) PSA
- [ ] b) Nucleus
- [ ] c) LPA
- [ ] d) Private Area

<details>
<summary>Reponse</summary>

**b) Nucleus**

Le **Nucleus** contient le noyau z/OS et les routines systeme essentielles. Il est situe au debut de l'espace d'adressage, juste apres la PSA.

- **PSA** (Prefixed Save Area) : Zone de 4 Ko par processeur
- **LPA** (Link Pack Area) : Modules reentrants partages
- **Private Area** : Code et donnees du programme utilisateur
</details>

---

### Question 11
Que signifie TCB ?

- [ ] a) Transaction Control Block
- [ ] b) Task Control Block
- [ ] c) Terminal Control Block
- [ ] d) Table Control Block

<details>
<summary>Reponse</summary>

**b) Task Control Block**

Un TCB represente une tache utilisateur dans z/OS. Chaque programme en execution possede un TCB qui contient son contexte d'execution (registres, etat, priorite).
</details>

---

### Question 12
Quelle est la difference entre un TCB et un SRB ?

<details>
<summary>Reponse</summary>

| Aspect | TCB (Task Control Block) | SRB (Service Request Block) |
|--------|--------------------------|----------------------------|
| **Nature** | Tache utilisateur | Routine systeme |
| **Interruptible** | Oui | Non |
| **Wait possible** | Oui (I/O, events) | Non |
| **Priorite** | Normale | Elevee |
| **Exemple** | Programme COBOL | Routine I/O systeme |

Les SRB sont utilises pour les operations systeme rapides qui ne doivent pas etre interrompues.
</details>

---

### Question 13
Quelles sont les phases d'execution d'un job batch gere par JES ?

<details>
<summary>Reponse</summary>

**Phases d'un job JES :**

1. **INPUT** : Lecture et reception du JCL
2. **CONVERSION** : Analyse syntaxique, creation du JCT (Job Control Table)
3. **INPUT QUEUE** : Mise en file d'attente selon classe et priorite
4. **EXECUTION** : Selection par un Initiator, allocation des ressources, execution
5. **OUTPUT** : Generation des sorties (SYSOUT)
6. **PURGE** : Liberation des ressources apres consultation
</details>

---

### Question 14
Qu'est-ce qu'un Initiator ?

- [ ] a) Un programme de demarrage
- [ ] b) Un espace d'adressage dedie a l'execution des jobs batch
- [ ] c) Un type de dataset
- [ ] d) Un composant reseau

<details>
<summary>Reponse</summary>

**b) Un espace d'adressage dedie a l'execution des jobs batch**

Un Initiator est un espace d'adressage z/OS qui selectionne et execute les jobs batch. Chaque Initiator est configure pour traiter certaines classes de jobs (CLASS=A, B, C...).
</details>

---

### Question 15
A quoi sert le parametre CLASS dans une carte JOB ?

- [ ] a) Definir le type de sortie
- [ ] b) Definir la priorite et la file d'execution du job
- [ ] c) Definir la taille memoire
- [ ] d) Definir le proprietaire

<details>
<summary>Reponse</summary>

**b) Definir la priorite et la file d'execution du job**

Le parametre CLASS (ex: CLASS=A) determine dans quelle file d'attente le job sera place et quel type d'Initiator pourra l'executer. Differentes classes ont des ressources et priorites differentes.
</details>

---

### Question 16
Quelle est la difference entre RECFM=FB et RECFM=VB ?

<details>
<summary>Reponse</summary>

| Attribut | FB (Fixed Blocked) | VB (Variable Blocked) |
|----------|-------------------|----------------------|
| **Longueur** | Fixe (tous les enregistrements identiques) | Variable (longueur differente par enregistrement) |
| **Structure** | Enregistrements contigus | RDW (4 octets) + donnees pour chaque enregistrement |
| **BDW** | Non | Oui (Block Descriptor Word) |
| **Usage** | Sources COBOL, JCL (80 car.) | Texte, logs, donnees heterogenes |
| **Efficacite** | Meilleure (pas d'overhead) | Flexible mais overhead |

- **F** = Fixed, **V** = Variable
- **B** = Blocked (plusieurs enregistrements par bloc)
</details>

---

### Question 17
Quels sont les quatre types de fichiers VSAM ?

<details>
<summary>Reponse</summary>

**Types de fichiers VSAM :**

| Type | Nom complet | Acces | Usage |
|------|-------------|-------|-------|
| **ESDS** | Entry-Sequenced Data Set | Sequentiel | Logs, journaux, ajouts en fin |
| **KSDS** | Key-Sequenced Data Set | Direct + Sequentiel | Fichiers indexes, bases de donnees |
| **RRDS** | Relative Record Data Set | Par numero | Acces par position relative |
| **LDS** | Linear Data Set | Flux d'octets | DB2, objets binaires |

Le KSDS est le plus utilise car il permet un acces direct par cle et un parcours sequentiel.
</details>

---

### Question 18
Qu'est-ce qu'un CI (Control Interval) dans VSAM ?

- [ ] a) Un index de controle
- [ ] b) L'unite de transfert entre memoire et disque
- [ ] c) Un intervalle de temps
- [ ] d) Un code d'erreur

<details>
<summary>Reponse</summary>

**b) L'unite de transfert entre memoire et disque**

Le CI (Control Interval) est l'unite de base pour les E/S VSAM. Chaque CI contient :
- Les enregistrements de donnees
- L'espace libre
- Le CIDF (CI Definition Field) - informations de controle
</details>

---

### Question 19
Que contient la VTOC d'un volume DASD ?

- [ ] a) Les donnees des fichiers
- [ ] b) La table des matieres du volume (liste des datasets)
- [ ] c) Le systeme d'exploitation
- [ ] d) Les programmes utilisateur

<details>
<summary>Reponse</summary>

**b) La table des matieres du volume (liste des datasets)**

La VTOC (Volume Table Of Contents) contient la liste de tous les datasets sur un volume DASD, avec leurs attributs (nom, emplacement, taille, RECFM, LRECL, etc.).
</details>

---

### Question 20
Qu'est-ce qu'un catalogue sous z/OS ?

<details>
<summary>Reponse</summary>

**Le catalogue z/OS :**

Un **catalogue** est une structure qui reference les datasets et leur emplacement. Il permet de retrouver un dataset sans connaitre son volume.

**Hierarchie :**
- **Master Catalog** : Catalogue principal du systeme, contient les alias vers les User Catalogs
- **User Catalog** : Catalogues utilisateurs par domaine (PROD, TEST, USER...)

**Avantages :**
- Localisation automatique des datasets
- Pas besoin de specifier le volume
- Gestion centralisee
</details>

---

### Question 21
Quelle est la difference entre l'allocation TRACKS et CYLINDERS ?

<details>
<summary>Reponse</summary>

| Unite | Description | Usage |
|-------|-------------|-------|
| **TRACKS** | Pistes individuelles | Petits fichiers |
| **CYLINDERS** | Ensemble de pistes (cylindre complet) | Gros fichiers, meilleure performance |
| **BLOCKS** | Nombre de blocs | Rarement utilise |

Un cylindre comprend toutes les pistes a la meme position sur tous les plateaux du disque (acces plus rapide car pas de deplacement de tete).
</details>

---

### Question 22
Qu'est-ce que le TLB et a quoi sert-il ?

<details>
<summary>Reponse</summary>

**TLB (Translation Lookaside Buffer) :**

Le TLB est un **cache materiel** qui stocke les traductions d'adresses recentes (virtuelle → reelle).

**Fonctionnement :**
1. Le processeur cherche d'abord dans le TLB
2. Si trouve (TLB Hit) → traduction immediate
3. Si non trouve (TLB Miss) → parcours des tables de translation (plus lent)

Le TLB accelere considerablement la traduction d'adresses car la plupart des acces concernent des pages recemment utilisees.
</details>

---

## Resume

| Concept | Definition |
|---------|------------|
| **Memoire CPC** | Memoire vive (RAM) - rapide, volatile |
| **Memoire auxiliaire** | Stockage disque pour pagination |
| **Page** | Unite de memoire virtuelle (4 Ko) |
| **Frame** | Unite de memoire physique (4 Ko) |
| **Slot** | Emplacement de page sur disque |
| **DAT** | Dynamic Address Translation |
| **TLB** | Cache de traduction d'adresses |
| **Page Fault** | Acces a page non residente |
| **TCB** | Task Control Block (tache utilisateur) |
| **SRB** | Service Request Block (routine systeme) |
| **Initiator** | Executeur de jobs batch |
| **VTOC** | Table des matieres d'un volume |

---
*Formation z/OS - M2i Formation*
