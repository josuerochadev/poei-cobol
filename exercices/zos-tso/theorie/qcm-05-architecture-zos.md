# QCM 05 - Architecture z/OS

## Chapitre V - SYSPLEX, SMS et RACF

---

## Partie 1 : SYSPLEX

### Question 1
Que signifie SYSPLEX ?

- [ ] a) System Parallel Executive
- [ ] b) System Complex
- [ ] c) Symmetric Processor Complex
- [ ] d) System Plex Architecture

<details>
<summary>Reponse</summary>

**b) System Complex**

Un SYSPLEX est un cluster de systemes z/OS qui travaillent ensemble comme une seule entite logique, permettant haute disponibilite et scalabilite.
</details>

---

### Question 2
Combien de systemes z/OS peut-on connecter au maximum dans un SYSPLEX ?

- [ ] a) 8
- [ ] b) 16
- [ ] c) 32
- [ ] d) 64

<details>
<summary>Reponse</summary>

**c) 32**

Un SYSPLEX peut connecter jusqu'a 32 systemes z/OS (images LPAR) qui fonctionnent comme un systeme unique.
</details>

---

### Question 3
Quelle est la difference principale entre un Basic SYSPLEX et un Parallel SYSPLEX ?

<details>
<summary>Reponse</summary>

| Caracteristique | Basic SYSPLEX | Parallel SYSPLEX |
|-----------------|---------------|------------------|
| **Coupling Facility** | Non | Oui |
| **Partage de donnees** | Limite | Complet (Data Sharing) |
| **Cache partage** | Non | Oui |
| **Synchronisation** | Par canaux CTC | Par Coupling Facility |
| **Haute disponibilite** | Limitee | Optimale |

Le Parallel SYSPLEX, grace a la Coupling Facility, permet un partage reel des donnees et une haute disponibilite maximale.
</details>

---

### Question 4
Qu'est-ce que la Coupling Facility (CF) ?

<details>
<summary>Reponse</summary>

**La Coupling Facility :**

La CF est le composant central du Parallel SYSPLEX qui permet le partage de ressources entre systemes.

**Structures principales :**
- **Lock Structures** : Gestion des verrous inter-systemes
- **Cache Structures** : Cache partage (DB2, VSAM)
- **List Structures** : Files d'attente partagees

**Avantages :**
- Acces concurrent aux memes donnees
- Coherence automatique des caches
- Failover transparent
- Single System Image (SSI)
</details>

---

### Question 5
Que signifie XCF ?

- [ ] a) External Coupling Facility
- [ ] b) Cross-system Coupling Facility
- [ ] c) Extended Control Function
- [ ] d) eXtended Communication Framework

<details>
<summary>Reponse</summary>

**b) Cross-system Coupling Facility**

XCF permet la communication entre les systemes d'un SYSPLEX. Il fournit les services de base pour la coordination inter-systemes (messages, signalisation, groupe de travail).
</details>

---

### Question 6
Quel composant SYSPLEX gere la repartition de charge entre les systemes ?

- [ ] a) ARM
- [ ] b) GRS
- [ ] c) WLM
- [ ] d) SFM

<details>
<summary>Reponse</summary>

**c) WLM**

**WLM (Workload Manager)** gere la repartition de charge et l'allocation des ressources selon des objectifs de performance definis.

Autres composants :
- **ARM** = Automatic Restart Manager (redemarrage automatique)
- **GRS** = Global Resource Serialization (verrous)
- **SFM** = Sysplex Failure Management (gestion pannes)
</details>

---

### Question 7
Qu'est-ce que le DB2 Data Sharing ?

<details>
<summary>Reponse</summary>

**DB2 Data Sharing :**

Le Data Sharing permet a plusieurs membres DB2 (sur differents systemes du SYSPLEX) d'acceder simultanement aux memes bases de donnees.

**Fonctionnement :**
- Chaque systeme execute un membre DB2
- Tous les membres accedent aux memes donnees
- La Coupling Facility assure la coherence (Group Buffer Pool)
- En cas de panne, les autres membres prennent le relais

**Avantages :**
- Haute disponibilite (failover automatique)
- Scalabilite (ajout de membres)
- Equilibrage de charge
</details>

---

---

## Partie 2 : SMS

### Question 8
Que signifie SMS ?

- [ ] a) System Management Services
- [ ] b) Storage Management Subsystem
- [ ] c) Shared Memory System
- [ ] d) Sequential Management System

<details>
<summary>Reponse</summary>

**b) Storage Management Subsystem**

SMS est le composant de DFSMS qui automatise la gestion du stockage sous z/OS (allocation, placement, sauvegarde, migration).
</details>

---

### Question 9
Quel est l'avantage principal de SMS par rapport a la gestion manuelle du stockage ?

<details>
<summary>Reponse</summary>

**Avantages de SMS :**

| Aspect | Sans SMS | Avec SMS |
|--------|----------|----------|
| **Volume** | Choix manuel (VOL=xxx) | Selection automatique |
| **Espace** | Gestion manuelle | Equilibrage automatique |
| **Attributs** | Specifies par l'utilisateur | Standards appliques par classes |
| **Sauvegarde** | Manuelle | Automatisee par politique |
| **Migration** | Manuelle | HSM automatique |

SMS simplifie l'administration et garantit la coherence des attributs.
</details>

---

### Question 10
Quelles sont les quatre classes SMS ?

<details>
<summary>Reponse</summary>

**Les quatre classes SMS :**

| Classe | Fonction | Exemples d'attributs |
|--------|----------|---------------------|
| **Data Class** | Attributs physiques | RECFM, LRECL, BLKSIZE, SPACE |
| **Storage Class** | Performance et disponibilite | Temps de reponse, type de disque |
| **Management Class** | Cycle de vie | Sauvegarde, migration, retention |
| **Storage Group** | Groupe de volumes | Pool de disques, VIO, bandes |
</details>

---

### Question 11
Que definit la Data Class ?

- [ ] a) Les politiques de sauvegarde
- [ ] b) Les attributs physiques du dataset (RECFM, LRECL...)
- [ ] c) Les exigences de performance
- [ ] d) Le groupe de volumes

<details>
<summary>Reponse</summary>

**b) Les attributs physiques du dataset (RECFM, LRECL...)**

La Data Class definit :
- RECFM (format d'enregistrement)
- LRECL (longueur d'enregistrement)
- BLKSIZE (taille de bloc)
- SPACE (allocation primaire/secondaire)
- DSORG (organisation)
</details>

---

### Question 12
Que definit la Management Class ?

- [ ] a) Les attributs physiques
- [ ] b) Le groupe de volumes
- [ ] c) Les politiques de cycle de vie (backup, migration, retention)
- [ ] d) La performance requise

<details>
<summary>Reponse</summary>

**c) Les politiques de cycle de vie (backup, migration, retention)**

La Management Class definit :
- Frequence de sauvegarde
- Duree de retention
- Migration automatique (HSM)
- Expiration et suppression
- Compression
</details>

---

### Question 13
Que sont les ACS Routines ?

<details>
<summary>Reponse</summary>

**ACS (Automatic Class Selection) Routines :**

Les ACS Routines sont des programmes qui determinent automatiquement les classes SMS a appliquer lors de la creation d'un dataset.

**Criteres de selection :**
- Nom du dataset (HLQ, qualificateurs)
- Userid du createur
- Job name
- Type de dataset

**Exemple de logique :**
```
Si le dataset commence par PROD.**
    → Storage Class = SCPROD (haute performance)
    → Management Class = MCPROD (backup quotidien)
Sinon
    → Storage Class = SCSTD (standard)
```
</details>

---

### Question 14
Qu'est-ce qu'un Storage Group de type VIO ?

- [ ] a) Un groupe de volumes bande
- [ ] b) Un stockage virtuel en memoire (pas d'I/O disque)
- [ ] c) Un groupe de volumes haute performance
- [ ] d) Un stockage de sauvegarde

<details>
<summary>Reponse</summary>

**b) Un stockage virtuel en memoire (pas d'I/O disque)**

VIO (Virtual I/O) stocke les datasets temporaires en memoire virtuelle. Les donnees ne sont jamais ecrites sur disque physique, ce qui offre une performance maximale pour les fichiers de travail temporaires.
</details>

---

---

## Partie 3 : RACF

### Question 15
Que signifie RACF ?

- [ ] a) Resource Access Configuration Facility
- [ ] b) Resource Access Control Facility
- [ ] c) Remote Access Control Function
- [ ] d) Restricted Access Control Facility

<details>
<summary>Reponse</summary>

**b) Resource Access Control Facility**

RACF est le systeme de securite principal de z/OS. Il gere l'authentification des utilisateurs et le controle d'acces aux ressources.
</details>

---

### Question 16
Quelles sont les trois fonctions principales de RACF ?

<details>
<summary>Reponse</summary>

**Les trois fonctions principales de RACF :**

1. **Identification et Authentification**
   - Verification de l'identite (userid)
   - Validation des credentials (mot de passe, certificat, MFA)

2. **Autorisation (Controle d'acces)**
   - Verification des droits d'acces aux ressources
   - Application des profils de securite

3. **Audit et Journalisation**
   - Enregistrement des tentatives d'acces
   - Generation de rapports de securite (SMF records)
</details>

---

### Question 17
Quels sont les niveaux d'acces RACF, du moins au plus permissif ?

<details>
<summary>Reponse</summary>

**Niveaux d'acces RACF (du moins au plus permissif) :**

| Niveau | Description |
|--------|-------------|
| **NONE** | Aucun acces |
| **EXECUTE** | Execution seulement (programmes) |
| **READ** | Lecture seule |
| **UPDATE** | Lecture et ecriture |
| **CONTROL** | Controle (VSAM, modification attributs) |
| **ALTER** | Tous droits (y compris suppression) |
</details>

---

### Question 18
Que signifie UACC dans un profil RACF ?

- [ ] a) User Access Control Configuration
- [ ] b) Universal Access (acces par defaut)
- [ ] c) Unauthorized Access Check
- [ ] d) User Authentication Code

<details>
<summary>Reponse</summary>

**b) Universal Access (acces par defaut)**

UACC (Universal Access) definit le niveau d'acces accorde par defaut a tous les utilisateurs qui ne sont pas explicitement listes dans le profil.

Exemple : `UACC(NONE)` signifie que par defaut, personne n'a acces.
</details>

---

### Question 19
Quelle commande RACF cree un nouvel utilisateur ?

- [ ] a) CREATEUSER
- [ ] b) NEWUSER
- [ ] c) ADDUSER
- [ ] d) DEFUSER

<details>
<summary>Reponse</summary>

**c) ADDUSER**

Syntaxe : `ADDUSER userid NAME('Nom Complet') DFLTGRP(groupe) PASSWORD(pwd) OWNER(owner)`

Exemple :
```
ADDUSER USER01 NAME('DUPONT JEAN') DFLTGRP(DEVGROUP) PASSWORD(INITIAL) OWNER(ADMIN)
```
</details>

---

### Question 20
Quelle commande RACF protege un dataset ?

- [ ] a) PROTECT
- [ ] b) SECURE
- [ ] c) ADDSD
- [ ] d) DEFDS

<details>
<summary>Reponse</summary>

**c) ADDSD**

ADDSD (Add Security Dataset) cree un profil de protection pour un dataset.

Syntaxe : `ADDSD 'dsname' UACC(niveau) OWNER(owner)`

Exemple :
```
ADDSD 'PROD.PAYROLL.**' UACC(NONE) OWNER(PAYROLL)
```
</details>

---

### Question 21
Quelle commande RACF autorise un utilisateur a acceder a un dataset ?

- [ ] a) GRANT
- [ ] b) ALLOW
- [ ] c) PERMIT
- [ ] d) AUTHORIZE

<details>
<summary>Reponse</summary>

**c) PERMIT**

Syntaxe : `PERMIT 'dsname' ID(userid) ACCESS(niveau)`

Exemples :
```
PERMIT 'PROD.PAYROLL.**' ID(USER01) ACCESS(READ)
PERMIT 'PROD.PAYROLL.**' ID(PAYGRP) ACCESS(UPDATE)
```
</details>

---

### Question 22
Quelle commande affiche les informations de protection d'un dataset ?

- [ ] a) SHOWSD
- [ ] b) LISTDSD
- [ ] c) DISPLAYSD
- [ ] d) INFOSD

<details>
<summary>Reponse</summary>

**b) LISTDSD**

Syntaxe : `LISTDSD DATASET('dsname') [AUTH] [ALL]`

- **AUTH** : Affiche la liste d'acces
- **ALL** : Affiche toutes les informations
</details>

---

### Question 23
Quel message d'erreur indique que l'utilisateur n'est pas defini dans RACF ?

- [ ] a) ICH70001I
- [ ] b) ICH408I
- [ ] c) ICH409I
- [ ] d) IRR012I

<details>
<summary>Reponse</summary>

**b) ICH408I**

Messages d'erreur RACF courants :
- **ICH408I** : USER NOT DEFINED (utilisateur inconnu)
- **ICH409I** : PASSWORD NOT AUTHORIZED (mauvais mot de passe)
- **ICH70001I** : NOT AUTHORIZED TO DATASET (acces refuse)
- **IRR012I** : VERIFICATION FAILED (echec authentification)
</details>

---

### Question 24
Qu'est-ce qu'un groupe RACF ?

<details>
<summary>Reponse</summary>

**Groupe RACF :**

Un groupe est un ensemble d'utilisateurs qui partagent des droits d'acces communs.

**Structure hierarchique :**
```
SYS1 (Master)
├── DEVGROUP (Developpeurs)
│   ├── USER01
│   └── USER02
├── PRODGROUP (Production)
│   └── USER03
└── ADMGROUP (Administrateurs)
    └── ADMIN01
```

**Avantages :**
- Simplification de l'administration
- Attribution de droits par groupe
- Heritage des permissions
</details>

---

### Question 25
Ecrivez les commandes RACF pour :
1. Proteger le dataset PROD.SALES.DATA avec UACC=NONE
2. Autoriser USER01 en lecture
3. Autoriser le groupe SALESGRP en mise a jour

<details>
<summary>Reponse</summary>

```
1. Protection du dataset :
   ADDSD 'PROD.SALES.DATA' UACC(NONE) OWNER(ADMIN)

2. Autorisation en lecture pour USER01 :
   PERMIT 'PROD.SALES.DATA' ID(USER01) ACCESS(READ)

3. Autorisation en mise a jour pour SALESGRP :
   PERMIT 'PROD.SALES.DATA' ID(SALESGRP) ACCESS(UPDATE)
```

Pour verifier : `LISTDSD DATASET('PROD.SALES.DATA') AUTH`
</details>

---

## Resume

| Concept | Definition |
|---------|------------|
| **SYSPLEX** | Cluster de systemes z/OS |
| **Parallel SYSPLEX** | SYSPLEX avec Coupling Facility |
| **Coupling Facility** | Partage cache, verrous, files entre systemes |
| **XCF** | Communication inter-systemes |
| **WLM** | Workload Manager (repartition charge) |
| **SMS** | Storage Management Subsystem |
| **Data Class** | Attributs physiques (RECFM, LRECL...) |
| **Storage Class** | Performance et disponibilite |
| **Management Class** | Cycle de vie (backup, migration) |
| **ACS Routine** | Selection automatique des classes |
| **RACF** | Resource Access Control Facility |
| **UACC** | Universal Access (acces par defaut) |
| **ADDUSER** | Creer un utilisateur |
| **ADDSD** | Proteger un dataset |
| **PERMIT** | Autoriser l'acces |
| **LISTDSD** | Afficher la protection |

---
*Formation z/OS - M2i Formation*
