# QCM 03 - TSO (Time Sharing Option)

## Chapitre III - Commandes TSO et Gestion des Donnees

---

### Question 1
Que signifie TSO ?

- [ ] a) Terminal Service Option
- [ ] b) Time Sharing Option
- [ ] c) Transaction System Operation
- [ ] d) Terminal Session Online

<details>
<summary>Reponse</summary>

**b) Time Sharing Option**

TSO permet a plusieurs utilisateurs de partager les ressources du mainframe de maniere interactive. Chaque utilisateur dispose d'une session avec son propre espace d'adressage.
</details>

---

### Question 2
Quels sont les trois modes de traitement principaux sous z/OS ?

<details>
<summary>Reponse</summary>

**Les trois modes de traitement z/OS :**

| Mode | Description | Exemple |
|------|-------------|---------|
| **BATCH** | Travaux planifies, non-interactifs | Jobs JCL, traitements de nuit |
| **INTERACTIF (TSO)** | Sessions utilisateur en temps reel | Developpement, edition de code |
| **TRANSACTIONNEL (CICS)** | Transactions en ligne, temps de reponse court | Applications bancaires, reservations |
</details>

---

### Question 3
Quelle commande permet d'ouvrir une session TSO ?

- [ ] a) LOGIN
- [ ] b) LOGON
- [ ] c) CONNECT
- [ ] d) START

<details>
<summary>Reponse</summary>

**b) LOGON**

La commande LOGON permet d'ouvrir une session TSO. Syntaxe : `LOGON userid [/password]`

Parametres optionnels : SIZE (memoire), ACCT (comptabilisation), PROC (procedure de logon), RECONNECT (reprise de session).
</details>

---

### Question 4
Quelle est la longueur maximale d'un UserID TSO ?

- [ ] a) 4 caracteres
- [ ] b) 7 caracteres
- [ ] c) 8 caracteres
- [ ] d) 12 caracteres

<details>
<summary>Reponse</summary>

**b) 7 caracteres**

L'UserID TSO peut avoir jusqu'a 7 caracteres alphanumeriques. Le premier caractere doit etre alphabetique ou un caractere national (@, #, $).
</details>

---

### Question 5
Quelle commande affiche l'aide sur une commande TSO ?

- [ ] a) ASSIST
- [ ] b) INFO
- [ ] c) HELP
- [ ] d) MAN

<details>
<summary>Reponse</summary>

**c) HELP**

La commande HELP affiche l'aide sur les commandes TSO.

Exemples :
- `HELP ALLOCATE` - Aide sur la commande ALLOCATE
- `HELP ALLOCATE SYNTAX` - Affiche uniquement la syntaxe
- `HELP ALLOCATE OPERANDS(DSNAME)` - Aide sur un parametre specifique
</details>

---

### Question 6
Quelle commande TSO permet de creer un nouveau dataset ?

- [ ] a) CREATE
- [ ] b) NEW
- [ ] c) ALLOCATE
- [ ] d) DEFINE

<details>
<summary>Reponse</summary>

**c) ALLOCATE**

La commande ALLOCATE cree et alloue des datasets. Elle peut aussi associer un fichier logique (DDNAME) a un fichier physique.

Exemple : `ALLOCATE DATASET(USER01.TEST.DATA) NEW SPACE(5,2) TRACKS RECFM(FB) LRECL(80)`
</details>

---

### Question 7
Dans la commande ALLOCATE, que signifie le parametre SPACE(5,2) TRACKS ?

<details>
<summary>Reponse</summary>

**Signification de SPACE(5,2) TRACKS :**

- **5** = Allocation primaire (5 pistes)
- **2** = Allocation secondaire (2 pistes par extension)
- **TRACKS** = Unite d'allocation (pistes)

Lorsque l'espace primaire est plein, z/OS alloue automatiquement une extension secondaire de 2 pistes (jusqu'a 15 extensions maximum).
</details>

---

### Question 8
Quel parametre de ALLOCATE permet de creer un PDS ?

- [ ] a) DSORG(PO)
- [ ] b) DSORG(PS)
- [ ] c) TYPE(PDS)
- [ ] d) LIBRARY

<details>
<summary>Reponse</summary>

**a) DSORG(PO)**

- **DSORG(PO)** : Partitioned Organization (PDS/PDSE)
- **DSORG(PS)** : Physical Sequential (fichier sequentiel)
- **DSORG(DA)** : Direct Access

Pour un PDS, il faut aussi specifier DIR(n) pour le nombre de blocs directory.
</details>

---

### Question 9
Quelle commande supprime un dataset ?

- [ ] a) REMOVE
- [ ] b) ERASE
- [ ] c) DELETE
- [ ] d) DROP

<details>
<summary>Reponse</summary>

**c) DELETE**

La commande DELETE supprime des datasets.

Exemples :
- `DELETE USER01.TEST.DATA` - Supprime un dataset non-VSAM
- `DELETE USER01.KSDS.FILE CLUSTER` - Supprime un cluster VSAM
</details>

---

### Question 10
Quelle commande affiche les informations sur un dataset ?

- [ ] a) DISPLAY
- [ ] b) LISTDS
- [ ] c) SHOW
- [ ] d) INFO

<details>
<summary>Reponse</summary>

**b) LISTDS**

La commande LISTDS affiche les caracteristiques d'un dataset.

Options :
- `LISTDS 'dsname' STATUS` - Informations de base
- `LISTDS 'dsname' MEMBERS` - Liste des membres (PDS)
- `LISTDS 'dsname' HISTORY` - Historique
- `LISTDS 'dsname' ALL` - Toutes les informations
</details>

---

### Question 11
Quelle est la difference entre LISTDS et LISTCAT ?

<details>
<summary>Reponse</summary>

**Difference LISTDS vs LISTCAT :**

| Commande | Usage | Type de fichiers |
|----------|-------|------------------|
| **LISTDS** | Informations sur un dataset specifique | Non-VSAM et VSAM |
| **LISTCAT** | Liste les entrees du catalogue | VSAM principalement |

Exemples :
- `LISTDS 'USER01.COBOL.SOURCE' MEMBERS` - Liste membres d'un PDS
- `LISTCAT ENTRIES('USER01.KSDS') ALL` - Details complets d'un VSAM
- `LISTCAT LEVEL('USER01')` - Liste tous les datasets commencant par USER01
</details>

---

### Question 12
Quelle commande renomme un dataset ?

- [ ] a) MOVE
- [ ] b) RENAME
- [ ] c) CHANGE
- [ ] d) ALTER

<details>
<summary>Reponse</summary>

**b) RENAME**

La commande RENAME change le nom d'un dataset.

Syntaxe : `RENAME 'ancien.nom' 'nouveau.nom'`

Note : On ne peut pas changer le HLQ (High Level Qualifier) sauf si on a les droits RACF adequats.
</details>

---

### Question 13
Quelle commande soumet un job JCL pour execution ?

- [ ] a) RUN
- [ ] b) EXEC
- [ ] c) SUBMIT
- [ ] d) START

<details>
<summary>Reponse</summary>

**c) SUBMIT**

La commande SUBMIT soumet un job JCL au sous-systeme JES pour execution.

Exemples :
- `SUBMIT 'USER01.JCL(MYJOB)'` - Soumet un membre JCL
- `SUBMIT *` - Soumet le dataset en cours d'edition (en ISPF)
</details>

---

### Question 14
Quelle commande affiche le statut des jobs soumis ?

- [ ] a) JOBS
- [ ] b) STATUS
- [ ] c) LIST
- [ ] d) QUEUE

<details>
<summary>Reponse</summary>

**b) STATUS**

La commande STATUS affiche l'etat des jobs.

Exemples :
- `STATUS` - Statut des jobs de l'utilisateur courant
- `STATUS jobname` - Statut d'un job specifique
</details>

---

### Question 15
Quelle commande annule un job en cours d'execution ?

- [ ] a) STOP
- [ ] b) ABORT
- [ ] c) CANCEL
- [ ] d) KILL

<details>
<summary>Reponse</summary>

**c) CANCEL**

La commande CANCEL arrete un job.

Syntaxe : `CANCEL jobname`

Options :
- `CANCEL jobname PURGE` - Annule et supprime les sorties
</details>

---

### Question 16
Quelle commande permet d'envoyer un message a un autre utilisateur TSO ?

- [ ] a) MESSAGE
- [ ] b) WRITE
- [ ] c) SEND
- [ ] d) NOTIFY

<details>
<summary>Reponse</summary>

**c) SEND**

La commande SEND envoie un message a d'autres utilisateurs.

Exemples :
- `SEND 'Message texte' USER(USER02)` - Envoie a un utilisateur
- `SEND 'Message' USER(USER02 USER03)` - Envoie a plusieurs utilisateurs
- `SEND 'Message urgent' OPERATOR` - Envoie a la console operateur
</details>

---

### Question 17
Quelle commande affiche les fichiers alloues dans la session courante ?

- [ ] a) LISTALLOC
- [ ] b) LISTALC
- [ ] c) SHOWALLOC
- [ ] d) ALLOCATED

<details>
<summary>Reponse</summary>

**b) LISTALC**

La commande LISTALC liste les fichiers alloues dans la session.

Options :
- `LISTALC STATUS` - Avec statut (OLD, NEW, SHR)
- `LISTALC SYSNAMES` - Avec les DDNAMEs systeme
- `LISTALC MEMBERS` - Avec les membres PDS ouverts
</details>

---

### Question 18
Que fait la commande PROFILE ?

<details>
<summary>Reponse</summary>

**La commande PROFILE :**

PROFILE configure les caracteristiques de la session TSO de l'utilisateur.

Options principales :
- `PROFILE PROMPT/NOPROMPT` - Active/desactive l'assistance de saisie
- `PROFILE PREFIX('prefix')` - Definit le prefixe par defaut des noms de fichiers
- `PROFILE MSGID/NOMSGID` - Affiche/masque les codes de messages
- `PROFILE INTERCOM/NOINTERCOM` - Active/desactive la reception de messages
- `PROFILE LIST` - Affiche le profil actuel

Exemple : `PROFILE PREFIX('USER01') PROMPT MSGID`
</details>

---

### Question 19
Quel parametre de ALLOCATE permet de creer un fichier base sur un modele existant ?

- [ ] a) MODEL
- [ ] b) LIKE
- [ ] c) COPY
- [ ] d) TEMPLATE

<details>
<summary>Reponse</summary>

**b) LIKE**

Le parametre LIKE copie les attributs d'un dataset existant.

Exemple : `ALLOCATE DA(USER01.NEW.DATA) LIKE('USER01.MODEL.DATA') SPACE(10,5) TRACKS`

Les attributs copies sont : RECFM, LRECL, BLKSIZE, DSORG, etc. L'espace peut etre redefini.
</details>

---

### Question 20
Ecrivez la commande ALLOCATE pour creer un PDS nomme USER01.COBOL.SOURCE avec :
- Espace primaire : 10 pistes, secondaire : 5 pistes
- 5 blocs directory
- RECFM=FB, LRECL=80

<details>
<summary>Reponse</summary>

```
ALLOCATE DATASET('USER01.COBOL.SOURCE') NEW -
  DSORG(PO) -
  SPACE(10,5) TRACKS -
  DIR(5) -
  RECFM(FB) -
  LRECL(80) -
  CATALOG
```

Notes :
- Le tiret (-) permet de continuer sur plusieurs lignes
- CATALOG inscrit le dataset dans le catalogue
- BLKSIZE est calcule automatiquement par le systeme si omis
</details>

---

### Question 21
Quelles sont les trois fonctions principales de TSO ?

<details>
<summary>Reponse</summary>

**Trois fonctions principales de TSO :**

1. **Gestion des fichiers (Data Sets)**
   - Creation, suppression, renommage
   - Allocation, catalogage
   - Consultation des attributs

2. **Developpement de programmes**
   - Edition de code source
   - Compilation, link-edit
   - Execution de programmes

3. **Gestion des travaux batch (Jobs)**
   - Soumission de jobs JCL
   - Surveillance de l'execution
   - Consultation des sorties
</details>

---

### Question 22
Quelle commande permet de quitter TSO ?

- [ ] a) QUIT
- [ ] b) EXIT
- [ ] c) LOGOFF
- [ ] d) LOGOUT

<details>
<summary>Reponse</summary>

**c) LOGOFF**

La commande LOGOFF termine la session TSO.

En ISPF, on peut aussi utiliser `=X` pour sortir d'ISPF puis taper LOGOFF.
</details>

---

## Resume

| Commande | Fonction |
|----------|----------|
| **LOGON/LOGOFF** | Ouvrir/fermer session TSO |
| **HELP** | Aide sur les commandes |
| **PROFILE** | Configuration session |
| **ALLOCATE** | Creer/allouer un dataset |
| **DELETE** | Supprimer un dataset |
| **RENAME** | Renommer un dataset |
| **LISTDS** | Informations dataset |
| **LISTCAT** | Liste catalogue (VSAM) |
| **LISTALC** | Fichiers alloues en session |
| **SUBMIT** | Soumettre un job |
| **STATUS** | Statut des jobs |
| **CANCEL** | Annuler un job |
| **SEND** | Envoyer un message |

---
*Formation z/OS - M2i Formation*
