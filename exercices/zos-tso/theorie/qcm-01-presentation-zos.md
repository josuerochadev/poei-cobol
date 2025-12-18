# QCM 01 - Presentation Generale de z/OS

## Chapitre I - Historique, Architecture et Terminologie

---

### Question 1
Quelle est la signification de z/OS ?

- [ ] a) Zero Operating System
- [ ] b) z Architecture Operating System
- [ ] c) Zoned Operating System
- [ ] d) Zenith Operating System

<details>
<summary>Reponse</summary>

**b) z Architecture Operating System**

z/OS est le systeme d'exploitation IBM pour les mainframes bases sur l'architecture z (zSeries, System z, IBM Z). Le "z" fait reference a l'architecture z/Architecture introduite en 2000.
</details>

---

### Question 2
En quelle annee IBM a-t-il lance le System/360, marquant le debut de l'ere moderne des mainframes ?

- [ ] a) 1954
- [ ] b) 1964
- [ ] c) 1974
- [ ] d) 1984

<details>
<summary>Reponse</summary>

**b) 1964**

Le System/360 a ete lance en avril 1964. Il a revolutionne l'informatique en introduisant le concept de famille de machines compatibles entre elles (compatibilite ascendante).
</details>

---

### Question 3
Quel pourcentage approximatif des donnees mondiales est heberge sur des mainframes (selon les statistiques IBM) ?

- [ ] a) 30%
- [ ] b) 50%
- [ ] c) 70%
- [ ] d) 90%

<details>
<summary>Reponse</summary>

**c) 70%**

Environ 70% des donnees mondiales sont hebergees sur des mainframes, notamment dans les secteurs bancaires, assurances, et grandes entreprises. Cela inclut les transactions bancaires, les reservations aeriennes, et les donnees gouvernementales.
</details>

---

### Question 4
Quel est le taux de disponibilite typique d'un mainframe en configuration Parallel SYSPLEX ?

- [ ] a) 99,9% (environ 8h d'arret par an)
- [ ] b) 99,99% (environ 53 min d'arret par an)
- [ ] c) 99,999% (environ 5 min d'arret par an)
- [ ] d) 100%

<details>
<summary>Reponse</summary>

**c) 99,999% (environ 5 min d'arret par an)**

Ce niveau est souvent appele "five nines" et represente la haute disponibilite typique des configurations Parallel SYSPLEX. C'est un avantage majeur des mainframes pour les applications critiques.
</details>

---

### Question 5
Comment s'appelle un fichier dans l'environnement z/OS ?

- [ ] a) File
- [ ] b) Data Set
- [ ] c) Document
- [ ] d) Record

<details>
<summary>Reponse</summary>

**b) Data Set**

Dans l'environnement z/OS, un fichier est appele **Data Set** (ou dataset). Ce terme est specifique au vocabulaire IBM mainframe.
</details>

---

### Question 6
Que signifie l'acronyme VSAM ?

- [ ] a) Virtual Storage Access Mode
- [ ] b) Variable Sequential Access Method
- [ ] c) Virtual Storage Access Method
- [ ] d) Volume Storage Access Manager

<details>
<summary>Reponse</summary>

**c) Virtual Storage Access Method**

VSAM est la methode d'acces aux fichiers la plus utilisee sous z/OS. Elle permet d'organiser les donnees de maniere sequentielle (ESDS), indexee (KSDS), ou relative (RRDS).
</details>

---

### Question 7
Qu'est-ce qu'un PDS (Partitioned Data Set) ?

- [ ] a) Un fichier sequentiel
- [ ] b) Une bibliotheque contenant des membres
- [ ] c) Un fichier VSAM
- [ ] d) Un volume disque

<details>
<summary>Reponse</summary>

**b) Une bibliotheque contenant des membres**

Un PDS est une bibliotheque partitionnee qui contient plusieurs membres (fichiers individuels). C'est comme un repertoire contenant des fichiers. Exemples : bibliotheque de sources COBOL, bibliotheque de JCL.
</details>

---

### Question 8
Que signifie l'acronyme JCL ?

- [ ] a) Job Command Language
- [ ] b) Job Control Language
- [ ] c) Just Command Line
- [ ] d) Java Control Language

<details>
<summary>Reponse</summary>

**b) Job Control Language**

JCL est le langage de controle des travaux batch sous z/OS. Il permet de definir les programmes a executer, les fichiers a utiliser, et les ressources necessaires.
</details>

---

### Question 9
Qu'est-ce que le SPOOL sous z/OS ?

- [ ] a) Une zone de memoire vive
- [ ] b) Une zone de stockage temporaire pour les sorties JES
- [ ] c) Un type de disque
- [ ] d) Un protocole reseau

<details>
<summary>Reponse</summary>

**b) Une zone de stockage temporaire pour les sorties JES**

Le SPOOL est une zone de stockage temporaire geree par JES (Job Entry Subsystem) qui contient les jobs en attente, en cours d'execution, et leurs sorties (SYSOUT).
</details>

---

### Question 10
Que signifie l'acronyme DASD ?

- [ ] a) Direct Access Storage Device
- [ ] b) Data Access System Drive
- [ ] c) Disk Array Storage Device
- [ ] d) Dynamic Access Storage Disk

<details>
<summary>Reponse</summary>

**a) Direct Access Storage Device**

DASD designe les disques durs dans l'environnement mainframe. Contrairement aux bandes magnetiques (acces sequentiel), les DASD permettent un acces direct aux donnees.
</details>

---

### Question 11
Quelle est la signification d'IPL ?

- [ ] a) Initial Processing Load
- [ ] b) Initial Program Load
- [ ] c) Internal Program Launcher
- [ ] d) Integrated Power Level

<details>
<summary>Reponse</summary>

**b) Initial Program Load**

IPL est l'equivalent du "boot" ou demarrage du systeme. C'est le processus de chargement initial de z/OS en memoire depuis le volume systeme (SYSRES).
</details>

---

### Question 12
Que signifie ABEND ?

- [ ] a) Abnormal Beginning
- [ ] b) Abnormal End
- [ ] c) Application Backend
- [ ] d) Automatic Benchmark

<details>
<summary>Reponse</summary>

**b) Abnormal End**

ABEND signifie terminaison anormale d'un programme. Les codes ABEND (ex: S0C7, S0C4, S322) indiquent la nature de l'erreur.
</details>

---

### Question 13
Quel code retour indique une execution reussie sans erreur ?

- [ ] a) 0
- [ ] b) 4
- [ ] c) 8
- [ ] d) -1

<details>
<summary>Reponse</summary>

**a) 0**

Un code retour 0 indique un succes total. Les codes 4, 8, 12, 16 indiquent des avertissements ou erreurs de gravite croissante.
</details>

---

### Question 14
Que signifie l'acronyme LPAR ?

- [ ] a) Logical Partition
- [ ] b) Local Partition Area Region
- [ ] c) Linear Processing Array
- [ ] d) Logical Processing Area

<details>
<summary>Reponse</summary>

**a) Logical Partition**

Une LPAR est une partition logique qui permet de diviser un mainframe physique en plusieurs systemes virtuels independants, chacun pouvant executer son propre systeme d'exploitation.
</details>

---

### Question 15
Quelle est la longueur maximale du nom d'un dataset ?

- [ ] a) 8 caracteres
- [ ] b) 22 caracteres
- [ ] c) 44 caracteres
- [ ] d) 64 caracteres

<details>
<summary>Reponse</summary>

**c) 44 caracteres**

Un nom de dataset peut avoir jusqu'a 44 caracteres, compose de qualificateurs (1-8 caracteres chacun) separes par des points. Exemple : `USER01.COBOL.SOURCE.PROGRAMS`
</details>

---

### Question 16
Quels caracteres sont autorises dans les noms de datasets ?

- [ ] a) A-Z, 0-9 uniquement
- [ ] b) A-Z, 0-9, @, #, $
- [ ] c) Tous les caracteres ASCII
- [ ] d) A-Z, 0-9, tirets

<details>
<summary>Reponse</summary>

**b) A-Z, 0-9, @, #, $**

Les noms de datasets peuvent contenir des lettres (A-Z), des chiffres (0-9), et les caracteres speciaux @, #, $. Le premier caractere de chaque qualificateur doit etre alphabetique ou un caractere special.
</details>

---

### Question 17
Citez trois avantages majeurs des mainframes par rapport aux serveurs distribues.

<details>
<summary>Reponse</summary>

**Avantages des mainframes :**

1. **Haute disponibilite** : 99,999% de disponibilite (5 nines), architecture redondante native, failover automatique

2. **Securite integree** : Chiffrement materiel (CP Assist), RACF, certification EAL5+, Pervasive Encryption

3. **Performance** : Millions de transactions par seconde, jusqu'a 200 processeurs, 48 To de memoire

4. **Virtualisation native** : LPAR, z/VM depuis 1972, jusqu'a 85 partitions logiques

5. **Efficacite energetique** : Un mainframe remplace 50-100 serveurs avec moins d'energie

6. **Compatibilite** : Code COBOL de 1970 fonctionne encore aujourd'hui
</details>

---

### Question 18
Quel code ABEND indique une erreur de donnees non numeriques ?

- [ ] a) S0C1
- [ ] b) S0C4
- [ ] c) S0C7
- [ ] d) S0CB

<details>
<summary>Reponse</summary>

**c) S0C7**

S0C7 (Data Exception) indique qu'une operation arithmetique a ete tentee sur des donnees non numeriques. C'est une erreur frequente en COBOL.

Autres codes courants :
- S0C1 : Operation invalide
- S0C4 : Violation de protection memoire
- S0CB : Division par zero
</details>

---

### Question 19
Quelle est la difference entre JES2 et JES3 ?

<details>
<summary>Reponse</summary>

**JES2 vs JES3 :**

| Aspect | JES2 | JES3 |
|--------|------|------|
| **Architecture** | Decentralisee | Centralisee |
| **Gestion** | Chaque systeme autonome | Global (un systeme maitre) |
| **Allocation** | A l'execution du job | Avant l'execution |
| **Complexite** | Plus simple | Plus complexe |
| **Usage** | Plus repandu (~95%) | Environnements specifiques |

JES2 est le plus utilise. JES3 offre une meilleure gestion centralisee dans les complexes multi-systemes.
</details>

---

### Question 20
Associez chaque terme IBM a son equivalent courant :

| Terme IBM | Equivalent |
|-----------|------------|
| 1. DASD | a) Crash |
| 2. IPL | b) Machine virtuelle |
| 3. ABEND | c) Disque dur |
| 4. LPAR | d) Boot |

<details>
<summary>Reponse</summary>

| Terme IBM | Equivalent |
|-----------|------------|
| 1. DASD | **c) Disque dur** |
| 2. IPL | **d) Boot** |
| 3. ABEND | **a) Crash** |
| 4. LPAR | **b) Machine virtuelle** |
</details>

---

## Resume

| Concept | Definition |
|---------|------------|
| **z/OS** | Systeme d'exploitation IBM pour mainframes (z Architecture) |
| **Data Set** | Fichier dans l'environnement z/OS |
| **PDS** | Partitioned Data Set - Bibliotheque de membres |
| **VSAM** | Virtual Storage Access Method - Methode d'acces aux fichiers |
| **JCL** | Job Control Language - Langage de controle des travaux |
| **SPOOL** | Zone de stockage temporaire JES |
| **DASD** | Direct Access Storage Device - Disque dur |
| **LPAR** | Logical Partition - Partition logique |
| **IPL** | Initial Program Load - Demarrage du systeme |
| **ABEND** | Abnormal End - Terminaison anormale |

---
*Formation z/OS - M2i Formation*
