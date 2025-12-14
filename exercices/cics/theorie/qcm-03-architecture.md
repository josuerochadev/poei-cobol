# QCM 03 - Architecture Multicouches

## Questions (Chapitres III-IV)

### Question 1
Combien de couches comporte l'architecture applicative CICS classique ?

- [ ] a) 2 couches
- [ ] b) 3 couches
- [ ] c) 4 couches
- [ ] d) 5 couches

<details>
<summary>Reponse</summary>

**b) 3 couches**

L'architecture CICS comporte 3 couches :
1. Couche Presentation (ecrans BMS)
2. Couche Traitement (logique metier)
3. Couche Donnees (acces VSAM/DB2)
</details>

---

### Question 2
Quelle couche gere les ecrans utilisateur ?

- [ ] a) Couche Donnees
- [ ] b) Couche Traitement
- [ ] c) Couche Presentation
- [ ] d) Couche Metier

<details>
<summary>Reponse</summary>

**c) Couche Presentation**

La couche Presentation gere l'interface utilisateur : ecrans BMS, saisie et affichage des donnees.
</details>

---

### Question 3
Quel est le role de la couche Traitement ?

- [ ] a) Afficher les ecrans
- [ ] b) Acceder aux fichiers VSAM
- [ ] c) Contenir la logique metier et les regles de gestion
- [ ] d) Gerer les terminaux

<details>
<summary>Reponse</summary>

**c) Contenir la logique metier et les regles de gestion**

La couche Traitement contient la logique metier, les validations fonctionnelles, les calculs et l'orchestration des operations.
</details>

---

### Question 4
Que signifie BMS ?

- [ ] a) Basic Map Support
- [ ] b) Block Mode System
- [ ] c) Buffer Management System
- [ ] d) Binary Map Service

<details>
<summary>Reponse</summary>

**a) Basic Mapping Support**

BMS est le systeme de gestion des ecrans sous CICS. Il permet de definir des ecrans (maps) et de gerer les entrees/sorties ecran.
</details>

---

### Question 5
Qu'est-ce qu'un MAPSET ?

- [ ] a) Un programme COBOL
- [ ] b) Un ensemble de maps (ecrans) compiles ensemble
- [ ] c) Une table de couleurs
- [ ] d) Un fichier de configuration

<details>
<summary>Reponse</summary>

**b) Un ensemble de maps (ecrans) compiles ensemble**

Un MAPSET est un ensemble de maps regroupees dans une meme source BMS et compilees ensemble. Il genere un module physique et un DSECT symbolique.
</details>

---

### Question 6
Quelle zone permet l'echange de donnees entre programmes CICS ?

- [ ] a) DFHCOMMAREA
- [ ] b) WORKING-STORAGE
- [ ] c) LINKAGE SECTION
- [ ] d) FILE SECTION

<details>
<summary>Reponse</summary>

**a) DFHCOMMAREA**

La DFHCOMMAREA (Communication Area) est la zone standard d'echange de donnees entre programmes CICS. Elle est passee via LINK ou XCTL.
</details>

---

### Question 7
Quelle commande appelle un sous-programme avec retour ?

- [ ] a) XCTL
- [ ] b) LINK
- [ ] c) CALL
- [ ] d) PERFORM

<details>
<summary>Reponse</summary>

**b) LINK**

`EXEC CICS LINK PROGRAM('pgm')` appelle un sous-programme et retourne apres son execution. XCTL transfere le controle sans retour.
</details>

---

### Question 8
Quelle est la difference entre LINK et XCTL ?

<details>
<summary>Reponse</summary>

| LINK | XCTL |
|------|------|
| Appel avec retour | Transfert sans retour |
| Le programme appelant reste en memoire | Le programme appelant est decharge |
| Comme un CALL | Comme un GO TO entre programmes |
| `EXEC CICS LINK PROGRAM('X')` | `EXEC CICS XCTL PROGRAM('X')` |
</details>

---

### Question 9
Qu'est-ce que la COMMAREA ?

- [ ] a) Une zone temporaire en memoire centrale
- [ ] b) Une zone de communication entre programmes
- [ ] c) Un fichier temporaire
- [ ] d) Une file d'attente

<details>
<summary>Reponse</summary>

**b) Une zone de communication entre programmes**

La COMMAREA est une zone de donnees passee entre programmes via LINK, XCTL ou RETURN. Elle permet de transmettre des parametres et contexte.
</details>

---

### Question 10
Dans quel fichier COBOL declare-t-on la COMMAREA recue ?

- [ ] a) WORKING-STORAGE SECTION
- [ ] b) FILE SECTION
- [ ] c) LINKAGE SECTION
- [ ] d) LOCAL-STORAGE SECTION

<details>
<summary>Reponse</summary>

**c) LINKAGE SECTION**

La DFHCOMMAREA recue d'un autre programme ou d'une transaction precedente est declaree dans la LINKAGE SECTION.
</details>

---

### Question 11
Quelle commande envoie un ecran a l'utilisateur ?

- [ ] a) `EXEC CICS READ MAP`
- [ ] b) `EXEC CICS SEND MAP`
- [ ] c) `EXEC CICS WRITE MAP`
- [ ] d) `EXEC CICS DISPLAY MAP`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS SEND MAP`**

`EXEC CICS SEND MAP('mapname')` envoie un ecran au terminal.
</details>

---

### Question 12
Quelle commande recoit les donnees saisies par l'utilisateur ?

- [ ] a) `EXEC CICS READ MAP`
- [ ] b) `EXEC CICS GET MAP`
- [ ] c) `EXEC CICS RECEIVE MAP`
- [ ] d) `EXEC CICS INPUT MAP`

<details>
<summary>Reponse</summary>

**c) `EXEC CICS RECEIVE MAP`**

`EXEC CICS RECEIVE MAP('mapname')` recoit les donnees saisies depuis le terminal.
</details>

---

### Question 13
Qu'est-ce que l'EIB ?

- [ ] a) Executive Interface Block
- [ ] b) Extended Information Buffer
- [ ] c) External Input Block
- [ ] d) Error Information Base

<details>
<summary>Reponse</summary>

**a) Executive Interface Block**

L'EIB est une zone fournie automatiquement par CICS contenant des informations sur la tache en cours (transaction, terminal, date, heure, touches appuyees, etc.).
</details>

---

### Question 14
Quel champ EIB contient la touche PF pressee ?

- [ ] a) EIBTRNID
- [ ] b) EIBCALEN
- [ ] c) EIBAID
- [ ] d) EIBTRMID

<details>
<summary>Reponse</summary>

**c) EIBAID**

EIBAID (Attention IDentifier) contient le code de la touche pressee (ENTER, PF1-24, CLEAR, PA1-3).
</details>

---

### Question 15
Quel champ EIB contient la longueur de la COMMAREA ?

- [ ] a) EIBTRNID
- [ ] b) EIBCALEN
- [ ] c) EIBAID
- [ ] d) EIBTRMID

<details>
<summary>Reponse</summary>

**b) EIBCALEN**

EIBCALEN contient la longueur de la COMMAREA recue. Si EIBCALEN = 0, c'est le premier appel de la transaction.
</details>

---

## Resume

### Les 3 couches CICS

| Couche | Role | Composants |
|--------|------|------------|
| Presentation | Interface utilisateur | Maps BMS, SEND/RECEIVE MAP |
| Traitement | Logique metier | Programmes COBOL-CICS |
| Donnees | Acces donnees | VSAM, DB2, LINK vers DAO |

### Communication entre programmes

| Commande | Type | Retour |
|----------|------|--------|
| LINK | Appel sous-programme | Oui |
| XCTL | Transfert de controle | Non |
| RETURN | Fin de programme | N/A |

### Champs EIB importants

| Champ | Contenu |
|-------|---------|
| EIBTRNID | Code transaction (4 car.) |
| EIBAID | Touche pressee |
| EIBCALEN | Longueur COMMAREA |
| EIBTRMID | ID terminal |
| EIBDATE | Date (format 0CYYDDD) |
| EIBTIME | Heure (format HHMMSS) |

---
*Formation CICS - M2i Formation*
