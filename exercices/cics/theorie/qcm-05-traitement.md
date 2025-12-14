# QCM 05 - Couche de Traitement (Commandes CICS)

## Questions (Chapitre VI)

### Question 1
Quelle commande lit un enregistrement VSAM ?

- [ ] a) `EXEC CICS GET FILE`
- [ ] b) `EXEC CICS READ FILE`
- [ ] c) `EXEC CICS FETCH FILE`
- [ ] d) `EXEC CICS SELECT FILE`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS READ FILE`**

La commande READ FILE permet de lire un enregistrement dans un fichier VSAM.
</details>

---

### Question 2
Quelle option de READ verrouille l'enregistrement pour mise a jour ?

- [ ] a) LOCK
- [ ] b) HOLD
- [ ] c) UPDATE
- [ ] d) RESERVE

<details>
<summary>Reponse</summary>

**c) UPDATE**

L'option UPDATE verrouille l'enregistrement pour permettre une modification ulterieure via REWRITE ou DELETE.
</details>

---

### Question 3
Quelle commande cree un nouvel enregistrement ?

- [ ] a) `EXEC CICS INSERT FILE`
- [ ] b) `EXEC CICS CREATE FILE`
- [ ] c) `EXEC CICS WRITE FILE`
- [ ] d) `EXEC CICS ADD FILE`

<details>
<summary>Reponse</summary>

**c) `EXEC CICS WRITE FILE`**

La commande WRITE FILE permet de creer un nouvel enregistrement dans un fichier VSAM.
</details>

---

### Question 4
Que faut-il faire obligatoirement avant un REWRITE ?

- [ ] a) Un DELETE
- [ ] b) Un READ avec UPDATE
- [ ] c) Un WRITE
- [ ] d) Un STARTBR

<details>
<summary>Reponse</summary>

**b) Un READ avec UPDATE**

Un REWRITE necessite obligatoirement un READ UPDATE prealable. Sans cela, CICS retourne INVREQ (code 16).
</details>

---

### Question 5
Quel code RESP indique que l'enregistrement n'existe pas ?

- [ ] a) NORMAL (0)
- [ ] b) NOTFND (13)
- [ ] c) DUPREC (14)
- [ ] d) NOTOPEN (19)

<details>
<summary>Reponse</summary>

**b) NOTFND (13)**

Le code DFHRESP(NOTFND) indique que l'enregistrement recherche n'a pas ete trouve.
</details>

---

### Question 6
Quel code RESP indique un doublon de cle ?

- [ ] a) NORMAL (0)
- [ ] b) NOTFND (13)
- [ ] c) DUPREC (14)
- [ ] d) INVREQ (16)

<details>
<summary>Reponse</summary>

**c) DUPREC (14)**

Le code DFHRESP(DUPREC) indique qu'un enregistrement avec la meme cle existe deja.
</details>

---

### Question 7
Quelle option READ permet une recherche sur le debut de la cle ?

- [ ] a) PARTIAL
- [ ] b) GENERIC
- [ ] c) LIKE
- [ ] d) PREFIX

<details>
<summary>Reponse</summary>

**b) GENERIC**

L'option GENERIC permet une recherche partielle sur le debut de la cle, avec KEYLENGTH pour specifier la longueur utilisee.
</details>

---

### Question 8
Que signifie GTEQ dans une commande READ ?

- [ ] a) Greater Than or EQual
- [ ] b) Get Equal
- [ ] c) Generic Equal
- [ ] d) Group Equal

<details>
<summary>Reponse</summary>

**a) Greater Than or EQual**

GTEQ retourne le premier enregistrement dont la cle est superieure ou egale a la cle recherchee.
</details>

---

### Question 9
Comment annuler un verrouillage sans modifier l'enregistrement ?

- [ ] a) `EXEC CICS RELEASE FILE`
- [ ] b) `EXEC CICS UNLOCK FILE`
- [ ] c) `EXEC CICS FREE FILE`
- [ ] d) `EXEC CICS CANCEL FILE`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS UNLOCK FILE`**

La commande UNLOCK libere le verrouillage sans effectuer de modification.
</details>

---

### Question 10
Completez la syntaxe READ :

```cobol
EXEC CICS
    READ FILE('CLIENTS')
         ????(WS-CLIENT-REC)
         ????(WS-CLIENT-KEY)
         RESP(WS-RESP)
END-EXEC
```

<details>
<summary>Reponse</summary>

```cobol
EXEC CICS
    READ FILE('CLIENTS')
         INTO(WS-CLIENT-REC)
         RIDFLD(WS-CLIENT-KEY)
         RESP(WS-RESP)
END-EXEC
```

- **INTO** : Zone receptrice des donnees
- **RIDFLD** : Record IDentification FieLD (cle de recherche)
</details>

---

### Question 11
Completez la syntaxe WRITE :

```cobol
EXEC CICS
    WRITE FILE('CLIENTS')
          ????(WS-CLIENT-REC)
          ????(WS-CLIENT-KEY)
          RESP(WS-RESP)
END-EXEC
```

<details>
<summary>Reponse</summary>

```cobol
EXEC CICS
    WRITE FILE('CLIENTS')
          FROM(WS-CLIENT-REC)
          RIDFLD(WS-CLIENT-KEY)
          RESP(WS-RESP)
END-EXEC
```

- **FROM** : Zone source des donnees a ecrire
- **RIDFLD** : Cle de l'enregistrement
</details>

---

### Question 12
Quelle commande supprime un enregistrement ?

- [ ] a) `EXEC CICS REMOVE FILE`
- [ ] b) `EXEC CICS DELETE FILE`
- [ ] c) `EXEC CICS DROP FILE`
- [ ] d) `EXEC CICS ERASE FILE`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS DELETE FILE`**

La commande DELETE supprime un enregistrement. Elle peut etre directe (avec RIDFLD) ou apres un READ UPDATE.
</details>

---

### Question 13
Quelle option DELETE permet de supprimer plusieurs enregistrements ?

- [ ] a) MULTI
- [ ] b) GENERIC
- [ ] c) BATCH
- [ ] d) GROUP

<details>
<summary>Reponse</summary>

**b) GENERIC**

L'option GENERIC avec DELETE supprime tous les enregistrements dont la cle correspond au prefixe. NUMREC retourne le nombre supprime.
</details>

---

### Question 14
Quel code RESP indique "pas de READ UPDATE avant REWRITE" ?

- [ ] a) NOTFND (13)
- [ ] b) DUPREC (14)
- [ ] c) INVREQ (16)
- [ ] d) NOSPACE (18)

<details>
<summary>Reponse</summary>

**c) INVREQ (16)**

INVREQ (Invalid Request) est retourne si REWRITE est execute sans READ UPDATE prealable.
</details>

---

### Question 15
Quel code RESP indique que le fichier est plein ?

- [ ] a) FULL (17)
- [ ] b) NOSPACE (18)
- [ ] c) OVERFLOW (20)
- [ ] d) LIMIT (21)

<details>
<summary>Reponse</summary>

**b) NOSPACE (18)**

DFHRESP(NOSPACE) indique qu'il n'y a plus d'espace disponible pour ecrire dans le fichier.
</details>

---

## Resume des commandes CICS fichiers

| Commande | Syntaxe | Description |
|----------|---------|-------------|
| READ | `READ FILE(...) INTO(...) RIDFLD(...)` | Lecture |
| WRITE | `WRITE FILE(...) FROM(...) RIDFLD(...)` | Creation |
| REWRITE | `REWRITE FILE(...) FROM(...)` | Mise a jour |
| DELETE | `DELETE FILE(...) [RIDFLD(...)]` | Suppression |
| UNLOCK | `UNLOCK FILE(...)` | Liberation verrou |

## Codes RESP principaux

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | NORMAL | Operation reussie |
| 13 | NOTFND | Non trouve |
| 14 | DUPREC | Cle en double |
| 16 | INVREQ | Requete invalide |
| 18 | NOSPACE | Plus d'espace |
| 19 | NOTOPEN | Fichier non ouvert |
| 22 | DISABLED | Fichier desactive |

---
*Formation CICS - M2i Formation*
