# QCM 06 - Couche Donnees (Browse et Transactions)

## Questions (Chapitre VII)

### Question 1
Quelle commande initialise un parcours sequentiel de fichier ?

- [ ] a) `EXEC CICS OPEN FILE`
- [ ] b) `EXEC CICS START FILE`
- [ ] c) `EXEC CICS STARTBR FILE`
- [ ] d) `EXEC CICS BEGIN FILE`

<details>
<summary>Reponse</summary>

**c) `EXEC CICS STARTBR FILE`**

STARTBR (Start Browse) initialise un parcours sequentiel a partir d'une cle donnee.
</details>

---

### Question 2
Quelle commande lit l'enregistrement suivant dans un parcours ?

- [ ] a) `EXEC CICS READ FILE`
- [ ] b) `EXEC CICS READNEXT FILE`
- [ ] c) `EXEC CICS NEXT FILE`
- [ ] d) `EXEC CICS FETCH FILE`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS READNEXT FILE`**

READNEXT lit l'enregistrement suivant dans le parcours. READPREV lit le precedent.
</details>

---

### Question 3
Quelle commande termine obligatoirement un parcours ?

- [ ] a) `EXEC CICS CLOSE FILE`
- [ ] b) `EXEC CICS STOP FILE`
- [ ] c) `EXEC CICS ENDBR FILE`
- [ ] d) `EXEC CICS END FILE`

<details>
<summary>Reponse</summary>

**c) `EXEC CICS ENDBR FILE`**

ENDBR (End Browse) termine le parcours. C'est **obligatoire** pour liberer les ressources.
</details>

---

### Question 4
Quel code RESP indique la fin de fichier ?

- [ ] a) EOF (15)
- [ ] b) ENDFILE (20)
- [ ] c) NOTFND (13)
- [ ] d) END (99)

<details>
<summary>Reponse</summary>

**b) ENDFILE (20)**

DFHRESP(ENDFILE) indique qu'on a atteint la fin du fichier lors d'un READNEXT.
</details>

---

### Question 5
Dans quel ordre doit-on executer les commandes de parcours ?

- [ ] a) READNEXT, STARTBR, ENDBR
- [ ] b) ENDBR, STARTBR, READNEXT
- [ ] c) STARTBR, READNEXT, ENDBR
- [ ] d) STARTBR, ENDBR, READNEXT

<details>
<summary>Reponse</summary>

**c) STARTBR, READNEXT, ENDBR**

Sequence correcte :
1. STARTBR - Initialise le parcours
2. READNEXT (boucle) - Lit les enregistrements
3. ENDBR - Termine le parcours
</details>

---

### Question 6
Quelle commande valide les modifications d'une transaction ?

- [ ] a) `EXEC CICS COMMIT`
- [ ] b) `EXEC CICS SYNCPOINT`
- [ ] c) `EXEC CICS VALIDATE`
- [ ] d) `EXEC CICS SAVE`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS SYNCPOINT`**

SYNCPOINT valide toutes les modifications effectuees depuis le dernier point de synchronisation.
</details>

---

### Question 7
Quelle commande annule les modifications d'une transaction ?

- [ ] a) `EXEC CICS CANCEL`
- [ ] b) `EXEC CICS UNDO`
- [ ] c) `EXEC CICS SYNCPOINT ROLLBACK`
- [ ] d) `EXEC CICS ABORT`

<details>
<summary>Reponse</summary>

**c) `EXEC CICS SYNCPOINT ROLLBACK`**

SYNCPOINT ROLLBACK annule toutes les modifications depuis le dernier point de synchronisation.
</details>

---

### Question 8
Que garantissent les proprietes ACID ?

<details>
<summary>Reponse</summary>

| Lettre | Propriete | Description |
|--------|-----------|-------------|
| **A** | Atomicite | Tout ou rien : la transaction s'execute completement ou pas du tout |
| **C** | Coherence | La base reste dans un etat coherent avant et apres |
| **I** | Isolation | Les transactions concurrentes n'interferent pas |
| **D** | Durabilite | Une fois validee, la transaction persiste meme en cas de panne |
</details>

---

### Question 9
Qu'est-ce qu'une LUW ?

- [ ] a) Logical User Work
- [ ] b) Logical Unit of Work
- [ ] c) Limited Update Window
- [ ] d) Local Update Work

<details>
<summary>Reponse</summary>

**b) Logical Unit of Work**

Une LUW (Logical Unit of Work) est un ensemble d'operations qui doivent etre validees ou annulees ensemble.
</details>

---

### Question 10
Dans un virement bancaire, que se passe-t-il si le credit echoue apres le debit ?

- [ ] a) Le debit reste effectue
- [ ] b) SYNCPOINT ROLLBACK annule le debit
- [ ] c) Les deux operations restent en attente
- [ ] d) Le systeme plante

<details>
<summary>Reponse</summary>

**b) SYNCPOINT ROLLBACK annule le debit**

Grace aux proprietes ACID, un ROLLBACK annule toutes les modifications de la LUW, y compris le debit deja effectue.
</details>

---

### Question 11
Quel code RESP indique qu'un enregistrement est verrouille ?

- [ ] a) LOCKED (25)
- [ ] b) BUSY (26)
- [ ] c) RECORDBUSY (27)
- [ ] d) INUSE (28)

<details>
<summary>Reponse</summary>

**c) RECORDBUSY**

DFHRESP(RECORDBUSY) indique qu'un autre utilisateur a verrouille l'enregistrement via READ UPDATE.
</details>

---

### Question 12
Quel champ EIB contient l'ID utilisateur ?

- [ ] a) EIBTRNID
- [ ] b) EIBUSERID
- [ ] c) EIBTRMID
- [ ] d) EIBUSER

<details>
<summary>Reponse</summary>

Pour obtenir l'ID utilisateur, on utilise :
```cobol
EXEC CICS ASSIGN USERID(WS-USERID) END-EXEC
```

EIBTRMID contient l'ID du terminal, pas de l'utilisateur.
</details>

---

### Question 13
Comment journaliser une operation dans une file transiente ?

- [ ] a) `EXEC CICS WRITE TD`
- [ ] b) `EXEC CICS WRITEQ TD`
- [ ] c) `EXEC CICS LOG TD`
- [ ] d) `EXEC CICS PUT TD`

<details>
<summary>Reponse</summary>

**b) `EXEC CICS WRITEQ TD`**

WRITEQ TD ecrit dans une Transient Data Queue pour la journalisation ou l'impression.
</details>

---

### Question 14
Quelle est la difference entre TS Queue et TD Queue ?

<details>
<summary>Reponse</summary>

| Caracteristique | TS Queue | TD Queue |
|-----------------|----------|----------|
| Type | Temporary Storage | Transient Data |
| Acces | Random (par numero item) | Sequentiel FIFO |
| Declenchement | Non | Possible (ATI) |
| Usage | Scratch pad, pagination | Journalisation, impression |
| Definition | Dynamique | Predenie (DCT) |
</details>

---

### Question 15
Qu'est-ce que l'ATI (Automatic Transaction Initiation) ?

<details>
<summary>Reponse</summary>

L'**ATI** est un mecanisme qui declenche automatiquement une transaction lorsqu'une TD Queue atteint un certain nombre d'enregistrements.

Exemple : Une file d'impression declenche automatiquement le programme d'impression apres 100 lignes.
</details>

---

## Resume des commandes Browse

| Commande | Description |
|----------|-------------|
| STARTBR | Debut du parcours |
| READNEXT | Lecture suivante |
| READPREV | Lecture precedente |
| RESETBR | Repositionnement |
| ENDBR | Fin du parcours |

## Gestion transactionnelle

| Commande | Description |
|----------|-------------|
| SYNCPOINT | Validation (commit) |
| SYNCPOINT ROLLBACK | Annulation (rollback) |

## Files d'attente

| Type | Commandes | Usage |
|------|-----------|-------|
| TS Queue | WRITEQ TS, READQ TS, DELETEQ TS | Donnees temporaires |
| TD Queue | WRITEQ TD, READQ TD | Journalisation, impression |

---
*Formation CICS - M2i Formation*
