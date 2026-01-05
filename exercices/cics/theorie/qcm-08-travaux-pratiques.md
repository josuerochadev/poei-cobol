# QCM 08 - Travaux Pratiques : Application Gestion des Crédits

## Questions (Chapitre VIII)

### Question 1
Combien de programmes compose l'application de gestion des crédits dans l'architecture 3 tiers ?

- [ ] a) 1 programme
- [ ] b) 2 programmes
- [ ] c) 3 programmes
- [ ] d) 4 programmes

<details>
<summary>Réponse</summary>

**c) 3 programmes**

L'application est composée de 3 programmes :
1. **CREDPRES** - Couche Présentation
2. **CREDTRT** - Couche Traitement
3. **CREDDAO** - Couche Données (Data Access Object)
</details>

---

### Question 2
Quel type de fichiers VSAM est utilisé pour stocker les données employés et crédits ?

- [ ] a) ESDS (Entry Sequenced Data Set)
- [ ] b) KSDS (Key Sequenced Data Set)
- [ ] c) RRDS (Relative Record Data Set)
- [ ] d) LSDS (Linear Sequenced Data Set)

<details>
<summary>Réponse</summary>

**b) KSDS (Key Sequenced Data Set)**

Les fichiers EMPLOYE et CRE-EMP sont de type KSDS avec ID-EMPL comme clé primaire. Ce type permet l'accès direct par clé.
</details>

---

### Question 3
Quelle est la clé commune entre les fichiers EMPLOYE et CRE-EMP ?

- [ ] a) NAME-EMPL
- [ ] b) DEPT-EMPL
- [ ] c) ID-EMPL
- [ ] d) LIB-CREDIT

<details>
<summary>Réponse</summary>

**c) ID-EMPL**

ID-EMPL (PIC X(6)) est la clé primaire des deux fichiers et établit la relation logique entre un employé et son crédit.
</details>

---

### Question 4
Comment identifier si un employé a un crédit actif ?

- [ ] a) Si le montant du salaire est supérieur à 3000
- [ ] b) Si ETAT-CRED-EMPL = 'Y'
- [ ] c) Si un enregistrement existe dans CRE-EMP
- [ ] d) Si RESTE-CREDIT > 0

<details>
<summary>Réponse</summary>

**b) Si ETAT-CRED-EMPL = 'Y'**

Le champ ETAT-CRED-EMPL indique l'état du crédit :
- 'Y' = Crédit actif
- 'N' = Pas de crédit

On vérifie d'abord ce champ avant d'accéder au fichier CRE-EMP.
</details>

---

### Question 5
Quel format est utilisé pour les montants numériques (SALAIRE, MONTANT-TOTAL, etc.) ?

- [ ] a) PIC 9(7)V99 DISPLAY
- [ ] b) PIC 9(7)V99 COMP-3
- [ ] c) PIC S9(7)V99 COMP
- [ ] d) PIC 9(7).99

<details>
<summary>Réponse</summary>

**b) PIC 9(7)V99 COMP-3**

Le format COMP-3 (packed decimal) est utilisé pour les montants car il économise de l'espace disque et permet des calculs efficaces. Un champ PIC 9(7)V99 COMP-3 occupe 5 octets au lieu de 9 en DISPLAY.
</details>

---

### Question 6
Que se passe-t-il quand le RESTE-CREDIT devient égal à zéro ?

- [ ] a) L'enregistrement crédit est supprimé
- [ ] b) Le programme affiche un message d'erreur
- [ ] c) ETAT-CRED-EMPL passe à 'N' dans le fichier EMPLOYE
- [ ] d) La transaction se termine automatiquement

<details>
<summary>Réponse</summary>

**c) ETAT-CRED-EMPL passe à 'N' dans le fichier EMPLOYE**

Quand le crédit est soldé (RESTE = 0), le programme met à jour ETAT-CRED-EMPL à 'N' dans le fichier EMPLOYE via le paragraphe 2100-SOLDER-CREDIT.
</details>

---

### Question 7
Quel champ de l'EIB permet de détecter le premier appel d'une transaction ?

- [ ] a) EIBAID
- [ ] b) EIBTRNID
- [ ] c) EIBCALEN
- [ ] d) EIBTRMID

<details>
<summary>Réponse</summary>

**c) EIBCALEN**

EIBCALEN contient la longueur de la COMMAREA reçue. Si EIBCALEN = 0, c'est le premier appel de la transaction (pas de COMMAREA précédente).

```cobol
IF EIBCALEN = 0
    SET PREMIER-PASSAGE TO TRUE
    PERFORM 1000-AFFICHER-ECRAN-VIDE
END-IF
```
</details>

---

### Question 8
Quelle commande CICS est utilisée pour appeler un programme de couche inférieure avec retour ?

- [ ] a) XCTL
- [ ] b) LINK
- [ ] c) CALL
- [ ] d) INVOKE

<details>
<summary>Réponse</summary>

**b) LINK**

`EXEC CICS LINK PROGRAM('CREDTRT')` appelle le programme avec retour automatique après son exécution. XCTL transfert le contrôle sans retour.
</details>

---

### Question 9
Comment la couche Présentation communique-t-elle avec la couche Traitement ?

- [ ] a) Via des fichiers temporaires
- [ ] b) Via la COMMAREA
- [ ] c) Via des variables globales
- [ ] d) Via des files MQ

<details>
<summary>Réponse</summary>

**b) Via la COMMAREA**

La COMMAREA est la zone d'échange standard entre programmes CICS. Elle contient :
- CA-ACTION (consulter/payer)
- CA-CODE-RETOUR
- CA-MESSAGE
- CA-EMPLOYE-DATA
- CA-CREDIT-DATA
</details>

---

### Question 10
Quelle option de READ est nécessaire pour pouvoir faire un REWRITE ensuite ?

- [ ] a) LOCK
- [ ] b) HOLD
- [ ] c) UPDATE
- [ ] d) MODIFY

<details>
<summary>Réponse</summary>

**c) UPDATE**

L'option UPDATE verrouille l'enregistrement pour modification :

```cobol
EXEC CICS
    READ FILE(DAO-FICHIER)
         INTO(WS-DATA-BUFFER)
         RIDFLD(DAO-CLE)
         UPDATE
         RESP(WS-RESP)
END-EXEC
```

Sans UPDATE, le REWRITE échouerait avec une erreur INVREQ.
</details>

---

### Question 11
Quel code retour indique qu'un enregistrement n'a pas été trouvé (NOTFND) ?

- [ ] a) 00
- [ ] b) 13
- [ ] c) 14
- [ ] d) 99

<details>
<summary>Réponse</summary>

**b) 13**

Les codes retour utilisés dans l'application :
- **00** : Opération réussie (NORMAL)
- **13** : Enregistrement non trouvé (NOTFND)
- **14** : Doublon (DUPREC)
- **18** : Plus d'espace (NOSPACE)
- **22** : Fichier désactivé (DISABLED)
- **99** : Autre erreur
</details>

---

### Question 12
Quelle action est associée au code 'P' dans CA-ACTION ?

- [ ] a) Produire un rapport
- [ ] b) Payer une échéance
- [ ] c) Programmer un crédit
- [ ] d) Parcourir les enregistrements

<details>
<summary>Réponse</summary>

**b) Payer une échéance**

CA-ACTION définit l'opération demandée :
- 'C' = Consulter (CA-CONSULTER)
- 'P' = Payer une échéance (CA-PAYER)
</details>

---

### Question 13
Quelle touche fonction déclenche le paiement d'une échéance ?

- [ ] a) ENTER
- [ ] b) PF3
- [ ] c) PF5
- [ ] d) PF12

<details>
<summary>Réponse</summary>

**c) PF5**

Les touches de l'application :
- **ENTER** : Rechercher un employé
- **PF5** : Payer une échéance
- **PF3** : Quitter la transaction
</details>

---

### Question 14
Quel est le rôle du programme CREDDAO ?

- [ ] a) Afficher les écrans
- [ ] b) Calculer les échéances
- [ ] c) Accéder aux fichiers VSAM
- [ ] d) Valider les données saisies

<details>
<summary>Réponse</summary>

**c) Accéder aux fichiers VSAM**

CREDDAO (Data Access Object) est la couche Données. Il encapsule tous les accès fichiers :
- READ (lecture simple)
- READ UPDATE (lecture pour mise à jour)
- REWRITE (mise à jour)
- WRITE (création)
- DELETE (suppression)
</details>

---

### Question 15
Comment le programme Présentation gère-t-il les touches non autorisées ?

- [ ] a) Il ignore la touche
- [ ] b) Il affiche "Touche non autorisée"
- [ ] c) Il termine la transaction
- [ ] d) Il appelle un programme d'erreur

<details>
<summary>Réponse</summary>

**b) Il affiche "Touche non autorisée"**

```cobol
EVALUATE EIBAID
    WHEN DFHENTER
        PERFORM 2100-RECHERCHER-EMPLOYE
    WHEN DFHPF5
        PERFORM 2200-PAYER-ECHEANCE
    WHEN DFHPF3
        PERFORM 9000-QUITTER
    WHEN OTHER
        MOVE 'Touche non autorisée' TO MSGO
        PERFORM 3000-AFFICHER-MESSAGE
END-EVALUATE
```
</details>

---

### Question 16
Quel copybook contient la structure de l'enregistrement employé ?

- [ ] a) DFHAID.cpy
- [ ] b) DFHBMSCA.cpy
- [ ] c) EMPLOYE.cpy
- [ ] d) CREDSET.cpy

<details>
<summary>Réponse</summary>

**c) EMPLOYE.cpy**

Les copybooks de l'application :
- **EMPLOYE.cpy** : Structure enregistrement employé
- **CREDEMP.cpy** : Structure enregistrement crédit
- **DFHAID** : Constantes touches (DFHENTER, DFHPF3, etc.)
- **DFHBMSCA** : Constantes attributs BMS
- **CREDSET** : Structure écran BMS
</details>

---

### Question 17
Quelle option SEND MAP permet d'envoyer uniquement les données sans rafraîchir la structure de l'écran ?

- [ ] a) MAPONLY
- [ ] b) DATAONLY
- [ ] c) ERASE
- [ ] d) FREEKB

<details>
<summary>Réponse</summary>

**b) DATAONLY**

```cobol
3000-AFFICHER-MESSAGE.
    EXEC CICS
        SEND MAP('CREDMAP')
             MAPSET('CREDSET')
             FROM(CREDMAPO)
             DATAONLY
    END-EXEC.
```

DATAONLY envoie uniquement les données variables (plus performant quand la structure est déjà affichée).
</details>

---

### Question 18
Que fait la condition 88 EMP-A-CREDIT dans le copybook EMPLOYE ?

- [ ] a) Définit un niveau conditionnel pour tester si EMP-ETAT-CRED = 'Y'
- [ ] b) Déclare une variable booléenne
- [ ] c) Crée un index sur le champ crédit
- [ ] d) Initialise le champ à 'Y'

<details>
<summary>Réponse</summary>

**a) Définit un niveau conditionnel pour tester si EMP-ETAT-CRED = 'Y'**

```cobol
05  EMP-ETAT-CRED       PIC X(1).
    88  EMP-A-CREDIT    VALUE 'Y'.
    88  EMP-SANS-CREDIT VALUE 'N'.
```

Les niveaux 88 permettent d'écrire des conditions lisibles :
- `IF EMP-A-CREDIT` au lieu de `IF EMP-ETAT-CRED = 'Y'`
- `SET EMP-SANS-CREDIT TO TRUE` pour affecter 'N'
</details>

---

### Question 19
Quel est le code transaction de l'application de gestion des crédits ?

- [ ] a) EMPL
- [ ] b) CRED
- [ ] c) GCRE
- [ ] d) PAIE

<details>
<summary>Réponse</summary>

**b) CRED**

Le code transaction est CRED (4 caractères maximum). Il est utilisé dans RETURN TRANSID :

```cobol
EXEC CICS
    RETURN TRANSID('CRED')
           COMMAREA(WS-COMMAREA)
           LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```
</details>

---

### Question 20
Pourquoi le calcul du reste après paiement vérifie-t-il si le résultat est négatif ?

```cobol
SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE
IF CRD-RESTE < 0
    MOVE 0 TO CRD-RESTE
END-IF
```

- [ ] a) Pour éviter un abend ON SIZE ERROR
- [ ] b) Pour gérer le cas où l'échéance dépasse le reste dû
- [ ] c) Pour respecter une contrainte DB2
- [ ] d) Pour afficher un message d'erreur

<details>
<summary>Réponse</summary>

**b) Pour gérer le cas où l'échéance dépasse le reste dû**

Cette vérification protège contre le cas où le dernier paiement serait supérieur au reste dû. Par exemple, si RESTE = 100 et ECHEANCE = 250, sans cette protection on aurait RESTE = -150 ce qui n'a pas de sens métier.
</details>

---

## Résumé

### Architecture de l'application

| Couche | Programme | Rôle |
|--------|-----------|------|
| Présentation | CREDPRES | Écrans BMS, validation format, touches |
| Traitement | CREDTRT | Logique métier, règles de gestion |
| Données | CREDDAO | Accès fichiers VSAM |

### Fichiers VSAM

| Fichier | Type | Clé | Contenu |
|---------|------|-----|---------|
| EMPLOYE | KSDS | ID-EMPL | Données employé + état crédit |
| CRE-EMP | KSDS | ID-EMPL | Détails crédit |

### Flux de traitement

```
Terminal → CREDPRES → CREDTRT → CREDDAO → VSAM
             ↑           ↑          ↑
         COMMAREA    COMMAREA   READ/REWRITE
```

### Codes retour DAO

| Code | Signification |
|------|---------------|
| 00 | NORMAL - Succès |
| 13 | NOTFND - Non trouvé |
| 14 | DUPREC - Doublon |
| 18 | NOSPACE - Plus d'espace |
| 22 | DISABLED - Fichier désactivé |
| 99 | Autre erreur |

---
*Formation CICS - M2i Formation*
