# QCM 04 - Couche Presentation (BMS)

## Questions (Chapitre V)

### Question 1
Quel est le role du BMS ?

- [ ] a) Gerer les fichiers VSAM
- [ ] b) Gerer les ecrans et l'interface utilisateur
- [ ] c) Gerer les transactions
- [ ] d) Gerer la securite

<details>
<summary>Reponse</summary>

**b) Gerer les ecrans et l'interface utilisateur**

BMS (Basic Mapping Support) gere la presentation des ecrans : definition, formatage, envoi et reception des donnees ecran.
</details>

---

### Question 2
Quelle macro definit le debut d'un MAPSET ?

- [ ] a) DFHMDF
- [ ] b) DFHMSD
- [ ] c) DFHMDI
- [ ] d) DFHBMS

<details>
<summary>Reponse</summary>

**b) DFHMSD**

DFHMSD (Map Set Definition) definit le debut et les caracteristiques generales d'un MAPSET.
</details>

---

### Question 3
Quelle macro definit une MAP individuelle ?

- [ ] a) DFHMDF
- [ ] b) DFHMSD
- [ ] c) DFHMDI
- [ ] d) DFHMAP

<details>
<summary>Reponse</summary>

**c) DFHMDI**

DFHMDI (Map Definition Initial) definit une MAP (ecran) a l'interieur d'un MAPSET.
</details>

---

### Question 4
Quelle macro definit un champ sur l'ecran ?

- [ ] a) DFHMDF
- [ ] b) DFHMSD
- [ ] c) DFHMDI
- [ ] d) DFHFLD

<details>
<summary>Reponse</summary>

**a) DFHMDF**

DFHMDF (Map Definition Field) definit un champ (texte fixe ou saisissable) sur l'ecran.
</details>

---

### Question 5
Quels sont les attributs standards d'un champ ?

<details>
<summary>Reponse</summary>

| Attribut | Valeurs | Description |
|----------|---------|-------------|
| ATTRB | ASKIP | Champ non modifiable (saut automatique) |
| | PROT | Champ protege (non modifiable) |
| | UNPROT | Champ non protege (saisissable) |
| | NUM | Numerique uniquement |
| | BRT | Brillant (bright) |
| | NORM | Intensite normale |
| | DRK | Sombre (invisible) |
| | IC | Initial Cursor (curseur initial) |
| | FSET | Field Set (force le renvoi) |
</details>

---

### Question 6
Comment definir un champ de saisie de 10 caracteres ?

<details>
<summary>Reponse</summary>

```assembler
NOM      DFHMDF POS=(5,20),LENGTH=10,ATTRB=(UNPROT,IC),INITIAL=' '
```

- `UNPROT` : Champ saisissable
- `IC` : Curseur positionne ici
- `LENGTH=10` : 10 caracteres max
</details>

---

### Question 7
Quelle option SEND MAP efface l'ecran avant l'affichage ?

- [ ] a) FREEKB
- [ ] b) ERASE
- [ ] c) CLEAR
- [ ] d) MAPONLY

<details>
<summary>Reponse</summary>

**b) ERASE**

L'option ERASE efface l'ecran avant d'afficher la nouvelle MAP.
</details>

---

### Question 8
Quelle est la difference entre MAPONLY et DATAONLY ?

<details>
<summary>Reponse</summary>

| Option | Comportement |
|--------|--------------|
| **MAPONLY** | Envoie uniquement les donnees fixes de la MAP (sans donnees variables) |
| **DATAONLY** | Envoie uniquement les donnees variables (sans rafraichir la structure) |
| **(sans option)** | Envoie les deux (MAP complete + donnees) |
</details>

---

### Question 9
Comment s'appelle la structure symbolique generee par BMS pour COBOL ?

- [ ] a) COPYBOOK
- [ ] b) DSECT
- [ ] c) CSECT
- [ ] d) COMMAREA

<details>
<summary>Reponse</summary>

**b) DSECT**

BMS genere un DSECT (Dummy Section) symbolique qui est le copybook COBOL decrivant les champs de l'ecran.
</details>

---

### Question 10
Quels sont les 3 suffixes des champs dans le DSECT BMS ?

<details>
<summary>Reponse</summary>

Pour un champ nomme XXXXX :

| Suffixe | Type | Utilisation |
|---------|------|-------------|
| XXXXXI | Input (PIC X) | Donnees recues de l'ecran |
| XXXXXO | Output (PIC X) | Donnees a envoyer a l'ecran |
| XXXXXA | Attribute (PIC X) | Attribut du champ (couleur, protection) |
| XXXXXL | Length (PIC S9(4) COMP) | Longueur effectivement saisie |
| XXXXXF | Flag (PIC X) | Flag de modification |
</details>

---

### Question 11
Quelle commande envoie un ecran formatee au terminal ?

```cobol
EXEC CICS
    ???? MAP('MENU01')
         MAPSET('MENUSET')
         FROM(MENU01O)
         ERASE
END-EXEC
```

- [ ] a) DISPLAY
- [ ] b) WRITE
- [ ] c) SEND
- [ ] d) OUTPUT

<details>
<summary>Reponse</summary>

**c) SEND**

`EXEC CICS SEND MAP('...') ...` envoie un ecran au terminal.
</details>

---

### Question 12
Quelle commande recoit les donnees saisies ?

```cobol
EXEC CICS
    ???? MAP('MENU01')
         MAPSET('MENUSET')
         INTO(MENU01I)
END-EXEC
```

- [ ] a) GET
- [ ] b) READ
- [ ] c) RECEIVE
- [ ] d) INPUT

<details>
<summary>Reponse</summary>

**c) RECEIVE**

`EXEC CICS RECEIVE MAP('...') ...` recoit les donnees saisies par l'utilisateur.
</details>

---

### Question 13
Que signifie CURSOR dans SEND MAP ?

- [ ] a) Affiche un curseur clignotant
- [ ] b) Positionne le curseur a un endroit specifique
- [ ] c) Active la touche curseur
- [ ] d) Verrouille le curseur

<details>
<summary>Reponse</summary>

**b) Positionne le curseur a un endroit specifique**

L'option CURSOR permet de positionner le curseur sur un champ particulier de l'ecran.
</details>

---

### Question 14
Comment positionner le curseur sur le champ NOM dans SEND MAP ?

<details>
<summary>Reponse</summary>

Methode 1 - Position absolue :
```cobol
EXEC CICS SEND MAP('ECRAN1')
          MAPSET('MAPSET1')
          CURSOR(position)
END-EXEC
```

Methode 2 - Via l'attribut du champ :
```cobol
MOVE -1 TO NOML    (longueur = -1)
EXEC CICS SEND MAP('ECRAN1')
          MAPSET('MAPSET1')
          CURSOR
END-EXEC
```
</details>

---

### Question 15
Que fait l'option FREEKB ?

- [ ] a) Libere le clavier apres l'envoi
- [ ] b) Verrouille le clavier
- [ ] c) Efface le clavier
- [ ] d) Reinitialise le clavier

<details>
<summary>Reponse</summary>

**a) Libere le clavier apres l'envoi**

FREEKB (Free KeyBoard) deverrouille le clavier pour permettre la saisie. Sans cette option, le clavier peut rester verrouille.
</details>

---

## Resume

### Macros BMS

| Macro | Role |
|-------|------|
| DFHMSD | Definition du MAPSET |
| DFHMDI | Definition d'une MAP |
| DFHMDF | Definition d'un champ |

### Attributs de champs

| Attribut | Description |
|----------|-------------|
| ASKIP | Saut automatique |
| PROT | Protege |
| UNPROT | Saisissable |
| NUM | Numerique |
| BRT | Brillant |
| IC | Curseur initial |

### Commandes SEND/RECEIVE

| Option | Effet |
|--------|-------|
| ERASE | Efface l'ecran |
| FREEKB | Libere le clavier |
| CURSOR | Positionne le curseur |
| MAPONLY | MAP sans donnees variables |
| DATAONLY | Donnees sans structure MAP |

---
*Formation CICS - M2i Formation*
