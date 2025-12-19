# Exercice 13 : Suppression dans une chaine (DELETE)

## Partie A : Suppression premiere occurrence

### Enonce

Ecrire une procedure `DELETE (C, CH, TROUVE)` qui supprime la premiere occurrence d'un element C dans une chaine de caracteres CH, s'il existe.

### Parametres

| Parametre | Type | Mode | Description |
|-----------|------|------|-------------|
| C | caractere | Entree | Caractere a supprimer |
| CH | chaine | Entree/Sortie | Chaine a modifier |
| TROUVE | booleen | Sortie | VRAI si caractere trouve et supprime |

### Exemple

```text
Avant : CH = "bonjour" (l = 7), C = 'o'
Apres : CH = "bnjour" (l = 6), TROUVE = VRAI

Avant : CH = "test" (l = 4), C = 'x'
Apres : CH = "test" (l = 4), TROUVE = FAUX
```

---

## Partie B : Suppression toutes occurrences

### Enonce

Ecrire une procedure `DELETE_ALL (C, CH, TROUVE)` qui supprime TOUTES les occurrences d'un element C dans une chaine de caracteres CH, s'il existe.

### Exemple

```text
Avant : CH = "bonjour" (l = 7), C = 'o'
Apres : CH = "bnjur" (l = 5), TROUVE = VRAI

Avant : CH = "abracadabra" (l = 11), C = 'a'
Apres : CH = "brcdbr" (l = 6), TROUVE = VRAI
```

---

## Concepts a utiliser

- Reutilisation de la procedure SEARCH
- Decalage d'elements dans un tableau
- Mise a jour de la longueur
- Boucle avec condition d'arret vs boucle complete

---

<details>
<summary>Solution Partie A - Version 1 (avec SEARCH)</summary>

```text
PROCEDURE DELETE (C: caractere; var CH: chaine, var TROUVE: booleen)
VAR i, POS : Entier
Debut
    SEARCH (C, CH, TROUVE, POS)

    Si TROUVE Alors
        // Decaler tous les elements a gauche
        Pour i de POS a CH.l - 1 faire
            CH.str[i] := CH.str[i+1]
        Fpour
        CH.l := CH.l - 1
    Fsi
Fin
```

**Avantage** : Reutilise la procedure SEARCH existante (modularite)

</details>

<details>
<summary>Solution Partie A - Version 2 (autonome)</summary>

```text
PROCEDURE DELETE (C: caractere; var CH: chaine, var TROUVE: booleen)
VAR i, j : Entier
Debut
    i := 1
    TROUVE := faux

    Tantque (non TROUVE) et (i <= CH.l) faire
        Si CH.str[i] = C Alors
            // Decaler les elements
            Pour j de i a CH.l - 1 faire
                CH.str[j] := CH.str[j+1]
            Fpour
            CH.l := CH.l - 1
            TROUVE := vrai
        Fsi
        i := i + 1
    Ftantque
Fin
```

</details>

<details>
<summary>Solution Partie B - DELETE_ALL</summary>

```text
PROCEDURE DELETE_ALL (C: caractere; var CH: chaine, var TROUVE: booleen)
VAR i, j : Entier
Debut
    i := 1
    TROUVE := faux

    Tantque (i <= CH.l) faire
        Si CH.str[i] = C Alors
            // Decaler les elements
            Pour j de i a CH.l - 1 faire
                CH.str[j] := CH.str[j+1]
            Fpour
            CH.l := CH.l - 1
            TROUVE := vrai
            // NE PAS incrementer i car le nouvel element est a la meme position
        Sinon
            i := i + 1
        Fsi
    Ftantque
Fin
```

**Attention** : Ne pas incrementer `i` quand on supprime, sinon on rate des caracteres consecutifs !

</details>

<details>
<summary>Solution Partie B - Version optimisee (sans decalages multiples)</summary>

```text
PROCEDURE DELETE_ALL_OPT (C: caractere; var CH: chaine, var TROUVE: booleen)
VAR i, j : Entier
Debut
    j := 0
    TROUVE := faux

    Pour i de 1 a CH.l faire
        Si CH.str[i] <> C Alors
            j := j + 1
            CH.str[j] := CH.str[i]
        Sinon
            TROUVE := vrai
        Fsi
    Fpour

    CH.l := j
Fin
```

**Avantage** : Un seul parcours O(n) au lieu de O(n^2) dans le pire cas

</details>

---

## Trace d'execution DELETE_ALL

```text
CH = "abba", C = 'b'

Iteration 1: i=1, CH.str[1]='a' <> 'b' -> j=1, CH.str[1]='a'
Iteration 2: i=2, CH.str[2]='b' = 'b'  -> TROUVE=vrai (on saute)
Iteration 3: i=3, CH.str[3]='b' = 'b'  -> TROUVE=vrai (on saute)
Iteration 4: i=4, CH.str[4]='a' <> 'b' -> j=2, CH.str[2]='a'

Resultat : CH = "aa" (l = 2)
```

---

## Correspondance COBOL

```cobol
* Suppression avec INSPECT et STRING
WORKING-STORAGE SECTION.
01 WS-CHAINE-IN     PIC X(100).
01 WS-CHAINE-OUT    PIC X(100).
01 WS-CARACTERE     PIC X.
01 WS-PTR           PIC 9(3).

PROCEDURE DIVISION.
* Suppression de tous les espaces par exemple
    MOVE 1 TO WS-PTR
    INSPECT WS-CHAINE-IN
        REPLACING ALL WS-CARACTERE BY SPACE
    STRING WS-CHAINE-IN DELIMITED BY '  '
        INTO WS-CHAINE-OUT
        WITH POINTER WS-PTR.
```

---

*Exercice 13 - Chaines de caracteres - Algorithmique Chapitre 02 - M2i*
