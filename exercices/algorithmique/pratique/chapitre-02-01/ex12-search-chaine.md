# Exercice 12 : Recherche dans une chaine (SEARCH)

## Enonce

Ecrire une procedure `SEARCH (C, CH, TROUVE, POS)` qui recherche un element C dans une chaine de caracteres CH, et rend sa position s'il existe.

## Structure de donnees

```text
// Definition du type chaine
Chaine = enregistrement
    str : tableau [1..MAX] de caractere
    l : entier    // longueur effective
fin enregistrement
```

## Parametres

| Parametre | Type | Mode | Description |
|-----------|------|------|-------------|
| C | caractere | Entree | Caractere a rechercher |
| CH | chaine | Entree | Chaine dans laquelle chercher |
| TROUVE | booleen | Sortie | VRAI si caractere trouve |
| POS | entier | Sortie | Position du caractere (0 si non trouve) |

## Exemple

```text
CH = "algorithme" (l = 10)
C = 'r'

Resultat : TROUVE = VRAI, POS = 4
```

```text
CH = "bonjour"
C = 'x'

Resultat : TROUVE = FAUX, POS = 0
```

## Concepts a utiliser

- Parcours de chaine avec arret anticipe
- Recherche sequentielle avec sentinelle booleenne
- Acces aux champs d'un enregistrement (CH.str[i], CH.l)

---

<details>
<summary>Solution</summary>

```text
PROCEDURE SEARCH (C: caractere, CH: chaine; var TROUVE: booleen, var POS: entier)
Var i : Entier
Debut
    i := 1
    POS := 0
    TROUVE := faux

    Tantque (non TROUVE) et (i <= CH.l) faire
        Si CH.str[i] = C Alors
            TROUVE := vrai
            POS := i
        Fsi
        i := i + 1
    Ftantque
Fin
```

**Analyse** :
- Complexite : O(n) dans le pire cas
- Arret des qu'on trouve le caractere (optimisation)
- Retourne la position de la PREMIERE occurrence

</details>

<details>
<summary>Variante - Version fonction</summary>

```text
FONCTION SEARCH (C: caractere, CH: chaine) : entier
// Retourne la position ou 0 si non trouve
Var i : Entier
Debut
    i := 1
    Tantque (i <= CH.l) faire
        Si CH.str[i] = C Alors
            Retourner i
        Fsi
        i := i + 1
    Ftantque
    Retourner 0
Fin
```

</details>

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-CHAINE        PIC X(100).
01 WS-LONGUEUR      PIC 9(3).
01 WS-CARACTERE     PIC X.
01 WS-POSITION      PIC 9(3) VALUE 0.
01 WS-TROUVE        PIC X VALUE 'N'.
   88 TROUVE        VALUE 'O'.
   88 NON-TROUVE    VALUE 'N'.

PROCEDURE DIVISION.
    INSPECT WS-CHAINE TALLYING WS-POSITION
        FOR LEADING CHARACTERS BEFORE INITIAL WS-CARACTERE.
    ADD 1 TO WS-POSITION.
    IF WS-POSITION <= WS-LONGUEUR
        SET TROUVE TO TRUE
    ELSE
        SET NON-TROUVE TO TRUE
        MOVE 0 TO WS-POSITION
    END-IF.
```

---

*Exercice 12 - Chaines de caracteres - Algorithmique Chapitre 02 - M2i*
