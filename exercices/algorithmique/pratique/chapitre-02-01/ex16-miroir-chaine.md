# Exercice 16 : Image miroir d'une chaine (MIROIR)

## Enonce

Ecrire une procedure `MIROIR (CH1, CH2)` qui retourne dans CH2 l'image miroir de CH1.

## Parametres

| Parametre | Type | Mode | Description |
|-----------|------|------|-------------|
| CH1 | chaine | Entree | Chaine source |
| CH2 | chaine | Sortie | Chaine inversee |

## Exemples

```text
CH1 = "Bonjour" (l = 7)

Resultat : CH2 = "ruojnoB" (l = 7)
```

```text
CH1 = "12345" (l = 5)

Resultat : CH2 = "54321" (l = 5)
```

```text
CH1 = "radar" (l = 5)

Resultat : CH2 = "radar" (l = 5)  // palindrome !
```

## Schema visuel

```text
CH1:    | B | o | n | j | o | u | r |
index:    1   2   3   4   5   6   7

CH2:    | r | u | o | j | n | o | B |
index:    1   2   3   4   5   6   7

Correspondance : CH2[i] = CH1[N - i + 1]
- CH2[1] = CH1[7] = 'r'
- CH2[2] = CH1[6] = 'u'
- CH2[3] = CH1[5] = 'o'
- ...
```

## Concepts a utiliser

- Parcours inverse d'un tableau
- Formule de correspondance des indices : `N - i + 1`

---

<details>
<summary>Solution</summary>

```text
PROCEDURE MIROIR (CH1: chaine, var CH2: chaine)
VAR i, N : Entier
Debut
    N := CH1.l

    Pour i de 1 a N faire
        CH2.str[i] := CH1.str[N - i + 1]
    Fpour

    CH2.l := N
Fin
```

</details>

<details>
<summary>Solution - Version avec deux indices</summary>

```text
PROCEDURE MIROIR (CH1: chaine, var CH2: chaine)
VAR i, j : Entier
Debut
    j := CH1.l

    Pour i de 1 a CH1.l faire
        CH2.str[i] := CH1.str[j]
        j := j - 1
    Fpour

    CH2.l := CH1.l
Fin
```

</details>

<details>
<summary>Variante - Inversion en place</summary>

Ecrire une procedure `INVERSER (CH)` qui inverse la chaine directement sans utiliser de chaine auxiliaire.

```text
PROCEDURE INVERSER (var CH: chaine)
VAR i, j : Entier
    temp : caractere
Debut
    i := 1
    j := CH.l

    Tantque i < j faire
        // Echanger CH.str[i] et CH.str[j]
        temp := CH.str[i]
        CH.str[i] := CH.str[j]
        CH.str[j] := temp

        i := i + 1
        j := j - 1
    Ftantque
Fin
```

**Avantage** : Pas besoin de chaine supplementaire (economie memoire)

</details>

---

## Application : Test de palindrome

Un **palindrome** est un mot qui se lit de la meme maniere dans les deux sens.

```text
FONCTION EstPalindrome (CH: chaine) : booleen
VAR CH2 : chaine
Debut
    MIROIR(CH, CH2)

    // Comparer CH et CH2
    Retourner SontEgales(CH, CH2)
Fin
```

Ou version optimisee (sans chaine auxiliaire) :

```text
FONCTION EstPalindrome (CH: chaine) : booleen
VAR i, j : Entier
    palindrome : booleen
Debut
    palindrome := vrai
    i := 1
    j := CH.l

    Tantque palindrome ET (i < j) faire
        Si CH.str[i] <> CH.str[j] Alors
            palindrome := faux
        Fsi
        i := i + 1
        j := j - 1
    Ftantque

    Retourner palindrome
Fin
```

Exemples de palindromes :
- "radar"
- "kayak"
- "elle"
- "ici"
- "12321"

---

## Trace d'execution

```text
CH1 = "ALGO" (N = 4)

i=1: CH2.str[1] = CH1.str[4-1+1] = CH1.str[4] = 'O'
i=2: CH2.str[2] = CH1.str[4-2+1] = CH1.str[3] = 'G'
i=3: CH2.str[3] = CH1.str[4-3+1] = CH1.str[2] = 'L'
i=4: CH2.str[4] = CH1.str[4-4+1] = CH1.str[1] = 'A'

CH2.l = 4

Resultat : CH2 = "OGLA"
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-CHAINE-IN     PIC X(50).
01 WS-CHAINE-OUT    PIC X(50).
01 WS-LONGUEUR      PIC 9(2).
01 WS-I             PIC 9(2).
01 WS-J             PIC 9(2).

PROCEDURE DIVISION.
* Inversion caractere par caractere
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-CHAINE-IN))
        TO WS-LONGUEUR

    MOVE WS-LONGUEUR TO WS-J
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LONGUEUR
        MOVE WS-CHAINE-IN(WS-J:1) TO WS-CHAINE-OUT(WS-I:1)
        SUBTRACT 1 FROM WS-J
    END-PERFORM.

* Alternative avec FUNCTION REVERSE (COBOL 2002+)
    MOVE FUNCTION REVERSE(WS-CHAINE-IN) TO WS-CHAINE-OUT.
```

---

## Complexite

- Temps : O(n) - un seul parcours
- Espace : O(n) pour CH2, ou O(1) pour l'inversion en place

---

*Exercice 16 - Chaines de caracteres - Algorithmique Chapitre 02 - M2i*
