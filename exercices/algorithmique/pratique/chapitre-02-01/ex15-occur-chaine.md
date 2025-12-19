# Exercice 15 : Comptage d'occurrences (OCCUR)

## Enonce

Ecrire une procedure `OCCUR (C, CH, N)` qui calcule le nombre d'occurrences d'un caractere C dans une chaine de caracteres CH.

## Parametres

| Parametre | Type | Mode | Description |
|-----------|------|------|-------------|
| C | caractere | Entree | Caractere a compter |
| CH | chaine | Entree | Chaine a analyser |
| N | entier | Sortie | Nombre d'occurrences |

## Exemples

```text
CH = "abracadabra" (l = 11)
C = 'a'

Resultat : N = 5
```

```text
CH = "bonjour" (l = 7)
C = 'o'

Resultat : N = 2
```

```text
CH = "test" (l = 4)
C = 'x'

Resultat : N = 0
```

## Concepts a utiliser

- Pattern de comptage (initialisation a 0, incrementation)
- Parcours complet de la chaine
- Comparaison de caracteres

---

<details>
<summary>Solution</summary>

```text
PROCEDURE OCCUR (C: caractere, CH: chaine, var N: Entier)
VAR i : Entier
Debut
    N := 0
    i := 1

    Tantque (i <= CH.l) faire
        Si CH.str[i] = C Alors
            N := N + 1
        Fsi
        i := i + 1
    Ftantque
Fin
```

</details>

<details>
<summary>Solution - Version avec boucle Pour</summary>

```text
PROCEDURE OCCUR (C: caractere, CH: chaine, var N: Entier)
VAR i : Entier
Debut
    N := 0

    Pour i de 1 a CH.l faire
        Si CH.str[i] = C Alors
            N := N + 1
        Fsi
    Fpour
Fin
```

</details>

<details>
<summary>Variante - Version fonction</summary>

```text
FONCTION OCCUR (C: caractere, CH: chaine) : Entier
VAR i, compteur : Entier
Debut
    compteur := 0

    Pour i de 1 a CH.l faire
        Si CH.str[i] = C Alors
            compteur := compteur + 1
        Fsi
    Fpour

    Retourner compteur
Fin
```

</details>

---

## Application : Test d'anagrammes

La procedure OCCUR est utilisee dans le test d'anagrammes (voir ex06-anagrammes.md).

Deux mots sont des anagrammes s'ils contiennent les memes caracteres avec les memes frequences.

```text
PROCEDURE ANAGRAMME (MOT1, MOT2: chaine, var ANAG: booleen)
VAR
    i: Entier
    N1, N2: Entier
Debut
    ANAG := vrai
    i := 1

    Tantque ANAG et (i <= MOT1.l) faire
        OCCUR (MOT1.str[i], MOT1, N1)
        OCCUR (MOT1.str[i], MOT2, N2)

        Si N1 <> N2 Alors
            ANAG := faux
        Fsi
        i := i + 1
    Ftantque
Fin
```

Exemple :
- "MARIE" et "AIMER" sont des anagrammes
- "AIMERA" et "MARIEE" ne sont PAS des anagrammes

---

## Exercices supplementaires

### Exercice A : Compter les voyelles

```text
FONCTION CompterVoyelles (CH: chaine) : Entier
VAR i, compteur : Entier
Debut
    compteur := 0

    Pour i de 1 a CH.l faire
        Si CH.str[i] = 'a' OU CH.str[i] = 'e' OU
           CH.str[i] = 'i' OU CH.str[i] = 'o' OU
           CH.str[i] = 'u' Alors
            compteur := compteur + 1
        Fsi
    Fpour

    Retourner compteur
Fin
```

### Exercice B : Frequence de tous les caracteres

```text
PROCEDURE Frequences (CH: chaine, var Freq: Tableau[1..26] de Entier)
// Compte la frequence de chaque lettre (a-z)
VAR i, pos : Entier
Debut
    // Initialiser toutes les frequences a 0
    Pour i de 1 a 26 faire
        Freq[i] := 0
    Fpour

    // Compter chaque lettre
    Pour i de 1 a CH.l faire
        pos := Ord(CH.str[i]) - Ord('a') + 1
        Si pos >= 1 ET pos <= 26 Alors
            Freq[pos] := Freq[pos] + 1
        Fsi
    Fpour
Fin
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-CHAINE        PIC X(100).
01 WS-CARACTERE     PIC X.
01 WS-COMPTEUR      PIC 9(3) VALUE 0.

PROCEDURE DIVISION.
* Comptage avec INSPECT TALLYING
    INITIALIZE WS-COMPTEUR
    INSPECT WS-CHAINE TALLYING WS-COMPTEUR
        FOR ALL WS-CARACTERE.

    DISPLAY "Nombre d'occurrences : " WS-COMPTEUR.
```

---

## Complexite

- Temps : O(n) - parcours complet obligatoire
- Espace : O(1) - seulement le compteur

---

*Exercice 15 - Chaines de caracteres - Algorithmique Chapitre 02 - M2i*
