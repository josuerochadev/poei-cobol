# Exercice 02 : Palindrome avec pile

## Enonce

Un **palindrome** est un mot qui se lit de la meme facon de gauche a droite et de droite a gauche.

Ecrire une fonction `Palindrome` qui verifie si un mot donne est un palindrome ou non, en utilisant une pile.

## Exemples

```text
"RADAR"  -> VRAI (palindrome)
"ELLE"   -> VRAI (palindrome)
"1991"   -> VRAI (palindrome)
"ETE"    -> VRAI (palindrome)
"KAYAK"  -> VRAI (palindrome)
"BONJOUR"-> FAUX (pas un palindrome)
```

## Schema visuel

```text
Mot : "RADAR" (longueur 5)

Version 1 - Empiler tout :
+---+
| R |  <- sommet
| A |
| D |
| A |
| R |
+---+

Comparaison:
R == Depiler() = R  OK
A == Depiler() = A  OK
D == Depiler() = D  OK
A == Depiler() = A  OK
R == Depiler() = R  OK
-> PALINDROME
```

---

## Version 1 : Empiler toute la chaine

### Principe

1. Empiler tous les caracteres de la chaine
2. Parcourir la chaine du debut en comparant avec les elements depiles
3. Si tous les caracteres correspondent, c'est un palindrome

<details>
<summary>Solution Version 1</summary>

```text
FONCTION Palindrome (Ch : Chaine) : Booleen
VAR
    P : Pile
    i : entier
    valid : Booleen
Debut
    // Empiler tous les caracteres
    Pour i de 1 a Ch.l faire
        Empiler(P, Ch.str[i])
    Fpour

    // Comparer
    i := 1
    valid := vrai

    Tantque (i <= Ch.l) ET valid faire
        Si Ch.str[i] = Depiler(P) Alors
            i := i + 1
        Sinon
            valid := faux
        Fsi
    Ftantque

    Palindrome := valid
Fin
```

</details>

---

## Version 2 : Empiler la moitie (optimisee)

### Principe

1. Empiler seulement la premiere moitie
2. Si longueur impaire, sauter le caractere du milieu
3. Comparer la seconde moitie avec les elements depiles

### Schema pour longueur impaire

```text
Mot : "RADAR" (longueur 5, m = 5 div 2 = 2)

Empiler indices 1 et 2: R, A
         +---+
   Pile: | A |  <- sommet
         | R |
         +---+

Sauter indice 3 (le 'D' du milieu)

Comparer indices 4 et 5:
  Ch.str[4]='A' == Depiler()='A'  OK
  Ch.str[5]='R' == Depiler()='R'  OK
-> PALINDROME
```

### Schema pour longueur paire

```text
Mot : "ELLE" (longueur 4, m = 4 div 2 = 2)

Empiler indices 1 et 2: E, L
         +---+
   Pile: | L |  <- sommet
         | E |
         +---+

Pas de caractere a sauter (longueur paire)

Comparer indices 3 et 4:
  Ch.str[3]='L' == Depiler()='L'  OK
  Ch.str[4]='E' == Depiler()='E'  OK
-> PALINDROME
```

<details>
<summary>Solution Version 2</summary>

```text
FONCTION Palindrome (Ch : Chaine) : Booleen
VAR
    P : Pile
    i, m : entier
    valid : Booleen
Debut
    m := Ch.l div 2

    // Empiler la premiere moitie
    Pour i de 1 a m faire
        Empiler(P, Ch.str[i])
    Fpour

    // Position de depart pour la comparaison
    i := m + 1

    // Si longueur impaire, sauter le caractere du milieu
    Si Ch.l mod 2 <> 0 Alors
        i := i + 1
    Fsi

    // Comparer la seconde moitie
    valid := vrai

    Tantque (i <= Ch.l) ET valid faire
        Si Ch.str[i] = Depiler(P) Alors
            i := i + 1
        Sinon
            valid := faux
        Fsi
    Ftantque

    Palindrome := valid
Fin
```

</details>

---

## Trace d'execution (Version 2)

### Cas longueur impaire

```text
Ch = "KAYAK" (l = 5)
m = 5 div 2 = 2

Empiler:
  i=1: Empiler('K')
  i=2: Empiler('A')
Pile: ['K','A'] (A au sommet)

Position: i = 2 + 1 = 3
Longueur impaire (5 mod 2 = 1): i = 3 + 1 = 4

Comparer:
  i=4: 'A' == Depiler()='A' OK, i=5
  i=5: 'K' == Depiler()='K' OK, i=6

Resultat: VRAI
```

### Cas longueur paire

```text
Ch = "ABBA" (l = 4)
m = 4 div 2 = 2

Empiler:
  i=1: Empiler('A')
  i=2: Empiler('B')
Pile: ['A','B'] (B au sommet)

Position: i = 2 + 1 = 3
Longueur paire (4 mod 2 = 0): pas de saut

Comparer:
  i=3: 'B' == Depiler()='B' OK, i=4
  i=4: 'A' == Depiler()='A' OK, i=5

Resultat: VRAI
```

### Cas non-palindrome

```text
Ch = "ABC" (l = 3)
m = 3 div 2 = 1

Empiler:
  i=1: Empiler('A')
Pile: ['A']

Position: i = 1 + 1 = 2
Longueur impaire: i = 2 + 1 = 3

Comparer:
  i=3: 'C' == Depiler()='A' FAUX

Resultat: FAUX
```

---

## Comparaison des versions

| Critere | Version 1 | Version 2 |
|---------|-----------|-----------|
| Espace pile | O(n) | O(n/2) |
| Temps | O(n) | O(n) |
| Complexite code | Simple | Un peu plus complexe |
| Comparaisons | n | n/2 |

---

## Exercices supplementaires

### Variante A : Ignorer la casse

```text
FONCTION PalindromeIgnoreCase (Ch : Chaine) : Booleen
// Considere 'A' et 'a' comme identiques
VAR
    P : Pile
    i, m : entier
    valid : Booleen
    c1, c2 : caractere
Debut
    m := Ch.l div 2

    Pour i de 1 a m faire
        Empiler(P, Majuscule(Ch.str[i]))
    Fpour

    i := m + 1
    Si Ch.l mod 2 <> 0 Alors
        i := i + 1
    Fsi

    valid := vrai
    Tantque (i <= Ch.l) ET valid faire
        c1 := Majuscule(Ch.str[i])
        c2 := Depiler(P)
        Si c1 = c2 Alors
            i := i + 1
        Sinon
            valid := faux
        Fsi
    Ftantque

    Retourner valid
Fin
```

### Variante B : Ignorer les espaces

```text
FONCTION PalindromeSansEspaces (Ch : Chaine) : Booleen
// "A B C C B A" est considere comme palindrome
// Pretraiter la chaine en supprimant les espaces d'abord
```

### Variante C : Phrase palindrome

```text
"Esope reste ici et se repose" (sans espaces ni accents)
-> "esopresteicietsereposes" -> presque palindrome!

"A man a plan a canal Panama"
-> "amanaplanacanalpanama" -> PALINDROME!
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-MOT            PIC X(50).
01 WS-LONGUEUR       PIC 9(2).
01 WS-MOITIE         PIC 9(2).
01 WS-INDEX          PIC 9(2).
01 WS-PILE           PIC X(50).
01 WS-PILE-PTR       PIC 9(2) VALUE 0.
01 WS-PALINDROME     PIC 9 VALUE 1.
01 WS-CAR-PILE       PIC X.
01 WS-CAR-MOT        PIC X.

PROCEDURE DIVISION.
VERIFIER-PALINDROME.
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-MOT))
        TO WS-LONGUEUR
    DIVIDE WS-LONGUEUR BY 2 GIVING WS-MOITIE

    PERFORM VARYING WS-INDEX FROM 1 BY 1
        UNTIL WS-INDEX > WS-MOITIE
        ADD 1 TO WS-PILE-PTR
        MOVE WS-MOT(WS-INDEX:1) TO WS-PILE(WS-PILE-PTR:1)
    END-PERFORM

    COMPUTE WS-INDEX = WS-MOITIE + 1
    IF FUNCTION MOD(WS-LONGUEUR, 2) NOT = 0
        ADD 1 TO WS-INDEX
    END-IF

    PERFORM UNTIL WS-INDEX > WS-LONGUEUR
        OR WS-PALINDROME = 0
        MOVE WS-PILE(WS-PILE-PTR:1) TO WS-CAR-PILE
        SUBTRACT 1 FROM WS-PILE-PTR
        MOVE WS-MOT(WS-INDEX:1) TO WS-CAR-MOT
        IF WS-CAR-MOT NOT = WS-CAR-PILE
            MOVE 0 TO WS-PALINDROME
        END-IF
        ADD 1 TO WS-INDEX
    END-PERFORM.
```

---

## Complexite

| Version | Temps | Espace |
|---------|-------|--------|
| Version 1 | O(n) | O(n) |
| Version 2 | O(n) | O(n/2) |

---

*Exercice 02 - Piles et Files - Algorithmique Chapitre 02-03 - M2i*
