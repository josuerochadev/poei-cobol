# Exercice 01 : Reconnaissance de chaine avec pile

## Enonce

On se propose de reconnaitre les chaines de caracteres definies par la regle suivante :

> Une chaine quelconque **S** suivie du caractere `*`, suivie de la meme chaine **S** inversee.

On suppose que S ne contient pas le caractere `*`.

Ecrire une fonction `chaine_valide` qui verifie si une chaine Ch est valide ou pas, en utilisant une pile.

## Exemple

```text
"ab2c*c2ba" -> VALIDE (S = "ab2c", S inverse = "c2ba")
"abc*cba"   -> VALIDE (S = "abc", S inverse = "cba")
"abc*abc"   -> INVALIDE (abc n'est pas l'inverse de abc)
"abc*cb"    -> INVALIDE (longueurs differentes)
"*"         -> VALIDE (S vide)
```

## Schema visuel

```text
Chaine : "ab2c*c2ba"

Phase 1 : Empiler jusqu'a '*'
         +---+
   Pile: | c |  <- sommet
         | 2 |
         | b |
         | a |
         +---+

Phase 2 : Comparer avec la partie apres '*'
   c = Depiler() = 'c' == Ch[6] = 'c' OK
   2 = Depiler() = '2' == Ch[7] = '2' OK
   b = Depiler() = 'b' == Ch[8] = 'b' OK
   a = Depiler() = 'a' == Ch[9] = 'a' OK
   Pile vide et fin de chaine -> VALIDE
```

## Principe

1. Empiler tous les caracteres avant le `*`
2. Pour chaque caractere apres le `*`, comparer avec le sommet de pile
3. La chaine est valide si :
   - Tous les caracteres correspondent
   - La pile est vide a la fin
   - On a parcouru toute la chaine

---

<details>
<summary>Solution</summary>

```text
FONCTION chaine_valide (Ch : Chaine) : Booleen
VAR
    P : Pile
    i : entier
    valid : Booleen
Debut
    i := 1

    // Phase 1 : Empiler jusqu'au '*'
    Tantque Ch.str[i] <> '*' faire
        Empiler(P, Ch.str[i])
        i := i + 1
    Ftantque

    // Passer le '*'
    i := i + 1

    // Phase 2 : Comparer avec la suite
    valid := vrai

    Tantque (i <= Ch.l) ET (NON pile_vide(P)) ET valid faire
        Si Ch.str[i] = Depiler(P) Alors
            i := i + 1
        Sinon
            valid := faux
        Fsi
    Ftantque

    // Verifier que pile vide ET fin de chaine
    chaine_valide := (valid ET pile_vide(P) ET i > Ch.l)
Fin
```

</details>

---

## Trace d'execution

```text
Ch = "ab*ba" (l = 5)

Phase 1 - Empiler:
  i=1: Ch.str[1]='a' <> '*' -> Empiler(P,'a'), i=2
  i=2: Ch.str[2]='b' <> '*' -> Empiler(P,'b'), i=3
  i=3: Ch.str[3]='*' -> Sortir de la boucle

Pile apres Phase 1: ['a','b'] (b au sommet)

i := 4 (passer le '*')
valid := vrai

Phase 2 - Comparer:
  i=4: Ch.str[4]='b', Depiler(P)='b' -> Egal, i=5
  i=5: Ch.str[5]='a', Depiler(P)='a' -> Egal, i=6

Fin: valid=vrai, pile_vide(P)=vrai, i=6 > l=5
Resultat: VRAI
```

---

## Cas particuliers

### Chaine vide avant '*'

```text
Ch = "*" (l = 1)

Phase 1: i=1, Ch.str[1]='*' -> Boucle non executee
Pile vide

Phase 2: i=2 > l=1 -> Boucle non executee

Resultat: valid=vrai, pile_vide=vrai, i=2 > l=1 -> VRAI
```

### Longueurs differentes

```text
Ch = "abc*cb" (l = 6)

Phase 1: Empile 'a','b','c'
Phase 2:
  i=5: 'c' = Depiler()='c' OK
  i=6: 'b' = Depiler()='b' OK
  i=7 > l=6 -> Sortie

Resultat: pile_vide(P)=faux (reste 'a') -> FAUX
```

---

## Exercices supplementaires

### Variante A : Sans le separateur '*'

Verifier si une chaine est de la forme SS' (S concatene avec son inverse).

```text
FONCTION chaine_miroir (Ch : Chaine) : Booleen
VAR
    P : Pile
    i, m : entier
    valid : Booleen
Debut
    // La chaine doit avoir une longueur paire
    Si Ch.l mod 2 <> 0 Alors
        Retourner faux
    Fsi

    m := Ch.l div 2

    // Empiler la premiere moitie
    Pour i de 1 a m faire
        Empiler(P, Ch.str[i])
    Fpour

    // Comparer avec la seconde moitie
    valid := vrai
    i := m + 1

    Tantque (i <= Ch.l) ET valid faire
        Si Ch.str[i] = Depiler(P) Alors
            i := i + 1
        Sinon
            valid := faux
        Fsi
    Ftantque

    Retourner valid
Fin
```

### Variante B : Verification de parentheses

```text
FONCTION parentheses_valides (Ch : Chaine) : Booleen
VAR
    P : Pile
    i : entier
    valid : Booleen
Debut
    valid := vrai
    i := 1

    Tantque (i <= Ch.l) ET valid faire
        Si Ch.str[i] = '(' Alors
            Empiler(P, '(')
        Sinon Si Ch.str[i] = ')' Alors
            Si pile_vide(P) Alors
                valid := faux
            Sinon
                Depiler(P)
            Fsi
        Fsi
        i := i + 1
    Ftantque

    Retourner valid ET pile_vide(P)
Fin
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-PILE.
   05 WS-PILE-TAB    PIC X(100).
   05 WS-PILE-PTR    PIC 9(3) VALUE 0.
01 WS-CHAINE         PIC X(100).
01 WS-LONGUEUR       PIC 9(3).
01 WS-INDEX          PIC 9(3).
01 WS-VALID          PIC 9 VALUE 1.
01 WS-CAR            PIC X.

PROCEDURE DIVISION.
* Empiler un caractere
EMPILER-PARA.
    ADD 1 TO WS-PILE-PTR
    MOVE WS-CAR TO WS-PILE-TAB(WS-PILE-PTR:1).

* Depiler un caractere
DEPILER-PARA.
    MOVE WS-PILE-TAB(WS-PILE-PTR:1) TO WS-CAR
    SUBTRACT 1 FROM WS-PILE-PTR.

* Test pile vide
PILE-VIDE-PARA.
    IF WS-PILE-PTR = 0
        MOVE 1 TO WS-PILE-VIDE
    ELSE
        MOVE 0 TO WS-PILE-VIDE
    END-IF.
```

---

## Complexite

| Operation | Complexite |
|-----------|------------|
| Phase 1 (empiler) | O(n/2) |
| Phase 2 (comparer) | O(n/2) |
| **Total** | **O(n)** |

Espace : O(n/2) pour la pile

---

*Exercice 01 - Piles et Files - Algorithmique Chapitre 02-03 - M2i*
