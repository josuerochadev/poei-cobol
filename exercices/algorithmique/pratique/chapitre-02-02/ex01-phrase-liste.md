# Exercice 17 : Phrase comme liste chainee

## Enonce

On veut representer une phrase comme une liste chainee de mots. Ecrire les sous-programmes suivants :

1. **Trouver_mot** : Fonction qui cherche un mot dans une phrase
2. **Remplacer_mot** : Procedure qui remplace toutes les occurrences d'un mot par un autre

## Structure de donnees

```text
Type mot = enregistrement
    Texte : chaine
    Next : ^mot
fin enregistrement

Type phrase = ^mot
```

## Schema visuel

```text
phrase --> | "Je"  | --> | "suis" | --> | "la" | --> NIL
              |              |             |
            Texte          Texte        Texte
```

---

## Partie A : Trouver_mot

### Specification

```text
FONCTION Trouver_mot (ph: phrase, M: chaine) : booleen
```

Retourne VRAI si le mot M existe dans la phrase ph, FAUX sinon.

### Exemple

```text
phrase = "Je" -> "suis" -> "la" -> NIL
M = "suis"

Resultat : VRAI
```

```text
phrase = "Je" -> "suis" -> "la" -> NIL
M = "absent"

Resultat : FAUX
```

---

<details>
<summary>Solution Trouver_mot - Version iterative</summary>

```text
FONCTION Trouver_mot (ph: phrase, M: chaine) : booleen
VAR
    courant : ^mot
    trouve : booleen
Debut
    courant := ph
    trouve := faux

    Tantque (courant <> NIL) ET (non trouve) faire
        Si comparer_ch(courant^.Texte, M) Alors
            trouve := vrai
        Sinon
            courant := courant^.Next
        Fsi
    Ftantque

    Retourner trouve
Fin
```

</details>

<details>
<summary>Solution Trouver_mot - Version recursive</summary>

```text
FONCTION Trouver_mot (ph: phrase, M: chaine) : booleen
Debut
    Si ph = NIL Alors
        Retourner faux
    Sinon Si comparer_ch(ph^.Texte, M) Alors
        Retourner vrai
    Sinon
        Retourner Trouver_mot(ph^.Next, M)
    Fsi
Fin
```

</details>

---

## Partie B : Remplacer_mot

### Specification

```text
PROCEDURE Remplacer_mot (var ph: phrase, ancien: chaine, nouveau: chaine)
```

Remplace TOUTES les occurrences du mot `ancien` par le mot `nouveau` dans la phrase.

### Exemple

```text
Avant : phrase = "le" -> "chat" -> "mange" -> "le" -> "poisson" -> NIL
ancien = "le", nouveau = "un"

Apres : phrase = "un" -> "chat" -> "mange" -> "un" -> "poisson" -> NIL
```

---

<details>
<summary>Solution Remplacer_mot</summary>

```text
PROCEDURE Remplacer_mot (var ph: phrase, ancien: chaine, nouveau: chaine)
VAR
    courant : ^mot
Debut
    courant := ph

    Tantque courant <> NIL faire
        Si comparer_ch(courant^.Texte, ancien) Alors
            copier_ch(nouveau, courant^.Texte)
        Fsi
        courant := courant^.Next
    Ftantque
Fin
```

</details>

---

## Fonctions auxiliaires

### comparer_ch : Comparaison de deux chaines

```text
FONCTION comparer_ch (ch1, ch2: chaine) : booleen
VAR
    i : entier
    egales : booleen
Debut
    Si ch1.l <> ch2.l Alors
        Retourner faux
    Fsi

    egales := vrai
    i := 1

    Tantque egales ET (i <= ch1.l) faire
        Si ch1.str[i] <> ch2.str[i] Alors
            egales := faux
        Fsi
        i := i + 1
    Ftantque

    Retourner egales
Fin
```

### copier_ch : Copie d'une chaine

```text
PROCEDURE copier_ch (source: chaine, var dest: chaine)
VAR i : entier
Debut
    Pour i de 1 a source.l faire
        dest.str[i] := source.str[i]
    Fpour
    dest.l := source.l
Fin
```

---

## Exercices supplementaires

### Exercice A : Construire une phrase

Ecrire une procedure pour ajouter un mot a la fin d'une phrase.

```text
PROCEDURE Ajouter_mot (var ph: phrase, M: chaine)
VAR
    nouveau, courant : ^mot
Debut
    // Creer le nouveau maillon
    Creer(nouveau)
    copier_ch(M, nouveau^.Texte)
    nouveau^.Next := NIL

    Si ph = NIL Alors
        ph := nouveau
    Sinon
        // Parcourir jusqu'au dernier element
        courant := ph
        Tantque courant^.Next <> NIL faire
            courant := courant^.Next
        Ftantque
        courant^.Next := nouveau
    Fsi
Fin
```

### Exercice B : Supprimer un mot

Ecrire une procedure pour supprimer la premiere occurrence d'un mot.

```text
PROCEDURE Supprimer_mot (var ph: phrase, M: chaine)
VAR
    courant, precedent : ^mot
    trouve : booleen
Debut
    Si ph = NIL Alors
        Sortir  // Liste vide
    Fsi

    // Cas special : le mot est en tete
    Si comparer_ch(ph^.Texte, M) Alors
        courant := ph
        ph := ph^.Next
        Liberer(courant)
        Sortir
    Fsi

    // Rechercher dans le reste de la liste
    precedent := ph
    courant := ph^.Next
    trouve := faux

    Tantque (courant <> NIL) ET (non trouve) faire
        Si comparer_ch(courant^.Texte, M) Alors
            precedent^.Next := courant^.Next
            Liberer(courant)
            trouve := vrai
        Sinon
            precedent := courant
            courant := courant^.Next
        Fsi
    Ftantque
Fin
```

### Exercice C : Compter les mots

```text
FONCTION Compter_mots (ph: phrase) : entier
VAR
    courant : ^mot
    compteur : entier
Debut
    compteur := 0
    courant := ph

    Tantque courant <> NIL faire
        compteur := compteur + 1
        courant := courant^.Next
    Ftantque

    Retourner compteur
Fin
```

---

## Trace d'execution

```text
Remplacer_mot avec phrase = "a" -> "b" -> "a" -> NIL
ancien = "a", nouveau = "x"

Iteration 1: courant -> "a"
  comparer_ch("a", "a") = vrai
  copier_ch("x", courant^.Texte)
  phrase = "x" -> "b" -> "a" -> NIL
  courant := courant^.Next

Iteration 2: courant -> "b"
  comparer_ch("b", "a") = faux
  courant := courant^.Next

Iteration 3: courant -> "a"
  comparer_ch("a", "a") = vrai
  copier_ch("x", courant^.Texte)
  phrase = "x" -> "b" -> "x" -> NIL
  courant := courant^.Next

Iteration 4: courant = NIL -> Fin

Resultat : phrase = "x" -> "b" -> "x" -> NIL
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-PHRASE.
   05 WS-MOTS OCCURS 100 TIMES.
      10 WS-MOT-TEXTE    PIC X(30).
      10 WS-MOT-SUIVANT  PIC 9(3).
01 WS-NB-MOTS            PIC 9(3).
01 WS-TETE               PIC 9(3).

* Note: En COBOL standard, les listes chainees sont simulees
* avec des tableaux et des indices comme "pointeurs"

PROCEDURE DIVISION.
* Parcours de la liste
    MOVE WS-TETE TO WS-INDEX
    PERFORM UNTIL WS-INDEX = 0
        DISPLAY WS-MOT-TEXTE(WS-INDEX)
        MOVE WS-MOT-SUIVANT(WS-INDEX) TO WS-INDEX
    END-PERFORM.
```

---

## Complexite

| Operation | Complexite |
|-----------|------------|
| Trouver_mot | O(n) - parcours lineaire |
| Remplacer_mot | O(n) - parcours complet |
| Ajouter_mot (fin) | O(n) - parcours jusqu'a la fin |
| Supprimer_mot | O(n) - recherche + suppression |
| Compter_mots | O(n) - parcours complet |

---

*Exercice 17 - Listes chainees - Algorithmique Chapitre 02 - M2i*
