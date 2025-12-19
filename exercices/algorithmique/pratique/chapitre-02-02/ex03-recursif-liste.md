# Exercice 19 : Fonctions recursives sur listes chainees

## Enonce

Implementer les fonctions recursives suivantes sur des listes chainees d'entiers :

1. **plus_courte** : Retourne la plus courte de deux listes
2. **Double** : Double chaque element de la liste
3. **apparait** : Verifie si un element apparait dans la liste
4. **croissante** : Verifie si la liste est triee en ordre croissant

## Structure de donnees

```text
Type Elmt = enregistrement
    Val : entier
    Next : ^Elmt
fin enregistrement

Type Liste = ^Elmt
```

---

## Fonction 1 : plus_courte

### Specification

```text
FONCTION plus_courte (L1, L2: Liste) : Liste
```

Retourne un pointeur vers la liste la plus courte. En cas d'egalite, retourne L1.

### Exemple

```text
L1 = 1 -> 2 -> 3 -> NIL         (longueur 3)
L2 = 4 -> 5 -> NIL              (longueur 2)

Resultat : L2 (pointeur vers la liste la plus courte)
```

### Principe recursif

On avance simultanement dans les deux listes. La premiere qui atteint NIL est la plus courte.

---

<details>
<summary>Solution</summary>

```text
FONCTION plus_courte (L1, L2: Liste) : Liste
Debut
    // Si L1 est vide, L1 est la plus courte (ou egale)
    Si L1 = NIL Alors
        Retourner L1
    Fsi

    // Si L2 est vide, L2 est la plus courte
    Si L2 = NIL Alors
        Retourner L2
    Fsi

    // Sinon, on avance dans les deux listes
    Retourner plus_courte(L1^.Next, L2^.Next)
Fin
```

**Note** : Cette version retourne NIL si les listes sont de meme longueur. Pour retourner L1 en cas d'egalite :

```text
FONCTION plus_courte_v2 (L1, L2: Liste, orig1, orig2: Liste) : Liste
Debut
    Si L1 = NIL Alors
        Retourner orig1
    Fsi

    Si L2 = NIL Alors
        Retourner orig2
    Fsi

    Retourner plus_courte_v2(L1^.Next, L2^.Next, orig1, orig2)
Fin

// Appel : plus_courte_v2(L1, L2, L1, L2)
```

</details>

---

## Fonction 2 : Double

### Specification

```text
PROCEDURE Double (var L: Liste)
```

Multiplie par 2 la valeur de chaque element de la liste.

### Exemple

```text
Avant : L = 1 -> 2 -> 3 -> NIL
Apres : L = 2 -> 4 -> 6 -> NIL
```

---

<details>
<summary>Solution</summary>

```text
PROCEDURE Double (var L: Liste)
Debut
    Si L <> NIL Alors
        L^.Val := L^.Val * 2
        Double(L^.Next)
    Fsi
Fin
```

</details>

<details>
<summary>Variante - Version fonction qui retourne une nouvelle liste</summary>

```text
FONCTION Double_copie (L: Liste) : Liste
VAR nouveau : ^Elmt
Debut
    Si L = NIL Alors
        Retourner NIL
    Fsi

    Creer(nouveau)
    nouveau^.Val := L^.Val * 2
    nouveau^.Next := Double_copie(L^.Next)

    Retourner nouveau
Fin
```

</details>

---

## Fonction 3 : apparait

### Specification

```text
FONCTION apparait (x: entier, L: Liste) : booleen
```

Retourne VRAI si x apparait dans la liste L, FAUX sinon.

### Exemple

```text
L = 1 -> 5 -> 3 -> 7 -> NIL

apparait(5, L) = VRAI
apparait(4, L) = FAUX
```

---

<details>
<summary>Solution</summary>

```text
FONCTION apparait (x: entier, L: Liste) : booleen
Debut
    Si L = NIL Alors
        Retourner faux
    Sinon Si L^.Val = x Alors
        Retourner vrai
    Sinon
        Retourner apparait(x, L^.Next)
    Fsi
Fin
```

</details>

---

## Fonction 4 : croissante

### Specification

```text
FONCTION croissante (L: Liste) : booleen
```

Retourne VRAI si la liste est triee en ordre croissant (ou egale), FAUX sinon.

### Exemple

```text
L1 = 1 -> 3 -> 5 -> 7 -> NIL
croissante(L1) = VRAI

L2 = 1 -> 3 -> 2 -> 7 -> NIL
croissante(L2) = FAUX (3 > 2)

L3 = NIL
croissante(L3) = VRAI (liste vide est triee)

L4 = 5 -> NIL
croissante(L4) = VRAI (un seul element)
```

---

<details>
<summary>Solution</summary>

```text
FONCTION croissante (L: Liste) : booleen
Debut
    // Liste vide ou un seul element : triee par definition
    Si (L = NIL) OU (L^.Next = NIL) Alors
        Retourner vrai
    Fsi

    // Verifier que l'element courant <= suivant
    Si L^.Val > L^.Next^.Val Alors
        Retourner faux
    Sinon
        Retourner croissante(L^.Next)
    Fsi
Fin
```

</details>

<details>
<summary>Variante - Ordre strictement croissant</summary>

```text
FONCTION strictement_croissante (L: Liste) : booleen
Debut
    Si (L = NIL) OU (L^.Next = NIL) Alors
        Retourner vrai
    Fsi

    // Strictement : pas d'egalite
    Si L^.Val >= L^.Next^.Val Alors
        Retourner faux
    Sinon
        Retourner strictement_croissante(L^.Next)
    Fsi
Fin
```

</details>

---

## Exercices supplementaires

### Exercice A : Longueur recursive

```text
FONCTION longueur (L: Liste) : entier
Debut
    Si L = NIL Alors
        Retourner 0
    Sinon
        Retourner 1 + longueur(L^.Next)
    Fsi
Fin
```

### Exercice B : Somme des elements

```text
FONCTION somme (L: Liste) : entier
Debut
    Si L = NIL Alors
        Retourner 0
    Sinon
        Retourner L^.Val + somme(L^.Next)
    Fsi
Fin
```

### Exercice C : Maximum

```text
FONCTION maximum (L: Liste) : entier
VAR max_reste : entier
Debut
    Si L = NIL Alors
        Retourner -infini  // ou erreur
    Fsi

    Si L^.Next = NIL Alors
        Retourner L^.Val
    Fsi

    max_reste := maximum(L^.Next)

    Si L^.Val > max_reste Alors
        Retourner L^.Val
    Sinon
        Retourner max_reste
    Fsi
Fin
```

### Exercice D : Inverser une liste (recursif)

```text
FONCTION inverser (L: Liste) : Liste
VAR reste_inverse : Liste
Debut
    Si (L = NIL) OU (L^.Next = NIL) Alors
        Retourner L
    Fsi

    reste_inverse := inverser(L^.Next)

    // L^.Next pointe maintenant vers le dernier element du reste inverse
    // On fait pointer ce dernier element vers L
    L^.Next^.Next := L
    L^.Next := NIL

    Retourner reste_inverse
Fin
```

### Exercice E : Compter occurrences

```text
FONCTION compter (x: entier, L: Liste) : entier
Debut
    Si L = NIL Alors
        Retourner 0
    Sinon Si L^.Val = x Alors
        Retourner 1 + compter(x, L^.Next)
    Sinon
        Retourner compter(x, L^.Next)
    Fsi
Fin
```

---

## Traces d'execution

### Trace croissante

```text
croissante(1 -> 3 -> 5 -> NIL)

Appel 1: L = 1 -> 3 -> 5 -> NIL
  L^.Val (1) <= L^.Next^.Val (3) ? OUI
  -> croissante(3 -> 5 -> NIL)

Appel 2: L = 3 -> 5 -> NIL
  L^.Val (3) <= L^.Next^.Val (5) ? OUI
  -> croissante(5 -> NIL)

Appel 3: L = 5 -> NIL
  L^.Next = NIL -> Retourner VRAI

Resultat final : VRAI
```

### Trace Double

```text
Double(1 -> 2 -> 3 -> NIL)

Appel 1: L = 1 -> 2 -> 3 -> NIL
  L^.Val := 1 * 2 = 2
  -> Double(2 -> 3 -> NIL)

Appel 2: L = 2 -> 3 -> NIL
  L^.Val := 2 * 2 = 4
  -> Double(3 -> NIL)

Appel 3: L = 3 -> NIL
  L^.Val := 3 * 2 = 6
  -> Double(NIL)

Appel 4: L = NIL
  (rien a faire)

Resultat : L = 2 -> 4 -> 6 -> NIL
```

---

## Recursivite : Concepts cles

### Structure d'une fonction recursive

```text
FONCTION recursive (L: Liste) : TypeRetour
Debut
    // 1. CAS DE BASE : condition d'arret
    Si L = NIL Alors
        Retourner valeur_base
    Fsi

    // 2. CAS RECURSIF : traitement + appel recursif
    Traiter(L^.Val)
    Retourner combiner(L^.Val, recursive(L^.Next))
Fin
```

### Types de recursivite

| Type | Description | Exemple |
|------|-------------|---------|
| Terminale | L'appel recursif est la derniere operation | `apparait` |
| Non-terminale | Des operations suivent l'appel recursif | `somme` |

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
* COBOL ne supporte pas nativement la recursivite
* On simule avec une pile ou une boucle

01 WS-PILE.
   05 WS-PILE-NIVEAU OCCURS 100 TIMES.
      10 WS-PILE-INDEX  PIC 9(3).
      10 WS-PILE-VALEUR PIC 9(5).
01 WS-PILE-PTR         PIC 9(3).

PROCEDURE DIVISION.
* Simulation de longueur recursive avec boucle
LONGUEUR-PARA.
    MOVE 0 TO WS-COMPTEUR
    MOVE WS-TETE TO WS-INDEX
    PERFORM UNTIL WS-INDEX = 0
        ADD 1 TO WS-COMPTEUR
        MOVE WS-SUIVANT(WS-INDEX) TO WS-INDEX
    END-PERFORM.

* COBOL 2002+ supporte la recursivite avec RECURSIVE
* IDENTIFICATION DIVISION.
* PROGRAM-ID. LONGUEUR RECURSIVE.
```

---

## Complexite

| Fonction | Temps | Espace (pile) |
|----------|-------|---------------|
| plus_courte | O(min(n,m)) | O(min(n,m)) |
| Double | O(n) | O(n) |
| apparait | O(n) | O(n) |
| croissante | O(n) | O(n) |
| longueur | O(n) | O(n) |
| somme | O(n) | O(n) |
| inverser | O(n) | O(n) |

**Note** : L'espace O(n) pour la pile d'appels peut etre problematique pour de tres longues listes (risque de stack overflow).

---

*Exercice 19 - Listes chainees - Algorithmique Chapitre 02 - M2i*
