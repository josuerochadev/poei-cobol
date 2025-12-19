# Exercice 08 : Diviser pour regner

## Introduction

Le paradigme "Diviser pour regner" consiste a :
1. **Diviser** le probleme en sous-problemes plus petits
2. **Regner** : resoudre les sous-problemes recursivement
3. **Combiner** les solutions des sous-problemes

---

## Partie 1 : Recherche dichotomique

### Enonce

Ecrire une fonction recursive `Recherche_Dicho` qui recherche un element X dans un tableau trie T.

### Principe

- Comparer X avec l'element du milieu
- Si egal, element trouve
- Si X < milieu, chercher dans la moitie gauche
- Si X > milieu, chercher dans la moitie droite

### Schema visuel

```text
Rechercher X = 7 dans T = [1, 3, 5, 7, 9, 11, 13]

Etape 1 : milieu = 7
     1    2    3    4    5    6    7
   +----+----+----+----+----+----+----+
   | 1  | 3  | 5  | 7  | 9  | 11 | 13 |
   +----+----+----+----+----+----+----+
     d              ^              f
                 milieu=4
                 T[4]=7 = X

TROUVE a l'indice 4 !
```

```text
Rechercher X = 5 dans T = [1, 3, 5, 7, 9, 11, 13]

Etape 1 : milieu = 4, T[4] = 7 > 5 -> chercher a gauche
     1    2    3    4    5    6    7
   +----+----+----+----+----+----+----+
   | 1  | 3  | 5  | 7  | 9  | 11 | 13 |
   +----+----+----+----+----+----+----+
     d         f

Etape 2 : milieu = 2, T[2] = 3 < 5 -> chercher a droite
     1    2    3
   +----+----+----+
   | 1  | 3  | 5  |
   +----+----+----+
              d=f

Etape 3 : milieu = 3, T[3] = 5 = X
TROUVE a l'indice 3 !
```

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Recherche_Dicho (T : Tableau; d, f, X : Entier) : Entier
VAR milieu : Entier
DEBUT
    Si d > f alors
        // Element non trouve
        Recherche_Dicho := 0
    Sinon
        milieu := (d + f) DIV 2

        Si T[milieu] = X alors
            Recherche_Dicho := milieu
        Sinon Si X < T[milieu] alors
            // Chercher dans la moitie gauche
            Recherche_Dicho := Recherche_Dicho(T, d, milieu - 1, X)
        Sinon
            // Chercher dans la moitie droite
            Recherche_Dicho := Recherche_Dicho(T, milieu + 1, f, X)
        Fsi
    Fsi
FIN
```

</details>

### Trace d'execution

```text
T = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
Rechercher X = 23

Recherche_Dicho(T, 1, 10, 23)
  milieu = 5, T[5] = 16 < 23 -> droite
  Recherche_Dicho(T, 6, 10, 23)
    milieu = 8, T[8] = 56 > 23 -> gauche
    Recherche_Dicho(T, 6, 7, 23)
      milieu = 6, T[6] = 23 = X -> TROUVE

Resultat : indice 6
Nombre de comparaisons : 3 (au lieu de 6 en lineaire)
```

---

## Partie 2 : Minimum et Maximum simultanement

### Enonce

Ecrire une fonction recursive qui trouve simultanement le minimum et le maximum d'un tableau en minimisant le nombre de comparaisons.

### Principe naif

```text
Trouver min : n-1 comparaisons
Trouver max : n-1 comparaisons
Total : 2(n-1) comparaisons
```

### Principe optimise (diviser pour regner)

```text
1. Diviser le tableau en deux moities
2. Trouver min/max de chaque moitie recursivement
3. Combiner : min = min(min_gauche, min_droite)
              max = max(max_gauche, max_droite)

Nombre de comparaisons : environ 3n/2 - 2
```

### Solution

<details>
<summary>Solution</summary>

```text
TYPE Couple = enregistrement
    min : Entier
    max : Entier
fin enregistrement

FONCTION MinMax (T : Tableau; d, f : Entier) : Couple
VAR gauche, droite : Couple
    milieu : Entier
    resultat : Couple
DEBUT
    Si d = f alors
        // Un seul element
        resultat.min := T[d]
        resultat.max := T[d]
    Sinon Si d + 1 = f alors
        // Deux elements
        Si T[d] < T[f] alors
            resultat.min := T[d]
            resultat.max := T[f]
        Sinon
            resultat.min := T[f]
            resultat.max := T[d]
        Fsi
    Sinon
        // Plus de deux elements : diviser
        milieu := (d + f) DIV 2
        gauche := MinMax(T, d, milieu)
        droite := MinMax(T, milieu + 1, f)

        // Combiner
        Si gauche.min < droite.min alors
            resultat.min := gauche.min
        Sinon
            resultat.min := droite.min
        Fsi

        Si gauche.max > droite.max alors
            resultat.max := gauche.max
        Sinon
            resultat.max := droite.max
        Fsi
    Fsi

    MinMax := resultat
FIN
```

</details>

### Schema d'execution

```text
T = [3, 1, 4, 1, 5, 9, 2, 6]

                    MinMax(1, 8)
                   /            \
          MinMax(1, 4)      MinMax(5, 8)
          /        \          /        \
   MinMax(1,2) MinMax(3,4) MinMax(5,6) MinMax(7,8)
      |           |           |           |
   (1,3)       (1,4)       (5,9)       (2,6)

Combiner :
  MinMax(1,4) : min=min(1,1)=1, max=max(3,4)=4 -> (1,4)
  MinMax(5,8) : min=min(5,2)=2, max=max(9,6)=9 -> (2,9)
  MinMax(1,8) : min=min(1,2)=1, max=max(4,9)=9 -> (1,9)

Resultat : min=1, max=9
```

---

## Partie 3 : Compter les elements noirs

### Enonce

Etant donne un tableau T d'elements et une fonction `Couleur(x)` qui retourne "noir" ou "blanc", ecrire une fonction recursive qui compte le nombre d'elements noirs.

### Solution lineaire

<details>
<summary>Solution</summary>

```text
FONCTION Nb_Noir (T : Tableau; d, f : Entier) : Entier
DEBUT
    Si d > f alors
        Nb_Noir := 0
    Sinon
        Si Couleur(T[d]) = "noir" alors
            Nb_Noir := 1 + Nb_Noir(T, d + 1, f)
        Sinon
            Nb_Noir := Nb_Noir(T, d + 1, f)
        Fsi
    Fsi
FIN
```

</details>

### Solution diviser pour regner

<details>
<summary>Solution</summary>

```text
FONCTION Nb_Noir_Dicho (T : Tableau; d, f : Entier) : Entier
VAR milieu : Entier
DEBUT
    Si d > f alors
        Nb_Noir_Dicho := 0
    Sinon Si d = f alors
        Si Couleur(T[d]) = "noir" alors
            Nb_Noir_Dicho := 1
        Sinon
            Nb_Noir_Dicho := 0
        Fsi
    Sinon
        milieu := (d + f) DIV 2
        Nb_Noir_Dicho := Nb_Noir_Dicho(T, d, milieu) +
                         Nb_Noir_Dicho(T, milieu + 1, f)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
T = [N, B, N, N, B, N, B, B]  (N=noir, B=blanc)

                    Nb_Noir(1, 8)
                   /            \
          Nb_Noir(1, 4)    Nb_Noir(5, 8)
          /        \          /        \
   Nb_Noir(1,2) Nb_Noir(3,4) Nb_Noir(5,6) Nb_Noir(7,8)
      |           |           |           |
     1+0=1       1+1=2       0+1=1       0+0=0

Combiner :
  Nb_Noir(1,4) = 1 + 2 = 3
  Nb_Noir(5,8) = 1 + 0 = 1
  Nb_Noir(1,8) = 3 + 1 = 4

Resultat : 4 elements noirs
```

---

## Partie 4 : Tri fusion (Merge Sort)

### Enonce

Implementer le tri fusion de maniere recursive.

### Principe

1. Diviser le tableau en deux moities
2. Trier chaque moitie recursivement
3. Fusionner les deux moities triees

### Solution

<details>
<summary>Solution</summary>

```text
PROCEDURE Tri_Fusion (VAR T : Tableau; d, f : Entier)
VAR milieu : Entier
DEBUT
    Si d < f alors
        milieu := (d + f) DIV 2

        // Trier les deux moities
        Tri_Fusion(T, d, milieu)
        Tri_Fusion(T, milieu + 1, f)

        // Fusionner
        Fusionner(T, d, milieu, f)
    Fsi
FIN

PROCEDURE Fusionner (VAR T : Tableau; d, m, f : Entier)
VAR Temp : Tableau
    i, j, k : Entier
DEBUT
    i := d
    j := m + 1
    k := d

    // Fusionner les deux parties
    Tantque i <= m ET j <= f faire
        Si T[i] <= T[j] alors
            Temp[k] := T[i]
            i := i + 1
        Sinon
            Temp[k] := T[j]
            j := j + 1
        Fsi
        k := k + 1
    Ftantque

    // Copier les elements restants
    Tantque i <= m faire
        Temp[k] := T[i]
        i := i + 1
        k := k + 1
    Ftantque

    Tantque j <= f faire
        Temp[k] := T[j]
        j := j + 1
        k := k + 1
    Ftantque

    // Recopier dans T
    Pour k de d a f faire
        T[k] := Temp[k]
    Fpour
FIN
```

</details>

### Schema d'execution

```text
T = [38, 27, 43, 3, 9, 82, 10]

                [38, 27, 43, 3, 9, 82, 10]
               /                          \
       [38, 27, 43, 3]              [9, 82, 10]
       /            \                /        \
   [38, 27]      [43, 3]        [9, 82]      [10]
   /      \      /     \        /     \        |
 [38]    [27]  [43]   [3]    [9]    [82]     [10]

Fusion (bottom-up) :
 [27, 38]    [3, 43]      [9, 82]      [10]
       \      /                \        /
    [3, 27, 38, 43]          [9, 10, 82]
              \                  /
         [3, 9, 10, 27, 38, 43, 82]
```

---

## Partie 5 : Recherche du k-ieme plus petit element

### Enonce

Trouver le k-ieme plus petit element d'un tableau non trie.

### Solution (partition style QuickSelect)

<details>
<summary>Solution</summary>

```text
FONCTION Kieme_Plus_Petit (T : Tableau; d, f, k : Entier) : Entier
VAR pivot_pos : Entier
DEBUT
    pivot_pos := Partitionner(T, d, f)

    Si pivot_pos = k alors
        Kieme_Plus_Petit := T[pivot_pos]
    Sinon Si k < pivot_pos alors
        Kieme_Plus_Petit := Kieme_Plus_Petit(T, d, pivot_pos - 1, k)
    Sinon
        Kieme_Plus_Petit := Kieme_Plus_Petit(T, pivot_pos + 1, f, k)
    Fsi
FIN

FONCTION Partitionner (VAR T : Tableau; d, f : Entier) : Entier
VAR pivot, i, j : Entier
DEBUT
    pivot := T[f]
    i := d - 1

    Pour j de d a f - 1 faire
        Si T[j] <= pivot alors
            i := i + 1
            Permuter(T, i, j)
        Fsi
    Fpour

    Permuter(T, i + 1, f)
    Partitionner := i + 1
FIN
```

</details>

---

## Comparaison des complexites

| Algorithme | Temps moyen | Temps pire cas | Espace |
|------------|-------------|----------------|--------|
| Recherche lineaire | O(n) | O(n) | O(1) |
| Recherche dichotomique | O(log n) | O(log n) | O(log n) |
| MinMax naif | O(2n) | O(2n) | O(1) |
| MinMax diviser | O(3n/2) | O(3n/2) | O(log n) |
| Tri fusion | O(n log n) | O(n log n) | O(n) |
| QuickSelect | O(n) | O(n^2) | O(log n) |

---

## Schema general "Diviser pour regner"

```text
FONCTION DiviserPourRegner (probleme P)
DEBUT
    Si P est trivial alors
        Retourner solution directe
    Sinon
        Diviser P en sous-problemes P1, P2, ...
        S1 := DiviserPourRegner(P1)
        S2 := DiviserPourRegner(P2)
        ...
        Retourner Combiner(S1, S2, ...)
    Fsi
FIN
```

---

## Points cles

1. **Division** : Couper le probleme en parties egales (si possible)
2. **Cas de base** : Un ou deux elements
3. **Combinaison** : Souvent la partie la plus complexe
4. **Profondeur** : O(log n) pour une division par 2
5. **Equilibrage** : Important pour garantir O(log n) divisions

---

*Exercice 08 - Recursivite - Algorithmique Chapitre 03 - M2i*
