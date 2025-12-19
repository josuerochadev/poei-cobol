# Exercice 01 : Operations recursives sur les ensembles

## Enonce

On voudrait developper quelques operations sur les ensembles d'entiers. Un ensemble est represente par un tableau :

```text
Type Ensemble = Tableau [1..Max] de Entier
```

## Schema visuel

```text
Ensemble T contenant N elements :

     1    2    3    4    5    6    7    8    9   10
   +----+----+----+----+----+----+----+----+----+----+
 T |  x |  x |  x |  x |    |    |  x |    |  x |    |
   +----+----+----+----+----+----+----+----+----+----+
     d                                            f
```

---

## Partie 1 : Nombre d'occurrences

Ecrire une fonction recursive `Nbr_X` qui calcule le nombre d'occurrences d'un entier x dans un ensemble T contenant N entiers.

### Principe

- Isoler le dernier element
- Si egal a x, ajouter 1 au resultat recursif
- Sinon, retourner simplement le resultat recursif

<details>
<summary>Solution</summary>

```text
FONCTION Nbr_X (T : Tab; d,f : Entier; x : Entier) : Entier
DEBUT
    Si d > f alors
        Nbr_X := 0
    Sinon
        Si T[f] = x alors
            Nbr_X := 1 + Nbr_X(T, d, f-1, x)
        Sinon
            Nbr_X := Nbr_X(T, d, f-1, x)
        Fsi
    Fsi
FIN
```

</details>

### Trace d'execution

```text
T = [3, 5, 3, 7, 3]  x = 3

Nbr_X(T, 1, 5, 3)
  T[5]=3 = x -> 1 + Nbr_X(T, 1, 4, 3)
    T[4]=7 <> x -> Nbr_X(T, 1, 3, 3)
      T[3]=3 = x -> 1 + Nbr_X(T, 1, 2, 3)
        T[2]=5 <> x -> Nbr_X(T, 1, 1, 3)
          T[1]=3 = x -> 1 + Nbr_X(T, 1, 0, 3)
            d > f -> 0
          = 1 + 0 = 1
        = 1
      = 1 + 1 = 2
    = 2
  = 1 + 2 = 3

Resultat : 3 occurrences
```

---

## Partie 2 : Inverser un ensemble

Ecrire une procedure recursive `Inverser` qui inverse un ensemble E contenant N entiers.

### Exemple

```text
E = {4, 7, 4, 9, 8, 2}  ->  E = {2, 8, 9, 4, 7, 4}
```

### Schema d'execution

```text
     1    2    3    4    5    6
    d|                        |f
   +----+----+----+----+----+----+
   | 4  | 7  | 4  | 9  | 8  | 2  |   Permuter(T, 1, 6)
   +----+----+----+----+----+----+

        d|              |f
   +----+----+----+----+----+----+
   | 2  | 7  | 4  | 9  | 8  | 4  |   Permuter(T, 2, 5)
   +----+----+----+----+----+----+

            d|    |f
   +----+----+----+----+----+----+
   | 2  | 8  | 4  | 9  | 7  | 4  |   Permuter(T, 3, 4)
   +----+----+----+----+----+----+

             f|  |d
   +----+----+----+----+----+----+
   | 2  | 8  | 9  | 4  | 7  | 4  |   f < d -> Arret
   +----+----+----+----+----+----+
```

<details>
<summary>Solution</summary>

```text
PROCEDURE Inverser (VAR T : Tab; d,f : Entier)
DEBUT
    Si f > d alors
        Permuter(T, d, f)
        Inverser(T, d+1, f-1)
    Fsi
FIN

PROCEDURE Permuter (VAR T : Tab; i,j : Entier)
VAR temp : Entier
DEBUT
    temp := T[i]
    T[i] := T[j]
    T[j] := temp
FIN
```

</details>

---

## Partie 3 : Incrementer les elements

Ecrire une procedure recursive `Increment` qui transforme chaque element de E en l'augmentant de 1.

### Exemple

```text
E = {3, 4, 5}  ->  E = {4, 5, 6}
```

### Schema d'execution

```text
     1    2    3    4    5    6
     |
   +----+----+----+----+----+----+
   | 3  | 4  | 5  | 5  | 7  | 8  |   E[1] := E[1] + 1
   +----+----+----+----+----+----+

          |
   +----+----+----+----+----+----+
   | 4  | 4  | 5  | 5  | 7  | 8  |   E[2] := E[2] + 1
   +----+----+----+----+----+----+

               |
   +----+----+----+----+----+----+
   | 4  | 5  | 5  | 5  | 7  | 8  |   E[3] := E[3] + 1
   +----+----+----+----+----+----+
   ...
```

<details>
<summary>Solution</summary>

```text
PROCEDURE Increment (VAR E : Ensemble; d,f : Entier)
DEBUT
    Si d <= f alors
        E[d] := E[d] + 1
        Increment(E, d+1, f)
    Fsi
FIN
```

</details>

---

## Partie 4 : Rang du Dernier Positif (RDP)

Ecrire une fonction recursive `RDP` qui calcule le rang du dernier element strictement positif (>0) dans un ensemble.

Si l'ensemble ne contient aucun element positif, la fonction retournera 0.

### Exemple

```text
E = {1, 28, -2, 5, 4, -9, 0, 7, -2, 0}
RDP(E) = 8  (8 est le rang de l'element 7, dernier positif)
```

### Version 1 : Isoler le dernier element

<details>
<summary>Solution Version 1</summary>

```text
FONCTION RDP_der (E : Ensemble; d,f : Entier) : Entier
DEBUT
    Si d <= f alors
        Si E[f] > 0 alors
            RDP_der := f
        Sinon
            RDP_der := RDP_der(E, d, f-1)
        Fsi
    Sinon
        RDP_der := 0
    Fsi
FIN
```

</details>

### Trace d'execution (Version 1)

```text
E = {1, 9, -2, 5, 4, -9, 0, 7, -2, 0}

RDP_der(E, 1, 10)
  E[10]=0 <= 0 -> RDP_der(E, 1, 9)
    E[9]=-2 <= 0 -> RDP_der(E, 1, 8)
      E[8]=7 > 0 -> Retourner 8

Resultat : 8
```

### Version 2 : Isoler le premier element

<details>
<summary>Solution Version 2</summary>

```text
FONCTION RDP_prem (E : Ensemble; d,f : Entier) : Entier
VAR resultat : Entier
DEBUT
    Si d <= f alors
        resultat := RDP_prem(E, d+1, f)
        Si resultat = 0 alors
            Si E[d] > 0 alors
                RDP_prem := d
            Sinon
                RDP_prem := 0
            Fsi
        Sinon
            RDP_prem := resultat
        Fsi
    Sinon
        RDP_prem := 0
    Fsi
FIN
```

</details>

---

## Exercices supplementaires

### Variante A : Somme recursive

```text
FONCTION Somme (T : Tab; d,f : Entier) : Entier
DEBUT
    Si d > f alors
        Somme := 0
    Sinon
        Somme := T[d] + Somme(T, d+1, f)
    Fsi
FIN
```

### Variante B : Produit recursif

```text
FONCTION Produit (T : Tab; d,f : Entier) : Entier
DEBUT
    Si d > f alors
        Produit := 1
    Sinon
        Produit := T[d] * Produit(T, d+1, f)
    Fsi
FIN
```

### Variante C : Maximum recursif

```text
FONCTION Maximum (T : Tab; d,f : Entier) : Entier
DEBUT
    Si d = f alors
        Maximum := T[d]
    Sinon
        Si T[d] > Maximum(T, d+1, f) alors
            Maximum := T[d]
        Sinon
            Maximum := Maximum(T, d+1, f)
        Fsi
    Fsi
FIN
```

---

## Complexite

| Fonction | Temps | Espace (pile) |
|----------|-------|---------------|
| Nbr_X | O(n) | O(n) |
| Inverser | O(n) | O(n) |
| Increment | O(n) | O(n) |
| RDP_der | O(n) | O(n) |
| RDP_prem | O(n) | O(n) |

---

## Structure de la recursivite

```text
Cas de base : condition d'arret (d > f ou d = f)
Cas recursif : reduction du probleme

Schema general :
  FONCTION f(T, d, f) : TypeRetour
  DEBUT
      Si CAS_BASE alors
          f := VALEUR_BASE
      Sinon
          f := COMBINER(T[d ou f], f(T, d+1 ou d, f-1 ou f))
      Fsi
  FIN
```

---

*Exercice 01 - Recursivite - Algorithmique Chapitre 03 - M2i*
