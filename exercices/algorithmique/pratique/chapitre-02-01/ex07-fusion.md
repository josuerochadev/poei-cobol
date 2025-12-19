# Exercice 7 : Fusion de deux tableaux tries

## Enonce

Fusionner deux tableaux T1 et T2 tries par ordre croissant en un tableau T3 egalement trie.

## Exemple

```text
T1 : | 1 | 3 | 5 | 8 | 9 | 11 | 14 |
T2 : | 3 | 4 | 6 | 7 | 13 | 18 |
T3 : | 1 | 3 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 11 | 13 | 14 | 18 |
```

## Concepts a utiliser

- Trois indices (un pour chaque tableau)
- Comparaison des elements courants
- Copie des elements restants

## Principe de l'algorithme

1. Comparer T1[i] et T2[j]
2. Copier le plus petit dans T3[k]
3. Avancer l'indice du tableau dont on a copie l'element
4. Quand un tableau est epuise, copier le reste de l'autre

---

<details>
<summary>Solution</summary>

```text
Procedure Fusion(T1, T2 : Type_TAB; N, M : Entier; Var T3 : Type_TAB; Var P : Entier)
Var   i, j, k : Entier

DEBUT
    i := 1      // Index pour T1
    j := 1      // Index pour T2
    k := 1      // Index pour T3

    // Phase 1 : Fusion tant que les deux tableaux ont des elements
    TantQue (i <= N) et (j <= M) Faire
        Si T1[i] <= T2[j] Alors
            T3[k] := T1[i]
            i := i + 1
        SiNon
            T3[k] := T2[j]
            j := j + 1
        finSi
        k := k + 1
    finTantQue

    // Phase 2 : Copier les elements restants de T1
    TantQue i <= N Faire
        T3[k] := T1[i]
        i := i + 1
        k := k + 1
    finTantQue

    // Phase 3 : Copier les elements restants de T2
    TantQue j <= M Faire
        T3[k] := T2[j]
        j := j + 1
        k := k + 1
    finTantQue

    P := N + M
FIN
```

</details>

---

## Trace d'execution

```text
T1 : | 1 | 5 | 9 |     T2 : | 3 | 7 |
      i=1                    j=1

Etape 1 : T1[1]=1 < T2[1]=3 -> T3[1]=1, i=2
Etape 2 : T1[2]=5 > T2[1]=3 -> T3[2]=3, j=2
Etape 3 : T1[2]=5 < T2[2]=7 -> T3[3]=5, i=3
Etape 4 : T1[3]=9 > T2[2]=7 -> T3[4]=7, j=3
Etape 5 : j > M, copier T1[3]=9 -> T3[5]=9

T3 : | 1 | 3 | 5 | 7 | 9 |
```

---

## Illustration visuelle

```text
   T1: [1, 5, 9]           T2: [3, 7]
        ^                       ^
        i=1                     j=1

Comparaison: 1 < 3  -->  T3[1] = 1

   T1: [1, 5, 9]           T2: [3, 7]
           ^                    ^
           i=2                  j=1

Comparaison: 5 > 3  -->  T3[2] = 3

   T1: [1, 5, 9]           T2: [3, 7]
           ^                       ^
           i=2                     j=2

Comparaison: 5 < 7  -->  T3[3] = 5

... et ainsi de suite
```

---

## Complexite

- **Temps** : O(N + M) - chaque element est traite exactement une fois
- **Espace** : O(N + M) - tableau resultat de taille N + M

---

## Application : Merge Sort

Cette procedure de fusion est la base de l'algorithme de tri fusion (Merge Sort) :

```text
MergeSort(T, debut, fin)
    Si debut < fin Alors
        milieu := (debut + fin) / 2
        MergeSort(T, debut, milieu)      // Trier la moitie gauche
        MergeSort(T, milieu+1, fin)      // Trier la moitie droite
        Fusion(T, debut, milieu, fin)    // Fusionner les deux moities
    finSi
```

---

*Exercice 7 - Algorithmique Chapitre 02 - M2i*
