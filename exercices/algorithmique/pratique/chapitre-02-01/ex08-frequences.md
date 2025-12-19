# Exercice 8 : Tableau de frequences

## Enonce

Construire un tableau T de frequences a partir d'un tableau A, ou T contient les valeurs distinctes triees et leur nombre d'occurrences.

## Exemple

```text
A : | 5 | 3 | 1 | 2 | 5 | 6 | 4 | 3 | 3 | 9 | 1 | 5 | 3 |

T (resultat) :
| Valeur | Frequence |
|--------|-----------|
| 1      | 2         |
| 2      | 1         |
| 3      | 4         |
| 4      | 1         |
| 5      | 3         |
| 6      | 1         |
| 9      | 1         |
```

## Concepts a utiliser

- Matrice a 2 colonnes (valeur, frequence)
- Insertion triee
- Recherche et mise a jour
- Decalage d'elements

---

## Partie 1 : Construction du tableau T

<details>
<summary>Solution</summary>

```text
Procedure Construire_T(A : Tab_int; N : Entier; Var T : Mat_int; Var M : Entier)
Var   i : Entier

DEBUT
    M := 0
    Pour i de 1 a N Faire
        Inserer_T(T, M, A[i])
    finPour
FIN

// Procedure d'insertion avec maintien de l'ordre croissant
Procedure Inserer_T(Var T : Mat_int; Var M : Entier; x : Entier)
Var   i, j : Entier

DEBUT
    // Rechercher la position de x
    i := 1
    TantQue (i <= M) et (T[i,1] < x) Faire
        i := i + 1
    finTantQue

    Si (i <= M) et (T[i,1] = x) Alors
        // x existe deja : incrementer sa frequence
        T[i,2] := T[i,2] + 1
    SiNon
        // x n'existe pas : l'inserer a la position i
        // Decaler les elements vers la droite
        j := M
        TantQue j >= i Faire
            T[j+1,1] := T[j,1]
            T[j+1,2] := T[j,2]
            j := j - 1
        finTantQue

        T[i,1] := x
        T[i,2] := 1
        M := M + 1
    finSi
FIN
```

</details>

---

## Partie 2 : Trouver les elements le plus et le moins frequents

<details>
<summary>Solution</summary>

```text
Procedure Plus_Moins_frequent(T : Mat_int; M : Entier; Var pF, mF : Entier)
Var   i, posPf, posMf : Entier

DEBUT
    posPf := 1
    posMf := 1

    Pour i de 2 a M Faire
        Si T[i,2] > T[posPf,2] Alors
            posPf := i      // Nouveau plus frequent
        finSi
        Si T[i,2] < T[posMf,2] Alors
            posMf := i      // Nouveau moins frequent
        finSi
    finPour

    pF := T[posPf,1]    // Valeur du plus frequent
    mF := T[posMf,1]    // Valeur du moins frequent
FIN
```

**Pour l'exemple :**
- Plus frequent : 3 (frequence = 4)
- Moins frequents : 2, 4, 6, 9 (frequence = 1)

</details>

---

## Partie 3 : Modification de A[i] avec mise a jour de T

<details>
<summary>Solution</summary>

```text
Procedure Changer_T(A : Tab_int; N, i, x : Entier; Var T : Mat_int; Var M : Entier)
DEBUT
    Supprimer_T(T, M, A[i])    // Retirer l'ancienne valeur
    Inserer_T(T, M, x)         // Ajouter la nouvelle valeur
    A[i] := x                  // Mettre a jour A
FIN

// Procedure de suppression
Procedure Supprimer_T(Var T : Mat_int; Var M : Entier; y : Entier)
Var   i, j : Entier

DEBUT
    // Rechercher y
    i := 1
    TantQue (i <= M) et (T[i,1] < y) Faire
        i := i + 1
    finTantQue

    Si (i <= M) et (T[i,1] = y) Alors
        T[i,2] := T[i,2] - 1

        // Si frequence = 0, supprimer la ligne
        Si T[i,2] = 0 Alors
            Pour j de i a M-1 Faire
                T[j,1] := T[j+1,1]
                T[j,2] := T[j+1,2]
            finPour
            M := M - 1
        finSi
    finSi
FIN
```

</details>

---

## Trace d'execution

Pour A = [5, 3, 1, 2, 5] :

| Etape | Element | Action | T |
|-------|---------|--------|---|
| 1 | 5 | Inserer 5 | [(5,1)] |
| 2 | 3 | Inserer 3 avant 5 | [(3,1), (5,1)] |
| 3 | 1 | Inserer 1 avant 3 | [(1,1), (3,1), (5,1)] |
| 4 | 2 | Inserer 2 entre 1 et 3 | [(1,1), (2,1), (3,1), (5,1)] |
| 5 | 5 | Incrementer freq de 5 | [(1,1), (2,1), (3,1), (5,2)] |

---

## Complexites

| Operation | Complexite |
|-----------|-----------|
| Recherche dans T | O(M) |
| Insertion dans T | O(M) - decalage |
| Suppression dans T | O(M) - decalage |
| Construction complete | O(N x M) |

**Note :** Avec un arbre binaire de recherche ou une table de hachage, on pourrait reduire a O(N log M) ou O(N).

---

*Exercice 8 - Algorithmique Chapitre 02 - M2i*
