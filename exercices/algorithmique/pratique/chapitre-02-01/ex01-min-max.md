# Exercice 1 : Min et Max d'un tableau

## Enonce

Ecrire un algorithme qui :
1. Lit un entier N (0 < N < 100)
2. Saisit N entiers et les stocke dans un tableau T
3. Affiche la plus grande et la plus petite valeur dans T

## Exemple

```text
Entree : N = 5, T = [7, 2, 9, 1, 5]
Sortie : Min = 1, Max = 9
```

## Concepts a utiliser

- Declaration et remplissage d'un tableau
- Parcours d'un tableau
- Recherche d'extremums (min/max)
- Initialisation avec le premier element

## Etapes suggerees

1. Saisir N avec controle (0 < N <= 100)
2. Remplir le tableau avec N valeurs
3. Initialiser min et max avec T[1]
4. Parcourir le tableau de 2 a N
5. Mettre a jour min si T[i] < min
6. Mettre a jour max si T[i] > max
7. Afficher les resultats

---

<details>
<summary>Solution - Version 1 (deux boucles separees)</summary>

```text
Programme Min_Max
Var   N, i, max, min : entier
      T : Type_TAB

DEBUT
    // Lecture du nombre de valeurs avec validation
    Repeter
        Ecrire("Nombre d'elements (1-100) : ")
        Lire(N)
    Jusqu'a N > 0 et N <= 100

    // Remplissage du tableau
    Pour i de 1 a N Faire
        Ecrire("T[", i, "] = ")
        Lire(T[i])
    finPour

    // Recherche du maximum
    max := T[1]
    Pour i de 2 a N Faire
        Si T[i] > max Alors
            max := T[i]
        finSi
    finPour

    // Recherche du minimum
    min := max
    Pour i de 1 a N Faire
        Si T[i] < min Alors
            min := T[i]
        finSi
    finPour

    Ecrire("Minimum : ", min)
    Ecrire("Maximum : ", max)
FIN
```

</details>

<details>
<summary>Solution - Version 2 (une seule boucle, optimisee)</summary>

```text
Programme Min_Max
Var   N, i, max, min : entier
      T : Type_TAB

DEBUT
    // Lecture et remplissage (identique)
    Repeter
        Lire(N)
    Jusqu'a N > 0 et N <= 100

    Pour i de 1 a N Faire
        Lire(T[i])
    finPour

    // Recherche min et max en une seule boucle
    max := T[1]
    min := T[1]
    Pour i de 2 a N Faire
        Si T[i] > max Alors
            max := T[i]
        finSi
        Si T[i] < min Alors
            min := T[i]
        finSi
    finPour

    Ecrire("Min = ", min, ", Max = ", max)
FIN
```

**Avantage** : La version 2 ne parcourt le tableau qu'une seule fois (N-1 iterations au lieu de 2N-1).

</details>

---

*Exercice 1 - Algorithmique Chapitre 02 - M2i*
