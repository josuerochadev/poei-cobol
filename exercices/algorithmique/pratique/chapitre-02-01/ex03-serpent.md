# Exercice 3 : Parcours en serpent d'une matrice

## Enonce

Lire une matrice N x P et l'afficher colonne par colonne avec :
- Colonnes d'indice **impair** : de haut en bas
- Colonnes d'indice **pair** : de bas en haut

## Exemple

```text
Matrice 4x5 :
| 1  | 43 | 2  | 33 | 5  |
| 11 | 42 | 21 | 32 | 51 |
| 12 | 41 | 22 | 31 | 52 |
| 13 | 4  | 23 | 3  | 53 |

Affichage serpent :
Col 1 (impair, haut->bas) : 1, 11, 12, 13
Col 2 (pair, bas->haut)   : 4, 41, 42, 43
Col 3 (impair, haut->bas) : 2, 21, 22, 23
Col 4 (pair, bas->haut)   : 3, 31, 32, 33
Col 5 (impair, haut->bas) : 5, 51, 52, 53
```

## Concepts a utiliser

- Declaration et remplissage d'une matrice
- Boucles imbriquees
- Alternance de direction (pas positif/negatif)
- Procedure Permuter

## Etapes suggerees

1. Saisir les dimensions N (lignes) et P (colonnes)
2. Remplir la matrice M[i,j]
3. Pour chaque colonne :
   - Si impaire : parcourir de 1 a N
   - Si paire : parcourir de N a 1
4. Alterner la direction a chaque colonne

---

<details>
<summary>Solution - Version 1 (avec pas de 2)</summary>

```text
Programme Parcours_matrice
Var   N, P, i, j, k : entier
      M : Type_MAT

DEBUT
    // Lecture des dimensions
    Repeter
        Ecrire("Nombre de lignes : ")
        Lire(N)
    Jusqu'a N > 0 et N <= 100

    Repeter
        Ecrire("Nombre de colonnes : ")
        Lire(P)
    Jusqu'a P > 0 et P <= 100

    // Remplissage de la matrice
    Pour i de 1 a N Faire
        Pour j de 1 a P Faire
            Ecrire("M[", i, ",", j, "] = ")
            Lire(M[i,j])
        finPour
    finPour

    // Affichage en serpent
    Ecrire("Parcours serpent :")

    Pour j de 1 a P pas 2 Faire
        k := j + 1

        // Colonne impaire : haut -> bas
        Pour i de 1 a N Faire
            Ecrire(M[i,j], " ")
        finPour

        // Colonne paire : bas -> haut (si elle existe)
        Si k <= P Alors
            Pour i de N a 1 pas -1 Faire
                Ecrire(M[i,k], " ")
            finPour
        finSi
    finPour
FIN
```

</details>

<details>
<summary>Solution - Version 2 (avec procedure Permuter)</summary>

```text
Programme Parcours_matrice
Var   N, P, i, j, deb, fin, inc : entier
      M : Type_MAT

DEBUT
    // Lecture et remplissage (identique a Version 1)
    ...

    // Affichage avec alternance automatique
    deb := 1
    fin := N
    inc := 1

    Pour j de 1 a P Faire
        // Parcourir la colonne j dans la direction courante
        Pour i de deb a fin pas inc Faire
            Ecrire(M[i,j], " ")
        finPour

        // Inverser la direction pour la prochaine colonne
        Permuter(deb, fin)
        inc := inc * -1
    finPour
FIN

// Procedure auxiliaire
Procedure Permuter(var x, y : Entier)
Var   temp : entier
DEBUT
    temp := x
    x := y
    y := temp
FIN
```

**Avantage** : La version 2 est plus elegante et evite la duplication de code.

</details>

---

## Trace d'execution

Pour une matrice 3x4 :

| Colonne | Direction | deb | fin | inc | Elements |
|---------|-----------|-----|-----|-----|----------|
| 1       | Bas       | 1   | 3   | +1  | M[1,1], M[2,1], M[3,1] |
| 2       | Haut      | 3   | 1   | -1  | M[3,2], M[2,2], M[1,2] |
| 3       | Bas       | 1   | 3   | +1  | M[1,3], M[2,3], M[3,3] |
| 4       | Haut      | 3   | 1   | -1  | M[3,4], M[2,4], M[1,4] |

---

*Exercice 3 - Algorithmique Chapitre 02 - M2i*
