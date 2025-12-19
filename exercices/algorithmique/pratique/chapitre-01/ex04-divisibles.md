# Exercice 4 : Entiers Divisibles dans un Intervalle

## Enonce

Ecrire un algorithme qui lit 3 entiers A, B et C, affiche les entiers compris entre A et B qui sont divisibles par C ainsi que leur nombre.

## Exemple

Pour A=3, B=19, C=4 :

```text
Intervalle : 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19
Divisibles :   | 4 |   |   |   | 8 |   |    |    | 12 |    |    |    | 16 |    |    |

Resultat : 4, 8, 12, 16
Nombre : 4
```

## Concepts a utiliser

- Boucle `Pour` pour parcourir l'intervalle
- Test de divisibilite avec `MOD`
- Comptage des elements trouves

## Etapes suggerees

1. Saisir A, B, C
2. Initialiser le compteur a 0
3. Parcourir tous les entiers de A a B
4. Pour chaque entier divisible par C : l'afficher et incrementer le compteur
5. Afficher le nombre total

---

<details>
<summary>Solution</summary>

```text
Algorithme Nb_Divisible
var i : Entier
    Nbr : Entier
    A, B, C : Entier

DEBUT
    Nbr := 0

    // Saisie de A, B, C
    Ecrire("Entrez A, B, C : ")
    Lire(A, B, C)

    Ecrire("Nombres entre ", A, " et ", B, " divisibles par ", C, " :")

    // Affichage des entiers divisibles par C et leur nombre
    Pour i de A a B Faire
        Si i MOD C = 0 Alors
            Ecrire(i)
            Nbr := Nbr + 1
        finSi
    finPour

    Ecrire("Nombre total : ", Nbr)
FIN
```

**Points cles :**
- `i MOD C = 0` signifie que `i` est divisible par `C` (reste = 0)
- Le compteur `Nbr` est incremente a chaque nombre trouve
- On peut aussi calculer le nombre sans boucle : `(B DIV C) - ((A-1) DIV C)`

</details>

---

*Exercice 4 - Algorithmique M2i*
