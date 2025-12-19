# Exercice 3 : Somme des Nombres Impairs <= N

## Enonce

Ecrire un algorithme qui lit un entier `N >= 0`, calcule et affiche la somme des nombres impairs inferieurs ou egaux a N.

## Rappel mathematique

```text
Somme des impairs jusqu'a :
1                       = 1
1 + 3                   = 4
1 + 3 + 5               = 9
1 + 3 + 5 + 7           = 16
1 + 3 + 5 + 7 + 9       = 25
1 + 3 + 5 + 7 + 9 + 11  = 36
```

**Observation** : La somme des n premiers nombres impairs = n^2

## Concepts a utiliser

- Boucle `Pour` avec accumulation
- Test de parite avec `MOD`
- Ou utilisation du pas de 2

## Etapes suggerees

1. Saisir N avec controle (N >= 0)
2. Initialiser la somme S a 0
3. Parcourir les nombres de 1 a N
4. Ajouter seulement les impairs
5. Afficher le resultat

---

<details>
<summary>Solution - Version 1 (avec pas de 2)</summary>

```text
Algorithme somme_imp_01
var i : Entier
    N : Entier
    S : Entier

DEBUT
    // Saisie controlee de N
    Repeter
        Ecrire("Entrez N (>= 0) : ")
        Lire(N)
    Jusqu'a N >= 0

    // Calcul de la somme des nombres impairs
    S := 0
    Pour i de 1 a N pas 2 Faire
        S := S + i
    finPour

    // Affichage du resultat
    Ecrire("Somme des impairs <= ", N, " = ", S)
FIN
```

**Note** : Cette version parcourt directement les impairs (1, 3, 5, 7, ...) grace au pas de 2.

</details>

<details>
<summary>Solution - Version 2 (avec test de parite)</summary>

```text
Algorithme somme_imp_02
var i : Entier
    N : Entier
    S : Entier

DEBUT
    // Saisie controlee de N
    Repeter
        Ecrire("Entrez N (>= 0) : ")
        Lire(N)
    Jusqu'a N >= 0

    // Calcul de la somme des nombres impairs
    S := 0
    Pour i de 1 a N Faire
        Si i MOD 2 = 1 Alors
            S := S + i
        finSi
    finPour

    // Affichage du resultat
    Ecrire("Somme des impairs <= ", N, " = ", S)
FIN
```

**Note** : Cette version teste chaque nombre. Un nombre est impair si son reste par 2 vaut 1.

</details>

---

**Comparaison des versions :**
- Version 1 : Plus efficace (n/2 iterations) mais necessite que N soit impair pour inclure N
- Version 2 : Plus robuste (teste chaque nombre), fonctionne pour tout N

---

*Exercice 3 - Algorithmique M2i*
