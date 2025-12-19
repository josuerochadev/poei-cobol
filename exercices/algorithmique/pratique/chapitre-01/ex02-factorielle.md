# Exercice 2 : Calcul de Factorielle (N!)

## Enonce

Ecrire un algorithme qui, etant donne un entier naturel N, calcule son factoriel (N!).

## Rappel mathematique

```text
0! = 1
1! = 1
2! = 1 x 2 = 2
3! = 1 x 2 x 3 = 6
4! = 1 x 2 x 3 x 4 = 24
5! = 1 x 2 x 3 x 4 x 5 = 120
...
N! = 1 x 2 x 3 x ... x (N-1) x N
```

## Concepts a utiliser

- Boucle `Pour` avec accumulation
- Initialisation a l'element neutre (1)
- Saisie controlee

## Etapes suggerees

1. Saisir N avec controle (N >= 0)
2. Initialiser le resultat F a 1
3. Multiplier F par chaque entier de 1 a N
4. Afficher le resultat

---

<details>
<summary>Solution</summary>

```text
Algorithme Factoriel
var i : Entier
    N : Entier
    F : Entier

DEBUT
    // Saisie controlee de n
    Repeter
        Ecrire("Entrez N (>= 0) : ")
        Lire(N)
    Jusqu'a N >= 0

    // Calcul factoriel de N
    F := 1
    Pour i de 1 a N Faire
        F := F * i
    finPour

    // Affichage du resultat
    Ecrire(N, "! = ", F)
FIN
```

**Points cles :**
- Tres similaire au calcul de puissance
- On multiplie par l'indice `i` au lieu d'une valeur fixe
- Si N = 0, la boucle ne s'execute pas et F = 1 (correct : 0! = 1)

</details>

---

*Exercice 2 - Algorithmique M2i*
