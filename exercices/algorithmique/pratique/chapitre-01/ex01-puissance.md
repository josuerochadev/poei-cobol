# Exercice 1 : Calcul de Puissance (x^n)

## Enonce

Ecrire un algorithme qui lit une valeur reelle `x > 0` et un entier `n >= 0` et affiche la valeur de x a la puissance n (x^n).

## Rappel mathematique

```text
2^0 = 1
5^2 = 5 x 5 = 25
7^3 = 7 x 7 x 7 = 343
3^5 = 3 x 3 x 3 x 3 x 3 = 243

X^n = X x X x ... x X  (n fois)

Cas particulier : si n = 0, alors X^0 = 1
```

## Concepts a utiliser

- Saisie controlee avec `Repeter...Jusqu'a`
- Boucle `Pour` avec nombre d'iterations connu
- Accumulation par multiplication
- Initialisation de l'accumulateur a 1 (element neutre)

## Etapes suggerees

1. Saisir x avec controle (x > 0)
2. Saisir n avec controle (n >= 0)
3. Initialiser le resultat p a 1
4. Multiplier p par x, n fois
5. Afficher le resultat

---

<details>
<summary>Solution</summary>

```text
Algorithme Puissance
var i : Entier
    p : Reel
    x : Reel
    n : Entier

DEBUT
    // Saisie controlee de x
    Repeter
        Ecrire("Entrez x (> 0) : ")
        Lire(x)
    Jusqu'a x > 0

    // Saisie controlee de n
    Repeter
        Ecrire("Entrez n (>= 0) : ")
        Lire(n)
    Jusqu'a n >= 0

    // Calcul de x a la puissance n
    p := 1
    Pour i de 1 a n Faire
        p := p * x
    finPour

    // Affichage du resultat
    Ecrire(x, " ^ ", n, " = ", p)
FIN
```

**Points cles :**
- `p := 1` car 1 est l'element neutre de la multiplication
- La boucle multiplie p par x exactement n fois
- Si n = 0, la boucle ne s'execute pas et p reste a 1 (correct : x^0 = 1)

</details>

---

*Exercice 1 - Algorithmique M2i*
