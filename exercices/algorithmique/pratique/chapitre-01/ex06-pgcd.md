# Exercice 6 : Calcul du PGCD

## Enonce

Ecrire un algorithme qui, etant donne 2 entiers naturels strictement positifs A et B, calcule et affiche leur PGCD.

## Definition

Le **PGCD** (Plus Grand Commun Diviseur) de deux nombres entiers est le plus grand entier qui divise ces deux nombres sans laisser de reste.

## Methode par soustraction (variante de l'algorithme d'Euclide)

```text
PGCD(A, B) = PGCD(A-B, B)    pour A > B
PGCD(A, B) = PGCD(A, B-A)    pour B > A
PGCD(A, A) = A               pour A = B
```

Si deux nombres sont differents, on soustrait le plus petit du plus grand, et on recommence jusqu'a ce qu'ils deviennent egaux. Ce nombre commun est le PGCD.

## Exemple : PGCD(48, 18)

```text
A = 48, B = 18  -> A > B  -> A := 48 - 18 = 30
A = 30, B = 18  -> A > B  -> A := 30 - 18 = 12
A = 12, B = 18  -> B > A  -> B := 18 - 12 = 6
A = 12, B = 6   -> A > B  -> A := 12 - 6 = 6
A = 6,  B = 6   -> A = B  -> PGCD = 6
```

## Concepts a utiliser

- Boucle `TantQue` (nombre d'iterations inconnu)
- Condition de sortie : A = B
- Saisie controlee

## Etapes suggerees

1. Saisir A avec controle (A > 0)
2. Saisir B avec controle (B > 0)
3. Tant que A est different de B :
   - Si A > B alors A := A - B
   - Sinon B := B - A
4. Afficher A (ou B, ils sont egaux)

---

<details>
<summary>Solution</summary>

```text
Algorithme PGCD
var A, B : Entier

DEBUT
    // Saisie controlee de A
    Repeter
        Ecrire("Entrez A (> 0) : ")
        Lire(A)
    Jusqu'a A > 0

    // Saisie controlee de B
    Repeter
        Ecrire("Entrez B (> 0) : ")
        Lire(B)
    Jusqu'a B > 0

    // Calcul du PGCD par soustractions successives
    TantQue A <> B Faire
        Si A > B Alors
            A := A - B
        SiNon
            B := B - A
        finSi
    finTantQue

    // Affichage du PGCD (A = B a ce stade)
    Ecrire("PGCD = ", A)
FIN
```

**Points cles :**
- Boucle `TantQue` qui continue tant que A est different de B
- A chaque iteration, le plus grand est reduit
- Quand A = B, on a trouve le PGCD
- Cette methode est moins efficace que Euclide avec modulo mais plus intuitive

</details>

<details>
<summary>Variante avec modulo (algorithme d'Euclide classique)</summary>

```text
Algorithme PGCD_Euclide
var A, B, reste : Entier

DEBUT
    Lire(A, B)

    TantQue B <> 0 Faire
        reste := A MOD B
        A := B
        B := reste
    finTantQue

    Ecrire("PGCD = ", A)
FIN
```

Cette version est plus rapide car elle utilise le reste de la division.

</details>

---

*Exercice 6 - Algorithmique M2i*
