# Exercice 07 : Calculs numeriques recursifs

## Partie 1 : Nombre de chiffres

### Enonce

Ecrire une fonction recursive `Nb_Chiffres` qui retourne le nombre de chiffres d'un entier positif N.

### Exemples

```text
Nb_Chiffres(5)      = 1
Nb_Chiffres(42)     = 2
Nb_Chiffres(123)    = 3
Nb_Chiffres(9999)   = 4
Nb_Chiffres(10000)  = 5
```

### Principe

- Si N < 10, il y a 1 chiffre (cas de base)
- Sinon, supprimer le dernier chiffre (N DIV 10) et ajouter 1

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Nb_Chiffres (N : Entier) : Entier
DEBUT
    Si N < 10 alors
        Nb_Chiffres := 1
    Sinon
        Nb_Chiffres := 1 + Nb_Chiffres(N DIV 10)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
Nb_Chiffres(4527)
  4527 >= 10 -> 1 + Nb_Chiffres(452)
    452 >= 10 -> 1 + Nb_Chiffres(45)
      45 >= 10 -> 1 + Nb_Chiffres(4)
        4 < 10 -> 1
      = 1 + 1 = 2
    = 1 + 2 = 3
  = 1 + 3 = 4

Resultat : 4 chiffres
```

### Schema visuel

```text
N = 4527

   4527
   ----
    DIV 10
   452  (+1)
   ---
    DIV 10
   45   (+1)
   --
    DIV 10
   4    (+1)
   -
   < 10 -> 1

Total : 1 + 1 + 1 + 1 = 4 chiffres
```

---

## Partie 2 : Somme des chiffres

### Enonce

Ecrire une fonction recursive `Somme_Chiffres` qui calcule la somme des chiffres d'un entier.

### Exemples

```text
Somme_Chiffres(123)   = 1 + 2 + 3 = 6
Somme_Chiffres(4527)  = 4 + 5 + 2 + 7 = 18
Somme_Chiffres(999)   = 9 + 9 + 9 = 27
```

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Somme_Chiffres (N : Entier) : Entier
DEBUT
    Si N < 10 alors
        Somme_Chiffres := N
    Sinon
        Somme_Chiffres := (N MOD 10) + Somme_Chiffres(N DIV 10)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
Somme_Chiffres(4527)
  4527 >= 10 -> (4527 MOD 10) + Somme_Chiffres(452)
                     7        + Somme_Chiffres(452)
    452 >= 10 -> (452 MOD 10) + Somme_Chiffres(45)
                     2        + Somme_Chiffres(45)
      45 >= 10 -> (45 MOD 10) + Somme_Chiffres(4)
                     5        + Somme_Chiffres(4)
        4 < 10 -> 4
      = 5 + 4 = 9
    = 2 + 9 = 11
  = 7 + 11 = 18

Resultat : 18
```

---

## Partie 3 : Modulo sans division

### Enonce

Ecrire une fonction recursive `Modulo_bis` qui calcule `A MOD B` en n'utilisant que la soustraction.

### Principe

Le reste de la division de A par B est :
- A si A < B (cas de base)
- Sinon, c'est le reste de (A - B) par B

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Modulo_bis (A, B : Entier) : Entier
DEBUT
    Si A < B alors
        Modulo_bis := A
    Sinon
        Modulo_bis := Modulo_bis(A - B, B)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
Modulo_bis(17, 5)
  17 >= 5 -> Modulo_bis(12, 5)
    12 >= 5 -> Modulo_bis(7, 5)
      7 >= 5 -> Modulo_bis(2, 5)
        2 < 5 -> 2

Resultat : 17 MOD 5 = 2

Verification : 17 = 3 * 5 + 2 ✓
```

### Schema visuel

```text
A = 17, B = 5

17  ->  17 >= 5 ? OUI  ->  17 - 5 = 12
12  ->  12 >= 5 ? OUI  ->  12 - 5 = 7
7   ->   7 >= 5 ? OUI  ->   7 - 5 = 2
2   ->   2 >= 5 ? NON  ->  Resultat = 2
```

---

## Partie 4 : Division entiere sans operateur DIV

### Enonce

Ecrire une fonction recursive `Division_bis` qui calcule `A DIV B` en n'utilisant que la soustraction.

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Division_bis (A, B : Entier) : Entier
DEBUT
    Si A < B alors
        Division_bis := 0
    Sinon
        Division_bis := 1 + Division_bis(A - B, B)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
Division_bis(17, 5)
  17 >= 5 -> 1 + Division_bis(12, 5)
    12 >= 5 -> 1 + Division_bis(7, 5)
      7 >= 5 -> 1 + Division_bis(2, 5)
        2 < 5 -> 0
      = 1 + 0 = 1
    = 1 + 1 = 2
  = 1 + 2 = 3

Resultat : 17 DIV 5 = 3

Verification : 17 = 3 * 5 + 2 ✓
```

---

## Partie 5 : Multiplication sans operateur *

### Enonce

Ecrire une fonction recursive qui calcule le produit A * B en n'utilisant que l'addition.

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Multiplication_bis (A, B : Entier) : Entier
DEBUT
    Si B = 0 alors
        Multiplication_bis := 0
    Sinon
        Multiplication_bis := A + Multiplication_bis(A, B - 1)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
Multiplication_bis(7, 4)
  B=4 > 0 -> 7 + Multiplication_bis(7, 3)
    B=3 > 0 -> 7 + Multiplication_bis(7, 2)
      B=2 > 0 -> 7 + Multiplication_bis(7, 1)
        B=1 > 0 -> 7 + Multiplication_bis(7, 0)
          B=0 -> 0
        = 7 + 0 = 7
      = 7 + 7 = 14
    = 7 + 14 = 21
  = 7 + 21 = 28

Resultat : 7 * 4 = 28 ✓
```

---

## Partie 6 : Puissance

### Enonce

Ecrire une fonction recursive qui calcule A^N (A a la puissance N).

### Version simple

<details>
<summary>Solution simple</summary>

```text
FONCTION Puissance (A, N : Entier) : Entier
DEBUT
    Si N = 0 alors
        Puissance := 1
    Sinon
        Puissance := A * Puissance(A, N - 1)
    Fsi
FIN
```

</details>

### Version optimisee (exponentiation rapide)

<details>
<summary>Solution optimisee O(log n)</summary>

```text
FONCTION Puissance_Rapide (A, N : Entier) : Entier
DEBUT
    Si N = 0 alors
        Puissance_Rapide := 1
    Sinon Si N MOD 2 = 0 alors
        // N pair : A^N = (A^(N/2))^2
        Puissance_Rapide := Puissance_Rapide(A * A, N DIV 2)
    Sinon
        // N impair : A^N = A * A^(N-1)
        Puissance_Rapide := A * Puissance_Rapide(A, N - 1)
    Fsi
FIN
```

</details>

### Comparaison

```text
Puissance(2, 10) - Version simple :
  2^10 = 2 * 2^9 = 2 * 2 * 2^8 = ... (10 appels)

Puissance_Rapide(2, 10) - Version optimisee :
  2^10 : N=10 pair -> (2*2)^5 = 4^5
  4^5 : N=5 impair -> 4 * 4^4
  4^4 : N=4 pair -> (4*4)^2 = 16^2
  16^2 : N=2 pair -> (16*16)^1 = 256^1
  256^1 : N=1 impair -> 256 * 256^0
  256^0 = 1

  = 256 * 1 = 256
  = 16 * 16 = 256 ✓
  = 4 * 256 = 1024
  (seulement 5 appels)
```

---

## Partie 7 : PGCD (Plus Grand Commun Diviseur)

### Algorithme d'Euclide

<details>
<summary>Solution</summary>

```text
FONCTION PGCD (A, B : Entier) : Entier
DEBUT
    Si B = 0 alors
        PGCD := A
    Sinon
        PGCD := PGCD(B, A MOD B)
    Fsi
FIN
```

</details>

### Trace d'execution

```text
PGCD(48, 18)
  B=18 <> 0 -> PGCD(18, 48 MOD 18) = PGCD(18, 12)
    B=12 <> 0 -> PGCD(12, 18 MOD 12) = PGCD(12, 6)
      B=6 <> 0 -> PGCD(6, 12 MOD 6) = PGCD(6, 0)
        B=0 -> 6

Resultat : PGCD(48, 18) = 6

Verification : 48 = 6 * 8, 18 = 6 * 3 ✓
```

---

## Exercices supplementaires

### Variante A : Inverser un nombre

```text
// Inverser les chiffres d'un nombre
// 1234 -> 4321
FONCTION Inverser_Nombre (N, Acc : Entier) : Entier
DEBUT
    Si N = 0 alors
        Inverser_Nombre := Acc
    Sinon
        Inverser_Nombre := Inverser_Nombre(N DIV 10, Acc * 10 + N MOD 10)
    Fsi
FIN

// Appel : Inverser_Nombre(1234, 0) = 4321
```

### Variante B : Verifier si un nombre est palindrome

```text
FONCTION Est_Palindrome_Nombre (N : Entier) : Booleenne
DEBUT
    Est_Palindrome_Nombre := (N = Inverser_Nombre(N, 0))
FIN
```

### Variante C : Fibonacci

```text
FONCTION Fibonacci (N : Entier) : Entier
DEBUT
    Si N <= 1 alors
        Fibonacci := N
    Sinon
        Fibonacci := Fibonacci(N - 1) + Fibonacci(N - 2)
    Fsi
FIN
```

### Variante D : Factorielle

```text
FONCTION Factorielle (N : Entier) : Entier
DEBUT
    Si N <= 1 alors
        Factorielle := 1
    Sinon
        Factorielle := N * Factorielle(N - 1)
    Fsi
FIN
```

---

## Complexite

| Fonction | Temps | Espace |
|----------|-------|--------|
| Nb_Chiffres | O(log N) | O(log N) |
| Somme_Chiffres | O(log N) | O(log N) |
| Modulo_bis | O(A/B) | O(A/B) |
| Division_bis | O(A/B) | O(A/B) |
| Multiplication_bis | O(B) | O(B) |
| Puissance simple | O(N) | O(N) |
| Puissance rapide | O(log N) | O(log N) |
| PGCD | O(log(min(A,B))) | O(log(min(A,B))) |
| Fibonacci | O(2^N) | O(N) |
| Factorielle | O(N) | O(N) |

---

## Points cles

1. **DIV et MOD** : Operations fondamentales pour decomposer les nombres
2. **Reduction** : A chaque appel, le probleme devient plus petit
3. **Cas de base** : Generalement quand N < 10 ou N = 0
4. **Optimisation** : L'exponentiation rapide reduit O(n) a O(log n)
5. **Recursivite terminale** : Modulo_bis et Division_bis peuvent etre optimises

---

*Exercice 07 - Recursivite - Algorithmique Chapitre 03 - M2i*
