# Exercice 06 : Operations recursives sur les tableaux

## Enonce

On dispose d'un tableau T de N entiers. On souhaite ecrire des fonctions recursives pour verifier certaines proprietes du tableau.

```text
Type Tableau = Tableau [1..MAX] de Entier
```

---

## Partie 1 : Tableau de nombres pairs

Ecrire une fonction recursive `Pair` qui verifie si tous les elements d'un tableau sont pairs.

### Methode 1 : Isoler le dernier element

<details>
<summary>Solution</summary>

```text
FONCTION Pair_Der (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d > f alors
        // Tableau vide -> vrai par convention
        Pair_Der := vrai
    Sinon
        Si T[f] MOD 2 = 0 alors
            // Dernier element pair, verifier le reste
            Pair_Der := Pair_Der(T, d, f - 1)
        Sinon
            // Element impair trouve
            Pair_Der := faux
        Fsi
    Fsi
FIN
```

</details>

### Schema d'execution

```text
T = [4, 8, 2, 6]

Pair_Der(T, 1, 4)
  T[4]=6, 6 MOD 2 = 0 -> pair -> Pair_Der(T, 1, 3)
    T[3]=2, 2 MOD 2 = 0 -> pair -> Pair_Der(T, 1, 2)
      T[2]=8, 8 MOD 2 = 0 -> pair -> Pair_Der(T, 1, 1)
        T[1]=4, 4 MOD 2 = 0 -> pair -> Pair_Der(T, 1, 0)
          d > f -> vrai

Resultat : VRAI
```

---

### Methode 2 : Isoler le premier element

<details>
<summary>Solution</summary>

```text
FONCTION Pair_Prem (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d > f alors
        Pair_Prem := vrai
    Sinon
        Si T[d] MOD 2 = 0 alors
            Pair_Prem := Pair_Prem(T, d + 1, f)
        Sinon
            Pair_Prem := faux
        Fsi
    Fsi
FIN
```

</details>

---

### Methode 3 : Diviser pour regner

<details>
<summary>Solution</summary>

```text
FONCTION Pair_Dicho (T : Tableau; d, f : Entier) : Booleenne
VAR milieu : Entier
DEBUT
    Si d > f alors
        Pair_Dicho := vrai
    Sinon Si d = f alors
        // Un seul element
        Pair_Dicho := (T[d] MOD 2 = 0)
    Sinon
        milieu := (d + f) DIV 2
        // Verifier les deux moities
        Pair_Dicho := Pair_Dicho(T, d, milieu) ET Pair_Dicho(T, milieu + 1, f)
    Fsi
FIN
```

</details>

### Schema diviser pour regner

```text
T = [4, 8, 2, 6, 10, 12]

                    Pair_Dicho(1, 6)
                   /                \
          Pair_Dicho(1, 3)    Pair_Dicho(4, 6)
           /          \          /         \
    Pair(1,2)    Pair(3,3)  Pair(4,5)   Pair(6,6)
     /     \         |        /    \        |
  Pair(1,1) Pair(2,2) 2%2=0  Pair(4,4) Pair(5,5)  12%2=0
   4%2=0    8%2=0             6%2=0    10%2=0

Tous pairs -> VRAI
```

---

## Partie 2 : Tableau alterne

Ecrire une fonction recursive `Alterne` qui verifie si un tableau est alterne, c'est-a-dire si les elements de rang impair sont positifs et les elements de rang pair sont negatifs (ou inversement).

### Definition

Un tableau est alterne si :
- Position 1 : signe S1
- Position 2 : signe oppose a S1
- Position 3 : signe S1
- etc.

### Exemple

```text
T = [3, -2, 5, -8, 1, -4]  -> ALTERNE (+, -, +, -, +, -)
T = [-3, 2, -5, 8, -1, 4]  -> ALTERNE (-, +, -, +, -, +)
T = [3, -2, 5, 8, 1, -4]   -> NON ALTERNE (5 et 8 meme signe)
```

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Signe (x : Entier) : Entier
DEBUT
    Si x >= 0 alors
        Signe := 1
    Sinon
        Signe := -1
    Fsi
FIN

FONCTION Alterne (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        // 0 ou 1 element -> toujours alterne
        Alterne := vrai
    Sinon
        Si Signe(T[d]) <> Signe(T[d + 1]) alors
            // Signes differents, verifier la suite
            Alterne := Alterne(T, d + 1, f)
        Sinon
            // Deux elements consecutifs de meme signe
            Alterne := faux
        Fsi
    Fsi
FIN
```

</details>

### Trace d'execution

```text
T = [3, -2, 5, -8]

Alterne(T, 1, 4)
  Signe(T[1])=Signe(3)=1, Signe(T[2])=Signe(-2)=-1
  1 <> -1 ? OUI -> Alterne(T, 2, 4)
    Signe(T[2])=-1, Signe(T[3])=Signe(5)=1
    -1 <> 1 ? OUI -> Alterne(T, 3, 4)
      Signe(T[3])=1, Signe(T[4])=Signe(-8)=-1
      1 <> -1 ? OUI -> Alterne(T, 4, 4)
        d >= f -> vrai

Resultat : VRAI
```

---

## Partie 3 : Tableau symetrique

Ecrire une fonction recursive qui verifie si un tableau est symetrique (palindrome de nombres).

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Symetrique (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Symetrique := vrai
    Sinon
        Si T[d] = T[f] alors
            Symetrique := Symetrique(T, d + 1, f - 1)
        Sinon
            Symetrique := faux
        Fsi
    Fsi
FIN
```

</details>

### Exemple

```text
T = [1, 2, 3, 2, 1]

     1    2    3    4    5
   +----+----+----+----+----+
   | 1  | 2  | 3  | 2  | 1  |
   +----+----+----+----+----+
     ^                   ^
     d=1                f=5   -> T[1]=1 = T[5]=1 ? OUI

     1    2    3    4    5
   +----+----+----+----+----+
   | 1  | 2  | 3  | 2  | 1  |
   +----+----+----+----+----+
          ^         ^
         d=2       f=4        -> T[2]=2 = T[4]=2 ? OUI

     1    2    3    4    5
   +----+----+----+----+----+
   | 1  | 2  | 3  | 2  | 1  |
   +----+----+----+----+----+
               ^
              d=f=3           -> d >= f -> VRAI

Resultat : SYMETRIQUE
```

---

## Partie 4 : Tableau trie

Ecrire une fonction recursive qui verifie si un tableau est trie par ordre croissant.

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Trie (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Trie := vrai
    Sinon
        Si T[d] <= T[d + 1] alors
            Trie := Trie(T, d + 1, f)
        Sinon
            Trie := faux
        Fsi
    Fsi
FIN
```

</details>

---

## Exercices supplementaires

### Variante A : Compter les elements pairs

```text
FONCTION Compter_Pairs (T : Tableau; d, f : Entier) : Entier
DEBUT
    Si d > f alors
        Compter_Pairs := 0
    Sinon
        Si T[d] MOD 2 = 0 alors
            Compter_Pairs := 1 + Compter_Pairs(T, d + 1, f)
        Sinon
            Compter_Pairs := Compter_Pairs(T, d + 1, f)
        Fsi
    Fsi
FIN
```

### Variante B : Verifier si au moins un element pair existe

```text
FONCTION Au_Moins_Un_Pair (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d > f alors
        Au_Moins_Un_Pair := faux
    Sinon
        Si T[d] MOD 2 = 0 alors
            Au_Moins_Un_Pair := vrai
        Sinon
            Au_Moins_Un_Pair := Au_Moins_Un_Pair(T, d + 1, f)
        Fsi
    Fsi
FIN
```

### Variante C : Tableau strictement positif

```text
FONCTION Tous_Positifs (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d > f alors
        Tous_Positifs := vrai
    Sinon
        Si T[d] > 0 alors
            Tous_Positifs := Tous_Positifs(T, d + 1, f)
        Sinon
            Tous_Positifs := faux
        Fsi
    Fsi
FIN
```

### Variante D : Tableau sans doublons

```text
FONCTION Sans_Doublon (T : Tableau; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Sans_Doublon := vrai
    Sinon
        Si NON Existe(T, d + 1, f, T[d]) alors
            Sans_Doublon := Sans_Doublon(T, d + 1, f)
        Sinon
            Sans_Doublon := faux
        Fsi
    Fsi
FIN

FONCTION Existe (T : Tableau; d, f, x : Entier) : Booleenne
DEBUT
    Si d > f alors
        Existe := faux
    Sinon
        Si T[d] = x alors
            Existe := vrai
        Sinon
            Existe := Existe(T, d + 1, f, x)
        Fsi
    Fsi
FIN
```

---

## Comparaison des methodes

```text
Methode              | Avantage                  | Inconvenient
---------------------|---------------------------|------------------
Isoler le premier    | Intuitive                 | Recursion arriere
Isoler le dernier    | Recursion terminale       | Moins naturelle
Diviser pour regner  | Parallelisable            | Plus de code
```

---

## Complexite

| Fonction | Temps | Espace (pile) |
|----------|-------|---------------|
| Pair (lineaire) | O(n) | O(n) |
| Pair (dichotomie) | O(n) | O(log n) |
| Alterne | O(n) | O(n) |
| Symetrique | O(n/2) | O(n/2) |
| Trie | O(n) | O(n) |

---

## Points cles

1. **Cas de base** : Tableau vide (d > f) ou un element (d = f)
2. **Reduction** : Isoler un element et appliquer la recursion sur le reste
3. **Court-circuit** : Retourner faux des qu'une condition n'est pas remplie
4. **Diviser pour regner** : Alternative pour reduire la profondeur de pile

---

*Exercice 06 - Recursivite - Algorithmique Chapitre 03 - M2i*
