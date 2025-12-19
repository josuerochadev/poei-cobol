# Chapitre V - Complexite Algorithmique

## 1. Introduction

### Pourquoi mesurer la complexite ?

Lorsqu'on ecrit un algorithme, il est essentiel de pouvoir evaluer son **efficacite** :
- Combien de temps prendra-t-il ?
- Combien de memoire utilisera-t-il ?
- Comment se comportera-t-il avec de grandes quantites de donnees ?

### Deux types de complexite

| Type | Question | Mesure |
|------|----------|--------|
| **Temporelle** | Combien de temps ? | Nombre d'operations |
| **Spatiale** | Combien de memoire ? | Espace utilise |

### Pourquoi pas mesurer en secondes ?

Le temps d'execution depend de :
- La vitesse du processeur
- Le langage de programmation
- Le systeme d'exploitation
- Les autres programmes en cours

On prefere donc compter le **nombre d'operations elementaires**.

---

## 2. La notation Big-O

### Definition

La notation **Big-O** (ou notation asymptotique) decrit le comportement d'un algorithme quand la taille des donnees **tend vers l'infini**.

```
O(f(n)) = "de l'ordre de f(n)"
```

On s'interesse au **pire cas** et on ignore :
- Les constantes multiplicatives
- Les termes de plus faible ordre

### Exemples de simplification

| Expression exacte | Notation Big-O |
|-------------------|----------------|
| 3n + 5 | O(n) |
| 2n² + 10n + 100 | O(n²) |
| 5 × 2^n + n³ | O(2^n) |
| log₂(n) + 1000 | O(log n) |

### Regles de simplification

1. **Ignorer les constantes** : O(3n) = O(n)
2. **Garder le terme dominant** : O(n² + n) = O(n²)
3. **Les bases de log sont equivalentes** : O(log₂ n) = O(log₁₀ n) = O(log n)

---

## 3. Classes de complexite courantes

### Hierarchie (de la plus rapide a la plus lente)

```
O(1) < O(log n) < O(n) < O(n log n) < O(n²) < O(n³) < O(2^n) < O(n!)
```

### Description de chaque classe

| Complexite | Nom | Description | Exemple |
|------------|-----|-------------|---------|
| O(1) | Constante | Temps fixe, independant de n | Acces tableau par indice |
| O(log n) | Logarithmique | Divise le probleme par 2 | Recherche dichotomique |
| O(n) | Lineaire | Proportionnel a n | Recherche sequentielle |
| O(n log n) | Quasi-lineaire | Optimal pour les tris | Tri fusion, tri rapide |
| O(n²) | Quadratique | Boucles imbriquees | Tri selection, tri bulles |
| O(n³) | Cubique | Triple imbrication | Multiplication matricielle naive |
| O(2^n) | Exponentielle | Double a chaque element | Fibonacci naif, sous-ensembles |
| O(n!) | Factorielle | Toutes les permutations | Probleme du voyageur (brute force) |

### Visualisation comparative

Pour n = 1000 elements :

| Complexite | Nombre d'operations |
|------------|---------------------|
| O(1) | 1 |
| O(log n) | ~10 |
| O(n) | 1 000 |
| O(n log n) | ~10 000 |
| O(n²) | 1 000 000 |
| O(n³) | 1 000 000 000 |
| O(2^n) | Astronomique ! |

---

## 4. Analyser la complexite temporelle

### Regles de base

#### Operations elementaires : O(1)

```
x := 5              // O(1)
y := x + 3          // O(1)
Si x > 0 Alors      // O(1)
```

#### Sequence d'instructions

On additionne, puis on garde le terme dominant.

```
x := 5              // O(1)
y := x + 3          // O(1)
z := y * 2          // O(1)
// Total : O(1) + O(1) + O(1) = O(1)
```

#### Boucle simple

```
Pour i de 1 a n faire    // Execute n fois
    x := x + 1           // O(1)
Fpour
// Total : n × O(1) = O(n)
```

#### Boucles imbriquees

```
Pour i de 1 a n faire        // n fois
    Pour j de 1 a n faire    // n fois chacune
        x := x + 1           // O(1)
    Fpour
Fpour
// Total : n × n × O(1) = O(n²)
```

#### Boucle avec division

```
i := n
Tant que i > 1 faire
    i := i DIV 2         // Divise par 2
Fait
// Nombre d'iterations : log₂(n) = O(log n)
```

### Cas particuliers

#### Boucle dependante

```
Pour i de 1 a n faire
    Pour j de 1 a i faire    // j va de 1 a i (pas n)
        x := x + 1
    Fpour
Fpour
// Total : 1 + 2 + 3 + ... + n = n(n+1)/2 = O(n²)
```

#### Condition dans une boucle

```
Pour i de 1 a n faire
    Si condition Alors
        // O(n) operations
    Sinon
        // O(1) operation
    Fsi
Fpour
// Pire cas : O(n) × O(n) = O(n²)
```

---

## 5. Analyser la complexite spatiale

### Definition

La complexite spatiale mesure la **memoire supplementaire** utilisee par l'algorithme (hors donnees d'entree).

### Exemples

#### O(1) - Espace constant

```
Fonction Somme(T : Tableau, n : entier) : entier
Var i, s : entier    // Seulement 2 variables
Debut
    s := 0
    Pour i de 1 a n faire
        s := s + T[i]
    Fpour
    Retourner s
Fin
// Espace : O(1) - independant de n
```

#### O(n) - Espace lineaire

```
Fonction Copie(T : Tableau, n : entier) : Tableau
Var R : Tableau[1..n]    // Tableau supplementaire de taille n
Debut
    Pour i de 1 a n faire
        R[i] := T[i]
    Fpour
    Retourner R
Fin
// Espace : O(n)
```

#### Recursivite

Chaque appel recursif utilise de l'espace sur la pile.

```
Fonction Fact(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        Retourner n * Fact(n-1)    // n appels empiles
    Fsi
Fin
// Espace : O(n) - profondeur de recursion
```

---

## 6. Meilleur, moyen et pire cas

### Trois scenarios

| Cas | Description | Exemple (recherche sequentielle) |
|-----|-------------|----------------------------------|
| **Meilleur** | Situation la plus favorable | Element en 1ere position |
| **Moyen** | Situation typique/moyenne | Element au milieu |
| **Pire** | Situation la plus defavorable | Element absent ou en derniere position |

### Exemple : Recherche sequentielle

```
Fonction Recherche(T : Tableau, n, x : entier) : entier
Var i : entier
Debut
    i := 1
    Tant que i <= n ET T[i] <> x faire
        i := i + 1
    Fait
    Si i <= n Alors
        Retourner i
    Sinon
        Retourner 0
    Fsi
Fin
```

| Cas | Complexite | Situation |
|-----|------------|-----------|
| Meilleur | O(1) | x est en T[1] |
| Moyen | O(n/2) = O(n) | x est au milieu |
| Pire | O(n) | x n'est pas dans T |

### Exemple : Tri par insertion

| Cas | Complexite | Situation |
|-----|------------|-----------|
| Meilleur | O(n) | Tableau deja trie |
| Moyen | O(n²) | Ordre aleatoire |
| Pire | O(n²) | Tableau trie en ordre inverse |

---

## 7. Complexite des algorithmes vus

### Recherche

| Algorithme | Meilleur | Moyen | Pire | Espace |
|------------|----------|-------|------|--------|
| Recherche sequentielle | O(1) | O(n) | O(n) | O(1) |
| Recherche dichotomique | O(1) | O(log n) | O(log n) | O(1) |

### Tri

| Algorithme | Meilleur | Moyen | Pire | Espace | Stable |
|------------|----------|-------|------|--------|--------|
| Selection | O(n²) | O(n²) | O(n²) | O(1) | Non |
| Insertion | O(n) | O(n²) | O(n²) | O(1) | Oui |
| Bulles | O(n) | O(n²) | O(n²) | O(1) | Oui |
| Fusion | O(n log n) | O(n log n) | O(n log n) | O(n) | Oui |
| Rapide | O(n log n) | O(n log n) | O(n²) | O(log n) | Non |
| Denombrement | O(n+k) | O(n+k) | O(n+k) | O(k) | Oui |

### Structures de donnees

| Operation | Tableau | Liste chainee | Pile/File |
|-----------|---------|---------------|-----------|
| Acces par indice | O(1) | O(n) | - |
| Recherche | O(n) | O(n) | - |
| Insertion debut | O(n) | O(1) | O(1) |
| Insertion fin | O(1) | O(n) ou O(1)* | O(1) |
| Suppression | O(n) | O(1)** | O(1) |

*O(1) avec pointeur last | **Si on a le pointeur vers le noeud

---

## 8. Optimisation et compromis

### Compromis temps-espace

Parfois on peut echanger de la memoire contre du temps (et vice-versa).

| Approche | Temps | Espace | Exemple |
|----------|-------|--------|---------|
| Recalculer | Plus lent | Moins | Fibonacci recursif |
| Memoisation | Plus rapide | Plus | Fibonacci avec cache |

### Exemple : Fibonacci

```
// Sans memoisation : O(2^n) temps, O(n) espace
Fonction Fib(n) : entier
    Si n <= 1 Alors Retourner n
    Sinon Retourner Fib(n-1) + Fib(n-2)
Fin

// Avec memoisation : O(n) temps, O(n) espace
Fonction Fib_Memo(n, cache) : entier
    Si cache[n] existe Alors Retourner cache[n]
    Si n <= 1 Alors Retourner n
    cache[n] := Fib_Memo(n-1) + Fib_Memo(n-2)
    Retourner cache[n]
Fin

// Iteratif : O(n) temps, O(1) espace
Fonction Fib_Iter(n) : entier
    a := 0; b := 1
    Pour i de 2 a n faire
        c := a + b
        a := b
        b := c
    Fpour
    Retourner b
Fin
```

---

## 9. Importance pratique

### Temps d'execution estimee

Pour un ordinateur executant 10^9 operations/seconde :

| n | O(n) | O(n log n) | O(n²) | O(2^n) |
|---|------|------------|-------|--------|
| 10 | 0.00001 ms | 0.00003 ms | 0.0001 ms | 0.001 ms |
| 100 | 0.0001 ms | 0.0007 ms | 0.01 ms | 10^17 annees |
| 1 000 | 0.001 ms | 0.01 ms | 1 ms | - |
| 10 000 | 0.01 ms | 0.13 ms | 100 ms | - |
| 100 000 | 0.1 ms | 1.7 ms | 10 s | - |
| 1 000 000 | 1 ms | 20 ms | 17 min | - |

### Regles pratiques

1. **O(n²) acceptable** pour n < 10 000
2. **O(n log n) necessaire** pour n > 100 000
3. **O(2^n) inutilisable** pour n > 30

### Application COBOL/Mainframe

Dans le contexte batch :
- Fichiers de **millions** d'enregistrements
- Un algorithme O(n²) peut prendre des **heures**
- Preferer les algorithmes O(n) ou O(n log n)
- Le **SORT JCL** utilise des algorithmes optimises O(n log n)

---

## 10. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-05-complexite.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-05/)

---

*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
