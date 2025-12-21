# Chapitre I - Introduction à l'Algorithmique

## 1. Qu'est-ce qu'un algorithme ?

Un **algorithme** est une suite finie et ordonnée d'instructions permettant de résoudre un problème ou d'accomplir une tâche. Chaque instruction doit être :
- **Non ambiguë** : une seule interprétation possible
- **Exécutable** : réalisable en un temps fini
- **Déterministe** : même entrée = même résultat

### Structure générale d'un algorithme

```
Algorithme NomAlgorithme
var declarations : Types

DEBUT
    instructions
FIN
```

---

## 2. Variables et Types de données

### Types de base

| Type | Description | Exemples |
|------|-------------|----------|
| **Entier** | Nombres entiers (positifs ou négatifs) | -5, 0, 42, 1000 |
| **Réel** | Nombres à virgule | 3.14, -0.5, 2.718 |
| **Caractère** | Un seul symbole | 'A', '7', '+' |
| **Chaîne** | Suite de caractères | "Bonjour", "COBOL" |
| **Booléen** | Valeur logique | vrai, faux |

### Déclaration de variables

```
var i, j : Entier
    x, y : Réel
    nom : Chaîne
    trouve : Booléen
```

---

## 3. Opérateurs

### Opérateurs arithmétiques

| Opérateur | Signification | Exemple | Résultat |
|-----------|---------------|---------|----------|
| `+` | Addition | `5 + 3` | `8` |
| `-` | Soustraction | `5 - 3` | `2` |
| `*` | Multiplication | `5 * 3` | `15` |
| `/` | Division | `5 / 2` | `2.5` |
| `DIV` | Division entière | `5 DIV 2` | `2` |
| `MOD` | Reste (modulo) | `5 MOD 2` | `1` |

### Opérateurs de comparaison

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `=` | Égal à | `x = 5` |
| `<>` | Différent de | `x <> 0` |
| `<` | Inférieur à | `x < 10` |
| `>` | Supérieur à | `x > 0` |
| `<=` | Inférieur ou égal | `x <= 100` |
| `>=` | Supérieur ou égal | `x >= 1` |

### Opérateurs logiques

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `ET` | Conjonction (AND) | `(x > 0) ET (x < 10)` |
| `OU` | Disjonction (OR) | `(x = 0) OU (x = 1)` |
| `NON` | Négation (NOT) | `NON trouve` |

---

## 4. Instructions de base

### Affectation

L'affectation attribue une valeur à une variable.

```
x := 5          // x reçoit la valeur 5
x ← 5           // notation alternative
y := x + 3      // y reçoit la valeur de x + 3
```

### Lecture (entrée)

```
Lire(x)                  // lit une valeur dans x
Lire(a, b, c)            // lit trois valeurs
```

### Écriture (sortie)

```
Ecrire(x)                        // affiche la valeur de x
Ecrire("Résultat : ", x)         // affiche un message suivi de x
Ecrire("Somme = " & S)           // concaténation avec &
```

---

## 5. Structures conditionnelles

### Si...Alors...finSi

Exécute un bloc d'instructions si la condition est vraie.

```
Si condition Alors
    instructions
finSi
```

**Exemple** :
```
Si x > 0 Alors
    Ecrire("x est positif")
finSi
```

### Si...Alors...SiNon...finSi

Exécute un bloc ou un autre selon la condition.

```
Si condition Alors
    instructions_si_vrai
SiNon
    instructions_si_faux
finSi
```

**Exemple** :
```
Si x MOD 2 = 0 Alors
    Ecrire("x est pair")
SiNon
    Ecrire("x est impair")
finSi
```

### Si...SiNonSi...finSi

Enchaînement de conditions multiples.

```
Si condition1 Alors
    instructions1
SiNonSi condition2 Alors
    instructions2
SiNonSi condition3 Alors
    instructions3
SiNon
    instructions_defaut
finSi
```

**Exemple** :
```
Si note >= 16 Alors
    Ecrire("Très bien")
SiNonSi note >= 14 Alors
    Ecrire("Bien")
SiNonSi note >= 12 Alors
    Ecrire("Assez bien")
SiNonSi note >= 10 Alors
    Ecrire("Passable")
SiNon
    Ecrire("Insuffisant")
finSi
```

---

## 6. Structures itératives (boucles)

### Boucle Pour

Utilisée quand le **nombre d'itérations est connu** à l'avance.

```
Pour i de debut à fin Faire
    instructions
finPour
```

**Avec un pas différent de 1** :
```
Pour i de debut à fin pas p Faire
    instructions
finPour
```

**Exemples** :
```
// Afficher les nombres de 1 à 10
Pour i de 1 à 10 Faire
    Ecrire(i)
finPour

// Afficher les nombres pairs de 2 à 20
Pour i de 2 à 20 pas 2 Faire
    Ecrire(i)
finPour
```

### Boucle TantQue

Utilisée quand le **nombre d'itérations est inconnu** mais on connaît la **condition d'arrêt**.
Le test est effectué **avant** chaque itération (peut ne jamais s'exécuter).

```
TantQue condition Faire
    instructions
finTantQue
```

**Exemple** :
```
// Calcul du PGCD par soustractions
TantQue A <> B Faire
    Si A > B Alors
        A := A - B
    SiNon
        B := B - A
    finSi
finTantQue
```

### Boucle Répéter...Jusqu'à

Le test est effectué **après** chaque itération (s'exécute **au moins une fois**).

```
Repeter
    instructions
Jusqu'a condition
```

**Exemple** : Saisie contrôlée
```
Repeter
    Ecrire("Entrez un nombre positif : ")
    Lire(n)
Jusqu'a n > 0
```

### Comparaison des boucles

| Boucle | Test | Exécution minimale | Usage typique |
|--------|------|-------------------|---------------|
| **Pour** | Implicite | 0 fois (si debut > fin) | Nombre d'itérations connu |
| **TantQue** | Avant | 0 fois | Condition d'arrêt connue |
| **Répéter** | Après | 1 fois | Saisie contrôlée, menu |

---

## 7. Patterns algorithmiques courants

### Accumulation (somme, produit)

```
// Somme des n premiers entiers
S := 0                      // Initialisation à l'élément neutre
Pour i de 1 à n Faire
    S := S + i              // Accumulation
finPour
```

```
// Produit (factorielle)
P := 1                      // Initialisation à l'élément neutre
Pour i de 1 à n Faire
    P := P * i
finPour
```

### Comptage

```
// Compter les éléments vérifiant une condition
Compteur := 0
Pour i de 1 à n Faire
    Si condition(i) Alors
        Compteur := Compteur + 1
    finSi
finPour
```

### Recherche de minimum/maximum

```
// Initialisation avec le premier élément
Lire(x)
min := x
max := x

// Comparaison avec les suivants
Pour i de 2 à n Faire
    Lire(x)
    Si x < min Alors
        min := x
    finSi
    Si x > max Alors
        max := x
    finSi
finPour
```

### Saisie contrôlée

```
Repeter
    Lire(valeur)
Jusqu'a valeur >= min ET valeur <= max
```

---

## 8. Rappels mathématiques utiles

### Puissance

```
X^0 = 1
X^1 = X
X^n = X × X × ... × X  (n fois)
```

### Factorielle

```
0! = 1
1! = 1
N! = 1 × 2 × 3 × ... × N
```

### Divisibilité

- `a` est divisible par `b` si `a MOD b = 0`
- Un nombre est pair si `n MOD 2 = 0`
- Un nombre est impair si `n MOD 2 = 1`

### PGCD (Plus Grand Commun Diviseur)

Méthode par soustractions successives :
```
PGCD(a, b) = PGCD(a-b, b)  si a > b
PGCD(a, b) = PGCD(a, b-a)  si b > a
PGCD(a, a) = a
```

---

## 9. Bonnes pratiques

1. **Nommer clairement** les variables (éviter `x`, préférer `compteur`, `total`)
2. **Initialiser** les accumulateurs avant utilisation
3. **Indenter** correctement le code pour la lisibilité
4. **Commenter** les parties complexes
5. **Tester** avec des valeurs limites (0, 1, négatifs, etc.)
6. **Décomposer** les problèmes complexes en sous-problèmes

---

## 10. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices théoriques (QCM)](../../exercices/algorithmique/theorie/qcm-01-introduction.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Module Algorithmique](README.md) | [Chapitre II - Structures de Données](02-01-structures-donnees.md) |

---
*Formation POEI Développeur COBOL Grand Système - M2i Formation*
