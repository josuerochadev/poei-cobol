# Chapitre III - Recursivite

## 1. Introduction a la recursivite

### Definition

La **recursivite** est une technique permettant de definir ou resoudre un probleme en le definissant **a partir de lui-meme**.

Une fonction ou une procedure est dite **recursive** lorsqu'elle s'appelle elle-meme.

### Deux visions de la recursivite

1. **Mise en abyme** : Une image contenant une version plus petite d'elle-meme (poupees russes, miroirs face a face)

2. **Baguette magique algorithmique** : "Supposons que je sache resoudre le probleme pour N-1, comment resoudre pour N ?"

### Exemple mathematique : les entiers naturels

Les entiers naturels peuvent etre definis recursivement :
- 0 est un entier naturel (cas de base)
- Le successeur d'un entier naturel est un entier naturel (regle recursive)

Cette definition permet de decrire un **ensemble infini** avec un **nombre fini de regles**.

---

## 2. Exemple fondamental : la factorielle

### Definition mathematique

```
0! = 1                    (cas de base)
n! = n × (n-1)!  pour n > 0   (formule de recurrence)
```

### Deroulement de 4!

```
4! = 4 × 3!
   = 4 × (3 × 2!)
   = 4 × (3 × (2 × 1!))
   = 4 × (3 × (2 × (1 × 0!)))
   = 4 × (3 × (2 × (1 × 1)))
   = 4 × (3 × (2 × 1))
   = 4 × (3 × 2)
   = 4 × 6
   = 24
```

### Fonction recursive

```
Fonction Factorielle(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1                     // Cas de base
    Sinon
        Retourner n * Factorielle(n-1)  // Appel recursif
    Fsi
Fin
```

### Representation de la pile d'appels

```
Factorielle(4)
    └── Factorielle(3)
            └── Factorielle(2)
                    └── Factorielle(1)
                            └── Factorielle(0) → retourne 1
                        ← retourne 1 × 1 = 1
                ← retourne 2 × 1 = 2
        ← retourne 3 × 2 = 6
    ← retourne 4 × 6 = 24
```

---

## 3. Pourquoi utiliser la recursivite ?

### Avantages

| Avantage | Description |
|----------|-------------|
| Simplicite conceptuelle | Code souvent plus court et elegant |
| Preuve formelle | Plus facile a demontrer (induction) |
| Structures recursives | Naturel pour arbres, listes, graphes |
| Diviser pour regner | Decomposition en sous-problemes |

### Quand l'utiliser ?

- Lorsque le probleme se decompose en **sous-problemes similaires**
- Lorsque les donnees sont **naturellement recursives** :
  - Chaines de caracteres
  - Listes chainees
  - Arbres
  - Expressions mathematiques

### Quand l'eviter ?

- Si une solution iterative **simple** existe
- Si la profondeur de recursion est trop grande (risque de debordement de pile)
- Si les memes sous-problemes sont recalcules plusieurs fois (sans memoisation)

---

## 4. Methodologie de conception recursive

### Demarche en 4 etapes

1. **Decomposer** le probleme en sous-problemes plus petits
2. **Resoudre** les sous-problemes (appels recursifs)
3. **Identifier** les cas de base (condition d'arret)
4. **Combiner** les solutions des sous-problemes

### Les 3 criteres indispensables

| Critere | Description | Exemple (factorielle) |
|---------|-------------|----------------------|
| Formule de recurrence | Comment se decompose le probleme | n! = n × (n-1)! |
| Condition d'arret | Quand s'arreter | n = 0 |
| Regle de combinaison | Comment combiner les resultats | Multiplication |

---

## 5. Regles de conception

### Regle 1 : Cas de base obligatoire

Tout algorithme recursif doit posseder **au moins un cas** sans appel recursif.

```
// INCORRECT - Pas de cas de base
Fonction Infini(n : entier) : entier
Debut
    Retourner Infini(n-1)    // Boucle infinie !
Fin

// CORRECT - Avec cas de base
Fonction Correct(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1          // Cas de base
    Sinon
        Retourner Correct(n-1)
    Fsi
Fin
```

### Regle 2 : Progression vers l'arret

Chaque appel recursif doit **rapprocher** des conditions d'arret.

```
// INCORRECT - Ne progresse pas
Fonction Bloque(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        Retourner Bloque(n)  // n ne change pas !
    Fsi
Fin

// CORRECT - Progresse vers 0
Fonction Progresse(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        Retourner Progresse(n-1)  // n diminue
    Fsi
Fin
```

---

## 6. Types de recursivite

### 6.1 Recursivite simple (lineaire)

Un seul appel recursif par execution.

```
Fonction Somme(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 0
    Sinon
        Retourner n + Somme(n-1)    // Un seul appel
    Fsi
Fin
```

### 6.2 Recursivite terminale

L'appel recursif est la **derniere operation** de la fonction.

```
// Recursivite NON terminale
Fonction Fact(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        Retourner n * Fact(n-1)     // Multiplication APRES l'appel
    Fsi
Fin

// Recursivite terminale (avec accumulateur)
Fonction Fact_term(n : entier, acc : entier) : entier
Debut
    Si n = 0 Alors
        Retourner acc
    Sinon
        Retourner Fact_term(n-1, n * acc)   // Rien apres l'appel
    Fsi
Fin
```

**Avantage** : Peut etre optimisee par le compilateur (pas de pile).

### 6.3 Recursivite multiple (arborescente)

Plusieurs appels recursifs par execution.

```
Fonction Fibonacci(n : entier) : entier
Debut
    Si n <= 1 Alors
        Retourner n
    Sinon
        Retourner Fibonacci(n-1) + Fibonacci(n-2)   // Deux appels
    Fsi
Fin
```

**Attention** : Complexite exponentielle sans optimisation !

### 6.4 Recursivite croisee (mutuelle)

Deux fonctions s'appellent mutuellement.

```
Fonction Pair(n : entier) : booleen
Debut
    Si n = 0 Alors
        Retourner vrai
    Sinon
        Retourner Impair(n-1)
    Fsi
Fin

Fonction Impair(n : entier) : booleen
Debut
    Si n = 0 Alors
        Retourner faux
    Sinon
        Retourner Pair(n-1)
    Fsi
Fin
```

### 6.5 Recursivite imbriquee

L'argument de l'appel recursif contient un appel recursif.

```
Fonction Ackermann(m, n : entier) : entier
Debut
    Si m = 0 Alors
        Retourner n + 1
    SinonSi n = 0 Alors
        Retourner Ackermann(m-1, 1)
    Sinon
        Retourner Ackermann(m-1, Ackermann(m, n-1))   // Imbrique
    Fsi
Fin
```

---

## 7. Exemples classiques

### 7.1 Puissance rapide

Calcul de a^n en O(log n) au lieu de O(n).

```
Fonction Puissance(a, n : entier) : entier
Var u : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        u := Puissance(a, n DIV 2)
        Si (n MOD 2) = 0 Alors
            Retourner u * u           // n pair
        Sinon
            Retourner u * u * a       // n impair
        Fsi
    Fsi
Fin
```

**Principe** : a^n = (a^(n/2))^2 si n pair, a × (a^(n/2))^2 si n impair.

### 7.2 PGCD (Euclide)

```
Fonction PGCD(a, b : entier) : entier
Var r : entier
Debut
    r := a MOD b
    Si r = 0 Alors
        Retourner b           // Cas de base
    Sinon
        Retourner PGCD(b, r)  // Appel recursif
    Fsi
Fin
```

### 7.3 Somme d'un tableau (4 versions)

#### Version 1 : De la fin vers le debut

```
Fonction Somme(T : Tableau, N : entier) : entier
Debut
    Si N = 0 Alors
        Retourner 0
    Sinon
        Retourner T[N] + Somme(T, N-1)
    Fsi
Fin
```

#### Version 2 : Du debut vers la fin

```
Fonction Somme(T : Tableau, d, f : entier) : entier
Debut
    Si d > f Alors
        Retourner 0
    Sinon
        Retourner T[d] + Somme(T, d+1, f)
    Fsi
Fin
```

#### Version 3 : Des deux cotes

```
Fonction Somme(T : Tableau, d, f : entier) : entier
Debut
    Si d = f Alors
        Retourner T[d]
    Sinon
        Retourner T[d] + Somme(T, d+1, f-1) + T[f]
    Fsi
Fin
```

#### Version 4 : Diviser pour regner

```
Fonction Somme(T : Tableau, d, f : entier) : entier
Var m : entier
Debut
    Si d > f Alors
        Retourner 0
    Sinon
        m := (d + f) DIV 2
        Retourner Somme(T, d, m) + Somme(T, m+1, f)
    Fsi
Fin
```

### 7.4 Recherche dichotomique recursive

```
Fonction Recherche_Dicho(T : Tableau, d, f, x : entier) : entier
Var m : entier
Debut
    Si d > f Alors
        Retourner 0    // Non trouve
    Sinon
        m := (d + f) DIV 2
        Si T[m] = x Alors
            Retourner m
        SinonSi T[m] < x Alors
            Retourner Recherche_Dicho(T, m+1, f, x)
        Sinon
            Retourner Recherche_Dicho(T, d, m-1, x)
        Fsi
    Fsi
Fin
```

---

## 8. Recursivite vs Iteration

### Comparaison

| Aspect | Recursivite | Iteration |
|--------|-------------|-----------|
| Lisibilite | Souvent plus claire | Parfois plus verbose |
| Performance | Overhead d'appels | Generalement plus rapide |
| Memoire | Utilise la pile | Memoire constante |
| Debordement | Risque de stack overflow | Pas de risque |
| Structures recursives | Naturel | Necessite une pile explicite |

### Conversion recursif → iteratif

Toute fonction recursive peut etre convertie en version iterative (en utilisant une pile explicite si necessaire).

```
// Version recursive
Fonction Fact_rec(n : entier) : entier
Debut
    Si n = 0 Alors
        Retourner 1
    Sinon
        Retourner n * Fact_rec(n-1)
    Fsi
Fin

// Version iterative
Fonction Fact_iter(n : entier) : entier
Var i, resultat : entier
Debut
    resultat := 1
    Pour i de 1 a n faire
        resultat := resultat * i
    Fpour
    Retourner resultat
Fin
```

---

## 9. Correspondance avec COBOL

En COBOL, la recursivite est supportee depuis COBOL-85, mais rarement utilisee dans les programmes legacy.

| Algorithmique | COBOL |
|---------------|-------|
| Fonction recursive | `PROGRAM-ID. xxx RECURSIVE.` |
| Appel recursif | `CALL xxx` |
| Variables locales | `LOCAL-STORAGE SECTION` |

### Exemple COBOL recursif

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. FACTORIELLE RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01 WS-N PIC 9(4).
01 WS-RESULT PIC 9(10).

LINKAGE SECTION.
01 LS-N PIC 9(4).
01 LS-RESULT PIC 9(10).

PROCEDURE DIVISION USING LS-N LS-RESULT.
    IF LS-N = 0
        MOVE 1 TO LS-RESULT
    ELSE
        SUBTRACT 1 FROM LS-N GIVING WS-N
        CALL 'FACTORIELLE' USING WS-N WS-RESULT
        MULTIPLY LS-N BY WS-RESULT GIVING LS-RESULT
    END-IF.
    STOP RUN.
```

**Note** : En pratique, on prefere souvent l'approche iterative en COBOL pour des raisons de performance et de compatibilite.

---

## 10. Pieges et bonnes pratiques

### Pieges courants

1. **Oubli du cas de base** → Boucle infinie
2. **Mauvaise progression** → Ne converge pas vers l'arret
3. **Recalculs inutiles** → Fibonacci naif est en O(2^n)
4. **Profondeur excessive** → Stack overflow

### Bonnes pratiques

1. **Toujours definir le cas de base en premier**
2. **Verifier la progression vers l'arret**
3. **Utiliser la memoisation** pour eviter les recalculs
4. **Preferer la recursivite terminale** quand possible
5. **Tester avec des valeurs limites** (0, 1, negatifs)

---

## 11. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-03-recursivite.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-03/)

---

*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
