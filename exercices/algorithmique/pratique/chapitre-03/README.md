# Chapitre 03 - Recursivite

## Objectifs

- Comprendre le principe de la recursivite
- Identifier les cas de base et les cas recursifs
- Maitriser les schemas recursifs classiques
- Appliquer le paradigme "Diviser pour regner"
- Analyser la complexite des algorithmes recursifs

---

## Principes fondamentaux

### Structure d'une fonction recursive

```text
FONCTION Recursive (parametres)
DEBUT
    Si CAS_DE_BASE alors
        Retourner VALEUR_DIRECTE
    Sinon
        // Reduction du probleme
        Retourner COMBINER(element, Recursive(sous_probleme))
    Fsi
FIN
```

### Les trois regles d'or

1. **Cas de base** : Condition d'arret obligatoire
2. **Progression** : Chaque appel doit se rapprocher du cas de base
3. **Confiance** : Supposer que les appels recursifs fonctionnent

---

## Liste des Exercices

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 1 | Operations sur ensembles | Nbr_X, Inverser, Increment, RDP | Fondamental | `ex01-operations-ensembles.md` |
| 2 | Suites recursives | Valide, Croissante, Extraite | Fondamental | `ex02-suites-recursives.md` |
| 3 | Fonction d'Ackermann | Recursivite non primitive | Avance | `ex03-ackermann.md` |
| 4 | Palindrome recursif | Chaines, espaces, casse | Intermediaire | `ex04-palindrome-recursif.md` |
| 5 | Tours de Hanoi | Probleme classique | Intermediaire | `ex05-tours-hanoi.md` |
| 6 | Tableaux recursifs | Pair, Alterne, Symetrique | Intermediaire | `ex06-tableaux-recursifs.md` |
| 7 | Calculs numeriques | Nb_Chiffres, Modulo, PGCD | Intermediaire | `ex07-calculs-numeriques.md` |
| 8 | Diviser pour regner | Dichotomie, MinMax, Tri fusion | Avance | `ex08-diviser-regner.md` |

---

## Progression recommandee

### Niveau Fondamental
1. **Exercice 1** : Operations de base sur tableaux
2. **Exercice 2** : Validation et comparaison de suites

### Niveau Intermediaire
3. **Exercice 4** : Palindromes (manipulation de chaines)
4. **Exercice 5** : Tours de Hanoi (probleme classique)
5. **Exercice 6** : Proprietes des tableaux
6. **Exercice 7** : Calculs numeriques recursifs

### Niveau Avance
7. **Exercice 3** : Ackermann (recursivite complexe)
8. **Exercice 8** : Diviser pour regner

---

## Resume des algorithmes cles

### Exercice 1 : Operations sur ensembles

```text
Nbr_X(T, d, f, x)     // Compter les occurrences
Inverser(T, d, f)     // Inverser un tableau
Increment(T, d, f)    // Incrementer chaque element
RDP(T, d, f)          // Rang du dernier positif
```

### Exercice 2 : Suites

```text
Valide(A, d, f)       // Elements uniques et positifs
Croissante(A, d, f)   // Suite croissante
Extraite(A, B, ...)   // B sous-suite de A
```

### Exercice 3 : Ackermann

```text
A(0, n) = n + 1
A(m, 0) = A(m-1, 1)
A(m, n) = A(m-1, A(m, n-1))
```

### Exercice 5 : Tours de Hanoi

```text
Hanoi(n, dep, arr, inter)
  Si n > 0 alors
    Hanoi(n-1, dep, inter, arr)
    Deplacer n de dep vers arr
    Hanoi(n-1, inter, arr, dep)
```

### Exercice 8 : Recherche dichotomique

```text
Recherche_Dicho(T, d, f, X)
  milieu := (d + f) / 2
  Si T[milieu] = X alors TROUVE
  Sinon Si X < T[milieu] -> chercher gauche
  Sinon -> chercher droite
```

---

## Complexites

| Algorithme | Temps | Espace (pile) |
|------------|-------|---------------|
| Recherche lineaire | O(n) | O(n) |
| Recherche dichotomique | O(log n) | O(log n) |
| Parcours simple | O(n) | O(n) |
| Tri fusion | O(n log n) | O(n) |
| Hanoi | O(2^n) | O(n) |
| Ackermann | Non primitive | Tres grand |
| Fibonacci naif | O(2^n) | O(n) |
| Factorielle | O(n) | O(n) |

---

## Schemas de recursivite

### Schema 1 : Parcours lineaire (premier element)

```text
f(T, d, f) = cas_base                    si d > f
           = combiner(T[d], f(T, d+1, f)) sinon
```

### Schema 2 : Parcours lineaire (dernier element)

```text
f(T, d, f) = cas_base                    si d > f
           = combiner(T[f], f(T, d, f-1)) sinon
```

### Schema 3 : Diviser pour regner

```text
f(T, d, f) = cas_base                    si d >= f
           = combiner(f(T, d, m), f(T, m+1, f)) sinon
           ou m = (d+f)/2
```

### Schema 4 : Double pointeur

```text
f(T, d, f) = cas_base                    si d >= f
           = combiner(T[d], T[f], f(T, d+1, f-1)) sinon
```

---

## Conseils de resolution

1. **Identifier le cas de base** en premier
2. **Verifier la progression** vers le cas de base
3. **Tester avec des petits exemples** (n=0, n=1, n=2)
4. **Tracer l'execution** pour comprendre les appels
5. **Attention aux appels multiples** (ex: Fibonacci naif)

---

## Erreurs courantes

| Erreur | Consequence | Solution |
|--------|-------------|----------|
| Pas de cas de base | Recursion infinie | Toujours definir un arret |
| Mauvaise reduction | Recursion infinie | Verifier d+1 ou f-1 |
| Appels redondants | Complexite exponentielle | Memoisation |
| Pile trop profonde | Stack overflow | Iteration ou tail recursion |

---

## Correspondance avec les cahiers

Ce chapitre correspond au **Cahier 7** (Recursivite) qui regroupe :
- Cahier 5a : Operations sur ensembles et suites, Ackermann, Palindrome, Hanoi
- Cahier 5b : Tableaux recursifs, calculs numeriques
- Cahier 5c : Diviser pour regner

---

*Exercices Algorithmique Chapitre 03 - M2i Formation*
