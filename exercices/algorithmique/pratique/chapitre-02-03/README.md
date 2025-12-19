# Chapitre 02-03 - Piles et Files

## Objectifs

- Comprendre le fonctionnement des piles (LIFO - Last In First Out)
- Comprendre le fonctionnement des files (FIFO - First In First Out)
- Maitriser les operations primitives sur piles et files
- Appliquer ces structures a des problemes concrets
- Implementer des algorithmes de tri et de traitement de texte

---

## Types de donnees utilises

### Pile (Stack)

```text
Type Pile = enregistrement
    Tab : tableau [1..MAX] de element
    Sommet : entier    // Indice du sommet (0 si vide)
fin enregistrement
```

### File (Queue)

```text
Type File = enregistrement
    Tab : tableau [1..MAX] de element
    Tete : entier      // Indice du premier element
    Queue : entier     // Indice du dernier element
fin enregistrement
```

---

## Primitives de base

### Operations sur les piles

```text
Empiler(P, x)     // Ajoute x au sommet de la pile
Depiler(P)        // Retire et retourne l'element au sommet
Sommet(P)         // Retourne l'element au sommet sans le retirer
pile_vide(P)      // Retourne vrai si la pile est vide
```

### Operations sur les files

```text
Init_file(F)      // Initialise une file vide
Enfiler(F, x)     // Ajoute x en fin de file (queue)
Defiler(F)        // Retire et retourne l'element en tete
Vide(F)           // Retourne vrai si la file est vide
```

---

## Liste des Exercices

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 1 | Reconnaissance de chaine | Pile, validation S*S' | Intermediaire | `ex01-reconnaissance-pile.md` |
| 2 | Palindrome | Pile, 2 versions (complete/optimisee) | Intermediaire | `ex02-palindrome-pile.md` |
| 3 | Editeur de texte | Deux piles (G et D), curseur | Avance | `ex03-editeur-pile.md` |
| 4 | Tri FIFO | File, fusion de listes, merge sort | Avance | `ex04-tri-fifo.md` |

---

## Progression Recommandee

1. **Niveau Intermediaire** : Exercices 1, 2 (manipulation de base des piles)
2. **Niveau Avance** : Exercices 3, 4 (applications complexes)

---

## Resume des Exercices

### Exercice 1 : Reconnaissance de chaine

Verifier si une chaine est de la forme `S*S'` ou S' est l'inverse de S.

```text
"ab2c*c2ba" -> VALIDE
"abc*abc"   -> INVALIDE
```

### Exercice 2 : Palindrome

Verifier si un mot se lit de la meme facon dans les deux sens.

- **Version 1** : Empiler toute la chaine, puis comparer
- **Version 2** : Empiler seulement la moitie (optimisee)

### Exercice 3 : Editeur de texte

Representer un texte avec curseur a l'aide de deux piles :

- **G** : caracteres avant le curseur
- **D** : caracteres apres le curseur

Operations : Inserer, Effacer, Avancer, Reculer, Rechercher, BackSpace (#), Clear (%)

### Exercice 4 : Tri FIFO

Trier un ensemble de nombres par fusion iterative avec une file :

1. Chaque nombre devient une liste a un element
2. Fusionner deux listes, remettre le resultat dans la file
3. Repeter jusqu'a obtenir une seule liste triee

---

## Resume des Complexites

| Algorithme | Exercice | Temps | Espace |
|------------|----------|-------|--------|
| Reconnaissance S*S' | Ex 1 | O(n) | O(n/2) |
| Palindrome V1 | Ex 2 | O(n) | O(n) |
| Palindrome V2 | Ex 2 | O(n) | O(n/2) |
| Editeur - operations simples | Ex 3 | O(1) | - |
| Editeur - recherche | Ex 3 | O(n) | - |
| Tri FIFO | Ex 4 | O(n log n) | O(n) |

---

## Concepts cles

### Pile - Principe LIFO

```text
Empiler(A), Empiler(B), Empiler(C)

    +---+
    | C |  <- Sommet (dernier entre, premier sorti)
    | B |
    | A |
    +---+

Depiler() retourne C
Depiler() retourne B
Depiler() retourne A
```

### File - Principe FIFO

```text
Enfiler(A), Enfiler(B), Enfiler(C)

Tete -> [A] [B] [C] <- Queue

Defiler() retourne A (premier entre, premier sorti)
Defiler() retourne B
Defiler() retourne C
```

### Applications typiques

| Structure | Applications |
|-----------|-------------|
| **Pile** | Evaluation d'expressions, appels de fonctions, undo/redo, backtracking |
| **File** | Gestion de processus, impression, BFS (parcours en largeur), buffers |

---

## Schema comparatif

```text
PILE (LIFO)                    FILE (FIFO)

    +---+                      Tete          Queue
    | 3 | <- Empiler/Depiler   [1] [2] [3] <- Enfiler
    | 2 |                       ^
    | 1 |                       Defiler
    +---+

Ordre de sortie: 3, 2, 1      Ordre de sortie: 1, 2, 3
```

---

*Exercices Algorithmique Chapitre 02-03 - M2i Formation*
