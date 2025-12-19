# Chapitre 02-02 - Pointeurs et Listes Chainees

## Objectifs

- Comprendre les pointeurs et l'allocation dynamique
- Maitriser les listes chainees simples
- Implementer des operations sur listes (insertion, recherche, parcours)
- Utiliser les listes pour representer des structures complexes
- Implementer des operations ensemblistes (union, intersection, inclusion)
- Maitriser la recursivite sur les listes chainees

---

## Types de donnees utilises

### Liste chainee simple

```text
Type Elmt = enregistrement
    Val : entier
    Next : ^Elmt       // Pointeur vers l'element suivant
fin enregistrement

Type Liste = ^Elmt     // Pointeur vers le premier element
```

### Liste chainee de mots (phrase)

```text
Type mot = enregistrement
    Texte : chaine
    Next : ^mot
fin enregistrement

Type phrase = ^mot
```

### Ensemble simple

```text
Type simple_Ens = ^Elmt    // Chaque element apparait une fois
```

### Multi-ensemble

```text
Type ElmtMulti = enregistrement
    Val : entier
    Nbr : entier       // Nombre d'occurrences
    Next : ^ElmtMulti
fin enregistrement

Type multi_Ens = ^ElmtMulti
```

---

## Primitives de base

```text
Creer(p)      // Alloue memoire et retourne pointeur dans p
Liberer(p)    // Libere la memoire pointee par p
p := NIL      // Pointeur nul (fin de liste)
p^.Val        // Acces a la valeur de l'element pointe
p^.Next       // Acces au pointeur suivant
```

---

## Liste des Exercices

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 1 | Phrase liste | Liste de mots, Trouver_mot, Remplacer_mot | Intermediaire | `ex01-phrase-liste.md` |
| 2 | Ensembles | Union, Intersection, Inclusion, Multi-ensembles | Avance | `ex02-ensembles-liste.md` |
| 3 | Recursivite | plus_courte, Double, apparait, croissante | Avance | `ex03-recursif-liste.md` |

---

## Progression Recommandee

1. **Niveau Intermediaire** : Exercice 1 (manipulation de base)
2. **Niveau Avance** : Exercices 2, 3 (operations complexes et recursivite)

---

## Resume des Complexites

| Algorithme | Exercice | Complexite |
|------------|----------|------------|
| Parcours liste | Ex 1, 3 | O(n) |
| Recherche dans liste | Ex 1, 2 | O(n) |
| Union/Intersection | Ex 2 | O(n x m) |
| Inclusion | Ex 2 | O(n x m) |
| Fonctions recursives | Ex 3 | O(n) temps, O(n) pile |

---

## Concepts cles

### Parcours iteratif

```text
courant := tete
Tantque courant <> NIL faire
    Traiter(courant^.Val)
    courant := courant^.Next
Ftantque
```

### Insertion en tete

```text
PROCEDURE Insert_tete (x: entier, var L: Liste)
VAR nouveau : ^Elmt
Debut
    Creer(nouveau)
    nouveau^.Val := x
    nouveau^.Next := L
    L := nouveau
Fin
```

### Structure recursive

```text
FONCTION recursive (L: Liste) : TypeRetour
Debut
    Si L = NIL Alors
        Retourner valeur_base    // Cas de base
    Sinon
        Retourner combiner(L^.Val, recursive(L^.Next))  // Cas recursif
    Fsi
Fin
```

---

*Exercices Algorithmique Chapitre 02-02 - M2i Formation*
