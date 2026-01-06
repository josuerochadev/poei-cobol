# Module Algorithmique & Programmation Structurée

**POEI Développeur COBOL Grand Système** - POE00529
**Dates** : du 4 Nov. 2025 au 7 Nov. 2025 (SE25-205699)

---

## Objectifs du module

- Comprendre les concepts fondamentaux de l'algorithmique
- Maîtriser les structures de contrôle (conditionnelles et itératives)
- Savoir concevoir et analyser des algorithmes
- Préparer la transition vers la programmation COBOL structurée

---

## Chapitres

| # | Chapitre | Description |
|---|----------|-------------|
| 1 | [Introduction à l'Algorithmique](01-introduction-algorithmique.md) | Variables, types, structures de contrôle, opérateurs |
| 2.1 | [Structures de Données](02-01-structures-donnees.md) | Tableaux, recherche, enregistrements, chaînes |
| 2.2 | [Pointeurs et Listes Chaînées](02-02-pointeurs-listes.md) | Pointeurs, allocation dynamique, listes |
| 2.3 | [Piles et Files (TAD)](02-03-piles-files.md) | LIFO, FIFO, implementations statiques et dynamiques |
| 3 | [Récursivité](03-recursivite.md) | Fonctions récursives, cas de base, types de récursion |
| 4 | [Algorithmes de Tri](04-algorithmes-tri.md) | Sélection, insertion, bulles, fusion, rapide |
| 5 | [Complexité Algorithmique](05-complexite-algorithmique.md) | Notation Big-O, analyse temps/espace |
| 6 | [Algorithmes sur Fichiers](06-algorithmes-fichiers.md) | Lecture, rupture, fusion, appareillement, mise à jour |
| 7 | [Modularité](07-modularite.md) | Procédures, fonctions, paramètres, portée |

---

## Correspondance Algorithme → COBOL

| Algorithmique | COBOL |
|---------------|-------|
| `Lire(x)` | `ACCEPT x` |
| `Ecrire(x)` | `DISPLAY x` |
| `x := valeur` | `MOVE valeur TO x` |
| `Si...Alors...finSi` | `IF...END-IF` |
| `Pour i de 1 à N` | `PERFORM VARYING i FROM 1 BY 1 UNTIL i > N` |
| `TantQue...finTantQue` | `PERFORM UNTIL NOT condition` |
| `Repeter...Jusqu'a` | `PERFORM UNTIL condition` |

---

## Ressources

- [Exercices théoriques (QCM)](../../exercices/algorithmique/theorie/)
- [Exercices pratiques](../../exercices/algorithmique/pratique/)

---

## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [VSAM](../vsam/README.md) | [COBOL](../cobol/README.md) |

---
*Formation Algorithmique - M2i Formation*
