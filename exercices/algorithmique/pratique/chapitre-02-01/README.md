# Chapitre 02-01 - Tableaux et Enregistrements

## Objectifs

- Manipuler des tableaux a une dimension (vecteurs)
- Manipuler des matrices (tableaux 2D)
- Maitriser les algorithmes de recherche (lineaire, dichotomique)
- Implementer des algorithmes de tri et fusion
- Definir et utiliser des enregistrements (records)
- Maitriser les structures imbriquees
- Gerer des tableaux d'enregistrements
- Manipuler les chaines de caracteres (recherche, suppression, concatenation)
- Implementer des operations sur chaines (miroir, occurrences, anagrammes)

---

## Types de donnees utilises

### Tableaux

```text
Type_TAB = tableau [1 .. max] de entier      // Tableau 1D d'entiers
Type_TAB = tableau [1 .. max] de caractere   // Tableau 1D de caracteres
Type_TAB = tableau [1 .. max] de Couleur     // Tableau 1D de couleurs
Type_MAT = tableau [1..max, 1..max] de entier // Matrice 2D d'entiers
```

### Enregistrements

```text
Type t_adresse = enregistrement
    numero : entier
    rue : chaine[40]
    codePostal : entier
    ville : chaine[30]
fin enregistrement

Type EMPLOYE = enregistrement
    matricule : entier
    nom : chaine[30]
    adresse : t_adresse    // Structure imbriquee
    salaire : reel
fin enregistrement
```

### Chaines de caracteres

```text
Chaine = enregistrement
    str : tableau [1..MAX] de caractere
    l : entier    // longueur effective
fin enregistrement
```

---

## Liste des Exercices

### Partie A : Tableaux (ex01-08)

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 1 | Min et Max | Parcours, extremums | Debutant | `ex01-min-max.md` |
| 2 | Detection des pics | Voisinage, conditions | Debutant | `ex02-pics.md` |
| 3 | Parcours serpent | Matrice, alternance | Intermediaire | `ex03-serpent.md` |
| 4 | Recherche | Lineaire, dichotomique | Intermediaire | `ex04-recherche.md` |
| 5 | Drapeau neerlandais | Tri 3 partitions | Avance | `ex05-drapeau.md` |
| 6 | Anagrammes | Comparaison chaines | Avance | `ex06-anagrammes.md` |
| 7 | Fusion tableaux | Merge sort | Intermediaire | `ex07-fusion.md` |
| 8 | Frequences | Insertion triee | Avance | `ex08-frequences.md` |

### Partie B : Enregistrements (ex09-11)

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 9 | Gestion employes | Records, structures imbriquees, CRUD | Intermediaire | `ex09-employes.md` |
| 10 | Gestion hotel | Structures imbriquees, recherche multicritere | Avance | `ex10-hotel.md` |
| 11 | Collection pieces | Records, statistiques, agregation | Intermediaire | `ex11-pieces.md` |

### Partie C : Chaines de caracteres (ex12-16)

| # | Exercice | Concepts | Difficulte | Fichier |
|---|----------|----------|------------|---------|
| 12 | SEARCH | Recherche caractere dans chaine | Debutant | `ex12-search-chaine.md` |
| 13 | DELETE | Suppression premiere/toutes occurrences | Intermediaire | `ex13-delete-chaine.md` |
| 14 | CONCAT | Concatenation de chaines | Debutant | `ex14-concat-chaine.md` |
| 15 | OCCUR | Comptage occurrences, anagrammes | Intermediaire | `ex15-occur-chaine.md` |
| 16 | MIROIR | Inversion chaine, palindromes | Intermediaire | `ex16-miroir-chaine.md` |

---

## Progression Recommandee

### Tableaux
1. **Niveau Debutant** : Exercices 1, 2
2. **Niveau Intermediaire** : Exercices 3, 4, 7
3. **Niveau Avance** : Exercices 5, 6, 8

### Enregistrements
1. **Niveau Intermediaire** : Exercices 9, 11
2. **Niveau Avance** : Exercice 10

### Chaines de caracteres
1. **Niveau Debutant** : Exercices 12, 14
2. **Niveau Intermediaire** : Exercices 13, 15, 16

---

## Resume des Complexites

| Algorithme | Exercice | Complexite |
|------------|----------|------------|
| Parcours simple | Ex 1, 2, 9, 11, 12, 15, 16 | O(n) |
| Parcours matrice | Ex 3 | O(n x m) |
| Recherche lineaire | Ex 4.1, 4.2, 9, 10, 11, 12 | O(n) |
| Recherche dichotomique | Ex 4.3 | O(log n) |
| Tri 3 partitions | Ex 5 | O(n) |
| Comparaison chaines | Ex 6 | O(n^2) |
| Fusion tableaux tries | Ex 7 | O(n + m) |
| Gestion frequences | Ex 8 | O(n^2) |
| Operations CRUD | Ex 9, 10, 11 | O(n) |
| Suppression chaine | Ex 13 | O(n^2) pire cas |
| Concatenation | Ex 14 | O(n + m) |

---

*Exercices Algorithmique Chapitre 02-01 - M2i Formation*
