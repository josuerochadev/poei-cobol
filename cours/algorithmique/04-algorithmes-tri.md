# Chapitre IV - Algorithmes de Tri

## 1. Introduction au tri

### Definition generale

Le **tri** est une operation consistant a organiser ou classer un ensemble d'elements selon un ordre determine, base sur une ou plusieurs caracteristiques appelees **cles de tri**.

### Objectifs du tri

| Objectif | Description |
|----------|-------------|
| Lisibilite | Faciliter la lecture des donnees |
| Recherche | Accelerer les recherches (dichotomie) |
| Analyse | Optimiser l'analyse et les traitements |
| Fusion | Permettre la fusion efficace de fichiers |

### Importance en informatique

Les algorithmes de tri sont fondamentaux pour :
- Les bases de donnees (indexation, requetes ORDER BY)
- Les systemes d'information (rapports, etats)
- L'optimisation des traitements batch (COBOL, JCL SORT)

---

## 2. Principe des tris par comparaison

Les algorithmes de tri par comparaison fonctionnent en comparant les elements deux a deux pour determiner leur ordre relatif.

### Principales familles

| Famille | Algorithmes | Principe |
|---------|-------------|----------|
| Selection | Tri selection, Min-Max | Chercher le min/max, le placer |
| Insertion | Tri insertion | Inserer dans une partie triee |
| Echange | Bulles, Shaker, Gnome | Echanger les elements mal places |
| Division | Fusion, Rapide | Diviser pour regner |

### Complexite theorique minimale

Les tris par comparaison ont une complexite minimale de **O(N log N)** dans le cas general.

---

## 3. Tri par selection

### Principe

1. Rechercher le **minimum** dans la partie non triee
2. Le placer en premiere position de cette partie
3. Recommencer sur la partie restante

**Analogie** : Le photographe scolaire qui place les enfants par taille.

### Algorithme

```
Procedure Tri_Selection(var T : Tableau, N : entier)
Var i, j, pmin, temp : entier
Debut
    Pour i de 1 a N-1 faire
        // Recherche du minimum dans T[i..N]
        pmin := i
        Pour j de i+1 a N faire
            Si T[j] < T[pmin] Alors
                pmin := j
            Fsi
        Fpour

        // Echange si necessaire
        Si i <> pmin Alors
            temp := T[i]
            T[i] := T[pmin]
            T[pmin] := temp
        Fsi
    Fpour
Fin
```

### Exemple de deroulement

Tableau initial : [64, 25, 12, 22, 11]

```
i=1: min=11 (pos 5) → [11, 25, 12, 22, 64]
i=2: min=12 (pos 3) → [11, 12, 25, 22, 64]
i=3: min=22 (pos 4) → [11, 12, 22, 25, 64]
i=4: min=25 (pos 4) → [11, 12, 22, 25, 64] (deja en place)
```

### Complexite

| Cas | Complexite |
|-----|------------|
| Meilleur | O(N²) |
| Moyen | O(N²) |
| Pire | O(N²) |

**Nombre de comparaisons** : N(N-1)/2 (toujours)

### Variante Min-Max

Recherche simultanee du minimum ET du maximum a chaque iteration.

```
Procedure Tri_Min_Max(var T : Tableau, N : entier)
Var d, f, pmin, pmax : entier
Debut
    d := 1
    f := N
    Tant que d < f faire
        // Trouver min et max dans T[d..f]
        Min_Max(T, d, f, pmin, pmax)

        // Placer le minimum au debut
        Permuter(T[d], T[pmin])

        // Attention : si le max etait en position d
        Si pmax = d Alors
            Permuter(T[f], T[pmin])
        Sinon
            Permuter(T[f], T[pmax])
        Fsi

        d := d + 1
        f := f - 1
    Fait
Fin
```

---

## 4. Tri par insertion

### Principe

Inserer progressivement chaque element dans la partie deja triee, a sa bonne position.

**Analogie** : Trier des cartes a jouer dans sa main.

### Algorithme

```
Procedure Tri_Insertion(var T : Tableau, N : entier)
Var i, j, temp : entier
Debut
    Pour i de 2 a N faire
        temp := T[i]           // Element a inserer
        j := i - 1

        // Decaler les elements plus grands
        Tant que j >= 1 ET T[j] > temp faire
            T[j+1] := T[j]
            j := j - 1
        Fait

        T[j+1] := temp         // Insertion
    Fpour
Fin
```

### Exemple de deroulement

Tableau initial : [5, 2, 4, 6, 1, 3]

```
i=2: inserer 2 → [2, 5, 4, 6, 1, 3]
i=3: inserer 4 → [2, 4, 5, 6, 1, 3]
i=4: inserer 6 → [2, 4, 5, 6, 1, 3] (deja en place)
i=5: inserer 1 → [1, 2, 4, 5, 6, 3]
i=6: inserer 3 → [1, 2, 3, 4, 5, 6]
```

### Complexite

| Cas | Complexite | Situation |
|-----|------------|-----------|
| Meilleur | O(N) | Tableau deja trie |
| Moyen | O(N²) | Ordre aleatoire |
| Pire | O(N²) | Tableau inverse |

**Avantage** : Tres efficace sur les tableaux presque tries.

---

## 5. Tri a bulles (Bubble Sort)

### Principe

Echanger les elements consecutifs mal ordonnes. Les plus grands elements "remontent" vers la fin comme des bulles.

### Algorithme (version simple)

```
Procedure Tri_Bulle(var T : Tableau, N : entier)
Var i, j : entier
Debut
    Pour i de N a 2 pas -1 faire
        Pour j de 1 a i-1 faire
            Si T[j] > T[j+1] Alors
                Permuter(T[j], T[j+1])
            Fsi
        Fpour
    Fpour
Fin
```

### Exemple de deroulement

Tableau initial : [5, 1, 4, 2, 8]

```
Passe 1: [1, 4, 2, 5, 8] (8 en place)
Passe 2: [1, 2, 4, 5, 8] (5 en place)
Passe 3: [1, 2, 4, 5, 8] (deja trie)
```

### Amelioration 1 : Arret anticipe

S'arreter si aucun echange n'a ete fait (tableau trie).

```
Procedure Tri_Bulle_Optimise(var T : Tableau, N : entier)
Var i, j : entier
    permutation : booleen
Debut
    i := N
    Repeter
        permutation := Faux
        Pour j de 1 a i-1 faire
            Si T[j] > T[j+1] Alors
                Permuter(T[j], T[j+1])
                permutation := Vrai
            Fsi
        Fpour
        i := i - 1
    Jusqu'a NON permutation
Fin
```

### Amelioration 2 : Tri Shaker (Cocktail Sort)

Parcours alternatif gauche-droite puis droite-gauche.

```
Procedure Tri_Shaker(var T : Tableau, N : entier)
Var d, f, i : entier
    permutation : booleen
Debut
    d := 1
    f := N
    Repeter
        permutation := Faux

        // Passe gauche → droite (bulle vers la fin)
        Pour i de d a f-1 faire
            Si T[i] > T[i+1] Alors
                Permuter(T[i], T[i+1])
                permutation := Vrai
            Fsi
        Fpour
        f := f - 1

        // Passe droite → gauche (bulle vers le debut)
        Pour i de f a d+1 pas -1 faire
            Si T[i] < T[i-1] Alors
                Permuter(T[i], T[i-1])
                permutation := Vrai
            Fsi
        Fpour
        d := d + 1

    Jusqu'a NON permutation OU f <= d
Fin
```

### Complexite

| Cas | Complexite |
|-----|------------|
| Meilleur | O(N) avec optimisation |
| Moyen | O(N²) |
| Pire | O(N²) |

---

## 6. Tri Gnome

### Principe

Similaire au tri par insertion mais avec des echanges comme le tri a bulles. L'algorithme avance ou recule selon les comparaisons.

**Analogie** : Un gnome de jardin qui trie des pots de fleurs.

### Algorithme

```
Procedure Tri_Gnome(var T : Tableau, N : entier)
Var pos : entier
Debut
    pos := 2
    Tant que pos <= N faire
        Si T[pos] >= T[pos-1] Alors
            pos := pos + 1      // Avancer
        Sinon
            Permuter(T[pos], T[pos-1])
            Si pos > 2 Alors
                pos := pos - 1  // Reculer
            Fsi
        Fsi
    Fait
Fin
```

### Complexite

| Cas | Complexite |
|-----|------------|
| Meilleur | O(N) |
| Moyen | O(N²) |
| Pire | O(N²) |

---

## 7. Tri par denombrement (Counting Sort)

### Principe

Compter les occurrences de chaque valeur puis reconstruire le tableau.

**Conditions** :
- Valeurs entieres
- Intervalle de valeurs connu et raisonnable [0..P]

### Algorithme

```
Procedure Tri_Denombrement(var T : Tableau, N, P : entier)
Var D : Tableau[0..P]    // Tableau de comptage
    i, j, k : entier
Debut
    // Initialisation
    Pour i de 0 a P faire
        D[i] := 0
    Fpour

    // Comptage
    Pour i de 1 a N faire
        D[T[i]] := D[T[i]] + 1
    Fpour

    // Reconstruction
    k := 1
    Pour i de 0 a P faire
        Pour j de 1 a D[i] faire
            T[k] := i
            k := k + 1
        Fpour
    Fpour
Fin
```

### Complexite

**O(N + P)** ou P est la plage de valeurs.

**Avantage** : Lineaire si P est petit par rapport a N.

---

## 8. Tri par fusion (Merge Sort)

### Principe

Paradigme **"diviser pour regner"** :
1. **Diviser** le tableau en deux moities
2. **Trier** recursivement chaque moitie
3. **Fusionner** les deux moities triees

### Algorithme principal

```
Procedure Tri_Fusion(T : Tableau, d, f : entier, var R : Tableau)
Var m : entier
    R1, R2 : Tableau
Debut
    Si d = f Alors
        R[d] := T[d]        // Cas de base
    Sinon
        m := (d + f) DIV 2
        Tri_Fusion(T, d, m, R1)      // Trier moitie gauche
        Tri_Fusion(T, m+1, f, R2)    // Trier moitie droite
        Fusion(R1, d, m, R2, m+1, f, R)  // Fusionner
    Fsi
Fin
```

### Procedure de fusion

```
Procedure Fusion(A : Tableau, dA, fA : entier,
                 B : Tableau, dB, fB : entier,
                 var R : Tableau)
Var i, j, k : entier
Debut
    i := dA
    j := dB
    k := dA

    Tant que i <= fA ET j <= fB faire
        Si A[i] <= B[j] Alors
            R[k] := A[i]
            i := i + 1
        Sinon
            R[k] := B[j]
            j := j + 1
        Fsi
        k := k + 1
    Fait

    // Copier les elements restants
    Tant que i <= fA faire
        R[k] := A[i]
        i := i + 1
        k := k + 1
    Fait

    Tant que j <= fB faire
        R[k] := B[j]
        j := j + 1
        k := k + 1
    Fait
Fin
```

### Exemple visuel

```
[38, 27, 43, 3, 9, 82, 10]
        ↓ Diviser
[38, 27, 43, 3] | [9, 82, 10]
        ↓ Diviser
[38, 27] | [43, 3] | [9, 82] | [10]
        ↓ Diviser
[38] [27] [43] [3] [9] [82] [10]
        ↓ Fusionner
[27, 38] | [3, 43] | [9, 82] | [10]
        ↓ Fusionner
[3, 27, 38, 43] | [9, 10, 82]
        ↓ Fusionner
[3, 9, 10, 27, 38, 43, 82]
```

### Complexite

| Cas | Complexite |
|-----|------------|
| Tous les cas | O(N log N) |

**Inconvenient** : Necessite de la memoire supplementaire O(N).

---

## 9. Tri rapide (Quick Sort)

### Principe

Paradigme **"diviser pour regner"** :
1. Choisir un **pivot**
2. **Partitionner** : elements < pivot a gauche, > pivot a droite
3. **Trier** recursivement les deux parties

### Algorithme principal

```
Procedure Tri_Rapide(var T : Tableau, d, f : entier)
Var m : entier
Debut
    Si d < f Alors
        m := Partition(T, d, f)
        Tri_Rapide(T, d, m-1)    // Partie gauche
        Tri_Rapide(T, m+1, f)    // Partie droite
    Fsi
Fin
```

### Procedure de partition

```
Fonction Partition(var T : Tableau, d, f : entier) : entier
Var pivot, i, j : entier
Debut
    pivot := T[f]    // Dernier element comme pivot
    i := d - 1

    Pour j de d a f-1 faire
        Si T[j] <= pivot Alors
            i := i + 1
            Permuter(T[i], T[j])
        Fsi
    Fpour

    Permuter(T[i+1], T[f])
    Retourner i + 1    // Position finale du pivot
Fin
```

### Exemple de deroulement

Tableau : [10, 80, 30, 90, 40, 50, 70], pivot = 70

```
Partition: [10, 30, 40, 50, 70, 90, 80]
                        ↑ pivot en place

Recursivement sur [10, 30, 40, 50] et [90, 80]
```

### Complexite

| Cas | Complexite | Situation |
|-----|------------|-----------|
| Meilleur | O(N log N) | Pivot median |
| Moyen | O(N log N) | Ordre aleatoire |
| Pire | O(N²) | Tableau deja trie (mauvais pivot) |

**Avantages** : Tri en place, cache-friendly, tres rapide en pratique.

---

## 10. Classification des algorithmes de tri

### Selon la memoire

| Type | Description | Algorithmes |
|------|-------------|-------------|
| En place | O(1) memoire supplementaire | Selection, Insertion, Bulles, Rapide |
| Non en place | O(N) memoire supplementaire | Fusion |

### Selon la stabilite

Un tri est **stable** s'il preserve l'ordre relatif des elements egaux.

| Stables | Instables |
|---------|-----------|
| Insertion | Selection |
| Bulles | Rapide |
| Fusion | |

### Selon l'environnement

| Type | Description | Usage |
|------|-------------|-------|
| Tri interne | Donnees en RAM | Tableaux en memoire |
| Tri externe | Donnees sur disque | Gros fichiers, COBOL SORT |

---

## 11. Comparaison des algorithmes

| Algorithme | Meilleur | Moyen | Pire | Memoire | Stable |
|------------|----------|-------|------|---------|--------|
| Selection | O(N²) | O(N²) | O(N²) | O(1) | Non |
| Insertion | O(N) | O(N²) | O(N²) | O(1) | Oui |
| Bulles | O(N) | O(N²) | O(N²) | O(1) | Oui |
| Fusion | O(N log N) | O(N log N) | O(N log N) | O(N) | Oui |
| Rapide | O(N log N) | O(N log N) | O(N²) | O(log N) | Non |
| Denombrement | O(N+P) | O(N+P) | O(N+P) | O(P) | Oui |

### Recommandations

| Situation | Algorithme recommande |
|-----------|----------------------|
| Petits tableaux (N < 50) | Insertion |
| Tableau presque trie | Insertion |
| Cas general | Rapide ou Fusion |
| Stabilite requise | Fusion ou Insertion |
| Memoire limitee | Rapide |
| Valeurs entieres bornees | Denombrement |

---

## 12. Correspondance avec COBOL/JCL

### SORT JCL (tri externe)

```jcl
//TRIFICH  EXEC PGM=SORT
//SORTIN   DD DSN=FICHIER.ENTREE,DISP=SHR
//SORTOUT  DD DSN=FICHIER.SORTIE,DISP=(NEW,CATLG)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,15,5,ZD,D)
/*
```

### COBOL interne

```cobol
SORT SORT-FILE
    ON ASCENDING KEY SORT-NOM
    ON DESCENDING KEY SORT-DATE
    INPUT PROCEDURE IS LECTURE-FILTRAGE
    OUTPUT PROCEDURE IS ECRITURE-RAPPORT.
```

| Algorithmique | COBOL/JCL |
|---------------|-----------|
| Tri en memoire | SORT interne COBOL |
| Tri de fichiers | SORT JCL (DFSORT, SYNCSORT) |
| Cle de tri | SORT FIELDS, KEY |
| Ordre croissant | A (Ascending) |
| Ordre decroissant | D (Descending) |

---

## 13. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-04-algorithmes-tri.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-04/)

---

*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
