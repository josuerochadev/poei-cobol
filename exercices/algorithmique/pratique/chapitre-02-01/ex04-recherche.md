# Exercice 4 : Recherche dans un tableau

## Partie 1 : Premiere et derniere occurrence (tableau non trie)

### Enonce

Ecrire une procedure qui trouve la premiere et la derniere position d'une valeur X dans un tableau T.

### Exemple

```text
T = [3, 5, 2, 5, 8, 5, 1]
X = 5
Resultat : pos1 = 2, pos2 = 6
```

---

<details>
<summary>Solution Partie 1</summary>

```text
Procedure prem_et_dern_positions(T : TAB; N : Entier; X : Reel; Var pos1, pos2 : Entier)
Var   i : Entier

DEBUT
    pos1 := 0
    pos2 := 0

    Pour i de 1 a N Faire
        Si T[i] = X Alors
            Si pos1 = 0 Alors
                pos1 := i      // Premiere occurrence
            finSi
            pos2 := i          // Derniere occurrence (mise a jour continue)
        finSi
    finPour
FIN
```

**Points cles :**
- `pos1` est mis a jour uniquement la premiere fois (quand il vaut encore 0)
- `pos2` est mis a jour a chaque occurrence trouvee
- Si X n'est pas trouve, pos1 = pos2 = 0

</details>

---

## Partie 2 : Optimisation pour tableau trie

### Enonce

Si le tableau est trie par ordre croissant, optimiser la recherche en s'arretant des que T[i] > X.

---

<details>
<summary>Solution Partie 2</summary>

```text
Procedure prem_et_dern_positions_trie(T : TAB; N : Entier; X : Reel; Var pos1, pos2 : Entier)
Var   i : Entier

DEBUT
    pos1 := 0
    pos2 := 0
    i := 1

    // On s'arrete des que T[i] > X (tableau trie)
    Tantque (i <= N) et (T[i] <= X) Faire
        Si T[i] = X Alors
            Si pos1 = 0 Alors
                pos1 := i
            finSi
            pos2 := i
        finSi
        i := i + 1
    finTantQue
FIN
```

**Avantage :** On ne parcourt pas tout le tableau si X est petit.

</details>

---

## Partie 3 : Recherche dichotomique (tableau trie)

### Enonce

Implementer une recherche dichotomique (binary search) pour trouver une valeur dans un tableau trie.

### Principe

Diviser l'espace de recherche par 2 a chaque iteration :
1. Calculer le milieu m = (d + f) / 2
2. Si X < T[m] : chercher dans la moitie gauche (f := m - 1)
3. Si X > T[m] : chercher dans la moitie droite (d := m + 1)
4. Si X = T[m] : trouve !

### Exemple

```text
T = [1, 3, 5, 7, 9, 11, 13, 15]
X = 7

Etape 1 : d=1, f=8, m=4, T[4]=7 -> Trouve !
```

```text
T = [1, 3, 5, 7, 9, 11, 13, 15]
X = 11

Etape 1 : d=1, f=8, m=4, T[4]=7 < 11 -> d=5
Etape 2 : d=5, f=8, m=6, T[6]=11 -> Trouve !
```

---

<details>
<summary>Solution Partie 3</summary>

```text
Procedure chercher_position(T : TAB; N : Entier; X : Reel; Var pos : Entier)
Var   d, f, m : Entier
      trouve : Booleen

DEBUT
    d := 1
    f := N
    trouve := Faux

    Repeter
        m := (d + f) Div 2           // Milieu du tableau

        Si X < T[m] Alors
            f := m - 1               // Chercher dans la moitie gauche
        SiNonSi X > T[m] Alors
            d := m + 1               // Chercher dans la moitie droite
        SiNon
            trouve := Vrai           // Trouve !
        finSi
    Jusqu'a (trouve) ou (d > f)

    Si trouve Alors
        pos := m
    SiNon
        pos := 0
    finSi
FIN
```

**Complexite :** O(log n) - beaucoup plus rapide que O(n) pour les grands tableaux.

| Taille N | Recherche lineaire | Recherche dichotomique |
|----------|-------------------|------------------------|
| 1 000    | 1 000 operations  | 10 operations          |
| 1 000 000| 1 000 000 operations | 20 operations       |

</details>

---

## Comparaison des algorithmes

| Algorithme | Complexite | Prerequis | Usage |
|------------|-----------|-----------|-------|
| Lineaire (Partie 1) | O(n) | Aucun | Petits tableaux, non tries |
| Lineaire optimise (Partie 2) | O(n) | Tableau trie | Valeurs en debut de tableau |
| Dichotomique (Partie 3) | O(log n) | Tableau trie | Grands tableaux |

---

*Exercice 4 - Algorithmique Chapitre 02 - M2i*
