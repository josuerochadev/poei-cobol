# Chapitre II - Structures de Donnees

## 1. Introduction aux structures de donnees

Une **structure de donnees** est une maniere particuliere de stocker et d'organiser des donnees afin de pouvoir les utiliser efficacement.

Une structure de donnees regroupe :
- Un certain nombre de **donnees a gerer**
- Un ensemble d'**operations** pouvant etre appliquees a ces donnees

Dans la plupart des cas :
- Il existe plusieurs manieres de representer les donnees
- Il existe differents algorithmes de manipulation

---

## 2. Tableaux unidimensionnels (vecteurs)

### Definition

Un **tableau unidimensionnel** (ou vecteur) est une variable composee de donnees de meme type, stockees de maniere **contigue en memoire**.

### Syntaxe de declaration

```
<id_tableau> : tableau [bi..bs] de <type_base>
```

Ou :
- `bi` = borne inferieure (indice de debut)
- `bs` = borne superieure (indice de fin)
- `type_base` = type des elements (entier, reel, caractere...)

### Acces a un element

```
nom_tableau[indice]
```

### Exemple

```
T : tableau [1..10] de entier
T[7] := 13    // Affecte 13 a la 7eme case
```

### Type de tableau personnalise

On peut definir un type tableau pour le reutiliser :

```
Tab_TypeEntier = tableau [1..10] de entier
T : Tab_TypeEntier
```

---

## 3. Tableaux extensibles

Un tableau extensible a :
- Une **taille maximale** (MAX) fixee a la declaration
- Une **taille logique** (N) qui represente le nombre d'elements reellement utilises

```
T : tableau [1..MAX] de entier
N : entier    // N <= MAX toujours
```

### Avantages

- Flexibilite : on peut ajouter/supprimer des elements
- Efficacite : on ne traite que les N elements utiles

---

## 4. Operations de base sur les tableaux

### 4.1 Lecture d'un tableau (taille fixe)

```
Algo Gestion_Des_Ventes
Var i : entier
    T : tableau [1..12] de reel
Debut
    Pour i de 1 a 12 faire
        Lire(T[i])
    Fpour
Fin
```

### 4.2 Lecture avec taille variable

```
Algo Lire_tab
Var T : tableau[1..100] de entier
    i, N : entier
Debut
    Repeter
        Lire(N)
    Jusqu'a N > 0 ET N <= 100

    Pour i de 1 a N faire
        Lire(T[i])
    Fpour
Fin
```

### 4.3 Lecture avec sentinelle

```
Algo Lire_tab_sentinelle
Var T : tableau[1..100] de entier
    i, x : entier
Debut
    i := 0
    Lire(x)
    Tant que (x > 0) ET (i < 100) faire
        i := i + 1
        T[i] := x
        Lire(x)
    Fait
Fin
```

### 4.4 Inversion d'un tableau

```
Algo Inverse_Tab
Var T : tableau[1..100] de entier
    i, temp, N, m : entier
Debut
    // Lecture du tableau
    Repeter
        Lire(N)
    Jusqu'a N > 0 ET N <= 100

    Pour i de 1 a N faire
        Lire(T[i])
    Fpour

    // Inversion
    m := N DIV 2
    Pour i de 1 a m faire
        temp := T[i]
        T[i] := T[N-i+1]
        T[N-i+1] := temp
    Fpour
Fin
```

---

## 5. Recherche dans un tableau

### 5.1 Recherche sequentielle (tableau non ordonne)

**Principe** : Parcours de gauche a droite, comparaison avec la valeur recherchee.

#### Version 1 - Derniere occurrence

```
Fonction Rech_Sequentielle(E : tableau, N : entier, x : entier) : entier
Var i, p : entier
Debut
    p := 0
    Pour i de 1 a N faire
        Si E[i] = x Alors
            p := i
        Fsi
    Fpour
    Retourner p
Fin
```

#### Version 2 - Premiere occurrence (avec drapeau)

```
Fonction Rech_Sequentielle(E : tableau, N : entier, x : entier) : entier
Var i, p : entier
    trouve : booleen
Debut
    p := 0
    trouve := faux
    Pour i de 1 a N faire
        Si E[i] = x ET NON trouve Alors
            p := i
            trouve := vrai
        Fsi
    Fpour
    Retourner p
Fin
```

#### Version 3 - Avec sortie anticipee (TantQue)

```
Fonction Rech_Sequentielle(E : tableau, N : entier, x : entier) : entier
Var i, p : entier
Debut
    p := 0
    i := 1
    Tant que (i <= N) ET (E[i] <> x) faire
        i := i + 1
    Fait
    Si (i <= N) Alors
        p := i
    Fsi
    Retourner p
Fin
```

### 5.2 Recherche sequentielle optimisee (tableau ordonne)

Si le tableau est **trie**, on peut arreter des que l'element courant depasse la valeur recherchee :

```
Tant que (i <= N) ET (E[i] < x) faire
    i := i + 1
Fait
Si (i <= N) ET (E[i] = x) Alors
    p := i
Fsi
```

### 5.3 Recherche dichotomique

**Prerequis** : Le tableau doit etre **trie**.

**Principe** : Division par deux de l'espace de recherche a chaque iteration.

**Complexite** : O(log N) contre O(N) pour la recherche sequentielle.

```
Fonction Rech_Dichotomique(E : tableau, N : entier, x : entier) : entier
Var d, f, m, p : entier
Debut
    d := 1          // debut
    f := N          // fin
    p := 0          // position (0 = non trouve)

    Tant que (d <= f) faire
        m := (d + f) DIV 2
        Si E[m] = x Alors
            p := m
            Sortir      // Element trouve
        Sinon Si E[m] < x Alors
            d := m + 1  // Chercher dans la moitie droite
        Sinon
            f := m - 1  // Chercher dans la moitie gauche
        Fsi
    Fait
    Retourner p
Fin
```

### Comparaison des methodes de recherche

| Methode | Tableau trie requis | Complexite | Cas d'usage |
|---------|-------------------|------------|-------------|
| Sequentielle | Non | O(N) | Petits tableaux, recherche unique |
| Seq. optimisee | Oui | O(N) | Tableaux tries, element absent |
| Dichotomique | Oui | O(log N) | Grands tableaux tries |

---

## 6. Ajout dans un tableau

### 6.1 Tableau non ordonne

Simple ajout en fin de tableau :

```
N := N + 1
T[N] := x
```

### 6.2 Tableau ordonne (maintien de l'ordre)

Il faut decaler les elements pour inserer a la bonne position :

```
// Recherche de la position et decalage
i := N
Tant que (i > 0) ET (T[i] > x) faire
    T[i+1] := T[i]
    i := i - 1
Fait
T[i+1] := x
N := N + 1
```

---

## 7. Modification d'un element

### 7.1 Tableau non ordonne

```
i := 1
Tant que (i <= N) ET (T[i] <> x) faire
    i := i + 1
Fait
Si (i <= N) Alors
    T[i] := y       // Remplacer x par y
Fsi
```

### 7.2 Tableau ordonne

Meme principe avec optimisation de la recherche :

```
i := 1
Tant que (i <= N) ET (T[i] < x) faire
    i := i + 1
Fait
Si (i <= N) ET (T[i] = x) Alors
    T[i] := y
Fsi
```

**Attention** : Si la nouvelle valeur `y` change l'ordre, il faut repositionner l'element.

---

## 8. Suppression d'un element

### 8.1 Tableau non ordonne

Remplacement par le dernier element (O(1)) :

```
T[i] := T[N]
N := N - 1
```

### 8.2 Tableau ordonne

Decalage des elements suivants (O(N)) :

```
Pour j de i a N-1 faire
    T[j] := T[j+1]
Fpour
N := N - 1
```

---

## 9. Tableaux multidimensionnels

### Declaration

```
T : tableau [1..12, 1..8] de reel
```

Represente une matrice de 12 lignes et 8 colonnes.

### Acces

```
T[i, j]     // Element a la ligne i, colonne j
```

### Parcours d'une matrice

```
// Parcours ligne par ligne
Pour i de 1 a 12 faire
    Pour j de 1 a 8 faire
        Traiter(T[i, j])
    Fpour
Fpour
```

### Exemple : Somme d'une colonne

```
somme := 0
Pour i de 1 a 12 faire
    somme := somme + T[i, 5]    // Somme de la colonne 5
Fpour
```

---

## 10. Enregistrements (structures)

Un **enregistrement** permet de regrouper des donnees de types differents.

### Declaration

```
type_Date = enreg
    jour : entier
    mois : entier
    an   : entier
Fenreg

type_Etudiant = enreg
    numero  : entier
    nom     : tableau [0..99] de caractere
    dn      : type_Date
    moyenne : reel
Fenreg
```

### Utilisation

```
E : type_Etudiant

E.numero := 12345
E.moyenne := 14.5
E.dn.jour := 15
E.dn.mois := 3
E.dn.an := 2000
```

### Tableau d'enregistrements

```
Tab_Etudiants : tableau [1..100] de type_Etudiant

Tab_Etudiants[1].nom := "DUPONT"
Tab_Etudiants[1].moyenne := 15.2
```

---

## 11. Chaines de caracteres

### Structure d'une chaine

```
Chaine = enreg
    str : tableau [1..MAX] de caractere
    lth : entier    // longueur effective
Fenreg
```

### Primitives courantes

| Primitive | Description | Exemple |
|-----------|-------------|---------|
| `Long(ch)` | Longueur de la chaine | `Long("Hello")` = 5 |
| `Pos(ch, sous)` | Position d'une sous-chaine | `Pos("Hello", "ll")` = 3 |
| `Concat(ch1, ch2)` | Concatenation | `Concat("Bon", "jour")` = "Bonjour" |
| `Sous_chaine(ch, deb, fin)` | Extraction | `Sous_chaine("Bonjour", 1, 3)` = "Bon" |
| `Effacer(ch, pos, nb)` | Suppression | `Effacer("Bonjour", 4, 4)` = "Bon" |
| `Majus(ch)` | Majuscules | `Majus("hello")` = "HELLO" |
| `Minus(ch)` | Minuscules | `Minus("HELLO")` = "hello" |
| `ConvNum(ch)` | Chaine vers nombre | `ConvNum("123")` = 123 |
| `ConvCh(n)` | Nombre vers chaine | `ConvCh(123)` = "123" |

### Recherche naive de chaine (pattern matching)

```
Fonction Recherche_chaine(A : Chaine, M : Chaine) : entier
Var i, j, p : entier
Debut
    i := 1
    j := 1
    p := 0

    Tant que (j <= M.lth) ET (i <= A.lth) faire
        Si A.str[i] = M.str[j] Alors
            i := i + 1
            j := j + 1
        Sinon
            i := i - j + 2      // Retour en arriere
            j := 1              // Recommencer le motif
        Fsi
    Fait

    Si j > M.lth Alors
        p := i - M.lth          // Position trouvee
    Fsi
    Retourner p
Fin
```

---

## 12. Correspondance avec COBOL

| Algorithmique | COBOL |
|---------------|-------|
| `T : tableau [1..10] de entier` | `01 T OCCURS 10 TIMES PIC 9(4).` |
| `T[i]` | `T(I)` |
| `T : tableau [1..12, 1..8]` | `01 T OCCURS 12 TIMES. 05 COL OCCURS 8 TIMES.` |
| `enreg...Fenreg` | Structure avec niveaux (01, 05, 10...) |
| `E.nom` | `NOM OF E` ou simplement `NOM` si unique |

---

## 13. Resume des complexites

| Operation | Non ordonne | Ordonne |
|-----------|-------------|---------|
| Recherche | O(N) | O(log N) dichotomique |
| Ajout | O(1) en fin | O(N) avec decalage |
| Suppression | O(1) swap | O(N) avec decalage |
| Modification | O(N) recherche | O(log N) + repositionnement |

---

## 14. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-02-01-structures-donnees.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-02/)

---

*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
