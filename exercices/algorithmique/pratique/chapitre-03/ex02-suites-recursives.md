# Exercice 02 : Operations recursives sur les suites

## Enonce

Soit A une suite de M entiers differents positifs ou nuls. On represente A par un tableau d'entiers de taille MAX (M <= MAX). On se propose d'etudier quelques operations sur de telles suites.

```text
Type Suite = Tableau [1..MAX] de Entier
```

---

## Partie 1 : Validation d'une suite

Ecrire une fonction recursive `Valide` pour tester si une suite A est valide, c'est-a-dire si chaque element est positif ou nul et n'apparait qu'une seule fois dans la suite.

### Principe

Pour chaque element :
1. Verifier qu'il est >= 0
2. Verifier qu'il n'existe pas dans le reste de la suite

### Fonction auxiliaire : Existe

<details>
<summary>Solution Existe</summary>

```text
FONCTION Existe (A : Suite; deb,fin : Entier; x : Entier) : Booleenne
DEBUT
    Si deb <= fin alors
        Si x = A[deb] alors
            Existe := vrai
        Sinon
            Existe := Existe(A, deb+1, fin, x)
        Fsi
    Sinon
        Existe := faux
    Fsi
FIN
```

</details>

### Fonction Valide

<details>
<summary>Solution Valide</summary>

```text
FONCTION Valide (A : Suite; d,f : Entier) : Booleenne
DEBUT
    Si d <= f alors
        Si (A[d] >= 0) ET NON Existe(A, d+1, f, A[d]) alors
            Valide := Valide(A, d+1, f)
        Sinon
            Valide := faux
        Fsi
    Sinon
        Valide := vrai
    Fsi
FIN
```

</details>

### Trace d'execution

```text
A = {1, 3, 5, 2}

Valide(A, 1, 4)
  A[1]=1 >= 0 ET NON Existe(A, 2, 4, 1) ?
    Existe(A, 2, 4, 1) : 1<>3, 1<>5, 1<>2 -> faux
  -> NON faux = vrai -> Valide(A, 2, 4)
    A[2]=3 >= 0 ET NON Existe(A, 3, 4, 3) ?
      Existe(A, 3, 4, 3) : 3<>5, 3<>2 -> faux
    -> vrai -> Valide(A, 3, 4)
      A[3]=5 >= 0 ET NON Existe(A, 4, 4, 5) ?
        Existe(A, 4, 4, 5) : 5<>2 -> faux
      -> vrai -> Valide(A, 4, 4)
        A[4]=2 >= 0 ET NON Existe(A, 5, 4, 2) ?
          Existe(A, 5, 4, 2) : d > f -> faux
        -> vrai -> Valide(A, 5, 4)
          d > f -> vrai

Resultat : VRAI (suite valide)
```

---

## Partie 2 : Suite croissante

Ecrire une fonction recursive `Croissante` permettant de verifier si une suite donnee est croissante.

Une suite A = (a1, a2, ..., aM) est dite croissante si ai <= ai+1 pour 1 <= i < M.

<details>
<summary>Solution</summary>

```text
FONCTION Croissante (A : Suite; d,f : Entier) : Booleenne
DEBUT
    Si d < f alors
        Si A[d] <= A[d+1] alors
            Croissante := Croissante(A, d+1, f)
        Sinon
            Croissante := faux
        Fsi
    Sinon
        Croissante := vrai
    Fsi
FIN
```

</details>

### Exemples

```text
A = {1, 2, 5, 8, 9}  -> VRAI (croissante)
A = {1, 3, 2, 5}     -> FAUX (3 > 2)
A = {5}              -> VRAI (un seul element)
A = {}               -> VRAI (vide)
```

### Trace d'execution

```text
A = {1, 3, 5, 4}

Croissante(A, 1, 4)
  A[1]=1 <= A[2]=3 ? OUI -> Croissante(A, 2, 4)
    A[2]=3 <= A[3]=5 ? OUI -> Croissante(A, 3, 4)
      A[3]=5 <= A[4]=4 ? NON -> faux

Resultat : FAUX
```

---

## Partie 3 : Suite extraite

Ecrire une fonction recursive `Extraite` qui, etant donnees deux suites A et B, permet de verifier si B est extraite de A.

Une suite B = (b1, b2, ..., bM) est dite extraite de A si :
- Les elements de B appartiennent a A
- Ils apparaissent dans le meme ordre

### Exemple

```text
A = (1, 2, 5, 3, 9, 7)

B = (1, 5, 2) -> NON extraite (le 2 apparait apres le 5 dans B mais avant dans A)
B = (1, 2, 3, 7) -> OUI extraite (meme ordre)
```

### Schema visuel

```text
     1    2    3    4    5    6    7    8    9
   +----+----+----+----+----+----+----+----+----+
 A |  1 |  2 |  5 |  3 |  9 |  7 |  4 |  8 |  9 |
   +----+----+----+----+----+----+----+----+----+
     ^                                       ^

     1    2    3    4
   +----+----+----+----+
 B |  1 |  2 |  3 |  7 |   -> Extraite de A
   +----+----+----+----+
     ^              ^
```

<details>
<summary>Solution</summary>

```text
FONCTION Extraite (A,B : Suite; dA,fA,dB,fB : Entier) : Booleenne
DEBUT
    Si dB <= fB alors
        // Il reste des elements dans B a trouver
        Si dA <= fA alors
            // Il reste des elements dans A a parcourir
            Si B[dB] = A[dA] alors
                // Element trouve, chercher le suivant
                Extraite := Extraite(A, B, dA+1, fA, dB+1, fB)
            Sinon
                // Continuer a chercher dans A
                Extraite := Extraite(A, B, dA+1, fA, dB, fB)
            Fsi
        Sinon
            // A epuise mais B non vide -> echec
            Extraite := faux
        Fsi
    Sinon
        // B entierement trouve -> succes
        Extraite := vrai
    Fsi
FIN
```

</details>

### Trace d'execution

```text
A = {1, 2, 5, 3, 9, 7}
B = {1, 2, 3, 7}

Extraite(A, B, 1, 6, 1, 4)
  B[1]=1, A[1]=1 -> egaux -> Extraite(A, B, 2, 6, 2, 4)
    B[2]=2, A[2]=2 -> egaux -> Extraite(A, B, 3, 6, 3, 4)
      B[3]=3, A[3]=5 -> diff -> Extraite(A, B, 4, 6, 3, 4)
        B[3]=3, A[4]=3 -> egaux -> Extraite(A, B, 5, 6, 4, 4)
          B[4]=7, A[5]=9 -> diff -> Extraite(A, B, 6, 6, 4, 4)
            B[4]=7, A[6]=7 -> egaux -> Extraite(A, B, 7, 6, 5, 4)
              dB=5 > fB=4 -> vrai

Resultat : VRAI
```

---

## Exercices supplementaires

### Variante A : Suite strictement croissante

```text
FONCTION StrictCroissante (A : Suite; d,f : Entier) : Booleenne
DEBUT
    Si d < f alors
        Si A[d] < A[d+1] alors    // < au lieu de <=
            StrictCroissante := StrictCroissante(A, d+1, f)
        Sinon
            StrictCroissante := faux
        Fsi
    Sinon
        StrictCroissante := vrai
    Fsi
FIN
```

### Variante B : Suite decroissante

```text
FONCTION Decroissante (A : Suite; d,f : Entier) : Booleenne
DEBUT
    Si d < f alors
        Si A[d] >= A[d+1] alors
            Decroissante := Decroissante(A, d+1, f)
        Sinon
            Decroissante := faux
        Fsi
    Sinon
        Decroissante := vrai
    Fsi
FIN
```

### Variante C : Sous-suite contigue

```text
// Verifier si B est une sous-suite contigue de A
FONCTION SousSuiteContigue (A,B : Suite; dA,fA,dB,fB : Entier) : Booleenne
VAR i, lenB : Entier
    match : Booleenne
DEBUT
    lenB := fB - dB + 1

    Si lenB > (fA - dA + 1) alors
        SousSuiteContigue := faux
    SinonSi lenB = 0 alors
        SousSuiteContigue := vrai
    Sinon
        Si A[dA] = B[dB] alors
            // Verifier si toute la suite B correspond
            match := VerifierMatch(A, B, dA, dB, lenB)
            Si match alors
                SousSuiteContigue := vrai
            Sinon
                SousSuiteContigue := SousSuiteContigue(A, B, dA+1, fA, dB, fB)
            Fsi
        Sinon
            SousSuiteContigue := SousSuiteContigue(A, B, dA+1, fA, dB, fB)
        Fsi
    Fsi
FIN
```

---

## Complexite

| Fonction | Temps | Espace (pile) |
|----------|-------|---------------|
| Existe | O(n) | O(n) |
| Valide | O(n^2) | O(n) |
| Croissante | O(n) | O(n) |
| Extraite | O(n + m) | O(n + m) |

---

## Points cles

1. **Suite vide** : Une suite vide est toujours valide et croissante
2. **Suite a un element** : Toujours valide et croissante
3. **Ordre des elements** : Crucial pour la fonction Extraite
4. **Unicite** : La fonction Valide verifie l'unicite des elements

---

*Exercice 02 - Recursivite - Algorithmique Chapitre 03 - M2i*
