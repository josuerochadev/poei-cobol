# Exercice 04 : Palindrome recursif

## Enonce

Un palindrome est un mot qui se lit de la meme facon de gauche a droite et de droite a gauche.

**Exemples** : "kayak", "radar", "elle", "ete"

Ecrire une fonction recursive qui determine si une chaine de caracteres est un palindrome.

---

## Partie 1 : Palindrome simple

### Principe

Un mot est un palindrome si :
- Il est vide ou contient un seul caractere (cas de base)
- OU le premier et le dernier caractere sont identiques ET la sous-chaine centrale est un palindrome

### Schema visuel

```text
Mot : "K A Y A K"
       ^       ^
      d=1     f=5

Verification :
1. M[1]='K' = M[5]='K' ? OUI -> verifier "AYA"
2. M[2]='A' = M[4]='A' ? OUI -> verifier "Y"
3. Un seul caractere -> palindrome !

     d           f
     |           |
   +---+---+---+---+---+
   | K | A | Y | A | K |
   +---+---+---+---+---+
     1   2   3   4   5

Etape 1: Comparer K et K -> OK
         +---+---+---+---+---+
         |[K]| A | Y | A |[K]|
         +---+---+---+---+---+

Etape 2: Comparer A et A -> OK
         +---+---+---+---+---+
         | K |[A]| Y |[A]| K |
         +---+---+---+---+---+

Etape 3: Un seul element -> OK
         +---+---+---+---+---+
         | K | A |[Y]| A | K |
         +---+---+---+---+---+
```

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Palindrome (M : Chaine; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        // Cas de base : 0 ou 1 caractere
        Palindrome := vrai
    Sinon
        Si M[d] = M[f] alors
            // Extremites egales, verifier le centre
            Palindrome := Palindrome(M, d + 1, f - 1)
        Sinon
            // Extremites differentes
            Palindrome := faux
        Fsi
    Fsi
FIN
```

</details>

### Trace d'execution

```text
M = "RADAR"

Palindrome("RADAR", 1, 5)
  d=1 < f=5
  M[1]='R' = M[5]='R' ? OUI -> Palindrome("RADAR", 2, 4)
    d=2 < f=4
    M[2]='A' = M[4]='A' ? OUI -> Palindrome("RADAR", 3, 3)
      d=3 >= f=3 -> vrai
    = vrai
  = vrai

Resultat : VRAI
```

```text
M = "RATER"

Palindrome("RATER", 1, 5)
  d=1 < f=5
  M[1]='R' = M[5]='R' ? OUI -> Palindrome("RATER", 2, 4)
    d=2 < f=4
    M[2]='A' = M[4]='E' ? NON -> faux
  = faux

Resultat : FAUX
```

---

## Partie 2 : Palindrome en ignorant les espaces

### Enonce

Adapter la fonction pour ignorer les espaces dans la verification.

**Exemples** :
- "esope reste ici et se repose" -> PALINDROME (en ignorant les espaces)
- "a b c b a" -> PALINDROME

### Principe

Quand on rencontre un espace :
- Si M[d] est un espace, avancer d
- Si M[f] est un espace, reculer f
- Sinon, comparer normalement

### Solution

<details>
<summary>Solution</summary>

```text
FONCTION Palindrome_Espace (M : Chaine; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Palindrome_Espace := vrai
    Sinon
        Si M[d] = ' ' alors
            // Ignorer l'espace a gauche
            Palindrome_Espace := Palindrome_Espace(M, d + 1, f)
        Sinon Si M[f] = ' ' alors
            // Ignorer l'espace a droite
            Palindrome_Espace := Palindrome_Espace(M, d, f - 1)
        Sinon Si M[d] = M[f] alors
            // Caracteres egaux, continuer
            Palindrome_Espace := Palindrome_Espace(M, d + 1, f - 1)
        Sinon
            Palindrome_Espace := faux
        Fsi
    Fsi
FIN
```

</details>

### Trace d'execution

```text
M = "A B A"

Palindrome_Espace("A B A", 1, 5)
  M[1]='A' = M[5]='A' ? OUI -> Palindrome_Espace("A B A", 2, 4)
    M[2]=' ' -> espace -> Palindrome_Espace("A B A", 3, 4)
      M[3]='B', M[4]=' ' -> espace -> Palindrome_Espace("A B A", 3, 3)
        d >= f -> vrai

Resultat : VRAI
```

---

## Partie 3 : Palindrome case-insensitive

### Enonce

Adapter la fonction pour ignorer la casse (majuscules/minuscules).

### Solution

<details>
<summary>Solution avec fonction auxiliaire</summary>

```text
FONCTION Majuscule (c : Caractere) : Caractere
DEBUT
    Si c >= 'a' ET c <= 'z' alors
        Majuscule := chr(ord(c) - 32)
    Sinon
        Majuscule := c
    Fsi
FIN

FONCTION Palindrome_NoCase (M : Chaine; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Palindrome_NoCase := vrai
    Sinon
        Si Majuscule(M[d]) = Majuscule(M[f]) alors
            Palindrome_NoCase := Palindrome_NoCase(M, d + 1, f - 1)
        Sinon
            Palindrome_NoCase := faux
        Fsi
    Fsi
FIN
```

</details>

---

## Partie 4 : Version complete

### Enonce

Combiner les deux : ignorer espaces ET casse.

### Solution

<details>
<summary>Solution complete</summary>

```text
FONCTION Palindrome_Complet (M : Chaine; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Palindrome_Complet := vrai
    Sinon
        Si M[d] = ' ' alors
            Palindrome_Complet := Palindrome_Complet(M, d + 1, f)
        Sinon Si M[f] = ' ' alors
            Palindrome_Complet := Palindrome_Complet(M, d, f - 1)
        Sinon Si Majuscule(M[d]) = Majuscule(M[f]) alors
            Palindrome_Complet := Palindrome_Complet(M, d + 1, f - 1)
        Sinon
            Palindrome_Complet := faux
        Fsi
    Fsi
FIN
```

</details>

### Exemple

```text
M = "Esope reste ici et se repose"

Sans espaces : "Esoperesteicietserepoe"
En majuscules : "ESOPERESTEICIETSEREPOE"

Verification recursive :
E = E, S = S, O = O, P = P, E = E, R = R, E = E, S = S, T = T, ...
-> PALINDROME
```

---

## Schema comparatif des versions

```text
Version         | "Kayak" | "Kay ak" | "KAYAK" | "K a Y a K"
----------------|---------|----------|---------|------------
Simple          | VRAI    | FAUX     | FAUX    | FAUX
Ignore espaces  | VRAI    | VRAI     | FAUX    | VRAI
Ignore casse    | VRAI    | FAUX     | VRAI    | FAUX
Complete        | VRAI    | VRAI     | VRAI    | VRAI
```

---

## Exercices supplementaires

### Variante A : Compter les comparaisons

```text
FONCTION Palindrome_Compte (M : Chaine; d, f : Entier; VAR nb : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Palindrome_Compte := vrai
    Sinon
        nb := nb + 1  // Compteur de comparaisons
        Si M[d] = M[f] alors
            Palindrome_Compte := Palindrome_Compte(M, d + 1, f - 1, nb)
        Sinon
            Palindrome_Compte := faux
        Fsi
    Fsi
FIN
```

### Variante B : Ignorer la ponctuation

```text
FONCTION Est_Lettre (c : Caractere) : Booleenne
DEBUT
    Est_Lettre := (c >= 'A' ET c <= 'Z') OU (c >= 'a' ET c <= 'z')
FIN

FONCTION Palindrome_Lettres (M : Chaine; d, f : Entier) : Booleenne
DEBUT
    Si d >= f alors
        Palindrome_Lettres := vrai
    Sinon
        Si NON Est_Lettre(M[d]) alors
            Palindrome_Lettres := Palindrome_Lettres(M, d + 1, f)
        Sinon Si NON Est_Lettre(M[f]) alors
            Palindrome_Lettres := Palindrome_Lettres(M, d, f - 1)
        Sinon Si Majuscule(M[d]) = Majuscule(M[f]) alors
            Palindrome_Lettres := Palindrome_Lettres(M, d + 1, f - 1)
        Sinon
            Palindrome_Lettres := faux
        Fsi
    Fsi
FIN
```

### Variante C : Trouver le plus long prefixe palindrome

```text
FONCTION Plus_Long_Prefixe_Palindrome (M : Chaine; n : Entier) : Entier
VAR i : Entier
DEBUT
    Pour i de n a 1 pas -1 faire
        Si Palindrome(M, 1, i) alors
            Plus_Long_Prefixe_Palindrome := i
            Retourner
        Fsi
    Fpour
    Plus_Long_Prefixe_Palindrome := 1  // Au moins le premier caractere
FIN
```

---

## Complexite

| Version | Temps | Espace (pile) |
|---------|-------|---------------|
| Simple | O(n/2) = O(n) | O(n/2) |
| Ignore espaces | O(n) | O(n) |
| Ignore casse | O(n) | O(n) |
| Complete | O(n) | O(n) |

---

## Exemples celebres de palindromes

```text
Mots simples :
- kayak, radar, rotor, civic, level, refer

Phrases (en ignorant espaces et ponctuation) :
- "A man a plan a canal Panama"
- "Was it a car or a cat I saw"
- "Never odd or even"

En francais :
- "Esope reste ici et se repose"
- "La mariee ira mal"
- "Engage le jeu que je le gagne"
```

---

## Points cles

1. **Cas de base** : Chaine vide ou un seul caractere = toujours palindrome
2. **Comparaison symetrique** : Comparer premier/dernier, puis recurser sur le centre
3. **Gestion des espaces** : Avancer/reculer les indices quand on rencontre un espace
4. **Normalisation** : Convertir en majuscules pour comparaison case-insensitive
5. **Efficacite** : Maximum n/2 comparaisons necessaires

---

*Exercice 04 - Recursivite - Algorithmique Chapitre 03 - M2i*
