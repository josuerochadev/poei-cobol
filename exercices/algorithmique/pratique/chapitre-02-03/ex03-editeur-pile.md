# Exercice 03 : Editeur de texte avec deux piles

## Enonce

Un editeur de texte doit memoriser le texte sur lequel il travaille ainsi que l'emplacement du curseur. L'ensemble texte-curseur est represente a l'aide de deux piles **G** et **D** :

- **G** contient les caracteres situes en debut de texte (avant le curseur), le dernier etant en sommet de la pile
- **D** contient les caracteres de fin de texte, le premier en sommet de pile

Le caractere `$` indique la fin de ligne.

## Schema visuel

```text
Texte : "Algorithmique de Bas|e et Structures de D"
                            ^
                         curseur

Pile G (avant curseur)     Pile D (apres curseur)
    +---+                      +---+
    | s |  <- sommet           | e |  <- sommet
    | a |                      | e |
    | B |                      | t |
    |   |                      | S |
    | e |                      | t |
    | d |                      | r |
    |   |                      | u |
    | e |                      | c |
    | u |                      | t |
    | q |                      | u |
    | i |                      | r |
    | m |                      | e |
    | h |                      | s |
    | t |                      |   |
    | i |                      | d |
    | r |                      | e |
    | o |                      |   |
    | g |                      | D |
    | l |                      | $ |
    | A |                      +---+
    +---+
```

## Operations a implementer

En utilisant les operations primitives du type abstrait pile (`Sommet`, `Empiler`, `Depiler`, `pile_vide`), implementer :

1. **Inserer** un caractere C et positionner le curseur apres
2. **Effacer** le caractere qui suit le curseur
3. **Avancer** le curseur d'un caractere (vers la droite)
4. **Reculer** le curseur d'un caractere (vers la gauche)
5. **Rechercher** un caractere et positionner le curseur apres
6. **Debut de ligne** (curseur au debut)
7. **Traitement BackSpace et Clear** (`#` et `%`)

---

## Solutions

### 1. Inserer un caractere

```text
// Inserer C a la position du curseur
Empiler(G, C)
```

Le caractere est ajoute a la pile G (avant le curseur).

---

### 2. Effacer le caractere suivant

```text
// Effacer le caractere qui suit le curseur
Depiler(D)
```

Le caractere au sommet de D (premier apres le curseur) est supprime.

---

### 3. Avancer le curseur

```text
// Deplacer le curseur d'un caractere vers la droite
Empiler(G, Depiler(D))
```

Le premier caractere de D passe dans G.

---

### 4. Reculer le curseur

```text
// Deplacer le curseur d'un caractere vers la gauche
Empiler(D, Depiler(G))
```

Le dernier caractere de G passe dans D.

---

### 5. Rechercher un caractere

<details>
<summary>Solution</summary>

```text
PROCEDURE Recherche (var G, D : Pile, C : caractere)
VAR
    Arret : Booleen
    x : caractere
Debut
    Arret := faux

    Tantque (Sommet(D) <> '$') ET (NON Arret) faire
        x := Depiler(D)
        Empiler(G, x)

        Si x = C Alors
            Arret := vrai
        Fsi
    Ftantque
Fin
```

</details>

**Comportement** : Avance jusqu'a trouver C ou atteindre la fin de ligne. Le curseur est positionne juste apres C s'il est trouve, en fin de texte sinon.

---

### 6. Aller au debut de la ligne

<details>
<summary>Solution</summary>

```text
PROCEDURE Debut_Ligne (var G, D : Pile)
Debut
    Tantque NON pile_vide(G) faire
        Empiler(D, Depiler(G))
    Ftantque
Fin
```

</details>

**Comportement** : Transfere tous les caracteres de G vers D.

---

### 7. Traitement BackSpace et Clear

#### Enonce

- `#` : Efface le caractere precedent (BackSpace)
- `%` : Efface toute la ligne (Clear)
- `$` : Fin de ligne

**Exemple** : `"Jem# m'euh%Je m'#euh## suit#s trop#mp&#e$"`

Resultat : `"Je me suis trompe"`

<details>
<summary>Solution</summary>

```text
PROGRAMME Edit
VAR
    P, R : Pile
    C : caractere
Debut
    Lire(C)

    Tantque C <> '$' faire
        Si C = '#' Alors
            supp_car(P)
        Sinon Si C = '%' Alors
            supp_ligne(P)
        Sinon
            Empiler(P, C)
        Fsi
        Lire(C)
    Ftantque

    // Inverser la pile pour afficher dans le bon ordre
    Tantque NON pile_vide(P) faire
        Empiler(R, Depiler(P))
    Ftantque

    // Afficher le resultat
    Tantque NON pile_vide(R) faire
        Ecrire(Depiler(R))
    Ftantque
Fin

PROCEDURE supp_car (var P : Pile)
Debut
    Si NON pile_vide(P) Alors
        Depiler(P)
    Fsi
Fin

PROCEDURE supp_ligne (var P : Pile)
Debut
    Tantque NON pile_vide(P) faire
        Depiler(P)
    Ftantque
Fin
```

</details>

---

## Trace d'execution (BackSpace)

```text
Entree : "Jem# m'euh%Je m'#euh## suit#s trop#mp&#e$"

Caractere par caractere :
'J' -> Empiler(P,'J')           P: [J]
'e' -> Empiler(P,'e')           P: [J,e]
'm' -> Empiler(P,'m')           P: [J,e,m]
'#' -> Depiler(P)               P: [J,e]        (efface 'm')
' ' -> Empiler(P,' ')           P: [J,e, ]
'm' -> Empiler(P,'m')           P: [J,e, ,m]
'\'' -> Empiler(P,'\'')         P: [J,e, ,m,']
'e' -> Empiler(P,'e')           P: [J,e, ,m,',e]
'u' -> Empiler(P,'u')           P: [J,e, ,m,',e,u]
'h' -> Empiler(P,'h')           P: [J,e, ,m,',e,u,h]
'%' -> supp_ligne(P)            P: []           (efface tout)
'J' -> Empiler(P,'J')           P: [J]
'e' -> Empiler(P,'e')           P: [J,e]
' ' -> Empiler(P,' ')           P: [J,e, ]
'm' -> Empiler(P,'m')           P: [J,e, ,m]
'\'' -> Empiler(P,'\'')         P: [J,e, ,m,']
'#' -> Depiler(P)               P: [J,e, ,m]    (efface apostrophe)
'e' -> Empiler(P,'e')           P: [J,e, ,m,e]
... (continue)

Resultat final : "Je me suis trompe"
```

---

## Schema des operations

```text
Operation           Avant                      Apres
---------------------------------------------------------------------------
Inserer 'X'         G:[A,B] D:[C,D]           G:[A,B,X] D:[C,D]
                    "AB|CD"                    "ABX|CD"

Effacer             G:[A,B] D:[C,D]           G:[A,B] D:[D]
                    "AB|CD"                    "AB|D"

Avancer             G:[A,B] D:[C,D]           G:[A,B,C] D:[D]
                    "AB|CD"                    "ABC|D"

Reculer             G:[A,B] D:[C,D]           G:[A] D:[B,C,D]
                    "AB|CD"                    "A|BCD"

Debut               G:[A,B] D:[C,D]           G:[] D:[A,B,C,D]
                    "AB|CD"                    "|ABCD"
```

---

## Exercices supplementaires

### Variante A : Aller a la fin de ligne

```text
PROCEDURE Fin_Ligne (var G, D : Pile)
Debut
    Tantque Sommet(D) <> '$' faire
        Empiler(G, Depiler(D))
    Ftantque
Fin
```

### Variante B : Supprimer le caractere precedent

```text
PROCEDURE Suppr_Precedent (var G : Pile)
Debut
    Si NON pile_vide(G) Alors
        Depiler(G)
    Fsi
Fin
```

### Variante C : Compter les caracteres

```text
FONCTION Longueur_Texte (G, D : Pile) : entier
VAR temp : Pile
    compteur : entier
Debut
    compteur := 0

    // Compter G
    Tantque NON pile_vide(G) faire
        compteur := compteur + 1
        Empiler(temp, Depiler(G))
    Ftantque
    // Restaurer G
    Tantque NON pile_vide(temp) faire
        Empiler(G, Depiler(temp))
    Ftantque

    // Compter D (sans le $)
    Tantque Sommet(D) <> '$' faire
        compteur := compteur + 1
        Empiler(temp, Depiler(D))
    Ftantque
    // Restaurer D
    Tantque NON pile_vide(temp) faire
        Empiler(D, Depiler(temp))
    Ftantque

    Retourner compteur
Fin
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-PILE-G.
   05 WS-G-TAB       PIC X(100).
   05 WS-G-PTR       PIC 9(3) VALUE 0.
01 WS-PILE-D.
   05 WS-D-TAB       PIC X(100).
   05 WS-D-PTR       PIC 9(3) VALUE 0.
01 WS-CARACTERE      PIC X.

PROCEDURE DIVISION.
* Inserer un caractere
INSERER-PARA.
    ADD 1 TO WS-G-PTR
    MOVE WS-CARACTERE TO WS-G-TAB(WS-G-PTR:1).

* Avancer le curseur
AVANCER-PARA.
    IF WS-D-PTR > 0
        MOVE WS-D-TAB(WS-D-PTR:1) TO WS-CARACTERE
        SUBTRACT 1 FROM WS-D-PTR
        ADD 1 TO WS-G-PTR
        MOVE WS-CARACTERE TO WS-G-TAB(WS-G-PTR:1)
    END-IF.

* Reculer le curseur
RECULER-PARA.
    IF WS-G-PTR > 0
        MOVE WS-G-TAB(WS-G-PTR:1) TO WS-CARACTERE
        SUBTRACT 1 FROM WS-G-PTR
        ADD 1 TO WS-D-PTR
        MOVE WS-CARACTERE TO WS-D-TAB(WS-D-PTR:1)
    END-IF.
```

---

## Complexite

| Operation | Complexite |
|-----------|------------|
| Inserer | O(1) |
| Effacer | O(1) |
| Avancer | O(1) |
| Reculer | O(1) |
| Rechercher | O(n) |
| Debut de ligne | O(n) |
| BackSpace (#) | O(1) |
| Clear (%) | O(n) |

Cette representation avec deux piles permet des operations d'edition en temps constant pour la plupart des cas.

---

*Exercice 03 - Piles et Files - Algorithmique Chapitre 02-03 - M2i*
