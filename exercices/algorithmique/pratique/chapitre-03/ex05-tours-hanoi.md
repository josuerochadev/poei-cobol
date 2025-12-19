# Exercice 05 : Tours de Hanoi

## Enonce

Le probleme des Tours de Hanoi est un jeu de reflexion classique :

- On dispose de 3 piquets (A, B, C) et de N disques de tailles differentes
- Au depart, tous les disques sont empiles sur le piquet A, du plus grand (en bas) au plus petit (en haut)
- L'objectif est de deplacer tous les disques vers le piquet C

**Regles** :
1. On ne peut deplacer qu'un seul disque a la fois
2. Un disque ne peut etre pose que sur un disque plus grand ou sur un piquet vide
3. On ne peut prendre que le disque au sommet d'une pile

---

## Schema initial

```text
Etat initial (N = 3 disques) :

     |              |              |
    [1]             |              |
   [ 2 ]            |              |
  [  3  ]           |              |
-----------    -----------    -----------
  Piquet A       Piquet B       Piquet C
  (depart)     (auxiliaire)    (arrivee)
```

## Etat final souhaite

```text
     |              |              |
     |              |             [1]
     |              |            [ 2 ]
     |              |           [  3  ]
-----------    -----------    -----------
  Piquet A       Piquet B       Piquet C
```

---

## Principe recursif

Pour deplacer N disques de A vers C en utilisant B comme intermediaire :

1. **Deplacer N-1 disques** de A vers B (en utilisant C comme intermediaire)
2. **Deplacer le disque N** (le plus grand) de A vers C
3. **Deplacer N-1 disques** de B vers C (en utilisant A comme intermediaire)

### Schema du principe

```text
Etape 1: Deplacer N-1 disques de A vers B
     |              |              |
     |             [1]             |
     |            [ 2 ]            |
  [  3  ]           |              |
-----------    -----------    -----------
     A              B              C

Etape 2: Deplacer le disque 3 de A vers C
     |              |              |
     |             [1]             |
     |            [ 2 ]            |
     |              |           [  3  ]
-----------    -----------    -----------
     A              B              C

Etape 3: Deplacer N-1 disques de B vers C
     |              |              |
     |              |             [1]
     |              |            [ 2 ]
     |              |           [  3  ]
-----------    -----------    -----------
     A              B              C
```

---

## Solution

<details>
<summary>Solution</summary>

```text
PROCEDURE Hanoi (n : Entier; depart, arrivee, intermediaire : Caractere)
DEBUT
    Si n > 0 alors
        // Deplacer n-1 disques du depart vers l'intermediaire
        Hanoi(n - 1, depart, intermediaire, arrivee)

        // Deplacer le disque n du depart vers l'arrivee
        Ecrire("Deplacer disque ", n, " de ", depart, " vers ", arrivee)

        // Deplacer n-1 disques de l'intermediaire vers l'arrivee
        Hanoi(n - 1, intermediaire, arrivee, depart)
    Fsi
FIN
```

</details>

### Appel initial

```text
Hanoi(3, 'A', 'C', 'B')
// Deplacer 3 disques de A vers C en utilisant B
```

---

## Trace d'execution pour N = 3

```text
Hanoi(3, A, C, B)
|  Hanoi(2, A, B, C)
|  |  Hanoi(1, A, C, B)
|  |  |  Hanoi(0, A, B, C) -> rien
|  |  |  Ecrire "Deplacer 1 de A vers C"
|  |  |  Hanoi(0, B, C, A) -> rien
|  |  Ecrire "Deplacer 2 de A vers B"
|  |  Hanoi(1, C, B, A)
|  |  |  Hanoi(0, C, A, B) -> rien
|  |  |  Ecrire "Deplacer 1 de C vers B"
|  |  |  Hanoi(0, A, B, C) -> rien
|  Ecrire "Deplacer 3 de A vers C"
|  Hanoi(2, B, C, A)
|  |  Hanoi(1, B, A, C)
|  |  |  Hanoi(0, B, C, A) -> rien
|  |  |  Ecrire "Deplacer 1 de B vers A"
|  |  |  Hanoi(0, C, A, B) -> rien
|  |  Ecrire "Deplacer 2 de B vers C"
|  |  Hanoi(1, A, C, B)
|  |  |  Hanoi(0, A, B, C) -> rien
|  |  |  Ecrire "Deplacer 1 de A vers C"
|  |  |  Hanoi(0, B, C, A) -> rien

Mouvements (dans l'ordre) :
1. Deplacer disque 1 de A vers C
2. Deplacer disque 2 de A vers B
3. Deplacer disque 1 de C vers B
4. Deplacer disque 3 de A vers C
5. Deplacer disque 1 de B vers A
6. Deplacer disque 2 de B vers C
7. Deplacer disque 1 de A vers C
```

---

## Visualisation etape par etape

```text
Etat initial :
  [1]      |       |
 [ 2 ]     |       |
[  3  ]    |       |
---A---  ---B---  ---C---

Apres mouvement 1 (1: A->C) :
   |       |       |
 [ 2 ]     |       |
[  3  ]    |      [1]
---A---  ---B---  ---C---

Apres mouvement 2 (2: A->B) :
   |       |       |
   |       |       |
[  3  ]  [ 2 ]    [1]
---A---  ---B---  ---C---

Apres mouvement 3 (1: C->B) :
   |       |       |
   |      [1]      |
[  3  ]  [ 2 ]     |
---A---  ---B---  ---C---

Apres mouvement 4 (3: A->C) :
   |       |       |
   |      [1]      |
   |     [ 2 ]  [  3  ]
---A---  ---B---  ---C---

Apres mouvement 5 (1: B->A) :
   |       |       |
   |       |       |
  [1]    [ 2 ]  [  3  ]
---A---  ---B---  ---C---

Apres mouvement 6 (2: B->C) :
   |       |       |
   |       |     [ 2 ]
  [1]      |    [  3  ]
---A---  ---B---  ---C---

Apres mouvement 7 (1: A->C) :
   |       |      [1]
   |       |     [ 2 ]
   |       |    [  3  ]
---A---  ---B---  ---C---

TERMINE !
```

---

## Nombre de mouvements

### Formule

Pour N disques, le nombre de mouvements est : **2^N - 1**

| N disques | Mouvements |
|-----------|------------|
| 1 | 1 |
| 2 | 3 |
| 3 | 7 |
| 4 | 15 |
| 5 | 31 |
| 10 | 1023 |
| 20 | 1 048 575 |
| 64 | 18 446 744 073 709 551 615 |

### Demonstration recursive

```text
M(1) = 1
M(n) = M(n-1) + 1 + M(n-1) = 2 * M(n-1) + 1

En developpant :
M(n) = 2^n - 1
```

---

## Exercices supplementaires

### Variante A : Compter les mouvements

```text
FONCTION Hanoi_Compte (n : Entier) : Entier
DEBUT
    Si n = 0 alors
        Hanoi_Compte := 0
    Sinon
        Hanoi_Compte := 2 * Hanoi_Compte(n - 1) + 1
    Fsi
FIN
```

### Variante B : Formule directe

```text
FONCTION Nb_Mouvements (n : Entier) : Entier
DEBUT
    Nb_Mouvements := Puissance(2, n) - 1
FIN
```

### Variante C : Hanoi avec affichage des etats

```text
PROCEDURE Hanoi_Etats (n : Entier; dep, arr, inter : Caractere;
                       VAR A, B, C : Pile)
DEBUT
    Si n > 0 alors
        Hanoi_Etats(n - 1, dep, inter, arr, A, B, C)

        // Effectuer le deplacement
        Si dep = 'A' alors
            Si arr = 'B' alors Empiler(B, Depiler(A))
            Sinon Empiler(C, Depiler(A))
            Fsi
        Sinon Si dep = 'B' alors
            Si arr = 'A' alors Empiler(A, Depiler(B))
            Sinon Empiler(C, Depiler(B))
            Fsi
        Sinon
            Si arr = 'A' alors Empiler(A, Depiler(C))
            Sinon Empiler(B, Depiler(C))
            Fsi
        Fsi

        Afficher_Etat(A, B, C)
        Hanoi_Etats(n - 1, inter, arr, dep, A, B, C)
    Fsi
FIN
```

### Variante D : Hanoi iteratif

```text
PROCEDURE Hanoi_Iteratif (n : Entier)
VAR i, total : Entier
    source, dest : Caractere
DEBUT
    total := Puissance(2, n) - 1

    Pour i de 1 a total faire
        // Determiner source et destination selon le numero du mouvement
        // Algorithme base sur la representation binaire de i
        source := Piquet_Source(i, n)
        dest := Piquet_Dest(i, n)
        Ecrire("Mouvement ", i, ": ", source, " -> ", dest)
    Fpour
FIN
```

---

## Complexite

| Aspect | Valeur |
|--------|--------|
| Nombre de mouvements | O(2^n) |
| Profondeur de recursion | O(n) |
| Espace pile | O(n) |

**Note** : La complexite exponentielle rend le probleme impraticable pour de grandes valeurs de N.

---

## La legende

Selon la legende, des moines dans un temple deplacent 64 disques d'or selon ces regles. Quand ils auront termine, le monde prendra fin.

```text
Nombre de mouvements pour 64 disques :
2^64 - 1 = 18 446 744 073 709 551 615 mouvements

A raison d'un mouvement par seconde :
= environ 585 milliards d'annees
(soit 42 fois l'age de l'univers !)
```

---

## Arbre des appels recursifs (N = 3)

```text
                        Hanoi(3,A,C,B)
                      /       |        \
              Hanoi(2,A,B,C)  3:A->C   Hanoi(2,B,C,A)
              /     |     \           /     |     \
      Hanoi(1,..)  2:A->B  Hanoi(1,..)  Hanoi(1,..)  2:B->C  Hanoi(1,..)
         / | \              / | \        / | \              / | \
        ...1:A->C...       ...1:C->B...  ...1:B->A...       ...1:A->C...
```

---

## Points cles

1. **Cas de base** : n = 0, rien a faire
2. **Decomposition** : Probleme de taille N se reduit a 2 problemes de taille N-1
3. **Intermediaire** : Utilisation du piquet auxiliaire crucial
4. **Optimalite** : L'algorithme recursif donne le nombre minimum de mouvements
5. **Croissance exponentielle** : Le nombre de mouvements double (+1) a chaque disque ajoute

---

*Exercice 05 - Recursivite - Algorithmique Chapitre 03 - M2i*
