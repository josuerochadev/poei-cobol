# Exercice 03 : Fonction d'Ackermann

## Enonce

La fonction d'Ackermann est une fonction recursive celebre en informatique theorique. Elle est definie comme suit :

```text
A(m, n) = n + 1                    si m = 0
A(m, n) = A(m-1, 1)                si m > 0 et n = 0
A(m, n) = A(m-1, A(m, n-1))        si m > 0 et n > 0
```

Ecrire cette fonction de maniere recursive.

---

## Caracteristiques

La fonction d'Ackermann est remarquable car :
- Elle est **totalement definie** (termine toujours)
- Elle croit **extremement vite**
- Elle n'est pas **primitive recursive**
- Elle illustre les limites de la recursivite simple

---

## Solution

<details>
<summary>Implementation</summary>

```text
FONCTION Ackermann (m, n : Entier) : Entier
DEBUT
    Si m = 0 alors
        Ackermann := n + 1
    Sinon Si n = 0 alors
        Ackermann := Ackermann(m - 1, 1)
    Sinon
        Ackermann := Ackermann(m - 1, Ackermann(m, n - 1))
    Fsi
FIN
```

</details>

---

## Trace d'execution

### Exemple : A(1, 2)

```text
A(1, 2)
  m=1 > 0, n=2 > 0 -> A(0, A(1, 1))
    A(1, 1)
      m=1 > 0, n=1 > 0 -> A(0, A(1, 0))
        A(1, 0)
          m=1 > 0, n=0 -> A(0, 1)
            m=0 -> 1 + 1 = 2
          = 2
        A(0, 2)
          m=0 -> 2 + 1 = 3
      = 3
    A(0, 3)
      m=0 -> 3 + 1 = 4
  = 4

Resultat : A(1, 2) = 4
```

### Exemple : A(2, 1)

```text
A(2, 1)
  m=2 > 0, n=1 > 0 -> A(1, A(2, 0))
    A(2, 0)
      m=2 > 0, n=0 -> A(1, 1)
        A(1, 1) = 3  (calcule precedemment)
      = 3
    A(1, 3)
      m=1 > 0, n=3 > 0 -> A(0, A(1, 2))
        A(1, 2) = 4  (calcule precedemment)
      A(0, 4)
        m=0 -> 4 + 1 = 5
      = 5
  = 5

Resultat : A(2, 1) = 5
```

---

## Arbre des appels pour A(2, 2)

```text
                         A(2,2)
                           |
                    A(1, A(2,1))
                   /            \
              A(2,1)          A(1, 5)
                |                |
          A(1, A(2,0))     A(0, A(1,4))
         /          \            |
      A(2,0)      A(1,3)      A(1,4)
        |           |           |
      A(1,1)      A(0,A(1,2))  A(0,A(1,3))
        |           |           |
      A(0,A(1,0)) A(1,2)      A(1,3)
        |           |           |
       ...         ...         ...
```

---

## Table des valeurs

| m \ n | 0 | 1 | 2 | 3 | 4 |
|-------|---|---|---|---|---|
| 0 | 1 | 2 | 3 | 4 | 5 |
| 1 | 2 | 3 | 4 | 5 | 6 |
| 2 | 3 | 5 | 7 | 9 | 11 |
| 3 | 5 | 13 | 29 | 61 | 125 |
| 4 | 13 | 65533 | ... | ... | ... |

**Remarque** : A(4, 2) a plus de 19000 chiffres decimaux !

---

## Formules simplifiees

Pour les petites valeurs de m, on peut etablir des formules :

```text
A(0, n) = n + 1
A(1, n) = n + 2
A(2, n) = 2n + 3
A(3, n) = 2^(n+3) - 3
A(4, n) = 2^2^2^...^2 - 3  (tour de 2 de hauteur n+3)
```

---

## Schema de la croissance

```text
Croissance de A(m, n) :

m=0: lineaire        n+1
m=1: lineaire        n+2
m=2: lineaire        2n+3
m=3: exponentielle   2^(n+3) - 3
m=4: tetration       nombre astronomique

         A(m,n)
           ^
           |     A(4,n)
           |        /
           |       /
           |   A(3,n)
           |     /
           | A(2,n)
           |  /
           | A(1,n)
           |/
     ------+-----------> n
```

---

## Analyse de la complexite

### Nombre d'appels recursifs

Pour A(m, n), le nombre d'appels est :
- **A(0, n)** : 1 appel
- **A(1, n)** : n + 1 appels
- **A(2, n)** : 2n + 3 appels
- **A(3, n)** : 2^(n+3) - 3 appels
- **A(4, n)** : nombre inimaginable

### Profondeur de pile

La profondeur maximale de la pile d'appels peut etre tres grande, ce qui rend le calcul impraticable pour m >= 4.

---

## Exercices supplementaires

### Variante A : Ackermann avec compteur d'appels

```text
FONCTION Ackermann_Compteur (m, n : Entier; VAR compteur : Entier) : Entier
DEBUT
    compteur := compteur + 1

    Si m = 0 alors
        Ackermann_Compteur := n + 1
    Sinon Si n = 0 alors
        Ackermann_Compteur := Ackermann_Compteur(m - 1, 1, compteur)
    Sinon
        Ackermann_Compteur := Ackermann_Compteur(m - 1,
                               Ackermann_Compteur(m, n - 1, compteur),
                               compteur)
    Fsi
FIN
```

### Variante B : Ackermann iteratif (avec pile)

```text
FONCTION Ackermann_Iter (m, n : Entier) : Entier
VAR P : Pile
DEBUT
    Empiler(P, m)

    Tantque NON pile_vide(P) faire
        m := Depiler(P)

        Si m = 0 alors
            n := n + 1
        Sinon Si n = 0 alors
            Empiler(P, m - 1)
            n := 1
        Sinon
            Empiler(P, m - 1)
            Empiler(P, m)
            n := n - 1
        Fsi
    Ftantque

    Ackermann_Iter := n
FIN
```

### Variante C : Verification des arguments

```text
FONCTION Ackermann_Safe (m, n : Entier) : Entier
DEBUT
    // Limiter pour eviter le debordement de pile
    Si m < 0 OU n < 0 alors
        Ackermann_Safe := -1  // Erreur
    Sinon Si m > 3 OU (m = 3 ET n > 10) alors
        Ackermann_Safe := -2  // Trop grand
    Sinon
        Ackermann_Safe := Ackermann(m, n)
    Fsi
FIN
```

---

## Applications

1. **Test de compilateurs** : La fonction d'Ackermann est utilisee pour tester l'optimisation de la recursivite
2. **Benchmark** : Mesure de performance des appels recursifs
3. **Theorie de la calculabilite** : Exemple de fonction non primitive recursive

---

## Points cles

1. **Trois cas de base** : Distinguer m=0, n=0, et le cas general
2. **Double recursivite** : Le cas general contient un appel recursif dans un autre
3. **Croissance explosive** : Meme pour de petites valeurs, les resultats sont enormes
4. **Importance theorique** : Demontre les limites de la recursivite primitive

---

*Exercice 03 - Recursivite - Algorithmique Chapitre 03 - M2i*
