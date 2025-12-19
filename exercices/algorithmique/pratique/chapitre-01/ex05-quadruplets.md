# Exercice 5 : Min/Max de Quadruplets

## Enonce

Lire une suite de N quadruplets (un quadruplet est une suite de 4 nombres).
- Pour chaque quadruplet, afficher le Minimum et le Maximum des 4 nombres
- Puis, afficher le plus petit et le plus grand de tous les nombres

## Exemple

| # | Val1 | Val2 | Val3 | Val4 | Min | Max |
|---|------|------|------|------|-----|-----|
| 1 | 6    | 1    | 5    | 7    | 1   | 7   |
| 2 | 8    | 17   | 3    | 5    | 3   | 17  |
| 3 | 5    | 25   | 41   | 82   | 5   | 82  |
| 4 | 23   | 15   | 95   | 7    | 7   | 95  |
| 5 | 3    | 4    | 19   | 24   | 3   | 24  |
| 6 | 62   | 47   | 51   | 35   | 35  | 62  |
| 7 | 26   | 53   | 14   | 42   | 14  | 53  |

**Resultat global** : Min absolu = 1, Max absolu = 95

## Concepts a utiliser

- Boucles imbriquees
- Recherche de minimum et maximum
- Initialisation des extremums

## Etapes suggerees

1. Initialiser min_abs a +infini et max_abs a -infini
2. Pour chaque quadruplet :
   - Lire le premier nombre (initialise min et max du quadruplet)
   - Lire les 3 suivants en mettant a jour min et max
   - Afficher min et max du quadruplet
   - Mettre a jour les extremums absolus
3. Afficher min_abs et max_abs

---

<details>
<summary>Solution</summary>

```text
Algorithme quadruplets
var min, max, min_abs, max_abs : Entier
    x : Entier
    N : Entier
    i, j : Entier

DEBUT
    Ecrire("Combien de quadruplets ? ")
    Lire(N)

    min_abs := +infini
    max_abs := -infini

    Pour i de 1 a N Faire
        // Lecture du premier nombre du quadruplet
        Ecrire("Quadruplet ", i, " - Nombre 1 : ")
        Lire(x)
        min := x
        max := x

        // Lecture des 3 nombres suivants
        Pour j de 2 a 4 Faire
            Ecrire("Quadruplet ", i, " - Nombre ", j, " : ")
            Lire(x)

            Si x < min Alors
                min := x
            finSi

            Si x > max Alors
                max := x
            finSi
        finPour

        Ecrire("Minimum du quadruplet : ", min)
        Ecrire("Maximum du quadruplet : ", max)

        // Mise a jour des extremums absolus
        Si min < min_abs Alors
            min_abs := min
        finSi

        Si max > max_abs Alors
            max_abs := max
        finSi
    finPour

    Ecrire("Minimum absolu de tous les quadruplets : ", min_abs)
    Ecrire("Maximum absolu de tous les quadruplets : ", max_abs)
FIN
```

**Points cles :**
- **Initialisation des extremums absolus** : +infini pour min_abs, -infini pour max_abs
- **Double boucle** : externe pour les quadruplets, interne pour les 4 valeurs
- Le premier element initialise min et max du quadruplet courant
- On peut aussi utiliser min_abs := premier nombre lu (sans +infini)

</details>

---

*Exercice 5 - Algorithmique M2i*
