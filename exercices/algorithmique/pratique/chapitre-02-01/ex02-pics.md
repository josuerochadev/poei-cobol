# Exercice 2 : Detection des pics dans un tableau

## Enonce

Un **pic** est une case encadree par des valeurs strictement inferieures.

Ecrire un algorithme qui detecte et affiche tous les pics d'un tableau avec leur position et leurs voisins.

## Exemple

```text
Index :  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 |
Valeur : | 0 | 3 | 5 | 2 | 4 | 0 | 7 | 7 | 7 |  6 |  2 |  5 |  0 |
               ^       ^                            ^
            pic(3)  pic(5)                       pic(12)
```

**Sortie attendue :**

| Valeur | Position | Gauche | Droite |
|--------|----------|--------|--------|
| 5      | 3        | 3      | 2      |
| 4      | 5        | 2      | 0      |
| 5      | 12       | 2      | 0      |

## Concepts a utiliser

- Parcours de tableau avec acces aux voisins
- Condition composee (T[i] > T[i-1] ET T[i] > T[i+1])
- Gestion des bornes (indices 2 a N-1)

## Etapes suggerees

1. Saisir N et remplir le tableau T
2. Parcourir de l'indice 2 a N-1 (les extremites n'ont qu'un voisin)
3. Pour chaque element, verifier s'il est superieur a ses deux voisins
4. Si oui, afficher la valeur, la position et les valeurs des voisins

---

<details>
<summary>Solution</summary>

```text
Programme Pic_Tab
Var   N, i, seuil : entier
      T : Type_TAB

DEBUT
    // Lecture de N avec validation
    Repeter
        Ecrire("Nombre d'elements (>= 3) : ")
        Lire(N)
    Jusqu'a N >= 3 et N <= 100

    // Remplissage avec valeurs positives uniquement
    Pour i de 1 a N Faire
        Repeter
            Ecrire("T[", i, "] = ")
            Lire(T[i])
        Jusqu'a T[i] >= 0
    finPour

    // Detection des pics (de l'indice 2 a N-1)
    Ecrire("Pics detectes :")
    Ecrire("Valeur | Position | Gauche | Droite")
    Ecrire("-------|----------|--------|-------")

    seuil := N - 1
    Pour i de 2 a seuil Faire
        Si T[i] > T[i-1] et T[i] > T[i+1] Alors
            Ecrire(T[i], " | ", i, " | ", T[i-1], " | ", T[i+1])
        finSi
    finPour
FIN
```

**Points cles :**
- On parcourt de 2 a N-1 car les elements aux extremites ne peuvent pas etre des pics
- La condition utilise `>` strict (pas `>=`) car un pic doit etre STRICTEMENT superieur
- Les plateaux (valeurs egales consecutives) ne sont pas des pics

</details>

---

## Variante : Detection des creux

Un **creux** est l'inverse d'un pic : une case encadree par des valeurs strictement superieures.

<details>
<summary>Modification pour detecter les creux</summary>

```text
Si T[i] < T[i-1] et T[i] < T[i+1] Alors
    Ecrire("Creux en position ", i, " : ", T[i])
finSi
```

</details>

---

*Exercice 2 - Algorithmique Chapitre 02 - M2i*
