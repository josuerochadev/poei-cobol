# Exercice 5 : Tri du drapeau neerlandais (Dutch Flag Problem)

## Enonce

Trier un tableau de couleurs (Bleu, Blanc, Rouge) en un seul passage :
- Bleus a gauche
- Blancs au centre
- Rouges a droite

C'est un probleme classique pose par Edsger Dijkstra.

## Exemple

```text
Etat initial :
| R | B | B | R | W | B | W | W | B | W | B | R | W |

Apres tri :
| B | B | B | B | B | W | W | W | W | W | R | R | R |
```

## Concepts a utiliser

- Deux pointeurs (i pour les Bleus, j pour les Rouges)
- Echange (permutation) d'elements
- Partitionnement en 3 zones

## Principe de l'algorithme

1. Maintenir deux pointeurs : `i` (debut) et `j` (fin)
2. Si T[i] est Bleu : avancer i
3. Sinon : chercher un Bleu a droite et permuter
4. Si T[j] est Rouge : reculer j
5. Sinon : chercher un Rouge a gauche et permuter
6. Continuer tant que i < j

---

<details>
<summary>Solution</summary>

```text
Procedure classement(var T : Type_TAB; N : Entier)
Var   i, i', j, j' : Entier

DEBUT
    i := 1
    j := N
    i' := 1
    j' := N

    TantQue i < j Faire
        // ===== Traitement cote gauche (Bleus) =====
        Si T[i] = Bleu Alors
            i := i + 1
        SiNon
            // Chercher le prochain Bleu a droite
            i' := i + 1
            TantQue (i' <= j) et (T[i'] <> Bleu) Faire
                i' := i' + 1
            finTantQue
            Si i' <= j et T[i'] = Bleu Alors
                Permuter(T[i], T[i'])
            finSi
            i := i + 1
        finSi

        // ===== Traitement cote droit (Rouges) =====
        Si T[j] = Rouge Alors
            j := j - 1
        SiNon
            // Chercher le prochain Rouge a gauche
            j' := j - 1
            TantQue (j' >= i) et (T[j'] <> Rouge) Faire
                j' := j' - 1
            finTantQue
            Si j' >= i et T[j'] = Rouge Alors
                Permuter(T[j], T[j'])
            finSi
            j := j - 1
        finSi
    finTantQue
FIN
```

</details>

---

## Trace d'execution

```text
Initial : | R | B | B | R | W | B |
           i=1                 j=6

Etape 1 : T[1]=R, chercher Bleu -> i'=2, permuter
          | B | R | B | R | W | B |
           i=2                 j=6
          T[6]=B, chercher Rouge -> j'=4, permuter
          | B | R | B | R | W | R |
                             j=5

Etape 2 : T[2]=R, chercher Bleu -> i'=3, permuter
          | B | B | R | R | W | R |
           i=3               j=5
          T[5]=W (pas Rouge), j=4

Etape 3 : T[3]=R, pas de Bleu apres, i=4
          i >= j, fin

Resultat : | B | B | R | R | W | R |
                       ^zones partiellement triees
```

---

## Variante : Algorithme de Dijkstra (3-way partitioning)

<details>
<summary>Solution alternative plus elegante</summary>

```text
Procedure dutch_flag(var T : Type_TAB; N : Entier)
Var   low, mid, high : Entier

DEBUT
    low := 1
    mid := 1
    high := N

    TantQue mid <= high Faire
        Si T[mid] = Bleu Alors
            Permuter(T[low], T[mid])
            low := low + 1
            mid := mid + 1
        SiNonSi T[mid] = Blanc Alors
            mid := mid + 1
        SiNon  // T[mid] = Rouge
            Permuter(T[mid], T[high])
            high := high - 1
        finSi
    finTantQue
FIN
```

Cette version est plus simple : un seul parcours avec 3 pointeurs.

</details>

---

## Complexite

- **Temps** : O(n) - un seul parcours du tableau
- **Espace** : O(1) - tri en place, pas de memoire supplementaire

---

*Exercice 5 - Algorithmique Chapitre 02 - M2i*
