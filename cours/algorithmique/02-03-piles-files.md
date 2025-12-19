# Chapitre II (Partie 3) - Piles et Files (TAD)

## 1. Introduction aux listes lineaires

Soit T un type de donnees. Une **liste lineaire** de valeurs de type T est une suite de n (n >= 0) valeurs v1, ..., vn telle que :
- Si n > 0, v1 est la **premiere** valeur et vn la **derniere**
- Si 1 < k < n, vk est precedee de vk-1 et suivie de vk+1

### Types Abstraits de Donnees (TAD)

Un **TAD** definit une structure de donnees par :
- Son **interface** : les operations disponibles
- Ses **proprietes** : les regles de comportement
- Sans specifier l'**implementation** interne

Deux TAD fondamentaux bases sur les listes lineaires :
- **Pile (Stack)** : acces LIFO
- **File (Queue)** : acces FIFO

---

## 2. Les Piles (Stack)

### Definition

Une **pile** est une liste lineaire geree selon le principe **LIFO** (Last In - First Out).

- Toutes les operations se font au **sommet** (top)
- Le dernier element empile est le premier depile

### Analogie

Comme une pile d'assiettes :
- On pose (empile) une assiette sur le dessus
- On prend (depile) l'assiette du dessus

```
    ┌─────┐
    │  C  │  ← Sommet (dernier entre, premier sorti)
    ├─────┤
    │  B  │
    ├─────┤
    │  A  │  ← Base (premier entre, dernier sorti)
    └─────┘
```

### Applications des piles

| Application | Description |
|-------------|-------------|
| Appels recursifs | La pile d'execution stocke les contextes |
| Navigation Web | Bouton "Retour" (historique) |
| Expressions postfixees | Evaluation de calculatrices |
| Fonction Annuler (Undo) | Historique des actions |
| Parcours en profondeur | Algorithmes de graphes (DFS) |
| Inversion | Inverser un tableau ou une chaine |

---

## 3. Operations primitives sur les piles (TAD)

On note `Pile(T)` le type pile de valeurs de type T.

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Init_P` | `Init_P(var P : Pile)` | Initialise une pile vide |
| `Vide` | `Vide(P : Pile) : booleen` | Teste si la pile est vide |
| `Pleine` | `Pleine(P : Pile) : booleen` | Teste si la pile est pleine |
| `Sommet` | `Sommet(P : Pile) : valeur` | Retourne l'element au sommet (sans le retirer) |
| `Empiler` | `Empiler(var P : Pile, x : valeur)` | Ajoute x au sommet (Push) |
| `Depiler` | `Depiler(var P : Pile) : valeur` | Retire et retourne le sommet (Pop) |

### Preconditions

- **Empiler** : la pile ne doit pas etre pleine
- **Depiler / Sommet** : la pile ne doit pas etre vide

---

## 4. Implementation statique d'une pile (tableau)

### Structure

```
PILE = enreg
    stack : tableau[1..MAX] de valeur
    top   : entier    // Indice du sommet (0 si vide)
Fenreg
```

### Representation visuelle

```
Indices:   1     2     3     4     5    ...   MAX
         ┌─────┬─────┬─────┬─────┬─────┬───┬─────┐
stack:   │  A  │  B  │  C  │     │     │...│     │
         └─────┴─────┴─────┴─────┴─────┴───┴─────┘
                       ↑
                      top = 3
```

### Operations

#### Initialisation

```
Procedure Init_pile(var P : PILE)
Debut
    P.top := 0
Fin
```

#### Test pile vide

```
Fonction Pile_vide(P : PILE) : booleen
Debut
    Retourner (P.top = 0)
Fin
```

#### Test pile pleine

```
Fonction Pile_pleine(P : PILE) : booleen
Debut
    Retourner (P.top = MAX)
Fin
```

#### Empiler (Push)

```
Procedure Empiler(var P : PILE, x : valeur)
Debut
    // Precondition : NON Pile_pleine(P)
    P.top := P.top + 1
    P.stack[P.top] := x
Fin
```

#### Depiler (Pop)

```
Fonction Depiler(var P : PILE) : valeur
Var resultat : valeur
Debut
    // Precondition : NON Pile_vide(P)
    resultat := P.stack[P.top]
    P.top := P.top - 1
    Retourner resultat
Fin
```

#### Consulter le sommet

```
Fonction Sommet(P : PILE) : valeur
Debut
    // Precondition : NON Pile_vide(P)
    Retourner P.stack[P.top]
Fin
```

### Complexite

Toutes les operations sont en **O(1)** (temps constant).

---

## 5. Implementation dynamique d'une pile (liste chainee)

### Structure

```
noeud = enreg
    val  : valeur
    next : ^noeud
Fenreg

PILE : ^noeud    // Pointeur vers le sommet
```

### Representation visuelle

```
P --> [C|next] --> [B|next] --> [A|next] --> NIL
       ↑
     Sommet
```

### Operations

#### Initialisation

```
Procedure Init_pile(var P : PILE)
Debut
    P := NIL
Fin
```

#### Test pile vide

```
Fonction Pile_vide(P : PILE) : booleen
Debut
    Retourner (P = NIL)
Fin
```

#### Empiler (insertion en tete)

```
Procedure Empiler(var P : PILE, x : valeur)
Var nouveau : ^noeud
Debut
    Creer(nouveau)
    nouveau^.val := x
    nouveau^.next := P
    P := nouveau
Fin
```

#### Depiler (suppression en tete)

```
Fonction Depiler(var P : PILE) : valeur
Var resultat : valeur
    temp : ^noeud
Debut
    // Precondition : NON Pile_vide(P)
    resultat := P^.val
    temp := P
    P := P^.next
    Liberer(temp)
    Retourner resultat
Fin
```

#### Consulter le sommet

```
Fonction Sommet(P : PILE) : valeur
Debut
    // Precondition : NON Pile_vide(P)
    Retourner P^.val
Fin
```

### Avantages de l'implementation dynamique

- Pas de limite de taille (sauf memoire disponible)
- Pas de gaspillage memoire

---

## 6. Les Files (Queue)

### Definition

Une **file** est une liste lineaire geree selon le principe **FIFO** (First In - First Out).

- Insertion a l'**arriere** (queue)
- Suppression a l'**avant** (tete)

### Analogie

Comme une file d'attente :
- Les nouveaux arrivent a la fin
- Les premiers arrives sont servis en premier

```
Sortie ←  [A] [B] [C] [D]  ← Entree
          ↑               ↑
        Avant          Arriere
        (first)        (last)
```

### Applications des files

| Application | Description |
|-------------|-------------|
| Files d'impression | Ordonnancement des travaux |
| Transactions | Files de messages (queues) |
| Ordonnancement | Gestion des processus (Round Robin) |
| Parcours en largeur | Algorithmes de graphes (BFS) |
| Buffers | Tampons de communication |
| Gestion des stocks | FIFO physique (perissables) |

---

## 7. Operations primitives sur les files (TAD)

On note `File(T)` le type file de valeurs de type T.

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Init_F` | `Init_F(var F : File)` | Initialise une file vide |
| `Vide` | `Vide(F : File) : booleen` | Teste si la file est vide |
| `Pleine` | `Pleine(F : File) : booleen` | Teste si la file est pleine |
| `Enfiler` | `Enfiler(var F : File, x : valeur)` | Ajoute x a l'arriere (Enqueue) |
| `Defiler` | `Defiler(var F : File) : valeur` | Retire et retourne l'avant (Dequeue) |

### Preconditions

- **Enfiler** : la file ne doit pas etre pleine
- **Defiler** : la file ne doit pas etre vide

---

## 8. Implementation statique d'une file (tableau circulaire)

### Probleme du tableau lineaire

Avec un tableau simple, apres plusieurs enfilements/defilements, l'espace au debut est gaspille.

### Solution : tableau circulaire

Les indices "bouclent" : apres MAX, on revient a 1.

### Structure

```
FILE = enreg
    fifo  : tableau[1..MAX] de valeur
    first : entier    // Indice du premier element
    last  : entier    // Indice du dernier element
Fenreg
```

### Representation visuelle

```
         1     2     3     4     5
       ┌─────┬─────┬─────┬─────┬─────┐
fifo:  │     │  B  │  C  │  D  │     │
       └─────┴─────┴─────┴─────┴─────┘
               ↑           ↑
             first=2     last=4

Apres circularite (MAX=5, on ajoute E puis F) :
         1     2     3     4     5
       ┌─────┬─────┬─────┬─────┬─────┐
fifo:  │  F  │  B  │  C  │  D  │  E  │
       └─────┴─────┴─────┴─────┴─────┘
         ↑     ↑
       last=1 first=2
```

### Operations

#### Initialisation

```
Procedure Init_file(var F : FILE)
Debut
    F.first := 1
    F.last := 0
Fin
```

#### Test file vide

```
Fonction File_vide(F : FILE) : booleen
Debut
    Si F.last = MAX Alors
        Retourner (F.first = 1)
    Sinon
        Retourner (F.first = F.last + 1)
    Fsi
Fin
```

#### Test file pleine

```
Fonction File_pleine(F : FILE) : booleen
Var next_last : entier
Debut
    Si F.last = MAX Alors
        next_last := 1
    Sinon
        next_last := F.last + 1
    Fsi
    Retourner (next_last = F.first) OU (next_last + 1 = F.first)
Fin
```

#### Enfiler (Enqueue)

```
Procedure Enfiler(var F : FILE, x : valeur)
Debut
    // Precondition : NON File_pleine(F)
    Si F.last = MAX Alors
        F.last := 1
    Sinon
        F.last := F.last + 1
    Fsi
    F.fifo[F.last] := x
Fin
```

#### Defiler (Dequeue)

```
Fonction Defiler(var F : FILE) : valeur
Var resultat : valeur
Debut
    // Precondition : NON File_vide(F)
    resultat := F.fifo[F.first]
    Si F.first = MAX Alors
        F.first := 1
    Sinon
        F.first := F.first + 1
    Fsi
    Retourner resultat
Fin
```

### Gestion du modulo (alternative)

On peut simplifier avec l'operateur modulo :

```
F.last := (F.last MOD MAX) + 1
F.first := (F.first MOD MAX) + 1
```

---

## 9. Implementation dynamique d'une file (liste chainee F/L)

### Structure

```
noeud = enreg
    val  : valeur
    next : ^noeud
Fenreg

FILE = enreg
    first : ^noeud    // Tete (sortie)
    last  : ^noeud    // Queue (entree)
Fenreg
```

### Representation visuelle

```
F.first --> [A|next] --> [B|next] --> [C|next] --> NIL
                                        ↑
                                     F.last
```

### Operations

#### Initialisation

```
Procedure Init_file(var F : FILE)
Debut
    F.first := NIL
    F.last := NIL
Fin
```

#### Test file vide

```
Fonction File_vide(F : FILE) : booleen
Debut
    Retourner (F.first = NIL)
Fin
```

#### Enfiler (insertion en queue)

```
Procedure Enfiler(var F : FILE, x : valeur)
Var nouveau : ^noeud
Debut
    Creer(nouveau)
    nouveau^.val := x
    nouveau^.next := NIL

    Si F.last = NIL Alors
        // File etait vide
        F.first := nouveau
        F.last := nouveau
    Sinon
        F.last^.next := nouveau
        F.last := nouveau
    Fsi
Fin
```

#### Defiler (suppression en tete)

```
Fonction Defiler(var F : FILE) : valeur
Var resultat : valeur
    temp : ^noeud
Debut
    // Precondition : NON File_vide(F)
    resultat := F.first^.val
    temp := F.first
    F.first := F.first^.next

    Si F.first = NIL Alors
        // File devient vide
        F.last := NIL
    Fsi

    Liberer(temp)
    Retourner resultat
Fin
```

---

## 10. Comparaison Pile vs File

| Aspect | Pile (LIFO) | File (FIFO) |
|--------|-------------|-------------|
| Principe | Dernier entre, premier sorti | Premier entre, premier sorti |
| Insertion | Au sommet | A l'arriere |
| Suppression | Au sommet | A l'avant |
| Analogie | Pile d'assiettes | File d'attente |
| Parcours | En profondeur (DFS) | En largeur (BFS) |

## 11. Comparaison des implementations

| Aspect | Statique (tableau) | Dynamique (liste) |
|--------|-------------------|-------------------|
| Taille | Fixe (MAX) | Illimitee |
| Memoire | Pre-allouee | A la demande |
| Complexite | O(1) toutes operations | O(1) toutes operations |
| Overhead | Aucun | Pointeurs supplementaires |
| File | Necessite circularite | Structure F/L |

---

## 12. Correspondance avec COBOL

| Algorithmique | COBOL |
|---------------|-------|
| Pile statique | Tableau OCCURS avec indice TOP |
| File circulaire | Tableau OCCURS avec FIRST/LAST |
| Empiler | `ADD 1 TO TOP`, `MOVE x TO STACK(TOP)` |
| Depiler | `MOVE STACK(TOP) TO x`, `SUBTRACT 1 FROM TOP` |

### Exemple COBOL (pile)

```cobol
01 WS-PILE.
   05 WS-STACK OCCURS 100 TIMES PIC 9(4).
   05 WS-TOP PIC 9(3) VALUE 0.

EMPILER-ELEMENT.
    ADD 1 TO WS-TOP.
    MOVE WS-VALEUR TO WS-STACK(WS-TOP).

DEPILER-ELEMENT.
    MOVE WS-STACK(WS-TOP) TO WS-VALEUR.
    SUBTRACT 1 FROM WS-TOP.
```

---

## 13. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-02-03-piles-files.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-02/)

---

*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
