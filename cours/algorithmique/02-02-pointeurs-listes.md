# Chapitre II (Partie 2) - Pointeurs et Listes Chainees

## 1. Variables et adresses memoire

### Principe fondamental

Chaque variable en memoire possede :
- Un **nom** (identifiant utilise par le programmeur)
- Une **valeur** (le contenu stocke)
- Une **adresse** (l'emplacement physique en memoire)

```
Algorithme Exemple
Var X, Y : entier
    M : reel
Debut
    Ecrire("Donner 2 entiers :")
    Lire(X, Y)
    M := (X + Y) / 2
    Ecrire("Moyenne =", M)
Fin
```

Le processeur ne manipule pas les noms de variables, mais leurs **adresses memoire**.

### Organisation de la memoire

| Concept | Description |
|---------|-------------|
| Mot memoire | Unite de base, taille fixe |
| Adresse | Numero unique identifiant chaque mot |
| Variable | Peut occuper plusieurs mots consecutifs |
| Adresse variable | Adresse de son premier mot |

### Cas des enregistrements

L'adresse d'un champ depend de :
- L'adresse de base de l'enregistrement
- La taille des champs precedents

```
Etud = enreg
    Mat : entier        // Adresse base
    Nom : chaine[20]    // Adresse base + taille(entier)
    Moy : reel          // Adresse base + taille(entier) + 20
    Sec : caractere     // ...
Fenreg
```

---

## 2. Notion de pointeur

### Definition

Un **pointeur** est une variable qui contient l'adresse d'une autre variable.

### Notation

| Notation | Signification |
|----------|---------------|
| `X` | Valeur de la variable X |
| `^X` | Adresse de X (pointeur vers X) |

**Exemple** :
```
X = 150        // X contient la valeur 150
^X = 654521    // X est stockee a l'adresse 654521
```

### Pointeur et pointe

```
P := ^X
```

- `P` est le **pointeur** (contient l'adresse)
- `X` est le **pointe** (la variable referencee)

### Declaration des pointeurs

```
pEnt  : ^entier         // Pointeur vers un entier
pRl   : ^reel           // Pointeur vers un reel
pEtud : ^T_Etudiant     // Pointeur vers un enregistrement
```

### Acces au pointe (dereferencement)

```
pEnt^           // Valeur de l'entier pointe par pEnt
pEtud^.Nom      // Champ Nom de l'enregistrement pointe
```

---

## 3. Manipulation des pointeurs

### Operations autorisees

| Operation | Syntaxe | Description |
|-----------|---------|-------------|
| Affectation | `P := Q` | P pointe vers la meme zone que Q |
| Modification | `P^ := valeur` | Modifie la valeur pointee |
| Lecture | `Lire(P^)` | Lit une valeur dans la zone pointee |
| Ecriture | `Ecrire(P^)` | Affiche la valeur pointee |
| Comparaison | `P = Q` | Compare les adresses |
| Test nil | `P = NIL` | Verifie si P ne pointe vers rien |

### La valeur NIL

`NIL` (ou `NULL`) represente un pointeur qui ne pointe vers rien.

```
P := NIL    // P ne reference aucune zone memoire
```

### Exemple detaille

```
Algorithme Pointeurs
Var P, Q : ^entier
Debut
    Creer(P)          // Alloue une zone pour un entier, P pointe dessus
    Lire(P^)          // Lit une valeur dans cette zone (ex: 10)
    Q := P            // Q pointe vers la meme zone que P
    Q^ := P^ + 5      // Modifie la valeur (10 + 5 = 15)
    Creer(Q)          // Alloue une NOUVELLE zone pour Q
    Q^ := P^          // Copie la valeur de P dans Q (15)
    P := NIL          // P ne pointe plus vers rien
    Ecrire(Q^)        // Affiche 15
    Liberer(Q)        // Libere la memoire de Q
Fin
```

### Attention aux pieges

**Fuite memoire** : Zone allouee mais plus referencee
```
Creer(P)
P := NIL    // La zone allouee est perdue !
```

**Pointeur pendant** : Pointeur vers une zone liberee
```
Liberer(P)
Ecrire(P^)  // ERREUR : P pointe vers une zone liberee
```

---

## 4. Gestion dynamique de la memoire

### Allocation statique vs dynamique

| Aspect | Statique | Dynamique |
|--------|----------|-----------|
| Moment | Compilation | Execution |
| Taille | Fixe | Variable |
| Duree de vie | Tout le programme | Controlee |
| Acces | Nom de variable | Pointeur |

### Operations fondamentales

```
Creer(p)     // Alloue une zone memoire, p pointe dessus
Liberer(p)   // Libere la zone pointee par p
```

### Regles importantes

```
// INTERDIT : affecter une valeur directement a un pointeur
p := 10        // ERREUR

// CORRECT : affecter une valeur au pointe
p^ := 10       // OK (si p a ete alloue)
```

### Bonnes pratiques

1. **Toujours liberer** apres usage
2. **Mettre a NIL** apres liberation
3. **Tester avant utilisation** : `Si P <> NIL Alors...`
4. **Eviter les copies superficielles** sans comprendre les implications

```
// Patron de liberation propre
Si P <> NIL Alors
    Liberer(P)
    P := NIL
Fsi
```

---

## 5. Listes simplement chainees

### Definition

Une **liste chainee** est une structure de donnees composee de noeuds relies entre eux par des pointeurs.

```
noeud = enreg
    val  : <type_valeur>
    next : ^noeud
Fenreg

L : ^noeud    // Pointeur vers le premier noeud (tete)
```

### Representation visuelle

```
L --> [val1|next] --> [val2|next] --> [val3|next] --> NIL
```

### Avantages et inconvenients

| Avantages | Inconvenients |
|-----------|---------------|
| Taille dynamique | Acces sequentiel O(N) |
| Insertion/Suppression O(1) | Memoire supplementaire (pointeurs) |
| Pas de decalage | Pas d'acces direct par indice |

### Operations fondamentales

#### Initialisation

```
L := NIL    // Liste vide
```

#### Test liste vide

```
Fonction EstVide(L : ^noeud) : booleen
Debut
    Retourner (L = NIL)
Fin
```

#### Parcours

```
Procedure Parcours(L : ^noeud)
Var p : ^noeud
Debut
    p := L
    Tant que p <> NIL faire
        Traiter(p^.val)
        p := p^.next
    Fait
Fin
```

#### Recherche sequentielle

```
Fonction Recherche(L : ^noeud, x : valeur) : ^noeud
Var p : ^noeud
Debut
    p := L
    Tant que (p <> NIL) ET (p^.val <> x) faire
        p := p^.next
    Fait
    Retourner p    // NIL si non trouve
Fin
```

#### Insertion en tete (O(1))

```
Procedure Insere_tete(var L : ^noeud, x : valeur)
Var p : ^noeud
Debut
    Creer(p)
    p^.val := x
    p^.next := L
    L := p
Fin
```

#### Insertion en queue (O(N))

```
Procedure Insere_queue(var L : ^noeud, x : valeur)
Var p, q : ^noeud
Debut
    Creer(p)
    p^.val := x
    p^.next := NIL

    Si L = NIL Alors
        L := p
    Sinon
        q := L
        Tant que q^.next <> NIL faire
            q := q^.next
        Fait
        q^.next := p
    Fsi
Fin
```

#### Suppression en tete (O(1))

```
Procedure Supprime_tete(var L : ^noeud)
Var p : ^noeud
Debut
    Si L <> NIL Alors
        p := L
        L := L^.next
        Liberer(p)
    Fsi
Fin
```

#### Suppression d'une valeur

```
Procedure Supprime_val(var L : ^noeud, x : valeur)
Var p, prec : ^noeud
Debut
    Si L = NIL Alors
        Retourner    // Liste vide
    Fsi

    // Cas special : suppression en tete
    Si L^.val = x Alors
        Supprime_tete(L)
        Retourner
    Fsi

    // Recherche avec memorisation du predecesseur
    prec := L
    p := L^.next
    Tant que (p <> NIL) ET (p^.val <> x) faire
        prec := p
        p := p^.next
    Fait

    // Suppression si trouve
    Si p <> NIL Alors
        prec^.next := p^.next
        Liberer(p)
    Fsi
Fin
```

---

## 6. Listes chainees avec First/Last (F/L)

### Structure amelioree

```
Liste_FL = enreg
    first : ^noeud    // Premier element
    last  : ^noeud    // Dernier element
Fenreg
```

### Avantage principal

Insertion en queue en **O(1)** (plus besoin de parcourir toute la liste).

### Insertion en queue optimisee

```
Procedure Insere_queue_FL(var L : Liste_FL, x : valeur)
Var p : ^noeud
Debut
    Creer(p)
    p^.val := x
    p^.next := NIL

    Si L.first = NIL Alors
        L.first := p
        L.last := p
    Sinon
        L.last^.next := p
        L.last := p
    Fsi
Fin
```

---

## 7. Listes doublement chainees

### Structure

```
noeud_double = enreg
    val  : <type_valeur>
    pred : ^noeud_double    // Predecesseur
    next : ^noeud_double    // Successeur
Fenreg
```

### Representation visuelle

```
NIL <-- [pred|val1|next] <--> [pred|val2|next] <--> [pred|val3|next] --> NIL
```

### Avantages et inconvenients

| Avantages | Inconvenients |
|-----------|---------------|
| Parcours bidirectionnel | Memoire double pour les pointeurs |
| Suppression en O(1) avec le noeud | Complexite de maintenance |
| Insertion avant/apres facile | Plus de pointeurs a gerer |

### Suppression d'un noeud (avec acces direct)

```
Procedure Supprime_noeud(var L : ^noeud_double, p : ^noeud_double)
Debut
    Si p^.pred <> NIL Alors
        p^.pred^.next := p^.next
    Sinon
        L := p^.next    // Suppression en tete
    Fsi

    Si p^.next <> NIL Alors
        p^.next^.pred := p^.pred
    Fsi

    Liberer(p)
Fin
```

---

## 8. Comparaison des structures

| Operation | Tableau | Liste simple | Liste F/L | Liste double |
|-----------|---------|--------------|-----------|--------------|
| Acces par indice | O(1) | O(N) | O(N) | O(N) |
| Recherche | O(N) ou O(log N) | O(N) | O(N) | O(N) |
| Insertion tete | O(N) | O(1) | O(1) | O(1) |
| Insertion queue | O(1) | O(N) | O(1) | O(1) |
| Suppression | O(N) | O(N) | O(N) | O(1)* |

*O(1) si on a deja le pointeur vers le noeud a supprimer.

---

## 9. Application : Gestion des depots

### Contexte

Une entreprise possede 10 depots, chacun stockant un nombre variable d'articles.

### Structures

```
article = enreg
    code : entier
    qte  : entier
Fenreg

noeud_article = enreg
    art  : article
    next : ^noeud_article
Fenreg

// Tableau de 10 listes (une par depot)
Entreprise : tableau[1..10] de ^noeud_article
```

### Initialisation

```
Pour i de 1 a 10 faire
    Entreprise[i] := NIL
Fpour
```

### Lecture des articles par depot

```
Pour i de 1 a 10 faire
    Lire(code)
    Tant que code <> 0 faire
        Lire(qte)
        Insere_article(Entreprise[i], code, qte)
        Lire(code)
    Fait
Fpour
```

---

## 10. Correspondance avec COBOL

En COBOL, la gestion dynamique de memoire n'est pas native comme dans les langages modernes. Cependant, des concepts similaires existent :

| Algorithmique | COBOL |
|---------------|-------|
| Pointeur | `USAGE POINTER` ou adresses avec `ADDRESS OF` |
| Allocation | `ALLOCATE` (COBOL 2002+) |
| Liberation | `FREE` (COBOL 2002+) |
| Liste chainee | Fichiers indexes ou simulation avec tableaux |

### Alternative COBOL classique

Les listes sont souvent simulees par :
- Fichiers sequentiels indexes (VSAM KSDS)
- Tableaux avec gestion d'indices "next"

---

## 11. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-02-02-pointeurs-listes.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-02/)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre II - Structures de Données](02-01-structures-donnees.md) | [Chapitre II (Partie 3) - Piles et Files](02-03-piles-files.md) |

---
*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
