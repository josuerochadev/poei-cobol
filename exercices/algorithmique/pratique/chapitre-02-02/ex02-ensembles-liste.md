# Exercice 18 : Operations sur les ensembles (Listes chainees)

## Enonce

Implementer les operations ensemblistes classiques en utilisant des listes chainees :
- **Union** : Elements presents dans E1 OU E2
- **Intersection** : Elements presents dans E1 ET E2
- **Inclusion** : E1 est-il inclus dans E2 ?

Ces operations seront implementees pour deux types d'ensembles :
1. **Ensembles simples** : Chaque element apparait une seule fois
2. **Multi-ensembles** : Chaque element peut apparaitre plusieurs fois (avec compteur)

---

## Partie A : Ensembles simples

### Structure de donnees

```text
Type Elmt = enregistrement
    Val : entier
    Next : ^Elmt
fin enregistrement

Type simple_Ens = ^Elmt
```

### Schema visuel

```text
E1 --> | 1 | --> | 3 | --> | 5 | --> NIL

E2 --> | 2 | --> | 3 | --> | 4 | --> NIL

Union(E1, E2) --> | 1 | --> | 3 | --> | 5 | --> | 2 | --> | 4 | --> NIL

Intersection(E1, E2) --> | 3 | --> NIL
```

---

### Fonction auxiliaire : Appartient

```text
FONCTION Appartient (x: entier, E: simple_Ens) : booleen
VAR courant : ^Elmt
Debut
    courant := E

    Tantque courant <> NIL faire
        Si courant^.Val = x Alors
            Retourner vrai
        Fsi
        courant := courant^.Next
    Ftantque

    Retourner faux
Fin
```

### Procedure auxiliaire : Insert_tete

```text
PROCEDURE Insert_tete (x: entier, var E: simple_Ens)
VAR nouveau : ^Elmt
Debut
    Creer(nouveau)
    nouveau^.Val := x
    nouveau^.Next := E
    E := nouveau
Fin
```

---

### A.1 : Union simple

<details>
<summary>Solution</summary>

```text
PROCEDURE Union_simple (E1, E2: simple_Ens, var E3: simple_Ens)
VAR courant : ^Elmt
Debut
    E3 := NIL

    // Ajouter tous les elements de E1
    courant := E1
    Tantque courant <> NIL faire
        Insert_tete(courant^.Val, E3)
        courant := courant^.Next
    Ftantque

    // Ajouter les elements de E2 qui ne sont pas dans E1
    courant := E2
    Tantque courant <> NIL faire
        Si non Appartient(courant^.Val, E1) Alors
            Insert_tete(courant^.Val, E3)
        Fsi
        courant := courant^.Next
    Ftantque
Fin
```

</details>

### A.2 : Intersection simple

<details>
<summary>Solution</summary>

```text
PROCEDURE Intersection_simple (E1, E2: simple_Ens, var E3: simple_Ens)
VAR courant : ^Elmt
Debut
    E3 := NIL

    courant := E1
    Tantque courant <> NIL faire
        Si Appartient(courant^.Val, E2) Alors
            Insert_tete(courant^.Val, E3)
        Fsi
        courant := courant^.Next
    Ftantque
Fin
```

</details>

### A.3 : Inclusion simple

<details>
<summary>Solution</summary>

```text
FONCTION Inclusion_simple (E1, E2: simple_Ens) : booleen
// Retourne VRAI si E1 est inclus dans E2
VAR
    courant : ^Elmt
    inclus : booleen
Debut
    inclus := vrai
    courant := E1

    Tantque inclus ET (courant <> NIL) faire
        Si non Appartient(courant^.Val, E2) Alors
            inclus := faux
        Fsi
        courant := courant^.Next
    Ftantque

    Retourner inclus
Fin
```

</details>

---

## Partie B : Multi-ensembles

### Structure de donnees

```text
Type Elmt = enregistrement
    Val : entier
    Nbr : entier       // Nombre d'occurrences
    Next : ^Elmt
fin enregistrement

Type multi_Ens = ^Elmt
```

### Schema visuel

```text
// Multi-ensemble {a, a, a, b, b, c}
E --> | a,3 | --> | b,2 | --> | c,1 | --> NIL
         |           |           |
       Val,Nbr    Val,Nbr     Val,Nbr
```

---

### Fonction auxiliaire : NbOccur

```text
FONCTION NbOccur (x: entier, E: multi_Ens) : entier
// Retourne le nombre d'occurrences de x dans E
VAR courant : ^Elmt
Debut
    courant := E

    Tantque courant <> NIL faire
        Si courant^.Val = x Alors
            Retourner courant^.Nbr
        Fsi
        courant := courant^.Next
    Ftantque

    Retourner 0
Fin
```

### Procedure auxiliaire : Insert_multi

```text
PROCEDURE Insert_multi (x: entier, n: entier, var E: multi_Ens)
// Insere x avec n occurrences (ou incremente si existe)
VAR
    courant : ^Elmt
    nouveau : ^Elmt
    trouve : booleen
Debut
    courant := E
    trouve := faux

    // Chercher si x existe deja
    Tantque (courant <> NIL) ET (non trouve) faire
        Si courant^.Val = x Alors
            courant^.Nbr := courant^.Nbr + n
            trouve := vrai
        Fsi
        courant := courant^.Next
    Ftantque

    // Si pas trouve, creer nouveau maillon
    Si non trouve Alors
        Creer(nouveau)
        nouveau^.Val := x
        nouveau^.Nbr := n
        nouveau^.Next := E
        E := nouveau
    Fsi
Fin
```

---

### B.1 : Union multi-ensemble

Pour l'union, on prend le maximum des occurrences.

<details>
<summary>Solution</summary>

```text
PROCEDURE Union_multi (E1, E2: multi_Ens, var E3: multi_Ens)
VAR
    courant : ^Elmt
    n1, n2, max_n : entier
Debut
    E3 := NIL

    // Parcourir E1
    courant := E1
    Tantque courant <> NIL faire
        n1 := courant^.Nbr
        n2 := NbOccur(courant^.Val, E2)

        // Prendre le maximum
        Si n1 >= n2 Alors
            max_n := n1
        Sinon
            max_n := n2
        Fsi

        Insert_multi(courant^.Val, max_n, E3)
        courant := courant^.Next
    Ftantque

    // Parcourir E2 pour les elements absents de E1
    courant := E2
    Tantque courant <> NIL faire
        Si NbOccur(courant^.Val, E1) = 0 Alors
            Insert_multi(courant^.Val, courant^.Nbr, E3)
        Fsi
        courant := courant^.Next
    Ftantque
Fin
```

</details>

### B.2 : Intersection multi-ensemble

Pour l'intersection, on prend le minimum des occurrences (si > 0).

<details>
<summary>Solution</summary>

```text
PROCEDURE Intersection_multi (E1, E2: multi_Ens, var E3: multi_Ens)
VAR
    courant : ^Elmt
    n1, n2, min_n : entier
Debut
    E3 := NIL

    courant := E1
    Tantque courant <> NIL faire
        n1 := courant^.Nbr
        n2 := NbOccur(courant^.Val, E2)

        Si n2 > 0 Alors
            // Prendre le minimum
            Si n1 <= n2 Alors
                min_n := n1
            Sinon
                min_n := n2
            Fsi

            Insert_multi(courant^.Val, min_n, E3)
        Fsi
        courant := courant^.Next
    Ftantque
Fin
```

</details>

### B.3 : Inclusion multi-ensemble

E1 inclus dans E2 si pour chaque element, NbOccur(x, E1) <= NbOccur(x, E2).

<details>
<summary>Solution</summary>

```text
FONCTION Inclusion_multi (E1, E2: multi_Ens) : booleen
VAR
    courant : ^Elmt
    inclus : booleen
Debut
    inclus := vrai
    courant := E1

    Tantque inclus ET (courant <> NIL) faire
        Si courant^.Nbr > NbOccur(courant^.Val, E2) Alors
            inclus := faux
        Fsi
        courant := courant^.Next
    Ftantque

    Retourner inclus
Fin
```

</details>

---

## Exemples d'execution

### Exemple 1 : Ensembles simples

```text
E1 = {1, 2, 3}
E2 = {2, 3, 4}

Union(E1, E2) = {1, 2, 3, 4}
Intersection(E1, E2) = {2, 3}
Inclusion(E1, E2) = FAUX (1 n'est pas dans E2)
Inclusion({2, 3}, E2) = VRAI
```

### Exemple 2 : Multi-ensembles

```text
E1 = {a:2, b:3}        // 2 fois 'a', 3 fois 'b'
E2 = {a:4, c:1}        // 4 fois 'a', 1 fois 'c'

Union(E1, E2) = {a:4, b:3, c:1}     // max(2,4)=4, max(3,0)=3, max(0,1)=1
Intersection(E1, E2) = {a:2}        // min(2,4)=2, b absent de E2
Inclusion(E1, E2) = FAUX            // b:3 > 0 dans E2
Inclusion({a:2}, E2) = VRAI         // 2 <= 4
```

---

## Trace d'execution : Intersection_simple

```text
E1 = 1 -> 3 -> 5 -> NIL
E2 = 2 -> 3 -> 5 -> NIL

E3 := NIL

Iteration 1: courant^.Val = 1
  Appartient(1, E2) = faux
  (on n'ajoute pas)

Iteration 2: courant^.Val = 3
  Appartient(3, E2) = vrai
  Insert_tete(3, E3)
  E3 = 3 -> NIL

Iteration 3: courant^.Val = 5
  Appartient(5, E2) = vrai
  Insert_tete(5, E3)
  E3 = 5 -> 3 -> NIL

Resultat : E3 = {3, 5}
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
* Simulation avec tableau
01 WS-ENSEMBLE.
   05 WS-ELEMENTS OCCURS 100 TIMES.
      10 WS-VALEUR      PIC 9(5).
      10 WS-OCCURRENCES PIC 9(3).
      10 WS-SUIVANT     PIC 9(3).
01 WS-NB-ELEMENTS       PIC 9(3).
01 WS-TETE              PIC 9(3).

PROCEDURE DIVISION.
* Verification d'appartenance
APPARTIENT-PARA.
    MOVE WS-TETE TO WS-INDEX
    MOVE 'N' TO WS-TROUVE
    PERFORM UNTIL WS-INDEX = 0 OR WS-TROUVE = 'O'
        IF WS-VALEUR(WS-INDEX) = WS-RECHERCHE
            MOVE 'O' TO WS-TROUVE
        ELSE
            MOVE WS-SUIVANT(WS-INDEX) TO WS-INDEX
        END-IF
    END-PERFORM.
```

---

## Complexite

| Operation | Ensemble simple | Multi-ensemble |
|-----------|-----------------|----------------|
| Appartient | O(n) | O(n) |
| NbOccur | - | O(n) |
| Union | O(n * m) | O(n * m) |
| Intersection | O(n * m) | O(n * m) |
| Inclusion | O(n * m) | O(n * m) |

Ou n = taille(E1) et m = taille(E2)

---

*Exercice 18 - Listes chainees - Algorithmique Chapitre 02 - M2i*
