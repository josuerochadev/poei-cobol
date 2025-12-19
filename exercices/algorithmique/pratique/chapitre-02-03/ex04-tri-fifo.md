# Exercice 04 : Tri par fusion avec file (FIFO)

## Enonce

On souhaite trier un ensemble de nombres en utilisant une **file (FIFO)** et l'algorithme de fusion. Le principe est le suivant :

1. Chaque nombre de l'ensemble devient une liste a un seul element
2. Toutes ces listes sont mises dans une file (FIFO)
3. On retire deux listes de la file, on les fusionne en ordre croissant
4. La liste fusionnee est remise en fin de file
5. On repete jusqu'a ce qu'il ne reste qu'une seule liste triee

## Schema visuel

```text
Ensemble initial : {5, 2, 8, 1, 9, 3}

Etape 1 : Creer des listes a 1 element
   [5] [2] [8] [1] [9] [3]

Etape 2 : Mettre dans la file FIFO
   File: [5] <- [2] <- [8] <- [1] <- [9] <- [3]
         tete                              queue

Etape 3 : Iterations de fusion

Iteration 1:
   Defiler [5] et [2]
   Fusionner: [2,5]
   Enfiler [2,5]
   File: [8] <- [1] <- [9] <- [3] <- [2,5]

Iteration 2:
   Defiler [8] et [1]
   Fusionner: [1,8]
   Enfiler [1,8]
   File: [9] <- [3] <- [2,5] <- [1,8]

Iteration 3:
   Defiler [9] et [3]
   Fusionner: [3,9]
   Enfiler [3,9]
   File: [2,5] <- [1,8] <- [3,9]

Iteration 4:
   Defiler [2,5] et [1,8]
   Fusionner: [1,2,5,8]
   Enfiler [1,2,5,8]
   File: [3,9] <- [1,2,5,8]

Iteration 5:
   Defiler [3,9] et [1,2,5,8]
   Fusionner: [1,2,3,5,8,9]
   Enfiler [1,2,3,5,8,9]
   File: [1,2,3,5,8,9]

Resultat : Une seule liste triee [1,2,3,5,8,9]
```

---

## Structures de donnees

### Type Liste chainee

```text
Type Elmt = enregistrement
    Val : entier
    Next : ^Elmt
fin enregistrement

Type Liste = ^Elmt
```

### Type File de listes

```text
Type ElmtFile = enregistrement
    L : Liste          // La liste stockee
    Next : ^ElmtFile   // Pointeur vers suivant dans la file
fin enregistrement

Type File = enregistrement
    Tete : ^ElmtFile
    Queue : ^ElmtFile
fin enregistrement
```

---

## Operations sur la file

```text
PROCEDURE Init_file (var F : File)
Debut
    F.Tete := NIL
    F.Queue := NIL
Fin

FONCTION Vide (F : File) : Booleen
Debut
    Retourner F.Tete = NIL
Fin

PROCEDURE Enfiler (var F : File, L : Liste)
VAR nouveau : ^ElmtFile
Debut
    Creer(nouveau)
    nouveau^.L := L
    nouveau^.Next := NIL

    Si F.Queue = NIL Alors
        F.Tete := nouveau
    Sinon
        F.Queue^.Next := nouveau
    Fsi

    F.Queue := nouveau
Fin

FONCTION Defiler (var F : File) : Liste
VAR temp : ^ElmtFile
    L : Liste
Debut
    temp := F.Tete
    L := temp^.L
    F.Tete := F.Tete^.Next

    Si F.Tete = NIL Alors
        F.Queue := NIL
    Fsi

    Liberer(temp)
    Retourner L
Fin

FONCTION Taille_File (F : File) : entier
VAR courant : ^ElmtFile
    compteur : entier
Debut
    compteur := 0
    courant := F.Tete

    Tantque courant <> NIL faire
        compteur := compteur + 1
        courant := courant^.Next
    Ftantque

    Retourner compteur
Fin
```

---

## Fonction de fusion de deux listes triees

<details>
<summary>Solution</summary>

```text
FONCTION Fusionner (L1, L2 : Liste) : Liste
VAR resultat, dernier, nouveau : ^Elmt
    p1, p2 : ^Elmt
Debut
    resultat := NIL
    dernier := NIL
    p1 := L1
    p2 := L2

    // Fusionner tant que les deux listes ont des elements
    Tantque (p1 <> NIL) ET (p2 <> NIL) faire
        Creer(nouveau)

        Si p1^.Val <= p2^.Val Alors
            nouveau^.Val := p1^.Val
            p1 := p1^.Next
        Sinon
            nouveau^.Val := p2^.Val
            p2 := p2^.Next
        Fsi

        nouveau^.Next := NIL

        Si resultat = NIL Alors
            resultat := nouveau
        Sinon
            dernier^.Next := nouveau
        Fsi

        dernier := nouveau
    Ftantque

    // Ajouter les elements restants de L1
    Tantque p1 <> NIL faire
        Creer(nouveau)
        nouveau^.Val := p1^.Val
        nouveau^.Next := NIL
        dernier^.Next := nouveau
        dernier := nouveau
        p1 := p1^.Next
    Ftantque

    // Ajouter les elements restants de L2
    Tantque p2 <> NIL faire
        Creer(nouveau)
        nouveau^.Val := p2^.Val
        nouveau^.Next := NIL
        dernier^.Next := nouveau
        dernier := nouveau
        p2 := p2^.Next
    Ftantque

    Retourner resultat
Fin
```

</details>

---

## Algorithme principal du tri FIFO

<details>
<summary>Solution</summary>

```text
FONCTION Tri_FIFO (T : Tableau[1..n] de entier) : Liste
VAR
    F : File
    L, L1, L2 : Liste
    nouveau : ^Elmt
    i : entier
Debut
    Init_file(F)

    // Etape 1 : Creer une liste a 1 element pour chaque nombre
    Pour i de 1 a n faire
        Creer(nouveau)
        nouveau^.Val := T[i]
        nouveau^.Next := NIL
        Enfiler(F, nouveau)
    Fpour

    // Etape 2 : Fusionner jusqu'a obtenir une seule liste
    Tantque Taille_File(F) > 1 faire
        L1 := Defiler(F)
        L2 := Defiler(F)
        L := Fusionner(L1, L2)
        Enfiler(F, L)
    Ftantque

    // Retourner la liste triee finale
    Si NON Vide(F) Alors
        Retourner Defiler(F)
    Sinon
        Retourner NIL
    Fsi
Fin
```

</details>

---

## Trace d'execution detaillee

```text
Tableau : T = [5, 2, 8, 1]

=== Phase 1 : Creation des listes ===
i=1: Creer liste [5], Enfiler
i=2: Creer liste [2], Enfiler
i=3: Creer liste [8], Enfiler
i=4: Creer liste [1], Enfiler

File initiale: [5] <- [2] <- [8] <- [1]
Taille = 4

=== Phase 2 : Fusions ===

--- Iteration 1 ---
Taille = 4 > 1, continuer
L1 = Defiler() = [5]
L2 = Defiler() = [2]
Fusionner([5], [2]):
  5 > 2 : prendre 2, resultat = [2]
  reste [5], resultat = [2, 5]
Enfiler([2, 5])
File: [8] <- [1] <- [2,5]
Taille = 3

--- Iteration 2 ---
Taille = 3 > 1, continuer
L1 = Defiler() = [8]
L2 = Defiler() = [1]
Fusionner([8], [1]):
  8 > 1 : prendre 1, resultat = [1]
  reste [8], resultat = [1, 8]
Enfiler([1, 8])
File: [2,5] <- [1,8]
Taille = 2

--- Iteration 3 ---
Taille = 2 > 1, continuer
L1 = Defiler() = [2, 5]
L2 = Defiler() = [1, 8]
Fusionner([2,5], [1,8]):
  2 > 1 : prendre 1, resultat = [1]
  2 < 8 : prendre 2, resultat = [1, 2]
  5 < 8 : prendre 5, resultat = [1, 2, 5]
  reste [8], resultat = [1, 2, 5, 8]
Enfiler([1, 2, 5, 8])
File: [1,2,5,8]
Taille = 1

=== Phase 3 : Resultat ===
Taille = 1 : Terminer
Retourner [1, 2, 5, 8]

Resultat final : 1 -> 2 -> 5 -> 8 -> NIL
```

---

## Variante : Avec comptage des listes

```text
FONCTION Tri_FIFO_Compteur (T : Tableau[1..n] de entier) : Liste
VAR
    F : File
    L, L1, L2 : Liste
    nouveau : ^Elmt
    i, nbListes : entier
Debut
    Init_file(F)
    nbListes := 0

    // Creer les listes initiales
    Pour i de 1 a n faire
        Creer(nouveau)
        nouveau^.Val := T[i]
        nouveau^.Next := NIL
        Enfiler(F, nouveau)
        nbListes := nbListes + 1
    Fpour

    // Fusionner tant qu'il y a plus d'une liste
    Tantque nbListes > 1 faire
        L1 := Defiler(F)
        L2 := Defiler(F)
        L := Fusionner(L1, L2)
        Enfiler(F, L)
        nbListes := nbListes - 1    // 2 listes retirees, 1 ajoutee
    Ftantque

    Retourner Defiler(F)
Fin
```

---

## Pourquoi une file (FIFO) ?

```text
L'utilisation d'une file garantit un tri equilibre :

Avec FIFO :
  - Les listes sont fusionnees dans l'ordre d'arrivee
  - Les fusions se font par "niveaux" (comme un arbre)
  - Complexite : O(n log n)

Avec LIFO (pile) :
  - Les listes seraient fusionnees dans l'ordre inverse
  - Risque de desequilibre (fusionner une grande liste avec une petite)
  - Moins efficace en moyenne

Exemple avec 8 elements et FIFO :
Niveau 0: [1] [2] [3] [4] [5] [6] [7] [8]  (8 listes)
Niveau 1: [1,2] [3,4] [5,6] [7,8]           (4 listes)
Niveau 2: [1,2,3,4] [5,6,7,8]               (2 listes)
Niveau 3: [1,2,3,4,5,6,7,8]                 (1 liste)

=> log2(8) = 3 niveaux de fusion
```

---

## Exercices supplementaires

### Variante A : Tri decroissant

```text
// Modifier la condition de fusion
Si p1^.Val >= p2^.Val Alors   // >= au lieu de <=
```

### Variante B : Afficher l'etat de la file

```text
PROCEDURE Afficher_File (F : File)
VAR courant : ^ElmtFile
    p : ^Elmt
Debut
    courant := F.Tete
    Ecrire("[")

    Tantque courant <> NIL faire
        Ecrire("[")
        p := courant^.L
        Tantque p <> NIL faire
            Ecrire(p^.Val)
            p := p^.Next
            Si p <> NIL Alors
                Ecrire(",")
            Fsi
        Ftantque
        Ecrire("]")

        courant := courant^.Next
        Si courant <> NIL Alors
            Ecrire(" <- ")
        Fsi
    Ftantque

    Ecrire("]")
Fin
```

### Variante C : Compter les comparaisons

```text
FONCTION Fusionner_Compte (L1, L2 : Liste, var nbComp : entier) : Liste
// Meme algorithme mais incremente nbComp a chaque comparaison
// Permet d'analyser la complexite reelle
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
* Structure pour la file de listes
01 WS-FILE.
   05 WS-FILE-TAB OCCURS 100 TIMES.
      10 WS-LISTE-TAB      PIC S9(5) OCCURS 100 TIMES.
      10 WS-LISTE-TAILLE   PIC 9(3).
   05 WS-FILE-TETE         PIC 9(3) VALUE 1.
   05 WS-FILE-QUEUE        PIC 9(3) VALUE 0.
   05 WS-FILE-TAILLE       PIC 9(3) VALUE 0.

01 WS-TRAVAIL.
   05 WS-L1-TAB            PIC S9(5) OCCURS 100 TIMES.
   05 WS-L1-TAILLE         PIC 9(3).
   05 WS-L2-TAB            PIC S9(5) OCCURS 100 TIMES.
   05 WS-L2-TAILLE         PIC 9(3).
   05 WS-RESULTAT-TAB      PIC S9(5) OCCURS 100 TIMES.
   05 WS-RESULTAT-TAILLE   PIC 9(3).

PROCEDURE DIVISION.
* Enfiler une liste
ENFILER-PARA.
    ADD 1 TO WS-FILE-QUEUE
    ADD 1 TO WS-FILE-TAILLE
    MOVE WS-LISTE-COURANTE TO WS-FILE-TAB(WS-FILE-QUEUE).

* Defiler une liste
DEFILER-PARA.
    MOVE WS-FILE-TAB(WS-FILE-TETE) TO WS-LISTE-COURANTE
    ADD 1 TO WS-FILE-TETE
    SUBTRACT 1 FROM WS-FILE-TAILLE.

* Fusion de deux listes triees
FUSIONNER-PARA.
    INITIALIZE WS-RESULTAT-TAB
    MOVE 0 TO WS-RESULTAT-TAILLE
    MOVE 1 TO WS-IDX1 WS-IDX2

    PERFORM UNTIL WS-IDX1 > WS-L1-TAILLE
              AND WS-IDX2 > WS-L2-TAILLE
        ADD 1 TO WS-RESULTAT-TAILLE
        EVALUATE TRUE
            WHEN WS-IDX1 > WS-L1-TAILLE
                MOVE WS-L2-TAB(WS-IDX2)
                    TO WS-RESULTAT-TAB(WS-RESULTAT-TAILLE)
                ADD 1 TO WS-IDX2
            WHEN WS-IDX2 > WS-L2-TAILLE
                MOVE WS-L1-TAB(WS-IDX1)
                    TO WS-RESULTAT-TAB(WS-RESULTAT-TAILLE)
                ADD 1 TO WS-IDX1
            WHEN WS-L1-TAB(WS-IDX1) <= WS-L2-TAB(WS-IDX2)
                MOVE WS-L1-TAB(WS-IDX1)
                    TO WS-RESULTAT-TAB(WS-RESULTAT-TAILLE)
                ADD 1 TO WS-IDX1
            WHEN OTHER
                MOVE WS-L2-TAB(WS-IDX2)
                    TO WS-RESULTAT-TAB(WS-RESULTAT-TAILLE)
                ADD 1 TO WS-IDX2
        END-EVALUATE
    END-PERFORM.
```

---

## Complexite

| Operation | Complexite |
|-----------|------------|
| Creation listes initiales | O(n) |
| Nombre de fusions | O(n - 1) |
| Cout d'une fusion | O(n) au total par niveau |
| Nombre de niveaux | O(log n) |
| **Complexite totale** | **O(n log n)** |

**Espace** : O(n) pour stocker les listes et la file

---

## Comparaison avec d'autres tris

| Algorithme | Temps moyen | Temps pire cas | Espace | Stable |
|------------|-------------|----------------|--------|--------|
| Tri FIFO (fusion) | O(n log n) | O(n log n) | O(n) | Oui |
| Tri bulle | O(n^2) | O(n^2) | O(1) | Oui |
| Tri rapide | O(n log n) | O(n^2) | O(log n) | Non |
| Tri par tas | O(n log n) | O(n log n) | O(1) | Non |

Le tri FIFO est une variante du tri fusion (merge sort) qui utilise explicitement une file pour gerer les sous-listes a fusionner.

---

*Exercice 04 - Piles et Files - Algorithmique Chapitre 02-03 - M2i*
