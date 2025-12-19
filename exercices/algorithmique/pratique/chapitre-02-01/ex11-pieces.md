# Exercice 11 : Gestion de pieces de monnaie

## Enonce

On souhaite gerer une collection de pieces de monnaie. Chaque piece est definie par :
- Un pays d'origine (chaine de 20 caracteres)
- Une annee de frappe (entier)
- Une valeur faciale (reel)
- Un metal ('O' = Or, 'A' = Argent, 'B' = Bronze, 'C' = Cuivre)
- Un etat de conservation ('TB' = Tres Bien, 'B' = Bien, 'M' = Moyen, 'U' = Use)
- Une valeur estimee (reel)

### Partie A : Definitions

1. Definir le type `Piece` pour representer une piece de monnaie
2. Definir un tableau `Collection` pouvant contenir jusqu'a 200 pieces

### Partie B : Operations CRUD

1. **AjouterPiece** : Ajoute une nouvelle piece a la collection
2. **AfficherPiece** : Affiche les details d'une piece
3. **AfficherCollection** : Affiche toute la collection
4. **SupprimerPiece** : Supprime une piece par son indice

### Partie C : Recherches

1. **RechercherParPays** : Affiche toutes les pieces d'un pays donne
2. **RechercherParMetal** : Affiche toutes les pieces d'un metal donne
3. **RechercherParPeriode** : Affiche les pieces frappees entre deux annees

### Partie D : Statistiques

1. **ValeurTotale** : Calcule la valeur totale estimee de la collection
2. **PieceLaPlusAncienne** : Trouve la piece la plus ancienne
3. **PieceLaPlusChere** : Trouve la piece avec la plus haute valeur estimee
4. **StatistiquesParMetal** : Affiche le nombre et la valeur par type de metal

## Structure de donnees

```text
Type Piece = enregistrement
    pays : chaine[20]
    annee : entier
    valeurFaciale : reel
    metal : caractere         // 'O', 'A', 'B', 'C'
    etat : chaine[2]          // 'TB', 'B', 'M', 'U'
    valeurEstimee : reel
fin enregistrement

Type Collection = tableau[1..200] de Piece
```

## Exemple d'utilisation

```text
--- MA COLLECTION DE PIECES ---
1. Ajouter une piece
2. Afficher la collection
3. Rechercher par pays
4. Rechercher par metal
5. Rechercher par periode
6. Statistiques
7. Supprimer une piece
0. Quitter

Choix : 1

Pays : FRANCE
Annee de frappe : 1912
Valeur faciale : 20.00
Metal (O/A/B/C) : O
Etat (TB/B/M/U) : TB
Valeur estimee : 450.00

Piece ajoutee !

Choix : 6

=== STATISTIQUES ===
Nombre total de pieces : 15
Valeur totale estimee : 2350.00 EUR

Piece la plus ancienne : GRECE 1850
Piece la plus chere : FRANCE 1912 - 450.00 EUR

Par metal :
- Or     : 3 pieces - 980.00 EUR
- Argent : 5 pieces - 650.00 EUR
- Bronze : 4 pieces - 420.00 EUR
- Cuivre : 3 pieces - 300.00 EUR
```

## Concepts a utiliser

- Enregistrements simples
- Tableaux d'enregistrements
- Recherche multicritere
- Calculs d'agregation (somme, min, max)
- Statistiques groupees

---

<details>
<summary>Solution - Partie A : Definitions</summary>

```text
// Definition des types
Type Piece = enregistrement
    pays : chaine[20]
    annee : entier
    valeurFaciale : reel
    metal : caractere
    etat : chaine[2]
    valeurEstimee : reel
fin enregistrement

Type Collection = tableau[1..200] de Piece

// Variables globales
Var maCollection : Collection
    nbPieces : entier
```

</details>

<details>
<summary>Solution - Partie B : Operations CRUD</summary>

```text
// Fonction pour obtenir le libelle du metal
Fonction LibelleMetal(m : caractere) : chaine
Debut
    Selon m Faire
        cas 'O': Retourner "Or"
        cas 'A': Retourner "Argent"
        cas 'B': Retourner "Bronze"
        cas 'C': Retourner "Cuivre"
        defaut: Retourner "Inconnu"
    finSelon
Fin

// Fonction pour obtenir le libelle de l'etat
Fonction LibelleEtat(e : chaine) : chaine
Debut
    Si e = "TB" Alors
        Retourner "Tres Bien"
    SinonSi e = "B" Alors
        Retourner "Bien"
    SinonSi e = "M" Alors
        Retourner "Moyen"
    SinonSi e = "U" Alors
        Retourner "Use"
    Sinon
        Retourner "Inconnu"
    finSi
Fin

// Ajouter une piece
Procedure AjouterPiece(var C : Collection, var n : entier)
Var p : Piece
Debut
    Si n >= 200 Alors
        Ecrire("Collection pleine !")
        Retourner
    finSi

    Ecrire("Pays : ")
    Lire(p.pays)

    Ecrire("Annee de frappe : ")
    Lire(p.annee)

    Ecrire("Valeur faciale : ")
    Lire(p.valeurFaciale)

    Ecrire("Metal (O=Or, A=Argent, B=Bronze, C=Cuivre) : ")
    Lire(p.metal)

    Ecrire("Etat (TB/B/M/U) : ")
    Lire(p.etat)

    Ecrire("Valeur estimee : ")
    Lire(p.valeurEstimee)

    n := n + 1
    C[n] := p
    Ecrire("Piece ajoutee !")
Fin

// Afficher une piece
Procedure AfficherPiece(p : Piece, indice : entier)
Debut
    Ecrire("--- Piece #", indice, " ---")
    Ecrire("Pays   : ", p.pays)
    Ecrire("Annee  : ", p.annee)
    Ecrire("Valeur faciale : ", p.valeurFaciale)
    Ecrire("Metal  : ", LibelleMetal(p.metal))
    Ecrire("Etat   : ", LibelleEtat(p.etat))
    Ecrire("Valeur estimee : ", p.valeurEstimee, " EUR")
    Ecrire("")
Fin

// Afficher toute la collection
Procedure AfficherCollection(C : Collection, n : entier)
Var i : entier
Debut
    Si n = 0 Alors
        Ecrire("La collection est vide.")
        Retourner
    finSi

    Ecrire("=== MA COLLECTION (", n, " pieces) ===")
    Pour i de 1 a n Faire
        AfficherPiece(C[i], i)
    finPour
Fin

// Supprimer une piece
Procedure SupprimerPiece(var C : Collection, var n : entier)
Var indice, i : entier
Debut
    Si n = 0 Alors
        Ecrire("Collection vide !")
        Retourner
    finSi

    Ecrire("Indice de la piece a supprimer (1-", n, ") : ")
    Lire(indice)

    Si indice < 1 OU indice > n Alors
        Ecrire("Indice invalide !")
        Retourner
    finSi

    // Afficher la piece a supprimer
    Ecrire("Piece a supprimer :")
    AfficherPiece(C[indice], indice)

    // Decaler les elements
    Pour i de indice a n-1 Faire
        C[i] := C[i+1]
    finPour

    n := n - 1
    Ecrire("Piece supprimee.")
Fin
```

</details>

<details>
<summary>Solution - Partie C : Recherches</summary>

```text
// Rechercher par pays
Procedure RechercherParPays(C : Collection, n : entier)
Var i, compteur : entier
    paysRecherche : chaine[20]
Debut
    Ecrire("Pays a rechercher : ")
    Lire(paysRecherche)

    Ecrire("=== PIECES DE ", paysRecherche, " ===")
    compteur := 0
    Pour i de 1 a n Faire
        Si C[i].pays = paysRecherche Alors
            AfficherPiece(C[i], i)
            compteur := compteur + 1
        finSi
    finPour

    Si compteur = 0 Alors
        Ecrire("Aucune piece trouvee pour ce pays.")
    Sinon
        Ecrire("Total : ", compteur, " piece(s)")
    finSi
Fin

// Rechercher par metal
Procedure RechercherParMetal(C : Collection, n : entier)
Var i, compteur : entier
    metalRecherche : caractere
Debut
    Ecrire("Metal (O/A/B/C) : ")
    Lire(metalRecherche)

    Ecrire("=== PIECES EN ", LibelleMetal(metalRecherche), " ===")
    compteur := 0
    Pour i de 1 a n Faire
        Si C[i].metal = metalRecherche Alors
            AfficherPiece(C[i], i)
            compteur := compteur + 1
        finSi
    finPour

    Si compteur = 0 Alors
        Ecrire("Aucune piece trouvee pour ce metal.")
    Sinon
        Ecrire("Total : ", compteur, " piece(s)")
    finSi
Fin

// Rechercher par periode
Procedure RechercherParPeriode(C : Collection, n : entier)
Var i, compteur : entier
    anneeDebut, anneeFin : entier
Debut
    Ecrire("Annee de debut : ")
    Lire(anneeDebut)
    Ecrire("Annee de fin : ")
    Lire(anneeFin)

    Ecrire("=== PIECES DE ", anneeDebut, " A ", anneeFin, " ===")
    compteur := 0
    Pour i de 1 a n Faire
        Si (C[i].annee >= anneeDebut) ET (C[i].annee <= anneeFin) Alors
            AfficherPiece(C[i], i)
            compteur := compteur + 1
        finSi
    finPour

    Si compteur = 0 Alors
        Ecrire("Aucune piece trouvee pour cette periode.")
    Sinon
        Ecrire("Total : ", compteur, " piece(s)")
    finSi
Fin
```

</details>

<details>
<summary>Solution - Partie D : Statistiques</summary>

```text
// Valeur totale de la collection
Fonction ValeurTotale(C : Collection, n : entier) : reel
Var i : entier
    total : reel
Debut
    total := 0
    Pour i de 1 a n Faire
        total := total + C[i].valeurEstimee
    finPour
    Retourner total
Fin

// Piece la plus ancienne
Fonction PieceLaPlusAncienne(C : Collection, n : entier) : entier
Var i, indiceMin : entier
Debut
    Si n = 0 Alors
        Retourner -1
    finSi

    indiceMin := 1
    Pour i de 2 a n Faire
        Si C[i].annee < C[indiceMin].annee Alors
            indiceMin := i
        finSi
    finPour
    Retourner indiceMin
Fin

// Piece la plus chere
Fonction PieceLaPlusChere(C : Collection, n : entier) : entier
Var i, indiceMax : entier
Debut
    Si n = 0 Alors
        Retourner -1
    finSi

    indiceMax := 1
    Pour i de 2 a n Faire
        Si C[i].valeurEstimee > C[indiceMax].valeurEstimee Alors
            indiceMax := i
        finSi
    finPour
    Retourner indiceMax
Fin

// Statistiques par metal
Procedure StatistiquesParMetal(C : Collection, n : entier)
Var i : entier
    nbOr, nbArgent, nbBronze, nbCuivre : entier
    valOr, valArgent, valBronze, valCuivre : reel
Debut
    nbOr := 0; valOr := 0
    nbArgent := 0; valArgent := 0
    nbBronze := 0; valBronze := 0
    nbCuivre := 0; valCuivre := 0

    Pour i de 1 a n Faire
        Selon C[i].metal Faire
            cas 'O':
                nbOr := nbOr + 1
                valOr := valOr + C[i].valeurEstimee
            cas 'A':
                nbArgent := nbArgent + 1
                valArgent := valArgent + C[i].valeurEstimee
            cas 'B':
                nbBronze := nbBronze + 1
                valBronze := valBronze + C[i].valeurEstimee
            cas 'C':
                nbCuivre := nbCuivre + 1
                valCuivre := valCuivre + C[i].valeurEstimee
        finSelon
    finPour

    Ecrire("Par metal :")
    Ecrire("- Or     : ", nbOr, " pieces - ", valOr, " EUR")
    Ecrire("- Argent : ", nbArgent, " pieces - ", valArgent, " EUR")
    Ecrire("- Bronze : ", nbBronze, " pieces - ", valBronze, " EUR")
    Ecrire("- Cuivre : ", nbCuivre, " pieces - ", valCuivre, " EUR")
Fin

// Afficher toutes les statistiques
Procedure AfficherStatistiques(C : Collection, n : entier)
Var idx : entier
Debut
    Si n = 0 Alors
        Ecrire("Collection vide, pas de statistiques.")
        Retourner
    finSi

    Ecrire("=== STATISTIQUES ===")
    Ecrire("Nombre total de pieces : ", n)
    Ecrire("Valeur totale estimee  : ", ValeurTotale(C, n), " EUR")
    Ecrire("")

    idx := PieceLaPlusAncienne(C, n)
    Si idx > 0 Alors
        Ecrire("Piece la plus ancienne : ", C[idx].pays, " ", C[idx].annee)
    finSi

    idx := PieceLaPlusChere(C, n)
    Si idx > 0 Alors
        Ecrire("Piece la plus chere    : ", C[idx].pays, " ", C[idx].annee, " - ", C[idx].valeurEstimee, " EUR")
    finSi

    Ecrire("")
    StatistiquesParMetal(C, n)
Fin
```

</details>

<details>
<summary>Solution - Programme principal</summary>

```text
Programme GestionCollection
Var maCollection : Collection
    nbPieces : entier
    choix : entier

Debut
    nbPieces := 0

    Repeter
        Ecrire("")
        Ecrire("--- MA COLLECTION DE PIECES ---")
        Ecrire("1. Ajouter une piece")
        Ecrire("2. Afficher la collection")
        Ecrire("3. Rechercher par pays")
        Ecrire("4. Rechercher par metal")
        Ecrire("5. Rechercher par periode")
        Ecrire("6. Statistiques")
        Ecrire("7. Supprimer une piece")
        Ecrire("0. Quitter")
        Ecrire("")
        Ecrire("Choix : ")
        Lire(choix)

        Selon choix Faire
            cas 1: AjouterPiece(maCollection, nbPieces)
            cas 2: AfficherCollection(maCollection, nbPieces)
            cas 3: RechercherParPays(maCollection, nbPieces)
            cas 4: RechercherParMetal(maCollection, nbPieces)
            cas 5: RechercherParPeriode(maCollection, nbPieces)
            cas 6: AfficherStatistiques(maCollection, nbPieces)
            cas 7: SupprimerPiece(maCollection, nbPieces)
        finSelon

    Jusqu'a choix = 0

    Ecrire("Au revoir !")
Fin
```

</details>

---

## Correspondance COBOL

```cobol
01 WS-COLLECTION.
   05 WS-NB-PIECES    PIC 9(3) VALUE 0.
   05 WS-PIECE OCCURS 200 TIMES.
      10 PC-PAYS       PIC X(20).
      10 PC-ANNEE      PIC 9(4).
      10 PC-VAL-FACIALE PIC 9(4)V99.
      10 PC-METAL      PIC X.
         88 PC-OR        VALUE 'O'.
         88 PC-ARGENT    VALUE 'A'.
         88 PC-BRONZE    VALUE 'B'.
         88 PC-CUIVRE    VALUE 'C'.
      10 PC-ETAT       PIC XX.
         88 PC-TRES-BIEN VALUE 'TB'.
         88 PC-BIEN      VALUE 'B '.
         88 PC-MOYEN     VALUE 'M '.
         88 PC-USE       VALUE 'U '.
      10 PC-VAL-ESTIMEE PIC 9(6)V99.
```

---

*Exercice 11 - Algorithmique Chapitre 02 - M2i*
