# Exercice 10 : Gestion d'un hotel

## Enonce

Un hotel dispose de 50 chambres. Chaque chambre est definie par :
- Un numero de chambre (entier)
- Un etage (entier)
- Un type ('S' = Simple, 'D' = Double, 'F' = Familiale)
- Un prix par nuit (reel)
- Un statut (booleen : occupee ou libre)
- Un occupant (si occupee) defini par :
  - Nom (chaine de 30 caracteres)
  - Prenom (chaine de 20 caracteres)
  - Date d'arrivee (chaine au format JJ/MM/AAAA)
  - Nombre de nuits prevues (entier)

### Partie A : Definitions

1. Definir le type `personne` pour representer un occupant
2. Definir le type `chambre` avec une structure imbriquee pour l'occupant
3. Definir un tableau `HOTEL` de 50 chambres

### Partie B : Initialisation

Ecrire une procedure `InitialiserHotel` qui :
- Initialise les 50 chambres (numerotees de 101 a 150)
- Les chambres 101-120 sont au 1er etage, 121-140 au 2eme, 141-150 au 3eme
- Types : 101-110 Simple, 111-130 Double, 131-150 Familiale
- Prix : Simple=50, Double=80, Familiale=120
- Toutes les chambres sont libres au depart

### Partie C : Operations de base

1. **AfficherChambre** : Affiche les details d'une chambre
2. **ChambresLibres** : Affiche toutes les chambres libres
3. **ChambresOccupees** : Affiche toutes les chambres occupees avec leurs occupants

### Partie D : Reservations

1. **ReserverChambre** : Reserve une chambre libre pour un client
2. **LibererChambre** : Libere une chambre (check-out)
3. **TrouverChambreLibre** : Trouve une chambre libre d'un type donne

### Partie E : Statistiques

1. **TauxOccupation** : Calcule le taux d'occupation de l'hotel (%)
2. **ChiffreAffaires** : Calcule le chiffre d'affaires journalier (chambres occupees)
3. **CompterParType** : Compte les chambres libres par type

## Structures de donnees

```text
Type personne = enregistrement
    nom : chaine[30]
    prenom : chaine[20]
    dateArrivee : chaine[10]
    nbNuits : entier
fin enregistrement

Type chambre = enregistrement
    numero : entier
    etage : entier
    typeCh : caractere        // 'S', 'D', 'F'
    prixNuit : reel
    occupee : booleen
    occupant : personne       // Structure imbriquee
fin enregistrement

Type HOTEL = tableau[1..50] de chambre
```

## Exemple d'utilisation

```text
--- GESTION HOTEL ***BELLEVUE*** ---
1. Afficher chambres libres
2. Afficher chambres occupees
3. Reserver une chambre
4. Liberer une chambre
5. Statistiques
0. Quitter

Choix : 1

=== CHAMBRES LIBRES ===
Chambre 101 - Etage 1 - Simple - 50.00 EUR/nuit
Chambre 102 - Etage 1 - Simple - 50.00 EUR/nuit
...

Choix : 3

Type souhaite (S/D/F) : D
Chambre 111 disponible.

Nom du client : MARTIN
Prenom : Pierre
Date d'arrivee : 15/11/2025
Nombre de nuits : 3

Reservation confirmee - Chambre 111
Total prevu : 240.00 EUR
```

## Concepts a utiliser

- Structures imbriquees (chambre contient personne)
- Initialisation conditionnelle
- Recherche avec criteres multiples
- Calculs statistiques sur tableaux de structures

---

<details>
<summary>Solution - Partie A et B : Definitions et Initialisation</summary>

```text
// Definitions des types (comme dans l'enonce)
Type personne = enregistrement
    nom : chaine[30]
    prenom : chaine[20]
    dateArrivee : chaine[10]
    nbNuits : entier
fin enregistrement

Type chambre = enregistrement
    numero : entier
    etage : entier
    typeCh : caractere
    prixNuit : reel
    occupee : booleen
    occupant : personne
fin enregistrement

Type HOTEL = tableau[1..50] de chambre

// Procedure d'initialisation
Procedure InitialiserHotel(var H : HOTEL)
Var i, numCh : entier
Debut
    Pour i de 1 a 50 Faire
        numCh := 100 + i
        H[i].numero := numCh

        // Determination de l'etage
        Si numCh <= 120 Alors
            H[i].etage := 1
        SinonSi numCh <= 140 Alors
            H[i].etage := 2
        Sinon
            H[i].etage := 3
        finSi

        // Determination du type et du prix
        Si numCh <= 110 Alors
            H[i].typeCh := 'S'
            H[i].prixNuit := 50.0
        SinonSi numCh <= 130 Alors
            H[i].typeCh := 'D'
            H[i].prixNuit := 80.0
        Sinon
            H[i].typeCh := 'F'
            H[i].prixNuit := 120.0
        finSi

        // Toutes libres au depart
        H[i].occupee := FAUX
        H[i].occupant.nom := ""
        H[i].occupant.prenom := ""
        H[i].occupant.dateArrivee := ""
        H[i].occupant.nbNuits := 0
    finPour
Fin
```

</details>

<details>
<summary>Solution - Partie C : Operations de base</summary>

```text
// Fonction pour obtenir le libelle du type
Fonction LibelleType(t : caractere) : chaine
Debut
    Selon t Faire
        cas 'S': Retourner "Simple"
        cas 'D': Retourner "Double"
        cas 'F': Retourner "Familiale"
        defaut: Retourner "Inconnu"
    finSelon
Fin

// Afficher une chambre
Procedure AfficherChambre(ch : chambre)
Debut
    Ecrire("Chambre ", ch.numero, " - Etage ", ch.etage)
    Ecrire("Type : ", LibelleType(ch.typeCh), " - Prix : ", ch.prixNuit, " EUR/nuit")

    Si ch.occupee Alors
        Ecrire("Statut : OCCUPEE")
        Ecrire("Occupant : ", ch.occupant.prenom, " ", ch.occupant.nom)
        Ecrire("Arrivee : ", ch.occupant.dateArrivee, " - Nuits : ", ch.occupant.nbNuits)
    Sinon
        Ecrire("Statut : LIBRE")
    finSi
    Ecrire("--------------------")
Fin

// Afficher toutes les chambres libres
Procedure ChambresLibres(H : HOTEL)
Var i, compteur : entier
Debut
    Ecrire("=== CHAMBRES LIBRES ===")
    compteur := 0
    Pour i de 1 a 50 Faire
        Si NON H[i].occupee Alors
            Ecrire("Chambre ", H[i].numero, " - Etage ", H[i].etage,
                   " - ", LibelleType(H[i].typeCh), " - ", H[i].prixNuit, " EUR/nuit")
            compteur := compteur + 1
        finSi
    finPour
    Ecrire("Total : ", compteur, " chambres libres")
Fin

// Afficher toutes les chambres occupees
Procedure ChambresOccupees(H : HOTEL)
Var i, compteur : entier
Debut
    Ecrire("=== CHAMBRES OCCUPEES ===")
    compteur := 0
    Pour i de 1 a 50 Faire
        Si H[i].occupee Alors
            Ecrire("Chambre ", H[i].numero, " - ", H[i].occupant.prenom, " ", H[i].occupant.nom)
            Ecrire("   Depuis : ", H[i].occupant.dateArrivee, " pour ", H[i].occupant.nbNuits, " nuits")
            compteur := compteur + 1
        finSi
    finPour
    Ecrire("Total : ", compteur, " chambres occupees")
Fin
```

</details>

<details>
<summary>Solution - Partie D : Reservations</summary>

```text
// Trouver une chambre libre d'un type donne
// Retourne l'indice ou -1 si aucune disponible
Fonction TrouverChambreLibre(H : HOTEL, typeVoulu : caractere) : entier
Var i : entier
Debut
    Pour i de 1 a 50 Faire
        Si (NON H[i].occupee) ET (H[i].typeCh = typeVoulu) Alors
            Retourner i
        finSi
    finPour
    Retourner -1
Fin

// Reserver une chambre
Procedure ReserverChambre(var H : HOTEL)
Var typeVoulu : caractere
    idx : entier
    client : personne
Debut
    Ecrire("Type souhaite (S=Simple, D=Double, F=Familiale) : ")
    Lire(typeVoulu)

    idx := TrouverChambreLibre(H, typeVoulu)

    Si idx = -1 Alors
        Ecrire("Aucune chambre de ce type disponible.")
    Sinon
        Ecrire("Chambre ", H[idx].numero, " disponible.")
        Ecrire("")

        Ecrire("Nom du client : ")
        Lire(client.nom)
        Ecrire("Prenom : ")
        Lire(client.prenom)
        Ecrire("Date d'arrivee (JJ/MM/AAAA) : ")
        Lire(client.dateArrivee)
        Ecrire("Nombre de nuits : ")
        Lire(client.nbNuits)

        // Enregistrer la reservation
        H[idx].occupee := VRAI
        H[idx].occupant := client

        Ecrire("")
        Ecrire("Reservation confirmee - Chambre ", H[idx].numero)
        Ecrire("Total prevu : ", H[idx].prixNuit * client.nbNuits, " EUR")
    finSi
Fin

// Liberer une chambre (check-out)
Procedure LibererChambre(var H : HOTEL)
Var numCh, i : entier
    trouve : booleen
    montant : reel
Debut
    Ecrire("Numero de chambre a liberer : ")
    Lire(numCh)

    // Rechercher la chambre
    trouve := FAUX
    Pour i de 1 a 50 Faire
        Si H[i].numero = numCh Alors
            trouve := VRAI

            Si NON H[i].occupee Alors
                Ecrire("Cette chambre est deja libre.")
            Sinon
                // Calculer le montant
                montant := H[i].prixNuit * H[i].occupant.nbNuits

                Ecrire("Check-out de ", H[i].occupant.prenom, " ", H[i].occupant.nom)
                Ecrire("Montant a regler : ", montant, " EUR")

                // Liberer la chambre
                H[i].occupee := FAUX
                H[i].occupant.nom := ""
                H[i].occupant.prenom := ""
                H[i].occupant.dateArrivee := ""
                H[i].occupant.nbNuits := 0

                Ecrire("Chambre ", numCh, " liberee.")
            finSi
        finSi
    finPour

    Si NON trouve Alors
        Ecrire("Chambre non trouvee.")
    finSi
Fin
```

</details>

<details>
<summary>Solution - Partie E : Statistiques</summary>

```text
// Taux d'occupation
Fonction TauxOccupation(H : HOTEL) : reel
Var i, occupees : entier
Debut
    occupees := 0
    Pour i de 1 a 50 Faire
        Si H[i].occupee Alors
            occupees := occupees + 1
        finSi
    finPour
    Retourner (occupees / 50) * 100
Fin

// Chiffre d'affaires journalier (chambres occupees)
Fonction ChiffreAffaires(H : HOTEL) : reel
Var i : entier
    total : reel
Debut
    total := 0
    Pour i de 1 a 50 Faire
        Si H[i].occupee Alors
            total := total + H[i].prixNuit
        finSi
    finPour
    Retourner total
Fin

// Compter chambres libres par type
Procedure CompterParType(H : HOTEL)
Var i, simples, doubles, familiales : entier
Debut
    simples := 0
    doubles := 0
    familiales := 0

    Pour i de 1 a 50 Faire
        Si NON H[i].occupee Alors
            Selon H[i].typeCh Faire
                cas 'S': simples := simples + 1
                cas 'D': doubles := doubles + 1
                cas 'F': familiales := familiales + 1
            finSelon
        finSi
    finPour

    Ecrire("=== CHAMBRES LIBRES PAR TYPE ===")
    Ecrire("Simples    : ", simples)
    Ecrire("Doubles    : ", doubles)
    Ecrire("Familiales : ", familiales)
    Ecrire("Total      : ", simples + doubles + familiales)
Fin

// Procedure affichant toutes les statistiques
Procedure AfficherStatistiques(H : HOTEL)
Debut
    Ecrire("=== STATISTIQUES HOTEL ===")
    Ecrire("Taux d'occupation : ", TauxOccupation(H), " %")
    Ecrire("CA journalier     : ", ChiffreAffaires(H), " EUR")
    Ecrire("")
    CompterParType(H)
Fin
```

</details>

<details>
<summary>Solution - Programme principal</summary>

```text
Programme GestionHotel
Var hotel : HOTEL
    choix : entier

Debut
    // Initialisation de l'hotel
    InitialiserHotel(hotel)
    Ecrire("Hotel initialise avec 50 chambres.")

    Repeter
        Ecrire("")
        Ecrire("--- GESTION HOTEL ***BELLEVUE*** ---")
        Ecrire("1. Afficher chambres libres")
        Ecrire("2. Afficher chambres occupees")
        Ecrire("3. Reserver une chambre")
        Ecrire("4. Liberer une chambre")
        Ecrire("5. Statistiques")
        Ecrire("0. Quitter")
        Ecrire("")
        Ecrire("Choix : ")
        Lire(choix)

        Selon choix Faire
            cas 1: ChambresLibres(hotel)
            cas 2: ChambresOccupees(hotel)
            cas 3: ReserverChambre(hotel)
            cas 4: LibererChambre(hotel)
            cas 5: AfficherStatistiques(hotel)
        finSelon

    Jusqu'a choix = 0

    Ecrire("Merci et a bientot !")
Fin
```

</details>

---

## Correspondance COBOL

```cobol
01 WS-HOTEL.
   05 WS-CHAMBRE OCCURS 50 TIMES.
      10 CH-NUMERO     PIC 9(3).
      10 CH-ETAGE      PIC 9.
      10 CH-TYPE       PIC X.
         88 CH-SIMPLE     VALUE 'S'.
         88 CH-DOUBLE     VALUE 'D'.
         88 CH-FAMILIALE  VALUE 'F'.
      10 CH-PRIX       PIC 9(3)V99.
      10 CH-OCCUPEE    PIC X.
         88 CH-LIBRE      VALUE 'N'.
         88 CH-RESERVEE   VALUE 'O'.
      10 CH-OCCUPANT.
         15 OCC-NOM     PIC X(30).
         15 OCC-PRENOM  PIC X(20).
         15 OCC-DATE-ARR PIC X(10).
         15 OCC-NB-NUITS PIC 99.
```

---

*Exercice 10 - Algorithmique Chapitre 02 - M2i*
