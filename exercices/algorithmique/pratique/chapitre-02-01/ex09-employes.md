# Exercice 9 : Gestion d'une liste d'employes

## Enonce

On souhaite gerer une liste d'employes d'une entreprise. Chaque employe est defini par :
- Un matricule (entier)
- Un nom (chaine de 30 caracteres)
- Un prenom (chaine de 20 caracteres)
- Une adresse composee de :
  - Numero (entier)
  - Rue (chaine de 40 caracteres)
  - Code postal (entier)
  - Ville (chaine de 30 caracteres)
- Un salaire (reel)

### Partie A : Definitions

1. Definir le type `t_adresse` pour representer une adresse
2. Definir le type `EMPLOYE` utilisant `t_adresse`
3. Definir un tableau `T_EMPLOYES` pouvant contenir jusqu'a 100 employes

### Partie B : Operations de base

Ecrire les procedures/fonctions suivantes :

1. **SaisirEmploye** : Procedure qui saisit les informations d'un employe
2. **AfficherEmploye** : Procedure qui affiche les informations d'un employe
3. **AfficherListe** : Procedure qui affiche la liste de tous les employes

### Partie C : Recherche et statistiques

1. **RechercherParMatricule** : Fonction qui recherche un employe par son matricule et retourne sa position (-1 si non trouve)
2. **CalculerMoyenneSalaires** : Fonction qui calcule la moyenne des salaires
3. **CompterParVille** : Fonction qui compte le nombre d'employes habitant une ville donnee

### Partie D : Modifications

1. **ModifierSalaire** : Procedure qui modifie le salaire d'un employe identifie par son matricule
2. **SupprimerEmploye** : Procedure qui supprime un employe de la liste (par matricule)

## Structures de donnees

```text
// Type pour l'adresse (structure imbriquee)
Type t_adresse = enregistrement
    numero : entier
    rue : chaine[40]
    codePostal : entier
    ville : chaine[30]
fin enregistrement

// Type pour l'employe
Type EMPLOYE = enregistrement
    matricule : entier
    nom : chaine[30]
    prenom : chaine[20]
    adresse : t_adresse
    salaire : reel
fin enregistrement

// Tableau d'employes
Type T_EMPLOYES = tableau[1..100] de EMPLOYE
```

## Exemple d'utilisation

```text
--- MENU GESTION EMPLOYES ---
1. Ajouter un employe
2. Afficher la liste
3. Rechercher par matricule
4. Modifier un salaire
5. Supprimer un employe
6. Statistiques
0. Quitter

Choix : 1

Matricule : 1001
Nom : DUPONT
Prenom : Jean
Adresse - Numero : 12
Adresse - Rue : Avenue de la Liberte
Adresse - Code postal : 67000
Adresse - Ville : STRASBOURG
Salaire : 2500.00

Employe ajoute avec succes !
```

## Concepts a utiliser

- Enregistrements (records)
- Structures imbriquees (nested records)
- Tableaux d'enregistrements
- Acces aux champs avec notation pointee (employe.adresse.ville)
- Passage de parametres par reference pour les structures

---

<details>
<summary>Solution - Partie A : Definitions</summary>

```text
// Types deja definis dans l'enonce
Type t_adresse = enregistrement
    numero : entier
    rue : chaine[40]
    codePostal : entier
    ville : chaine[30]
fin enregistrement

Type EMPLOYE = enregistrement
    matricule : entier
    nom : chaine[30]
    prenom : chaine[20]
    adresse : t_adresse
    salaire : reel
fin enregistrement

Type T_EMPLOYES = tableau[1..100] de EMPLOYE

// Variables globales
Var listeEmployes : T_EMPLOYES
    nbEmployes : entier
```

</details>

<details>
<summary>Solution - Partie B : Operations de base</summary>

```text
// Procedure de saisie d'un employe
Procedure SaisirEmploye(var emp : EMPLOYE)
Debut
    Ecrire("Matricule : ")
    Lire(emp.matricule)

    Ecrire("Nom : ")
    Lire(emp.nom)

    Ecrire("Prenom : ")
    Lire(emp.prenom)

    Ecrire("Adresse - Numero : ")
    Lire(emp.adresse.numero)

    Ecrire("Adresse - Rue : ")
    Lire(emp.adresse.rue)

    Ecrire("Adresse - Code postal : ")
    Lire(emp.adresse.codePostal)

    Ecrire("Adresse - Ville : ")
    Lire(emp.adresse.ville)

    Ecrire("Salaire : ")
    Lire(emp.salaire)
Fin

// Procedure d'affichage d'un employe
Procedure AfficherEmploye(emp : EMPLOYE)
Debut
    Ecrire("----------------------------------------")
    Ecrire("Matricule : ", emp.matricule)
    Ecrire("Nom : ", emp.nom, " ", emp.prenom)
    Ecrire("Adresse : ", emp.adresse.numero, " ", emp.adresse.rue)
    Ecrire("          ", emp.adresse.codePostal, " ", emp.adresse.ville)
    Ecrire("Salaire : ", emp.salaire, " EUR")
    Ecrire("----------------------------------------")
Fin

// Procedure d'affichage de la liste complete
Procedure AfficherListe(T : T_EMPLOYES, n : entier)
Var i : entier
Debut
    Si n = 0 Alors
        Ecrire("La liste est vide.")
    Sinon
        Ecrire("=== LISTE DES ", n, " EMPLOYES ===")
        Pour i de 1 a n Faire
            AfficherEmploye(T[i])
        finPour
    finSi
Fin
```

</details>

<details>
<summary>Solution - Partie C : Recherche et statistiques</summary>

```text
// Recherche par matricule - retourne la position ou -1
Fonction RechercherParMatricule(T : T_EMPLOYES, n : entier, mat : entier) : entier
Var i : entier
    trouve : booleen
Debut
    i := 1
    trouve := FAUX

    TantQue (i <= n) ET (NON trouve) Faire
        Si T[i].matricule = mat Alors
            trouve := VRAI
        Sinon
            i := i + 1
        finSi
    finTantQue

    Si trouve Alors
        Retourner i
    Sinon
        Retourner -1
    finSi
Fin

// Calcul de la moyenne des salaires
Fonction CalculerMoyenneSalaires(T : T_EMPLOYES, n : entier) : reel
Var i : entier
    somme : reel
Debut
    Si n = 0 Alors
        Retourner 0
    finSi

    somme := 0
    Pour i de 1 a n Faire
        somme := somme + T[i].salaire
    finPour

    Retourner somme / n
Fin

// Compter les employes d'une ville
Fonction CompterParVille(T : T_EMPLOYES, n : entier, ville : chaine) : entier
Var i, compteur : entier
Debut
    compteur := 0
    Pour i de 1 a n Faire
        Si T[i].adresse.ville = ville Alors
            compteur := compteur + 1
        finSi
    finPour
    Retourner compteur
Fin
```

</details>

<details>
<summary>Solution - Partie D : Modifications</summary>

```text
// Modifier le salaire d'un employe
Procedure ModifierSalaire(var T : T_EMPLOYES, n : entier, mat : entier, nouveauSalaire : reel)
Var pos : entier
Debut
    pos := RechercherParMatricule(T, n, mat)

    Si pos = -1 Alors
        Ecrire("Employe non trouve !")
    Sinon
        Ecrire("Ancien salaire : ", T[pos].salaire)
        T[pos].salaire := nouveauSalaire
        Ecrire("Nouveau salaire : ", T[pos].salaire)
    finSi
Fin

// Supprimer un employe (decalage des elements)
Procedure SupprimerEmploye(var T : T_EMPLOYES, var n : entier, mat : entier)
Var pos, i : entier
Debut
    pos := RechercherParMatricule(T, n, mat)

    Si pos = -1 Alors
        Ecrire("Employe non trouve !")
    Sinon
        // Decaler tous les elements suivants
        Pour i de pos a n-1 Faire
            T[i] := T[i+1]
        finPour
        n := n - 1
        Ecrire("Employe supprime avec succes.")
    finSi
Fin
```

</details>

<details>
<summary>Solution - Programme principal complet</summary>

```text
Programme GestionEmployes
Var listeEmployes : T_EMPLOYES
    nbEmployes : entier
    choix : entier
    emp : EMPLOYE
    matricule : entier
    nouveauSalaire : reel
    ville : chaine[30]
    pos : entier

Debut
    nbEmployes := 0

    Repeter
        Ecrire("")
        Ecrire("--- MENU GESTION EMPLOYES ---")
        Ecrire("1. Ajouter un employe")
        Ecrire("2. Afficher la liste")
        Ecrire("3. Rechercher par matricule")
        Ecrire("4. Modifier un salaire")
        Ecrire("5. Supprimer un employe")
        Ecrire("6. Statistiques")
        Ecrire("0. Quitter")
        Ecrire("")
        Ecrire("Choix : ")
        Lire(choix)

        Selon choix Faire
            cas 1:
                Si nbEmployes < 100 Alors
                    SaisirEmploye(emp)
                    nbEmployes := nbEmployes + 1
                    listeEmployes[nbEmployes] := emp
                    Ecrire("Employe ajoute avec succes !")
                Sinon
                    Ecrire("Liste pleine !")
                finSi

            cas 2:
                AfficherListe(listeEmployes, nbEmployes)

            cas 3:
                Ecrire("Matricule a rechercher : ")
                Lire(matricule)
                pos := RechercherParMatricule(listeEmployes, nbEmployes, matricule)
                Si pos = -1 Alors
                    Ecrire("Employe non trouve.")
                Sinon
                    Ecrire("Employe trouve en position ", pos)
                    AfficherEmploye(listeEmployes[pos])
                finSi

            cas 4:
                Ecrire("Matricule de l'employe : ")
                Lire(matricule)
                Ecrire("Nouveau salaire : ")
                Lire(nouveauSalaire)
                ModifierSalaire(listeEmployes, nbEmployes, matricule, nouveauSalaire)

            cas 5:
                Ecrire("Matricule a supprimer : ")
                Lire(matricule)
                SupprimerEmploye(listeEmployes, nbEmployes, matricule)

            cas 6:
                Ecrire("Moyenne des salaires : ", CalculerMoyenneSalaires(listeEmployes, nbEmployes), " EUR")
                Ecrire("Ville a rechercher : ")
                Lire(ville)
                Ecrire("Nombre d'employes a ", ville, " : ", CompterParVille(listeEmployes, nbEmployes, ville))
        finSelon

    Jusqu'a choix = 0

    Ecrire("Au revoir !")
Fin
```

</details>

---

## Correspondance COBOL

```cobol
01 WS-EMPLOYE.
   05 EMP-MATRICULE    PIC 9(4).
   05 EMP-NOM          PIC X(30).
   05 EMP-PRENOM       PIC X(20).
   05 EMP-ADRESSE.
      10 ADR-NUMERO    PIC 9(4).
      10 ADR-RUE       PIC X(40).
      10 ADR-CP        PIC 9(5).
      10 ADR-VILLE     PIC X(30).
   05 EMP-SALAIRE      PIC 9(5)V99.
```

---

*Exercice 9 - Algorithmique Chapitre 02 - M2i*
