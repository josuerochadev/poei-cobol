# Exercice 9 : Jeu de Devinette

## Enonce

Simuler un jeu de devinette :
1. Saisir un nombre cache `NbCache`
2. Choisir un niveau de difficulte :
   - Facile : 10 essais maximum
   - Difficile : 5 essais maximum
3. Le joueur propose des valeurs, l'algorithme indique si c'est plus grand ou plus petit
4. Gagne si trouve avant d'epuiser les essais, sinon perdu

## Exemple de partie

```text
Nombre cache : 42
Niveau : Facile (10 essais)

Essai 1 - Proposition : 50
> Trop grand !

Essai 2 - Proposition : 25
> Trop petit !

Essai 3 - Proposition : 40
> Trop petit !

Essai 4 - Proposition : 42
> Bravo ! Trouve en 4 essais !
```

## Concepts a utiliser

- Boucle `Repeter...Jusqu'a` avec double condition de sortie
- Structure conditionnelle pour le feedback
- Compteur d'essais

## Etapes suggerees

1. Saisir le nombre cache
2. Choisir le niveau (1 = facile, 2 = difficile)
3. Definir le nombre maximum d'essais
4. Boucle de jeu :
   - Lire une proposition
   - Comparer et donner un indice
   - Incrementer le compteur
   - Sortir si trouve ou essais epuises
5. Afficher le message de fin (gagne/perdu)

---

<details>
<summary>Solution</summary>

```text
Algorithme Devinette
var NbCache, NbEssaiMax, Proposition, niveau, essai : Entier

DEBUT
    // Saisie du nombre a deviner (par un autre joueur)
    Ecrire("Joueur 1 - Entrez le nombre cache : ")
    Lire(NbCache)

    // Choix du niveau de difficulte
    Ecrire("Niveau de jeu :")
    Ecrire("  1 = Facile (10 essais)")
    Ecrire("  2 = Difficile (5 essais)")
    Repeter
        Ecrire("Votre choix : ")
        Lire(niveau)
    Jusqu'a (niveau = 1) ou (niveau = 2)

    Si niveau = 1 Alors
        NbEssaiMax := 10
    SiNon
        NbEssaiMax := 5
    finSi

    Ecrire("Joueur 2 - Devinez le nombre !")
    Ecrire("Vous avez ", NbEssaiMax, " essais.")

    // Boucle de jeu
    essai := 0
    Repeter
        essai := essai + 1
        Ecrire("Essai ", essai, " - Votre proposition : ")
        Lire(Proposition)

        Si Proposition < NbCache Alors
            Ecrire("Trop petit !")
        SiNonSi Proposition > NbCache Alors
            Ecrire("Trop grand !")
        SiNon
            Ecrire("Bravo ! Trouve en ", essai, " essais !")
        finSi

    Jusqu'a (essai = NbEssaiMax) ou (Proposition = NbCache)

    // Message de fin si perdu
    Si (essai = NbEssaiMax) et (Proposition <> NbCache) Alors
        Ecrire("Perdu ! Le nombre etait ", NbCache)
    finSi
FIN
```

**Points cles :**
- **Double condition de sortie** : nombre d'essais epuise OU nombre trouve
- **Feedback a chaque essai** : guide le joueur vers la solution
- **Condition de victoire** : si on sort de la boucle avec `Proposition = NbCache`, c'est gagne
- En vrai jeu, le nombre serait genere aleatoirement plutot que saisi

</details>

<details>
<summary>Variante avec nombre aleatoire</summary>

```text
Algorithme Devinette_Aleatoire
var NbCache, NbEssaiMax, Proposition, niveau, essai : Entier
    borneMin, borneMax : Entier

DEBUT
    borneMin := 1
    borneMax := 100

    // Generation d'un nombre aleatoire entre borneMin et borneMax
    NbCache := Aleatoire(borneMin, borneMax)

    // ... (reste identique)
FIN
```

La fonction `Aleatoire(min, max)` genere un entier entre min et max.

</details>

---

*Exercice 9 - Algorithmique M2i*
