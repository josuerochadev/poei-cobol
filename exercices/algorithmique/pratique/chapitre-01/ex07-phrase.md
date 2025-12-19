# Exercice 7 : Analyse d'une Phrase

## Enonce

Lire une phrase (qui se termine par un point) caractere par caractere et determiner :
- La longueur de la phrase (nombre de caracteres)
- Le nombre de mots (separes par un seul espace)
- Le nombre de voyelles

## Exemple

```text
Phrase : "Bonjour le monde."

Longueur : 17 caracteres
Voyelles : 6 (o, o, u, e, o, e)
Mots : 3 (Bonjour, le, monde)
```

## Concepts a utiliser

- Boucle `Repeter...Jusqu'a` (lecture jusqu'au point)
- Test d'appartenance (voyelles)
- Detection des mots (espaces)
- Compteurs multiples

## Etapes suggerees

1. Initialiser les compteurs (lettres, voyelles, mots)
2. Lire caractere par caractere jusqu'au point
3. Pour chaque caractere :
   - Incrementer le compteur de lettres
   - Si voyelle, incrementer le compteur de voyelles
   - Si espace, incrementer le compteur de mots (si mot en cours)
4. Compter le dernier mot (avant le point)
5. Afficher les resultats

---

<details>
<summary>Solution</summary>

```text
Algorithme phrase
var C : Caractere
    NbVoyelles, NbLettres, NbMots, NbLettresMot : Entier

DEBUT
    NbVoyelles := 0
    NbLettres := 0
    NbLettresMot := 0
    NbMots := 0

    Ecrire("Entrez une phrase (terminer par un point) :")

    Repeter
        Lire(C)
        NbLettres := NbLettres + 1

        // Comptage des voyelles
        Si C dans {'a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'} Alors
            NbVoyelles := NbVoyelles + 1
        finSi

        // Detection des mots (separes par espaces)
        Si C = ' ' Alors
            Si NbLettresMot > 0 Alors
                NbMots := NbMots + 1
                NbLettresMot := 0
            finSi
        SiNon
            NbLettresMot := NbLettresMot + 1
        finSi

    Jusqu'a C = '.'

    // Comptage du dernier mot (avant le point)
    Si NbLettresMot > 0 Alors
        NbMots := NbMots + 1
    finSi

    // Affichage des resultats
    Ecrire("Phrase de ", NbLettres, " caracteres")
    Ecrire("dont ", NbVoyelles, " voyelles")
    Ecrire("et qui contient ", NbMots, " mots")
FIN
```

**Points cles :**
- **Test d'appartenance** : `C dans {...}` pour les voyelles (majuscules et minuscules)
- **Comptage de mots** : detection des transitions espace -> caractere
- **Cas particulier** : le dernier mot n'est pas suivi d'un espace, il faut le compter apres la boucle
- `NbLettresMot` sert a savoir si on est dans un mot ou dans des espaces consecutifs

</details>

---

*Exercice 7 - Algorithmique M2i*
