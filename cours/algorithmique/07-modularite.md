# Chapitre VII - Modularite et Sous-programmes

## 1. Introduction a la modularite

### Definition

La **modularite** est un principe de conception qui consiste a decomposer un programme en **modules** (ou sous-programmes) independants et reutilisables.

### Pourquoi la modularite ?

| Avantage | Description |
|----------|-------------|
| **Lisibilite** | Code plus court et comprehensible |
| **Reutilisation** | Un module peut etre appele plusieurs fois |
| **Maintenance** | Modification localisee, impact limite |
| **Tests** | Chaque module peut etre teste separement |
| **Travail d'equipe** | Plusieurs developpeurs en parallele |

### Analogie

Un programme modulaire est comme une recette de cuisine :
- Le plat principal appelle des sous-recettes (sauce, garniture...)
- Chaque sous-recette peut etre reutilisee dans d'autres plats
- Modifier la sauce n'affecte pas la garniture

---

## 2. Types de sous-programmes

### Procedures

Une **procedure** execute une action sans retourner de valeur.

```
Procedure Afficher_Titre()
Debut
    Ecrire("================================")
    Ecrire("       RAPPORT MENSUEL          ")
    Ecrire("================================")
Fin
```

### Fonctions

Une **fonction** execute une action et **retourne une valeur**.

```
Fonction Carre(x : entier) : entier
Debut
    Retourner x * x
Fin
```

### Difference principale

| Aspect | Procedure | Fonction |
|--------|-----------|----------|
| Retour | Aucune valeur | Une valeur |
| Appel | Instruction seule | Dans une expression |
| Exemple | `Afficher_Titre()` | `y := Carre(5)` |

---

## 3. Parametres

### Definition

Les **parametres** permettent de transmettre des donnees entre le programme appelant et le sous-programme.

### Parametres formels vs effectifs

| Type | Definition | Exemple |
|------|------------|---------|
| **Formel** | Declare dans la definition du sous-programme | `Fonction Somme(a, b : entier)` |
| **Effectif** | Valeur passee lors de l'appel | `Somme(5, 3)` |

```
// Definition avec parametres formels
Fonction Somme(a, b : entier) : entier
Debut
    Retourner a + b
Fin

// Appel avec parametres effectifs
x := 5
y := 3
resultat := Somme(x, y)    // x et y sont les parametres effectifs
```

---

## 4. Passage de parametres

### Passage par valeur

Le sous-programme recoit une **copie** de la valeur. La modification du parametre n'affecte pas la variable originale.

```
Procedure Double(x : entier)    // Passage par valeur
Debut
    x := x * 2
    Ecrire("Dans la procedure : x = ", x)
Fin

// Programme principal
a := 5
Double(a)
Ecrire("Apres l'appel : a = ", a)

// Affichage :
// Dans la procedure : x = 10
// Apres l'appel : a = 5      (a n'a pas change !)
```

### Passage par reference

Le sous-programme recoit l'**adresse** de la variable. La modification affecte directement la variable originale.

```
Procedure Double(var x : entier)    // var = par reference
Debut
    x := x * 2
    Ecrire("Dans la procedure : x = ", x)
Fin

// Programme principal
a := 5
Double(a)
Ecrire("Apres l'appel : a = ", a)

// Affichage :
// Dans la procedure : x = 10
// Apres l'appel : a = 10     (a a change !)
```

### Comparaison

| Aspect | Par valeur | Par reference |
|--------|------------|---------------|
| Transmission | Copie de la valeur | Adresse de la variable |
| Modification | N'affecte pas l'original | Affecte l'original |
| Notation | `x : entier` | `var x : entier` |
| Usage | Donnees en entree | Donnees en sortie ou entree/sortie |

### Quand utiliser quoi ?

| Situation | Mode recommande |
|-----------|-----------------|
| Parametre en entree uniquement | Par valeur |
| Parametre en sortie | Par reference |
| Parametre en entree/sortie | Par reference |
| Gros volume de donnees | Par reference (performance) |
| Tableaux | Par reference (toujours) |

---

## 5. Portee des variables

### Definition

La **portee** (ou visibilite) d'une variable determine ou elle peut etre utilisee dans le programme.

### Variables locales

Declarees **dans** un sous-programme, accessibles uniquement dans ce sous-programme.

```
Procedure Exemple()
Var x : entier    // Variable locale
Debut
    x := 10
    Ecrire(x)     // OK
Fin

// Programme principal
Exemple()
Ecrire(x)         // ERREUR : x n'existe pas ici
```

### Variables globales

Declarees **hors** des sous-programmes, accessibles partout.

```
Var compteur : entier    // Variable globale

Procedure Incrementer()
Debut
    compteur := compteur + 1    // Acces a la globale
Fin

// Programme principal
compteur := 0
Incrementer()
Incrementer()
Ecrire(compteur)    // Affiche 2
```

### Masquage

Une variable locale peut **masquer** une variable globale de meme nom.

```
Var x : entier    // Globale

Procedure Test()
Var x : entier    // Locale - masque la globale
Debut
    x := 100      // Modifie la locale
Fin

// Programme principal
x := 5
Test()
Ecrire(x)         // Affiche 5 (la globale n'a pas change)
```

### Bonnes pratiques

| Pratique | Raison |
|----------|--------|
| Limiter les variables globales | Evite les effets de bord |
| Preferer le passage de parametres | Plus explicite |
| Noms explicites pour les globales | Evite la confusion |

---

## 6. Conception modulaire

### Decomposition descendante

Methode **Top-Down** : partir du probleme global et le decomposer en sous-problemes.

```
Programme Principal
├── Initialisation
│   ├── Ouvrir_Fichiers
│   └── Initialiser_Variables
├── Traitement
│   ├── Lire_Donnees
│   ├── Calculer
│   └── Afficher_Resultats
└── Finalisation
    └── Fermer_Fichiers
```

### Principes de bonne conception

| Principe | Description |
|----------|-------------|
| **Cohesion forte** | Un module fait UNE seule chose |
| **Couplage faible** | Modules independants |
| **Abstraction** | Cacher les details d'implementation |
| **Encapsulation** | Regrouper donnees et traitements |

### Taille d'un module

- Idealement : 20-50 lignes
- Maximum recommande : 100 lignes
- Si plus grand : decomposer en sous-modules

---

## 7. Exemples de conception modulaire

### Exemple 1 : Calcul de moyenne

```
Fonction Lire_Notes(var T : Tableau, var n : entier)
// Lit les notes et retourne le nombre lu
Debut
    n := 0
    Repeter
        Ecrire("Note (0 pour terminer) : ")
        Lire(note)
        Si note > 0 Alors
            n := n + 1
            T[n] := note
        Fsi
    Jusqu'a note = 0
Fin

Fonction Calculer_Somme(T : Tableau, n : entier) : reel
Var i : entier
    somme : reel
Debut
    somme := 0
    Pour i de 1 a n faire
        somme := somme + T[i]
    Fpour
    Retourner somme
Fin

Fonction Calculer_Moyenne(T : Tableau, n : entier) : reel
Debut
    Si n = 0 Alors
        Retourner 0
    Sinon
        Retourner Calculer_Somme(T, n) / n
    Fsi
Fin

// Programme principal
Algorithme Moyenne_Notes
Var notes : Tableau[1..100]
    nb : entier
    moy : reel
Debut
    Lire_Notes(notes, nb)
    moy := Calculer_Moyenne(notes, nb)
    Ecrire("Moyenne : ", moy)
Fin
```

### Exemple 2 : Validation de donnees

```
Fonction Est_Valide_Age(age : entier) : booleen
Debut
    Retourner (age >= 0) ET (age <= 150)
Fin

Fonction Est_Valide_Email(email : chaine) : booleen
Debut
    Retourner (Pos(email, "@") > 0) ET (Pos(email, ".") > 0)
Fin

Fonction Valider_Personne(p : Personne) : booleen
Debut
    Retourner Est_Valide_Age(p.age) ET Est_Valide_Email(p.email)
Fin
```

---

## 8. Documentation des sous-programmes

### En-tete standard

```
//--------------------------------------------------------------
// Nom       : Calculer_TVA
// Objet     : Calcule le montant de la TVA
// Parametres:
//   - montant_ht (E) : Montant hors taxe
//   - taux (E)       : Taux de TVA (ex: 20 pour 20%)
// Retour    : Montant de la TVA
// Auteur    : J. Dupont
// Date      : 2025-11-15
//--------------------------------------------------------------
Fonction Calculer_TVA(montant_ht, taux : reel) : reel
Debut
    Retourner montant_ht * taux / 100
Fin
```

### Conventions de documentation

| Element | Description |
|---------|-------------|
| (E) | Parametre en Entree |
| (S) | Parametre en Sortie |
| (E/S) | Parametre en Entree/Sortie |

---

## 9. Correspondance avec COBOL

### Procedures en COBOL

```cobol
PROCEDURE DIVISION.
    PERFORM INITIALISATION.
    PERFORM TRAITEMENT UNTIL FIN-FICHIER = 'O'.
    PERFORM FINALISATION.
    STOP RUN.

INITIALISATION.
    OPEN INPUT FICHIER-ENTREE.
    MOVE 'N' TO FIN-FICHIER.

TRAITEMENT.
    READ FICHIER-ENTREE
        AT END MOVE 'O' TO FIN-FICHIER
    END-READ.
    IF FIN-FICHIER = 'N'
        PERFORM TRAITER-ENREG
    END-IF.

FINALISATION.
    CLOSE FICHIER-ENTREE.
```

### Passage de parametres en COBOL

Avec `PERFORM ... USING` (COBOL 2002+) ou via la WORKING-STORAGE.

```cobol
WORKING-STORAGE SECTION.
01 WS-PARAM-1 PIC 9(4).
01 WS-PARAM-2 PIC 9(4).
01 WS-RESULTAT PIC 9(8).

PROCEDURE DIVISION.
    MOVE 5 TO WS-PARAM-1.
    MOVE 3 TO WS-PARAM-2.
    PERFORM CALCULER-SOMME.
    DISPLAY "Resultat : " WS-RESULTAT.

CALCULER-SOMME.
    ADD WS-PARAM-1 TO WS-PARAM-2 GIVING WS-RESULTAT.
```

### Appel de sous-programmes externes (CALL)

```cobol
CALL 'CALCUL' USING WS-PARAM-1 WS-PARAM-2 WS-RESULTAT.
```

| Algorithmique | COBOL |
|---------------|-------|
| Procedure interne | `PERFORM paragraphe` |
| Procedure externe | `CALL 'programme'` |
| Parametre par valeur | `BY CONTENT` |
| Parametre par reference | `BY REFERENCE` (defaut) |

---

## 10. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-07-modularite.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-07/)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI - Algorithmes sur Fichiers](06-algorithmes-fichiers.md) | [Module Algorithmique](README.md) |

---
*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
