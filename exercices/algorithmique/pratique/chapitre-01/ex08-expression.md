# Exercice 8 : Validation d'Expression Arithmetique

## Enonce

Controler la validite d'une expression arithmetique. Une expression est valide si :
1. Tout caractere est : un chiffre ('0'...'9'), un operateur (+, -, *, /), ou une parenthese
2. Les parentheses sont coherentes (chaque '(' a une ')' correspondante posterieure)

L'expression se termine par le caractere '#'.

## Exemples

| Expression | Validite | Raison |
|------------|----------|--------|
| `((7+3)*5)-6#` | Valide | Parentheses equilibrees, caracteres valides |
| `(7+3)*5)-6#` | Non valide | Parenthese fermante orpheline |
| `(7+3)*((5-6)#` | Non valide | Parenthese ouvrante non fermee |
| `7+3a*5#` | Non valide | Caractere 'a' invalide |

## Concepts a utiliser

- Boucle `Repeter...Jusqu'a`
- Compteur de parentheses
- Test d'appartenance aux caracteres valides
- Sortie anticipee si invalide

## Etapes suggerees

1. Initialiser : valide := vrai, nbp := 0
2. Lire caractere par caractere :
   - Si '(' : incrementer nbp
   - Si ')' : decrementer nbp
   - Si autre caractere non valide : valide := faux
   - Si nbp < 0 : parenthese fermante orpheline
3. Continuer jusqu'a '#' ou expression invalide
4. Verifier que nbp = 0 (toutes parentheses fermees)
5. Afficher le resultat

---

<details>
<summary>Solution</summary>

```text
Algorithme Expression_valide
var c : Caractere
    valide : Booleen
    nbp : Entier

DEBUT
    valide := vrai
    nbp := 0

    Ecrire("Entrez l'expression (terminer par #) :")

    Repeter
        Lire(c)

        Si c = '(' Alors
            nbp := nbp + 1
        SiNonSi c = ')' Alors
            nbp := nbp - 1
        SiNonSi c <> '#' Alors
            // Verifier si c'est un caractere valide
            Si c Non dans {'0'..'9', '+', '-', '*', '/'} Alors
                valide := faux
            finSi
        finSi

        // Parenthese fermante sans ouvrante correspondante
        Si nbp < 0 Alors
            valide := faux
        finSi

    Jusqu'a c = '#' ou non valide

    // Verification finale : toutes les parentheses fermees ?
    Si nbp > 0 Alors
        valide := faux
    finSi

    // Affichage du resultat
    Si valide Alors
        Ecrire("Expression valide")
    SiNon
        Ecrire("Expression non valide")
    finSi
FIN
```

**Points cles :**
- **Compteur de parentheses** (`nbp`) : +1 pour '(', -1 pour ')'
- Si `nbp < 0` : parenthese fermante orpheline (ex: `5)+3`)
- Si `nbp > 0` a la fin : parenthese ouvrante non fermee (ex: `(5+3`)
- **Sortie anticipee** : des qu'on detecte une erreur, on arrete l'analyse
- Cette methode ne verifie pas la syntaxe complete (ex: `++5` serait considere valide)

</details>

---

*Exercice 8 - Algorithmique M2i*
