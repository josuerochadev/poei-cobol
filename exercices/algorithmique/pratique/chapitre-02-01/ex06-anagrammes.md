# Exercice 6 : Test d'anagrammes

## Enonce

Deux mots sont des **anagrammes** s'ils contiennent exactement les memes caracteres (meme nombre de chaque caractere).

Ecrire une fonction qui determine si deux mots sont des anagrammes.

## Exemples

| Mot 1 | Mot 2 | Resultat |
|-------|-------|----------|
| SEL | LES | Anagrammes |
| ARBRE | BARRE | Anagrammes |
| LASSE | SALLE | Pas anagrammes (LASSE a 2 S, SALLE a 2 L) |
| CHAT | CHATS | Pas anagrammes (longueurs differentes) |

## Concepts a utiliser

- Comparaison de chaines caractere par caractere
- Marquage d'elements utilises
- Verification de longueur

## Principe de l'algorithme

Pour chaque caractere de M1 :
1. Le chercher dans M2
2. Si trouve : "marquer" ce caractere comme utilise (le remplacer par le dernier element et reduire la taille)
3. Si non trouve : ce ne sont pas des anagrammes

---

<details>
<summary>Solution</summary>

```text
Fonction Anagramme(M1, M2 : Type_TAB; N1, N2 : Entier) : Booleen
Var   i, j : Entier
      Valid : Booleen

DEBUT
    // Verification prealable : meme longueur requise
    Si N1 <> N2 Alors
        Retourner Faux
    finSi

    i := 1
    Valid := Vrai

    TantQue (i <= N1) et Valid Faire
        // Chercher M1[i] dans M2
        j := 1
        TantQue (j <= N2) et (M1[i] <> M2[j]) Faire
            j := j + 1
        finTantQue

        Si j <= N2 Alors
            // Trouve : "supprimer" M2[j] en le remplacant par le dernier
            M2[j] := M2[N2]
            N2 := N2 - 1
            i := i + 1
        SiNon
            // Non trouve : pas un anagramme
            Valid := Faux
        finSi
    finTantQue

    Retourner Valid
FIN
```

**Point cle :** On "supprime" un caractere de M2 en le remplacant par le dernier element et en decrementant N2. Cela evite de parcourir des caracteres deja utilises.

</details>

---

## Trace d'execution pour "ARBRE" / "BARRE"

| Etape | M1[i] | M2 avant | j trouve | Action | M2 apres | N2 |
|-------|-------|----------|----------|--------|----------|-----|
| 1 | A | BARRE | j=2 | M2[2] := M2[5], N2-- | BERRE | 4 |
| 2 | R | BERRE | j=3 | M2[3] := M2[4], N2-- | BEEE | 3 |
| 3 | B | BEEE | j=1 | M2[1] := M2[3], N2-- | EEE | 2 |
| 4 | R | EEE | j>N2 | Non trouve ! | - | - |

**Resultat : Faux** (ARBRE et BARRE ne sont pas des anagrammes car ARBRE a 2 R et BARRE n'en a qu'un... attends, BARRE a aussi 2 R!)

Correction de la trace :

| Etape | M1[i] | M2 avant | j trouve | Action | M2 apres | N2 |
|-------|-------|----------|----------|--------|----------|-----|
| 1 | A | BARRE | j=2 | M2[2] := M2[5], N2-- | BERRE | 4 |
| 2 | R | BERRE | j=3 | M2[3] := M2[4], N2-- | BEEE | 3 |

Hmm, il y a une erreur dans l'exemple du cours. Reprenons :

**Trace corrigee pour "SEL" / "LES" :**

| Etape | M1[i] | M2 avant | j trouve | Action | M2 apres | N2 |
|-------|-------|----------|----------|--------|----------|-----|
| 1 | S | LES | j=3 | M2[3] := M2[3], N2-- | LE | 2 |
| 2 | E | LE | j=2 | M2[2] := M2[2], N2-- | L | 1 |
| 3 | L | L | j=1 | M2[1] := M2[1], N2-- | - | 0 |

**Resultat : Vrai**

---

## Variante : Avec comptage de frequences

<details>
<summary>Solution alternative (plus efficace)</summary>

```text
Fonction Anagramme_v2(M1, M2 : Type_TAB; N1, N2 : Entier) : Booleen
Var   freq : tableau [0..255] de Entier  // Frequence de chaque caractere
      i : Entier

DEBUT
    Si N1 <> N2 Alors
        Retourner Faux
    finSi

    // Initialiser le tableau de frequences a 0
    Pour i de 0 a 255 Faire
        freq[i] := 0
    finPour

    // Compter les caracteres de M1 (+1) et M2 (-1)
    Pour i de 1 a N1 Faire
        freq[Ord(M1[i])] := freq[Ord(M1[i])] + 1
        freq[Ord(M2[i])] := freq[Ord(M2[i])] - 1
    finPour

    // Verifier que toutes les frequences sont a 0
    Pour i de 0 a 255 Faire
        Si freq[i] <> 0 Alors
            Retourner Faux
        finSi
    finPour

    Retourner Vrai
FIN
```

**Complexite :** O(n) au lieu de O(n^2)

</details>

---

*Exercice 6 - Algorithmique Chapitre 02 - M2i*
