# Exercice 14 : Concatenation de chaines (CONCAT)

## Enonce

Ecrire une procedure `CONCAT (CH1, CH2, CH3)` qui concatene deux chaines de caracteres CH1 et CH2, l'une a la suite de l'autre, pour construire la chaine CH3.

## Parametres

| Parametre | Type | Mode | Description |
|-----------|------|------|-------------|
| CH1 | chaine | Entree | Premiere chaine |
| CH2 | chaine | Entree | Deuxieme chaine |
| CH3 | chaine | Sortie | Chaine resultat (CH1 + CH2) |

## Exemple

```text
CH1 = "Bon" (l = 3)
CH2 = "jour" (l = 4)

Resultat : CH3 = "Bonjour" (l = 7)
```

```text
CH1 = "Hello " (l = 6)
CH2 = "World!" (l = 6)

Resultat : CH3 = "Hello World!" (l = 12)
```

## Schema visuel

```text
CH1:  | B | o | n |           (l = 3)
CH2:  | j | o | u | r |       (l = 4)

CH3:  | B | o | n | j | o | u | r |  (l = 7)
       \_________/ \_____________/
          CH1           CH2
```

## Concepts a utiliser

- Copie de tableaux
- Calcul d'offset (decalage)
- Deux boucles successives

---

<details>
<summary>Solution</summary>

```text
PROCEDURE CONCAT (CH1, CH2: chaine, var CH3: chaine)
VAR i, j : Entier
Debut
    // Copier CH1 dans CH3
    Pour i de 1 a CH1.l faire
        CH3.str[i] := CH1.str[i]
    Fpour

    // Position de depart pour CH2
    j := CH1.l

    // Copier CH2 a la suite
    Pour i de 1 a CH2.l faire
        CH3.str[j + i] := CH2.str[i]
    Fpour

    // Calculer la longueur totale
    CH3.l := CH1.l + CH2.l
Fin
```

</details>

<details>
<summary>Solution - Version alternative</summary>

```text
PROCEDURE CONCAT (CH1, CH2: chaine, var CH3: chaine)
VAR i : Entier
Debut
    // Copier CH1
    Pour i de 1 a CH1.l faire
        CH3.str[i] := CH1.str[i]
    Fpour

    // Copier CH2 (en continuant l'indice)
    Pour i de 1 a CH2.l faire
        CH3.str[CH1.l + i] := CH2.str[i]
    Fpour

    CH3.l := CH1.l + CH2.l
Fin
```

</details>

<details>
<summary>Exercice supplementaire - Concatenation en place</summary>

Ecrire une procedure `APPEND (CH1, CH2)` qui ajoute CH2 a la fin de CH1 (modifie CH1).

```text
PROCEDURE APPEND (var CH1: chaine, CH2: chaine)
VAR i : Entier
Debut
    Pour i de 1 a CH2.l faire
        CH1.str[CH1.l + i] := CH2.str[i]
    Fpour

    CH1.l := CH1.l + CH2.l
Fin
```

**Note** : Verifier que `CH1.l + CH2.l <= MAX` pour eviter le debordement !

</details>

<details>
<summary>Exercice supplementaire - Concatenation avec separateur</summary>

Ecrire une procedure `CONCAT_SEP (CH1, CH2, SEP, CH3)` qui concatene avec un separateur.

```text
PROCEDURE CONCAT_SEP (CH1, CH2: chaine, SEP: caractere, var CH3: chaine)
VAR i : Entier
Debut
    // Copier CH1
    Pour i de 1 a CH1.l faire
        CH3.str[i] := CH1.str[i]
    Fpour

    // Ajouter le separateur
    CH3.str[CH1.l + 1] := SEP

    // Copier CH2
    Pour i de 1 a CH2.l faire
        CH3.str[CH1.l + 1 + i] := CH2.str[i]
    Fpour

    CH3.l := CH1.l + 1 + CH2.l
Fin
```

Exemple : `CONCAT_SEP("Jean", "Dupont", '-', CH3)` donne `"Jean-Dupont"`

</details>

---

## Trace d'execution

```text
CH1 = "ABC" (l = 3)
CH2 = "12" (l = 2)

Etape 1 - Copie CH1:
  i=1: CH3.str[1] = 'A'
  i=2: CH3.str[2] = 'B'
  i=3: CH3.str[3] = 'C'

j = 3

Etape 2 - Copie CH2:
  i=1: CH3.str[3+1] = CH3.str[4] = '1'
  i=2: CH3.str[3+2] = CH3.str[5] = '2'

Etape 3 - Longueur:
  CH3.l = 3 + 2 = 5

Resultat : CH3 = "ABC12"
```

---

## Correspondance COBOL

```cobol
WORKING-STORAGE SECTION.
01 WS-CHAINE1       PIC X(50).
01 WS-CHAINE2       PIC X(50).
01 WS-RESULTAT      PIC X(100).
01 WS-PTR           PIC 9(3) VALUE 1.

PROCEDURE DIVISION.
* Concatenation avec STRING
    INITIALIZE WS-RESULTAT
    STRING WS-CHAINE1 DELIMITED BY SPACE
           WS-CHAINE2 DELIMITED BY SPACE
        INTO WS-RESULTAT
        WITH POINTER WS-PTR.

* Ou simplement (si tailles fixes)
    STRING WS-CHAINE1 DELIMITED BY SIZE
           WS-CHAINE2 DELIMITED BY SIZE
        INTO WS-RESULTAT.
```

---

## Complexite

- Temps : O(n + m) ou n = longueur(CH1) et m = longueur(CH2)
- Espace : O(n + m) pour CH3

---

*Exercice 14 - Chaines de caracteres - Algorithmique Chapitre 02 - M2i*
