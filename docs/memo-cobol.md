# Mémo COBOL

## Structure d'un programme COBOL

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. NOM-PROGRAMME.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.

PROCEDURE DIVISION.
```

## Divisions principales

1. **IDENTIFICATION DIVISION** : Identifie le programme
2. **ENVIRONMENT DIVISION** : Configuration de l'environnement (fichiers, etc.)
3. **DATA DIVISION** : Déclaration des variables
4. **PROCEDURE DIVISION** : Code exécutable

## Types de données (PICTURE)

| Type | Description | Exemple |
|------|-------------|---------|
| `9` | Numérique | `PIC 9(5)` = 5 chiffres |
| `X` | Alphanumérique | `PIC X(10)` = 10 caractères |
| `A` | Alphabétique | `PIC A(5)` = 5 lettres |
| `S` | Signé | `PIC S9(3)` = nombre signé |
| `V` | Virgule décimale | `PIC 9(3)V99` = 3 chiffres + 2 décimales |

## Commandes de base

- `DISPLAY` : Afficher
- `ACCEPT` : Saisir
- `MOVE` : Affecter
- `ADD` : Additionner
- `SUBTRACT` : Soustraire
- `MULTIPLY` : Multiplier
- `DIVIDE` : Diviser
- `COMPUTE` : Calcul (formule)

## Structure de contrôle

### IF
```cobol
IF condition
    statement
ELSE
    statement
END-IF.
```

### EVALUATE (équivalent switch/case)
```cobol
EVALUATE variable
    WHEN valeur1
        statement
    WHEN valeur2
        statement
    WHEN OTHER
        statement
END-EVALUATE.
```

### PERFORM (boucles)
```cobol
PERFORM procedure-name
PERFORM UNTIL condition
PERFORM VARYING compteur FROM 1 BY 1 UNTIL compteur > 10
```
