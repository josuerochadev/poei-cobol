# Chapitre II - Interface ISPF et commandes de base

## II-1 : Présentation de ISPF

### Définition
```
ISPF = Interactive System Productivity Facility
```
Interface utilisateur du Mainframe z/OS.

### Menu principal ISPF
| Option | Fonction |
|--------|----------|
| 0 | Settings - Paramètres |
| 1 | View - Visualiser (lecture seule) |
| 2 | Edit - Éditer |
| 3 | Utilities - Utilitaires |
| S | SDSF - Voir résultats des jobs |
| X | Exit - Quitter |

### Organisation des données : PDS
```
USERID.COBOL.SOURCE          ← PDS (Partitioned Data Set)
    ├── PG01CH01             ← MEMBER
    ├── PG02CH01             ← MEMBER
    └── ...

USERID.COBOL.LOAD            ← PDS pour exécutables
```

---

## II-2 : Commandes ligne de l'éditeur

### Commandes simples (1 ligne)
| Cmd | Action | Variante |
|-----|--------|----------|
| `I` | Insert - Insérer après | `I5` = 5 lignes |
| `D` | Delete - Supprimer | `D3` = 3 lignes |
| `R` | Repeat - Dupliquer | `R5` = 5 copies |
| `C` | Copy - Copier | Avec A ou B |
| `M` | Move - Déplacer | Avec A ou B |
| `A` | After - Après cette ligne | Destination |
| `B` | Before - Avant cette ligne | Destination |

### Commandes bloc (plusieurs lignes)
| Cmd | Action |
|-----|--------|
| `CC`...`CC` | Copier un bloc |
| `DD`...`DD` | Supprimer un bloc |
| `MM`...`MM` | Déplacer un bloc |
| `RR`...`RR` | Répéter un bloc |

### Commandes primaires (Command ===>)
| Commande | Action |
|----------|--------|
| `SAVE` | Sauvegarder |
| `CANCEL` | Annuler modifications |
| `SUB` ou `SUBMIT` | Soumettre un JCL |
| `FIND texte` | Rechercher |
| `CHANGE old new` | Remplacer |
| `COLS` | Afficher règle colonnes |
| `RESET` | Réinitialiser affichage |

---

## II-3 : Workflow de compilation

```
SOURCE          JCL           COMPILATEUR      LOAD
(PG01CH01) --> (COMPIL) --> (IGYCRCTL) --> (PG01CH01)
                  |
                  v
            Command ===> SUB
                  |
                  v
               SDSF
          (Return Code)
```

### Return Codes
| RC | Signification |
|----|---------------|
| 0 | Succès |
| 4 | Warning (avertissement) |
| 8+ | Erreur |

---

## Programmes du chapitre

### PG01CH01 : DISPLAY et chaînes
Concepts :
- Instruction DISPLAY
- Guillemets et apostrophes
- Continuation de ligne (col 7 = `-`)
- Mode DEBUGGING (col 7 = `D`)

### PG02CH01 : Nombres décimaux et signés
Concepts :
- Virgule décimale implicite (`V`)
- Nombres signés (`S`)
- Variables d'édition (`Z`, `+`, `-`)

---

## Syntaxe des chaînes de caractères

| Besoin | Syntaxe | Résultat |
|--------|---------|----------|
| Texte simple | `'BONJOUR'` | BONJOUR |
| Guillemet dans chaîne | `'IL A DIT "OUI"'` | IL A DIT "OUI" |
| Apostrophe dans chaîne | `'J''AI'` | J'AI |

---

## Syntaxe des PICTURE numériques

### Stockage
| PIC | Description | Exemple |
|-----|-------------|---------|
| `9(n)` | n chiffres | `9(3)` = 3 chiffres |
| `S9(n)` | Signé | `S9(3)` = signé 3 chiffres |
| `9(n)V9(m)` | Décimal | `9(2)V9(2)` = 99.99 |

### Édition (affichage)
| PIC | Description | Valeur | Résultat |
|-----|-------------|--------|----------|
| `Z9(2)` | Zéros supprimés | 71 | ` 71` |
| `+Z9(2)` | Toujours signe | 71 | `+ 71` |
| `-Z9(2)` | Signe si négatif | 71 | `  71` |
| `-Z9(2)` | Signe si négatif | -71 | `- 71` |

---

## Compilation avec GnuCOBOL

```bash
# Compilation standard
cobc -x programme.cbl -o programme

# Compilation avec mode debug
cobc -x -fdebugging-line programme.cbl -o programme

# Exécution
./programme
```

---

## Points clés à retenir

1. **ISPF** = interface utilisateur Mainframe
2. **PDS** = Partitioned Data Set (dossier avec members)
3. **Commandes ligne** : I, D, R, C, M, CC, DD, MM, RR
4. **SUB** = soumettre un JCL
5. **SDSF** = voir les résultats des jobs
6. **Return Code 0** = succès
7. **V** = virgule virtuelle (pas stockée)
8. **S** = nombre signé
9. **Z** = suppression des zéros de tête

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Structure Programme](01-structure-programme.md) | [Chapitre III - Déclaration Variables](03-declaration-variables.md) |

---
*Formation COBOL - Module COBOL*
