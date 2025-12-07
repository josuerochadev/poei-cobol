# Exercices COBOL - Chapitre XII

## Thème : Fichiers d'impression et édition

Ce chapitre couvre la génération d'états et le formatage pour impression.

## Fichiers

### Exercices d'édition

| Programme | Description |
|-----------|-------------|
| `C12-EDITION-SIMPLE.cbl` | Caractères d'édition de base |
| `C12-EDITION-MONTANT.cbl` | Édition de montants (Z, *, virgule) |
| `C12-EDITION-SIGNE.cbl` | Édition avec signes (+, -, CR, DB) |

### Applications complètes

| Programme | Description |
|-----------|-------------|
| `C12-FACTURE.cbl` | Génération d'une facture formatée |
| `C12-RAPPORT.cbl` | Rapport avec en-têtes et totaux |
| `C12-RELEVE-PREP.cbl` | Préparation données relevé |
| `C12-RELEVE-PRINT.cbl` | Impression relevé bancaire |

## Compilation et exécution

```bash
# Compiler
cobc -x C12-FACTURE.cbl -o C12-FACTURE

# Exécuter
./C12-FACTURE
```

## Ordre d'exécution recommandé

**Exercices d'édition :**
1. `C12-EDITION-SIMPLE` - Caractères de base
2. `C12-EDITION-MONTANT` - Montants formatés
3. `C12-EDITION-SIGNE` - Gestion des signes

**Applications :**
1. `C12-FACTURE` - Facture complète
2. `C12-RAPPORT` - Rapport avec ruptures
3. `C12-RELEVE-PREP` → `C12-RELEVE-PRINT` - Relevé bancaire

## Concepts abordés

- Caractères d'édition : Z, *, 9, B, 0, /
- Symboles monétaires : $, €
- Virgule et point décimal
- Signes : +, -, CR, DB
- BLANK WHEN ZERO
- Gestion des pages (LINE, PAGE)
- En-têtes et pieds de page
- Totaux et sous-totaux

## Prérequis

- [Chapitre XII - Fichier d'impression](../../../cours/cobol/12-fichier-impression.md)

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre XI](../chapitre-11/README.md) | - |

---

*Félicitations ! Vous avez terminé tous les exercices COBOL.*
