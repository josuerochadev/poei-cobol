# Utilitaires

## Description

Ce dossier contient des scripts utilitaires pour le projet de formation COBOL.

## Scripts disponibles

| Script | Description |
|--------|-------------|
| `compile.sh` | Script de compilation COBOL avec GnuCOBOL |
| `audit-projet.sh` | Audit complet de la structure du projet |

## compile.sh

Script simple pour compiler un programme COBOL.

### Usage

```bash
./utils/compile.sh <fichier.cbl>
```

### Exemple

```bash
./utils/compile.sh exercices/cobol/pratique/chapitre-02/C02-DISPLAY.cbl
```

### Options de compilation

Le script utilise les options suivantes :
- `-x` : Crée un exécutable
- `-debug` : Active les informations de débogage
- `-g` : Génère les symboles de débogage

## audit-projet.sh

Script d'audit pour vérifier la cohérence du projet.

### Vérifications effectuées

- Structure des modules (cours et exercices)
- Présence des fichiers attendus
- Liens de navigation entre chapitres
- Cohérence du README principal

### Usage

```bash
./utils/audit-projet.sh
```

### Sortie

Le script affiche :
- ✓ Les éléments conformes (en vert)
- ⚠ Les avertissements (en jaune)
- ✗ Les erreurs (en rouge)

## Navigation

| Retour |
|--------|
| [Racine du projet](../README.md) |

---
*Formation COBOL - M2i Formation*
