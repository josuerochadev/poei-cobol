# Exemples COBOL

## Description

Ce dossier contient des exemples simples pour tester l'environnement de compilation GnuCOBOL.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `hello-world.cbl` | Premier programme COBOL - Affiche un message de bienvenue |

## Compilation et exécution

```bash
# Compiler le programme
cobc -x hello-world.cbl -o hello-world

# Exécuter
./hello-world
```

## Sortie attendue

```
===============================
Bienvenue dans COBOL !
===============================
```

## Structure du programme

Le programme `hello-world.cbl` illustre :

- Les 4 divisions obligatoires (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- La déclaration de variable avec `WORKING-STORAGE SECTION`
- L'instruction `DISPLAY` pour l'affichage
- L'instruction `STOP RUN` pour terminer le programme

## Navigation

| Retour |
|--------|
| [Racine du projet](../README.md) |

---
*Formation COBOL - M2i Formation*
