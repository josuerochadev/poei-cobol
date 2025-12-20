# Chapitre II - Interface ISPF et commandes de base

Ce chapitre présente l'interface ISPF nécessaire pour la création d'un programme COBOL, depuis la saisie du code jusqu'à la vérification des résultats.

> **Note** : Pour une documentation complète d'ISPF, consultez le [Chapitre IV - ISPF/PDF](../zos-tso/04-ispf.md) du module z/OS-TSO.

---

## II-1 : Présentation de ISPF

### Définition

```
ISPF = Interactive System Productivity Facility
```

**ISPF** est un produit IBM permettant aux utilisateurs de mener plusieurs opérations :

- Création et modification des membres en tant qu'éditeur plein page
- Lancement des commandes d'exécution des travaux et de compilation des programmes
- Suivi de l'exécution des travaux à travers l'interface **SDSF** (Spool Search)
- Maintenance de l'environnement VSAM (création/maintenance des fichiers de données)
- Modification des fichiers non VSAM via l'éditeur

### Menu principal ISPF

| Option | Nom | Description |
|--------|-----|-------------|
| 0 | Settings | Paramètres terminal et utilisateur |
| 1 | View | Affichage lecture seule |
| 2 | Edit | Création et modification du code |
| 3 | Utilities | Utilitaires (gestion datasets) |
| S | SDSF | Spool Display - Résultats des jobs |
| X | Exit | Quitter ISPF |

### Touches de fonction essentielles

| Touche | Action |
|--------|--------|
| `F1` | Aide contextuelle |
| `F2` | Split (diviser l'écran) |
| `F3` | Exit / Retour |
| `F7` | Page précédente (Backward) |
| `F8` | Page suivante (Forward) |
| `F9` | Swap (basculer entre écrans splittés) |
| `F12` | Cancel |

### Organisation des données : PDS

```
USERID.COBOL.SOURCE          ← PDS (Partitioned Data Set)
    ├── PG01CH01             ← MEMBER (programme)
    ├── COMPIL1              ← MEMBER (JCL compilation)
    └── COMPIL2              ← MEMBER (JCL exécution)

USERID.COBOL.LOAD            ← PDS pour exécutables
```

---

## II-2 : Commandes ligne de l'éditeur

Les commandes ligne s'écrivent directement sur le numéro de ligne dans l'éditeur.

### Commandes simples (1 ligne)

| Cmd | Action | Variante |
|-----|--------|----------|
| `I` | Insert - Insérer après | `I5` = 5 lignes |
| `D` | Delete - Supprimer | `D3` = 3 lignes |
| `R` | Repeat - Dupliquer | `R5` = 5 copies |
| `C` | Copy - Copier | Avec A ou B |
| `M` | Move - Déplacer | Avec A ou B |
| `O` | Overlay - Copie par superposition | |
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
| `FIND 'texte'` | Rechercher |
| `CHANGE 'old' 'new'` | Remplacer |
| `COLS` | Afficher règle colonnes |
| `RESET` | Réinitialiser affichage (efface les messages Warning) |

---

## II-3 : Exemples de commandes ligne

### COPY (C) - Copier une ligne

Copier la ligne 400 avant la ligne 500 :

```
000300       PROCEDURE DIVISION.
00C400           DISPLAY 'BIENVENUE'.    ← C = copier cette ligne
00B500           STOP RUN.               ← B = insérer avant ici
```

**Résultat :**
```
000400           DISPLAY 'BIENVENUE'.
000410           DISPLAY 'BIENVENUE'.    ← Copie insérée
000500           STOP RUN.
```

### INSERT (I) - Insérer des lignes

Insérer 5 lignes après la ligne 300 :

```
000200       PROGRAM-ID. BIENVENU.
00I500       PROCEDURE DIVISION.         ← I5 = insérer 5 lignes après
```

### MOVE (M) - Déplacer une ligne

Déplacer la ligne 200 après la ligne 410 :

```
000100       IDENTIFICATION DIVISION.
0M0200       PROGRAM-ID. BIENVENU.       ← M = déplacer cette ligne
000400       PROCEDURE DIVISION.
0A0410           DISPLAY 'BIENVENUE'.    ← A = après cette ligne
```

### REPEAT (R) - Répéter une ligne

Répéter la ligne 400 quatre fois :

```
R40400       PROCEDURE DIVISION.         ← R4 = répéter 4 fois
```

**Résultat :**
```
000400       PROCEDURE DIVISION.
000401       PROCEDURE DIVISION.
000402       PROCEDURE DIVISION.
000403       PROCEDURE DIVISION.
000404       PROCEDURE DIVISION.
```

---

## II-4 : Exercice pratique - Programme PG01CH01

### Création du membre

Dans l'écran **Edit Entry Panel** (Option 2), renseigner :

```
ISPF Library:
   Project . . . FORM1011
   Group . . . . COBOL
   Type  . . . . SOURCE
   Member  . . . PG01CH01
```

### Programme COBOL à saisir

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG01CH01.
       AUTHOR. FORMATION.
      *---------------------------------------------------------
      * PROGRAMME D'AFFICHAGE (LE DISPLAY)
      *---------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. IBM-ZOS WITH DEBUGGING MODE.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-MESSAGE1     PIC X(35).
       01  WS-MESSAGE2     PIC X(35).
       01  WS-MESSAGE3     PIC X(35).
       01  WS-MESSAGE4     PIC X(99).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INIT
           PERFORM 2000-TRAITEMENT
           PERFORM 9000-FIN
           STOP RUN.

       1000-INIT.
           MOVE 'BIENVENUE A LA FORMATION COBOL' TO WS-MESSAGE1.

       2000-TRAITEMENT.
           DISPLAY WS-MESSAGE1.
           DISPLAY 'CECI EST UN AFFICHAGE DIRECT'.

       9000-FIN.
           DISPLAY 'FIN DU PROGRAMME PG01CH01'.
```

---

## II-5 : Compilation et exécution

### JCL de compilation (COMPIL1)

Créer un nouveau membre **COMPIL1** dans la library SOURCE :

```jcl
//COMPIL1  JOB COMPIL1,'COMPIL1',MSGLEVEL=(1,1),REGION=4M,
//             MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*------------------------------------------------
//* JCL DE COMPILATION & EDITION DE LIEN
//* PROGRAMME PG01CH01
//*------------------------------------------------
//COMPIL1  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=FORM1011.COBOL.SOURCE(PG01CH01),DISP=SHR
//LKED.SYSLMOD DD DSN=FORM1011.COBOL.LOAD(PG01CH01),DISP=SHR
/*
//
```

### JCL d'exécution (COMPIL2)

Créer un nouveau membre **COMPIL2** :

```jcl
//COMPIL2  JOB COMPIL2,'COMPIL2',MSGLEVEL=(1,1),REGION=4M,
//             MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*------------------------------------------------
//* EXECUTION PROGRAMME AFFICHAGE
//*------------------------------------------------
//STEPTEST EXEC PGM=PG01CH01
//STEPLIB  DD DSN=FORM1011.COBOL.LOAD,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
/*
//
```

---

## II-6 : Procédure de l'exercice

### Étape 1 : Préparation

1. Vérifier la présence des librairies :
   - `FORM1011.COBOL.SOURCE` (RECFM=FB, LRECL=80)
   - `FORM1011.COBOL.LOAD` (RECFM=U)

2. Créer le programme **PG01CH01** dans SOURCE

3. Créer les JCL **COMPIL1** et **COMPIL2** dans SOURCE

### Étape 2 : Compilation

1. Éditer le membre COMPIL1
2. Soumettre le JCL :
   ```
   Command ===> SUB
   ```
3. Retourner au menu principal (`F3`)

### Étape 3 : Vérification dans SDSF

1. Accéder à SDSF :
   ```
   Command ===> S      (depuis le menu principal)
   ```

2. Sélectionner Output Queue :
   ```
   Command ===> O
   ```

3. Taper `S` devant le JOBNAME pour afficher le contenu

4. Vérifier le **RETURN CODE** :

| RC | Signification | Action |
|----|---------------|--------|
| 0 | Succès | Passer à l'exécution |
| 4 | Warning | Compilation OK, vérifier les avertissements |
| 8+ | Erreur | Corriger et recompiler |

### Étape 4 : En cas d'erreur

1. Identifier le message d'erreur dans le listing SDSF
2. Noter le numéro de ligne et le code erreur
3. Consulter l'explication du message
4. Corriger l'erreur dans le programme source
5. Resoumettre le JCL de compilation

### Étape 5 : Exécution

1. Vérifier que le membre exécutable existe dans `FORM1011.COBOL.LOAD`
2. Éditer le membre **COMPIL2**
3. Soumettre le JCL :
   ```
   Command ===> SUB
   ```
4. Vérifier le résultat dans SDSF (Output Queue)

---

## II-7 : Syntaxe des chaînes et PICTURE

### Chaînes de caractères

| Besoin | Syntaxe | Résultat |
|--------|---------|----------|
| Texte simple | `'BONJOUR'` | BONJOUR |
| Guillemet dans chaîne | `'IL A DIT "OUI"'` | IL A DIT "OUI" |
| Apostrophe dans chaîne | `'J''AI'` | J'AI |

### PICTURE numériques - Stockage

| PIC | Description | Exemple |
|-----|-------------|---------|
| `9(n)` | n chiffres | `9(3)` = 3 chiffres |
| `S9(n)` | Signé | `S9(3)` = signé 3 chiffres |
| `9(n)V9(m)` | Décimal | `9(2)V9(2)` = 99.99 |

### PICTURE numériques - Édition (affichage)

| PIC | Description | Valeur | Résultat |
|-----|-------------|--------|----------|
| `Z9(2)` | Zéros supprimés | 71 | ` 71` |
| `+Z9(2)` | Toujours signe | 71 | `+ 71` |
| `-Z9(2)` | Signe si négatif | -71 | `- 71` |

---

## Compilation avec GnuCOBOL (environnement local)

```bash
# Compilation standard
cobc -x programme.cbl -o programme

# Compilation avec mode debug (active les lignes D en col 7)
cobc -x -fdebugging-line programme.cbl -o programme

# Exécution
./programme
```

---

## Récapitulatif des commandes essentielles

| Action | Commande |
|--------|----------|
| Soumettre un JCL | `SUB` |
| Effacer les messages | `RESET` |
| Sauvegarder et quitter | `F3` |
| Annuler les modifications | `CANCEL` |
| Rechercher | `FIND 'texte'` |
| Remplacer | `CHANGE 'old' 'new'` |
| Page précédente | `F7` |
| Page suivante | `F8` |
| Accéder à SDSF | `S` depuis le menu principal |
| Voir Output Queue | `O` dans SDSF |

---

## Points clés à retenir

1. **ISPF** = interface utilisateur Mainframe (voir [module zos-tso](../zos-tso/04-ispf.md) pour détails)
2. **PDS** = Partitioned Data Set (dossier avec members)
3. **Commandes ligne** : I, D, R, C, M, O, CC, DD, MM, RR
4. **SUB** = soumettre un JCL pour compilation/exécution
5. **SDSF** = voir les résultats des jobs (Return Code)
6. **Return Code 0** = succès, RC 4 = warning, RC 8+ = erreur
7. **V** = virgule virtuelle (pas stockée)
8. **S** = nombre signé
9. **Z** = suppression des zéros de tête

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre I - Structure Programme](01-structure-programme.md) | [Chapitre III - Déclaration Variables](03-declaration-variables.md) |

---
*Formation COBOL - M2i Formation*
