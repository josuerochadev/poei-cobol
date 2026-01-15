# Exercice 11 : Transaction de mise a jour (MAJO)

## Objectif

Definir la transaction MAJO dans CICS pour permettre l'execution du programme PRGMAJ de mise a jour client.

## Rappel : Architecture CICS

```
+-------------+     +-------------+     +-------------+
| TRANSACTION | --> | PROGRAMME   | --> | MAPSET      |
|    MAJO     |     |   PRGMAJ    |     |   CLIMAJ    |
+-------------+     +-------------+     +-------------+
                           |
                           v
                    +-------------+
                    |   FICHIER   |
                    |   FCLIENT   |
                    +-------------+
```

Une transaction CICS est le point d'entree utilisateur. Elle fait le lien entre :
- Le code transaction saisi (MAJO)
- Le programme a executer (PRGMAJ)

## Definition CICS

### 1. Definir la transaction

```
CEDA DEFINE TRANSACTION(MAJO) GROUP(CLIGROUP) PROGRAM(PRGMAJ)
```

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| TRANSACTION | MAJO | Code transaction (4 caracteres max) |
| GROUP | CLIGROUP | Groupe de ressources du projet |
| PROGRAM | PRGMAJ | Programme COBOL a executer |

### 2. Installer la transaction

```
CEDA INSTALL TRANSACTION(MAJO) GROUP(CLIGROUP)
```

> **Bonne pratique** : Installer uniquement la ressource ajoutee plutot que tout le groupe. Reinstaller le groupe entier peut causer des problemes si le fichier FCLIENT est ouvert ou en cours d'utilisation.

## Verification

### Verifier la transaction

```
CEDA VIEW TRANSACTION(MAJO) GROUP(CLIGROUP)
```

### Verifier le programme

```
CEMT INQ PROGRAM(PRGMAJ)
```

Resultat attendu : `Prog(PRGMAJ) Cob Ena`

## Test de la transaction

```
MAJO
```

Comportement attendu :
1. Ecran de saisie du numero de compte
2. Saisir un numero existant (ex: 100001)
3. Affichage des donnees du client
4. Modifier les champs souhaites
5. ENTER pour valider
6. Message "MISE A JOUR EFFECTUEE"

## Ressources du groupe CLIGROUP

| Type | Nom | Description |
|------|-----|-------------|
| FILE | FCLIENT | Fichier VSAM clients |
| MAPSET | CLIAFF | Ecran affichage |
| MAPSET | CLIAJT | Ecran ajout |
| MAPSET | CLIMAJ | Ecran mise a jour |
| PROGRAM | PRGCLIA | Programme affichage |
| PROGRAM | PRGAJT | Programme ajout |
| PROGRAM | PRGMAJ | Programme mise a jour |
| TRANSACTION | AFFI | Transaction affichage |
| TRANSACTION | AJOU | Transaction ajout |
| TRANSACTION | MAJO | Transaction mise a jour |
