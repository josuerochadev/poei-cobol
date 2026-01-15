# Exercice 6 : MAP pour ajout de client

## Objectif

Creer une MAP BMS permettant la saisie de tous les champs pour ajouter un nouveau client.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `CLIAJT.bms` | Source BMS de la MAP d'ajout |
| `ASMAJT.jcl` | JCL d'assemblage de la MAP |

## Differences avec CLIAFF (affichage)

| Aspect | CLIAFF | CLIAJT |
|--------|--------|--------|
| Champs donnees | ASKIP (affichage) | UNPROT (saisie) |
| Libelles calcules | Oui (region, sexe...) | Non |
| Aide contextuelle | Non | Oui (formats attendus) |

## Utilisation

### 1. Copier le source BMS dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CLIAJT
Copier le contenu de CLIAJT.bms
```

### 2. Soumettre le JCL d'assemblage

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member ASMAJT (copier ASMAJT.jcl)
SUB (submit)
```

### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre CLIAJT present dans ROCHA.CICS.LOAD
- Copybook CLIAJT genere dans ROCHA.CICS.LINK

### 4. Definir dans CICS

```
CEDA DEFINE MAPSET(CLIAJT) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIAJT) GROUP(CLIGROUP)
```

## Verification

```
CEDA VIEW MAPSET(CLIAJT) GROUP(CLIGROUP)
```

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour verifier un mapset, utiliser `CEDA VIEW`.
